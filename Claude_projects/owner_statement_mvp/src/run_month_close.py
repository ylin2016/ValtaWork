import argparse
from pathlib import Path
import secrets
import calendar
import json
import uuid

from .config import load_config
from .db import connect, init_db
from .mappings import load_class_mapping, load_account_rules
from .qbo_client import QBOClient
from .qbo_sync import sync_qbo_expenses
from .guesty_adapter import import_guesty_bookings_csv
from .statement_engine import create_run, build_statements
from .excel_writer import create_template, write_statement
from .booking_fees import get_booking_fees
from .utils import sha256_file

def upsert_from_mapping(conn, mapping_items: list[dict]):
    cur = conn.cursor()
    for m in mapping_items:
        owner_id = m["owner_id"]
        cur.execute("""INSERT OR REPLACE INTO owners(owner_id, owner_name, owner_email)
                       VALUES (?, ?, ?)""", (owner_id, m.get("owner_name",""), m.get("owner_email","")))
        cur.execute("""INSERT OR REPLACE INTO properties(property_id, property_name, owner_id, qbo_class_id, qbo_class_name, guesty_listing_id, is_active)
                       VALUES (?, ?, ?, ?, ?, ?, 1)""", (m["property_id"], m.get("property_name",""), owner_id, m["qbo_class_id"], m.get("qbo_class_name"), m.get("guesty_listing_id")))
        # Write pm_fee_rate into owner_contracts if present in mapping
        if m.get("pm_fee_rate") is not None:
            pid = m["property_id"]
            rate = float(m["pm_fee_rate"])
            existing = cur.execute(
                "SELECT contract_id FROM owner_contracts WHERE property_id=? LIMIT 1", (pid,)
            ).fetchone()
            if existing:
                cur.execute("UPDATE owner_contracts SET pm_fee_rate=? WHERE contract_id=?",
                            (rate, existing[0]))
            else:
                cur.execute("""INSERT INTO owner_contracts
                               (contract_id, property_id, effective_start, statement_basis,
                                pm_fee_type, pm_fee_rate, pm_fee_base, reserve_target)
                               VALUES (?, ?, '2020-01-01', 'cash', 'percent', ?, 'net_booking_revenue', 0)""",
                            (str(uuid.uuid4()), pid, rate))
    conn.commit()

def exception_logger(conn):
    def _log(sev, code, msg, obj, line_id=None, property_id=None):
        conn.execute(
            """INSERT INTO exceptions(exception_id, source, source_object, source_txn_id, source_line_id, property_id,
                                        severity, code, message, payload_json)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)""",
            (str(uuid.uuid4()),
             "qbo",
             obj.get("TxnType") or obj.get("MetaData",{}).get("CreateTime","unknown"),
             str(obj.get("Id","unknown")),
             line_id,
             property_id,
             sev, code, msg, json.dumps(obj)[:20000]),
        )
        conn.commit()
    return _log

def cmd_init_db(args):
    cfg = load_config(args.config)
    conn = connect(cfg["app"]["db_path"])
    init_db(conn, args.schema)
    print(f"DB initialized at {cfg['app']['db_path']}")

    tpath = Path(args.template)
    if not tpath.exists():
        tpath.parent.mkdir(parents=True, exist_ok=True)
        create_template(str(tpath))
        print(f"Template created at {tpath}")

def cmd_sync_mappings(args):
    cfg = load_config(args.config)
    conn = connect(cfg["app"]["db_path"])
    items = load_class_mapping(args.mapping_classes)
    upsert_from_mapping(conn, items)
    print("Mappings synced into DB.")

def cmd_qbo_auth(args):
    cfg = load_config(args.config)
    q = cfg["qbo"]
    qbo = QBOClient(realm_id=q["realm_id"], base_url=q["base_url"], minorversion=int(q.get("minorversion",75)))
    state = secrets.token_hex(16)
    print("Open this URL in your browser and approve access:")
    print(qbo.auth_url(state))
    print("")
    print("Then run:")
    print("python -m src.run_month_close qbo-exchange --code <CODE_FROM_REDIRECT_URL>")

def cmd_qbo_exchange(args):
    cfg = load_config(args.config)
    q = cfg["qbo"]
    qbo = QBOClient(realm_id=q["realm_id"], base_url=q["base_url"], minorversion=int(q.get("minorversion",75)))
    qbo.exchange_code_for_tokens(args.code)
    print("Tokens saved to ./data/qbo_tokens.json")

def cmd_qbo_sync(args):
    cfg = load_config(args.config)
    conn = connect(cfg["app"]["db_path"])

    items = load_class_mapping(args.mapping_classes)
    upsert_from_mapping(conn, items)

    property_map = {m["qbo_class_id"]: m["property_id"] for m in items if m.get("qbo_class_id") and m.get("property_id")}
    acct_rules = load_account_rules(args.mapping_accounts)

    q = cfg["qbo"]
    qbo = QBOClient(realm_id=q["realm_id"], base_url=q["base_url"], minorversion=int(q.get("minorversion",75)))

    exc_cb = exception_logger(conn)
    sync_qbo_expenses(conn, qbo, property_map, acct_rules, args.start, args.end, exceptions_cb=exc_cb)
    print("QBO sync complete.")

def cmd_guesty_import(args):
    cfg = load_config(args.config)
    conn = connect(cfg["app"]["db_path"])
    import_guesty_bookings_csv(conn, args.csv)
    print("Guesty bookings imported.")

def cmd_build(args):
    cfg = load_config(args.config)
    conn = connect(cfg["app"]["db_path"])
    period = args.period
    y, m = map(int, period.split("-"))
    period_start = f"{y:04d}-{m:02d}-01"
    last_day = calendar.monthrange(y, m)[1]
    period_end = f"{y:04d}-{m:02d}-{last_day:02d}"
    default_rate = float(cfg["statement_policy"]["default_pm_fee_rate"])

    # Load mappings for owner flags
    items = load_class_mapping(args.mapping_classes)

    run_id = create_run(conn, period_start, period_end, cfg["statement_policy"]["default_basis"])
    build_statements(conn, run_id, period_start, period_end,
                     default_pm_fee_rate=default_rate,
                     default_reserve_target=float(cfg["statement_policy"]["default_reserve_target"]))

    out_dir = Path(args.output_dir)/period/"statements"
    out_dir.mkdir(parents=True, exist_ok=True)

    props = conn.execute("""SELECT p.*, o.owner_name, o.owner_email
                              FROM properties p JOIN owners o ON p.owner_id=o.owner_id
                              WHERE p.is_active=1""").fetchall()
    generated = 0
    for p in props:
        pid = p["property_id"]

        # PM fee rate: contract > config default
        c = conn.execute("""SELECT pm_fee_rate FROM owner_contracts
                            WHERE property_id=? AND effective_start<=?
                              AND (effective_end IS NULL OR effective_end>=?)
                            ORDER BY effective_start DESC LIMIT 1""",
                         (pid, period_start, period_start)).fetchone()
        pm_fee_rate = float(c["pm_fee_rate"]) if c and c["pm_fee_rate"] is not None else default_rate

        # Guesty bookings with fees and owner costs
        guesty_rows = conn.execute("""
            SELECT source_txn_id as booking_id, vendor_customer as guest_name,
                   posting_date as checkin, service_date as checkout, amount as net_revenue
            FROM ledger_lines
            WHERE property_id=? AND posting_date>=? AND posting_date<=?
              AND source='guesty' AND category='INCOME' AND include_in_statement=1
            ORDER BY posting_date""", (pid, period_start, period_end)).fetchall()

        bookings = []
        for row in guesty_rows:
            booking_dict = dict(row)
            fees = get_booking_fees(conn, pid, booking_dict['booking_id'], period_start, period_end)
            booking_dict.update(fees)
            bookings.append(booking_dict)

        # QBO deposit income (lease rent, credits, etc.)
        other_income = conn.execute("""
            SELECT posting_date, description, vendor_customer, subcategory, amount
            FROM ledger_lines
            WHERE property_id=? AND posting_date>=? AND posting_date<=?
              AND source='qbo' AND category='INCOME' AND include_in_statement=1
            ORDER BY posting_date""", (pid, period_start, period_end)).fetchall()

        # Owner-responsibility QBO expenses only (account contains 'Owner')
        expense_rows = conn.execute("""
            SELECT posting_date, description, vendor_customer, qbo_account, subcategory, amount
            FROM ledger_lines
            WHERE property_id=? AND posting_date>=? AND posting_date<=?
              AND source='qbo' AND category='EXPENSE' AND include_in_statement=1
              AND qbo_account LIKE '%Owner Expenses%'
            ORDER BY subcategory, posting_date""", (pid, period_start, period_end)).fetchall()

        expenses_by_subcat = {}
        for exp in expense_rows:
            key = exp["subcategory"] or "Other Expense"
            expenses_by_subcat.setdefault(key, []).append(exp)

        # Skip properties with zero activity this period
        if not bookings and not other_income and not expense_rows:
            continue

        tot = conn.execute("""SELECT amount_due_to_owner FROM statement_property_totals
                               WHERE run_id=? AND property_id=?""",
                           (run_id, pid)).fetchone()
        totals = {
            "starting_balance": 0.0,
            "net_income": float(tot["amount_due_to_owner"]) if tot else 0.0,
        }

        # Load property flags from mapping
        owner_pays_cleaning = any(m["property_id"] == pid and m.get("owner_pays_cleaning")
                                  for m in items)
        owner_pays_taxes = any(m["property_id"] == pid and m.get("owner_pays_taxes")
                              for m in items)
        owner_pays_supplies = any(m["property_id"] == pid and m.get("owner_pays_supplies")
                                 for m in items)

        out_path = out_dir / f"{pid}_owner_statement_{period}.xlsx"
        write_statement(
            str(out_path), period,
            {"property_id": pid, "property_name": p["property_name"]},
            {"owner_name": p["owner_name"], "owner_email": p["owner_email"] or ""},
            pm_fee_rate, bookings, other_income, expenses_by_subcat, totals,
            owner_pays_cleaning=owner_pays_cleaning,
            owner_pays_supplies=owner_pays_supplies,
            owner_pays_taxes=owner_pays_taxes,
        )

        sha = sha256_file(str(out_path))
        conn.execute("""INSERT OR REPLACE INTO statement_outputs(run_id, property_id, output_path, output_sha256)
                        VALUES (?, ?, ?, ?)""", (run_id, pid, str(out_path), sha))
        conn.commit()
        generated += 1

    print(f"Statements generated for {period}: {generated} files → {out_dir}")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--config", default="config.yml")
    ap.add_argument("--schema", default="schema.sql")
    ap.add_argument("--template", default="templates/owner_statement_template.xlsx")
    ap.add_argument("--mapping-classes", default="mapping_classes.yml")
    ap.add_argument("--mapping-accounts", default="mapping_accounts.yml")
    ap.add_argument("--output-dir", default="output")

    sub = ap.add_subparsers(dest="cmd", required=True)

    sub.add_parser("init-db").set_defaults(func=cmd_init_db)
    sub.add_parser("sync-mappings").set_defaults(func=cmd_sync_mappings)
    sub.add_parser("qbo-auth").set_defaults(func=cmd_qbo_auth)

    p = sub.add_parser("qbo-exchange")
    p.add_argument("--code", required=True)
    p.set_defaults(func=cmd_qbo_exchange)

    p = sub.add_parser("qbo-sync")
    p.add_argument("--start", required=True)
    p.add_argument("--end", required=True)
    p.set_defaults(func=cmd_qbo_sync)

    p = sub.add_parser("guesty-import")
    p.add_argument("--csv", required=True)
    p.set_defaults(func=cmd_guesty_import)

    p = sub.add_parser("build")
    p.add_argument("--period", required=True)
    p.set_defaults(func=cmd_build)

    args = ap.parse_args()
    args.func(args)

if __name__ == "__main__":
    main()
