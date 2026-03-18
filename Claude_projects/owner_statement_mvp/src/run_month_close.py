import argparse
from pathlib import Path
import secrets
import pandas as pd
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
from .utils import sha256_file

def upsert_from_mapping(conn, mapping_items: list[dict]):
    cur = conn.cursor()
    for m in mapping_items:
        owner_id = m["owner_id"]
        cur.execute("""INSERT OR REPLACE INTO owners(owner_id, owner_name, owner_email)
                       VALUES (?, ?, ?)""", (owner_id, m.get("owner_name",""), m.get("owner_email","")))
        cur.execute("""INSERT OR REPLACE INTO properties(property_id, property_name, owner_id, qbo_class_id, qbo_class_name, guesty_listing_id, is_active)
                       VALUES (?, ?, ?, ?, ?, ?, 1)""", (m["property_id"], m.get("property_name",""), owner_id, m["qbo_class_id"], m.get("qbo_class_name"), m.get("guesty_listing_id")))
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

    run_id = create_run(conn, period_start, period_end, cfg["statement_policy"]["default_basis"])
    build_statements(conn, run_id, period_start, period_end,
                     default_pm_fee_rate=float(cfg["statement_policy"]["default_pm_fee_rate"]),
                     default_reserve_target=float(cfg["statement_policy"]["default_reserve_target"]))

    template_path = args.template
    out_dir = Path(args.output_dir)/period/"statements"
    out_dir.mkdir(parents=True, exist_ok=True)

    props = conn.execute("""SELECT p.*, o.owner_name, o.owner_email
                              FROM properties p JOIN owners o ON p.owner_id=o.owner_id
                              WHERE p.is_active=1""").fetchall()

    for p in props:
        pid = p["property_id"]

        book = conn.execute(
            """SELECT booking_id, vendor_customer, posting_date, amount
               FROM ledger_lines
               WHERE property_id=? AND posting_date>=? AND posting_date<=?
                 AND category='INCOME' AND source='guesty' AND include_in_statement=1""",
            (pid, period_start, period_end),
        ).fetchall()
        booking_df = pd.DataFrame([
            [pid, r["booking_id"], r["vendor_customer"], "", r["posting_date"], "", "", "confirmed",
             "", "", "", "", "", "", "", float(r["amount"]), ""]
            for r in book
        ], columns=["Property ID","Booking ID","Channel","Guest Name","Check-in","Check-out","Booked Date","Status",
                    "Rent","Cleaning Fee","Other Fees","Discount","Refund","Taxes","Channel Fee","Net Booking Revenue","Notes"])

        exp = conn.execute(
            """SELECT property_id, (SELECT qbo_class_id FROM properties WHERE property_id=ledger_lines.property_id) as qbo_class_id,
                      source_object as txn_type, source_txn_id as txn_id, source_line_id as line_id,
                      posting_date as txn_date, vendor_customer as vendor,
                      qbo_account as account, category, subcategory, description, amount
               FROM ledger_lines
               WHERE property_id=? AND posting_date>=? AND posting_date<=?
                 AND source='qbo' AND include_in_statement=1 AND category='EXPENSE'""",
            (pid, period_start, period_end),
        ).fetchall()
        expenses_df = pd.DataFrame([
            [r["property_id"], r["qbo_class_id"], r["txn_type"], r["txn_id"], r["line_id"], r["txn_date"],
             r["vendor"], r["vendor"], r["account"], r["category"], r["subcategory"], r["description"], float(r["amount"]), "Y", ""]
            for r in exp
        ], columns=["Property ID","QBO Class ID","Txn Type","Txn ID","Line ID","Txn Date","Vendor","Payee","Account",
                    "Category","Subcategory","Description / Memo","Amount","Include?","Notes"])

        fees = conn.execute(
            """SELECT category, subcategory, description, amount
               FROM ledger_lines
               WHERE property_id=? AND posting_date=? AND source='manual' AND include_in_statement=1""",
            (pid, period_end),
        ).fetchall()
        fees_df = pd.DataFrame([
            [pid, r["category"], r["subcategory"], r["description"], float(r["amount"]), "Calc", "Y"]
            for r in fees
        ], columns=["Property ID","Type","Subcategory","Description","Amount","Source","Include?"])

        setup = {
            "Statement Period Start": period_start,
            "Statement Period End": period_end,
            "Property ID": pid,
            "Property Name": p["property_name"],
            "Owner Name": p["owner_name"],
            "Owner Email": p["owner_email"],
            "QBO Class ID": p["qbo_class_id"],
            "Statement Version": "v1",
        }

        out_path = out_dir/f"{pid}_owner_statement_{period}.xlsx"
        write_statement(str(out_path), template_path, setup, booking_df, expenses_df, fees_df)

        sha = sha256_file(str(out_path))
        conn.execute(
            """INSERT OR REPLACE INTO statement_outputs(run_id, property_id, output_path, output_sha256)
               VALUES (?, ?, ?, ?)""",
            (run_id, pid, str(out_path), sha),
        )
        conn.commit()

    print(f"Statements generated for {period}: {out_dir}")

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
