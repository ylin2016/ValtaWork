import argparse
from pathlib import Path
import secrets
import calendar
import json
import uuid
import pandas as pd

from .config import load_config
from .db import connect, init_db
from .mappings import load_class_mapping, load_account_rules
from .qbo_client import QBOClient
from .qbo_sync import sync_qbo_expenses
from .guesty_adapter import import_guesty_bookings_csv
from .statement_engine import create_run, build_statements
from .pm_rate import resolve_pm_fee_rate
from .excel_writer import create_template, write_statement
from .booking_fees import get_qbo_fees, get_owner_costs
from .ltr_records import build_records as build_ltr_records, is_rent_income
from .listing_filter import allowed_property_ids
from .utils import sha256_file, now_iso

def _calculate_implied_channel_fee(booking_id: str, guesty_csv_path: str) -> float:
    """
    Calculate implied channel fee from converted Guesty CSV:
    ACCOMMODATION + PET + EXTRA PERSON - TOTAL PAYOUT (returns negative)
    Formula: implied_channel_fee = total_payout - (accom + pet + extra)
    """
    import pandas as pd
    from pathlib import Path

    csv_path = Path(guesty_csv_path)
    if not csv_path.exists():
        return 0.0

    try:
        df = pd.read_csv(str(csv_path), encoding='utf-8-sig')
        rows = df[df['booking_id'] == booking_id]
        if rows.empty:
            return 0.0

        row = rows.iloc[0]

        # Get values from converted CSV
        rent = float(row.get('rent', 0))  # accom + extra + pet
        total_payout = float(row.get('total_payout', 0))

        # Channel fee = total_payout - (accom + pet + extra)
        implied_fee = total_payout - rent

        # Return negative value (fee is a cost)
        return implied_fee if implied_fee < 0 else 0.0
    except Exception as e:
        return 0.0

def _apply_guesty_fees(conn, period_start: str, period_end: str, guesty_csv_path: str):
    """Recompute every guesty INCOME line as base_amount - QBO fees, IDEMPOTENTLY.

    Net is always derived from the immutable Guesty base (`base_amount`), never from
    the live `amount` (which a prior build may have already fee-adjusted) — so a
    re-run can never double-subtract fees. Channel rule (see CLAUDE.md):
      Cancelled        -> net = base (= total_payout, set in convert_guesty_export)
      Booking.com      -> net = base - channel_fee - stripe_fee - tax
      Other channels   -> net = base - channel_fee - stripe_fee
    QBO fees from get_qbo_fees are already negative; abs() to subtract.

    MUST run before build_statements so the stored totals reflect fee-adjusted net
    in the same build (the Excel loop later reads those totals back).
    """
    rows = conn.execute(
        """SELECT property_id, source_txn_id AS booking_id, posting_date AS checkin,
                  service_date AS checkout, subcategory AS channel, status AS booking_status,
                  COALESCE(base_amount, amount) AS base_net
           FROM ledger_lines
           WHERE posting_date>=? AND posting_date<=?
             AND source='guesty' AND category='INCOME' AND include_in_statement=1""",
        (period_start, period_end),
    ).fetchall()

    for r in rows:
        d = dict(r)
        base_net = round(float(d["base_net"] or 0), 2)
        net = base_net

        if (d["booking_status"] or "").lower() == "confirmed":
            fees = get_qbo_fees(conn, d["property_id"], d["booking_id"],
                                d["checkin"], d["checkout"], period_start, period_end)
            channel_fee = fees["channel_fee"]
            stripe_fee = fees["stripe_fee"]
            tax_fee = fees["tax"]

            # No channel fee in QBO -> fall back to implied fee from the converted CSV
            if channel_fee == 0.0:
                implied = _calculate_implied_channel_fee(d["booking_id"], guesty_csv_path)
                if implied != 0.0:
                    channel_fee = implied

            if "BOOKING.COM" in (d["channel"] or "").upper():
                net = base_net - abs(channel_fee) - abs(stripe_fee) - abs(tax_fee)
            else:
                net = base_net - abs(channel_fee) - abs(stripe_fee)

        # Store amounts at cent precision so downstream totals are penny-clean.
        net = round(net, 2)

        # Persist net and (re)assert the immutable base, backfilling legacy rows.
        conn.execute(
            """UPDATE ledger_lines SET amount=?, base_amount=?
               WHERE source='guesty' AND source_txn_id=? AND category='INCOME'""",
            (net, base_net, d["booking_id"]),
        )
    conn.commit()

def _read_guesty_cleaning_by_code(guesty_csv_path: str) -> dict[str, float]:
    """booking_id -> guest-paid cleaning fee, from the converted Guesty CSV.

    Single reader shared by the OWNER_ADJ cleaning credit (folded into amount_due)
    and the Excel 'Owner Cleaning Fee' display column, so the two can't drift.
    Returns {} if the CSV is missing.
    """
    try:
        gdf = pd.read_csv(guesty_csv_path)
    except FileNotFoundError:
        return {}
    return {str(r["booking_id"]): float(r.get("cleaning_fee") or 0)
            for _, r in gdf.iterrows() if pd.notna(r.get("booking_id"))}

def _apply_owner_cleaning_credit(conn, period_start: str, period_end: str,
                                 guesty_csv_path: str, mapping_items: list[dict]):
    """Credit owner_pays_cleaning properties the Guesty cleaning fee.

    For these properties the owner is responsible for cleaning, so the cleaning fee the
    guest paid (which the Guesty net-revenue formula subtracts out) belongs to the owner.
    We add it back as a NON-commissioned OWNER_ADJ line so the stored amount_due (Excel
    summary + end balances) includes it, matching the dashboard's Net Revenue table.
    OWNER_ADJ has no display section of its own, so it does not double-print the cleaning
    fee already shown per-booking. Idempotent: clears its own prior lines for the period.
    """
    conn.execute("""DELETE FROM ledger_lines
                    WHERE posting_date>=? AND posting_date<=?
                      AND source='manual' AND category='OWNER_ADJ'
                      AND source_object='OwnerCleaningCredit'""",
                 (period_start, period_end))

    cleaning_props = {m["property_id"] for m in mapping_items if m.get("owner_pays_cleaning")}
    if not cleaning_props:
        conn.commit()
        return

    # booking_id -> cleaning fee (guest-paid) from the converted Guesty CSV
    fee_by_code = _read_guesty_cleaning_by_code(guesty_csv_path)
    if not fee_by_code:
        conn.commit()
        return

    # Sum per property over this period's guesty INCOME bookings (canceled stays -> 0,
    # mirroring the dashboard which zeroes cleaning for canceled bookings).
    rows = conn.execute(
        """SELECT property_id, booking_id, status FROM ledger_lines
           WHERE posting_date>=? AND posting_date<=?
             AND source='guesty' AND category='INCOME' AND include_in_statement=1""",
        (period_start, period_end)).fetchall()
    total_by_pid: dict[str, float] = {}
    for r in rows:
        pid = r["property_id"]
        if pid not in cleaning_props:
            continue
        if str(r["status"] or "").lower() == "canceled":
            continue
        fee = fee_by_code.get(str(r["booking_id"]), 0.0)
        total_by_pid[pid] = round(total_by_pid.get(pid, 0.0) + fee, 2)

    for pid, amt in total_by_pid.items():
        if abs(amt) < 0.005:
            continue
        conn.execute(
            """INSERT INTO ledger_lines
               (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                posting_date, category, subcategory, description, amount, include_in_statement, status, last_updated_at)
               VALUES (?, 'manual', 'OwnerCleaningCredit', ?, NULL, ?, ?, 'OWNER_ADJ', 'Owner Cleaning Fee', ?, ?, 1, 'posted', ?)""",
            (str(uuid.uuid4()), f"cleancredit_{pid}_{period_start}", pid, period_end,
             "Owner Cleaning Fee (guest-paid, owner keeps)", amt, now_iso()),
        )
    conn.commit()

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
    base_dir = Path(__file__).parent.parent
    period = args.period
    y, m = map(int, period.split("-"))
    period_start = f"{y:04d}-{m:02d}-01"
    last_day = calendar.monthrange(y, m)[1]
    period_end = f"{y:04d}-{m:02d}-{last_day:02d}"

    # Load mappings for owner flags
    items = load_class_mapping(args.mapping_classes)

    # Rollups: parent property_id -> member listings consolidated into one statement
    rollups = cfg.get("statement_rollups") or {}
    rollup_children = {child for kids in rollups.values() for child in kids}

    # Only generate statements for listings present in Listing_contacts.csv.
    allowed = allowed_property_ids(conn, base_dir)

    # Fee-adjust guesty INCOME in the ledger FIRST (idempotent), so build_statements
    # computes totals from the same fee-adjusted net the Excel statements will show.
    guesty_csv = str(Path(args.csv)) if getattr(args, "csv", None) else str(base_dir / "data/guesty_converted.csv")
    _apply_guesty_fees(conn, period_start, period_end, guesty_csv)

    # Credit owner_pays_cleaning properties the guest-paid cleaning fee (non-commissioned),
    # so amount_due matches the dashboard. Must run before build_statements.
    _apply_owner_cleaning_credit(conn, period_start, period_end, guesty_csv, items)

    # For owner_pays_cleaning statements, the Excel 'Owner Cleaning Fee' column shows the
    # guest-paid Guesty cleaning fee (owner income) — same figure the dashboard shows and
    # the OWNER_ADJ credit folds into amount_due — rather than the QBO cleaning cost.
    cleaning_props = {m["property_id"] for m in items if m.get("owner_pays_cleaning")}
    guesty_clean_by_code = _read_guesty_cleaning_by_code(guesty_csv)

    # owner_pays_taxes properties: the engine folds the "Taxes Paid to Owners" pass-through
    # into amount_due ONLY for these, matching the dashboard/Excel per-booking Tax column.
    tax_props = {m["property_id"] for m in items if m.get("owner_pays_taxes")}

    run_id = create_run(conn, period_start, period_end, cfg["statement_policy"]["default_basis"])
    build_statements(conn, run_id, period_start, period_end,
                     default_reserve_target=float(cfg["statement_policy"]["default_reserve_target"]),
                     rollups=rollups, allowed=allowed, tax_props=tax_props)

    out_dir = Path(args.output_dir)/period/"statements"
    out_dir.mkdir(parents=True, exist_ok=True)

    props = conn.execute("""SELECT p.*, o.owner_name, o.owner_email
                              FROM properties p JOIN owners o ON p.owner_id=o.owner_id
                              WHERE p.is_active=1""").fetchall()
    generated = 0
    for p in props:
        pid = p["property_id"]

        # Only build listings present in Listing_contacts.csv (see listing_filter).
        if pid not in allowed:
            continue

        # Member listings of a rollup are folded into their parent's statement.
        if pid in rollup_children:
            continue
        member_ids = [pid] + list(rollups.get(pid, []))
        ph = ",".join("?" * len(member_ids))

        # PM fee rate — one shared resolver, no silent default. build_statements
        # (already run above) raises for any property with commissionable revenue but
        # no rate, so a None here can only be a zero-revenue property; 0.0 just renders
        # its (empty) commission column at 0%.
        pm_fee_rate = resolve_pm_fee_rate(conn, pid, period_start)
        if pm_fee_rate is None:
            pm_fee_rate = 0.0

        # Guesty bookings with fees and owner costs
        guesty_rows = conn.execute(f"""
            SELECT property_id as prop_id, source_txn_id as booking_id, vendor_customer as guest_name,
                   posting_date as checkin, service_date as checkout, amount as net_revenue,
                   subcategory as channel, status as booking_status
            FROM ledger_lines
            WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
              AND source='guesty' AND category='INCOME' AND include_in_statement=1
            ORDER BY posting_date""", (*member_ids, period_start, period_end)).fetchall()

        bookings = []
        for row in guesty_rows:
            booking_dict = dict(row)
            booking_pid = booking_dict.pop('prop_id')

            # Get QBO fees (channel + stripe + tax + deductions)
            qbo_fees = get_qbo_fees(conn, booking_pid, booking_dict['booking_id'], booking_dict['checkin'], booking_dict['checkout'], period_start, period_end)
            channel_fee = qbo_fees['channel_fee']
            stripe_fee = qbo_fees['stripe_fee']
            tax_fee = qbo_fees['tax']
            #channel_fee_deduction = qbo_fees['channel_fee_deduction']
            #stripe_fee_deduction = qbo_fees['stripe_fee_deduction']

            # If no channel fee found in QBO, calculate implied from converted Guesty CSV
            if channel_fee == 0.0:
                guesty_csv = Path(args.csv) if hasattr(args, 'csv') else Path(__file__).parent.parent / "data/guesty_converted.csv"
                implied_channel = _calculate_implied_channel_fee(booking_dict['booking_id'], str(guesty_csv))
                if implied_channel != 0.0:
                    channel_fee = implied_channel

            # Get owner costs (cleaning, tax). Pass dates so transient-occupancy tax
            # paid to the owner (matched by the stay's date range) is captured.
            owner_costs = get_owner_costs(conn, booking_pid, booking_dict['booking_id'], period_start, period_end,
                                          checkin=booking_dict['checkin'], checkout=booking_dict['checkout'])

            # net_revenue (from the SELECT) is already fee-adjusted in the ledger by
            # _apply_guesty_fees(), run before build_statements — so use it directly.
            # Do NOT re-subtract fees here, or a single build would double-deduct.
            # get_qbo_fees above is only needed for the channel/card-fee display column.
            booking_dict['total_channel_and_card_fees'] = abs(channel_fee) + abs(stripe_fee)
            if booking_pid in cleaning_props:
                # Show the guest-paid cleaning fee as owner income (0 for canceled stays).
                booking_dict['owner_cleaning_cost'] = (
                    0.0 if str(booking_dict.get('booking_status') or '').lower() == 'canceled'
                    else round(guesty_clean_by_code.get(str(booking_dict['booking_id']), 0.0), 2))
            else:
                booking_dict['owner_cleaning_cost'] = owner_costs['owner_cleaning_cost']
            booking_dict['owner_tax_cost'] = owner_costs['owner_tax_cost']

            bookings.append(booking_dict)

        # LTR rents + deferred bookings (from the LTR CSV) as Net Revenue lines —
        # one line per property (full monthly rent). PM commission is computed by
        # excel_writer from net_revenue * pm_fee_rate, like Guesty bookings.
        ltr_recs, ltr_covered = build_ltr_records(
            base_dir, period, member_ids,
            lambda code: conn.execute(
                "SELECT 1 FROM ledger_lines WHERE source='guesty' AND source_txn_id=? LIMIT 1",
                (code,)).fetchone() is not None)
        for rec in ltr_recs:
            bookings.append({
                "booking_id": rec["booking_id"],
                "guest_name": rec["guest_name"],
                "checkin": rec["checkin"],
                "checkout": rec["checkout"],
                "net_revenue": rec["net_revenue"],
                "total_channel_and_card_fees": 0.0,
                "owner_cleaning_cost": 0.0,
                "owner_tax_cost": 0.0,
            })

        # QBO deposit income (lease rent, credits, etc.). Rent for properties now
        # shown in the Net Revenue section is excluded so it isn't double-listed;
        # rent for properties WITHOUT a Net Revenue line stays here.
        other_income_rows = conn.execute(f"""
            SELECT posting_date, description, vendor_customer, subcategory, amount,
                   property_id, source_object
            FROM ledger_lines
            WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
              AND source='qbo' AND category='INCOME' AND include_in_statement=1
            ORDER BY posting_date""", (*member_ids, period_start, period_end)).fetchall()
        other_income = [
            oi for oi in other_income_rows
            if not (oi["property_id"] in ltr_covered
                    and is_rent_income(oi["source_object"], oi["description"]))
        ]

        # Owner-responsibility QBO expenses only (account contains 'Owner')
        expense_rows = conn.execute(f"""
            SELECT posting_date, description, vendor_customer, qbo_account, subcategory, amount
            FROM ledger_lines
            WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
              AND source='qbo' AND category='EXPENSE' AND include_in_statement=1
              AND qbo_account LIKE '%Owner Expenses%'
            ORDER BY subcategory, posting_date""", (*member_ids, period_start, period_end)).fetchall()

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
