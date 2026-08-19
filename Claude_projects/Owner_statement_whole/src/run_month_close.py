import argparse
from pathlib import Path
from datetime import date
import secrets
import calendar
import json
import uuid
import pandas as pd

from .common.config import load_config
from .common.db import connect, init_db
from .common.mappings import load_class_mapping, load_account_rules
from .expense.qbo_client import QBOClient
from .expense.qbo_sync import sync_qbo_expenses
from .breakdown.adapter import import_guesty_bookings_csv
from .netrevenue.engine import create_run, build_statements
from .scope.pm_rate import resolve_pm_fee_rate
from .reporting.excel_writer import create_template, write_statement
from .expense.booking_fees import get_qbo_fees, get_owner_costs
from .ltr.records import build_records as build_ltr_records, is_rent_income, ltr_claimed_codes
from .reporting.booking_breakdown import build_by_unit as build_breakdown_by_unit
from .scope.listing_filter import allowed_property_ids, central_supply_property_ids
from . import paths
from .common.utils import sha256_file, now_iso

# Central-supplies formula: $0.90 per guest per night, capped at 60 nights. Charged to
# 'central' Supplies properties per booking in place of QBO per-booking supply charges.
SUPPLY_RATE = 0.9
SUPPLY_MAX_NIGHTS = 60
SUPPLY_ACCOUNT = "Trust Liabilities:Owner Payables:1C - Owner Expenses:Supplies - Owner"


def supply_charge(guests, nights) -> float:
    return round(SUPPLY_RATE * int(guests or 0) * min(int(nights or 0), SUPPLY_MAX_NIGHTS), 2)

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


def _apply_central_supplies(conn, period_start, period_end, central_pids, guests_by_code):
    """'central' Supplies properties are charged a per-booking FORMULA supply fee
    (0.9 * guests * min(nights, 60)) instead of QBO per-booking supply charges.

    Steps (idempotent, re-run each build):
      1. Re-include, then SUPPRESS, the booking-related QBO 'Supplies Charge | …'
         lines for central properties. (These come as a double-entry pair — a
         Billable-Expense-Income side that never counted, and an Owner-Expenses side
         that did; suppressing both removes only the counted cost.) General /
         non-booking supply lines — anything not matching 'Supplies Charge | …' —
         are left untouched, so they still come from QBO.
      2. Insert one manual Supplies expense per CONFIRMED central-property Guesty
         booking, EXCLUDING the Yacinde 'old bookings' (source_object='YacindeOld'),
         which already carry their own supply from import_yacinde_old.

    Non-central properties are not touched. Returns (n_suppressed, n_added).
    """
    if not central_pids:
        return (0, 0)
    ph = ",".join("?" * len(central_pids))

    conn.execute("""DELETE FROM ledger_lines WHERE source_object='CentralSupply'
                    AND posting_date>=? AND posting_date<=?""", (period_start, period_end))

    # Re-include all booking supply lines in the period (so a property that leaves
    # 'central' gets its QBO supplies back), then suppress them for central properties.
    conn.execute("""UPDATE ledger_lines SET include_in_statement=1
                    WHERE source='qbo' AND category='EXPENSE' AND subcategory='Supplies'
                      AND description LIKE 'Supplies Charge |%'
                      AND posting_date>=? AND posting_date<=?""", (period_start, period_end))
    n_suppressed = conn.execute(f"""UPDATE ledger_lines SET include_in_statement=0
                    WHERE property_id IN ({ph})
                      AND source='qbo' AND category='EXPENSE' AND subcategory='Supplies'
                      AND description LIKE 'Supplies Charge |%'
                      AND posting_date>=? AND posting_date<=?""",
                 (*central_pids, period_start, period_end)).rowcount

    rows = conn.execute(f"""SELECT source_txn_id, vendor_customer, property_id,
                                   posting_date, service_date, status
                            FROM ledger_lines
                            WHERE property_id IN ({ph})
                              AND source='guesty' AND category='INCOME'
                              AND (source_object IS NULL OR source_object NOT IN ('YacindeOld','OsbrRV'))
                              AND posting_date>=? AND posting_date<=?""",
                        (*central_pids, period_start, period_end)).fetchall()
    n_added = missing = 0
    ts = now_iso()
    for code, guest, pid, checkin, checkout, status in rows:
        if str(status or "").lower() == "canceled":
            continue
        guests = guests_by_code.get(str(code))
        if not guests:
            missing += 1
            continue
        try:
            nights = (date.fromisoformat(str(checkout)[:10]) - date.fromisoformat(str(checkin)[:10])).days
        except (ValueError, TypeError):
            nights = 0
        if nights <= 0:
            continue
        amt = supply_charge(guests, nights)
        capped = " (capped at 60)" if nights > SUPPLY_MAX_NIGHTS else ""
        conn.execute(
            """INSERT INTO ledger_lines
               (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                booking_id, posting_date, service_date, category, subcategory, description,
                vendor_customer, qbo_account, amount, currency, include_in_statement, status,
                last_updated_at, created_at)
               VALUES (?, 'manual', 'CentralSupply', ?, NULL, ?, ?, ?, ?, 'EXPENSE', 'Supplies', ?, ?, ?, ?, 'USD', 1, 'posted', ?, ?)""",
            (str(uuid.uuid4()), f"{code}_csupply", pid, code, checkin, checkout,
             f"Supplies ({guests} guests x {min(int(nights), SUPPLY_MAX_NIGHTS)} nights x $0.90{capped}) - {guest} {code}",
             guest, SUPPLY_ACCOUNT, -amt, ts, ts),
        )
        n_added += 1
    conn.commit()
    if missing:
        print(f"  central supplies: {missing} bookings had no guest count in the converted CSV (skipped)")
    return (n_suppressed, n_added)


def _exclude_duplicate_guesty_deposits(conn, period_start: str, period_end: str) -> int:
    """A channel booking (Airbnb/Booking.com/VRBO) sometimes lands in QBO as a
    Deposit AND as a Guesty booking, double-counting the revenue. Guesty is the
    canonical STR revenue source (channel-specific net, properly commissioned), so
    any QBO INCOME Deposit whose description carries a confirmation code matching a
    Guesty booking in the period is marked include_in_statement=0 — Guesty wins.

    Idempotent and re-applied each build, so a later `qbo-sync` (which re-inserts
    the deposit as include_in_statement=1) is re-corrected on the next build. Only
    flips matches to 0; never re-includes, so it can't clobber other exclusions.
    """
    codes = [r[0] for r in conn.execute(
        """SELECT DISTINCT source_txn_id FROM ledger_lines
           WHERE source='guesty' AND category='INCOME'
             AND posting_date>=? AND posting_date<=? AND source_txn_id IS NOT NULL""",
        (period_start, period_end)).fetchall()]
    if not codes:
        return 0
    deposits = conn.execute(
        """SELECT ledger_id, property_id, amount, description FROM ledger_lines
           WHERE source='qbo' AND category='INCOME' AND source_object='Deposit'
             AND include_in_statement=1
             AND posting_date>=? AND posting_date<=?""",
        (period_start, period_end)).fetchall()
    n = 0
    for lid, pid, amount, desc in deposits:
        d = str(desc or "")
        hit = next((c for c in codes if c in d), None)
        if hit:
            conn.execute("UPDATE ledger_lines SET include_in_statement=0 WHERE ledger_id=?", (lid,))
            n += 1
            print(f"  excluded duplicate deposit: {pid} ${float(amount or 0):,.2f} (matches Guesty {hit})")
    conn.commit()
    return n


def _offset_refunded_deposits(conn, period_start: str, period_end: str) -> int:
    """Fully-refunded non-Guesty channel bookings: a QBO Deposit (shown in 'Other
    Credits') that was later refunded via a `…1A - Net Earnings:Resolutions` purchase.

    The deposit is counted as owner income but the refund posts to a Resolutions
    account that no statement section pulls (expenses only include `1C - Owner
    Expenses`), so the credit stands unoffset and overstates the payout. This pairs
    each such refund with its Other-Credits deposit (same property, equal magnitude,
    shared confirmation-code token) and **suppresses both** so the pair nets to $0.

    Scope is deliberately narrow (per owner decision): ONLY refunds matching a
    non-Guesty Other-Credits deposit. Refunds tied to Guesty bookings are left alone
    (Guesty net is the revenue basis; deducting them could double-count). Both rows
    must fall in the period so each build stays self-contained. Idempotent and
    re-applied each build, so a later `qbo-sync` (which re-includes them) is
    re-corrected. Returns the number of deposit/refund pairs offset.
    """
    import re

    def _code_tokens(s):
        return set(t for t in re.findall(r'[A-Za-z0-9\-]{6,}', str(s or ''))
                   if any(ch.isdigit() for ch in t))

    refunds = conn.execute(
        """SELECT ledger_id, property_id, description, amount FROM ledger_lines
           WHERE qbo_account LIKE '%Resolutions%' AND category='EXPENSE' AND amount < 0
             AND posting_date>=? AND posting_date<=?""",
        (period_start, period_end)).fetchall()
    deposits = conn.execute(
        """SELECT ledger_id, property_id, description, amount FROM ledger_lines
           WHERE source='qbo' AND category='INCOME' AND source_object='Deposit'
             AND posting_date>=? AND posting_date<=?""",
        (period_start, period_end)).fetchall()

    n = 0
    used_deposits = set()
    for r in refunds:
        rtoks = _code_tokens(r["description"])
        if not rtoks:
            continue
        for d in deposits:
            if d["ledger_id"] in used_deposits or d["property_id"] != r["property_id"]:
                continue
            if round(abs(d["amount"]), 2) != round(abs(r["amount"]), 2):
                continue
            if rtoks & _code_tokens(d["description"]):
                conn.execute("UPDATE ledger_lines SET include_in_statement=0 WHERE ledger_id IN (?,?)",
                             (d["ledger_id"], r["ledger_id"]))
                used_deposits.add(d["ledger_id"])
                n += 1
                print(f"  offset refunded deposit: {r['property_id']} ${abs(r['amount']):,.2f} (deposit + refund netted to $0)")
                break
    conn.commit()
    return n


def _apply_osbr_rv(conn, period_start: str, period_end: str) -> int:
    """OSBR 'RV' income: Hipcamp.com credits posted to OSBR are RV-site bookings.

    They arrive as plain QBO INCOME Deposits (property_id='osbr') and would otherwise
    sit uncommissioned in 'Other Credits'. This moves each into the Net Revenue section
    as an 'osbr_rv' sub-listing so it is commissioned at OSBR's PM rate:
      1. Ensure the 'osbr_rv' display property exists (labelled 'OSBR-RV').
      2. Delete prior OsbrRV net-revenue lines (idempotent).
      3. For each OSBR Hipcamp.com QBO INCOME deposit: suppress it (drop from Other
         Credits) and insert a source='guesty' INCOME 'booking' — gross = net = the
         credit amount, reservation date = the credit's posting_date.
    'osbr_rv' is a member of the 'osbr' rollup (config.yml), so it renders as a
    sub-section of the OSBR statement and commissions at OSBR's parent rate. Idempotent
    and re-applied each build, so a later `qbo-sync` (which re-inserts the deposit as
    include_in_statement=1) is re-corrected on the next build. Returns bookings created.
    """
    # 1. Display property (reuse OSBR's owner + class; is_active=0 keeps it out of the
    #    dashboard property dropdown — it's a sub-listing, not a standalone statement).
    conn.execute("""INSERT OR IGNORE INTO properties
                    (property_id, property_name, owner_id, qbo_class_id, qbo_class_name, is_active)
                    SELECT 'osbr_rv', 'OSBR-RV', owner_id, qbo_class_id, qbo_class_name, 0
                    FROM properties WHERE property_id='osbr'""")

    # 2. Idempotent: clear prior RV net-revenue lines for the period.
    conn.execute("""DELETE FROM ledger_lines WHERE source_object='OsbrRV'
                    AND posting_date>=? AND posting_date<=?""", (period_start, period_end))

    # 3. Convert each OSBR Hipcamp.com credit into an RV booking.
    deposits = conn.execute(
        """SELECT ledger_id, posting_date, amount FROM ledger_lines
           WHERE property_id='osbr' AND source='qbo' AND category='INCOME'
             AND vendor_customer LIKE '%Hipcamp%'
             AND posting_date>=? AND posting_date<=?
           ORDER BY posting_date""",
        (period_start, period_end)).fetchall()
    ts = now_iso()
    n = 0
    for lid, pdate, amount in deposits:
        conn.execute("UPDATE ledger_lines SET include_in_statement=0 WHERE ledger_id=?", (lid,))
        amt = round(float(amount or 0), 2)
        code = f"HIPCAMP-{pdate}" if n == 0 else f"HIPCAMP-{pdate}-{n}"
        conn.execute(
            """INSERT INTO ledger_lines
               (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                booking_id, posting_date, service_date, category, subcategory, description,
                vendor_customer, qbo_account, amount, base_amount, currency,
                include_in_statement, status, last_updated_at, created_at)
               VALUES (?, 'guesty', 'OsbrRV', ?, NULL, 'osbr_rv',
                       ?, ?, ?, 'INCOME', 'Hipcamp', 'RV Booking',
                       'Hipcamp', NULL, ?, ?, 'USD', 1, 'confirmed', ?, ?)""",
            (str(uuid.uuid4()), code, code, pdate, pdate, amt, amt, ts, ts))
        n += 1
    conn.commit()
    return n

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
        # Persist the full QBO object only for errors. MISSING_CLASS *warnings* are
        # high-volume (99%+ of rows) and already self-identified by source_txn_id, so
        # dumping their ~3.5KB JSON each ballooned this diagnostic table to ~34MB for
        # no triage gain — the txn id is enough to look the row up in QBO.
        payload = json.dumps(obj)[:20000] if sev == "error" else None
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
             sev, code, msg, payload),
        )
        conn.commit()
    return _log

def cmd_init_db(args):
    cfg = load_config(args.config)
    conn = connect(str(paths.DB_PATH))
    init_db(conn, args.schema)
    print(f"DB initialized at {cfg['app']['db_path']}")

    tpath = Path(args.template)
    if not tpath.exists():
        tpath.parent.mkdir(parents=True, exist_ok=True)
        create_template(str(tpath))
        print(f"Template created at {tpath}")

def cmd_sync_mappings(args):
    cfg = load_config(args.config)
    conn = connect(str(paths.DB_PATH))
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
    conn = connect(str(paths.DB_PATH))

    items = load_class_mapping(args.mapping_classes)
    upsert_from_mapping(conn, items)

    property_map = {m["qbo_class_id"]: m["property_id"] for m in items if m.get("qbo_class_id") and m.get("property_id")}
    acct_rules = load_account_rules(args.mapping_accounts)

    q = cfg["qbo"]
    qbo = QBOClient(realm_id=q["realm_id"], base_url=q["base_url"], minorversion=int(q.get("minorversion",75)))

    # `exceptions` is a per-sync diagnostic scratch table — nothing in the pipeline
    # reads it (only ad-hoc onetime/ scripts do). Clear it before each sync so rows
    # don't accumulate across every run (they piled up to ~7.9k rows / 34MB before
    # this). Reviewed right after the sync that produced them.
    conn.execute("DELETE FROM exceptions")
    conn.commit()

    exc_cb = exception_logger(conn)
    sync_qbo_expenses(conn, qbo, property_map, acct_rules, args.start, args.end, exceptions_cb=exc_cb)
    print("QBO sync complete.")

def cmd_guesty_import(args):
    cfg = load_config(args.config)
    conn = connect(str(paths.DB_PATH))
    import_guesty_bookings_csv(conn, args.csv)
    print("Guesty bookings imported.")

def _apply_payment_breakdown_net(conn, period_start, period_end, pb_csv_path):
    """Override each STR (guesty) booking's ledger net with the payment_breakdown
    net_revenue — the sheet-faithful Guesty-basis net (see breakdown/payment_model).
    Matched by confirmation code (source_txn_id). Bookings ABSENT from the breakdown
    (deactivated listings the API drops; $0/comp rows the breakdown drops) keep their
    existing DB net as a fallback.

    This makes the payment-breakdown net the single STR commission basis: statement_engine
    (stored amount_due), the dashboard, Excel, and PDF all read `amount`, so overriding it
    here propagates to every product at once. Idempotent: overwrites `amount` only
    (base_amount stays the immutable Guesty base) and re-derives from the CSV each build.
    MUST run AFTER _apply_guesty_fees (so it wins over the QBO-fee-adjusted amount) and
    BEFORE build_statements.
    """
    p = Path(pb_csv_path)
    if not p.exists():
        print(f"  payment-breakdown net: {p.name} not found — STR net keeps its DB basis")
        return 0
    df = pd.read_csv(p)
    net_by_code = {str(r["confirmationCode"]): round(float(r["net_revenue"]), 2)
                   for _, r in df.iterrows()
                   if pd.notna(r.get("confirmationCode")) and pd.notna(r.get("net_revenue"))}
    rows = conn.execute(
        """SELECT ledger_id, source_txn_id FROM ledger_lines
           WHERE posting_date>=? AND posting_date<=?
             AND source='guesty' AND category='INCOME'""",
        (period_start, period_end)).fetchall()
    n = 0
    for r in rows:
        if str(r["source_txn_id"]) in net_by_code:
            conn.execute("UPDATE ledger_lines SET amount=? WHERE ledger_id=?",
                         (net_by_code[str(r["source_txn_id"])], r["ledger_id"]))
            n += 1
    conn.commit()
    print(f"  payment-breakdown net: overrode {n} STR booking net(s); "
          f"{len(rows) - n} kept DB net (fallback).")
    return n

def cmd_build(args):
    cfg = load_config(args.config)
    conn = connect(str(paths.DB_PATH))
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

    # property_id -> name, for per-listing Net Revenue sub-sections + expense 'Type' labels.
    property_names = {r["property_id"]: r["property_name"]
                      for r in conn.execute("SELECT property_id, property_name FROM properties")}

    # OSBR Hipcamp.com credits → 'osbr_rv' Net Revenue bookings (commissioned at OSBR's
    # rate), out of Other Credits. Runs before _apply_guesty_fees so the RV rows are
    # fee-adjusted (a no-op: no QBO fees) from their base like any guesty booking.
    n_rv = _apply_osbr_rv(conn, period_start, period_end)
    if n_rv:
        print(f"OSBR-RV: {n_rv} Hipcamp booking(s) moved to Net Revenue (commissioned).")

    # Fee-adjust guesty INCOME in the ledger FIRST (idempotent), so build_statements
    # computes totals from the same fee-adjusted net the Excel statements will show.
    guesty_csv = str(Path(args.csv)) if getattr(args, "csv", None) else str(paths.guesty_converted_csv(period))
    _apply_guesty_fees(conn, period_start, period_end, guesty_csv)

    # Override STR net with the payment_breakdown (sheet-faithful) net where available.
    # This is the single STR commission basis — statement_engine/dashboard/Excel/PDF all
    # read `amount`. Bookings not in the breakdown keep their DB net (fallback).
    _apply_payment_breakdown_net(conn, period_start, period_end,
                                 str(paths.payment_breakdown_csv(period)))

    # Credit owner_pays_cleaning properties the guest-paid cleaning fee (non-commissioned),
    # so amount_due matches the dashboard. Must run before build_statements.
    _apply_owner_cleaning_credit(conn, period_start, period_end, guesty_csv, items)

    # Drop QBO Deposits that duplicate a Guesty booking (channel payout recorded in
    # both) — Guesty is the canonical STR revenue source. Runs before build_statements.
    n_dup = _exclude_duplicate_guesty_deposits(conn, period_start, period_end)
    if n_dup:
        print(f"Excluded {n_dup} duplicate channel-booking deposit(s) (Guesty kept).")

    # Fully-refunded non-Guesty channel bookings: suppress the Other-Credits deposit and
    # its matching Resolutions refund so the pair nets to $0 (owner decision: offset only
    # Other-Credits matches; Guesty-linked refunds are left untouched to avoid double-count).
    n_ref = _offset_refunded_deposits(conn, period_start, period_end)
    if n_ref:
        print(f"Offset {n_ref} refunded deposit/refund pair(s) to $0.")

    # 'central' Supplies properties: charge a per-booking formula supply fee
    # (0.9*guests*min(nights,60)) and suppress the QBO per-booking supply charges.
    central_pids = central_supply_property_ids(conn, base_dir)
    guests_by_code = {}
    try:
        _gdf = pd.read_csv(guesty_csv)
        guests_by_code = {str(r["booking_id"]): int(r["guests"]) for _, r in _gdf.iterrows()
                          if pd.notna(r.get("booking_id")) and pd.notna(r.get("guests"))}
    except (FileNotFoundError, KeyError):
        pass
    n_supp, n_csup = _apply_central_supplies(conn, period_start, period_end, central_pids, guests_by_code)
    if n_csup or n_supp:
        print(f"Central supplies: {n_csup} formula charges added, {n_supp} QBO booking-supply lines suppressed.")

    # For owner_pays_cleaning statements, the Excel 'Owner Cleaning Fee' column shows the
    # guest-paid Guesty cleaning fee (owner income) — same figure the dashboard shows and
    # the OWNER_ADJ credit folds into amount_due — rather than the QBO cleaning cost.
    cleaning_props = {m["property_id"] for m in items if m.get("owner_pays_cleaning")}
    guesty_clean_by_code = _read_guesty_cleaning_by_code(guesty_csv)

    # Section-1 Booking Breakdown source (SHARED with dashboard/PDF via
    # reporting.booking_breakdown): the Guesty payment-breakdown CSV + the dedup set of
    # LTR-claimed codes (bookings LTR/DEFERRED superseded). Built per statement in the
    # loop below and rendered above each unit's Net Revenue table.
    try:
        _breakdown_pb_df = pd.read_csv(paths.payment_breakdown_csv(period))
    except FileNotFoundError:
        _breakdown_pb_df = None
    _breakdown_claimed = ltr_claimed_codes(conn, period)

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
            booking_dict['property_id'] = booking_pid  # kept for per-listing grouping

            # Get QBO fees (channel + stripe + tax + deductions)
            qbo_fees = get_qbo_fees(conn, booking_pid, booking_dict['booking_id'], booking_dict['checkin'], booking_dict['checkout'], period_start, period_end)
            channel_fee = qbo_fees['channel_fee']
            stripe_fee = qbo_fees['stripe_fee']
            tax_fee = qbo_fees['tax']
            #channel_fee_deduction = qbo_fees['channel_fee_deduction']
            #stripe_fee_deduction = qbo_fees['stripe_fee_deduction']

            # If no channel fee found in QBO, calculate implied from converted Guesty CSV
            if channel_fee == 0.0:
                guesty_csv = Path(args.csv) if getattr(args, 'csv', None) else paths.guesty_converted_csv(period)
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
                "property_id": rec["property_id"],
                "booking_id": rec["booking_id"],
                "guest_name": rec["guest_name"],
                "checkin": rec["checkin"],
                "checkout": rec["checkout"],
                "net_revenue": rec["net_revenue"],
                "total_channel_and_card_fees": 0.0,
                # LTR cleaning fee (non-commissioned owner income). Shown/credited in the
                # per-booking table only for owner_pays_cleaning statements (excel_writer
                # gate); the authoritative payout picks it up regardless via the
                # LtrCleaningCredit OWNER_ADJ folded into amount_due (see import_ltr).
                "owner_cleaning_cost": rec.get("cleaning_fee", 0.0),
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
            SELECT property_id, posting_date, description, vendor_customer, qbo_account, subcategory, amount
            FROM ledger_lines
            WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
              AND source IN ('qbo','manual') AND category='EXPENSE' AND include_in_statement=1
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

        # Section-1 Booking Breakdown for this statement (Guesty fee waterfall + LTR/
        # deferred, deduped) — shared builder, so it matches the dashboard/PDF exactly.
        bd_by_unit, bd_grand = build_breakdown_by_unit(
            _breakdown_pb_df, ltr_recs, member_ids, _breakdown_claimed)

        out_path = out_dir / f"{pid}_owner_statement_{period}.xlsx"
        write_statement(
            str(out_path), period,
            {"property_id": pid, "property_name": p["property_name"]},
            {"owner_name": p["owner_name"], "owner_email": p["owner_email"] or ""},
            pm_fee_rate, bookings, other_income, expenses_by_subcat, totals,
            owner_pays_cleaning=owner_pays_cleaning,
            owner_pays_supplies=owner_pays_supplies,
            owner_pays_taxes=owner_pays_taxes,
            property_names=property_names,
            multi_listing=len(member_ids) > 1,
            booking_breakdown=bd_by_unit,
            booking_breakdown_grand=bd_grand,
        )

        sha = sha256_file(str(out_path))
        conn.execute("""INSERT OR REPLACE INTO statement_outputs(run_id, property_id, output_path, output_sha256)
                        VALUES (?, ?, ?, ?)""", (run_id, pid, str(out_path), sha))
        conn.commit()
        generated += 1

    print(f"Statements generated for {period}: {generated} files → {out_dir}")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--config", default=str(paths.CONFIG_YML))
    ap.add_argument("--schema", default=str(paths.SCHEMA_SQL))
    ap.add_argument("--mapping-classes", default=str(paths.MAPPING_CLASSES))
    ap.add_argument("--mapping-accounts", default=str(paths.MAPPING_ACCOUNTS))
    ap.add_argument("--output-dir", default=str(paths.OUTPUT_DIR))

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
