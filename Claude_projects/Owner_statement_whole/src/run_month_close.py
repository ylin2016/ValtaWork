import argparse
from pathlib import Path
from datetime import date, datetime, timedelta
import calendar
import json
import re
import uuid
import pandas as pd

from .common.config import load_config
from .common.db import connect, init_db
from .common.mappings import load_class_mapping, load_account_rules
from .common.income_rules import NOT_OWNER_PRED
from .expense.qbo_client import QBOClient
from .expense.qbo_sync import sync_qbo_expenses
from .breakdown.adapter import import_guesty_bookings_csv
from .netrevenue.engine import create_run, build_statements
from .scope.pm_rate import owner_pays_cleaning_at, pm_rate_resolver, resolve_pm_fee_rate
from .reporting.excel_writer import create_template, write_statement
from .expense.owner_costs import get_owner_costs
from .ltr.records import is_rent_income
from .reporting.qbo_adjustments import adjusted_nets, unrated_adjustments
from .reporting.period_sources import PeriodSources
from .reporting.booking_breakdown import (DROPPED_CANCELLATION_MAX_INVOICE,
                                          build_by_unit as build_breakdown_by_unit,
                                          dropped_cancellation_codes,
                                          net_revenue_rows as build_net_revenue_rows)
from .reporting.other_income import ledger_keys as other_ledger_keys
from .scope.listing_filter import (allowed_property_ids, central_supply_property_ids,
                                   statement_rollups)
from . import paths
from .common.utils import sha256_file, now_iso

# Central-supplies formula: $0.90 per guest per night, capped at 60 nights. Charged to
# 'central' Supplies properties per booking in place of QBO per-booking supply charges.
SUPPLY_RATE = 0.9
SUPPLY_MAX_NIGHTS = 60
SUPPLY_ACCOUNT = "Trust Liabilities:Owner Payables:1C - Owner Expenses:Supplies - Owner"


def supply_charge(guests, nights) -> float:
    return round(SUPPLY_RATE * int(guests or 0) * min(int(nights or 0), SUPPLY_MAX_NIGHTS), 2)

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
      1. Re-include every booking-related QBO 'Supplies Charge … | …' line in the
         period (so a property that leaves 'central' gets its QBO supplies back).
         (These come as a double-entry pair — a Billable-Expense-Income side that
         never counted, and an Owner-Expenses side that did; suppressing both removes
         only the counted cost.) General / non-booking supply lines — anything without
         the 'Supplies Charge … | …' shape — are left untouched.
      2. Insert one manual Supplies expense per CONFIRMED central-property Guesty
         booking, EXCLUDING the Yacinde 'old bookings' (source_object='YacindeOld'),
         which already carry their own supply from import_yacinde_old.
      3. SUPPRESS the QBO lines ONLY for properties that actually received a formula
         charge in step 2. This is a SUBSTITUTION, not two blanket operations: a
         central property with QBO supply lines but no Guesty booking that month
         (an owner/timeshare stay, or a listing whose reservations never reached the
         API) would otherwise have its real supply cost suppressed with nothing put
         back, silently deleting it from the statement — that is how yacinde_f5 lost
         $43.20 and seattle_1424c $83.70/$62.10 in 2026-06/07.

    The description match is whitespace-tolerant ('Supplies Charge%|%'): QBO writes
    both 'Supplies Charge | …' and 'Supplies Charge  | …' (two spaces), and matching
    only the one-space form let the two-space rows escape suppression — yacinde_b1
    then carried BOTH its QBO supply cost and the formula charge for the same stay.

    Non-central properties are not touched. Returns (n_suppressed, n_added).
    """
    if not central_pids:
        return (0, 0)
    ph = ",".join("?" * len(central_pids))

    conn.execute("""DELETE FROM ledger_lines WHERE source_object='CentralSupply'
                    AND posting_date>=? AND posting_date<=?""", (period_start, period_end))

    # Re-include all booking supply lines in the period; step 3 below suppresses them
    # again, but only for the properties a formula charge actually replaces.
    conn.execute("""UPDATE ledger_lines SET include_in_statement=1
                    WHERE source='qbo' AND category='EXPENSE' AND subcategory='Supplies'
                      AND description LIKE 'Supplies Charge%|%'
                      AND posting_date>=? AND posting_date<=?""", (period_start, period_end))

    rows = conn.execute(f"""SELECT source_txn_id, vendor_customer, property_id,
                                   posting_date, service_date, status
                            FROM ledger_lines
                            WHERE property_id IN ({ph})
                              AND source='guesty' AND category='INCOME'
                              AND (source_object IS NULL OR source_object NOT IN ('YacindeOld','OsbrRV'))
                              AND posting_date>=? AND posting_date<=?""",
                        (*central_pids, period_start, period_end)).fetchall()
    n_added = missing = 0
    charged_pids = set()
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
        charged_pids.add(pid)

    # Step 3 — substitute, don't just delete: suppress the QBO booking-supply lines
    # only where a formula charge took their place.
    n_suppressed = 0
    if charged_pids:
        cph = ",".join("?" * len(charged_pids))
        n_suppressed = conn.execute(f"""UPDATE ledger_lines SET include_in_statement=0
                        WHERE property_id IN ({cph})
                          AND source='qbo' AND category='EXPENSE' AND subcategory='Supplies'
                          AND description LIKE 'Supplies Charge%|%'
                          AND posting_date>=? AND posting_date<=?""",
                     (*sorted(charged_pids), period_start, period_end)).rowcount

    # Central properties whose QBO supply cost is being kept because nothing replaced
    # it — worth seeing, since it usually means the stay never reached Guesty.
    kept = conn.execute(f"""SELECT DISTINCT property_id FROM ledger_lines
                            WHERE property_id IN ({ph})
                              AND source='qbo' AND category='EXPENSE' AND subcategory='Supplies'
                              AND description LIKE 'Supplies Charge%|%' AND include_in_statement=1
                              AND qbo_account LIKE '%Owner Expenses%'
                              AND posting_date>=? AND posting_date<=?""",
                        (*central_pids, period_start, period_end)).fetchall()
    conn.commit()
    if missing:
        print(f"  central supplies: {missing} bookings had no guest count in the converted CSV (skipped)")
    if kept:
        print(f"  central supplies: kept QBO supply cost for {len(kept)} property(ies) with no "
              f"Guesty booking to charge the formula on: {', '.join(sorted(r[0] for r in kept))}")
    return (n_suppressed, n_added)


def _exclude_duplicate_guesty_deposits(conn, period_start: str, period_end: str) -> int:
    """A channel booking (Airbnb/Booking.com/VRBO) sometimes lands in QBO as a
    Deposit AND as a Guesty booking, double-counting the revenue. Guesty is the
    canonical STR revenue source (channel-specific net, properly commissioned), so
    any QBO INCOME Deposit that RE-POSTS a Guesty booking in the period is marked
    include_in_statement=0 — Guesty wins.

    **Naming the booking is not the same as duplicating it.** A pet fee, an extra-guest
    fee, a damage charge or an extended night is EXTRA money for a stay, and the
    bookkeeper writes the booking's code into the deposit so it can be traced back — so
    a code match alone over-excludes and silently deletes that money from the payout.
    The discriminator is how the description is written, and across all 20 months it
    splits the two cases perfectly (22 code-matching deposits: 14 / 8):

      duplicate  description is EXACTLY the `<channel> - <guest> - <code>` customer
                 string QBO generated — nothing was added because there is nothing
                 extra to say (e.g. "airbnb - Sean Flinn - HMAM8B3N2C").
      add-on     the description carries extra words the bookkeeper typed: "pet fee",
                 "extended night", "extra guest fee", "damaged fee from … (broken
                 frame)". Kept, and `other_income._merge_addon` folds it into that
                 booking's Section-1 row.

    Amount is NOT a usable discriminator: genuine duplicates run 0.64x-1.39x the Guesty
    net and add-ons 0.03x-1.30x, so the ranges overlap.

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
        """SELECT ledger_id, property_id, amount, description, vendor_customer FROM ledger_lines
           WHERE source='qbo' AND category='INCOME' AND source_object='Deposit'
             AND include_in_statement=1
             AND posting_date>=? AND posting_date<=?""",
        (period_start, period_end)).fetchall()
    n = 0
    for lid, pid, amount, desc, vc in deposits:
        d = str(desc or "")
        hit = next((c for c in codes if c in d), None)
        if not hit:
            continue
        if " ".join(d.split()).casefold() != " ".join(str(vc or "").split()).casefold():
            print(f"  kept annotated deposit: {pid} ${float(amount or 0):,.2f} "
                  f"(names Guesty {hit} but is an add-on: {d[:60]})")
            continue
        conn.execute("UPDATE ledger_lines SET include_in_statement=0 WHERE ledger_id=?", (lid,))
        n += 1
        print(f"  excluded duplicate deposit: {pid} ${float(amount or 0):,.2f} (matches Guesty {hit})")
    conn.commit()
    return n


def _apply_addon_channel_fees(conn, sources) -> int:
    """Charge the channel's commission on an add-on billed AFTER the booking.

    A pet fee collected once the guest has checked out arrives as its own QBO income line
    (`…_BC-7JjOq7O4r_Ann Smith pet fee`) rather than inside the Guesty invoice, so the
    channel fee computed on InvoiceItem never touched it. The channel still takes its cut
    of that money (owner, 2026-09-03), so it has to come out here.

    It must be a LEDGER change, not a Section-1 display one: gross revenue, the commission
    base and the payout all read `amount`, so deducting it only in the rendered table would
    make Section 1 disagree with the stored totals by exactly the fee.

    `base_amount` holds the gross and is written ONCE, so re-running recomputes from the
    same number instead of compounding — the same guarantee `_apply_payment_breakdown_net`
    relies on. The rate comes from the period's payment-breakdown CSV (`channel_fee_rate`),
    which is also what `booking_breakdown._merge_addon` uses to render the fee, so the two
    cannot drift apart.
    """
    pb = sources.pb
    if pb is None or not len(pb) or "channel_fee_rate" not in pb.columns:
        return 0
    # NaN where the row predates the column (the deactivated-listing rows merged in from
    # an older snapshot). `NaN or 0.0` keeps the NaN, and every later comparison against
    # it is False — so it would slip past `rate <= 0` and make the whole update NULL.
    def _rate(v):
        try:
            f = float(v)
        except (TypeError, ValueError):
            return 0.0
        return 0.0 if f != f else f

    rate_by_code = {str(r["confirmationCode"]): _rate(r["channel_fee_rate"])
                    for _, r in pb.iterrows()}
    n = 0
    # Drive off the RESOLVED source-#3 records rather than re-matching here: they already
    # know which booking each add-on belongs to, including the ones that needed the
    # (property, guest) fallback because neither the vendor id nor the note carries the
    # code ("John Gee pet fee cottage 3"). Two matchers would drift the moment one is
    # taught something the other is not.
    for rec in sources.other:
        code, lid = str(rec.get("code") or ""), rec.get("ledger_id")
        if not lid or code not in rate_by_code:
            continue
        rate = rate_by_code[code]
        if rate <= 0:
            continue
        row = conn.execute("SELECT amount, base_amount FROM ledger_lines WHERE ledger_id=?",
                           (lid,)).fetchone()
        if row is None:
            continue
        amt, base = row[0], row[1]
        try:
            gross = round(float(base if base is not None else amt), 2)
        except (TypeError, ValueError):
            continue
        # A non-finite gross would round to NaN, which SQLite stores as NULL and the
        # NOT NULL constraint on `amount` then rejects — skip rather than corrupt a row.
        if gross != gross:
            continue
        cf = round(gross * rate, 2)
        conn.execute("UPDATE ledger_lines SET base_amount=?, amount=? WHERE ledger_id=?",
                     (gross, round(gross - cf, 2), lid))
        n += 1
        print(f"  add-on channel fee: {rec['property_id']} {code} "
              f"${gross:,.2f} - ${cf:,.2f} = ${gross - cf:,.2f}")
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

    cleaning_props = {m["property_id"] for m in mapping_items
                      if owner_pays_cleaning_at(m, period_start)}
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
        # Write pm_fee_rate into owner_contracts if present in mapping.
        #
        # `pm_fee_rate` is the rate that has applied since the beginning; an optional
        # `pm_fee_rate_history` list adds later dated changes:
        #     pm_fee_rate: 0.16
        #     pm_fee_rate_history:
        #       - from: '2026-08-01'
        #         rate: 0.18
        # A rate change is almost never retroactive, and `owner_contracts` already carries
        # effective_start/effective_end — so the periods are REBUILT from the mapping each
        # sync rather than the old behaviour of UPDATE-ing whichever single row came back
        # first, which silently restated every past month at the new rate (and would have
        # corrupted one of two rows once a second existed).
        if m.get("pm_fee_rate") is not None:
            pid = m["property_id"]
            spans = [("2020-01-01", float(m["pm_fee_rate"]))]
            for h in (m.get("pm_fee_rate_history") or []):
                spans.append((str(h["from"]), float(h["rate"])))
            spans.sort(key=lambda s: s[0])
            cur.execute("DELETE FROM owner_contracts WHERE property_id=?", (pid,))
            for i, (start, rate) in enumerate(spans):
                end = None
                if i + 1 < len(spans):                       # ends the day before the next starts
                    nxt = datetime.strptime(spans[i + 1][0], "%Y-%m-%d").date()
                    end = str(nxt - timedelta(days=1))
                cur.execute("""INSERT INTO owner_contracts
                               (contract_id, property_id, effective_start, effective_end,
                                statement_basis, pm_fee_type, pm_fee_rate, pm_fee_base, reserve_target)
                               VALUES (?, ?, ?, ?, 'cash', 'percent', ?, 'net_booking_revenue', 0)""",
                            (str(uuid.uuid4()), pid, start, end, rate))
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
    conn = connect(str(paths.DB_PATH))
    items = load_class_mapping(args.mapping_classes)
    upsert_from_mapping(conn, items)
    print("Mappings synced into DB.")

def cmd_qbo_auth(args):
    """The OAuth flow moved to QBO_operations, which owns the token store."""
    raise SystemExit(
        "QuickBooks OAuth now lives in the QBO_operations project, which owns the\n"
        "token store both projects share (Intuit rotates the refresh token, so there\n"
        "can only be one copy). Run it there:\n\n"
        "    cd ../QBO_operations\n"
        "    python -m src.auth url\n"
        "    python -m src.auth exchange --code <CODE_FROM_REDIRECT_URL>\n")

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
    conn = connect(str(paths.DB_PATH))
    import_guesty_bookings_csv(conn, args.csv)
    print("Guesty bookings imported.")

def _apply_payment_breakdown_net(conn, period_start, period_end, pb_csv_path):
    """Set every STR (guesty) booking's ledger net from the payment breakdown.

    Owner decision 2026-08-29: the payment breakdown IS the fee model. It already carries
    the channel / Guesty / Stripe fees for every booking (breakdown/payment_model, a
    transcription of payment_structure.xlsx), so there is nothing to look up in QBO. The
    old `_apply_guesty_fees` step — base_amount minus fees matched out of QBO Bills — ran
    first and was then overwritten here for every booking that HAS a breakdown row, i.e.
    it only ever survived on the bookings it was least able to price. It is deleted.

    Two cases, and only two:
      * in the breakdown  -> net = its `net_revenue`
      * not in it         -> net = `base_amount`, the immutable Guesty total payout
    A booking is absent because the API dropped its deactivated listing, or because it is
    a $0/comp row the breakdown drops. Falling back to the Guesty base is the honest
    answer; the old fallback matched QBO fees by DATE RANGE and channel name, which
    attached booking 6417694642's $132.20 Booking.com channel fee to the unrelated $0
    owner stay GY-9n5Dk4ZL and drove its net to -$132.20.

    Idempotent: `amount` is rewritten from the CSV / `base_amount` every build, and
    `base_amount` is never touched. MUST run BEFORE build_statements; the later overrides
    (_drop_tiny_cancellations, _apply_qbo_cancellation_adjustments) run after and win.
    """
    p = Path(pb_csv_path)
    net_by_code = {}
    if p.exists():
        df = pd.read_csv(p)
        net_by_code = {str(r["confirmationCode"]): round(float(r["net_revenue"]), 2)
                       for _, r in df.iterrows()
                       if pd.notna(r.get("confirmationCode")) and pd.notna(r.get("net_revenue"))}
    else:
        print(f"  !! payment-breakdown net: {p.name} NOT FOUND — every STR booking falls "
              f"back to its Guesty base (gross of channel/Stripe fees). Pull the period.")

    rows = conn.execute(
        """SELECT ledger_id, source_txn_id, amount, base_amount FROM ledger_lines
           WHERE posting_date>=? AND posting_date<=?
             AND source='guesty' AND category='INCOME'""",
        (period_start, period_end)).fetchall()
    n = 0
    for r in rows:
        code = str(r["source_txn_id"])
        if code in net_by_code:
            net = net_by_code[code]
            n += 1
        else:
            net = round(float(r["base_amount"] if r["base_amount"] is not None
                              else r["amount"] or 0.0), 2)
        conn.execute("UPDATE ledger_lines SET amount=?, base_amount=COALESCE(base_amount, ?) "
                     "WHERE ledger_id=?", (net, net, r["ledger_id"]))
    conn.commit()
    print(f"  payment-breakdown net: {n} STR booking net(s) from the breakdown; "
          f"{len(rows) - n} fell back to the Guesty base.")
    return n
def _drop_tiny_cancellations(conn, period_start, period_end, pb_csv_path):
    """Remove cancellation residues from the statement entirely (owner decision 2026-08-28).

    A canceled booking whose payment-breakdown InvoiceItem is at or below
    ``DROPPED_CANCELLATION_MAX_INVOICE`` ($6.10) is the flat fee VRBO/HomeAway keeps
    out of a refunded $200 deposit, not a stay. Marking the guesty INCOME row
    ``include_in_statement=0`` drops it from gross revenue, commission, amount_due,
    the dashboard, Excel, PDF and end_balances at once; the shared Section-1 builder
    (reporting.booking_breakdown) filters the same codes out of the fee waterfall, so
    both sections still foot.

    Idempotent: un-suppresses first, so a booking that leaves the CSV (or a raised
    threshold) comes straight back on the next build. Runs BEFORE build_statements.
    """
    conn.execute(
        """UPDATE ledger_lines SET include_in_statement=1
           WHERE source='guesty' AND category='INCOME' AND include_in_statement=0
             AND posting_date>=? AND posting_date<=?""", (period_start, period_end))

    p = Path(pb_csv_path)
    codes = dropped_cancellation_codes(pd.read_csv(p)) if p.exists() else set()
    n = 0
    if codes:
        ph = ",".join("?" * len(codes))
        n = conn.execute(
            f"""UPDATE ledger_lines SET include_in_statement=0
                WHERE source='guesty' AND category='INCOME'
                  AND posting_date>=? AND posting_date<=?
                  AND source_txn_id IN ({ph})""",
            (period_start, period_end, *sorted(codes))).rowcount
    conn.commit()
    if n:
        print(f"  dropped {n} booking(s) from the statement: cancellation residues with "
              f"InvoiceItem <= ${DROPPED_CANCELLATION_MAX_INVOICE:.2f}, plus any owner-listed "
              f"code in booking_breakdown.DROPPED_BOOKING_CODES.")
    return n


def _canceled_codes(pb_df):
    """Confirmation codes the payment breakdown marks canceled.

    A canceled stay earned no cleaning fee, so an owner_pays_cleaning statement shows $0
    in its Owner Cleaning Fee column for it — matching what _apply_owner_cleaning_credit
    credits into amount_due.
    """
    if pb_df is None or not len(pb_df):
        return set()
    st = pb_df["status"].astype(str).str.strip().str.lower()
    return set(pb_df.loc[st.isin(["canceled", "cancelled"]), "confirmationCode"].astype(str))


def _recognize_cancelled_retained_income(conn, period_start, period_end):
    """Recognise a cancelled booking's forfeited money when Guesty never returned it.

    A cancellation the guest forfeited money on IS owner revenue: QuickBooks recognises
    it on an Invoice line (`Guest Charges:Owner Income:*`) and bills $0.00 commission.
    Our revenue comes from Guesty, and QBO Invoice lines are deliberately stored
    `category='EXPENSE'` so they can never double-count a Guesty booking — which means a
    cancelled booking Guesty did NOT return has its money in no statement at all.
    `seattle_710_adu` / `6486562246` is the case: the guest paid, Payment 117169 cleared
    the $119.00 invoice, and the owner was credited nothing.

    Scoped tightly, because the failure mode is over-crediting an owner for money that
    was handed back. All five must hold:

      * the booking carries a QBO ``| Cancelled |`` line;
      * it has NO guesty INCOME row in ANY period — when Guesty has the booking, Guesty
        is the source and recognising here too would double-count (`GY-kFKR64Tc` is the
        working case: same shape, came through the pull, already in the statement);
      * its `Guest Charges:Owner Income:*` lines NET POSITIVE. A fully refunded
        cancellation books 0.00 there, which is how 14 of the 16 August candidates
        exclude themselves — `GY-j48ykHag` took in $4,997.07 and refunded all of it;
      * no ``REFUND FOR CHARGE (<code>)`` line offsets it (belt and braces: those refund
        legs post to `1A - Net Earnings`, not to a cash account, so a cash-only test
        misses them);
      * QBO RECOGNISED it in this period. For these bookings QuickBooks is the only
        source — Guesty never returned them — so the QBO invoice date is the recognition
        date, NOT the check-in. `GY-QgDUzUg5` is why: a 2026-09-07 stay cancelled early,
        whose $191.90 invoice QBO dated 2026-08-28 and whose $34.54 commission it billed
        2026-08-31 (Bill 116379). Both legs are August in the books, so August is the
        month the owner should see it in.

    Idempotent: the period's own rows are deleted and rebuilt on every build.

    **Double-count guard, and its one requirement.** If Guesty later returns the booking
    (a pull covering its check-in month), the "no guesty INCOME row" test drops it here
    automatically — but only on the NEXT BUILD of this period. So after pulling a month
    whose bookings a prior period recognised this way, REBUILD the earlier period too.
    For `GY-QgDUzUg5`: pulling 2026-09 will likely give it a guesty row, at which point
    2026-08 must be rebuilt or the $191.90 is counted in both months.
    """
    cur = conn.cursor()
    cur.execute("""DELETE FROM ledger_lines WHERE source_object='CancelledRetainedIncome'
                    AND posting_date>=? AND posting_date<=?""", (period_start, period_end))

    guesty_codes = {r[0] for r in conn.execute(
        "SELECT DISTINCT source_txn_id FROM ledger_lines WHERE source='guesty' AND category='INCOME'"
    ) if r[0]}

    owner_income = {}
    for r in conn.execute("""SELECT property_id, vendor_customer, posting_date, amount
                              FROM ledger_lines
                              WHERE source_object='Invoice' AND vendor_customer IS NOT NULL
                                AND qbo_account LIKE 'Guest Charges:Owner Income:%'"""):
        vc = r["vendor_customer"]
        code = vc.rsplit(" - ", 1)[-1].strip() if " - " in vc else vc
        e = owner_income.setdefault(code, {"pid": r["property_id"], "amt": 0.0,
                                           "recognized": r["posting_date"], "customer": vc})
        e["amt"] += float(r["amount"] or 0.0)
        # Earliest QBO invoice date is the recognition date.
        if r["posting_date"] and r["posting_date"] < e["recognized"]:
            e["recognized"] = r["posting_date"]

    added = []
    for code, e in owner_income.items():
        if code in guesty_codes or round(e["amt"], 2) <= 0:
            continue
        ci = e["recognized"]
        if not ci or not (period_start <= ci <= period_end):
            continue
        cancelled = conn.execute(
            """SELECT 1 FROM ledger_lines WHERE description LIKE ? AND description LIKE '%| Cancelled |%'
                LIMIT 1""", (f"%{code}%",)).fetchone()
        if not cancelled:
            continue
        refunded = conn.execute(
            """SELECT COALESCE(SUM(amount),0) FROM ledger_lines
                WHERE description LIKE ?""", (f"%REFUND FOR CHARGE ({code})%",)).fetchone()[0]
        if round(e["amt"] + float(refunded or 0.0), 2) <= 0:
            continue
        # NET OF THE CHANNEL'S CUT. The owner-income invoice is written GROSS of the
        # channel fee for the channels whose InvoiceItem includes it (Booking.com,
        # HomeAway, Expedia, Trip.com — `payment_model`'s `cf_in_net` group), and QBO
        # bills that fee separately. `6486562246`: $119.00 invoiced, $17.85 Booking.com
        # fee, so the owner nets $101.15 and QBO commissions 16% of THAT = $16.18.
        # The Stripe fee is NOT deducted — it is already out of the invoice before it is
        # written (`GY-QgDUzUg5`: $200.00 paid, $191.90 invoiced, $8.10 Stripe), so
        # subtracting it here would double-count it.
        chan_fee = conn.execute(
            """SELECT COALESCE(SUM(amount),0) FROM ledger_lines
                WHERE description LIKE ? AND lower(description) LIKE '%channel fee%'
                  AND qbo_account LIKE 'Billable Expense Income%'""",
            (f"%{code}%",)).fetchone()[0]
        amt = round(e["amt"] - float(chan_fee or 0.0), 2)
        if amt <= 0:
            continue
        cur.execute(
            """INSERT INTO ledger_lines
               (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                posting_date, category, subcategory, description, vendor_customer, amount,
                base_amount, include_in_statement, status, last_updated_at)
               VALUES (?, 'qbo', 'CancelledRetainedIncome', ?, NULL, ?, ?, 'INCOME',
                       'Cancelled Booking', ?, ?, ?, ?, 1, 'posted', ?)""",
            (str(uuid.uuid4()), code, e["pid"], ci,
             f"Cancelled booking retained | {code}", e["customer"], amt, amt, now_iso()))
        added.append((e["pid"], code, amt, float(chan_fee or 0.0)))

    conn.commit()
    if added:
        print(f"  cancelled bookings with retained income Guesty did not return: {len(added)}")
        for pid, code, amt, cf in sorted(added, key=lambda x: -x[2]):
            note = f"  (net of ${cf:,.2f} channel fee)" if cf else ""
            print(f"    {pid:22} {code:16} ${amt:>9,.2f}{note}")
    return added


def _drop_non_owner_income(conn, period_start, period_end):
    """Remove QBO INCOME that is not the owner's money at all (owner decision 2026-08-28).

    Valta's own legal / insurance matters, marked "(legal)" in the QBO description by
    the bookkeeper — see common.income_rules.NOT_OWNER_PRED. They are NOT rent and NOT
    an owner credit either: left alone they fall through to the uncommissioned
    "Other Credits" bucket, which still adds them to gross revenue and to the payout
    (seattle_1117 2026-07 was $99.36 the owner should never have been paid).

    Marking them ``include_in_statement=0`` drops them from gross revenue, the Booking
    Breakdown, amount_due, the dashboard, Excel, PDF and end_balances at once. Commission
    is unaffected — they were never in the base.

    Idempotent (suppressing a suppressed row is a no-op), and self-healing after a
    re-sync: qbo-sync re-inserts the raw row with include_in_statement=1 and the next
    build takes it back out. Must run BEFORE build_statements.

    It does NOT un-suppress first, unlike ``_drop_tiny_cancellations``. That function
    owns an identifiable set (the period's guesty INCOME) it can safely reset; here the
    only handle on a row is the predicate itself, so a reset would have to key on
    something shared — source_txn_id — and would release rows suppressed by
    ``_exclude_duplicate_guesty_deposits`` on the same QBO transaction. Narrowing the
    predicate therefore needs a one-off UPDATE to bring the affected rows back.
    """
    n = conn.execute(
        f"""UPDATE ledger_lines SET include_in_statement=0
             WHERE posting_date>=? AND posting_date<=? AND ({NOT_OWNER_PRED})""",
        (period_start, period_end)).rowcount
    conn.commit()
    if n:
        print(f"  dropped {n} non-owner '(legal)' income line(s) from the statement.")
    return n


def _apply_qbo_cancellation_adjustments(conn, period_start, period_end, period):
    """Recognize a cancelled/refunded booking on the QBO record when QuickBooks ADJUSTED it
    (owner decision 2026-08-28) — see reporting.qbo_adjustments for why and how.

    Overrides the guesty INCOME `amount` with the QBO-adjusted net, so the stored
    amount_due, the Net Revenue section, the commission, the dashboard, Excel and the PDF
    all follow — and our commission ties to the QuickBooks Bill to the cent. Section 1 gets
    the SAME map (build_breakdown_by_unit(..., net_overrides=...)), so the two sections
    still foot.

    MUST run AFTER _apply_payment_breakdown_net (it overrides that net) and BEFORE
    build_statements. Idempotent: touches `amount` only (base_amount stays the immutable
    Guesty base) and is re-derived from the ledger each build.
    """
    nets = adjusted_nets(conn, period)
    n = 0
    for code, net in nets.items():
        n += conn.execute(
            """UPDATE ledger_lines SET amount=?
               WHERE source='guesty' AND category='INCOME' AND source_txn_id=?
                 AND posting_date>=? AND posting_date<=?""",
            (net, code, period_start, period_end)).rowcount
    conn.commit()
    if n:
        print(f"  QBO cancellation adjustments: {n} booking(s) recognized on the QBO record.")
    missing = [c for c in nets if not conn.execute(
        """SELECT 1 FROM ledger_lines WHERE source='guesty' AND category='INCOME'
             AND source_txn_id=? AND posting_date>=? AND posting_date<=? LIMIT 1""",
        (c, period_start, period_end)).fetchone()]
    if missing:
        print(f"  ! {len(missing)} QBO-adjusted booking(s) have NO Guesty income row "
              f"(nothing to adjust): {', '.join(sorted(missing))}")
    for pid, code, comm in unrated_adjustments(conn, period):
        print(f"  ! QBO adjustment skipped, no PM rate for {pid}: {code} commission ${comm:,.2f}")
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

    # Rollups: parent property_id -> member listings consolidated into one statement.
    # Derived from Listing_contacts.csv (its Property column IS the statement), merged
    # with the legacy config.yml map for the few units the contacts file omits.
    rollups = statement_rollups(conn, cfg.get("statement_rollups") or {})
    rollup_children = {child for kids in rollups.values() for child in kids}

    # Only generate statements for listings present in Listing_contacts.csv.
    # Period-scoped so a Status=Inactive listing keeps its history but stops
    # producing new statements once it stops earning (see allowed_property_ids).
    allowed = allowed_property_ids(conn, base_dir, period_start, period_end)

    # property_id -> name, for per-listing Net Revenue sub-sections + expense 'Type' labels.
    property_names = {r["property_id"]: r["property_name"]
                      for r in conn.execute("SELECT property_id, property_name FROM properties")}

    # OSBR Hipcamp.com credits → 'osbr_rv' Net Revenue bookings (commissioned at OSBR's
    # rate), out of Other Credits. Runs before _apply_payment_breakdown_net so the RV
    # rows pick up their net from the same place every other guesty booking does.
    n_rv = _apply_osbr_rv(conn, period_start, period_end)
    if n_rv:
        print(f"OSBR-RV: {n_rv} Hipcamp booking(s) moved to Net Revenue (commissioned).")

    guesty_csv = str(Path(args.csv)) if getattr(args, "csv", None) else str(paths.guesty_converted_csv(period))

    # Set STR net from the payment breakdown — the ONE fee model (payment_structure.xlsx).
    # This is the single STR commission basis: statement_engine (stored amount_due), the
    # dashboard, Excel and PDF all read `amount`. Bookings absent from the breakdown fall
    # back to the immutable Guesty base, NOT to fees matched out of QBO by date range.
    _apply_payment_breakdown_net(conn, period_start, period_end,
                                 str(paths.payment_breakdown_csv(period)))

    # Cancellation residues (canceled + InvoiceItem <= $6.10) leave the statement.
    _drop_tiny_cancellations(conn, period_start, period_end,
                             str(paths.payment_breakdown_csv(period)))

    # Cancelled/refunded bookings QBO adjusted at month end: the QBO record is the basis.
    _apply_qbo_cancellation_adjustments(conn, period_start, period_end, period)

    # A cancelled booking Guesty never returned, whose forfeited money QBO recognised.
    # Runs after the adjustment steps: those handle cancellations that DO have a Guesty
    # row, this one handles the ones that do not.
    _recognize_cancelled_retained_income(conn, period_start, period_end)

    # QBO income that is not the owner's money at all ("(legal)" rows) leaves entirely.
    _drop_non_owner_income(conn, period_start, period_end)

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
    cleaning_props = {m["property_id"] for m in items if owner_pays_cleaning_at(m, period_start)}
    guesty_clean_by_code = _read_guesty_cleaning_by_code(guesty_csv)

    # Section-1 Booking Breakdown sources — all three, loaded ONCE for the whole
    # portfolio (reporting.period_sources, the same object the dashboard and the summary
    # sheets use, so the products cannot drift). build_by_unit narrows to each
    # statement's members inside the loop below.
    sources = PeriodSources(conn, period, list(allowed | rollup_children))
    # The channel commissions a later-billed add-on too. Applied to the ledger (so gross
    # revenue, the commission base and the payout all follow), then the sources are
    # reloaded so Section 1 renders the same net the ledger now holds.
    if _apply_addon_channel_fees(conn, sources):
        sources = PeriodSources(conn, period, list(allowed | rollup_children))

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
    written = set()
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

        # ---- Section 2 (Net Revenue) is Section 1 with commission applied ----------
        # Owner decision 2026-08-29. Both sections come from the ONE row set built by
        # reporting.booking_breakdown, so they cannot disagree; re-assembling Section 2
        # from the ledger + LTR CSV used to omit booking source #3 entirely (see
        # booking_breakdown.net_revenue_rows).
        bd_by_unit, bd_grand = build_breakdown_by_unit(
            sources.pb, sources.ltr, member_ids, sources.claimed, sources.other,
            net_overrides=sources.net_overrides)

        # Owner-facing cleaning / tax per booking. NOT the guest-paid cells on the
        # breakdown row: these are what the LEDGER credits the owner, and amount_due is
        # built from that (see _apply_owner_cleaning_credit / '%Taxes Paid to Owners%').
        _canceled = _canceled_codes(sources.pb)
        _ltr_clean = {str(r["booking_id"]): round(float(r.get("cleaning_fee") or 0.0), 2)
                      for r in sources.ltr}
        _clean_by_code, _tax_by_code = {}, {}
        for _upid, _urows in bd_by_unit.items():
            for _r in _urows:
                if _r.get("_total"):
                    continue
                _code = str(_r["Conf Code"])
                _oc = get_owner_costs(conn, _upid, _code, period_start, period_end,
                                      checkin=_r.get("_in"), checkout=_r.get("_out"))
                _tax_by_code[_code] = _oc["owner_tax_cost"]
                if _upid in cleaning_props:
                    # owner_pays_cleaning: the column shows the GUEST-paid cleaning fee as
                    # owner income (0 on a canceled stay) — the same figure the dashboard
                    # shows and the OWNER_ADJ credit folds into amount_due.
                    _clean_by_code[_code] = (
                        _ltr_clean.get(_code, 0.0) if _code in _ltr_clean
                        else 0.0 if _code in _canceled
                        else round(guesty_clean_by_code.get(_code, 0.0), 2))
                else:
                    _clean_by_code[_code] = _oc["owner_cleaning_cost"]

        # Per-BOOKING rate, not per-period: netrevenue.engine sums the stored PM fee the
        # same way off each line's posting_date, so a mid-period rate change (seattle_9021,
        # 22% for check-ins after 2026-08-10) keeps Section 2 equal to the stored fee.
        # Constant for every property without one, so nothing else moves.
        _rate_at = pm_rate_resolver(conn, pid, period_start, period_end)
        bookings = [b for recs in build_net_revenue_rows(
                        bd_by_unit, _rate_at, _clean_by_code, _tax_by_code).values()
                    for b in recs]


        # QBO deposit income (lease rent, credits, etc.) -> "Other Credits". Rent already
        # shown as a Net Revenue booking is excluded so it is not listed twice.
        other_income_rows = conn.execute(f"""
            SELECT posting_date, description, vendor_customer, subcategory, amount,
                   property_id, source_object
            FROM ledger_lines
            WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
              AND source='qbo' AND category='INCOME' AND include_in_statement=1
            ORDER BY posting_date""", (*member_ids, period_start, period_end)).fetchall()
        other_income = [
            oi for oi in other_income_rows
            if not (oi["property_id"] in sources.ltr_covered
                    and is_rent_income(oi["source_object"], oi["description"]))
        ]
        _bd_keys = other_ledger_keys(sources.other)
        other_income = [
            oi for oi in other_income
            if (oi["property_id"], oi["posting_date"], round(float(oi["amount"] or 0), 2)) not in _bd_keys
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

        out_path = out_dir / f"{pid}_owner_statement_{period}.xlsx"
        written.add(out_path.name)
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

    # Sweep statements this run did NOT write. `build` used to only ever add files, so a
    # scope change left a stale statement behind for a listing that is now a rollup member
    # or has no activity — and it still showed real money (mercer_3627_adu 2026-07 sat at
    # $574.63 against stored totals of $0.00). Only this period's generated .xlsx files are
    # touched, and every one of them is rewritten from the DB on the next build.
    stale = sorted(f for f in out_dir.glob(f"*_owner_statement_{period}.xlsx")
                   if f.name not in written)
    for f in stale:
        f.unlink()
    print(f"Statements generated for {period}: {generated} files → {out_dir}")
    if stale:
        print(f"  removed {len(stale)} stale statement(s) no longer in scope: "
              + ", ".join(f.name.split("_owner_statement")[0] for f in stale))

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
    # qbo-auth / qbo-exchange moved to QBO_operations; the stub explains where.
    sub.add_parser("qbo-auth").set_defaults(func=cmd_qbo_auth)

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
