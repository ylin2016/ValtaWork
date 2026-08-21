"""Import Yacinde "old bookings" (owner-direct / pre-management reservations that
Valta does not collect rental revenue on, but does charge a per-booking supplies fee).

For each qualifying booking:
  * a Net Revenue line is added with ZERO gross/net revenue (so it shows in the
    statement's Net Revenue table but contributes no rental revenue and no commission);
  * a Supplies expense is charged to the owner = 0.9 * guests * nights.

Only bookings whose CHECK-IN is on/after --min-checkin (default 2026-07-15) are added.
Each booking's posting_date is its check-in, so it lands in the statement for the
month it checks in (build each month separately).

Storage (mirrors the LTR trick so it flows through the existing pipeline unchanged):
  * Net Revenue line: source='guesty', category='INCOME', amount=0, base_amount=0,
    source_object='YacindeOld'  -> renders in the Net Revenue table at $0.
  * Supplies expense: source='qbo', category='EXPENSE', subcategory='Supplies',
    qbo_account='Trust Liabilities:Owner Payables:1C - Owner Expenses:Supplies - Owner',
    amount = -(0.9*guests*nights), source_object='YacindeSupply'
    -> matches the '%Owner Expenses%' filter (amount_due) AND the source='qbo'
       Supplies display bucket, so dashboard/Excel/amount_due all agree.

Idempotent: deletes its own prior rows (source_object IN YacindeOld/YacindeSupply)
across all periods, then re-inserts every qualifying booking.

CAVEAT: the Supplies expense is stored source='qbo'. A QBO re-sync that clears a
period's qbo rows (DELETE ... source='qbo') will remove that month's supply lines,
so re-run this importer after any qbo-sync (same discipline as import_ltr).

Usage:
    python -m src.import_yacinde_old [--csv "data/Yacinde old bookings.csv"] [--min-checkin 2026-07-15]
"""
import argparse
import uuid
from pathlib import Path

import pandas as pd

from .run_month_close import load_config, connect, supply_charge, SUPPLY_ACCOUNT, SUPPLY_MAX_NIGHTS
from .convert_guesty_export import to_property_id
from .utils import now_iso

MARK_INCOME = "YacindeOld"
MARK_SUPPLY = "YacindeSupply"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--csv", default="data/Yacinde old bookings.csv")
    ap.add_argument("--config", default="config.yml")
    ap.add_argument("--min-checkin", default="2026-07-15", help="YYYY-MM-DD; only add check-ins on/after this")
    args = ap.parse_args()

    cfg = load_config(args.config)
    conn = connect(cfg["app"]["db_path"])
    known = {r[0] for r in conn.execute("SELECT property_id FROM properties").fetchall()}

    df = pd.read_csv(args.csv)
    df["_ci"] = pd.to_datetime(df["Check In"], format="%m/%d/%y", errors="coerce")
    cutoff = pd.Timestamp(args.min_checkin)
    df = df[df["_ci"] >= cutoff].copy()

    # Idempotent: clear this importer's own prior rows (all periods) first.
    conn.execute("DELETE FROM ledger_lines WHERE source_object IN (?, ?)", (MARK_INCOME, MARK_SUPPLY))

    has_status = "STATUS" in df.columns
    n_income = n_supply = n_cancelled = 0
    skipped = {}
    ts = now_iso()
    for _, r in df.iterrows():
        pid = to_property_id(r["LISTING'S NICKNAME"])
        if pid not in known:
            skipped[pid] = skipped.get(pid, 0) + 1
            continue
        code = str(r["CONFIRMATION CODE"]).strip()
        checkin = r["_ci"].strftime("%Y-%m-%d")
        checkout = pd.to_datetime(r["Check Out"], format="%m/%d/%y").strftime("%Y-%m-%d")
        guest = str(r["GUEST"]).strip()
        channel = str(r["SOURCE"]).strip()
        guests = int(r["NUMBER OF GUESTS"])
        nights = int(r["NUMBER OF NIGHTS"])
        cancelled = has_status and "cancel" in str(r.get("STATUS") or "").strip().lower()

        # Net Revenue line at $0 (no rental revenue, no commission) — shown for every booking.
        conn.execute(
            """INSERT INTO ledger_lines
               (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                booking_id, posting_date, service_date, category, subcategory, description,
                vendor_customer, amount, base_amount, currency, include_in_statement, status,
                last_updated_at, created_at)
               VALUES (?, 'guesty', ?, ?, NULL, ?, ?, ?, ?, 'INCOME', ?, ?, ?, 0, 0, 'USD', 1, ?, ?, ?)""",
            (str(uuid.uuid4()), MARK_INCOME, code, pid, code, checkin, checkout, channel,
             f"{channel} - {guest} - {code}", guest, ("canceled" if cancelled else "confirmed"), ts, ts),
        )
        n_income += 1

        # No supplies charge for cancelled bookings (nobody stayed).
        if cancelled:
            n_cancelled += 1
            continue

        # Supplies expense = 0.9 * guests * min(nights, 60) (owner cost, negative).
        # Stored source='manual' — a formula-computed owner charge, NOT QBO data — so a
        # qbo-sync re-sync (which clears source='qbo' rows) does NOT wipe it.
        charge_nights = min(nights, SUPPLY_MAX_NIGHTS)
        supply = supply_charge(guests, nights)
        capped = " (capped at 60)" if nights > SUPPLY_MAX_NIGHTS else ""
        conn.execute(
            """INSERT INTO ledger_lines
               (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                booking_id, posting_date, service_date, category, subcategory, description,
                vendor_customer, qbo_account, amount, currency, include_in_statement, status,
                last_updated_at, created_at)
               VALUES (?, 'manual', ?, ?, NULL, ?, ?, ?, ?, 'EXPENSE', 'Supplies', ?, ?, ?, ?, 'USD', 1, 'posted', ?, ?)""",
            (str(uuid.uuid4()), MARK_SUPPLY, f"{code}_supply", pid, code, checkin, checkout,
             f"Supplies ({guests} guests x {charge_nights} nights x $0.90{capped}) - {guest} {code}",
             guest, SUPPLY_ACCOUNT, -supply, ts, ts),
        )
        n_supply += 1

    conn.commit()
    print(f"Yacinde old bookings: {n_income} net-revenue lines ($0) + {n_supply} supplies expenses "
          f"(source='manual', 0.9*guests*min(nights,60)) added (check-in >= {args.min_checkin}).")
    if n_cancelled:
        print(f"  cancelled bookings (no supply charge): {n_cancelled}")
    elif not has_status:
        print("  NOTE: CSV has no STATUS column — no bookings treated as cancelled.")
    if skipped:
        print("  skipped (unknown property_id): " + ", ".join(f"{k}={v}" for k, v in skipped.items()))


if __name__ == "__main__":
    main()
