"""Import long-term lease (LTR) rents and deferred-revenue records into the ledger.

The input CSV (e.g. data/LTR_2026-05.csv) holds two kinds of rows:
  - Source == 'LTR'  -> long-term monthly leases   -> tagged source_object='LTR'
  - Source != 'LTR'  -> deferred-revenue bookings  -> tagged source_object='DEFERRED'

Both are inserted as source='qbo', category='INCOME' so they flow through the
existing "other income" path in statement_engine/run_month_close: they appear on
the Excel statements and receive PM commission at each property's contract rate
(loaded from mapping_classes.yml). The 'qbo' source is required by the
ledger_lines CHECK constraint (only qbo/guesty/manual allowed); the distinct
source_object tags keep these rows identifiable and reversible without touching
real QBO-synced rows (whose source_object is Bill/Deposit/Invoice/JournalEntry/
Purchase) or guesty bookings.
"""
import argparse
import re
import uuid
import calendar
import pandas as pd
from pathlib import Path

from .config import load_config
from .db import connect
from .utils import now_iso

# Listings whose slugged name does not match the canonical property_id.
_ALIASES = {
    "bellevue 14507u3": "bellevue_14507_unit_3",
}


def to_property_id(listing: str) -> str:
    cleaned = str(listing).strip().lower()
    alias = _ALIASES.get(cleaned)
    if alias:
        return alias
    cleaned = re.sub(r"[^a-z0-9]+", "_", cleaned)
    cleaned = re.sub(r"_+", "_", cleaned).strip("_")
    return cleaned


def to_float(v) -> float:
    if pd.isna(v):
        return 0.0
    s = str(v).replace("$", "").replace(",", "").strip()
    return float(s) if s else 0.0


def import_ltr_csv(conn, csv_path: str, period: str):
    y, m = map(int, period.split("-"))
    period_start = f"{y:04d}-{m:02d}-01"
    period_end = f"{y:04d}-{m:02d}-{calendar.monthrange(y, m)[1]:02d}"

    df = pd.read_csv(csv_path, encoding="utf-8-sig")

    known_props = {r[0] for r in conn.execute("SELECT property_id FROM properties").fetchall()}

    rows = []
    unknown = []
    for _, r in df.iterrows():
        prop = to_property_id(r["Listing"])
        if prop not in known_props:
            unknown.append((str(r["Listing"]), prop, str(r["Confirmation.Code"])))
            continue

        src = str(r["Source"]).strip()
        is_ltr = src.upper() == "LTR"
        source_object = "LTR" if is_ltr else "DEFERRED"
        desc = "Long-term lease rent" if is_ltr else f"Deferred revenue ({src})"

        rows.append({
            "prop": prop,
            "code": str(r["Confirmation.Code"]).strip(),
            "tenant": str(r["Tenant"]).strip(),
            "checkin": pd.to_datetime(r["Checkin_date"]).strftime("%Y-%m-%d"),
            "checkout": pd.to_datetime(r["Checkout_date"]).strftime("%Y-%m-%d"),
            "amount": to_float(r["Net Revenue"]),
            "subcat": src,
            "source_object": source_object,
            "desc": desc,
        })

    # Idempotent: clear our own prior LTR/DEFERRED rows for this period, then insert.
    cur = conn.cursor()
    cur.execute(
        """DELETE FROM ledger_lines
           WHERE source='qbo' AND source_object IN ('LTR','DEFERRED')
             AND posting_date>=? AND posting_date<=?""",
        (period_start, period_end),
    )

    # Reset any prior LTR rent-deposit suppression for this period so re-runs (and
    # properties dropped from the CSV) start from a clean, fully-included state.
    cur.execute(
        """UPDATE ledger_lines SET include_in_statement=1
           WHERE source='qbo' AND category='INCOME'
             AND source_object NOT IN ('LTR','DEFERRED')
             AND posting_date>=? AND posting_date<=?
             AND LOWER(description) LIKE '%rent%' AND include_in_statement=0""",
        (period_start, period_end),
    )

    def _suppress_rent_deposits(prop):
        """Hide the QBO 'rent' deposits for this property/period from the statement
        (include_in_statement=0) and return (count, summed_amount).

        The CSV's LTR row carries the FULL monthly rent and is recognized as one
        commissioned line, so the raw QBO rent deposits that represent that same
        rent must be suppressed to avoid double-counting gross. Rent is identified
        by 'rent' in the description (e.g. "July Rent", "July Rent $1200/$2100");
        utilities and other credits are excluded so they stay in Other Credits.
        Our own LTR/DEFERRED rows are excluded by source_object.
        """
        row = cur.execute(
            """SELECT COUNT(*), COALESCE(SUM(amount),0) FROM ledger_lines
               WHERE source='qbo' AND category='INCOME'
                 AND source_object NOT IN ('LTR','DEFERRED')
                 AND property_id=? AND posting_date>=? AND posting_date<=?
                 AND LOWER(description) LIKE '%rent%'""",
            (prop, period_start, period_end)).fetchone()
        n, total = int(row[0] or 0), float(row[1] or 0.0)
        if n:
            cur.execute(
                """UPDATE ledger_lines SET include_in_statement=0
                   WHERE source='qbo' AND category='INCOME'
                     AND source_object NOT IN ('LTR','DEFERRED')
                     AND property_id=? AND posting_date>=? AND posting_date<=?
                     AND LOWER(description) LIKE '%rent%'""",
                (prop, period_start, period_end))
        return n, total

    def _replace_colliding(code):
        """LTR wins: delete any non-LTR/DEFERRED ledger row (e.g. a guesty
        booking) sharing this confirmation code, so the LTR/DEFERRED amount
        fully replaces it. Returns the number of rows deleted."""
        cur.execute(
            """DELETE FROM ledger_lines
               WHERE source_txn_id=? AND source_object NOT IN ('LTR','DEFERRED')""",
            (code,))
        return cur.rowcount

    ltr_n = deferred_n = 0
    replaced = []
    skipped = []
    suppressed = []
    ltr_total = def_total = 0.0
    for x in rows:
        add_amount = round(x["amount"], 2)  # store at cent precision
        if x["source_object"] == "DEFERRED":
            # LTR wins on code collision: drop the existing (guesty) row, then
            # insert this DEFERRED row in its place.
            n_del = _replace_colliding(x["code"])
            if n_del:
                replaced.append((x, n_del))
        else:  # LTR: recognize the FULL monthly rent as one commissioned line and
               # suppress the QBO rent deposits representing that same rent, so it is
               # counted + commissioned exactly once. (Was: add only the shortfall,
               # which left QBO-deposited rent sitting UNCOMMISSIONED in Other Credits.)
            add_amount = round(x["amount"], 2)
            n_sup, sup_total = _suppress_rent_deposits(x["prop"])
            if n_sup:
                suppressed.append((x, n_sup, sup_total))
                if sup_total - add_amount > 0.005:
                    print(f"  ! WARNING: {x['prop']} suppressed rent deposits "
                          f"${sup_total:,.2f} exceed CSV rent ${add_amount:,.2f}")

        cur.execute(
            """INSERT INTO ledger_lines
               (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                booking_id, posting_date, service_date, category, subcategory, description,
                vendor_customer, qbo_account, amount, include_in_statement, status, last_updated_at)
               VALUES (?, 'qbo', ?, ?, NULL, ?, ?, ?, ?, 'INCOME', ?, ?, ?, NULL, ?, 1, 'posted', ?)""",
            (str(uuid.uuid4()), x["source_object"], x["code"], x["prop"], x["code"],
             x["checkin"], x["checkout"], x["subcat"], x["desc"], x["tenant"],
             add_amount, now_iso()),
        )
        if x["source_object"] == "LTR":
            ltr_n += 1
            ltr_total += add_amount
        else:
            deferred_n += 1
            def_total += add_amount

    conn.commit()

    sup_total_all = sum(t for _, _, t in suppressed)
    print(f"LTR import: {ltr_n} LTR rows added (${ltr_total:,.2f}), "
          f"{deferred_n} deferred rows added (${def_total:,.2f}) into {period}; "
          f"{len(replaced)} guesty rows replaced by LTR; "
          f"{len(suppressed)} properties had QBO rent deposits suppressed "
          f"(${sup_total_all:,.2f}); {len(skipped)} skipped.")
    for x, n_del in replaced:
        print(f"  - replaced {n_del} guesty row(s) with DEFERRED: {x['prop']} {x['code']} ${x['amount']:,.2f}")
    for x, n_sup, sup_total in suppressed:
        print(f"  - suppressed {n_sup} QBO rent deposit(s) ${sup_total:,.2f} for {x['prop']}; "
              f"recognized full rent ${x['amount']:,.2f} (commissioned)")
    for x, why in skipped:
        print(f"  - skipped {x['source_object']}: {x['prop']} {x['code']} ${x['amount']:,.2f} ({why})")
    if unknown:
        print(f"WARNING: {len(unknown)} rows skipped (unknown property):")
        for listing, slug, code in unknown:
            print(f"  - '{listing}' -> '{slug}' (code {code})")


if __name__ == "__main__":
    base_dir = Path(__file__).parent.parent
    parser = argparse.ArgumentParser(description="Import LTR + deferred revenue CSV into the ledger")
    parser.add_argument("--csv", required=True, help="Path to LTR CSV (e.g. data/LTR_2026-05.csv)")
    parser.add_argument("--period", required=True, help="Period YYYY-MM (e.g. 2026-05)")
    parser.add_argument("--config", default=str(base_dir / "config.yml"))
    args = parser.parse_args()

    cfg = load_config(args.config)
    conn = connect(cfg["app"]["db_path"])
    import_ltr_csv(conn, args.csv, args.period)
