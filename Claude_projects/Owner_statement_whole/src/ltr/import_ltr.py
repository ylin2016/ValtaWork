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
import uuid
import calendar
import pandas as pd

from ..common.db import connect
from ..common.utils import now_iso
# Listing label -> property_id lives in `records` (the display-layer module that
# also ships in the deploy bundle), so the importer and the Booking Breakdown can
# never disagree about which unit a CSV row belongs to.
from .records import to_property_id  # noqa: F401  (re-exported for callers/tests)


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
            "cleaning": to_float(r.get("Cleaning.Fee")),
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
    # ...and our own prior LTR cleaning credits (re-derived from the CSV below).
    cur.execute(
        """DELETE FROM ledger_lines
           WHERE source='manual' AND category='OWNER_ADJ'
             AND source_object='LtrCleaningCredit'
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
    # ...and any prior _replace_colliding suppression of QBO deposits that carry a
    # confirmation code in vendor_customer, for the same reason.
    cur.execute(
        """UPDATE ledger_lines SET include_in_statement=1
           WHERE source='qbo' AND category='INCOME'
             AND source_object NOT IN ('LTR','DEFERRED')
             AND posting_date>=? AND posting_date<=?
             AND vendor_customer LIKE '% - %' AND include_in_statement=0""",
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
        """LTR wins: drop any non-LTR/DEFERRED ledger row for this confirmation code, so
        the LTR/DEFERRED amount fully replaces it. Returns the number of rows affected.

        TWO ways a row can carry the code, because QBO and Guesty key rows differently:

        * `source_txn_id` — guesty bookings. DELETED (guesty-import re-adds them, and the
          existing dedup expects them gone). **Scoped to the period**: the DELETE used to
          be unscoped, so importing a deferred row for month N wiped the ORIGINAL Guesty
          booking wherever it lived. Importing 2026-05 deleted `GY-LsV362As` from 2026-04
          and took $14,772.94 of Seattle 9021's April revenue with it (plus three more
          properties across 2026-03/04). Recovery is a guesty-import of the robbed period.
        * `vendor_customer` — QBO Deposits, which key on the QBO transaction id and put the
          code in Guesty's "<channel> - <guest> - <code>" customer string (the same shape
          reporting.other_income parses). SUPPRESSED rather than deleted: qbo-sync would
          re-add a deleted row, and include_in_statement=0 is reversible and idempotent.
          Missing these let one booking be counted twice — bellevue_1326 2026-07 carried
          BOTH the $9,112.25 DEFERRED row and a $6,519.38 QBO deposit for HMPA8BRAYA.
        """
        cur.execute(
            """DELETE FROM ledger_lines
               WHERE source_txn_id=? AND source_object NOT IN ('LTR','DEFERRED')
                 AND posting_date>=? AND posting_date<=?""",
            (code, period_start, period_end))
        n = cur.rowcount
        cur.execute(
            """UPDATE ledger_lines SET include_in_statement=0
               WHERE source='qbo' AND category='INCOME'
                 AND source_object NOT IN ('LTR','DEFERRED')
                 AND posting_date>=? AND posting_date<=?
                 AND vendor_customer LIKE ('% - ' || ?)""",
            (period_start, period_end, code))
        return n + cur.rowcount

    ltr_n = deferred_n = clean_n = 0
    replaced = []
    skipped = []
    suppressed = []
    ltr_total = def_total = clean_total = 0.0
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
            # Owner rule: if the LTR CSV carries the booking, the LTR record wins — so an
            # LTR row also displaces a QBO deposit that names its confirmation code.
            n_del = _replace_colliding(x["code"])
            if n_del:
                replaced.append((x, n_del))
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

        # LTR cleaning fee (from the CSV's Cleaning.Fee): non-commissioned owner income
        # (Total Payout = accommodation fare + cleaning). Add it as an OWNER_ADJ credit so
        # the build's amount_due / Net Income include it — mirrors the STR
        # OwnerCleaningCredit, but a distinct source_object so the build never wipes it.
        clean_amt = round(x.get("cleaning", 0.0), 2)
        if x["source_object"] == "LTR" and abs(clean_amt) >= 0.005:
            cur.execute(
                """INSERT INTO ledger_lines
                   (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                    posting_date, category, subcategory, description, amount, include_in_statement, status, last_updated_at)
                   VALUES (?, 'manual', 'LtrCleaningCredit', ?, NULL, ?, ?, 'OWNER_ADJ', 'LTR', ?, ?, 1, 'posted', ?)""",
                (str(uuid.uuid4()), f"ltrcleancredit_{x['code']}", x["prop"], x["checkin"],
                 "LTR cleaning fee (tenant-paid, owner keeps)", clean_amt, now_iso()),
            )
            clean_n += 1
            clean_total += clean_amt

    conn.commit()

    sup_total_all = sum(t for _, _, t in suppressed)
    print(f"LTR import: {ltr_n} LTR rows added (${ltr_total:,.2f}), "
          f"{deferred_n} deferred rows added (${def_total:,.2f}) into {period}; "
          f"{clean_n} LTR cleaning credits (${clean_total:,.2f}); "
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
    from .. import paths
    parser = argparse.ArgumentParser(description="Import LTR + deferred revenue CSV into the ledger")
    parser.add_argument("--csv", help="Path to LTR CSV (defaults to inputs/<period>/LTR_<period>.csv)")
    parser.add_argument("--period", required=True, help="Period YYYY-MM (e.g. 2026-05)")
    parser.add_argument("--config", default=str(paths.CONFIG_YML))
    args = parser.parse_args()

    csv_path = args.csv or str(paths.ltr_csv(args.period))
    conn = connect(str(paths.DB_PATH))
    import_ltr_csv(conn, csv_path, args.period)
