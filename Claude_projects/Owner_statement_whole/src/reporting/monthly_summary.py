"""Month-by-month statement summary — the shared engine behind both summary sheets.

    python -m src.reporting.monthly_summary                  # every statement, every period
    python -m src.reporting.monthly_summary --period 2026-07 2026-08
    python -m src.reporting.monthly_summary --property osbr seattle_10057

Writes ``output/all_properties_monthly_summary.{csv,xlsx}`` with three sheets:

* **By property**   one row per (statement, month) — the long table to filter/pivot.
* **All properties** those rows summed across properties, one row per month.
* **Due by month**  ``Amount Due to Owner`` as a property x month matrix.

Each row carries the same two blocks as the OSBR sheet:

* ``BK …`` — the Section-1 Booking Breakdown grand total, built through the SAME
  shared builder the dashboard/Excel/PDF use (``reporting.booking_breakdown``), so it
  covers ALL THREE booking sources: the Guesty payment breakdown, LTR/deferred, and
  fee-free rental income with no breakdown row (Hipcamp ``osbr_rv``, QBO-recorded rent).
* everything after it — the stored statement totals plus owner expenses split by
  subcategory (Repairs and Maintenance are separate — see config/mapping_accounts.yml).

``BK Net Rental Revenue`` and ``Net Rental Revenue`` are the same money counted two
ways, so they MUST match for every statement in every period; ``main`` asserts it and
prints any gap. ``BK Invoice Amount`` is Guesty-only by nature (the fee-free sources
carry no InvoiceItem), so it is the one BK column that does not foot to Guest Pay.

WHICH statements appear is ``scope.listing_filter.allowed_property_ids`` resolved PER
PERIOD, exactly as ``build`` resolves it — so rollup members never get their own row,
and a retired listing stops the month it winds down instead of minting $0 rows forever.

``osbr_summary`` is a thin wrapper over this module; keep the math here so the two
sheets can never disagree.
"""
import argparse
import sqlite3

import pandas as pd

from .. import paths
from ..common.config import load_config
from ..scope.listing_filter import allowed_property_ids, statement_rollups
from .booking_breakdown import build_by_unit
from .period_sources import PeriodSources, period_bounds

# Expense buckets — the statement subcategories, straight from the ledger. Repairs and
# Maintenance are separate subcategories (mapping_accounts.yml), so the dashboard, the
# Excel statements and these sheets all break them out the same way.
EXP_COLS = ["Cleaning Labor", "Other Expense", "Repairs", "Maintenance", "Supplies", "Utilities"]

# The measured columns, in order. The key columns in front of them differ per sheet
# (Property + Month on the long sheet, Month alone on the rolled-up ones).
METRICS = (["BK #Bookings", "BK Guest Pay", "BK Invoice Amount", "BK Channel Fee",
            "BK Guesty Fee", "BK Stripe Fee", "BK Cleaning Fee", "BK Tax",
            "BK Net Rental Revenue", "Net Rental Revenue", "Taxes", "Total Expenses",
            "PM Fee", "Owner Adjustments", "Amount Due to Owner"]
           + [f"Exp: {s}" for s in EXP_COLS])


def property_names(conn):
    return {pid: name for pid, name in
            conn.execute("SELECT property_id, property_name FROM properties")}


def breakdown_totals(src, members):
    """Section-1 grand total for `members` — all three booking sources."""
    by_unit, _ = build_by_unit(src.pb, src.ltr, members, src.claimed, src.other,
                               net_overrides=src.net_overrides)

    acc = {k: 0.0 for k in ("Guest Pay", "Fees", "Cleaning Fee", "Tax", "Net Rental Revenue",
                            "_ch", "_gu", "_st")}
    n = 0
    for rows in by_unit.values():
        n += len(rows) - 1                      # last row is the unit TOTAL
        for k in acc:
            acc[k] += rows[-1][k]
    invoice = 0.0
    if src.pb is not None and len(src.pb):
        seg = src.pb[src.pb["property_id"].isin(members)]
        invoice = float(seg["InvoiceItem"].fillna(0).sum())
    return n, acc, round(invoice, 2)


def stored_totals(conn, period, parent):
    row = conn.execute(
        """SELECT t.gross_booking_revenue, t.taxes, t.total_expenses, t.total_fees,
                  t.owner_adjustments, t.amount_due_to_owner
             FROM statement_property_totals t JOIN statement_runs r USING(run_id)
            WHERE t.property_id=? AND r.period_start=?
            ORDER BY r.created_at DESC LIMIT 1""", (parent, f"{period}-01")).fetchone()
    return tuple(round(float(v or 0.0), 2) for v in row) if row else (0.0,) * 6


def expenses(conn, period, members):
    ps, pe = period_bounds(period)
    ph = ",".join("?" * len(members))
    by = dict(conn.execute(
        f"""SELECT subcategory AS bucket, ROUND(SUM(amount), 2) FROM ledger_lines
             WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
               AND source IN ('qbo','manual') AND category='EXPENSE'
               AND include_in_statement=1 AND qbo_account LIKE '%Owner Expenses%'
             GROUP BY bucket""", (*members, ps, pe)).fetchall())
    return [round(float(by.get(s, 0.0)), 2) for s in EXP_COLS]


def metrics_row(conn, src, period, parent, members):
    """The METRICS values for one statement in one period."""
    n, bk, invoice = breakdown_totals(src, members)
    gross, taxes, exp, fees, oa, due = stored_totals(conn, period, parent)
    return [n, round(bk["Guest Pay"], 2), invoice, round(bk["_ch"], 2), round(bk["_gu"], 2),
            round(bk["_st"], 2), round(bk["Cleaning Fee"], 2), round(bk["Tax"], 2),
            round(bk["Net Rental Revenue"], 2), gross, taxes, exp, fees, oa, due] \
        + expenses(conn, period, members)


def build(conn, periods, only=None):
    """Long table: one row per (statement, period).

    `only` restricts to those parent property_ids (still period-scoped, so a retired
    one still drops out of the months it has no activity in).
    """
    names = property_names(conn)
    all_pids = list(names)
    # Same two-argument call the build and the dashboard make: the contacts file is the
    # standard, but config.yml's legacy members (beachwood_6, bellevue_14507_unit_*) have
    # no contacts row and carry real ledger lines. Passing {} here silently dropped their
    # rent out of the Booking Breakdown while the stored totals still counted it.
    rollups = statement_rollups(conn, load_config(str(paths.CONFIG_YML)).get("statement_rollups") or {})
    keep = set(only) if only else None

    out = []
    for period in periods:
        ps, pe = period_bounds(period)
        parents = allowed_property_ids(conn, paths.PROJECT_ROOT, ps, pe)
        if keep is not None:
            parents &= keep
        src = PeriodSources(conn, period, all_pids)
        for parent in sorted(parents):
            members = [parent] + list(rollups.get(parent, []))
            out.append([names.get(parent, parent), parent, period]
                       + metrics_row(conn, src, period, parent, members))

    df = pd.DataFrame(out, columns=["Property", "Property ID", "Month"] + METRICS)
    return df.sort_values(["Property", "Month"]).reset_index(drop=True)


def reconcile_gaps(df):
    """Rows where Section 1 does not equal Section 2 — should always be empty."""
    gap = (df["BK Net Rental Revenue"] - df["Net Rental Revenue"]).round(2)
    cols = [c for c in ("Property", "Month", "BK Net Rental Revenue", "Net Rental Revenue")
            if c in df.columns]
    return df.loc[gap.abs() > 0.01, cols]


def main():
    ap = argparse.ArgumentParser(
        description="Rebuild the all-properties month-by-month statement summary.")
    ap.add_argument("--period", nargs="+", default=None,
                    help="YYYY-MM … (default: every period in the DB)")
    ap.add_argument("--property", nargs="+", default=None,
                    help="statement property_id(s) to limit to (default: all)")
    args = ap.parse_args()

    conn = sqlite3.connect(str(paths.DB_PATH))
    conn.row_factory = sqlite3.Row      # other_income.build_records reads rows by name
    periods = args.period or [r[0] for r in conn.execute(
        "SELECT DISTINCT substr(period_start,1,7) FROM statement_runs ORDER BY 1")]

    df = build(conn, periods, only=args.property)
    by_month = df.groupby("Month", as_index=False)[METRICS].sum().sort_values("Month")
    due = df.pivot_table(index="Property", columns="Month",
                         values="Amount Due to Owner", aggfunc="sum", fill_value=0.0)
    due["Total"] = due.sum(axis=1)

    out_csv = paths.OUTPUT_DIR / "all_properties_monthly_summary.csv"
    out_xlsx = paths.OUTPUT_DIR / "all_properties_monthly_summary.xlsx"
    df.to_csv(out_csv, index=False)
    with pd.ExcelWriter(out_xlsx, engine="openpyxl") as xl:
        df.to_excel(xl, sheet_name="By property", index=False)
        by_month.to_excel(xl, sheet_name="All properties", index=False)
        due.round(2).to_excel(xl, sheet_name="Due by month")

    n_props = df["Property ID"].nunique()
    print(f"Wrote {len(df)} row(s) — {n_props} statement(s) x {len(periods)} period(s)")
    print(f"  -> {out_csv}")
    print(f"  -> {out_xlsx}")

    bad = reconcile_gaps(df)
    if len(bad):
        print(f"\n!! BK Net Rental Revenue != Net Rental Revenue in {len(bad)} row(s):")
        print(bad.to_string(index=False))
    else:
        print("Section 1 vs Section 2 reconcile for every statement in every period.")


if __name__ == "__main__":
    main()
