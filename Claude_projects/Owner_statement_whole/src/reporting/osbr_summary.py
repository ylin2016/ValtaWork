"""OSBR month-by-month summary — one row per period for the OSBR rollup.

    python -m src.reporting.osbr_summary            # all periods in the DB
    python -m src.reporting.osbr_summary --period 2026-07 2026-08

Writes ``output/osbr_monthly_summary.{csv,xlsx}``.

This is a THIN WRAPPER over ``reporting.monthly_summary`` — the all-properties sheet
and this one are the same numbers, so the math lives there and only the filter
(``PARENT``) and the output filename live here. Run
``python -m src.reporting.monthly_summary`` for every statement at once.

See that module for what each column means and why ``BK Net Rental Revenue`` must
equal ``Net Rental Revenue`` in every period.
"""
import argparse
import sqlite3

import pandas as pd

from .. import paths
from .monthly_summary import METRICS, build, reconcile_gaps

PARENT = "osbr"

# Kept for callers that imported these from here before the split.
EXP_COLS = [c[len("Exp: "):] for c in METRICS if c.startswith("Exp: ")]
COLUMNS = ["Month"] + METRICS


def build_osbr(conn, periods):
    """The OSBR rows, shaped like the original sheet (Month first, no Property columns)."""
    df = build(conn, periods, only=[PARENT])
    return df[COLUMNS].sort_values("Month").reset_index(drop=True)


def main():
    ap = argparse.ArgumentParser(description="Rebuild the OSBR month-by-month summary.")
    ap.add_argument("--period", nargs="+", default=None,
                    help="YYYY-MM … (default: every period in the DB)")
    args = ap.parse_args()

    conn = sqlite3.connect(str(paths.DB_PATH))
    conn.row_factory = sqlite3.Row      # other_income.build_records reads rows by name
    periods = args.period or [r[0] for r in conn.execute(
        "SELECT DISTINCT substr(period_start,1,7) FROM statement_runs ORDER BY 1")]

    df = build_osbr(conn, periods)

    out_csv = paths.OUTPUT_DIR / "osbr_monthly_summary.csv"
    out_xlsx = paths.OUTPUT_DIR / "osbr_monthly_summary.xlsx"
    df.to_csv(out_csv, index=False)
    with pd.ExcelWriter(out_xlsx, engine="openpyxl") as xl:
        df.to_excel(xl, sheet_name="OSBR by month", index=False)

    print(f"Wrote {len(df)} period(s) -> {out_csv}")
    print(f"                        -> {out_xlsx}")

    bad = reconcile_gaps(df)
    if len(bad):
        print("\n!! BK Net Rental Revenue != Net Rental Revenue:")
        print(bad.to_string(index=False))
    else:
        print("Section 1 vs Section 2 reconcile in every period (max gap 0.00).")


if __name__ == "__main__":
    main()
