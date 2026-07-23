"""Export one snapshot's tables to dated CSVs plus a single xlsx workbook."""
from pathlib import Path

import pandas as pd

from . import db

# Tables worth exporting (raw_responses is a debug safety-net, skipped by default).
EXPORT_TABLES = [
    "listings",
    "markets",
    "market_time_series",
    "market_distributions",
    "dynamic_sets",
    "dynamic_set_associated_listings",
    "dynamic_set_members",
    "dynamic_set_aggregated_metrics",
    "dynamic_set_time_series",
    "dynamic_set_distributions",
    "dynamic_set_changelog",
]


def export_snapshot(conn, export_dir: str, snapshot_date: str) -> str:
    """Write each table's rows for snapshot_date to CSV + one xlsx. Returns the folder."""
    out_dir = Path(export_dir) / snapshot_date
    out_dir.mkdir(parents=True, exist_ok=True)
    existing = set(db.tables(conn))

    frames = {}
    for table in EXPORT_TABLES:
        if table not in existing:
            continue
        df = pd.read_sql_query(
            f"SELECT * FROM {table} WHERE snapshot_date = ?",
            conn, params=(snapshot_date,),
        )
        frames[table] = df
        df.to_csv(out_dir / f"{table}.csv", index=False)

    xlsx_path = out_dir / f"wheelhouse_{snapshot_date}.xlsx"
    with pd.ExcelWriter(xlsx_path, engine="openpyxl") as xw:
        wrote_any = False
        for table, df in frames.items():
            if df.empty:
                continue
            # Drop raw_json in the workbook only — the blobs blow past Excel's
            # 32k-char cell limit and aren't useful in a spreadsheet. CSVs + the
            # SQLite DB keep the full raw payload.
            sheet_df = df.drop(columns=[c for c in ("raw_json", "json") if c in df.columns])
            # Excel sheet names max 31 chars.
            sheet_df.to_excel(xw, sheet_name=table[:31], index=False)
            wrote_any = True
        if not wrote_any:
            pd.DataFrame({"note": ["no rows for this snapshot"]}).to_excel(
                xw, sheet_name="empty", index=False)

    total = sum(len(df) for df in frames.values())
    print(f"  export: {total} rows across {len(frames)} tables -> {out_dir}")
    return str(out_dir)
