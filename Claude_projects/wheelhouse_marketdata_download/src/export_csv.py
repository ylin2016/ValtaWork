"""Export one snapshot's tables to dated per-table CSVs."""
from pathlib import Path

import pandas as pd

from . import db

# Tables exported to CSV. The high-volume daily/detail tables (time_series,
# distributions, changelog) are intentionally NOT exported — they still live in
# data/wheelhouse.sqlite; query them there. To re-add one, list it here.
EXPORT_TABLES = [
    "listings",
    "markets",
    "dynamic_sets",
    "dynamic_set_associated_listings",
    "dynamic_set_members",
    "dynamic_set_aggregated_metrics",
]


def export_snapshot(conn, export_dir: str, snapshot_date: str) -> str:
    """Write each table's rows for snapshot_date to one CSV per table.

    Returns the folder. (The SQLite DB remains the full source of truth,
    including raw_json blobs.)
    """
    out_dir = Path(export_dir) / snapshot_date
    out_dir.mkdir(parents=True, exist_ok=True)
    existing = set(db.tables(conn))

    total = n_tables = 0
    for table in EXPORT_TABLES:
        if table not in existing:
            continue
        df = pd.read_sql_query(
            f"SELECT * FROM {table} WHERE snapshot_date = ?",
            conn, params=(snapshot_date,),
        )
        df.to_csv(out_dir / f"{table}.csv", index=False)
        total += len(df)
        n_tables += 1

    print(f"  export: {total} rows across {n_tables} tables -> {out_dir}")
    return str(out_dir)
