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


# listings.csv gets an extra dynamic_set_ids column: the set(s) each listing is
# associated with (comma-joined for the few listings that sit in more than one).
_LISTINGS_SQL = """
SELECT l.*, s.dynamic_set_ids
FROM listings l
LEFT JOIN (
  SELECT snapshot_date, user_listing_id,
         GROUP_CONCAT(set_id) AS dynamic_set_ids
  FROM dynamic_set_associated_listings
  GROUP BY snapshot_date, user_listing_id
) s ON s.snapshot_date = l.snapshot_date AND s.user_listing_id = l.listing_id
WHERE l.snapshot_date = ?
"""

# Compact monthly market summary for HIGH performers, by bedroom — the daily
# market_time_series rows averaged per calendar month.
_MARKET_HIGH_PERF_MONTHLY_SQL = """
SELECT t.market_id,
       (SELECT name FROM markets m
        WHERE m.market_id = t.market_id AND m.snapshot_date = t.snapshot_date) AS market_name,
       t.bedrooms,
       substr(t.date, 1, 7) AS month,
       ROUND(AVG(CASE WHEN t.metric = 'adr_w_fees'        THEN t.value END), 2) AS adr,
       ROUND(AVG(CASE WHEN t.metric = 'occupancy_adjusted' THEN t.value END), 4) AS occupancy_adjusted,
       ROUND(AVG(CASE WHEN t.metric = 'revpar_w_fees'      THEN t.value END), 2) AS revpar,
       COUNT(DISTINCT t.date) AS days_in_avg
FROM market_time_series t
WHERE t.snapshot_date = ? AND t.performance = 'high'
GROUP BY t.market_id, t.bedrooms, month
ORDER BY market_name, t.bedrooms, month
"""


def export_snapshot(conn, export_dir: str, snapshot_date: str) -> str:
    """Write each table's rows for snapshot_date to one CSV per table, plus a
    couple of derived summary CSVs.

    Returns the folder. (The SQLite DB remains the full source of truth,
    including raw_json blobs and the high-volume detail tables.)
    """
    out_dir = Path(export_dir) / snapshot_date
    out_dir.mkdir(parents=True, exist_ok=True)
    existing = set(db.tables(conn))

    total = n_files = 0
    for table in EXPORT_TABLES:
        if table not in existing:
            continue
        sql = _LISTINGS_SQL if table == "listings" else \
            f"SELECT * FROM {table} WHERE snapshot_date = ?"
        df = pd.read_sql_query(sql, conn, params=(snapshot_date,))
        df.to_csv(out_dir / f"{table}.csv", index=False)
        total += len(df)
        n_files += 1

    # Derived: monthly high-performer market summary by bedroom.
    if "market_time_series" in existing:
        df = pd.read_sql_query(
            _MARKET_HIGH_PERF_MONTHLY_SQL, conn, params=(snapshot_date,))
        if not df.empty:
            df.to_csv(out_dir / "market_high_performer_monthly.csv", index=False)
            total += len(df)
            n_files += 1

    print(f"  export: {total} rows across {n_files} files -> {out_dir}")
    return str(out_dir)
