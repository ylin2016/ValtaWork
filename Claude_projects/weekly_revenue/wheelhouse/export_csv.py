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


# listings.csv gets extra columns:
#   market_name        — resolved from the listing's market_id
#   dynamic_set_ids    — the set(s) the listing is in (comma-joined)
#   dynamic_set_names  — the matching set name(s) (joined with ' | ', since set
#                        and market names themselves contain commas)
_LISTINGS_SQL = """
SELECT l.*,
       mk.name AS market_name,
       s.dynamic_set_ids,
       s.dynamic_set_names
FROM listings l
LEFT JOIN markets mk
  ON mk.market_id = l.market_id AND mk.snapshot_date = l.snapshot_date
LEFT JOIN (
  SELECT a.snapshot_date, a.user_listing_id,
         GROUP_CONCAT(a.set_id)        AS dynamic_set_ids,
         GROUP_CONCAT(d.name, ' | ')   AS dynamic_set_names
  FROM dynamic_set_associated_listings a
  LEFT JOIN dynamic_sets d
    ON d.set_id = a.set_id AND d.snapshot_date = a.snapshot_date
  GROUP BY a.snapshot_date, a.user_listing_id
) s ON s.snapshot_date = l.snapshot_date AND s.user_listing_id = l.listing_id
WHERE l.snapshot_date = ?
"""

# Compact monthly market summary for HIGH performers, by bedroom — pivoted from
# the pre-aggregated market_monthly table.
_MARKET_HIGH_PERF_MONTHLY_SQL = """
SELECT t.market_id,
       (SELECT name FROM markets m
        WHERE m.market_id = t.market_id AND m.snapshot_date = t.snapshot_date) AS market_name,
       t.bedrooms,
       t.month,
       MAX(CASE WHEN t.metric = 'adr_w_fees'         THEN t.value END) AS adr,
       MAX(CASE WHEN t.metric = 'occupancy_adjusted' THEN t.value END) AS occupancy_adjusted,
       MAX(CASE WHEN t.metric = 'revpar_w_fees'      THEN t.value END) AS revpar,
       MAX(t.days_in_avg) AS days_in_avg
FROM market_monthly t
WHERE t.snapshot_date = ? AND t.performance = 'high'
GROUP BY t.market_id, t.bedrooms, t.month
ORDER BY market_name, t.bedrooms, t.month
"""

# Monthly OCCUPANCY (high performers) per market, broken out by bedroom, for the
# current year only — restricted to the bedroom sizes we actually own in each
# market. `port` maps each of our listings to its market's bedroom bucket
# (0/1/2/3/4+, matching the market segments), so a market only shows the
# bedroom rows relevant to our portfolio there.
_MARKET_OCC_BY_BEDROOM_YEAR_SQL = """
WITH port AS (
  SELECT DISTINCT market_id,
         CASE WHEN bedrooms >= 4 THEN '4+'
              ELSE CAST(CAST(bedrooms AS INT) AS TEXT) END AS bedrooms
  FROM listings
  WHERE snapshot_date = ? AND market_id IS NOT NULL AND bedrooms IS NOT NULL
)
SELECT mm.market_id,
       (SELECT name FROM markets m
        WHERE m.market_id = mm.market_id AND m.snapshot_date = mm.snapshot_date) AS market_name,
       mm.bedrooms,
       mm.month,
       mm.value AS occupancy_adjusted
FROM market_monthly mm
JOIN port p ON p.market_id = mm.market_id AND p.bedrooms = mm.bedrooms
WHERE mm.snapshot_date = ?
  AND mm.performance = 'high'
  AND mm.metric = 'occupancy_adjusted'
  AND mm.month LIKE ? || '-%'
ORDER BY market_name, mm.bedrooms, mm.month
"""

# Monthly OCCUPANCY per dynamic set for the current year only, from the set's
# aggregated (monthly) metrics.
_SET_OCC_MONTHLY_YEAR_SQL = """
SELECT a.set_id,
       (SELECT name FROM dynamic_sets d
        WHERE d.set_id = a.set_id AND d.snapshot_date = a.snapshot_date) AS set_name,
       substr(a.period, 1, 7) AS month,
       a.value AS occupancy_adjusted
FROM dynamic_set_aggregated_metrics a
WHERE a.snapshot_date = ?
  AND a.metric = 'occupancy_adjusted'
  AND a.period LIKE ? || '-%'
ORDER BY set_name, month
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
    if "market_monthly" in existing:
        df = pd.read_sql_query(
            _MARKET_HIGH_PERF_MONTHLY_SQL, conn, params=(snapshot_date,))
        if not df.empty:
            df.to_csv(out_dir / "market_high_performer_monthly.csv", index=False)
            total += len(df)
            n_files += 1

    year = snapshot_date[:4]  # current-year occupancy views

    # Derived: monthly occupancy per market x bedroom (our portfolio's bedroom
    # sizes only), current year.
    if "market_monthly" in existing:
        df = pd.read_sql_query(
            _MARKET_OCC_BY_BEDROOM_YEAR_SQL, conn,
            params=(snapshot_date, snapshot_date, year))
        if not df.empty:
            df.to_csv(out_dir / f"market_occupancy_by_bedroom_{year}.csv", index=False)
            total += len(df)
            n_files += 1

    # Derived: monthly occupancy per dynamic set, current year.
    if "dynamic_set_aggregated_metrics" in existing:
        df = pd.read_sql_query(
            _SET_OCC_MONTHLY_YEAR_SQL, conn, params=(snapshot_date, year))
        if not df.empty:
            df.to_csv(out_dir / f"set_occupancy_monthly_{year}.csv", index=False)
            total += len(df)
            n_files += 1

    print(f"  export: {total} rows across {n_files} files -> {out_dir}")
    return str(out_dir)
