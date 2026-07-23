-- Wheelhouse market-data & dynamic-set downloader schema.
-- Every row is tagged with snapshot_date (the weekly pull date) and pulled_at (UTC).
-- Primary keys include snapshot_date so re-running a pull for the same week is
-- idempotent via INSERT OR REPLACE. raw_json preserves the full API payload so no
-- field is ever lost even before it is normalized into its own column.

-- Safety net: every raw API response, keyed by endpoint + reference id.
CREATE TABLE IF NOT EXISTS raw_responses (
  snapshot_date TEXT NOT NULL,
  endpoint      TEXT NOT NULL,
  ref_id        TEXT NOT NULL DEFAULT '',   -- listing_id / market_id / set_id / page
  status        INTEGER,
  json          TEXT,
  pulled_at     TEXT NOT NULL,
  PRIMARY KEY (snapshot_date, endpoint, ref_id)
);

-- Our listings (foundation for every per-listing pull).
CREATE TABLE IF NOT EXISTS listings (
  snapshot_date TEXT NOT NULL,
  listing_id    TEXT NOT NULL,
  channel       TEXT,
  name          TEXT,
  bedrooms      REAL,
  market_id     TEXT,
  raw_json      TEXT,
  pulled_at     TEXT NOT NULL,
  PRIMARY KEY (snapshot_date, listing_id)
);

-- ---------------------------------------------------------------------------
-- Market data
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS markets (
  snapshot_date TEXT NOT NULL,
  market_id     TEXT NOT NULL,
  name          TEXT,
  raw_json      TEXT,
  pulled_at     TEXT NOT NULL,
  PRIMARY KEY (snapshot_date, market_id)
);

CREATE TABLE IF NOT EXISTS market_time_series (
  snapshot_date TEXT NOT NULL,
  market_id     TEXT NOT NULL,
  date          TEXT NOT NULL,
  metric        TEXT NOT NULL,
  value         REAL,
  raw_json      TEXT,
  PRIMARY KEY (snapshot_date, market_id, date, metric)
);

-- Distribution rows: value = probability for a [bucket_min, bucket_max) bucket.
-- bucket_min/bucket_max/percentile carry the full histogram bar.
CREATE TABLE IF NOT EXISTS market_distributions (
  snapshot_date TEXT NOT NULL,
  market_id     TEXT NOT NULL,
  month         TEXT NOT NULL,
  metric        TEXT NOT NULL,
  bucket        TEXT NOT NULL,
  bucket_min    REAL,
  bucket_max    REAL,
  probability   REAL,
  percentile    REAL,
  value         REAL,
  raw_json      TEXT,
  PRIMARY KEY (snapshot_date, market_id, month, metric, bucket)
);

CREATE TABLE IF NOT EXISTS neighborhood_pricing (
  snapshot_date TEXT NOT NULL,
  listing_id    TEXT NOT NULL,
  date          TEXT NOT NULL,
  metric        TEXT NOT NULL,
  value         REAL,
  raw_json      TEXT,
  PRIMARY KEY (snapshot_date, listing_id, date, metric)
);

CREATE TABLE IF NOT EXISTS neighborhood_occupancy (
  snapshot_date TEXT NOT NULL,
  listing_id    TEXT NOT NULL,
  date          TEXT NOT NULL,
  metric        TEXT NOT NULL,
  value         REAL,
  raw_json      TEXT,
  PRIMARY KEY (snapshot_date, listing_id, date, metric)
);

-- ---------------------------------------------------------------------------
-- Dynamic sets ("dynamite sets") — comparable-property groupings
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS dynamic_sets (
  snapshot_date TEXT NOT NULL,
  set_id        TEXT NOT NULL,
  name          TEXT,
  listing_count REAL,
  raw_json      TEXT,
  pulled_at     TEXT NOT NULL,
  PRIMARY KEY (snapshot_date, set_id)
);

-- Which of OUR listings are associated with each set (listing -> set link).
CREATE TABLE IF NOT EXISTS dynamic_set_associated_listings (
  snapshot_date   TEXT NOT NULL,
  set_id          TEXT NOT NULL,
  user_listing_id TEXT NOT NULL,
  raw_json        TEXT,
  PRIMARY KEY (snapshot_date, set_id, user_listing_id)
);

-- Comp (member) listings that make up each set, grouped by membership status
-- (active / hidden / review / removed).
CREATE TABLE IF NOT EXISTS dynamic_set_members (
  snapshot_date     TEXT NOT NULL,
  set_id            TEXT NOT NULL,
  member_listing_id TEXT NOT NULL,
  status            TEXT NOT NULL DEFAULT '',
  raw_json          TEXT,
  PRIMARY KEY (snapshot_date, set_id, member_listing_id, status)
);

CREATE TABLE IF NOT EXISTS dynamic_set_aggregated_metrics (
  snapshot_date TEXT NOT NULL,
  set_id        TEXT NOT NULL,
  metric        TEXT NOT NULL,
  period        TEXT NOT NULL DEFAULT '',
  value         REAL,
  raw_json      TEXT,
  PRIMARY KEY (snapshot_date, set_id, metric, period)
);

CREATE TABLE IF NOT EXISTS dynamic_set_time_series (
  snapshot_date TEXT NOT NULL,
  set_id        TEXT NOT NULL,
  date          TEXT NOT NULL,
  metric        TEXT NOT NULL,
  value         REAL,
  raw_json      TEXT,
  PRIMARY KEY (snapshot_date, set_id, date, metric)
);

CREATE TABLE IF NOT EXISTS dynamic_set_distributions (
  snapshot_date TEXT NOT NULL,
  set_id        TEXT NOT NULL,
  month         TEXT NOT NULL,
  metric        TEXT NOT NULL,
  bucket        TEXT NOT NULL,
  bucket_min    REAL,
  bucket_max    REAL,
  probability   REAL,
  percentile    REAL,
  value         REAL,
  raw_json      TEXT,
  PRIMARY KEY (snapshot_date, set_id, month, metric, bucket)
);

CREATE TABLE IF NOT EXISTS dynamic_set_changelog (
  snapshot_date TEXT NOT NULL,
  set_id        TEXT NOT NULL,
  change_date   TEXT NOT NULL,
  change_type   TEXT NOT NULL DEFAULT '',
  detail        TEXT,
  raw_json      TEXT,
  PRIMARY KEY (snapshot_date, set_id, change_date, change_type, detail)
);
