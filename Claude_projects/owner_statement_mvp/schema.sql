-- Owner Statement MVP schema (SQLite)
PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS owners (
  owner_id TEXT PRIMARY KEY,
  owner_name TEXT NOT NULL,
  owner_email TEXT
);

CREATE TABLE IF NOT EXISTS properties (
  property_id TEXT PRIMARY KEY,
  property_name TEXT NOT NULL,
  owner_id TEXT NOT NULL,
  qbo_class_id TEXT NOT NULL,
  qbo_class_name TEXT,
  guesty_listing_id TEXT,
  is_active INTEGER NOT NULL DEFAULT 1,
  FOREIGN KEY (owner_id) REFERENCES owners(owner_id)
);

CREATE TABLE IF NOT EXISTS owner_contracts (
  contract_id TEXT PRIMARY KEY,
  property_id TEXT NOT NULL,
  effective_start TEXT NOT NULL,
  effective_end TEXT,
  statement_basis TEXT NOT NULL CHECK(statement_basis IN ('cash','accrual')),
  pm_fee_type TEXT NOT NULL CHECK(pm_fee_type IN ('percent','flat')),
  pm_fee_rate REAL,
  pm_flat_fee REAL,
  pm_fee_base TEXT NOT NULL CHECK(pm_fee_base IN ('net_booking_revenue','rent_only','gross_including_cleaning')),
  reserve_target REAL NOT NULL DEFAULT 0,
  cleaning_fee_to_owner INTEGER NOT NULL DEFAULT 1,
  taxes_included_in_revenue INTEGER NOT NULL DEFAULT 0,
  notes TEXT,
  FOREIGN KEY (property_id) REFERENCES properties(property_id)
);

CREATE TABLE IF NOT EXISTS ledger_lines (
  ledger_id TEXT PRIMARY KEY,
  source TEXT NOT NULL CHECK(source IN ('qbo','guesty','manual')),
  source_object TEXT NOT NULL,
  source_txn_id TEXT NOT NULL,
  source_line_id TEXT,
  property_id TEXT NOT NULL,
  booking_id TEXT,
  payout_id TEXT,
  posting_date TEXT NOT NULL,
  service_date TEXT,
  category TEXT NOT NULL CHECK(category IN ('INCOME','EXPENSE','FEE','TAX','OWNER_ADJ','RESERVE','TRANSFER')),
  subcategory TEXT NOT NULL,
  description TEXT,
  vendor_customer TEXT,
  qbo_account TEXT,
  amount REAL NOT NULL,
  currency TEXT NOT NULL DEFAULT 'USD',
  include_in_statement INTEGER NOT NULL DEFAULT 1,
  status TEXT NOT NULL DEFAULT 'posted',
  last_updated_at TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  FOREIGN KEY (property_id) REFERENCES properties(property_id)
);

CREATE INDEX IF NOT EXISTS idx_ledger_property_date ON ledger_lines(property_id, posting_date);
CREATE INDEX IF NOT EXISTS idx_ledger_source ON ledger_lines(source, source_object, source_txn_id);

CREATE TABLE IF NOT EXISTS exceptions (
  exception_id TEXT PRIMARY KEY,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  source TEXT NOT NULL,
  source_object TEXT NOT NULL,
  source_txn_id TEXT NOT NULL,
  source_line_id TEXT,
  property_id TEXT,
  severity TEXT NOT NULL CHECK(severity IN ('info','warning','error')),
  code TEXT NOT NULL,
  message TEXT NOT NULL,
  payload_json TEXT
);

CREATE INDEX IF NOT EXISTS idx_exceptions_property ON exceptions(property_id, created_at);

CREATE TABLE IF NOT EXISTS statement_runs (
  run_id TEXT PRIMARY KEY,
  period_start TEXT NOT NULL,
  period_end TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  basis TEXT NOT NULL CHECK(basis IN ('cash','accrual')),
  status TEXT NOT NULL CHECK(status IN ('draft','locked')) DEFAULT 'draft',
  notes TEXT
);

CREATE TABLE IF NOT EXISTS statement_property_totals (
  run_id TEXT NOT NULL,
  property_id TEXT NOT NULL,
  gross_booking_revenue REAL NOT NULL DEFAULT 0,
  taxes REAL NOT NULL DEFAULT 0,
  total_expenses REAL NOT NULL DEFAULT 0,
  total_fees REAL NOT NULL DEFAULT 0,
  owner_adjustments REAL NOT NULL DEFAULT 0,
  reserve_withheld REAL NOT NULL DEFAULT 0,
  reserve_released REAL NOT NULL DEFAULT 0,
  net_before_reserve REAL NOT NULL DEFAULT 0,
  net_after_reserve REAL NOT NULL DEFAULT 0,
  amount_due_to_owner REAL NOT NULL DEFAULT 0,
  PRIMARY KEY (run_id, property_id),
  FOREIGN KEY (run_id) REFERENCES statement_runs(run_id),
  FOREIGN KEY (property_id) REFERENCES properties(property_id)
);

CREATE TABLE IF NOT EXISTS statement_outputs (
  run_id TEXT NOT NULL,
  property_id TEXT NOT NULL,
  output_path TEXT NOT NULL,
  output_sha256 TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  PRIMARY KEY (run_id, property_id),
  FOREIGN KEY (run_id) REFERENCES statement_runs(run_id),
  FOREIGN KEY (property_id) REFERENCES properties(property_id)
);
