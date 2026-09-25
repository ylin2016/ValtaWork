from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
CONFIG = ROOT / "config"
OUTPUT = ROOT / "output"
EXTRACTED = OUTPUT / "extracted"  # one JSON per source file, keyed by content hash

CATEGORIES_YML = CONFIG / "categories.yml"
OVERRIDES_CSV = CONFIG / "overrides.csv"
EXCLUDED_CSV = CONFIG / "excluded_files.csv"  # source files dropped from the report entirely
STATEMENT_CSV = CONFIG / "statement_months.csv"  # exceptions to the file-name-date rule
MANUAL_CSV = CONFIG / "manual_expenses.csv"  # costs with no receipt file (e.g. cleaner payouts)

# Google Drive for Desktop mount (billing@). Only readable from a terminal that
# macOS has granted Drive access to (e.g. VS Code), not from Claude's shell.
DEFAULT_SRC = Path(
    "/Users/ylin/Google Drive/My Drive/Company Transactions/2026/2026-08/"
)
DEFAULT_MATCH = ["OSBR", "Cottage"]

# Owner-statement months the report covers (build --months overrides)
REPORT_MONTHS = ["2026-07", "2026-08"]
