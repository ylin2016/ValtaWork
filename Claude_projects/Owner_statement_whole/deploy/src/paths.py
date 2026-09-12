"""Central path resolution for Owner_statement_whole.

Every directory / file location lives here so the rest of the code never
hardcodes a path. This is the ONE place that knows the project layout:

    config/            — human-maintained mappings + config.yml + payment_structure.xlsx
    config/secrets/    — .env, qbo_tokens.json, guesty_token.json (git-ignored)
    inputs/<period>/   — per-month input CSVs (Guesty booking, converted, LTR, breakdown)
    output/<period>/   — per-month generated statements
    db/                — the single shared SQLite ledger (accumulates across months)

Dependency-free (only pathlib) so it can be imported both as a package module
(`python -m src.run_month_close`) and by scripts that add ``src`` to sys.path
(the Streamlit dashboard).
"""
from pathlib import Path

# src/paths.py -> project root is two levels up.
PROJECT_ROOT = Path(__file__).resolve().parent.parent

# --- top-level directories ---
CONFIG_DIR    = PROJECT_ROOT / "config"
SECRETS_DIR   = CONFIG_DIR / "secrets"
INPUTS_DIR    = PROJECT_ROOT / "inputs"
OUTPUT_DIR    = PROJECT_ROOT / "output"
DB_DIR        = PROJECT_ROOT / "db"
TEMPLATES_DIR = PROJECT_ROOT / "templates"

# --- config / reference files (rarely change) ---
CONFIG_YML        = CONFIG_DIR / "config.yml"
MAPPING_CLASSES   = CONFIG_DIR / "mapping_classes.yml"
MAPPING_ACCOUNTS  = CONFIG_DIR / "mapping_accounts.yml"
LISTING_CONTACTS  = CONFIG_DIR / "Listing_contacts.csv"
PAYMENT_STRUCTURE = CONFIG_DIR / "payment_structure.xlsx"
LISTING_TAX_RATES = CONFIG_DIR / "_listing_tax_rates.csv"
CHANNEL_MARKUPS   = CONFIG_DIR / "_channel_markups.json"   # Guesty account-level markups (cached API pull)
SCHEMA_SQL        = PROJECT_ROOT / "schema.sql"

# --- standalone tools (not per-month) ---
CHANNEL_CALCULATOR = OUTPUT_DIR / "channel_pricing_calculator.xlsx"

# --- secrets ---
ENV_FILE     = SECRETS_DIR / ".env"
GUESTY_TOKEN = SECRETS_DIR / "guesty_token.json"

# The QBO token store belongs to the QBO_operations project, which owns the
# QuickBooks connection and runs the OAuth flow.  Intuit rotates the refresh token
# on every refresh, so there is exactly ONE copy on disk and both projects share
# it -- a second copy here would invalidate that one the first time either
# refreshed.  This project only ever READS QuickBooks (see expense/qbo_client.py).
QBO_OPERATIONS = PROJECT_ROOT.parent / "QBO_operations"
QBO_TOKENS     = QBO_OPERATIONS / "config" / "secrets" / "qbo_tokens.json"

# --- the single shared ledger DB (NOT per-month: balances roll forward) ---
DB_PATH = DB_DIR / "owner_statement.sqlite"


# --- per-month locations -----------------------------------------------------
def inputs_dir(period: str) -> Path:
    """inputs/<period>/  (e.g. inputs/2026-07/)."""
    return INPUTS_DIR / period


def output_dir(period: str) -> Path:
    """output/<period>/."""
    return OUTPUT_DIR / period


def guesty_booking_csv(period: str) -> Path:
    """Raw Guesty-UI-shaped booking export for the month (Task 1 output A)."""
    return inputs_dir(period) / f"Guesty_booking_{period}.csv"


def guesty_converted_csv(period: str) -> Path:
    """convert_guesty_export.py output, per month."""
    return inputs_dir(period) / "guesty_converted.csv"


def ltr_csv(period: str) -> Path:
    """LTR / deferred-revenue input for the month."""
    return inputs_dir(period) / f"LTR_{period}.csv"


def payment_breakdown_csv(period: str) -> Path:
    """Per-booking Guesty fee waterfall for Section 1 (Task 1 output B)."""
    return inputs_dir(period) / f"payment_breakdown_{period}.csv"
