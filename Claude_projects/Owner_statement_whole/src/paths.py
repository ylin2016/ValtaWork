"""Central path resolution for Owner_statement_whole.

Every directory / file location lives here so the rest of the code never
hardcodes a path. This is the ONE place that knows the project layout:

    config/            — this project's own config: config.yml, mapping_accounts.yml, …
    ../shared/reference/ — the hand-maintained tables other projects read too
                         (Listing_contacts.csv, mapping_classes.yml, listing_tax_rates.csv);
                         this project still maintains them
    ../shared/secrets/ — .env (QBO + Guesty + Wheelhouse), qbo_tokens.json, guesty_token.json
                         — ONE copy for every project (git-ignored)
    inputs/<period>/   — per-month input CSVs (Guesty booking, converted, LTR, breakdown)
    output/<period>/   — per-month generated statements
    db/                — the single shared SQLite ledger (accumulates across months)

Dependency-free (only pathlib) so it can be imported both as a package module
(`python -m src.run_month_close`) and by scripts that add ``src`` to sys.path
(the Streamlit dashboard). It is also copied VERBATIM into deploy/, which has no
``shared/`` beside it, so the reference tables fall back to deploy/config/ there.
"""
from pathlib import Path

# src/paths.py -> project root is two levels up.
PROJECT_ROOT = Path(__file__).resolve().parent.parent

# --- top-level directories ---
CONFIG_DIR    = PROJECT_ROOT / "config"
# Claude_projects/shared/ (see its README). Absent in the hosted deploy bundle, whose
# packaged config/ then holds the reference tables it needs.
SHARED_DIR    = PROJECT_ROOT.parent / "shared"
REFERENCE_DIR = SHARED_DIR / "reference" if (SHARED_DIR / "reference").is_dir() else CONFIG_DIR
SECRETS_DIR   = SHARED_DIR / "secrets"
INPUTS_DIR    = PROJECT_ROOT / "inputs"
OUTPUT_DIR    = PROJECT_ROOT / "output"
DB_DIR        = PROJECT_ROOT / "db"
TEMPLATES_DIR = PROJECT_ROOT / "templates"

# --- config / reference files (rarely change) ---
CONFIG_YML        = CONFIG_DIR / "config.yml"
MAPPING_CLASSES   = REFERENCE_DIR / "mapping_classes.yml"     # shared
MAPPING_ACCOUNTS  = CONFIG_DIR / "mapping_accounts.yml"
LISTING_CONTACTS  = REFERENCE_DIR / "Listing_contacts.csv"     # shared
PAYMENT_STRUCTURE = CONFIG_DIR / "payment_structure.xlsx"
LISTING_TAX_RATES = REFERENCE_DIR / "listing_tax_rates.csv"    # shared
CHANNEL_MARKUPS   = CONFIG_DIR / "_channel_markups.json"   # Guesty account-level markups (cached API pull)
SCHEMA_SQL        = PROJECT_ROOT / "schema.sql"

# --- standalone tools (not per-month) ---
CHANNEL_CALCULATOR = OUTPUT_DIR / "channel_pricing_calculator.xlsx"

# --- secrets (shared/secrets/: one copy for every project) ---
ENV_FILE     = SECRETS_DIR / ".env"
GUESTY_TOKEN = SECRETS_DIR / "guesty_token.json"

# The QBO OAuth flow runs in QBO_operations, but the token store is the shared one.
# Intuit rotates the refresh token on every refresh, so there is exactly ONE copy on
# disk and every project reads and writes back to it -- a second copy would invalidate
# that one the first time either refreshed.  This project only ever READS QuickBooks
# (see expense/qbo_client.py).
QBO_TOKENS     = SECRETS_DIR / "qbo_tokens.json"

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
