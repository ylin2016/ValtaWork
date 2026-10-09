"""Central path resolution for QBO_operations.

This project OWNS the QuickBooks connection for realm 9130356236278636: the OAuth
flow runs here. The one token store is the shared
``Claude_projects/shared/secrets/qbo_tokens.json`` (with the shared ``.env``), which
Owner_statement_whole and qbo_reservation_bookkeeping read too -- Intuit rotates the
refresh token on every refresh, so two independent copies invalidate each other.

    config/            -- config.yml (realm) + accounts.yml (named QBO account Ids)
    ../shared/secrets/ -- .env, qbo_tokens.json (git-ignored; shared by every project)
    ../shared/reference/mapping_classes.yml -- property_id -> QBO class (read-only here)
    inputs/JE/         -- channel payout exports (Airbnb / Booking.com CSVs)
    inputs/Invoice_payment/ -- the Zelle payment tracking workbook
    inputs/owner_payout/    -- the monthly owner-payout sheet (one row per bank line)
    review/            -- generated review CSVs, reviewed BEFORE anything is posted

Dependency-free (only pathlib), like its opposite number in Owner_statement_whole.
"""
from pathlib import Path

# src/paths.py -> project root is two levels up.
PROJECT_ROOT = Path(__file__).resolve().parent.parent

CONFIG_DIR  = PROJECT_ROOT / "config"
SHARED_DIR  = PROJECT_ROOT.parent / "shared"          # Claude_projects/shared (see its README)
SECRETS_DIR = SHARED_DIR / "secrets"
INPUTS_DIR  = PROJECT_ROOT / "inputs"
JE_INPUTS   = INPUTS_DIR / "JE"
INVOICE_INPUTS = INPUTS_DIR / "Invoice_payment"
OWNER_PAYOUT_INPUTS = INPUTS_DIR / "owner_payout"
REVIEW_DIR  = PROJECT_ROOT / "review"

CONFIG_YML   = CONFIG_DIR / "config.yml"
ACCOUNTS_YML = CONFIG_DIR / "accounts.yml"
PAYEES_YML   = CONFIG_DIR / "payees.yml"
PAYOUT_CLASSES_YML = CONFIG_DIR / "payout_classes.yml"
BOOKING_IDS  = CONFIG_DIR / "booking_Id.csv"
MAPPING_CLASSES = SHARED_DIR / "reference" / "mapping_classes.yml"   # maintained by Owner_statement_whole

ENV_FILE   = SECRETS_DIR / ".env"
QBO_TOKENS = SECRETS_DIR / "qbo_tokens.json"

# --- the read-only sibling this project draws reference data from ----------------
# One direction only: QBO_operations reads Owner_statement_whole, never the reverse.
# Every use goes through src/bridge.py, and every consumer takes a CLI override.
STATEMENTS_ROOT = PROJECT_ROOT.parent / "Owner_statement_whole"


def review_csv(name: str) -> Path:
    """review/<name>.csv -- the file a human signs off on before a post."""
    return REVIEW_DIR / (name if name.endswith(".csv") else f"{name}.csv")
