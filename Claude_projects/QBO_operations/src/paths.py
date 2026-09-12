"""Central path resolution for QBO_operations.

This project OWNS the QuickBooks connection for realm 9130356236278636: the OAuth
flow runs here and ``config/secrets/qbo_tokens.json`` is the one token store.
Owner_statement_whole reads that same file (see its ``paths.QBO_TOKENS``) rather
than keeping a copy -- Intuit rotates the refresh token on every refresh, so two
independent copies invalidate each other.

    config/            -- config.yml (realm) + accounts.yml (named QBO account Ids)
    config/secrets/    -- .env, qbo_tokens.json (git-ignored)
    inputs/JE/         -- channel payout exports (Airbnb / Booking.com CSVs)
    inputs/Invoice_payment/ -- the Zelle payment tracking workbook
    review/            -- generated review CSVs, reviewed BEFORE anything is posted

Dependency-free (only pathlib), like its opposite number in Owner_statement_whole.
"""
from pathlib import Path

# src/paths.py -> project root is two levels up.
PROJECT_ROOT = Path(__file__).resolve().parent.parent

CONFIG_DIR  = PROJECT_ROOT / "config"
SECRETS_DIR = CONFIG_DIR / "secrets"
INPUTS_DIR  = PROJECT_ROOT / "inputs"
JE_INPUTS   = INPUTS_DIR / "JE"
INVOICE_INPUTS = INPUTS_DIR / "Invoice_payment"
REVIEW_DIR  = PROJECT_ROOT / "review"

CONFIG_YML   = CONFIG_DIR / "config.yml"
ACCOUNTS_YML = CONFIG_DIR / "accounts.yml"
PAYEES_YML   = CONFIG_DIR / "payees.yml"

ENV_FILE   = SECRETS_DIR / ".env"
QBO_TOKENS = SECRETS_DIR / "qbo_tokens.json"

# --- the read-only sibling this project draws reference data from ----------------
# One direction only: QBO_operations reads Owner_statement_whole, never the reverse.
# Every use goes through src/bridge.py, and every consumer takes a CLI override.
STATEMENTS_ROOT = PROJECT_ROOT.parent / "Owner_statement_whole"


def review_csv(name: str) -> Path:
    """review/<name>.csv -- the file a human signs off on before a post."""
    return REVIEW_DIR / (name if name.endswith(".csv") else f"{name}.csv")
