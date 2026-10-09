"""Locations in Claude_projects/shared/ (override the folder with VALTA_SHARED).

    data/        guesty.sqlite, wheelhouse.sqlite — written weekly by weekly_revenue,
                 read-only for everyone else
    reference/   hand-maintained tables (Owner_statement_whole maintains them)
    secrets/     .env (QBO, Guesty, Wheelhouse) + the ONE Guesty and QBO token caches
"""
import os
from pathlib import Path

SHARED = Path(os.environ.get("VALTA_SHARED", Path(__file__).resolve().parent.parent))

DATA_DIR = SHARED / "data"
GUESTY_DB = DATA_DIR / "guesty.sqlite"
WHEELHOUSE_DB = DATA_DIR / "wheelhouse.sqlite"

REFERENCE_DIR = SHARED / "reference"
LISTING_CONTACTS = REFERENCE_DIR / "Listing_contacts.csv"
MAPPING_CLASSES = REFERENCE_DIR / "mapping_classes.yml"
LISTING_TAX_RATES = REFERENCE_DIR / "listing_tax_rates.csv"

SECRETS_DIR = SHARED / "secrets"
ENV_FILE = SECRETS_DIR / ".env"
GUESTY_TOKEN = SECRETS_DIR / "guesty_token.json"
QBO_TOKENS = SECRETS_DIR / "qbo_tokens.json"


def read_only(db: Path) -> str:
    """sqlite3 URI that opens a shared DB read-only: sqlite3.connect(read_only(p), uri=True)."""
    return f"file:{db}?mode=ro"
