"""All filesystem locations for the weekly revenue update, in one place.

Everything the run reads or writes lives inside this project folder, except:
  * the shared databases in Claude_projects/shared_data/ (wheelhouse.sqlite,
    guesty.sqlite) — this project refreshes them weekly and other projects read
    them (override the folder with VALTA_SHARED_DATA);
  * the optional Google Drive folder: `sync_inputs.py` refreshes the live inputs
    from it and `--publish` writes the Excel copies to it; both are skipped when
    it is absent.
"""
import os
from pathlib import Path

PROJECT = Path(__file__).resolve().parent
CONFIG_DIR = PROJECT / "config"
SECRETS_DIR = PROJECT / "secrets"            # .env + cached Guesty token (git-ignored)
DATA_DIR = PROJECT / "data"                  # API pulls + inputs (git-ignored)
OUTPUT_DIR = PROJECT / "output"              # local copies of the report

ENV_FILE = SECRETS_DIR / ".env"
GUESTY_TOKEN = SECRETS_DIR / "guesty_token.json"
LISTING_TAX_RATES = CONFIG_DIR / "listing_tax_rates.csv"   # copy of Owner_statement_whole's

# Shared databases (written by this project's weekly run, read by other projects)
SHARED_DATA = Path(os.environ.get("VALTA_SHARED_DATA", PROJECT.parent / "shared_data"))
WHEELHOUSE_DB = SHARED_DATA / "wheelhouse.sqlite"
GUESTY_DB = SHARED_DATA / "guesty.sqlite"

# Static and slow-changing inputs (copies of the Google Drive files; see sync_inputs.py)
INPUTS = DATA_DIR / "inputs"
RENT_ROLL = DATA_DIR / "LTR" / "Rent roll.xlsx"   # monthly long-term lease income
PROPERTY_COHOST = INPUTS / "Property_Cohost.xlsx"
SOURCE_PLATFORM = INPUTS / "Source_Platform.xlsx"
GUESTY_BF2025 = INPUTS / "Guesty_bookings_bf2025.csv"
GUESTY_2025 = INPUTS / "Guesty_bookings_2025.csv"
LRT_BOOKINGS = INPUTS / "LRT_bookings.xlsx"
HIST_2023 = INPUTS / "history_2023"          # the notebook's Input_PowerBI files
OVERALL_RATINGS = INPUTS / "Property_OverallRatings.xlsx"
GUESTY_CANCELED = INPUTS / "GuestyCanceled.csv"
REVIEWS_DIR = INPUTS / "reviews"             # newest "* guesty_reviews.xlsx" is used
OWNER_PAYOUT_DIR = INPUTS / "owner_payout"   # 2024 / 2025 / 01- (current) OwnerPayout Records.xlsx

# Optional Google Drive folder (override with VALTA_DRIVE=/path/to/My Drive)
DRIVE_ROOT = Path(os.environ.get("VALTA_DRIVE", Path.home() / "Google Drive" / "My Drive"))
DRIVE = DRIVE_ROOT / "Data and Reporting"
# Published outputs (what the notebook used to write) — only with --publish
PUB_REPORT = DRIVE / "RevenueReport_upd.xlsx"
PUB_OCCUPANCY = DRIVE / "Valta_OccupancyRate.xlsx"
PUB_TRACKING_DIR = DRIVE / "03-Revenue & Pricing" / "Analytics" / "RevenueTrackings"
