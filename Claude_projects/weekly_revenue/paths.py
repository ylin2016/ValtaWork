"""All filesystem locations for the weekly revenue update, in one place.

Everything the run reads or writes lives inside this project folder, except:
  * Claude_projects/shared/ (valta_common; see its README): the shared code (Guesty
    client, fee rules, Wheelhouse client), the reference tax-rate table, the ONE copy
    of the API secrets, and the shared databases (wheelhouse.sqlite, guesty.sqlite) —
    this project refreshes those weekly and other projects read them;
  * the optional Google Drive folder: `sync_inputs.py` refreshes the live inputs
    from it and `--publish` writes the Excel copies to it; both are skipped when
    it is absent.
"""
import os
from pathlib import Path

# Shared databases (written by this project's weekly run, read by other projects),
# the shared tax-rate table and the shared secrets
from valta_common.paths import ENV_FILE, GUESTY_DB, LISTING_TAX_RATES, WHEELHOUSE_DB  # noqa: F401

PROJECT = Path(__file__).resolve().parent
DATA_DIR = PROJECT / "data"                  # API pulls + inputs (git-ignored)
OUTPUT_DIR = PROJECT / "output"              # local copies of the report

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
