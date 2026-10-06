"""All filesystem locations for the weekly revenue update, in one place."""
from pathlib import Path

PROJECT = Path(__file__).resolve().parent
DATA_DIR = PROJECT / "data"          # API pulls (git-ignored)
OUTPUT_DIR = PROJECT / "output"      # local copies of the report
RENT_ROLL = DATA_DIR / "LTR" / "Rent roll.xlsx"   # monthly long-term lease income

WORK = Path("/Users/ylin/ValtaWork")
REPORTING_CODE = WORK / "Data and Reporting"          # DataProcessing.py, RevenueReportHelpers.py
GUESTY_PROJECT = WORK / "Claude_projects" / "GuestyFinancials"
WHEELHOUSE_PROJECT = WORK / "Claude_projects" / "wheelhouse_marketdata_download"
WHEELHOUSE_DB = WHEELHOUSE_PROJECT / "data" / "wheelhouse.sqlite"

DRIVE = Path("/Users/ylin/Google Drive/My Drive/Data and Reporting")
REVENUE_DATA = DRIVE / "Data" / "Revenue"
# Published outputs (what the notebook used to write) — only with --publish
PUB_REPORT = DRIVE / "RevenueReport_upd.xlsx"
PUB_OCCUPANCY = DRIVE / "Valta_OccupancyRate.xlsx"
PUB_TRACKING_DIR = DRIVE / "03-Revenue & Pricing" / "Analytics" / "RevenueTrackings"
