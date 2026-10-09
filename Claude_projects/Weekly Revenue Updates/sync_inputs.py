"""Refresh data/inputs/ from Google Drive.

The run only ever reads the copies in data/inputs/, so the project works without
Drive. On a machine with Google Drive for desktop, this copies over any Drive file
that changed (run_weekly.py calls it first; `--no-sync` skips it). Without Drive it
prints a note and the existing copies are used.

    python sync_inputs.py
    VALTA_DRIVE="/path/to/My Drive" python sync_inputs.py   # Drive mounted elsewhere
"""
import argparse
import filecmp
import shutil

from paths import (DRIVE_ROOT, GUESTY_2025, GUESTY_BF2025, GUESTY_CANCELED, HIST_2023,
                   LRT_BOOKINGS, OVERALL_RATINGS, OWNER_PAYOUT_DIR, PROPERTY_COHOST,
                   REVIEWS_DIR, SOURCE_PLATFORM)

DR = "Data and Reporting"
PAYOUT = "Accounting/* Monthly/0-Process & Template"
# Drive path (under My Drive) -> local copy
FILES = {
    f"{DR}/Data/Property_Cohost.xlsx": PROPERTY_COHOST,
    f"{DR}/Data/Revenue/Source_Platform.xlsx": SOURCE_PLATFORM,
    f"{DR}/Data/Revenue/Guesty_bookings_bf2025.csv": GUESTY_BF2025,
    f"{DR}/Data/Revenue/Guesty_bookings_2025.csv": GUESTY_2025,
    f"{DR}/Data/Revenue/LRT_bookings.xlsx": LRT_BOOKINGS,
    f"{DR}/Data/Revenue/Property_OverallRatings.xlsx": OVERALL_RATINGS,
    f"{DR}/Data/Revenue/GuestyCanceled.csv": GUESTY_CANCELED,
    f"{DR}/Input_PowerBI/Guesty_PastBooking_airbnb_adj_12312023.csv":
        HIST_2023 / "Guesty_PastBooking_airbnb_adj_12312023.csv",
    f"{DR}/Input_PowerBI/Rev_CH_2023.csv": HIST_2023 / "Rev_CH_2023.csv",
    f"{DR}/Input_PowerBI/VRBO_20200101-20231230.csv": HIST_2023 / "VRBO_20200101-20231230.csv",
    f"{PAYOUT}/Old files/2024 OwnerPayout Records.xlsx": OWNER_PAYOUT_DIR / "2024 OwnerPayout Records.xlsx",
    f"{PAYOUT}/Old files/2025 OwnerPayout Records.xlsx": OWNER_PAYOUT_DIR / "2025 OwnerPayout Records.xlsx",
    f"{PAYOUT}/01-OwnerPayout Records.xlsx": OWNER_PAYOUT_DIR / "01-OwnerPayout Records.xlsx",
}
# Folder whose newest "* guesty_reviews.xlsx" is copied into REVIEWS_DIR
REVIEWS = "** Properties ** -- Valta/0_Cohosting/1-Reviews/Guesty reviews from Tech team"


def _copy(src, dst) -> bool:
    if dst.exists() and filecmp.cmp(src, dst, shallow=True):
        return False
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(src, dst)
    return True


def main(argv=None):
    argparse.ArgumentParser(description=__doc__.splitlines()[0]).parse_args(argv)
    if not DRIVE_ROOT.exists():
        print(f"  inputs: no Google Drive at {DRIVE_ROOT} — using the copies in data/inputs/")
        return
    changed, missing = [], []
    for rel, dst in FILES.items():
        src = DRIVE_ROOT / rel
        if not src.exists():
            missing.append(rel)
        elif _copy(src, dst):
            changed.append(dst.name)
    reviews = sorted((DRIVE_ROOT / REVIEWS).glob("* guesty_reviews.xlsx"))
    if reviews and _copy(reviews[-1], REVIEWS_DIR / reviews[-1].name):
        changed.append(reviews[-1].name)
    print(f"  inputs: {len(changed)} refreshed from Drive" + (f" ({', '.join(changed)})" if changed else ""))
    for rel in missing:
        print(f"  inputs: WARNING not on Drive, keeping the old copy: {rel}")


if __name__ == "__main__":
    main()
