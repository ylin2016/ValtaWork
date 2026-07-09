"""Write a blank Excel template with the exact columns create_listings expects.

    python -m src.make_template                       # -> templates/new_listings.xlsx
    python -m src.make_template --out some/path.xlsx

Sheet 1 ("listings") is where you fill in your rows. Sheet 2 ("column_guide")
documents each column and marks the required ones.
"""
import argparse

import pandas as pd

from .config import resolve
from .listing_mapper import COLUMNS, REQUIRED


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default="./templates/new_listings.xlsx")
    args = ap.parse_args()

    headers = [c[0] for c in COLUMNS]
    listings = pd.DataFrame(columns=headers)

    guide = pd.DataFrame(
        [
            {
                "column": col,
                "required": "YES" if col in REQUIRED else "",
                "type": dtype,
                "notes": note,
            }
            for (col, dtype, note) in COLUMNS
        ]
    )

    out = resolve(args.out)
    out.parent.mkdir(parents=True, exist_ok=True)
    with pd.ExcelWriter(out, engine="openpyxl") as xw:
        listings.to_excel(xw, sheet_name="listings", index=False)
        guide.to_excel(xw, sheet_name="column_guide", index=False)

    print(f"Wrote template -> {out}")
    print(f"Required columns: {', '.join(REQUIRED)}")


if __name__ == "__main__":
    main()
