"""Write a blank Excel template with the exact fields create_listings expects.

    python -m src.make_template                       # -> templates/new_listings.xlsx
    python -m src.make_template --out some/path.xlsx

Layout is **wide**: sheet 1 ("listings") has one COLUMN PER FIELD (header row =
field names) and one ROW PER LISTING — fill a row per property. Sheet 2
("column_guide") documents each field, section, and which are required.

Custom-field columns (`cf_*`) are appended from the cached definitions
(`data/custom_fields.json`); run `python -m src.export_template` (or pull) once
to populate that cache so custom fields appear here.
"""
import argparse

import pandas as pd

from .config import resolve
from .listing_mapper import COLUMNS, REQUIRED
from .custom_fields import load_definitions, cf_columns


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default="./templates/new_listings.xlsx")
    args = ap.parse_args()

    defs = load_definitions()
    cf_cols = cf_columns(defs)
    headers = [c[0] for c in COLUMNS] + cf_cols
    # Wide: one column per field, empty (one blank row so the sheet isn't headers-only).
    listings = pd.DataFrame([{h: "" for h in headers}], columns=headers)

    guide_rows = [
        {
            "section": section,
            "column": col,
            "required": "YES" if col in REQUIRED else "",
            "type": dtype,
            "notes": note,
        }
        for (col, dtype, section, note) in COLUMNS
    ]
    cf_defs = [d for d in defs if d.get("fieldId")]  # same order as cf_cols
    for col, d in zip(cf_cols, cf_defs):
        guide_rows.append({
            "section": "Custom fields",
            "column": col,
            "required": "",
            "type": d.get("type", ""),
            "notes": f'Guesty custom field "{d.get("key", "")}".',
        })

    guide = pd.DataFrame(guide_rows)

    out = resolve(args.out)
    out.parent.mkdir(parents=True, exist_ok=True)
    with pd.ExcelWriter(out, engine="openpyxl") as xw:
        listings.to_excel(xw, sheet_name="listings", index=False)
        guide.to_excel(xw, sheet_name="column_guide", index=False)

    print(f"Wrote template -> {out}  ({len(headers)} fields, {len(cf_cols)} custom)")
    print(f"Required columns: {', '.join(REQUIRED)}")


if __name__ == "__main__":
    main()
