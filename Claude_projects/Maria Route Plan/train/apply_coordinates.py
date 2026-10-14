"""Copy a filled coordinates sheet into data/Listings.xlsx and record units.

    .venv/bin/python train/apply_coordinates.py [--sheet data/Coordinates_to_fill.xlsx] [--dry-run]

For each row of the sheet (Tab, Listing, Address, Lat, Lng) the matching Listings.xlsx row
gets the sheet's Address, Lat, Lng and GeoSource = "entered". Then, on every row of both
tabs, a Unit column records the unit, if there is one, and UnitSource says where it came from:
  address       unit text in the address ("unit 315", "Apt B306", "Suite #111", "#T154") —
                moved out of Address into Unit, so addresses are street + city only
  old address   the unit was in the address before your correction
  listing name  the name states it: Elektra 1004 / Beachwood 3 (unit number), 14507U1 → U1,
                Seattle 98 U503 → 503, Microsoft 14620-E205 → E205, 1424C → C,
                Lower / Upper / ADU / Main / Whole
Nothing else is inferred. Rows whose address still ends in a bare letter (e.g. "… Ave S a")
are printed for you to check.
"""
import argparse
import re

import pandas as pd
from openpyxl import load_workbook

import dataset as d

UNIT_IN_ADDR = re.compile(r'\s*,?\s*\b(?:unit|apt|apartment|suite|ste)\b\.?\s*#?\s*([A-Za-z0-9-]+)|\s*,?\s*#\s*([A-Za-z0-9-]+)', re.I)


def unit_from_address(addr):
    """(unit, address without it) — only explicit 'unit / apt / suite / #' markers."""
    if not isinstance(addr, str): return None, addr
    m = UNIT_IN_ADDR.search(addr)
    if not m: return None, addr
    unit = m.group(1) or m.group(2)
    clean = (addr[:m.start()] + addr[m.end():]).strip()
    clean = re.sub(r'\s*,\s*,', ',', clean).strip(' ,')
    return unit, clean


def unit_from_name(name):
    n = str(name).strip()
    if m := re.match(r'(?i)^(elektra|beachwood)\s*#?\s*(\d+)$', n): return m[2]
    if m := re.search(r'(?i)\d+\s*(U\d+)$', n): return m[1].upper()                 # Bellevue 14507U1
    if m := re.search(r'(?i)\s(?:U)(\d+)$', n): return m[1]                         # Seattle 98 U503
    if m := re.search(r'(?i)\d+-([A-Za-z]?\d+)$', n): return m[1].upper()           # Microsoft 14620-E205
    if m := re.search(r'(?i)\s(lower|upper|adu|main|whole)$', n): return m[1].title() if m[1].lower() != 'adu' else 'ADU'
    if m := re.search(r'\d+([A-Za-z])$', n): return m[1].upper()                    # Seattle 1424C
    return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--sheet', default=str(d.DATA / 'Coordinates_to_fill.xlsx'))
    ap.add_argument('--dry-run', action='store_true')
    args = ap.parse_args()
    filled = pd.concat([pd.read_excel(args.sheet, s) for s in pd.ExcelFile(args.sheet).sheet_names])
    filled = filled[filled.Listing.notna()]
    by = {(r.Tab, str(r.Listing).strip()): r for r in filled.itertuples()}

    wb = load_workbook(d.DATA / 'Listings.xlsx')
    changes, units, check = [], [], []
    for tab in ('STR', 'Residential'):
        ws = wb[tab]
        head = [c.value for c in ws[1]]
        for col in ('Unit', 'UnitSource', 'Lat', 'Lng', 'GeoSource'):
            if col not in head:
                ws.cell(1, len(head) + 1).value = col
                head.append(col)
        ci = {h: i + 1 for i, h in enumerate(head)}
        for row in range(2, ws.max_row + 1):
            name = ws.cell(row, ci['Listing']).value
            if not name: continue
            old = ws.cell(row, ci['Address']).value
            new = old
            f = by.get((tab, str(name).strip()))
            if f is not None:
                if pd.notna(f.Address) and str(f.Address).strip() != str(old).strip():
                    new = str(f.Address).strip()
                if pd.notna(f.Lat) and pd.notna(f.Lng):
                    ws.cell(row, ci['Lat']).value = float(f.Lat)
                    ws.cell(row, ci['Lng']).value = float(f.Lng)
                    ws.cell(row, ci['GeoSource']).value = 'entered'
            unit, src = None, None
            u_new, new_clean = unit_from_address(new)
            if u_new: unit, src, new = u_new, 'address', new_clean
            elif (u_old := unit_from_address(old)[0]) and new != old:
                unit, src = u_old, 'old address'
            elif (u_name := unit_from_name(name)):
                unit, src = u_name, 'listing name'
            if new != old:
                ws.cell(row, ci['Address']).value = new
                changes.append((tab, name, old, new))
            if unit and not ws.cell(row, ci['Unit']).value:
                ws.cell(row, ci['Unit']).value = unit
                ws.cell(row, ci['UnitSource']).value = src
                units.append((tab, name, unit, src))
            if isinstance(new, str) and re.search(r'\s(?![NSEWnsew]$)[A-Za-z]$', new.split(',')[0]):   # not a direction
                check.append((tab, name, new))
    print(f'Address updated on {len(changes)} rows:')
    for t, n, o, nw in changes: print(f'  {t:<12}{n:<22}{str(o)[:48]:<50}→ {nw}')
    print(f'\nUnit recorded on {len(units)} rows:')
    for t, n, u, s in units: print(f'  {t:<12}{n:<24}{u:<8}({s})')
    if check:
        print('\nStill ends in a bare letter — is it a unit? (left as is):')
        for t, n, a in check: print(f'  {t:<12}{n:<22}{a}')
    n_ll = len(filled[filled.Lat.notna() & filled.Lng.notna()])
    print(f'\nLat/Lng from the sheet: {n_ll} rows.')
    if args.dry_run:
        print('(dry run — Listings.xlsx not changed)')
    else:
        wb.save(d.DATA / 'Listings.xlsx')
        print('Saved data/Listings.xlsx')


if __name__ == '__main__':
    main()
