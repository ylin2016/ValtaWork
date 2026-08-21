"""Export existing Guesty listings into the editable (wide) template.

    python -m src.export_template                         # all listings in data/listings.json
    python -m src.export_template --out templates/current_listings.xlsx
    python -m src.export_template --limit 10              # first 10 only

Reads the JSON produced by `pull_listings` and writes a workbook whose
"listings" sheet has one COLUMN PER FIELD (header row) and one ROW PER LISTING —
the same shape `create_listings` reads. This is the inverse of
`listing_mapper.row_to_payload`, so an exported row can be edited and fed
straight back into `create_listings` to clone a listing.

Named custom-field columns (`cf_*`) are included: their definitions are fetched
live (and cached to data/custom_fields.json); if the API is unreachable, the
cached definitions are used.
"""
import argparse
import json

import pandas as pd

from .config import resolve
from .listing_mapper import COLUMNS, REQUIRED
from . import custom_fields as cf


def _join(seq) -> str:
    return "|".join(str(x) for x in (seq or []))


def listing_to_row(x: dict, cf_id_to_col: dict | None = None) -> dict:
    """Guesty listing object -> template-field dict (inverse of row_to_payload)."""
    a = x.get("address", {}) or {}
    p = x.get("prices", {}) or {}
    t = x.get("terms", {}) or {}
    pd_ = x.get("publicDescription", {}) or {}
    pics = x.get("pictures", []) or []

    def b(v):
        return "" if v is None else ("true" if v is True else "false" if v is False else v)

    row = {
        # core / address
        "nickname": x.get("nickname", ""),
        "is_listed": b(x.get("isListed")),
        "address_full": a.get("full", ""),
        "city": a.get("city", ""), "state": a.get("state", ""),
        "zipcode": a.get("zipcode", ""), "country": a.get("country", ""),
        "lat": a.get("lat", ""), "lng": a.get("lng", ""),
        "timezone": x.get("timezone", ""),
        # details & layout
        "property_type": x.get("propertyType", ""), "room_type": x.get("roomType", ""),
        "accommodates": x.get("accommodates", ""), "bedrooms": x.get("bedrooms", ""),
        "bathrooms": x.get("bathrooms", ""), "beds": x.get("beds", ""),
        "area_square_feet": x.get("areaSquareFeet", ""), "minimum_age": x.get("minimumAge", ""),
        "default_check_in_time": x.get("defaultCheckInTime", ""),
        "default_check_in_end_time": x.get("defaultCheckInEndTime", ""),
        "default_check_out_time": x.get("defaultCheckOutTime", ""),
        "wifi_name": x.get("wifiName", ""), "wifi_password": x.get("wifiPassword", ""),
        "door_code": x.get("doorCode", ""), "lock_code": x.get("lockCode", ""),
        "parking_instructions": x.get("parkingInstructions", ""),
        "house_manual": x.get("houseManual", ""),
        "amenities": _join(x.get("amenities")),
        "amenities_not_included": _join(x.get("amenitiesNotIncluded")),
        # pricing & policies
        "base_price": p.get("basePrice", ""), "currency": p.get("currency", ""),
        "weekend_base_price": p.get("weekendBasePrice", ""),
        "weekend_days": _join(p.get("weekendDays")),
        "cleaning_fee": p.get("cleaningFee", ""), "extra_person_fee": p.get("extraPersonFee", ""),
        "guests_included": p.get("guestsIncludedInRegularFee", ""),
        "pet_fee": p.get("petFee", ""), "security_deposit": p.get("securityDepositFee", ""),
        "weekly_price_factor": p.get("weeklyPriceFactor", ""),
        "monthly_price_factor": p.get("monthlyPriceFactor", ""),
        "min_nights": t.get("minNights", ""), "max_nights": t.get("maxNights", ""),
        "use_account_taxes": b(x.get("useAccountTaxes")),
        "use_account_additional_fees": b(x.get("useAccountAdditionalFees")),
        # marketing
        "title": x.get("title", ""),
        "summary": pd_.get("summary", ""), "space": pd_.get("space", ""),
        "access": pd_.get("access", ""), "neighborhood": pd_.get("neighborhood", ""),
        "transit": pd_.get("transit", ""), "notes": pd_.get("notes", ""),
        "house_rules": pd_.get("houseRules", ""),
        "interaction_with_guests": pd_.get("interactionWithGuests", ""),
        "picture_urls": _join(pic.get("original", "") for pic in pics),
    }

    # custom fields: {fieldId, value} -> cf_<displayName> column
    if cf_id_to_col:
        for c in (x.get("customFields") or []):
            col = cf_id_to_col.get(c.get("fieldId"))
            if col:
                v = c.get("value", "")
                row[col] = "" if v is None else v

    return row


def _get_definitions():
    """Fetch custom-field definitions live (and cache); fall back to cache if offline."""
    try:
        from .guesty_client import GuestyClient
        defs = cf.fetch_and_cache(GuestyClient())
        print(f"Fetched {len(defs)} custom-field definition(s) from Guesty.")
        return defs
    except Exception as e:
        defs = cf.load_definitions()
        print(f"Could not fetch custom-field defs ({str(e)[:60]}); using {len(defs)} cached.")
        return defs


def main():
    ap = argparse.ArgumentParser(description="Export existing listings into the editable template.")
    ap.add_argument("--source", default="./data/listings.json", help="Listings JSON from pull_listings.")
    ap.add_argument("--out", default="./templates/current_listings.xlsx")
    ap.add_argument("--limit", type=int, default=None, help="Only export the first N listings.")
    args = ap.parse_args()

    src = resolve(args.source)
    if not src.exists():
        raise SystemExit(f"{src} not found — run `python -m src.pull_listings` first.")
    listings = json.loads(src.read_text())
    if args.limit is not None:
        listings = listings[: args.limit]
    if not listings:
        raise SystemExit("No listings to export.")

    defs = _get_definitions()
    cf_cols = cf.cf_columns(defs)
    cf_id_to_col = cf.id_to_column(defs)

    headers = [c[0] for c in COLUMNS] + cf_cols
    rows = [listing_to_row(x, cf_id_to_col) for x in listings]
    # Wide: one row per listing, columns = fields (+ custom fields).
    sheet = pd.DataFrame([{h: r.get(h, "") for h in headers} for r in rows], columns=headers)

    guide_rows = [{"section": sec, "column": c, "required": "YES" if c in REQUIRED else "", "type": dt, "notes": n}
                  for c, dt, sec, n in COLUMNS]
    for col, d in zip(cf_cols, [d for d in defs if d.get("fieldId")]):
        guide_rows.append({"section": "Custom fields", "column": col, "required": "",
                           "type": d.get("type", ""), "notes": f'Guesty custom field "{d.get("key", "")}".'})
    guide = pd.DataFrame(guide_rows)

    out = resolve(args.out)
    out.parent.mkdir(parents=True, exist_ok=True)
    with pd.ExcelWriter(out, engine="openpyxl") as xw:
        sheet.to_excel(xw, sheet_name="listings", index=False)
        guide.to_excel(xw, sheet_name="column_guide", index=False)

    print(f"Exported {len(rows)} listing(s) x {len(headers)} fields ({len(cf_cols)} custom) -> {out}")


if __name__ == "__main__":
    main()
