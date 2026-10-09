"""
Update mapping_classes.yml with Owner Clean and Tax to Owner flags from Listing_contacts.csv
"""
import csv, yaml, re
from pathlib import Path

BASE = Path(__file__).parent.parent

def norm(s):
    s = str(s).strip().lower()
    s = re.sub(r"[^a-z0-9]+", "_", s)
    s = re.sub(r"_+", "_", s).strip("_")
    return s

# Parse CSV
csv_lookup = {}
osbr_fallback = None

with open(BASE / "templates/Listing_contacts.csv", encoding="utf-8-sig") as f:
    for row in csv.DictReader(f):
        prop = (row.get("Property") or "").strip()
        if not prop:
            continue
        mcr_raw = (row.get("MCR%") or "").strip().rstrip("%")
        try:
            rate = round(float(mcr_raw) / 100, 4)
        except ValueError:
            rate = None

        owner = (row.get("Owner") or "").strip().replace("\n", " & ")
        email = ((row.get("Email") or "").strip().split("\n")[0]).strip()

        owner_clean = (row.get("Owner Clean") or "").strip().lower() == "owner"
        tax_to_owner = (row.get("Tax to Owner") or "").strip().lower() == "owner"
        supplies = (row.get("Supplies") or "").strip().lower()
        owner_pays_supplies = supplies == "owner"

        entry = {
            "owner_name":  owner or None,
            "owner_email": email or None,
            "pm_fee_rate": rate,
            "owner_pays_cleaning": owner_clean,
            "owner_pays_taxes": tax_to_owner,
            "owner_pays_supplies": owner_pays_supplies,
        }
        key = norm(prop)
        csv_lookup[key] = entry
        if key == "osbr_all":
            osbr_fallback = entry

# Load mapping
mapping_path = BASE / "mapping_classes.yml"
with open(mapping_path) as f:
    mappings = yaml.safe_load(f)

# Match & update
updated = skipped = 0
for m in mappings:
    pid   = m.get("property_id", "")
    pname = norm(m.get("property_name", ""))

    entry = csv_lookup.get(pname)

    # OSBR properties: try cottage_N first, then osbr_all fallback
    if not entry and pid.startswith("osbr"):
        num = pid.replace("osbr_", "")
        entry = csv_lookup.get(f"cottage_{num}") or osbr_fallback

    if not entry:
        skipped += 1
        continue

    if entry.get("owner_name"):
        m["owner_name"] = entry["owner_name"]
    if entry.get("owner_email") is not None:
        m["owner_email"] = entry["owner_email"]
    if entry.get("pm_fee_rate") is not None:
        m["pm_fee_rate"] = entry["pm_fee_rate"]
    if entry.get("owner_pays_cleaning"):
        m["owner_pays_cleaning"] = True
    if entry.get("owner_pays_taxes"):
        m["owner_pays_taxes"] = True
    if entry.get("owner_pays_supplies"):
        m["owner_pays_supplies"] = True
    updated += 1

print(f"Updated: {updated}  |  No CSV match (unchanged): {skipped}")

# Write back
with open(mapping_path, "w") as f:
    yaml.dump(mappings, f, default_flow_style=False, allow_unicode=True, sort_keys=False)

print(f"Saved {mapping_path}")
