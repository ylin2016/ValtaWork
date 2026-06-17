"""
Update mapping_classes.yml with Term field from Listing_contacts.csv
"""
import csv
import yaml
import re
from pathlib import Path

BASE = Path(__file__).parent.parent

def norm(s):
    """Normalize property name for matching."""
    s = str(s).strip().lower()
    s = re.sub(r"[^a-z0-9]+", "_", s)
    s = re.sub(r"_+", "_", s).strip("_")
    return s

# Parse Listing_contacts.csv
csv_lookup = {}
osbr_fallback = None

with open(BASE / "data/Listing_contacts.csv", encoding="utf-8-sig") as f:
    for row in csv.DictReader(f):
        prop = (row.get("Property") or "").strip()
        if not prop:
            continue

        term = (row.get("Term") or "").strip().upper()

        entry = {
            "term": term if term else None,
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
    pid = m.get("property_id", "")
    pname = norm(m.get("property_name", ""))

    entry = csv_lookup.get(pname)

    # OSBR properties: try cottage_N first, then osbr_all fallback
    if not entry and pid.startswith("osbr"):
        num = pid.replace("osbr_", "")
        entry = csv_lookup.get(f"cottage_{num}") or osbr_fallback

    if not entry:
        skipped += 1
        continue

    if entry.get("term"):
        m["term"] = entry["term"]
    updated += 1

print(f"Updated: {updated}  |  No CSV match (unchanged): {skipped}")

# Write back
with open(mapping_path, "w") as f:
    yaml.dump(mappings, f, default_flow_style=False, allow_unicode=True, sort_keys=False)

print(f"Saved {mapping_path}")
