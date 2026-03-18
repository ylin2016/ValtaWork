from pathlib import Path
import re
import yaml

from src.config import load_config
from src.qbo_client import QBOClient

cfg = load_config("config.yml")
q = cfg["qbo"]

qbo = QBOClient(
    realm_id=q["realm_id"],
    base_url=q["base_url"],
    minorversion=int(q.get("minorversion", 75)),
)

start = 1
page_size = 100
target_total = 128  # change if needed

all_classes = []
seen_ids = set()

while len(all_classes) < target_total:
    res = qbo.query("select * from Class", start_position=start, max_results=page_size)
    classes = res.get("QueryResponse", {}).get("Class", []) or []

    if not classes:
        break

    new_count = 0
    for c in classes:
        cid = str(c.get("Id"))
        if cid not in seen_ids:
            seen_ids.add(cid)
            all_classes.append(c)
            new_count += 1

    if new_count == 0:
        print("Repeated page detected, stopping.")
        break

    start += page_size


def slugify(text: str) -> str:
    text = text.strip().lower()
    text = text.replace("&", "and")
    text = re.sub(r"[^a-z0-9]+", "_", text)
    text = re.sub(r"_+", "_", text).strip("_")
    return text


def parse_class_name(full_name: str) -> tuple[str, str]:
    """
    Example:
      Listings:Bainbridge 11143
    returns:
      property_id   = prop_bainbridge_11143
      property_name = Bainbridge 11143
    """
    parts = [p.strip() for p in full_name.split(":") if p.strip()]

    # Use last segment as the actual property/listing name
    listing_name = parts[-1] if parts else full_name.strip()

    property_id = f"{slugify(listing_name)}"
    property_name = listing_name

    return property_id, property_name


def infer_owner_fields(property_name: str) -> tuple[str, str]:
    """
    Example:
      Bainbridge 11143
    returns:
      owner_id   = owner_bainbridge_11143
      owner_name = Bainbridge 11143 Owner
    """

    owner_slug = slugify(property_name)

    owner_id = f"owner_{owner_slug}"
    owner_name = f"{property_name} Owner"

    return owner_id, owner_name


rows = []
for c in all_classes:
    if not c.get("Active", True):
        continue

    qbo_class_name = c.get("FullyQualifiedName", c.get("Name", "")).strip()
    property_id, property_name = parse_class_name(qbo_class_name)
    owner_id, owner_name = infer_owner_fields(property_name)

    rows.append({
        "qbo_class_id": str(c["Id"]),
        "qbo_class_name": qbo_class_name,
        "property_id": property_id,
        "property_name": property_name,
        "owner_id": owner_id,
        "owner_name": owner_name,
        "owner_email": "",
    })

out = Path("mapping_classes_stub.yml")
out.write_text(
    yaml.safe_dump(rows, sort_keys=False, allow_unicode=True),
    encoding="utf-8"
)

print(f"Wrote {len(rows)} classes to {out}")