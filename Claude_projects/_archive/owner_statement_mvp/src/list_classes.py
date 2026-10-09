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
page_size = 200
target_total = 128

all_classes = []
seen_ids = set()

print("Starting QBO Class pull...")

while len(all_classes) < target_total:
    print(f"Fetching start_position={start}, max_results={page_size}")
    res = qbo.query("select * from Class", start_position=start, max_results=page_size)

    classes = res.get("QueryResponse", {}).get("Class", []) or []
    print(f"Received {len(classes)} classes")

    if not classes:
        print("No more classes returned.")
        break

    new_count = 0
    for c in classes:
        cid = str(c.get("Id"))
        if cid not in seen_ids:
            seen_ids.add(cid)
            all_classes.append(c)
            new_count += 1

    print(f"Added {new_count} new classes")
    print(f"Collected {len(all_classes)} / {target_total}")

    if new_count == 0:
        print("No new IDs returned; stopping to avoid infinite loop.")
        break

    start += page_size

print(f"\nFinal unique classes fetched: {len(all_classes)}")
for c in all_classes:
    print(f'{c["Id"]} | {c.get("FullyQualifiedName", c.get("Name", ""))} | Active={c.get("Active")}')