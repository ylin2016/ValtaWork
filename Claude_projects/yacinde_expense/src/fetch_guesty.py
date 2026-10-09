"""Fetch Yacinde reservations (status confirmed + canceled) from the Guesty Open API.

    python -m src.fetch_guesty --checkin-from 2026-06-01 --checkin-to 2026-12-31

Uses the shared Guesty client (valta_common, Claude_projects/shared/: one .env and one
cached 24h token for every project; Guesty allows only 5 tokens/24h per client, so never
force-refresh). Writes the
raw JSON to data/guesty/guesty_reservations.json.
"""
import argparse
import json
from pathlib import Path

from valta_common.guesty.client import GuestyClient

PROJECT_ROOT = Path(__file__).resolve().parent.parent
OUT = PROJECT_ROOT / "data" / "guesty" / "guesty_reservations.json"

PAGE = 100
STATUSES = ["confirmed", "canceled"]
FIELDS = ("confirmationCode source status checkIn checkOut nightsCount guestsCount "
          "guests listing.nickname listing.title listing.accommodates listingId "
          "guest.fullName createdAt canceledAt")


def fetch(client, dfrom: str, dto: str) -> list[dict]:
    filt = [
        {"field": "checkIn", "operator": "$gte", "value": dfrom},
        {"field": "checkIn", "operator": "$lte", "value": f"{dto}T23:59:59.999Z"},
        {"field": "status", "operator": "$in", "value": STATUSES},
    ]
    out, skip = [], 0
    while True:
        r = client.get("/reservations", params={
            "filters": json.dumps(filt), "fields": FIELDS,
            "limit": PAGE, "skip": skip, "sort": "checkIn"})
        batch = r.get("results", [])
        out.extend(batch)
        total = r.get("count", len(out))
        skip += len(batch)
        print(f"  fetched {len(out)}/{total}")
        if not batch or skip >= total:
            return out


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--checkin-from", default="2026-06-01")
    ap.add_argument("--checkin-to", default="2026-12-31")
    args = ap.parse_args()

    client = GuestyClient()
    res = fetch(client, args.checkin_from, args.checkin_to)
    yac = [x for x in res if "yacinde" in ((x.get("listing") or {}).get("nickname") or "").lower()]
    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(json.dumps(yac, indent=1, default=str))
    print(f"{len(yac)} Yacinde reservations (of {len(res)}) -> {OUT}")


if __name__ == "__main__":
    main()
