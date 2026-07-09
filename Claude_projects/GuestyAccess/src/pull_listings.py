"""Pull property/listing data from Guesty and save it locally.

    python -m src.pull_listings                 # all listings -> data/listings.{json,xlsx}
    python -m src.pull_listings --limit 5        # just peek at a few
    python -m src.pull_listings --fields _id,nickname,title,address.full,prices.basePrice

Guesty paginates listings; this walks every page. Output is written both as
raw JSON (full nested objects, for downstream code) and a flattened .xlsx
(one row per listing, for eyeballing in Excel).
"""
import argparse
import json

import pandas as pd

from .config import resolve, load_config
from .guesty_client import GuestyClient

PAGE_SIZE = 100  # Guesty max per page


def fetch_all_listings(client: GuestyClient, fields: str | None = None, limit: int | None = None) -> list[dict]:
    """Return every listing (or up to `limit`), walking pagination."""
    results: list[dict] = []
    skip = 0
    while True:
        page_limit = PAGE_SIZE if limit is None else min(PAGE_SIZE, limit - len(results))
        params = {"limit": page_limit, "skip": skip}
        if fields:
            params["fields"] = fields
        data = client.get("/listings", params=params)
        batch = data.get("results", data.get("data", []))
        results.extend(batch)

        total = data.get("count", data.get("total", len(results)))
        print(f"  fetched {len(results)}/{total}")

        if limit is not None and len(results) >= limit:
            return results[:limit]
        skip += len(batch)
        if not batch or skip >= total:
            return results


def main():
    ap = argparse.ArgumentParser(description="Pull listings from Guesty Open API.")
    ap.add_argument("--limit", type=int, default=None, help="Max listings to fetch (default: all).")
    ap.add_argument("--fields", default=None,
                    help="Comma-separated Guesty field paths to return (default: all fields).")
    ap.add_argument("--out", default="./data/listings", help="Output path stem (writes .json and .xlsx).")
    args = ap.parse_args()

    load_config()
    client = GuestyClient()

    print("Fetching listings…")
    listings = fetch_all_listings(client, fields=args.fields, limit=args.limit)
    print(f"Done. {len(listings)} listing(s).")

    stem = resolve(args.out)
    stem.parent.mkdir(parents=True, exist_ok=True)

    json_path = stem.with_suffix(".json")
    json_path.write_text(json.dumps(listings, indent=2, default=str))
    print(f"Wrote {json_path}")

    # Flatten nested objects (address.full, prices.basePrice, …) for Excel.
    df = pd.json_normalize(listings)
    xlsx_path = stem.with_suffix(".xlsx")
    df.to_excel(xlsx_path, index=False)
    print(f"Wrote {xlsx_path} ({df.shape[0]} rows x {df.shape[1]} cols)")


if __name__ == "__main__":
    main()
