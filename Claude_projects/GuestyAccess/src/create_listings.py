"""Create new Guesty listings from a prepared Excel sheet.

    # Always dry-run first — prints the exact payloads, calls NOTHING:
    python -m src.create_listings --input templates/new_listings.xlsx --dry-run

    # Then create for real (writes results to data/create_results.xlsx):
    python -m src.create_listings --input templates/new_listings.xlsx

    python -m src.create_listings --input <file> --limit 1   # create just the first row

Each row -> POST /listings. For a standard single-unit property that one call
is sufficient (spaces / house-rules / complex endpoints are only needed for
multi-units). Listings are created UNLISTED unless the sheet says otherwise.
"""
import argparse
import json
import time

import pandas as pd

from .config import load_config, resolve
from .guesty_client import GuestyClient
from .listing_mapper import row_to_payload


def load_rows(path) -> list[dict]:
    df = pd.read_excel(path, dtype=object)
    # Drop fully-empty rows.
    df = df.dropna(how="all")
    return df.to_dict(orient="records")


def main():
    ap = argparse.ArgumentParser(description="Create Guesty listings from an Excel sheet.")
    ap.add_argument("--input", required=True, help="Path to the prepared .xlsx (see make_template.py).")
    ap.add_argument("--dry-run", action="store_true", help="Build + print payloads but do NOT call the API.")
    ap.add_argument("--limit", type=int, default=None, help="Only process the first N rows.")
    ap.add_argument("--out", default="./data/create_results.xlsx", help="Where to write per-row results.")
    args = ap.parse_args()

    cfg = load_config()
    defaults = cfg.get("defaults", {})
    delay = float(cfg.get("create_delay_seconds", 1.0))

    rows = load_rows(resolve(args.input) if not str(args.input).startswith("/") else args.input)
    if args.limit is not None:
        rows = rows[: args.limit]
    print(f"Loaded {len(rows)} row(s) from {args.input}")

    # Validate/build every payload up front so a bad row fails before any writes.
    payloads = []
    for i, row in enumerate(rows, start=1):
        try:
            payloads.append(row_to_payload(row, defaults))
        except ValueError as e:
            raise SystemExit(f"Row {i} ({row.get('nickname', '?')}): {e}")

    if args.dry_run:
        for i, p in enumerate(payloads, start=1):
            print(f"\n--- Row {i}: {p['nickname']} ---")
            print(json.dumps(p, indent=2))
        print(f"\nDRY RUN — no API calls made. {len(payloads)} listing(s) would be created.")
        return

    client = GuestyClient()
    results = []
    for i, (row, payload) in enumerate(zip(rows, payloads), start=1):
        nickname = payload["nickname"]
        try:
            resp = client.post("/listings", payload)
            new_id = resp.get("_id") or resp.get("id")
            print(f"[{i}/{len(payloads)}] created '{nickname}' -> {new_id}")
            results.append({"nickname": nickname, "status": "created", "listing_id": new_id, "error": ""})
        except RuntimeError as e:
            print(f"[{i}/{len(payloads)}] FAILED '{nickname}': {e}")
            results.append({"nickname": nickname, "status": "error", "listing_id": "", "error": str(e)})
        time.sleep(delay)  # stay polite / under rate limits

    out = resolve(args.out)
    out.parent.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(results).to_excel(out, index=False)
    created = sum(1 for r in results if r["status"] == "created")
    print(f"\nDone. {created}/{len(results)} created. Results -> {out}")


if __name__ == "__main__":
    main()
