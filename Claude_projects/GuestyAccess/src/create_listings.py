"""Create new Guesty listings from a prepared Excel sheet.

    # Always dry-run first — prints the exact payloads, calls NOTHING:
    python -m src.create_listings --input templates/new_listings.xlsx --dry-run

    # Then create for real (writes results to data/create_results.xlsx):
    python -m src.create_listings --input templates/new_listings.xlsx

    python -m src.create_listings --input <file> --limit 1   # create just the first row

Each row -> POST /listings, then (by default) PUT /listings/{id} with the same
full body. The PUT "enrich" step is because Guesty's create endpoint may ignore
some sections (publicDescription, amenities, detailed pricing) — its docs say to
create first, then set those via updates. Pass --no-enrich to POST only.
Listings are created UNLISTED unless the sheet says otherwise.
"""
import argparse
import json
import time

import pandas as pd

from .config import load_config, resolve
from .guesty_client import GuestyClient
from .listing_mapper import row_to_payload, COLUMNS, _blank
from .custom_fields import load_definitions, column_to_id

_KNOWN_FIELDS = {c[0] for c in COLUMNS}


def load_rows(path) -> list[dict]:
    """Read the sheet into one dict per listing.

    Auto-detects orientation:
      - TRANSPOSED (default template): column A holds field names, each other
        column is one listing.
      - WIDE (legacy): header row holds field names, each row is one listing.
    """
    df = pd.read_excel(path, dtype=object, header=0)
    if df.shape[0] == 0 or df.shape[1] == 0:
        return []

    first_col = df.columns[0]
    col0 = [str(v).strip() for v in df[first_col].tolist() if not _blank(v)]
    hits = sum(1 for v in col0 if v in _KNOWN_FIELDS)
    transposed = bool(col0) and hits >= max(3, len(col0) // 2)

    if transposed:
        fields = df[first_col].tolist()
        rows = []
        for col in df.columns[1:]:
            rec = {}
            for f, val in zip(fields, df[col].tolist()):
                if not _blank(f):
                    rec[str(f).strip()] = val
            if any(not _blank(v) for v in rec.values()):  # skip empty listing columns
                rows.append(rec)
        return rows

    # wide format: one listing per row
    df = df.dropna(how="all")
    return df.to_dict(orient="records")


def main():
    ap = argparse.ArgumentParser(description="Create Guesty listings from an Excel sheet.")
    ap.add_argument("--input", required=True, help="Path to the prepared .xlsx (see make_template.py).")
    ap.add_argument("--dry-run", action="store_true", help="Build + print payloads but do NOT call the API.")
    ap.add_argument("--limit", type=int, default=None, help="Only process the first N rows.")
    ap.add_argument("--out", default="./data/create_results.xlsx", help="Where to write per-row results.")
    ap.add_argument("--no-enrich", action="store_true",
                    help="POST only; skip the follow-up PUT that applies descriptions/amenities/pricing.")
    args = ap.parse_args()

    cfg = load_config()
    defaults = cfg.get("defaults", {})
    delay = float(cfg.get("create_delay_seconds", 1.0))

    rows = load_rows(resolve(args.input) if not str(args.input).startswith("/") else args.input)
    if args.limit is not None:
        rows = rows[: args.limit]
    print(f"Loaded {len(rows)} row(s) from {args.input}")

    # Map cf_<name> columns -> customFields via cached definitions.
    cf_map = column_to_id(load_definitions())

    # Validate/build every payload up front so a bad row fails before any writes.
    payloads = []
    for i, row in enumerate(rows, start=1):
        try:
            payloads.append(row_to_payload(row, defaults, cf_map))
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
            enrich_status = "skipped"
            enrich_err = ""
            # Follow-up update so descriptions/amenities/pricing reliably apply.
            if not args.no_enrich and new_id:
                try:
                    client.request("PUT", f"/listings/{new_id}", json_body=payload)
                    enrich_status = "ok"
                    print(f"        enriched (full fields applied)")
                except RuntimeError as e:
                    enrich_status = "failed"
                    enrich_err = str(e)
                    print(f"        WARN enrich PUT failed (listing still created): {e}")
            results.append({"nickname": nickname, "status": "created", "listing_id": new_id,
                            "enrich": enrich_status, "error": enrich_err})
        except RuntimeError as e:
            print(f"[{i}/{len(payloads)}] FAILED '{nickname}': {e}")
            results.append({"nickname": nickname, "status": "error", "listing_id": "",
                            "enrich": "n/a", "error": str(e)})
        time.sleep(delay)  # stay polite / under rate limits

    out = resolve(args.out)
    out.parent.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(results).to_excel(out, index=False)
    created = sum(1 for r in results if r["status"] == "created")
    print(f"\nDone. {created}/{len(results)} created. Results -> {out}")


if __name__ == "__main__":
    main()
