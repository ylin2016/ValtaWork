"""Fetch Yacinde reservations (status confirmed + canceled) from the Guesty Open API.

    python -m src.fetch_guesty --checkin-from 2026-06-01 --checkin-to 2026-12-31

Reuses GuestyFinancials' client (its .env + cached 24h token; Guesty allows only
5 tokens/24h per client, so never force-refresh). Writes the raw JSON to
data/guesty/guesty_reservations.json.
"""
import argparse
import importlib
import importlib.util
import json
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
GF_SRC = PROJECT_ROOT.parent / "GuestyFinancials" / "src"
OUT = PROJECT_ROOT / "data" / "guesty" / "guesty_reservations.json"

PAGE = 100
STATUSES = ["confirmed", "canceled"]
FIELDS = ("confirmationCode source status checkIn checkOut nightsCount guestsCount "
          "guests listing.nickname listing.title listing.accommodates listingId "
          "guest.fullName createdAt canceledAt")


def _load_client():
    """Import GuestyFinancials' client under a unique package name (both projects have `src`)."""
    pkg = "guestyfinancials_src"
    if pkg not in sys.modules:
        spec = importlib.util.spec_from_file_location(
            pkg, GF_SRC / "__init__.py", submodule_search_locations=[str(GF_SRC)])
        mod = importlib.util.module_from_spec(spec)
        sys.modules[pkg] = mod
        spec.loader.exec_module(mod)
    return importlib.import_module(f"{pkg}.guesty_client").GuestyClient


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

    # GuestyFinancials' config resolves .env / token paths relative to its own root.
    import os
    os.chdir(GF_SRC.parent)
    client = _load_client()()
    res = fetch(client, args.checkin_from, args.checkin_to)
    yac = [x for x in res if "yacinde" in ((x.get("listing") or {}).get("nickname") or "").lower()]
    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(json.dumps(yac, indent=1, default=str))
    print(f"{len(yac)} Yacinde reservations (of {len(res)}) -> {OUT}")


if __name__ == "__main__":
    main()
