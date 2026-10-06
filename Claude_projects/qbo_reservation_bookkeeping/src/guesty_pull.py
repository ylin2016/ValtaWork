"""Pull every Guesty reservation created or changed since a cutoff, and store it.

    python -m src.guesty_pull --since 2026-09-14T00:00:00Z

Guesty allows 5 tokens / 24h shared across ALL projects, so this reuses
Owner_statement_whole's cached token and stores the raw result under
inputs/guesty/reservations_<stamp>.json. Builders read the stored file -- never pull
from a builder.

Filtering on `lastUpdatedAt` covers both new bookings (future check-ins too: VRP invoiced
a booking as soon as it appeared) and changes to old ones (alterations, cancellations).
"""
from __future__ import annotations

import argparse
import json
from datetime import datetime, timezone

from . import bridge
from .paths import GUESTY_INPUTS

PAGE = 100
FIELDS = ("_id confirmationCode source status checkIn checkOut checkInDateLocalized "
          "checkOutDateLocalized nightsCount guestsCount listingId listing.nickname "
          "listing.title listing.timezone guest.fullName money specialRequests "
          "createdAt lastUpdatedAt canceledAt integration.platform "
          "integration.bookingCom.reservationId integration.channelManager.externalReservationId")


def pull(since: str) -> list[dict]:
    client = bridge.guesty_client()
    filt = [{"field": "lastUpdatedAt", "operator": "$gte", "value": since}]
    out, skip, total = [], 0, None
    while True:
        r = client.get("/reservations", params={
            "filters": json.dumps(filt), "fields": FIELDS,
            "limit": PAGE, "skip": skip, "sort": "_id"})
        batch = r.get("results", [])
        out.extend(batch)
        total = r.get("count", len(out))
        print(f"  fetched {len(out)}/{total}")
        skip += len(batch)
        if not batch or skip >= total:
            break
    uniq = list({x["_id"]: x for x in out}.values())
    if len(uniq) < total:
        print(f"  !! {total - len(uniq)} missing after dedupe -- pagination skipped rows; re-run.")
    _fill_booking_ids(client, uniq)
    return uniq


def _fill_booking_ids(client, rows: list[dict]) -> None:
    """The list endpoint returns `integration.bookingCom` EMPTY, and Booking.com's own
    reservation number is VRP's DocNumber for those bookings -- so fetch it one by one."""
    need = [r for r in rows if str(r.get("source", "")).lower() == "booking.com"]
    for n, r in enumerate(need, 1):
        full = client.get(f"/reservations/{r['_id']}", params={"fields": "integration"})
        bc = ((full.get("integration") or {}).get("bookingCom") or {})
        r.setdefault("integration", {})["bookingCom"] = {"reservationId": bc.get("reservationId")}
        if n % 25 == 0 or n == len(need):
            print(f"  booking.com ids {n}/{len(need)}")
    missing = [r["confirmationCode"] for r in need if not r["integration"]["bookingCom"]["reservationId"]]
    if missing:
        print(f"  !! no Booking.com reservationId on {len(missing)}: {missing[:10]}")


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[1])
    ap.add_argument("--since", required=True, help="UTC ISO instant, e.g. 2026-09-14T00:00:00Z")
    a = ap.parse_args()
    rows = pull(a.since)
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    GUESTY_INPUTS.mkdir(parents=True, exist_ok=True)
    p = GUESTY_INPUTS / f"reservations_{stamp}.json"
    p.write_text(json.dumps({"since": a.since, "pulled_at": stamp, "results": rows},
                            indent=1, default=str))
    print(f"{len(rows)} reservation(s) -> {p}")


if __name__ == "__main__":
    main()
