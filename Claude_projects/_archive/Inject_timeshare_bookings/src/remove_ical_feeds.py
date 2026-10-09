"""Disconnect the old Yacinde iCal feeds from Guesty (the pre-migration blocks).

The earlier approach subscribed each Yacinde listing to a static netlify iCal
feed (`Yacinde_*_pre`), which shows as imported-calendar blocks on the calendar.
Those blocks occupy the dates we now want to fill with real owner reservations,
so they must be removed first.

This deletes each imported-calendar entity via the Open API with the
`remove_future_channel_events` strategy: future iCal blocks are cleared and the
feed stops syncing, while past blocks are kept as history. The feed is
re-addable later from its URL if ever needed.

Only feeds whose name ends with `_pre` (and that point at the netlify host) are
touched, so unrelated calendars are never removed.

Usage:
    python -m src.remove_ical_feeds --dry-run      # list what would be removed
    python -m src.remove_ical_feeds                # remove for real
"""
from __future__ import annotations

import argparse

from .inject_reservations import GuestyClient, load_listing_map, unwrap_items

STRATEGY = "remove_future_channel_events"
NETLIFY_HOST = "venerable-tapioca-07dda5.netlify.app"


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--dry-run", action="store_true", help="list feeds, delete nothing")
    args = ap.parse_args()

    client = GuestyClient()
    targets = []
    for nn, lid in sorted(load_listing_map().items()):
        res = client.get("/icalendar-api/imported-calendars", params={"listingId": lid})
        cals = unwrap_items(res)
        for cal in cals:
            name = cal.get("name") or ""
            url = cal.get("url") or ""
            cal_id = cal.get("id") or cal.get("_id")
            if name.endswith("_pre") and NETLIFY_HOST in url:
                targets.append((nn, cal_id, name, url))

    print(f"\nFound {len(targets)} old iCal feed(s) to remove (strategy={STRATEGY}):")
    for nn, cal_id, name, url in targets:
        print(f"  {nn}: {name}  id={cal_id}")

    if args.dry_run:
        print("\nDRY RUN — nothing deleted.")
        return 0

    removed = 0
    for nn, cal_id, name, url in targets:
        path = f"/icalendar-api/imported-calendars/{cal_id}"
        try:
            client.request("DELETE", path, params={"strategy": STRATEGY})
        except Exception as e:  # noqa: BLE001
            print(f"  {nn}: ERROR removing {name}: {e}")
            continue
        removed += 1
        print(f"  {nn}: removed {name}")

    print(f"\nDone. Removed {removed}/{len(targets)} iCal feed(s).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
