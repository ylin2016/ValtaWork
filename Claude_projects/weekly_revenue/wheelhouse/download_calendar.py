"""Night-by-night calendar of our own listings (/listings/{id}/price_calendar).

Each night is booked (a guest reservation), blocked (unavailable but not booked:
owner stays, maintenance, pre-launch) or open. The revenue report subtracts blocked
nights from the occupancy denominator so our occupancy is on the same basis as
Wheelhouse's occupancy_adjusted. Past nights keep their state, so one call per
listing covers the whole history window.

Only the latest snapshot is kept (the table is ~90k rows per pull).
"""
from datetime import datetime, timezone

from . import db
from .transforms import year_window


def _state(night: dict) -> str:
    if night.get("is_booked"):
        return "booked"
    return "open" if night.get("is_available") else "blocked"


def download_calendar(client, conn, cfg, snapshot_date: str) -> int:
    """Calendar for every listing stored in `listings` this snapshot (the real
    portfolio harvested from the dynamic sets). Returns nights stored."""
    p = cfg.get("params", {})
    start_date, end_date = year_window(snapshot_date, p.get("history_years", 2))
    listings = conn.execute(
        "SELECT listing_id, channel, name FROM listings WHERE snapshot_date = ?",
        (snapshot_date,)).fetchall()
    path = cfg["endpoints"]["listing_calendar"]
    jobs = [(path.format(listing_id=r["listing_id"]),
             {"channel": r["channel"] or "guesty", "start_date": start_date, "end_date": end_date},
             f"calendar:{r['listing_id']}") for r in listings]
    bodies = client.fetch_many(jobs)

    pulled_at = datetime.now(timezone.utc).isoformat()
    rows, skipped = [], 0
    for r, (body, err) in zip(listings, bodies):
        if err is not None or not isinstance(body, list):
            skipped += 1
            print(f"    calendar:{r['name']} skipped: {err or 'unexpected payload'}")
            continue
        rows += [{"snapshot_date": snapshot_date, "listing_id": r["listing_id"], "name": r["name"],
                  "stay_date": n["stay_date"], "state": _state(n), "pulled_at": pulled_at}
                 for n in body if n.get("stay_date")]

    conn.execute("DELETE FROM listing_calendar WHERE snapshot_date <> ?", (snapshot_date,))
    conn.commit()
    db.upsert(conn, "listing_calendar", rows)
    blocked = sum(1 for x in rows if x["state"] == "blocked")
    print(f"  listing_calendar: {len(rows)} nights for {len(listings) - skipped} listings "
          f"({blocked} blocked), {start_date} .. {end_date}")
    return len(rows)
