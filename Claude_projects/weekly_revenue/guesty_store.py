"""The shared Guesty database (Claude_projects/shared/data/guesty.sqlite).

Written by fetch_guesty.py on every weekly run; any project may read it (open it
read-only: sqlite3.connect(f"file:{path}?mode=ro", uri=True)). Tables:

  listings          every Guesty listing (the Open API's /listings, active or not)
  reservations      CONFIRMED reservations with local check-in >= the pull's
                    checkin_from (2026-01-01): key fields + the API record as raw_json.
                    Each pull replaces that whole scope, so a booking cancelled since
                    the last pull disappears. Deactivated listings' bookings are not in
                    the Open API, so they are not here (see weekly_revenue's
                    UI-export supplement).
  reservation_fees  per-reservation fee itemization (valta_common.guesty.summary.build_summary_frame,
                    the layout Owner_statement_whole's payment model reads)
  pulls             one row per table per pull: when, scope, row count
"""
import json
import sqlite3
from datetime import datetime, timezone

import pandas as pd

from paths import GUESTY_DB

LISTING_FIELDS = ("nickname title active listed bedrooms bathrooms accommodates "
                  "propertyType roomType address tags")

SCHEMA = """
CREATE TABLE IF NOT EXISTS listings (
    listing_id TEXT PRIMARY KEY, nickname TEXT, title TEXT, active INTEGER, listed INTEGER,
    bedrooms REAL, bathrooms REAL, accommodates INTEGER, property_type TEXT, room_type TEXT,
    address_full TEXT, city TEXT, state TEXT, zipcode TEXT, lat REAL, lng REAL,
    tags TEXT, pulled_at TEXT, raw_json TEXT);
CREATE TABLE IF NOT EXISTS reservations (
    confirmation_code TEXT PRIMARY KEY, reservation_id TEXT, listing_id TEXT,
    listing_nickname TEXT, status TEXT, source TEXT, platform TEXT,
    check_in TEXT, check_out TEXT, nights INTEGER, guests INTEGER,
    confirmed_at TEXT, created_at TEXT, currency TEXT, host_payout REAL, total_paid REAL,
    total_refunded REAL, fare_accommodation REAL, fare_cleaning REAL, guest_name TEXT,
    pulled_at TEXT, raw_json TEXT);
CREATE INDEX IF NOT EXISTS reservations_listing ON reservations (listing_nickname, check_in);
CREATE TABLE IF NOT EXISTS pulls (
    pulled_at TEXT, asof TEXT, table_name TEXT, scope TEXT, rows INTEGER);
"""


def _connect():
    GUESTY_DB.parent.mkdir(parents=True, exist_ok=True)
    con = sqlite3.connect(GUESTY_DB)
    con.executescript(SCHEMA)
    return con


def _now():
    return datetime.now(timezone.utc).isoformat(timespec="seconds")


def write_listings(listings: list[dict], asof: str) -> None:
    """Replace the listings table with a full /listings pull."""
    now = _now()
    rows = []
    for r in listings:
        a = r.get("address") or {}
        rows.append((r.get("_id"), r.get("nickname"), r.get("title"), r.get("active"), r.get("listed"),
                     r.get("bedrooms"), r.get("bathrooms"), r.get("accommodates"),
                     r.get("propertyType"), r.get("roomType"), a.get("full"), a.get("city"),
                     a.get("state"), a.get("zipcode"), a.get("lat"), a.get("lng"),
                     json.dumps(r.get("tags") or []), now, json.dumps(r)))
    con = _connect()
    with con:
        con.execute("DELETE FROM listings")
        con.executemany(f"INSERT INTO listings VALUES ({','.join('?' * 19)})", rows)
        con.execute("INSERT INTO pulls VALUES (?,?,?,?,?)", (now, asof, "listings", "all", len(rows)))
    con.close()
    print(f"  guesty db: {len(rows)} listings -> {GUESTY_DB}")


def write_reservations(raw: list[dict], summary: pd.DataFrame, checkin_from: str, asof: str) -> None:
    """Replace confirmed reservations with local check-in >= checkin_from, and the fee
    itemization, with this pull."""
    now = _now()
    rows = []
    for r in raw:
        if (r.get("checkInDateLocalized") or "") < checkin_from:
            continue   # the API filters on UTC checkIn; keep the local-date scope
        m = r.get("money") or {}
        rows.append((r.get("confirmationCode"), r.get("_id"), r.get("listingId"),
                     (r.get("listing") or {}).get("nickname"), r.get("status"), r.get("source"),
                     (r.get("integration") or {}).get("platform"),
                     r.get("checkInDateLocalized"), r.get("checkOutDateLocalized"),
                     r.get("nightsCount"), r.get("guestsCount"), r.get("confirmedAt"),
                     r.get("createdAt"), m.get("currency"), m.get("hostPayout"), m.get("totalPaid"),
                     m.get("totalRefunded"), m.get("fareAccommodation"), m.get("fareCleaning"),
                     (r.get("guest") or {}).get("fullName"), now, json.dumps(r)))
    scope = f"status=confirmed, check_in>={checkin_from}"
    con = _connect()
    with con:
        con.execute("DELETE FROM reservations WHERE status = 'confirmed' AND check_in >= ?", (checkin_from,))
        con.executemany(f"INSERT OR REPLACE INTO reservations VALUES ({','.join('?' * 22)})", rows)
        summary.assign(pulled_at=now).to_sql("reservation_fees", con, if_exists="replace", index=False)
        con.executemany("INSERT INTO pulls VALUES (?,?,?,?,?)",
                        [(now, asof, "reservations", scope, len(rows)),
                         (now, asof, "reservation_fees", scope, len(summary))])
    con.close()
    print(f"  guesty db: {len(rows)} reservations + {len(summary)} fee rows -> {GUESTY_DB}")


def listing_locations() -> dict:
    """{nickname: [lat, lng]} for every listing with coordinates (comp-set maps)."""
    if not GUESTY_DB.exists():
        return {}
    con = sqlite3.connect(f"file:{GUESTY_DB}?mode=ro", uri=True)
    rows = con.execute("SELECT nickname, lat, lng FROM listings "
                       "WHERE lat IS NOT NULL AND lng IS NOT NULL").fetchall()
    con.close()
    return {n: [round(a, 5), round(b, 5)] for n, a, b in rows}
