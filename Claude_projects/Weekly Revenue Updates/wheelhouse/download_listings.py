"""Pull our listings — the foundation for every per-listing market/comp-set pull."""
from datetime import datetime, timezone

from . import db
from .transforms import first, to_float, as_str, dumps


def download_listings(client, conn, cfg, snapshot_date: str, limit: int | None = None) -> list[dict]:
    """Fetch all listings, store them, and return lightweight dicts with the
    fields other modules need: listing_id, channel, market_id."""
    path = cfg["endpoints"]["listings"]
    pulled_at = datetime.now(timezone.utc).isoformat()

    rows = []
    light = []
    for item in client.get_paginated(path, ref_id="listings"):
        listing_id = first(item, "id", "listing_id", "uuid")
        if listing_id is None:
            continue
        listing_id = str(listing_id)
        channel = as_str(first(item, "channel", "channel_name", "platform"))
        market_id = as_str(first(item, "market_id", "market", "market_uuid"))
        rows.append(
            {
                "snapshot_date": snapshot_date,
                "listing_id": listing_id,
                "channel": channel,
                "name": as_str(first(item, "name", "title", "nickname")),
                "bedrooms": to_float(first(item, "bedrooms", "beds", "num_bedrooms")),
                "market_id": market_id,
                "raw_json": dumps(item),
                "pulled_at": pulled_at,
            }
        )
        light.append({"listing_id": listing_id, "channel": channel, "market_id": market_id})
        if limit and len(rows) >= limit:
            break

    db.upsert(conn, "listings", rows)
    print(f"  listings: {len(rows)} stored")
    return light
