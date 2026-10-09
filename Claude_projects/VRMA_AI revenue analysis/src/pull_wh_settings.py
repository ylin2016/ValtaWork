"""Pull each Wheelhouse listing's pricing preferences (min price, adjustment,
discounts, min stay) -> data/raw/wh_listing_settings.csv."""
from pathlib import Path

import pandas as pd

from wheelhouse_api.config import load_config
from wheelhouse_api.wheelhouse_client import WheelhouseClient, WheelhouseError

OUT = Path(__file__).resolve().parent.parent / "data" / "raw"

c = WheelhouseClient(load_config())
rows = []
for lid in pd.read_csv(OUT / "wh_listings.csv").listing_id:
    try:
        d = c.get_json(f"/listings/{lid}", {"channel": "guesty"})
    except WheelhouseError as e:
        print("skip", lid, str(e)[:80])
        continue
    p = d.get("listing_preferences") or {}
    rows.append({"listing_id": lid, "nickname": d.get("nickname"), "is_active": d.get("is_active"),
                 "min_price": p.get("min_price"), "base_price": p.get("base_price"),
                 "base_price_adjustment": p.get("base_price_adjustment"),
                 "auto_posting": p.get("automatic_rate_posting_enabled"),
                 "minimum_stay": p.get("minimum_stay"), "weekly_discount_pct": p.get("weekly_discount_pct"),
                 "monthly_discount_pct": p.get("monthly_discount_pct")})
pd.DataFrame(rows).to_csv(OUT / "wh_listing_settings.csv", index=False)
print(len(rows), "listings")
