"""Pull Wheelhouse data for the pricing-error scan.

Wheelhouse client in src/wheelhouse_api/ (key in secrets/.env); the listing/set
roster is the latest snapshot in the shared Claude_projects/shared_data/wheelhouse.sqlite
(refreshed weekly by Weekly Revenue Updates). Writes CSVs to data/raw/:
  wh_listings.csv               guesty listing id -> market, bedrooms, set
  wh_neighborhood_pricing.csv   comp low/median/high price per future date
  wh_neighborhood_occupancy.csv comp occupancy per future date (on the books)
  wh_price_recs.csv             Wheelhouse recommended price per future date
  wh_market_daily.csv           market adr/occupancy per day x bedroom, 2024-09..horizon
  wh_set_daily.csv              dynamic-set adr/occupancy per day, 2025..horizon
"""
import sqlite3
from datetime import date, timedelta
from pathlib import Path

import pandas as pd

from wheelhouse_api.config import load_config
from wheelhouse_api.wheelhouse_client import WheelhouseClient, WheelhouseError

ROOT = Path(__file__).resolve().parent.parent
OUT = ROOT / "data" / "raw"
# listing <-> comp-set roster: the shared DB, refreshed weekly by Weekly Revenue Updates
WH_DB = ROOT.parent / "shared_data" / "wheelhouse.sqlite"
TODAY = date(2026, 10, 6)
HORIZON = TODAY + timedelta(days=179)
METRICS = {"metric[]": ["adr_w_fees", "occupancy_adjusted"]}


def get(c, path, params):
    try:
        return c.get_json(path, params) or {}
    except WheelhouseError as e:
        print(f"  skip {path}: {str(e)[:100]}")
        return {}


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    c = WheelhouseClient(load_config())
    db = sqlite3.connect(f"file:{WH_DB}?mode=ro", uri=True)
    snap = db.execute("select max(snapshot_date) from listings").fetchone()[0]
    lst = pd.read_sql("select listing_id, name, bedrooms, market_id from listings where snapshot_date=?",
                      db, params=[snap])
    sets = pd.read_sql("select a.user_listing_id listing_id, a.set_id, s.name set_name "
                       "from dynamic_set_associated_listings a join dynamic_sets s using(snapshot_date,set_id) "
                       "where a.snapshot_date=?", db, params=[snap])
    lst = lst.merge(sets.groupby("listing_id").first().reset_index(), on="listing_id", how="left")
    lst.to_csv(OUT / "wh_listings.csv", index=False)
    print(f"wheelhouse roster snapshot {snap}: {len(lst)} listings, {sets.set_id.nunique()} sets")

    frames = {"pricing": [], "occupancy": [], "recs": []}
    eps = {"pricing": "neighborhood/pricing", "occupancy": "neighborhood/occupancy",
           "recs": "price_recommendations"}
    for i, lid in enumerate(lst.listing_id, 1):
        for k, ep in eps.items():
            d = pd.DataFrame(get(c, f"/listings/{lid}/{ep}", {"channel": "guesty"}).get("data", []))
            if len(d):
                d = d[(d.stay_date >= TODAY.isoformat()) & (d.stay_date <= HORIZON.isoformat())]
                d.insert(0, "listing_id", lid)
                frames[k].append(d)
        if i % 20 == 0:
            print(f"  listings {i}/{len(lst)}")
    for k, name in [("pricing", "wh_neighborhood_pricing"), ("occupancy", "wh_neighborhood_occupancy"),
                    ("recs", "wh_price_recs")]:
        pd.concat(frames[k]).to_csv(OUT / f"{name}.csv", index=False)

    mrows = []
    for mid in sorted(lst.market_id.dropna().unique()):
        for br in ["", "0", "1", "2", "3", "4+"]:
            p = {"start_date": "2024-09-01", "end_date": HORIZON.isoformat(), **METRICS}
            if br:
                p["bedrooms"] = br
            d = pd.DataFrame(get(c, f"/market_report/{mid}/time_series", p).get("data", []))
            if len(d):
                d.insert(0, "bedrooms", br or "all")
                d.insert(0, "market_id", mid)
                mrows.append(d)
    pd.concat(mrows).to_csv(OUT / "wh_market_daily.csv", index=False)
    print(f"market daily rows: {sum(map(len, mrows))}")

    srows = []
    for sid in sorted(sets.set_id.unique()):
        for start in ["2025-01-01", "2025-04-01", "2025-07-01", "2025-10-01"]:
            d = pd.DataFrame(get(c, f"/sets/{sid}/time_series",
                                 {"start_date": start, "end_date": HORIZON.isoformat(), **METRICS}).get("data", []))
            if len(d):
                d.insert(0, "set_id", sid)
                srows.append(d)
                break
    pd.concat(srows).to_csv(OUT / "wh_set_daily.csv", index=False)
    print(f"set daily rows: {sum(map(len, srows))}")


if __name__ == "__main__":
    main()
