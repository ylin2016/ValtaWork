"""Pull Guesty data for the pricing-error scan.

Reuses GuestyFinancials' client + cached token (5 tokens/24h shared limit —
never force-refresh). Writes CSVs to data/raw/:
  guesty_listings.csv      one row per listing
  guesty_reservations.csv  one row per non-canceled reservation (check-in window)
  guesty_calendar.csv      one row per listing x date (current price + status)
"""
import json
import sys
from datetime import date, timedelta
from pathlib import Path

import pandas as pd

GF = Path("/Users/ylin/ValtaWork/Claude_projects/GuestyFinancials")
sys.path.insert(0, str(GF))
from src.guesty_client import GuestyClient  # noqa: E402

OUT = Path(__file__).resolve().parent.parent / "data" / "raw"
TODAY = date(2026, 10, 6)
HORIZON = TODAY + timedelta(days=179)            # 180 nights incl. today
RES_FROM, RES_TO = "2025-03-01", HORIZON.isoformat()
STATUSES = ["confirmed"]  # user: confirmed bookings only
RES_FIELDS = ("confirmationCode source status checkInDateLocalized checkOutDateLocalized "
              "nightsCount listingId listing.nickname createdAt confirmedAt guestsCount "
              "integration.platform money.fareAccommodation money.fareAccommodationAdjusted "
              "money.hostPayout money.currency")


def paged(c, path, params, key="results"):
    out, skip = [], 0
    while True:
        r = c.get(path, params={**params, "limit": 100, "skip": skip})
        batch = r.get(key, [])
        out += batch
        skip += len(batch)
        if not batch or skip >= r.get("count", 0):
            return out


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    c = GuestyClient()

    ls = paged(c, "/listings", {"fields": "nickname title bedrooms accommodates active listed "
                                          "address.city address.full type propertyType"})
    lrows = [{"listing_id": l["_id"], "nickname": l.get("nickname"), "title": l.get("title"),
              "bedrooms": l.get("bedrooms"), "accommodates": l.get("accommodates"),
              "active": l.get("active"), "listed": l.get("listed"),
              "city": (l.get("address") or {}).get("city"), "type": l.get("type")} for l in ls]
    pd.DataFrame(lrows).to_csv(OUT / "guesty_listings.csv", index=False)
    print(f"listings: {len(lrows)}")

    filt = [{"field": "checkIn", "operator": "$gte", "value": RES_FROM},
            {"field": "checkIn", "operator": "$lte", "value": f"{RES_TO}T23:59:59Z"},
            {"field": "status", "operator": "$in", "value": STATUSES}]
    res = paged(c, "/reservations", {"filters": json.dumps(filt), "fields": RES_FIELDS, "sort": "checkIn"})
    rrows = []
    for r in res:
        m = r.get("money") or {}
        rrows.append({"confirmation_code": r.get("confirmationCode"), "listing_id": r.get("listingId"),
                      "nickname": (r.get("listing") or {}).get("nickname"), "source": r.get("source"),
                      "platform": (r.get("integration") or {}).get("platform"), "status": r.get("status"),
                      "check_in": r.get("checkInDateLocalized"), "check_out": r.get("checkOutDateLocalized"),
                      "nights": r.get("nightsCount"), "created_at": r.get("createdAt"),
                      "confirmed_at": r.get("confirmedAt"), "guests": r.get("guestsCount"),
                      "fare_accommodation": m.get("fareAccommodation"),
                      "fare_accommodation_adjusted": m.get("fareAccommodationAdjusted"),
                      "host_payout": m.get("hostPayout"), "currency": m.get("currency")})
    pd.DataFrame(rrows).to_csv(OUT / "guesty_reservations.csv", index=False)
    print(f"reservations: {len(rrows)}")

    crow = []
    active = [l for l in lrows if l["active"]]
    for i, l in enumerate(active, 1):
        try:
            d = c.get(f"/availability-pricing/api/calendar/listings/{l['listing_id']}",
                      params={"startDate": TODAY.isoformat(), "endDate": HORIZON.isoformat()})
        except RuntimeError as e:
            print(f"  calendar FAIL {l['nickname']}: {str(e)[:120]}")
            continue
        for day in (d.get("data") or {}).get("days", []):
            refs = day.get("blockRefs") or []
            res_ref = next((b for b in refs if b.get("reservationId")), None)
            crow.append({"listing_id": l["listing_id"], "nickname": l["nickname"], "date": day["date"],
                         "price": day.get("price"), "base_price": day.get("basePrice"),
                         "is_base_price": day.get("isBasePrice"), "min_nights": day.get("minNights"),
                         "status": day.get("status"),
                         "block_types": ",".join(k for k, v in (day.get("blocks") or {}).items() if v),
                         "reservation_id": res_ref.get("reservationId") if res_ref else None})
        if i % 20 == 0:
            print(f"  calendar {i}/{len(active)}")
    pd.DataFrame(crow).to_csv(OUT / "guesty_calendar.csv", index=False)
    print(f"calendar rows: {len(crow)} over {len(active)} active listings")


if __name__ == "__main__":
    main()
