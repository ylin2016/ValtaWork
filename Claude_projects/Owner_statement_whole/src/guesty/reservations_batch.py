"""Pull financial breakdowns for MANY reservations by check-in date range.

    # all reservations checking in July 2026 (default = current month):
    python -m src.guesty.reservations_batch --checkin-from 2026-07-01 --checkin-to 2026-07-31
    python -m src.guesty.reservations_batch --month 2026-07
    python -m src.guesty.reservations_batch --month 2026-07 --status confirmed

Efficient: fetches the `money` object inside the paginated list endpoint
(~100/page), so a whole month is a handful of API calls, not one-per-reservation.
Reuses `reservation_financials.build_breakdown` so the numbers match the
single-reservation tool exactly.

Writes an .xlsx with three sheets:
  * summary            — one row per reservation (guest + host scalar figures)
  * items_by_category  — one row per reservation, one column per fee CATEGORY
                         (cleaning_fee, pet_fee, taxes, host_channel_fee…), split
                         by item title via categorize_item() — every item, wide
  * line_items         — long format, one row per invoice line item
"""
import argparse
import calendar
import json

import pandas as pd

from .config import resolve
from .client import GuestyClient
from .reservation_financials import build_breakdown, categorize_item

PAGE = 100
FIELDS = ("confirmationCode source status checkIn checkOut nightsCount "
          "listing.nickname listingId guest.fullName money")


def fetch_range(client: GuestyClient, dfrom: str, dto: str, statuses: list[str] | None) -> list[dict]:
    filt = [
        {"field": "checkIn", "operator": "$gte", "value": dfrom},
        {"field": "checkIn", "operator": "$lte", "value": f"{dto}T23:59:59.999Z"},
    ]
    if statuses:
        filt.append({"field": "status", "operator": "$in", "value": statuses})
    out, skip = [], 0
    while True:
        r = client.get("/reservations", params={
            "filters": json.dumps(filt), "fields": FIELDS,
            "limit": PAGE, "skip": skip, "sort": "checkIn",
        })
        batch = r.get("results", [])
        out.extend(batch)
        total = r.get("count", len(out))
        print(f"  fetched {len(out)}/{total}")
        skip += len(batch)
        if not batch or len(out) >= total or skip >= total:
            return out


def summary_row(b: dict) -> dict:
    m, g, h = b["meta"], b["guest"], b["host"]
    return {
        "confirmationCode": m["confirmationCode"], "source": m["source"], "status": m["status"],
        "listing": m["listing"], "guest": m["guest"],
        "checkIn": m["checkIn"], "checkOut": m["checkOut"], "nights": m["nights"],
        "currency": m["currency"],
        "accommodation": g["Accommodation fare"], "cleaning": g["Cleaning fee"],
        "total_fees": g["Total fees (incl. cleaning)"], "total_taxes": g["Total taxes"],
        "guest_service_fee": g["Guest service fee"], "subtotal_host_basis": g["Sub-total (host basis)"],
        "total_paid": g["Total paid"], "balance_due": g["Balance due"], "total_refunded": g["Total refunded"],
        "host_payout": h["Host payout"], "host_service_fee": h["Host/channel service fee"],
        "pm_commission": h["PM commission"], "payment_fees": h["Payment processing fees"],
        "net_income": h["Net income"], "owner_revenue": h["Owner revenue (payout − commission)"],
        "line_items_sum": b["line_items_sum"],
    }


def main():
    ap = argparse.ArgumentParser(description="Batch reservation financials by check-in range.")
    ap.add_argument("--month", help="YYYY-MM shortcut (sets from/to to that whole month).")
    ap.add_argument("--checkin-from", help="YYYY-MM-DD inclusive.")
    ap.add_argument("--checkin-to", help="YYYY-MM-DD inclusive.")
    ap.add_argument("--status", default=None,
                    help="Comma-separated status filter, e.g. 'confirmed,canceled'. Omit for all.")
    ap.add_argument("--out", default=None)
    args = ap.parse_args()

    if args.month:
        y, mo = map(int, args.month.split("-"))
        dfrom = f"{y:04d}-{mo:02d}-01"
        dto = f"{y:04d}-{mo:02d}-{calendar.monthrange(y, mo)[1]:02d}"
        tag = args.month
    elif args.checkin_from and args.checkin_to:
        dfrom, dto, tag = args.checkin_from, args.checkin_to, f"{args.checkin_from}_{args.checkin_to}"
    else:
        raise SystemExit("Provide --month YYYY-MM, or both --checkin-from and --checkin-to.")

    statuses = [s.strip() for s in args.status.split(",") if s.strip()] if args.status else None
    client = GuestyClient()
    print(f"Fetching reservations with check-in {dfrom} … {dto}"
          f"{f' (status in {statuses})' if statuses else ''}")
    reservations = fetch_range(client, dfrom, dto, statuses)
    print(f"Building breakdowns for {len(reservations)} reservation(s)…")

    # Stable, readable column order for the category pivot.
    CAT_ORDER = [
        "accommodation_fare", "accommodation_adjustment", "accommodation_discount",
        "cleaning_fee", "pet_fee", "extra_person_fee", "parking_fee", "resort_fee",
        "service_fee", "management_fee", "damage_protection", "additional_fees", "other_fee",
        "tax_state", "tax_county", "tax_city", "tax_local", "tax_occupancy",
        "tax_residential", "tax_destination", "tax_reservation_total", "tax_other",
        "markup", "promotion", "discount_length_of_stay", "discount_channel",
        "discount_weekly_monthly", "airbnb_resolution_center", "host_channel_fee",
    ]

    summaries, line_rows, cat_rows, seen_cats = [], [], [], []
    for x in reservations:
        b = build_breakdown(x)
        code = b["meta"]["confirmationCode"] or x.get("_id")
        by_cat = {}
        for li in b["line_items"]:
            cat = categorize_item(li)
            line_rows.append({"confirmationCode": code, "source": b["meta"]["source"], "category": cat, **li})
            by_cat[cat] = round(by_cat.get(cat, 0.0) + li["amount"], 2)
            if cat not in seen_cats:
                seen_cats.append(cat)
        cat_rows.append({"confirmationCode": code, "source": b["meta"]["source"],
                         "listing": b["meta"]["listing"], **by_cat})
        # fold the itemized fee categories into the summary row too
        summaries.append({**summary_row(b),
                          **{c: by_cat.get(c, 0.0) for c in CAT_ORDER if c in by_cat}})

    # category columns: preferred order first, then any unexpected ones
    ordered = [c for c in CAT_ORDER if c in seen_cats] + [c for c in seen_cats if c not in CAT_ORDER]
    df_sum = pd.DataFrame(summaries)
    df_types = pd.DataFrame(cat_rows).reindex(columns=["confirmationCode", "source", "listing"] + ordered)
    df_lines = pd.DataFrame(line_rows)

    out = resolve(args.out) if args.out else resolve(f"./data/reservations_{tag}_financials.xlsx")
    out.parent.mkdir(parents=True, exist_ok=True)
    with pd.ExcelWriter(out, engine="openpyxl") as xw:
        df_sum.to_excel(xw, sheet_name="summary", index=False)
        df_types.to_excel(xw, sheet_name="items_by_category", index=False)
        df_lines.to_excel(xw, sheet_name="line_items", index=False)

    # console totals
    print(f"\nWrote {out}")
    print(f"  {len(df_sum)} reservations across {df_sum['source'].nunique()} channel(s)")
    tot = df_sum[["accommodation", "cleaning", "total_taxes", "host_payout",
                  "pm_commission", "owner_revenue"]].sum(numeric_only=True)
    for k, v in tot.items():
        print(f"  Σ {k:16} = {v:,.2f}")


if __name__ == "__main__":
    main()
