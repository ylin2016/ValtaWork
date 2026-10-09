"""One summary row per Guesty reservation (fee categories folded), the input
payment_model.compute_breakdown reads.

Copied from Owner_statement_whole (src/breakdown/fetch_month.py:
CAT_ORDER, FIELDS, build_summary_frame) so this project runs on its own.
"""
import pandas as pd

from .reservation_financials import build_breakdown, categorize_item
from .reservations_batch import summary_row

# Stable column order for the folded fee-category pivot (mirrors reservations_batch.main).
CAT_ORDER = [
    "accommodation_fare", "accommodation_adjustment", "accommodation_discount",
    "cleaning_fee", "pet_fee", "extra_person_fee", "parking_fee", "resort_fee",
    "service_fee", "management_fee", "damage_protection", "additional_fees", "other_fee",
    "tax_state", "tax_county", "tax_city", "tax_local", "tax_occupancy",
    "tax_residential", "tax_destination", "tax_reservation_total", "tax_other",
    "markup", "promotion", "discount_length_of_stay", "discount_channel",
    "discount_weekly_monthly", "airbnb_resolution_center", "host_channel_fee",
]

# summary FIELDS + guestsCount (needed for the UI-shaped export's NUMBER OF GUESTS)
# + specialRequests (Expedia stamps "Payment method: EXPEDIA VIRTUAL CARD" there, which
# selects the virtual-card channel-fee formula in payment_model).
FIELDS = ("confirmationCode source status checkIn checkOut nightsCount guestsCount "
          "listing.nickname listingId guest.fullName money specialRequests")




def build_summary_frame(reservations: list[dict]) -> pd.DataFrame:
    """One row per reservation: summary_row fields + folded fee categories +
    guestsCount. Matches the `summary` sheet reservations_batch writes, which is
    exactly what both adapters read."""
    rows = []
    for x in reservations:
        b = build_breakdown(x)
        by_cat = {}
        for li in b["line_items"]:
            cat = categorize_item(li)
            by_cat[cat] = round(by_cat.get(cat, 0.0) + li["amount"], 2)
        row = {**summary_row(b),
               **{c: by_cat.get(c, 0.0) for c in CAT_ORDER if c in by_cat}}
        row["guestsCount"] = x.get("guestsCount")
        # Successful Stripe transaction count — each captured charge (deposit +
        # balance, etc.) carries its own $0.30 flat fee in the Stripe-fee formula.
        # Count only status=='SUCCEEDED' payments (CANCELLED/FAILED/PENDING don't
        # settle; no security-deposit/auth-hold rows appear among SUCCEEDED).
        pays = (x.get("money") or {}).get("payments") or []
        row["n_transactions"] = sum(1 for p in pays if str(p.get("status")).upper() == "SUCCEEDED")
        # Virtual-card flag: Expedia stamps its payment method in specialRequests
        # ("Payment method: EXPEDIA VIRTUAL CARD"). Detecting it here (rather than from the
        # PCM invoice line) is what picks the correct Expedia channel-fee formula — the PCM
        # line is present on only some virtual-card bookings, so it under-detects.
        row["is_virtual_card"] = "virtual card" in str(x.get("specialRequests") or "").lower()
        rows.append(row)
    df = pd.DataFrame(rows)
    # Dedupe (overlapping pages / mixed statuses can repeat a confirmationCode).
    if "confirmationCode" in df.columns:
        df = df.drop_duplicates("confirmationCode", keep="first").reset_index(drop=True)
    return df
