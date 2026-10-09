"""What each month's guest payments are made of: rent, cleaning, tax, channel fee.

Every fee rule is Owner_statement_whole's, copied into guesty_api/ so this project
runs on its own: `guesty_api.payment_model.compute_breakdown` (the transcription of
the user-owned `payment_structure.xlsx`) run on the itemization fetch_guesty.py saves
with `guesty_api.summary.build_summary_frame`
(data/guesty/Guesty_summary_2026-<date>.csv), with `config/listing_tax_rates.csv`.
When the fee sheet or tax rates change in Owner_statement_whole, re-copy
payment_model.py / _listing_tax_rates.csv here (see CLAUDE.md).

From each booking's breakdown:
    guest total = guest_pay
    tax, cleaning = tax, cleaning_fee
    channel fee = channel_fee (+ VRBO's guest service fee: guest_pay - InvoiceItem)
    rent        = guest total - tax - cleaning - channel fee
                = accommodation + markup + add-ons (pet, extra guests, discounts, damage
                  waiver, ...) less the channel commission taken out of them

Each booking is split evenly over its nights, like Revenue_month. Only bookings in the
API pull are itemized (check-in 2026+, active listings); everything else in the month
is "not itemized" (payout only), and leases are rent.
"""
import numpy as np
import pandas as pd

from guesty_api.payment_model import compute_breakdown
from paths import LISTING_TAX_RATES

MIX_COLS = ["Mix_rent", "Mix_clean", "Mix_tax", "Mix_channel", "Mix_lease", "Mix_none",
            "Mix_acc", "Mix_pet", "Mix_disc", "Mix_other", "Mix_comm"]
DISCOUNTS = ["accommodation_discount", "promotion", "discount_length_of_stay",
             "discount_weekly_monthly", "discount_channel"]


def booking_mix(summary: pd.DataFrame) -> pd.DataFrame:
    """One row per booking: guest-total parts + what the rent is made of."""
    bd, _ = compute_breakdown(summary, LISTING_TAX_RATES)
    s = summary.set_index("confirmationCode")
    cat = lambda c: (pd.to_numeric(s[c], errors="coerce").fillna(0.0)
                     .reindex(bd["confirmationCode"]).fillna(0.0).values if c in s else 0.0)
    G, II = bd["guest_pay"].values, bd["InvoiceItem"].values
    channel = bd["channel_fee"].values + np.where(bd["row_group"].eq("homeaway"), G - II, 0.0)
    tax, clean = bd["tax"].values, bd["cleaning_fee"].values
    rent = G - tax - clean - channel
    acc = bd["accommodation"].values + bd["markup"].values
    pet = cat("pet_fee")
    disc = sum(cat(c) for c in DISCOUNTS)
    other = bd["addons"].values + bd["damage_waiver"].values - pet - disc
    comm = acc + pet + disc + other - rent        # channel commission taken out of the rent
    return pd.DataFrame({"code": bd["confirmationCode"].values, "group": bd["row_group"].values,
                         "rent": rent, "clean": clean, "tax": tax, "channel": channel,
                         "acc": acc, "pet": pet, "disc": disc, "other": other, "comm": comm})


def monthly_mix(data: pd.DataFrame, summary_csv) -> pd.DataFrame:
    """Listing x yearmonth guest-payment parts, each booking split over its nights.
    `data` is the report's booking table (after rent roll and filters)."""
    b = data[["Listing", "Confirmation.Code", "checkin_date", "checkout_date", "nights", "total_revenue", "Term"]].copy()
    b = b[(b["nights"] > 0) & b["checkout_date"].notna()]
    b["total_revenue"] = pd.to_numeric(b["total_revenue"], errors="coerce").fillna(0.0)
    try:
        mix = booking_mix(pd.read_csv(summary_csv))
    except FileNotFoundError:
        print(f"  revenue mix: {summary_csv} not found — nothing itemized")
        mix = pd.DataFrame(columns=["code"])
    b = b.merge(mix, left_on="Confirmation.Code", right_on="code", how="left")
    itemized = b["rent"].notna()
    lease = ~itemized & b["Term"].eq("LTR")
    for c in ["rent", "clean", "tax", "channel", "acc", "pet", "disc", "other", "comm"]:
        b[f"Mix_{c}"] = b[c].where(itemized, 0.0).fillna(0.0)
    b["Mix_lease"] = b["total_revenue"].where(lease, 0.0)
    b["Mix_none"] = b["total_revenue"].where(~itemized & ~lease, 0.0)
    for k in MIX_COLS:                     # split every part evenly over the nights
        b[k] = b[k] / b["nights"]
    b["date"] = [pd.date_range(a, c - pd.Timedelta(days=1)) for a, c in
                 zip(pd.to_datetime(b["checkin_date"]), pd.to_datetime(b["checkout_date"]))]
    b = b.explode("date").dropna(subset=["date"])
    b["yearmonth"] = pd.to_datetime(b["date"]).dt.strftime("%Y-%m")
    out = b.groupby(["Listing", "yearmonth"], as_index=False)[MIX_COLS].sum()
    groups = mix["group"].value_counts().to_dict() if "group" in mix else {}
    print(f"  revenue mix: {int(itemized.sum())}/{len(itemized)} bookings itemized; channel rows {groups}")
    if groups.get("unknown"):
        print(f"  revenue mix: !! {groups['unknown']} booking(s) on an unmapped channel — "
              "map them in guesty_api/payment_model._assign_row_group")
    return out
