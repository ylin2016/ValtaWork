"""Guesty invoice item -> QBO product/service, exactly as VRP mapped it.

Learned from 545 VRP invoices against the same reservations in Guesty (2026-10-04): every
label below is the item VRP actually used, per channel where the channel matters. A label
this does not recognise returns None with a reason, and the builder BLOCKS the
reservation rather than guessing an item.
"""
from __future__ import annotations

ITEM = {
    "fare": "Guest Charges:Owner Income:Accommodation Fare",
    "fare_adj": "Guest Charges:Owner Income:Accommodation Fare Adjustment",
    "discount": "Guest Charges:Owner Income:Accommodation Fare Discount",
    "channel": "Guest Charges:Owner Income:Channel Commission",
    "clean_owner": "Guest Charges:Owner Income:Cleaning Fee (Owner)",
    "clean_pm": "Guest Charges:PM Income:Cleaning Fee (PM)",
    "pet": "Guest Charges:Owner Income:Pet Fee (Owner)",
    "misc_pm": "Guest Charges:PM Income:Misc Guest Fee (PM)",
    "damage": "Guest Charges:PM Income:Damage Waiver",
    "tax": "Guest Charges:Tax",
    "tax_owner": "Guest Charges:Owner Income:Taxes Paid to Owners",
    "cancel_fee": "Guest Charges:Owner Income:Cancellation Fee",
}

TAX_NTS = {"ST", "LT", "CT", "COT", "TRT", "TAX", "COUNTRY", "OTHER", "GST", "OCT"}
DISCOUNT_NTS = {"LOSD", "PRO", "AFWD", "GCD", "AFD"}


def is_homeaway(source: str) -> bool:
    s = (source or "").lower()
    return s.startswith(("vrbo", "homeaway"))


def item_for(source: str, it: dict, cleaning_owner: bool) -> tuple[str | None, str]:
    """(item FQN or None, reason). None + reason 'skip:*' = deliberately not invoiced."""
    title = (it.get("title") or "").strip()
    t = title.lower()
    nt = (it.get("normalType") or "").upper()
    typ = (it.get("type") or "").upper()
    src = (source or "").lower()

    # Airbnb Resolution Center money arrives later as its own resolution payout; VRP
    # never put it on the invoice.
    if nt == "ARC" or "resolution" in t:
        return None, "skip:resolution center (paid separately)"
    if nt in ("AF", "MAR", "EPF") or "extra person" in t:
        return ITEM["fare"], "fare"
    if nt == "AFA":
        return ITEM["fare_adj"], "fare adjustment"
    if nt in DISCOUNT_NTS or "discount" in t or typ in ("PROMOTION", "DISCOUNT"):
        return ITEM["discount"], "discount"
    if nt == "PCM":
        return ITEM["channel"], "channel commission"
    if nt == "CFE" or "cancellation fee" in t:
        return ITEM["cancel_fee"], "cancellation fee"
    # Transient occupancy tax is the owner's to remit (Keaau, VRBO TOT) -- before the
    # generic tax rule.
    if nt == "TOT" or "transient occupancy" in t:
        return ITEM["tax_owner"], "occupancy tax to owner"
    if typ == "TAX" or nt in TAX_NTS or "tax" in t:
        # VRBO remits its own taxes; VRP booked those as PM income.
        return (ITEM["misc_pm"], "vrbo tax") if is_homeaway(src) else (ITEM["tax"], "tax")
    # Only a real cleaning-fee line (CF) follows the listing's owner/PM split. A cleaning
    # fee a channel sends as a generic fee -- Trip.com "Cleaning", Expedia "Service" --
    # VRP always booked as PM income, OSBR included.
    if nt == "CF":
        return (ITEM["clean_owner"], "cleaning (owner)") if cleaning_owner else (ITEM["clean_pm"], "cleaning (PM)")
    if "clean" in t:
        return ITEM["clean_pm"], "cleaning sent as a fee"
    if src == "expedia" and t == "service":
        return ITEM["clean_pm"], "expedia cleaning ('Service')"
    if "pet" in t:
        return ITEM["pet"], "pet fee"
    if "damage" in t or "protection" in t:
        return ITEM["damage"], "damage waiver"
    if "service fee" in t:
        # VRBO's per-night service fee went to the fare; a direct booking's to PM income.
        return (ITEM["fare"], "vrbo service fee") if is_homeaway(src) else (ITEM["misc_pm"], "service fee")
    if "travel coverage" in t:
        return ITEM["misc_pm"], "travel coverage"
    return None, f"UNMAPPED item {title!r} ({nt or '-'}/{typ or '-'})"
