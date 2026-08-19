"""Show the financial breakdown of a single Guesty reservation.

    python -m src.guesty.reservation_financials HMTSFNDRDY          # by confirmation code
    python -m src.guesty.reservation_financials 699501306943289afe34fe27   # by reservation _id
    python -m src.guesty.reservation_financials HMTSFNDRDY --json   # also dump raw money to data/
    python -m src.guesty.reservation_financials HMTSFNDRDY --excel  # also write a line-item .xlsx

What it shows (all figures are Guesty's own — no derived channel math):
  * LINE ITEMS  — the reservation's invoiceItems (the host ledger). These sum to
    Host Payout. Positive = revenue; the negative `PCM` item is the channel's
    host fee.
  * GUEST PAYS  — accommodation, cleaning, other fees, itemized taxes, and the
    guest service fee (guestFeeBase/VAT), plus what has actually been paid.
  * HOST PAYOUT — Host Payout, the channel host/service fee, your PM commission,
    payment-processing fees, and Owner Revenue (= Host Payout − commission).

Different channels populate different items; this surfaces whatever is present.
"""
import argparse
import json
import re

from .config import resolve
from .client import GuestyClient

# normalType -> friendly label (titles on the items are used first when present).
NT_LABELS = {
    "AF": "Accommodation fare", "MAR": "Markup", "CF": "Cleaning fee",
    "AFE": "Additional / service fee", "PCM": "Host channel fee (channel commission)",
    "ST": "State tax", "LT": "Local tax", "CT": "City tax", "COT": "County tax",
    "TRT": "Total reservation taxes", "TAX": "Tax", "OTHER": "Other tax / fee",
    "PRO": "Promotion / discount", "AFWD": "Weekly / monthly discount",
}
TAX_NTS = {"ST", "LT", "CT", "COT", "TRT", "TAX", "COUNTRY", "OTHER"}

# Taxes: normalType -> clean category.
_TAX_CAT = {"ST": "tax_state", "LT": "tax_local", "CT": "tax_city", "COT": "tax_county",
            "TOT": "tax_occupancy", "TRT": "tax_reservation_total"}


def categorize_item(item: dict) -> str:
    """Map a line item to a clean, human category — by TITLE, not just normalType.

    Guesty lumps pet/parking/service/damage/misc fees all under normalType=AFE,
    so we read the title to separate them (this is what the batch pivot uses).
    """
    # raw invoiceItems use "title"; the built line_items use "label" — accept either.
    t = (item.get("title") or item.get("label") or "").lower()
    nt = item.get("normalType")
    typ = (item.get("type") or "").upper()

    if typ == "TAX" or nt in TAX_NTS:
        if "occupanc" in t or nt == "TOT":
            return "tax_occupancy"
        if "resident" in t:
            return "tax_residential"
        if "destination" in t:
            return "tax_destination"
        return _TAX_CAT.get(nt, "tax_other")

    # some channels mis-type taxes as ADDITIONAL/AFE (e.g. "Destination tax") — catch by title
    if "tax" in t:
        return "tax_destination" if "destination" in t else "tax_other"

    if nt == "AF":
        return "accommodation_fare"
    if nt == "AFA":
        return "accommodation_adjustment"
    if nt == "MAR":
        return "markup"
    if nt == "PCM":
        return "host_channel_fee"
    if nt == "CF" or "clean" in t:
        return "cleaning_fee"

    # discounts / promotions (usually negative)
    if nt == "LOSD" or "length of stay" in t:
        return "discount_length_of_stay"
    if nt in ("GCD",) or (typ in ("DISCOUNT", "GENERIC_CHANNEL_DISCOUNT") and "weekly" not in t):
        return "discount_channel"
    if "weekly" in t or "monthly" in t or nt == "AFWD":
        return "discount_weekly_monthly"
    if typ == "PROMOTION" or nt == "PRO":
        return "promotion"
    if nt == "AFD" or "fare discount" in t:
        return "accommodation_discount"
    if "resolution" in t or nt == "ARC":
        return "airbnb_resolution_center"

    # fees (AFE / EPF / MANUAL / etc.) — split by title keyword
    if "pet" in t:
        return "pet_fee"
    if "extra person" in t or "extra guest" in t or "additional guest" in t or nt == "EPF":
        return "extra_person_fee"
    if "parking" in t:
        return "parking_fee"
    if "resort" in t:
        return "resort_fee"
    if "damage" in t or "protection" in t or "deposit" in t:
        return "damage_protection"
    if "service" in t:
        return "service_fee"
    if "management" in t or "admin" in t:
        return "management_fee"
    if "additional fee" in t or "room fee" in t:
        return "additional_fees"
    return "other_fee"


def fetch_reservation(client: GuestyClient, key: str) -> dict:
    """Resolve a confirmation code or _id to the full reservation object."""
    if re.fullmatch(r"[0-9a-fA-F]{24}", key):
        rid = key
    else:
        filt = json.dumps([{"field": "confirmationCode", "operator": "$eq", "value": key}])
        res = client.get("/reservations", params={"filters": filt, "fields": "_id confirmationCode", "limit": 2})
        results = res.get("results", [])
        if not results:
            raise SystemExit(f"No reservation found with confirmation code '{key}'.")
        rid = results[0]["_id"]
    return client.get(f"/reservations/{rid}")


def _n(v):
    return 0.0 if v in (None, "") else float(v)


def _label(item: dict) -> str:
    title = (item.get("title") or "").strip()
    nt = item.get("normalType")
    if title and title.upper() not in (str(nt), (item.get("type") or "").upper()):
        return title
    return NT_LABELS.get(nt, title or nt or "?")


def build_breakdown(resv: dict) -> dict:
    m = resv.get("money") or {}
    cur = m.get("currency", "USD")
    items = m.get("invoiceItems") or []

    line_items = [{
        "label": _label(it),
        "type": it.get("type"),
        "normalType": it.get("normalType"),
        "amount": _n(it.get("amount")),
        "origin": it.get("origin"),
    } for it in items]

    taxes = [li for li in line_items if li["normalType"] in TAX_NTS or li["type"] == "TAX"]
    guest_fee = _n(m.get("guestFeeBase")) + _n(m.get("guestFeeVat"))

    return {
        "meta": {
            "confirmationCode": resv.get("confirmationCode"),
            "source": resv.get("source"),
            "status": resv.get("status"),
            "listing": (resv.get("listing") or {}).get("nickname") or resv.get("listingId"),
            "guest": (resv.get("guest") or {}).get("fullName"),
            "checkIn": resv.get("checkIn"), "checkOut": resv.get("checkOut"),
            "nights": resv.get("nightsCount"), "currency": cur,
        },
        "line_items": line_items,
        "line_items_sum": round(sum(li["amount"] for li in line_items), 2),
        "guest": {
            "Accommodation fare": _n(m.get("fareAccommodation")),
            "Cleaning fee": _n(m.get("fareCleaning")),
            "Total fees (incl. cleaning)": _n(m.get("totalFees")),
            "Total taxes": _n(m.get("totalTaxes")),
            "Guest service fee": guest_fee,
            "Sub-total (host basis)": _n(m.get("subTotalPrice")),
            "Total paid": _n(m.get("totalPaid")),
            "Balance due": _n(m.get("balanceDue")),
            "Total refunded": _n(m.get("totalRefunded")),
            "taxes_itemized": taxes,
        },
        "host": {
            "Host payout": _n(m.get("hostPayout")),
            "Host/channel service fee": _n(m.get("hostServiceFee")),
            "PM commission": _n(m.get("commission")),
            "Payment processing fees": _n(m.get("totalPaymentFees")),
            "Net income": _n(m.get("netIncome")),
            "Owner revenue (payout − commission)": _n(m.get("ownerRevenue")),
        },
    }


def _fmt(v, cur):
    return f"{cur} {v:,.2f}"


def print_breakdown(b: dict):
    m, cur = b["meta"], b["meta"]["currency"]
    W = 62
    print("=" * W)
    print(f"RESERVATION {m['confirmationCode']}   ({m['source']}, {m['status']})")
    print(f"  {m['listing']}  |  guest: {m['guest']}")
    print(f"  {m['checkIn']} → {m['checkOut']}  ({m['nights']} nights)")
    print("=" * W)

    print("\nLINE ITEMS  (host ledger — sums to Host Payout)")
    print("-" * W)
    for li in b["line_items"]:
        tag = f"[{li['normalType']}]" if li["normalType"] else ""
        print(f"  {li['label'][:38]:38} {tag:>7} {_fmt(li['amount'], cur):>14}")
    print("-" * W)
    print(f"  {'Σ line items = Host Payout':38} {'':>7} {_fmt(b['line_items_sum'], cur):>14}")

    print("\nGUEST PAYS")
    print("-" * W)
    g = b["guest"]
    for k in ["Accommodation fare", "Cleaning fee", "Total fees (incl. cleaning)", "Total taxes"]:
        if g[k]:
            print(f"  {k:46} {_fmt(g[k], cur):>14}")
    for t in g["taxes_itemized"]:
        print(f"      · {t['label'][:40]:40} {_fmt(t['amount'], cur):>14}")
    if g["Guest service fee"]:
        print(f"  {'Guest service fee (guestFeeBase/VAT)':46} {_fmt(g['Guest service fee'], cur):>14}")
    print(f"  {'— Total paid':46} {_fmt(g['Total paid'], cur):>14}")
    if g["Balance due"]:
        print(f"  {'— Balance due':46} {_fmt(g['Balance due'], cur):>14}")
    if g["Total refunded"]:
        print(f"  {'— Total refunded':46} {_fmt(g['Total refunded'], cur):>14}")

    print("\nHOST PAYOUT")
    print("-" * W)
    h = b["host"]
    print(f"  {'Host payout (from channel)':46} {_fmt(h['Host payout'], cur):>14}")
    for k in ["Host/channel service fee", "PM commission", "Payment processing fees"]:
        if h[k]:
            print(f"      − {k:42} {_fmt(h[k], cur):>14}")
    print(f"  {'Owner revenue (payout − commission)':46} {_fmt(h['Owner revenue (payout − commission)'], cur):>14}")
    print("=" * W)


def main():
    ap = argparse.ArgumentParser(description="Financial breakdown of one Guesty reservation.")
    ap.add_argument("reservation", help="Confirmation code (e.g. HMTSFNDRDY) or reservation _id.")
    ap.add_argument("--json", action="store_true", help="Also dump raw money object to data/.")
    ap.add_argument("--excel", action="store_true", help="Also write a line-item .xlsx to data/.")
    args = ap.parse_args()

    client = GuestyClient()
    resv = fetch_reservation(client, args.reservation)
    b = build_breakdown(resv)
    print_breakdown(b)

    code = b["meta"]["confirmationCode"]
    if args.json:
        p = resolve(f"./data/reservation_{code}_money.json")
        p.write_text(json.dumps(resv.get("money") or {}, indent=2, default=str))
        print(f"\nRaw money -> {p}")
    if args.excel:
        import pandas as pd
        p = resolve(f"./data/reservation_{code}_financials.xlsx")
        with pd.ExcelWriter(p, engine="openpyxl") as xw:
            pd.DataFrame(b["line_items"]).to_excel(xw, sheet_name="line_items", index=False)
            pd.DataFrame([{**{"section": "guest", **{k: v for k, v in b["guest"].items() if k != "taxes_itemized"}},
                           **{f"host.{k}": v for k, v in b["host"].items()}}]).to_excel(xw, sheet_name="summary", index=False)
        print(f"Line-item Excel -> {p}")


if __name__ == "__main__":
    main()
