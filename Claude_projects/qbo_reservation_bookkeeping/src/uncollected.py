"""Open invoices on stays that have ended, sorted by why they are open. READ-ONLY.

    python -m src.uncollected --month 2026-10      # stays checking out in October (month-end close)
    python -m src.uncollected                      # every 2026 stay checked out by today

Run before the month's owner statements go out (Accounting SOP Part C): the statement counts
Guesty's charges as owner income, so a charge that was never collected and is caught only
after the statement is out becomes Valta's loss (Avina 5949858184, Ancelet 6516358283).

Each open invoice gets one status:
    UNCOLLECTED      the open balance is a charge Guesty added that the channel never pays (pet
                     fee, service fee, fare adjustment) or a direct guest's unpaid balance:
                     collect it (Stripe link) or take it off in Guesty before statements go out
    NOT APPLIED      Guesty shows it paid in full: the money came in, find it and apply it
    CANCELLED        Guesty shows the booking cancelled: void / cut the invoice (cancellation rule)
    AWAITING PAYOUT  channel money not in yet, checked out under 30 days ago
    PAYOUT MISSING   channel money not in yet, 30+ days after checkout: check the payout files
    ROUNDING         under $2: write off to Reconciliation Discrepancies
    HELD CLEANING    Elektra owner-paid Airbnb (12/2024-12/2025): invoice paid into Airbnb clearing,
                     but the cleaning fee the owner collected was never charged -- it sits in
                     clearing until the owner decides (SOP Part E). Listed whatever the window
    CHECK            none of the above (not in Guesty, stale invoice, ...)

Guesty's balanceDue is not reliable for OTA bookings (it often shows the whole stay due after the
channel paid), so a channel booking is UNCOLLECTED only when the open balance matches the manual
items Guesty added. Uses one cached Guesty token (no new token) and read-only QBO queries.
Writes review/uncollected_<scope>_<stamp>.csv."""
from __future__ import annotations

import argparse
import calendar
import csv
import json
import re
from datetime import date, datetime, timedelta, timezone

from . import bridge, fix_owner_paid_airbnb
from .paths import REVIEW_DIR

MANUAL = ("MANUAL", "ACCOMMODATION_FARE_ADJUSTMENT")
DIRECT = ("manual", "website", "BE-API", "owner-guest")
ROUNDING, GRACE = 2.00, 30
FIELDS = ("confirmationCode status source checkIn checkOut listing.nickname money.hostPayout money.totalPaid "
          "money.balanceDue money.invoiceItems integration.bookingCom.reservationId")


def guesty_by_checkout(g, start: str, end: str) -> dict[str, dict]:
    """Guesty reservations checking out in [start, end], keyed by code and Booking.com id."""
    out, skip = {}, 0
    flt = json.dumps([{"field": "checkOut", "operator": "$between", "from": f"{start}T00:00:00Z", "to": f"{end}T23:59:59Z"}])
    while True:
        r = g.get("/reservations", {"filters": flt, "fields": FIELDS, "limit": 100, "skip": skip})
        for x in r.get("results", []):
            if x.get("confirmationCode"):
                out[x["confirmationCode"]] = x
            bc = ((x.get("integration") or {}).get("bookingCom") or {}).get("reservationId")
            if bc:
                out[str(bc)] = x
        skip += 100
        if skip >= r.get("count", 0):
            return out


def guesty_by_code(g, codes: list[str]) -> dict[str, dict]:
    """The reservations the checkout window missed (longer stays, deactivated listings), by code."""
    out = {}
    bc = [c for c in codes if re.fullmatch(r"\d{10}", c)]
    other = [c for c in codes if c not in bc]
    for i in range(0, len(other), 40):
        flt = json.dumps([{"field": "confirmationCode", "operator": "$in", "value": other[i:i + 40]}])
        for x in g.get("/reservations", {"filters": flt, "fields": FIELDS, "limit": 100}).get("results", []):
            out[x["confirmationCode"]] = x
    for c in bc:                           # $in does not match the Booking.com id; one call each
        flt = json.dumps([{"field": "integration.bookingCom.reservationId", "operator": "$eq", "value": c}])
        for x in g.get("/reservations", {"filters": flt, "fields": FIELDS}).get("results", []):
            out[c] = x
    return out


def code_of(inv: dict) -> str:
    return inv["CustomerRef"].get("name", "").rsplit(" - ", 1)[-1].strip()


def classify(inv: dict, g: dict | None, today: date) -> tuple[str, str]:
    bal = round(inv["Balance"], 2)
    if bal < ROUNDING:
        return "ROUNDING", f"{bal:.2f} left"
    if not g:
        return "CHECK", "not in Guesty for this checkout window (deactivated listing, not a reservation, or code differs)"
    m = g.get("money") or {}
    if g.get("status") == "canceled":
        return "CANCELLED", "void or cut the invoice to what was kept"
    total, paid, due = (round(float(m.get(k) or 0), 2) for k in ("hostPayout", "totalPaid", "balanceDue"))
    manual = [(i.get("title"), round(float(i.get("amount") or 0), 2)) for i in m.get("invoiceItems") or []
              if i.get("type") in MANUAL and float(i.get("amount") or 0) > 0]
    man_sum = round(sum(a for _, a in manual), 2)
    items = ", ".join(f"{t} {a:.2f}" for t, a in manual)
    if abs(round(inv["TotalAmt"], 2) - total) > 0.02:
        stale = f"; QBO invoice {inv['TotalAmt']:.2f} vs Guesty {total:.2f}"
    else:
        stale = ""
    if g.get("source") in DIRECT:
        if due > 0.005 and abs(due - bal) < 0.02:
            return "UNCOLLECTED", f"direct guest owes {due:.2f} (paid {paid:.2f} of {total:.2f})" + stale
        if due <= 0.005:
            return "NOT APPLIED", f"Guesty shows {paid:.2f} paid in full" + stale
        return "CHECK", f"Guesty due {due:.2f}, QBO open {bal:.2f}" + stale
    if manual and (abs(man_sum - bal) < 0.02 or any(abs(a - bal) < 0.02 for _, a in manual)):
        return "UNCOLLECTED", f"manual charge(s) the channel never pays: {items}" + stale
    co = date.fromisoformat((g.get("checkOut") or "")[:10] or today.isoformat())
    if (today - co).days < GRACE and bal >= round(inv["TotalAmt"], 2) - 0.02:
        # Airbnb shows a stay paid in Guesty before its payout lands: recent = still in transit
        return "AWAITING PAYOUT", f"checked out {co}" + stale
    if due <= 0.005 and paid >= total - 0.02:
        return "NOT APPLIED", f"Guesty shows {paid:.2f} paid in full" + stale
    if bal >= round(inv["TotalAmt"], 2) - 0.02:
        return (("AWAITING PAYOUT", f"checked out {co}") if (today - co).days < GRACE
                else ("PAYOUT MISSING", f"nothing applied {(today - co).days} days after checkout {co}")) if not stale \
            else ("CHECK", "nothing applied" + stale)
    return "CHECK", f"part paid; Guesty paid {paid:.2f} due {due:.2f}" + (f"; manual items {items}" if items else "") + stale


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--month", help="YYYY-MM: stays checking out that month")
    a = ap.parse_args()
    today = date.today()
    if a.month:
        y, mo = map(int, a.month.split("-"))
        start, end = f"{a.month}-01", f"{a.month}-{calendar.monthrange(y, mo)[1]:02d}"
    else:
        start, end = f"{today.year}-01-01", today.isoformat()
    end = min(end, today.isoformat())
    qbo, g = bridge.qbo_client(), bridge.guesty_client()
    gres = guesty_by_checkout(g, start, end)
    # invoices are dated at check-in, so look back far enough for long stays
    since = (date.fromisoformat(start) - timedelta(days=120)).isoformat()
    invs = qbo.query_all(f"SELECT * FROM Invoice WHERE Balance > '0' AND TxnDate >= '{since}' AND TxnDate <= '{end}'", "Invoice")
    find = lambda code: gres.get(code) or next((v for k, v in gres.items()
                                                if len(code) > 12 and k.startswith(code[:15])), None)
    gres.update(guesty_by_code(g, sorted({code_of(i) for i in invs if not find(code_of(i))})))
    rows = []
    for inv in invs:
        code = code_of(inv)
        gr = find(code)
        if gr is None and not (start <= inv["TxnDate"] <= end):
            continue                       # not a Guesty stay: keep it only if dated in the window
        if gr is not None and not (start <= (gr.get("checkOut") or "")[:10] <= end):
            continue                       # stay ends outside the window (or has not ended)
        status, why = classify(inv, gr, today)
        cls = next((l["SalesItemLineDetail"].get("ClassRef", {}).get("name") for l in inv["Line"]
                    if l.get("DetailType") == "SalesItemLineDetail"), "")
        rows.append({"status": status, "invoice": inv.get("DocNumber"), "guest": inv["CustomerRef"].get("name", ""),
                     "listing": (cls or "").replace("Listings:", ""), "source": (gr or {}).get("source", ""),
                     "check_in": inv["TxnDate"], "check_out": ((gr or {}).get("checkOut") or "")[:10],
                     "invoice_total": inv["TotalAmt"], "open": round(inv["Balance"], 2), "why": why})
    # Elektra owner-paid Airbnb: cleaning fees held in Airbnb clearing (no open invoice; any window)
    for h in fix_owner_paid_airbnb.held_cleaning(today.isoformat()):
        rows.append({"status": "HELD CLEANING", "source": "airbnb (owner-paid)", "check_out": "", **h})
    order = ["UNCOLLECTED", "HELD CLEANING", "NOT APPLIED", "CANCELLED", "PAYOUT MISSING", "CHECK", "AWAITING PAYOUT", "ROUNDING"]
    rows.sort(key=lambda r: (order.index(r["status"]), r["check_out"] or r["check_in"]))
    REVIEW_DIR.mkdir(exist_ok=True)
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    path = REVIEW_DIR / f"uncollected_{a.month or 'ytd'}_{stamp}.csv"
    with path.open("w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0]) if rows else ["status"])
        w.writeheader()
        w.writerows(rows)
    inv_rows = [r for r in rows if r["status"] != "HELD CLEANING"]
    print(f"Open invoices on stays checked out {start} to {end}: {len(inv_rows)}, ${sum(r['open'] for r in inv_rows):,.2f}"
          " (HELD CLEANING listed separately: not open invoices)")
    for s in order:
        part = [r for r in rows if r["status"] == s]
        if part:
            print(f"\n{s}: {len(part)}, ${sum(r['open'] for r in part):,.2f}")
            for r in part:
                print(f"  {str(r['invoice'])[:18]:18} {r['check_out'] or r['check_in']:10} {r['listing'][:20]:20} "
                      f"{r['open']:>9.2f}  {r['why'][:90]}")
    print(f"\n-> {path.relative_to(REVIEW_DIR.parent)}")


if __name__ == "__main__":
    main()
