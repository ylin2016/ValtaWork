"""Which DocNumber / customer a reservation carries in QBO (VRP's conventions)."""
from __future__ import annotations

DOC_MAX = 21          # QBO DocNumber limit; VRP truncated longer codes to it


def doc_code(r: dict) -> str:
    """The reservation's identity in QBO before truncation.

    Booking.com is the one channel VRP did not key on Guesty's confirmationCode
    (`BC-0BB5ZGqxV`): it used Booking's own 10-digit reservation number."""
    if str(r.get("source", "")).lower() == "booking.com":
        bid = ((r.get("integration") or {}).get("bookingCom") or {}).get("reservationId")
        if bid:
            return str(bid)
    return r["confirmationCode"]


def invoice_doc(r: dict) -> str:
    return doc_code(r)[:DOC_MAX]


def bill_doc(prefix: str, r: dict) -> str:
    return f"{prefix}_{doc_code(r)}"[:DOC_MAX]


def customer_name(r: dict) -> str:
    """`<source> - <guest> - <code>`; source lower-cased, VRBO storefronts -> homeaway."""
    src = str(r.get("source") or "").lower()
    src = {"booking.com": "bookingcom", "vrbo": "homeaway", "homeaway2": "homeaway",
           "homeaway ca": "homeaway", "airbnb2": "airbnb", "be-api": "manual",
           "website": "manual"}.get(src, src)
    guest = ((r.get("guest") or {}).get("fullName") or "").strip()
    return f"{src} - {guest} - {doc_code(r)}"
