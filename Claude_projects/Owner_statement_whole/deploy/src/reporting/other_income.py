"""Booking-Breakdown source #3: rental income that has NO payment-breakdown row.

The Section-1 fee waterfall is built from ``payment_breakdown_<period>.csv`` (Guesty
API) plus the LTR/deferred CSV. Two kinds of real rental income are in neither, yet
DO count toward Net Rental Revenue — which is why Section 1 used to total less than
Section 2 (the "BK Net Rental Revenue" vs "Net Rental Revenue" gap):

* **Synthetic Guesty income** — ledger rows with ``source='guesty'`` whose confirmation
  code never came back from the API pull. Today that is the Hipcamp RV bookings posted
  as ``source_object='OsbrRV'`` on ``osbr_rv``; they already flow into
  ``gross_booking_revenue``, they were just invisible in Section 1.
* **QBO-recorded rent** — long-term Lease Rent deposits, tenant Zelle payments, pet
  fees and extra-night charges booked straight into QuickBooks, never through Guesty.

Both render FEE-FREE (Guest Pay + Net Rental Revenue only, no channel/Guesty/Stripe
fee, no tax), matching the LTR convention.

Which QBO rows qualify is ``common.income_rules.RENT_PRED`` — the SAME predicate the
engine commissions on, so "earns commission" and "gets a breakdown row" are one rule.
Utilities, refundable deposits and misc company deposits (ACH transfers, payouts) match
neither and stay in the "Other Credits" section.

DB-reading but display-only: callers pass their own connection, so this ships in the
deploy bundle next to ``booking_breakdown.py``.
"""

# Works both as a package module (run_month_close) and as a top-level import
# (the Streamlit dashboard inserts src/ on sys.path).
try:
    from ..common.income_rules import RENT_PRED
except ImportError:  # flat import under the running dashboard / deploy bundle
    from common.income_rules import RENT_PRED

# LTR/DEFERRED ledger rows are already rendered from the LTR CSV by ltr.records, so
# they must not produce a second row here.
_LTR_OBJECTS = ("LTR", "DEFERRED")


def _split_vendor(vendor):
    """QBO customer strings look like 'bookingcom - Jessica Smith - 7000016281' (and
    'manual - Aaron Clyde Corsi - GY-HB8C8PPx'). Return (channel, guest, code); any
    piece that isn't there comes back ''. The code is what lets a pet fee attach to the
    booking it belongs to."""
    parts = [p.strip() for p in str(vendor or "").split(" - ")]
    if len(parts) >= 3:
        return parts[0], " - ".join(parts[1:-1]), parts[-1]
    if len(parts) == 2:
        return parts[0], parts[1], ""
    return "", parts[0] if parts else "", ""


_CHANNEL_LABELS = {
    "bookingcom": "Booking.com", "booking.com": "Booking.com", "airbnb": "Airbnb",
    "airbnb2": "Airbnb", "expedia": "Expedia", "hotels": "Hotels.com", "vrbo": "VRBO",
    "homeaway": "HomeAway", "hopper": "Hopper", "marriott": "Marriott",
    "hipcamp": "Hipcamp", "manual": "Manual",
}


def _label(channel, subcategory, source_txn_id=""):
    """The 'Bookings' column value — the channel when we can name one, else the QBO
    subcategory ('Lease Rent'), else a generic tag."""
    key = str(channel or "").strip().lower()
    if key in _CHANNEL_LABELS:
        return _CHANNEL_LABELS[key]
    head = str(source_txn_id or "").split("-")[0].strip().lower()
    if head in _CHANNEL_LABELS:
        return _CHANNEL_LABELS[head]
    sub = str(subcategory or "").strip()
    if sub and sub.lower() not in ("other deposit", "deposit"):
        return sub
    return channel or "Direct"


_ADDON_KEYWORDS = [
    ("pet fee", "Pet fee"), ("pet", "Pet fee"), ("parking", "Parking"),
    ("extra guest", "Extra guest"), ("extra person", "Extra guest"),
    ("additional guest", "Extra guest"), ("extended night", "Extended night"),
    ("extra night", "Extra night"), ("additional night", "Extra night"),
    ("extended stay", "Extended stay"), ("guest extended", "Extended stay"),
    ("resort", "Resort fee"), ("damage", "Damage fee"), ("cleaning", "Cleaning"),
    ("late checkout", "Late checkout"), ("early check", "Early check-in"),
]


def addon_label(description):
    """A short, readable name for a QBO-recorded add-on.

    QBO descriptions are receipt FILENAMES — "20260829_Ed Moore_OSBR 4_BC-7JjOq7O4r_Ann
    Smith pet fee_-50.png" — so showing them raw on an owner statement is unreadable.
    Match the fee word and return just that ("Pet fee"); fall back to the raw text when
    nothing matches, which is better than showing nothing at all.
    """
    d = str(description or "").lower()
    for needle, label in _ADDON_KEYWORDS:
        if needle in d:
            return label
    return str(description or "").strip()


def build_records(conn, period_start, period_end, member_ids, pb_codes=(),
                  pb_guest_index=None):
    """Return the fee-free breakdown records for `member_ids` over the period.

    Each record: property_id, code, label, guest, amount, date. `amount` is both the
    Guest Pay and the Net Rental Revenue (no fees, no tax are known for these).

    `pb_codes` is the set of confirmation codes the Guesty payment breakdown already
    covers — guesty ledger rows in that set are skipped so a booking is never shown
    twice (the breakdown row wins, it has the full fee waterfall).

    `pb_guest_index` maps (property_id, lowercased guest name) -> confirmation code, and
    ONLY where that pair identifies exactly one booking. It is the last resort for
    attaching a later-billed add-on: Booking.com puts its own numeric reservation id in
    vendor_customer, and a bookkeeper's note ("John Gee pet fee cottage 3") need not
    repeat the code either, so neither the vendor code nor the description can match.
    Ambiguous pairs are left out of the index by construction, so this can never guess
    between two bookings — it either resolves uniquely or the add-on stands alone.
    """
    if not member_ids:
        return []
    ph = ",".join("?" * len(member_ids))
    covered = {str(c) for c in (pb_codes or ())}
    out = []

    # (a) Guesty ledger income with no payment-breakdown row (Hipcamp / OsbrRV today).
    for r in conn.execute(f"""
            SELECT property_id, source_txn_id, source_object, subcategory,
                   vendor_customer, description, amount, base_amount, posting_date
              FROM ledger_lines
             WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
               AND include_in_statement=1 AND source='guesty' AND category='INCOME'
             ORDER BY posting_date, source_txn_id""",
            (*member_ids, period_start, period_end)):
        code = str(r["source_txn_id"] or "")
        if code in covered:
            continue
        channel, guest, _ = _split_vendor(r["vendor_customer"])
        out.append({
            "property_id": r["property_id"],
            "code": code,
            "label": _label(channel, r["source_object"], code),
            "guest": guest or str(r["description"] or ""),
            # What this add-on IS, for the Add-ons detail on the booking it merges into.
            "detail": addon_label(r["description"]),
            "amount": round(float(r["amount"] or 0.0), 2),
            # The gross the guest paid, before the channel's cut. `amount` is already net
            # of it (run_month_close._apply_addon_channel_fees writes the deduction into
            # the ledger), so the renderer takes the fee as gross - amount rather than
            # recomputing a rate that could drift from what the ledger actually did.
            "gross": round(float(r["base_amount"] if r["base_amount"] is not None
                                 else r["amount"] or 0.0), 2),
            "date": r["posting_date"],
        })

    # (b) QBO-recorded rent — same predicate the engine commissions on.
    for r in conn.execute(f"""
            SELECT ledger_id, property_id, source_object, subcategory, vendor_customer,
                   description, amount, base_amount, posting_date
              FROM ledger_lines
             WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
               AND include_in_statement=1 AND {RENT_PRED}
               AND (source_object IS NULL OR source_object NOT IN (?, ?))
             ORDER BY posting_date""",
            (*member_ids, period_start, period_end, *_LTR_OBJECTS)):
        channel, guest, code = _split_vendor(r["vendor_customer"])
        # Booking.com writes its own NUMERIC reservation id into vendor_customer
        # ("bookingcom - Ann Smith - 5081494039") while Guesty keys the booking
        # "BC-7JjOq7O4r", so the vendor code can never match a breakdown row and the
        # add-on lands as its own line under a bare number. The bookkeeper does put the
        # real code in the description ("…_BC-7JjOq7O4r_Ann Smith pet fee_-50.png"), so
        # prefer a code from there whenever the breakdown actually covers it — that is
        # what lets `_merge_addon` fold the fee into its booking's Net Rental Revenue.
        if code not in covered:
            desc = str(r["description"] or "")
            code = next((c for c in covered if c and c in desc), code)
        if code not in covered and pb_guest_index:
            # Still unmatched: try (property, guest). `guest` here is the name QBO carries
            # in vendor_customer, which is the same name Guesty reports on the booking.
            code = pb_guest_index.get((r["property_id"], str(guest or "").strip().lower()), code)
        out.append({
            "property_id": r["property_id"],
            "ledger_id": r["ledger_id"],
            "code": code,
            "label": _label(channel, r["subcategory"]),
            "guest": guest or str(r["description"] or ""),
            # What this add-on IS, shown next to the Add-ons figure on the booking it
            # merges into — "Add-ons $50.00" alone is not answerable for an owner.
            "detail": addon_label(r["description"]),
            "amount": round(float(r["amount"] or 0.0), 2),
            # The gross the guest paid, before the channel's cut. `amount` is already net
            # of it (run_month_close._apply_addon_channel_fees writes the deduction into
            # the ledger), so the renderer takes the fee as gross - amount rather than
            # recomputing a rate that could drift from what the ledger actually did.
            "gross": round(float(r["base_amount"] if r["base_amount"] is not None
                                 else r["amount"] or 0.0), 2),
            "date": r["posting_date"],
        })

    return out


def ledger_keys(records):
    """(property_id, posting_date, amount) triples for the records above — lets the
    dashboard/Excel drop these lines from the "Other Credits" section now that they
    render as booking rows, so the same money is never shown twice."""
    return {(r["property_id"], r["date"], round(float(r["amount"]), 2)) for r in records}
