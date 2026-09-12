"""How a QBO INCOME ledger line is classified — the ONE definition, shared by the
money math (``netrevenue.engine``) and the display layer (``reporting.other_income``,
which feeds the Section-1 Booking Breakdown).

Keeping both predicates here means the commission base, the Net-Rental-Revenue total,
and the rows the Booking Breakdown shows can never drift from each other: a line that
earns commission is exactly a line that gets a breakdown row.

Pure SQL-fragment constants — NO imports, so this module is safe to ship in the
display-only deploy bundle alongside ``reporting/booking_breakdown.py``.

Four buckets, evaluated in this order:

0. ``NOT_OWNER_PRED``  — not the owner's money at all. Leaves the statement entirely.
1. ``NON_RENTAL_PRED`` — owner money that is NOT rental revenue. Wins over everything.
2. ``RENT_PRED``       — commissionable rent; also what earns a Booking-Breakdown row.
3. everything else     — non-commissioned credits; stay in the "Other Credits" section.
"""

# -- 0. Not the owner's money ------------------------------------------------
# Valta's own legal / insurance matters, which the bookkeeper marks with a leading
# "(legal)" in the QBO description. Owner decision 2026-08-28: these are NOT the
# owner's money in any form -- unlike NON_RENTAL_PRED (which only reclassifies rent
# as an owner adjustment and leaves the payout unchanged), a NOT_OWNER row leaves the
# statement ENTIRELY: out of gross revenue, out of owner_adjustments, out of the payout.
# `run_month_close._drop_non_owner_income` marks the matching rows
# ``include_in_statement=0`` each build, so a re-sync that re-adds them is re-suppressed.
#
# Deliberately narrow. It does NOT sweep in insurance claims that ARE the owner's --
# a sofa replacement or a washing-machine claim pays for the owner's own property and
# stays in the statement. The "(legal)" marker is the only signal that separates them,
# so match on it and nothing else; ask the owner before widening this.
NOT_OWNER_PRED = (
    "source='qbo' AND category='INCOME' AND lower(description) LIKE '(legal)%'"
)

# ── 1. Not rental revenue ────────────────────────────────────────────────────
# Refundable deposits (held for the guest/tenant, refunded on move-out — never earned)
# and utility reimbursements (a pass-through of the owner's own bill). Owner decision
# 2026-08: both leave gross_booking_revenue and land in owner_adjustments instead, so
# the owner is paid EXACTLY the same amount — they simply stop reading as rent. Neither
# was ever commissioned.
#
# QBO spellings vary a lot ("Utilites", "Utilitites", "utilitities", "utility",
# "refundable pet deposit", "pet deposit (refundable) $500"); the two substrings below
# cover every variant present in the ledger.
NON_RENTAL_PRED = (
    "source='qbo' AND category='INCOME' AND ("
    "lower(description) LIKE '%refundable%' "
    "OR lower(description) LIKE '%utilit%')"
)

# ── 2a. Bookings QBO recorded without a usable description ───────────────────
# Some channel bookings are keyed into QuickBooks with an EMPTY description; the only
# thing identifying them is the customer string, which QBO writes in Guesty's
# "<channel> - <guest> - <confirmation code>" shape (e.g.
# "bookingcom - Jocelyn Robinson - 4787657392", "airbnb - Zachary Clarey - HM28AC9QJH",
# "manual - Sono Bello, LLC - GY-JHevDRd2"). Owner decision 2026-08: these ARE bookings
# — they earn commission and belong in the Booking Breakdown like any other.
#
# Matching on the channel prefix is deliberately strict: misc company deposits (bare
# "deposit", ACH transfers, cleaner payouts) carry no such customer string and stay out.
# The NON_RENTAL guard in RENT_PRED still wins, so "Refundable Deposit" booked against
# "manual - Aaron Clyde Corsi - GY-HB8C8PPx" remains a non-rental credit.
# The clause is PREFIX-anchored ("<channel> - %"), so every spelling QBO actually writes
# has to be listed. "homesvillasbymarriott" is Marriott's own string and does NOT match
# "marriott"; without it 13 booking lines (extra night, pet fee, tenant Zelle, cleaning,
# parking fee, and 5 bookings QBO left description-less) fell through to the
# uncommissioned "Other Credits" bucket -- in Net Rental Revenue, out of the commission
# base, and with no Section-1 row, so they could not reach `_merge_addon` either.
BOOKING_CHANNELS = (
    "airbnb", "airbnb2", "bookingcom", "booking.com", "bnbfinder", "blueground",
    "bluegroundnestpick", "expedia", "hipcamp", "homeaway", "homesvillasbymarriott",
    "hopper", "hotels", "manual", "marriott", "tripcom", "trip.com", "vrbo", "whimstay",
)
_CHANNEL_CLAUSE = " OR ".join(
    f"lower(vendor_customer) LIKE '{c} - %'" for c in BOOKING_CHANNELS
)

# ── 2. Commissionable rent ───────────────────────────────────────────────────
# import_ltr LTR/DEFERRED rows, plain QBO rent Deposits ("July Rent"), pet fees,
# extra-night charges, and tenant Zelle rent payments. Owner decision 2026-08:
# 'extra night', 'extended stay', 'pet fee' and tenant Zelle deposits ARE rent and DO
# earn commission. LONG-TERM rent does NOT belong here: the owner supplies it as an
# LTR CSV row, which import_ltr posts as source_object IN ('LTR','DEFERRED') and the
# first clause below already catches. Never try to infer a lease from a recurring QBO
# deposit -- 'DEPOSIT ID NUMBER XX8484' carries nothing to match on.
#
# Excluded by name:
#   * garage / parking rent      — not a booking, never commissioned
#   * VALTA HOMES LLC Zelle rows — internal company transfers, not tenant rent
#   * the "$5 testing" Zelle row — a setup artifact
#   * anything in NON_RENTAL_PRED — the guard below must come FIRST, so a description
#     matching both patterns ("Pet fee refundable deposit", "Zelle payment from Jennie
#     Allen utilities") stays out of rent.
#
# Misc company deposits (ACH transfers, "payment to Maria", bare "deposit") match none
# of these patterns, so they fall through to bucket 3 — uncommissioned, and shown under
# "Other Credits" rather than as a booking.
RENT_PRED = (
    "source='qbo' AND category='INCOME' "
    "AND NOT (lower(description) LIKE '%refundable%' "
    "         OR lower(description) LIKE '%utilit%') "
    "AND ("
    "source_object IN ('LTR','DEFERRED') "
    "OR (lower(description) LIKE '%rent%' "
    "    AND lower(description) NOT LIKE '%garage%' "
    "    AND lower(description) NOT LIKE '%parking%') "
    "OR lower(description) LIKE '%pet fee%' "
    "OR lower(description) LIKE '%extra night%' "
    "OR lower(description) LIKE '%additional night%' "
    "OR lower(description) LIKE '%extended stay%' "
    "OR (lower(description) LIKE '%zelle%' "
    "    AND lower(description) NOT LIKE '%valta homes%' "
    "    AND lower(description) NOT LIKE '%testing%') "
    f"OR ({_CHANNEL_CLAUSE}))"
)
