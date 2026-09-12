"""QBO month-end adjustments to cancelled / refunded bookings — the authoritative basis.

Owner decision 2026-08-28: **when QuickBooks has ADJUSTED a cancelled or refunded
booking, the QBO record wins** over the Guesty payment-breakdown net.

A cancellation that happens after check-in is not the flat cancellation fee the channel
reports — the owner keeps part of the stay, the rest is refunded, and Valta re-cuts the
numbers in QuickBooks at month end. Guesty's payment breakdown still shows the
pre-adjustment figures, so commissioning on it over- or under-charges the owner.
`HA-xdDv3ud` (Bellevue 2243, 2026-07): the guest paid $1,053.17 of a $5,419.74 booking
after a $4,366.57 refund; Guesty's net is $1,011.50, but QBO recognized $772.68 (the
$225.00 cleaning fee and the $55.49 Stripe fee come out) and billed $139.08 of commission.

**The commission Bill divided by the PM rate IS the adjusted net.** That is the only
signal present for every adjusted booking: some carry a full re-cut invoice set
(accommodation fare / cleaning / pet fee / Stripe fee), most carry nothing but the
`Management Commission | Cancelled | <code>` Bill. Dividing it back out recovers exactly
the basis QBO used and makes our commission tie to QuickBooks to the cent — the same
technique the LTR importer uses for long-term rent (see CLAUDE.md).

Rows whose commission Bill is $0.00 are NOT adjustments: that is how QuickBooks records a
cancellation it charged nothing for, including the $6.10 cancellation residues that leave
the statement entirely (see reporting.booking_breakdown).

DB-reading but display-only — callers pass their own connection, so this ships in the
deploy bundle next to ``booking_breakdown.py`` and ``other_income.py``.
"""
import calendar

# Works both as a package module (run_month_close) and as a top-level import
# (the Streamlit dashboard inserts src/ on sys.path).
try:
    from ..scope.pm_rate import resolve_pm_fee_rate
except ImportError:  # flat import under the running dashboard / deploy bundle
    from scope.pm_rate import resolve_pm_fee_rate

# QBO writes the adjustment as "Management Commission | Cancelled | <code> | <dates> | N nights".
# Read the OWNER-PAYABLE side of the bill, not the revenue side. Both lines carry the
# same description and magnitude, but only this one has a stable sign: QBO debits the
# owner payable (POSITIVE) to charge the owner, so `qbo_sync` stores it NEGATIVE — in
# every period, whether or not that period has been re-synced since the sign fix (the
# revenue side flips between vintages; see CLAUDE.md). It is also the semantically right
# line: what the OWNER was charged.
_COMMISSION_ACCOUNT = "Trust Liabilities:Owner Payables:1A - Net Earnings:Management Commissions"
_DESC_PREFIX = "Management Commission | Cancelled |"


def _code(description):
    """Confirmation code out of the QBO description, or '' if it is not the expected shape."""
    parts = [p.strip() for p in str(description or "").split("|")]
    return parts[2] if len(parts) >= 3 else ""


def adjusted_nets(conn, period):
    """{confirmation_code: adjusted_net_revenue} for `period`.

    One entry per booking QBO adjusted, keyed by confirmation code so both the ledger
    override (run_month_close) and the Section-1 Booking Breakdown read the SAME numbers
    and cannot drift. A booking whose property has no PM rate configured is skipped
    rather than guessed at — see scope.pm_rate.resolve_pm_fee_rate.
    """
    y, m = map(int, period.split("-"))
    start = f"{y:04d}-{m:02d}-01"
    end = f"{y:04d}-{m:02d}-{calendar.monthrange(y, m)[1]:02d}"

    by_code, rates = {}, {}
    for pid, desc, comm in conn.execute(
            """SELECT property_id, description, SUM(amount) FROM ledger_lines
                WHERE qbo_account=? AND description LIKE ? AND amount<>0
                  AND posting_date>=? AND posting_date<=?
                GROUP BY property_id, description""",
            (_COMMISSION_ACCOUNT, _DESC_PREFIX + "%", start, end)):
        code = _code(desc)
        if not code:
            continue
        if pid not in rates:
            rates[pid] = resolve_pm_fee_rate(conn, pid, start)
        rate = rates[pid]
        if not rate:
            continue
        # Stored negative (a cost to the owner); the basis is |commission| / rate. A
        # net-positive group is a pure credit back, not a re-cut basis, so skip it.
        if float(comm) >= 0:
            continue
        by_code[code] = round(-float(comm) / float(rate), 2)
    return by_code


def unrated_adjustments(conn, period):
    """Adjusted bookings SKIPPED because their property has no configured PM rate.

    Returned as (property_id, code, commission) so the build can print them instead of
    silently ignoring an adjustment the owner made.
    """
    y, m = map(int, period.split("-"))
    start = f"{y:04d}-{m:02d}-01"
    end = f"{y:04d}-{m:02d}-{calendar.monthrange(y, m)[1]:02d}"
    out = []
    for pid, desc, comm in conn.execute(
            """SELECT property_id, description, SUM(amount) FROM ledger_lines
                WHERE qbo_account=? AND description LIKE ? AND amount<>0
                  AND posting_date>=? AND posting_date<=?
                GROUP BY property_id, description""",
            (_COMMISSION_ACCOUNT, _DESC_PREFIX + "%", start, end)):
        if not resolve_pm_fee_rate(conn, pid, start):
            out.append((pid, _code(desc), round(-float(comm), 2)))
    return out
