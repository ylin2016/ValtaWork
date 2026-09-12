"""Single source of truth for a property's PM fee rate in a period.

Kept in its own dependency-free module (no relative imports) so it is importable
BOTH as part of the `src` package (statement_engine / run_month_close) and as a
bare sibling import from the Streamlit dashboard, which runs as a script.
"""


def resolve_pm_fee_rate(conn, property_id: str, period_start: str):
    """Return the owner_contracts PM rate effective for `period_start`, or None
    if the property has no configured rate for that period.

    There is DELIBERATELY no silent default: a missing rate must never quietly
    commission at a guessed number. Every caller (statement_engine for stored
    amount_due, dashboard table/PDF) resolves through this one helper, and each
    handles None by halting/asking rather than substituting a default — so a
    property that needs commissioning but has no rate surfaces loudly instead of
    being mis-stated. The effective-date logic mirrors what the stored totals use,
    so all products commission at the same rate.
    """
    c = conn.execute(
        """SELECT pm_fee_rate FROM owner_contracts
           WHERE property_id=? AND effective_start<=?
             AND (effective_end IS NULL OR effective_end>=?)
           ORDER BY effective_start DESC LIMIT 1""",
        (property_id, period_start, period_start),
    ).fetchone()
    rate = c[0] if c else None
    return float(rate) if rate is not None else None


def owner_pays_cleaning_at(item: dict, period_start: str) -> bool:
    """Is `owner_pays_cleaning` in force for this mapping entry in this period?

    `mapping_classes.yml` carries a plain boolean, which has NO time dimension — the same
    trap as `Listing_contacts.csv`. Flipping it bare would restate every past month, so an
    optional `owner_pays_cleaning_from: 'YYYY-MM-DD'` scopes it to periods on or after that
    date (OSBR took the guest cleaning fee from 2026-08-01; its 19 earlier months keep
    cleaning as Valta's income). No from-date means "has always applied", which is how
    every other property already behaves.
    """
    if not item.get("owner_pays_cleaning"):
        return False
    frm = item.get("owner_pays_cleaning_from")
    return True if not frm else str(period_start)[:10] >= str(frm)[:10]


def pm_rate_resolver(conn, property_id: str, period_start: str, period_end: str):
    """Return ``rate_at(date) -> float | None`` for one property across one period.

    `resolve_pm_fee_rate` answers "what rate applies to this PERIOD", which is all a
    statement needed while every rate change landed on a month boundary. A rate that
    changes MID-month cannot be expressed that way: `seattle_9021` went to 22% for
    check-ins after 2026-08-10, so August carries two rates and one scalar is wrong for
    half the bookings.

    `owner_contracts` was already effective-dated, so nothing new is stored — this just
    resolves at a booking's own date instead of at `period_start`. The date to pass is
    the guesty INCOME line's `posting_date`, which IS the check-in (that is how
    `breakdown.adapter` dates them), so "rate for a booking" and "rate on its check-in"
    are the same question.

    When only ONE rate covers the period the returned callable is a constant — byte-identical
    to the old behaviour for every property that has no mid-period change, which is all of
    them but one. Callers therefore cannot drift by adopting it.
    """
    rows = conn.execute(
        """SELECT effective_start, effective_end, pm_fee_rate FROM owner_contracts
           WHERE property_id=? AND effective_start<=?
             AND (effective_end IS NULL OR effective_end>=?)
           ORDER BY effective_start""",
        (property_id, period_end, period_start),
    ).fetchall()
    spans = [(r[0], r[1], float(r[2])) for r in rows if r[2] is not None]

    if not spans:
        return lambda _date: None
    if len(spans) == 1:
        only = spans[0][2]
        return lambda _date: only

    def rate_at(date):
        d = str(date)[:10] if date else period_start
        best = None
        for start, end, rate in spans:                 # ordered by effective_start
            if start <= d and (end is None or end >= d):
                best = rate
        # A date before the first span (a line dated outside its own period) falls back to
        # the earliest rate rather than None — None would raise "no PM rate configured"
        # for a property that plainly has one.
        return best if best is not None else spans[0][2]

    return rate_at
