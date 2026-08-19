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
