"""The period's booking sources, loaded ONCE — the single setup step behind Section 1.

Every product that renders a statement needs the same five things for a period, and
each used to assemble them itself: ``run_month_close`` (Excel), ``dashboard`` (web +
PDF) and ``reporting.monthly_summary`` each had their own copy of this block. Three
copies of "what are this month's bookings?" is exactly the drift risk the shared
booking-breakdown layer exists to remove, so it lives here instead:

    pb             the Guesty payment-breakdown CSV (the stored fee model), or None
    codes          every confirmation code that CSV covers
    ltr            LTR / deferred display records          (booking source #2)
    other          fee-free rental income with no pb row   (booking source #3)
    claimed        codes an LTR/DEFERRED row superseded — dedup, LTR wins
    net_overrides  QBO-adjusted nets for cancelled/refunded bookings

``build_by_unit`` filters by ``members`` itself, so the whole-portfolio record lists can
be handed to it unchanged. That is what makes loading once correct AND cheap: the Excel
build used to re-read the same CSV once per statement (72x a month).

Import-light on purpose — pandas, ``paths`` and the display-only readers, nothing from
the pipeline — so it ships in the read-only deploy bundle and imports the same way as a
package module (``python -m src.…``) and as a flat import under the Streamlit script.
"""

import pandas as pd

# Works both as a package module (run_month_close, monthly_summary) and as a top-level
# import under the flat dashboard script / deploy bundle — same shim as ltr.records.
try:
    from .. import paths
    from ..ltr.records import (build_records as _ltr_build, guesty_code_checker,
                               ltr_claimed_codes)
    from .other_income import build_records as _other_build
    from .qbo_adjustments import adjusted_nets
except ImportError:  # flat: src/ is on sys.path
    import paths
    from ltr.records import (build_records as _ltr_build, guesty_code_checker,
                             ltr_claimed_codes)
    from reporting.other_income import build_records as _other_build
    from reporting.qbo_adjustments import adjusted_nets


def period_bounds(period: str) -> tuple[str, str]:
    """('2026-07') -> ('2026-07-01', '2026-07-31')."""
    import calendar
    y, m = map(int, period.split("-"))
    return f"{y:04d}-{m:02d}-01", f"{y:04d}-{m:02d}-{calendar.monthrange(y, m)[1]:02d}"


class PeriodSources:
    """All three booking sources for one period, plus the dedup/override maps.

    ``pids`` is the set of property_ids to load for. Pass the whole portfolio (the
    Excel build, the summary sheets) or a single statement's members (the dashboard) —
    ``build_by_unit`` narrows to ``members`` either way, and every lookup below is
    keyed by confirmation code or by property_id, so a portfolio-wide load answers a
    per-statement question identically.
    """

    def __init__(self, conn, period: str, pids, pb=None):
        self.period = period
        self.period_start, self.period_end = period_bounds(period)

        # The stored fee model. ONE writer (breakdown/fetch_month); a payment_model
        # change does not reach a statement until that CSV is regenerated. `pb` lets a
        # caller supply an already-cached read (the dashboard memoises it).
        if pb is not None:
            self.pb = pb
        else:
            try:
                self.pb = pd.read_csv(paths.payment_breakdown_csv(period))
            except FileNotFoundError:
                self.pb = None
        self.codes = (set(self.pb["confirmationCode"].astype(str))
                      if self.pb is not None and len(self.pb) else set())

        # Source #2. "Already a Guesty booking THIS period?" is one shared, period-scoped
        # predicate (ltr.records.guesty_code_checker) so all three products dedup alike.
        self.ltr, self.ltr_covered = _ltr_build(period, pids,
                                                guesty_code_checker(conn, period))
        # Source #3: rental income with no payment-breakdown row — synthetic Guesty
        # income (Hipcamp/OsbrRV) and QBO-recorded rent. `codes` keeps a booking from
        # getting both a fee-waterfall row and a fee-free one.
        # (property, guest) -> code, ONLY where the pair is unique, so a later-billed
        # add-on whose note names neither the code nor a matching vendor id can still
        # find its booking. Ambiguous pairs are dropped rather than guessed.
        self.pb_guest_index = {}
        if self.pb is not None and len(self.pb) and "guest" in self.pb.columns:
            seen = {}
            for _, r in self.pb.iterrows():
                key = (r["property_id"], str(r["guest"] or "").strip().lower())
                if not key[1]:
                    continue
                seen[key] = None if key in seen else str(r["confirmationCode"])
            self.pb_guest_index = {k: v for k, v in seen.items() if v}
        self.other = _other_build(conn, self.period_start, self.period_end, pids,
                                  self.codes, self.pb_guest_index)

        # On a confirmation-code collision the LTR/DEFERRED row wins (owner, 2026-08-28).
        self.claimed = ltr_claimed_codes(conn, period)
        # The SAME map the build applied to the ledger, so Section 1 foots with the
        # stored totals rather than re-deriving a different net.
        self.net_overrides = adjusted_nets(conn, period)
