"""Shared Booking-Breakdown (Section 1) builder.

The per-unit fee waterfall shown ABOVE each unit's Net Revenue table. Built ONCE
here so the dashboard, the Excel writer, and the PDF render identical tables and can
never drift. Pure/data-only: a payment-breakdown DataFrame + LTR display records +
other-income records in, renderer-agnostic list-of-dict rows out. NO db / paths /
streamlit imports, so it imports the same way in the flat dashboard, the package
pipeline, and the deploy bundle.

THREE booking sources feed it, so Section 1 totals the same Net Rental Revenue as the
Net Revenue section below it:
  1. the Guesty payment breakdown  — full fee waterfall (guesty_row)
  2. LTR / deferred bookings       — fee-free (ltr_row)
  3. everything else that is rent  — fee-free (other_row); synthetic Guesty income
     (Hipcamp/OsbrRV) and QBO-recorded rent. See reporting.other_income.

Row schema (one dict per booking, plus a TOTAL row per unit):
  Conf Code, Bookings (channel; TOTAL cell shows the booking count), Guest Pay, Fees
  (channel+Guesty+Stripe folded; components kept as hidden _ch/_gu/_st for the dashboard
  hover), Cleaning Fee, Tax, Net Rental Revenue, and a hidden _total flag.
"""

import re

# Displayed columns in owner-set order; NUM = the right-aligned dollar columns.
# Accommodation / Markup / Add-ons split the booking amount so the owner can see how
# much of a stay was actual room rent (owner request 2026-09-03). Add-ons folds pet,
# parking, extra-guest, resort, service and other fees into one column; cleaning and
# tax keep their own. They are produced by payment_model.compute_breakdown, so a period
# pulled BEFORE that change has no such columns in its stored CSV — `guesty_row` yields
# None for them and the renderers show a blank, never a misleading $0.00.
COMPONENTS = ["Accommodation", "Markup", "Add-ons"]
DISP = ["Conf Code", "Bookings", "Guest Pay", "Accommodation", "Markup", "Add-ons",
        "Fees", "Cleaning Fee", "Tax", "Net Rental Revenue"]
NUM = ["Guest Pay"] + COMPONENTS + ["Fees", "Cleaning Fee", "Tax", "Net Rental Revenue"]
# _ar = an ARC the cohost keeps (payment_model.ARC_NOT_OWNER_INCOME); 0.00 on
# every ordinary booking.
FEE_PARTS = ["_ch", "_gu", "_st", "_ar"]

# --- Dropped cancellations (owner decision 2026-08-28) -----------------------------
# A canceled booking whose InvoiceItem is at or below this is a cancellation-fee
# residue, not a stay: VRBO/HomeAway keeps a flat $6.10 out of the $200 deposit and
# refunds the rest, leaving a ~$5.56 "booking" that clutters the statement for money
# the owner never really earned. Those rows leave the statement ENTIRELY — no
# Section-1 breakdown row and no Section-2 net-revenue line (run_month_close marks the
# guesty ledger row include_in_statement=0, so gross revenue, commission and
# amount_due drop it too).
#
# The threshold is a value, not a shape: it keeps every real cancellation (the next
# canceled booking up is $50.00, and Airbnb cancellations run into the thousands).
# Raising it starts deleting real money — check the gap first.
DROPPED_CANCELLATION_MAX_INVOICE = 6.10

# Owner-directed drops: a cancellation residue ABOVE the threshold that the owner has
# nonetheless confirmed is not their money. Listed per reservation rather than by raising
# the threshold, because the threshold is a value and not a shape -- the next canceled
# booking above $6.10 is $50.00, so lifting it far enough to catch a $54.20 residue would
# start deleting real cancellations. Same pattern as payment_model's per-reservation sets.
#
#   HA-LILLEKS  elektra_1212 2026-07 -- a $1,848.27 booking cancelled with $1,794.07
#               refunded; VRBO retained $54.20. QBO bills $0.00 of commission, channel fee
#               and Stripe fee on it, i.e. QuickBooks agrees the owner earned nothing.
#   EXP-2524018032-6QA31  osbr_11 2026-08 -- cancelled, but the Guesty pull still reports it
#               `confirmed`, so the model priced a full $81.46 stay. The status check below
#               would never catch it; only the code list can. Owner instruction 2026-09-04:
#               remove all of its income from the statement. NOTE QuickBooks still bills
#               $15.81 of commission on it, i.e. QBO has not been told either -- worth
#               reversing there so the two records agree.
DROPPED_BOOKING_CODES = {"HA-LILLEKS", "EXP-2524018032-6QA31"}


# --- Owner-directed non-commissioned bookings -------------------------------------
# Bookings that STAY in the statement as revenue but carry NO management commission.
# Distinct from DROPPED_BOOKING_CODES: the money is still the owner's, we simply do not
# charge on it. One definition, read by BOTH sides so they cannot disagree —
# `netrevenue.engine` leaves the line out of the PM-fee sum (and out of the commission
# base) while keeping it in gross revenue, and `net_revenue_rows` renders the row's
# commission as $0.00.
#
#   HMRN4KYMAB  kirkland_13070 2026-07, Airbnb, 1 night, $76.05 — owner instruction.
#               QuickBooks bills no commission on it either (there is no
#               `Management Commission | HMRN4KYMAB` line at all), so this matches QBO.
NON_COMMISSIONED_BOOKING_CODES = {"HMRN4KYMAB"}


def is_dropped_cancellation(status, invoice_item, code=None):
    """True when this payment-breakdown row is a cancellation residue to be dropped.

    Either an owner-listed code (``DROPPED_BOOKING_CODES``) or a canceled booking whose
    InvoiceItem is at or below ``DROPPED_CANCELLATION_MAX_INVOICE``.
    """
    if code is not None and str(code) in DROPPED_BOOKING_CODES:
        return True
    if str(status or "").strip().lower() != "canceled":
        return False
    try:
        return float(invoice_item or 0.0) <= DROPPED_CANCELLATION_MAX_INVOICE
    except (TypeError, ValueError):
        return False


def dropped_cancellation_codes(pb_df):
    """Confirmation codes in `pb_df` that ``is_dropped_cancellation`` rejects.

    The ONE definition, shared by Section 1 (``build_by_unit`` filters on it) and the
    pipeline (``run_month_close`` suppresses the matching ledger rows), so the two can
    never disagree about which bookings exist.
    """
    if pb_df is None or not len(pb_df) or "status" not in pb_df.columns:
        return set()
    invoice = pb_df["InvoiceItem"] if "InvoiceItem" in pb_df.columns else None
    return {str(r["confirmationCode"]) for _, r in pb_df.iterrows()
            if is_dropped_cancellation(r.get("status"),
                                       r.get("InvoiceItem") if invoice is not None else 0.0,
                                       r.get("confirmationCode"))}


def natural_key(pid):
    """Sort key so rollup members render in natural listing order rather than
    lexicographically: 'osbr_2' before 'osbr_10', and a non-numeric suffix
    ('osbr_rv', 'seattle_10057_lower') sorts after the numbered units of the same
    prefix. Shared by the booking-breakdown and Net-Revenue (excel_writer) sections
    so the two stay in sync. Splits a trailing integer: 'osbr_10' -> ('osbr_', 0, 10);
    'osbr_rv' -> ('osbr_rv', 1, 0)."""
    s = str(pid)
    m = re.match(r"^(.*?)(\d+)$", s)
    if m:
        return (m.group(1), 0, int(m.group(2)))
    return (s, 1, 0)


def guesty_row(b, net_override=None) -> dict:
    """One display row from a raw Guesty payment-breakdown record (dict/Series):
    fold the 3 fees into 'Fees' (components kept hidden for the dashboard tooltip).

    `net_override` is the QBO-adjusted net for a cancelled/refunded booking QuickBooks
    re-cut at month end (reporting.qbo_adjustments) — the owner's authoritative figure.
    The row then shows **Guest Pay as the QBO-recognized gross** (net + the fees/cleaning/
    tax Guesty reported) rather than the raw amount the guest paid, and leaves the fee
    components untouched. That keeps the waterfall footing in EVERY renderer — the Excel
    writer and the dashboard break the fees out into Channel/Guesty/Stripe columns, so
    absorbing the difference into the folded `Fees` value alone would leave those two
    tables not adding up — without inventing a fee that QBO never charged. For most
    adjusted bookings QBO records nothing but the commission Bill, so the split behind the
    adjustment is simply not knowable; only its total is.
    """
    cf, gf, sf = float(b["channel_fee"]), float(b["guesty_fee"]), float(b["stripe_fee"])
    # ARC the cohost keeps as a handling fee — a fee, not owner revenue. Absent from a
    # CSV pulled before 2026-09-03, so default to 0.00 rather than blowing up.
    try:
        ar = float(b["arc_fee"])
    except (KeyError, IndexError, TypeError, ValueError):
        ar = 0.0
    if ar != ar:
        ar = 0.0
    net = round(float(b["net_revenue"]), 2)
    fees = round(cf + gf + sf + ar, 2)
    guest_pay = round(float(b["guest_pay"]), 2)
    cleaning = round(float(b["cleaning_fee"]), 2)
    tax = round(float(b["tax"]), 2)
    if net_override is not None:
        net = round(float(net_override), 2)
        guest_pay = round(net + fees + cleaning + tax, 2)

    # Booking-amount components. None (not 0.0) when the stored CSV predates them, so a
    # renderer shows an empty cell instead of asserting this booking had no room rent.
    def _text(key):
        try:
            v = b[key]
        except (KeyError, IndexError, TypeError):
            return ""
        return "" if v is None or v != v else str(v)

    def _comp(key):
        try:
            v = b[key]
        except (KeyError, IndexError, TypeError):
            return None
        try:
            v = float(v)
        except (TypeError, ValueError):
            return None
        return None if v != v else round(v, 2)          # NaN -> None

    return {
        "Conf Code": b["confirmationCode"],
        "Bookings": b["channel"],
        "Guest Pay": guest_pay,
        "Accommodation": _comp("accommodation"),
        "Markup": _comp("markup"),
        "Add-ons": _comp("addons"),
        # What the add-ons ARE ("Pet fee 50.00; Parking 25.00"); renderers show it
        # beside the figure. Empty when the booking has none.
        "_addon_detail": _text("addons_detail"),
        # Channel commission rate, for a later-added add-on merged in below.
        "_cf_rate": _comp("channel_fee_rate") or 0.0,
        "Fees": fees,
        "Cleaning Fee": cleaning,
        "Tax": tax,
        "Net Rental Revenue": net,
        "_ch": round(cf, 2), "_gu": round(gf, 2), "_st": round(sf, 2),
        "_ar": round(ar, 2), "_total": False,
        "_parts": [net],
        "_guest": b.get("guest") or "", "_in": str(b.get("checkIn") or "")[:10],
        "_out": str(b.get("checkOut") or "")[:10],
    }


def _pick(rec, *keys, default=None):
    """First present key — lets ltr_row accept BOTH the raw ltr.records shape
    (property_id/net_revenue/cleaning_fee/booking_id/is_ltr, used by the Excel build)
    and the dashboard's display shape (_pid/_net/_cleaning/_code/_is_ltr)."""
    for k in keys:
        if k in rec and rec[k] is not None:
            return rec[k]
    return default


def ltr_pid(rec):
    return _pick(rec, "property_id", "_pid")


def ltr_row(rec) -> dict:
    """One breakdown row for an LTR/deferred booking (fee-free waterfall): Guest Pay
    (Total Payout) = accommodation fare + cleaning; no channel/Guesty/Stripe fee, no
    tax; cleaning is non-commissioned so Net Rental Revenue = accommodation fare."""
    rent = round(float(_pick(rec, "net_revenue", "_net", default=0.0)), 2)
    clean = round(float(_pick(rec, "cleaning_fee", "_cleaning", default=0.0)), 2)
    return {
        "Conf Code": _pick(rec, "booking_id", "_code", default=""),
        "Bookings": "LTR" if _pick(rec, "is_ltr", "_is_ltr", default=True) else "Deferred",
        "Guest Pay": round(rent + clean, 2),
        # An LTR booking IS room rent: no markup and no add-ons, so the split is exact.
        "Accommodation": rent, "Markup": 0.0, "Add-ons": 0.0, "_addon_detail": "",
        "Fees": 0.0,
        "Cleaning Fee": clean,
        "Tax": 0.0,
        "Net Rental Revenue": rent,
        "_ch": 0.0, "_gu": 0.0, "_st": 0.0, "_ar": 0.0, "_total": False,
        "_parts": [rent],
        "_guest": _pick(rec, "guest_name", "_guest", default="") or "",
        "_in": str(_pick(rec, "checkin", "_in", default="") or "")[:10],
        "_out": str(_pick(rec, "checkout", "_out", default="") or "")[:10],
    }


def other_row(rec) -> dict:
    """One breakdown row for rental income with no payment-breakdown row — synthetic
    Guesty income (Hipcamp/OsbrRV) or QBO-recorded rent (Lease Rent, tenant Zelle,
    extra night). Fee-free, exactly like ltr_row: the fee waterfall is unknown for
    these, so Guest Pay == Net Rental Revenue and every fee/tax cell is 0.
    See reporting.other_income for which ledger lines qualify."""
    amt = round(float(rec.get("amount", 0.0)), 2)
    return {
        "Conf Code": rec.get("code") or "",
        "Bookings": rec.get("label") or "Direct",
        "Guest Pay": amt,
        # The waterfall is unknown for these, so the split is too — blank, not 0.00.
        "Accommodation": None, "Markup": None, "Add-ons": None, "_addon_detail": "",
        "Fees": 0.0,
        "Cleaning Fee": 0.0,
        "Tax": 0.0,
        "Net Rental Revenue": amt,
        "_ch": 0.0, "_gu": 0.0, "_st": 0.0, "_ar": 0.0, "_total": False,
        # No stay dates on a QBO-recorded credit — the posting date stands in for both,
        # which the renderers show as a single date rather than a range.
        "_parts": [amt],
        "_guest": rec.get("guest") or "", "_in": str(rec.get("date") or "")[:10],
        "_out": str(rec.get("date") or "")[:10],
    }


def _merge_addon(rows, rec) -> bool:
    """Fold `rec` into the existing row for the same confirmation code, if there is one.

    A pet fee is booked in QBO against a booking that already has a full breakdown row
    ("Cottage 4 pet fee" -> EXP-DL35Nk4o). Owner decision 2026-08: it belongs IN that
    booking's Net Rental Revenue, not on a line of its own. Anything whose code doesn't
    match an existing row falls through and becomes its own fee-free row.
    Returns True when it was merged."""
    code = str(rec.get("code") or "")
    if not code:
        return False
    amt = round(float(rec.get("amount", 0.0)), 2)
    for r in rows:
        if not r["_total"] and str(r["Conf Code"]) == code:
            # The channel commissions a later-added fee too (owner, 2026-09-03): a pet fee
            # billed after checkout is still guest money on that booking, so the channel's
            # rate comes out of it before it reaches the owner. Charged on the add-on's own
            # gross — the deposit IS what the guest paid — and folded into the same
            # Fees/_ch cells, so the row keeps footing:
            #     Guest Pay += amt   Add-ons += amt   Fees += cf   Net += amt - cf
            gross = round(float(rec.get("gross", amt)), 2)
            cf = round(gross - amt, 2)          # what the ledger already deducted
            r["Guest Pay"] = round(r["Guest Pay"] + gross, 2)
            r["Net Rental Revenue"] = round(r["Net Rental Revenue"] + amt, 2)
            if cf:
                r["Fees"] = round(r["Fees"] + cf, 2)
                r["_ch"] = round(r["_ch"] + cf, 2)
            # A merged pet/parking fee is by definition an add-on, so it belongs in that
            # column too — otherwise Guest Pay grows and the split stops footing.
            if r.get("Add-ons") is not None:
                r["Add-ons"] = round(r["Add-ons"] + gross, 2)
            # Name it, so the statement says WHAT the extra $50 was.
            lbl = str(rec.get("detail") or "").strip()
            if lbl:
                r["_addon_detail"] = "; ".join(
                    x for x in [r.get("_addon_detail", ""), f"{lbl} {gross:,.2f}"] if x)
            # The add-on is its OWN ledger line, and netrevenue.engine rounds commission
            # per line. Keep the parts so net_revenue_rows rounds identically instead of
            # once on the merged total — otherwise the section drifts a penny from the
            # stored PM fee (elektra_1413 2026-07: 88.74 + 12.25 = 100.99, not 101.00).
            r.setdefault("_parts", []).append(amt)
            return True
    return False


def net_revenue_rows(by_unit, pm_fee_rate, cleaning_by_code=None, tax_by_code=None):
    """Section 2 (Net Revenue) rows, derived from the SAME Section-1 rows.

    Owner decision 2026-08-29: **the Net Revenue section IS the Booking Breakdown with
    commission applied.** It used to be re-assembled from the guesty ledger rows plus the
    LTR CSV, which silently omitted booking source #3 (QBO-recorded rental income and
    Hipcamp/OsbrRV) — so a statement could show a Net Revenue section that did not add up
    to the Net Rental Revenue the payout was computed on. `kirkland_8017` 2026-06 listed
    4 bookings totalling $8,834.75 under a $11,910.35 payout, the $6,115.00 `GY-cN8BWajQ`
    booking appearing only in Section 1. Deriving both sections from one row set makes
    that impossible.

    `cleaning_by_code` / `tax_by_code` stay as caller-supplied maps rather than being read
    off the row's own Cleaning Fee / Tax cells: those two columns show what the GUEST paid,
    while the owner-facing columns are the amounts credited to the owner, which the ledger
    (`_apply_owner_cleaning_credit`, `%Taxes Paid to Owners%`) is authoritative for. Taking
    them from the row would silently change `amount_due` reconciliation.

    `pm_fee_rate` is EITHER a float (one rate for the whole period, which is every
    property but one) OR a callable ``rate_at(checkin) -> float`` from
    `scope.pm_rate.pm_rate_resolver`, for a rate that changes mid-period —
    `seattle_9021` is 17.1% to 2026-08-10 and 22% after. `netrevenue.engine` resolves the
    stored PM fee the same way off each ledger line's `posting_date`, which for a guesty
    booking IS its check-in, so the displayed commissions still sum to the stored fee.

    Returns {property_id: [record, ...]} — one record per booking, no TOTAL row (each
    renderer totals it its own way).
    """
    clean_map, tax_map = cleaning_by_code or {}, tax_by_code or {}
    if callable(pm_fee_rate):
        rate_at = lambda d: float(pm_fee_rate(d) or 0.0)
    else:
        _r = float(pm_fee_rate or 0.0)
        rate_at = lambda _d: _r
    out = {}
    for pid, rows in by_unit.items():
        recs = []
        for r in rows:
            if r.get("_total"):
                continue
            net = round(float(r["Net Rental Revenue"]), 2)
            code = str(r["Conf Code"])
            clean = round(float(clean_map.get(code, 0.0) or 0.0), 2)
            tax = round(float(tax_map.get(code, 0.0) or 0.0), 2)
            # Round the commission PER LEDGER LINE, exactly as netrevenue.engine sums it,
            # or the section total drifts a penny from the stored PM fee. `_parts` is the
            # one-or-more lines behind this row (a booking plus any merged add-on).
            parts = r.get("_parts") or [net]
            # Owner-directed non-commissioned bookings keep their revenue but are charged
            # nothing (NON_COMMISSIONED_BOOKING_CODES). netrevenue.engine excludes the same
            # codes from the stored PM fee, so the displayed commissions still sum to it.
            rate = rate_at(r.get("_in") or "")
            comm = (0.0 if code in NON_COMMISSIONED_BOOKING_CODES
                    else round(sum(round(-float(x) * rate, 2) for x in parts), 2))
            recs.append({
                "property_id": pid,
                "booking_id": code,
                "guest_name": r.get("_guest") or "",
                "checkin": r.get("_in") or "",
                "checkout": r.get("_out") or "",
                "channel": r.get("Bookings") or "",
                "net_revenue": net,
                "owner_cleaning_cost": clean,
                "owner_tax_cost": tax,
                "commission": comm,
                "owner_proceeds": round(net + clean + tax + comm, 2),
            })
        out[pid] = recs
    return out


def _total_row(rows, label_count=None) -> dict:
    # A component is None on rows whose split is unknown (source #3, or a period pulled
    # before the columns existed). Skip those rather than coerce to 0: the total then
    # says "the bookings we CAN split add to this", and a column that is unknown for
    # every row in the unit totals to None so the renderer leaves it blank.
    def _sum(c):
        vals = [r.get(c) for r in rows]
        vals = [v for v in vals if v is not None]
        if c in COMPONENTS and not vals:
            return None
        return round(sum(vals), 2)

    t = {c: _sum(c) for c in NUM + FEE_PARTS}
    n = label_count if label_count is not None else len(rows)
    t.update({"Conf Code": "", "Bookings": f"{n} booking(s)", "_total": True,
              "_guest": "", "_in": "", "_out": "", "_parts": []})
    return t


def build_by_unit(pb_df, ltr_records, member_pids, claimed_codes=None, other_records=None,
                  net_overrides=None):
    """Return (by_unit, grand).

    by_unit: {property_id: [detail rows..., TOTAL row]} for member pids that have any
      booking. Rows come from ALL THREE sources — the Guesty payment breakdown (fee
      waterfall), LTR/deferred bookings (fee-free), and `other_records` (fee-free
      rental income with no breakdown row: Hipcamp/OsbrRV, QBO-recorded rent, pet
      fees) — so a rollup's units interleave.
    grand: the across-units TOTAL row (dict), or None (only built for multi-unit rollups).

    `claimed_codes` (from ltr.records.ltr_claimed_codes) are confirmation codes the LTR
    source superseded — their stale Guesty breakdown rows are dropped (dedup after the
    Guesty pull), so Section 1 foots with the Net Revenue section.

    `net_overrides` ({code: net}) come from reporting.qbo_adjustments.adjusted_nets — the
    QBO-adjusted net for a cancelled/refunded booking QuickBooks re-cut at month end. The
    build applies the same map to the ledger, so Section 1 and Section 2 move together.

    `other_records` come from reporting.other_income.build_records. They are applied
    LAST so a pet fee can attach to the booking row it belongs to (_merge_addon);
    whatever doesn't match an existing code becomes its own fee-free row. Including
    them is what makes Section 1's Net Rental Revenue equal Section 2's.
    """
    members = set(member_pids)
    claimed = {str(c) for c in (claimed_codes or ())}
    overrides = {str(k): v for k, v in (net_overrides or {}).items()}

    unit_rows = {}
    if pb_df is not None and len(pb_df):
        seg = pb_df[pb_df["property_id"].isin(members)].copy()
        if claimed:
            seg = seg[~seg["confirmationCode"].astype(str).isin(claimed)]
        # Cancellation residues leave the statement entirely (see
        # DROPPED_CANCELLATION_MAX_INVOICE); their ledger rows are suppressed in the
        # build, so dropping them here keeps Section 1 footing with Section 2.
        dropped = dropped_cancellation_codes(seg)
        if dropped:
            seg = seg[~seg["confirmationCode"].astype(str).isin(dropped)]
        for pid in sorted(seg["property_id"].unique(), key=natural_key):
            u = seg[seg["property_id"] == pid].sort_values(["checkIn", "confirmationCode"])
            unit_rows.setdefault(pid, []).extend(
                guesty_row(b, overrides.get(str(b["confirmationCode"])))
                for _, b in u.iterrows())
    for rec in ltr_records:
        pid = ltr_pid(rec)
        if pid in members:
            unit_rows.setdefault(pid, []).append(ltr_row(rec))
    for rec in (other_records or ()):
        pid = rec.get("property_id")
        if pid not in members:
            continue
        if str(rec.get("code") or "") in claimed:
            continue
        # Attach to the booking it belongs to when the code matches; else stand alone.
        if not _merge_addon(unit_rows.get(pid, []), rec):
            unit_rows.setdefault(pid, []).append(other_row(rec))

    by_unit, g_acc, g_n = {}, {c: None for c in NUM + FEE_PARTS}, 0
    for pid in sorted(unit_rows, key=natural_key):
        rows = unit_rows[pid]
        by_unit[pid] = rows + [_total_row(rows)]
        for c in NUM + FEE_PARTS:
            v = by_unit[pid][-1][c]
            if v is not None:                      # keep None until some unit contributes
                g_acc[c] = (g_acc[c] or 0.0) + v
        g_n += len(rows)

    grand = None
    if len(unit_rows) > 1:
        grand = {c: (None if g_acc[c] is None else round(g_acc[c], 2))
                 for c in NUM + FEE_PARTS}
        grand.update({"Conf Code": "", "Bookings": f"{g_n} booking(s)", "_total": True,
                      "_guest": "", "_in": "", "_out": "", "_parts": []})
    return by_unit, grand
