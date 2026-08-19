"""Shared Booking-Breakdown (Section 1) builder.

The per-unit fee waterfall shown ABOVE each unit's Net Revenue table. Built ONCE
here so the dashboard, the Excel writer, and the PDF render identical tables and can
never drift. Pure/data-only: a payment-breakdown DataFrame + LTR display records in,
renderer-agnostic list-of-dict rows out. NO db / paths / streamlit imports, so it
imports the same way in the flat dashboard, the package pipeline, and the deploy bundle.

Row schema (one dict per booking, plus a TOTAL row per unit):
  Conf Code, Bookings (channel; TOTAL cell shows the booking count), Guest Pay, Fees
  (channel+Guesty+Stripe folded; components kept as hidden _ch/_gu/_st for the dashboard
  hover), Cleaning Fee, Tax, Net Rental Revenue, and a hidden _total flag.
"""

# Displayed columns in owner-set order; NUM = the right-aligned dollar columns.
DISP = ["Conf Code", "Bookings", "Guest Pay", "Fees", "Cleaning Fee", "Tax", "Net Rental Revenue"]
NUM = ["Guest Pay", "Fees", "Cleaning Fee", "Tax", "Net Rental Revenue"]
FEE_PARTS = ["_ch", "_gu", "_st"]


def guesty_row(b) -> dict:
    """One display row from a raw Guesty payment-breakdown record (dict/Series):
    fold the 3 fees into 'Fees' (components kept hidden for the dashboard tooltip)."""
    cf, gf, sf = float(b["channel_fee"]), float(b["guesty_fee"]), float(b["stripe_fee"])
    return {
        "Conf Code": b["confirmationCode"],
        "Bookings": b["channel"],
        "Guest Pay": round(float(b["guest_pay"]), 2),
        "Fees": round(cf + gf + sf, 2),
        "Cleaning Fee": round(float(b["cleaning_fee"]), 2),
        "Tax": round(float(b["tax"]), 2),
        "Net Rental Revenue": round(float(b["net_revenue"]), 2),
        "_ch": round(cf, 2), "_gu": round(gf, 2), "_st": round(sf, 2), "_total": False,
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
        "Fees": 0.0,
        "Cleaning Fee": clean,
        "Tax": 0.0,
        "Net Rental Revenue": rent,
        "_ch": 0.0, "_gu": 0.0, "_st": 0.0, "_total": False,
    }


def _total_row(rows, label_count=None) -> dict:
    t = {c: round(sum(r[c] for r in rows), 2) for c in NUM + FEE_PARTS}
    n = label_count if label_count is not None else len(rows)
    t.update({"Conf Code": "", "Bookings": f"{n} booking(s)", "_total": True})
    return t


def build_by_unit(pb_df, ltr_records, member_pids, claimed_codes=None):
    """Return (by_unit, grand).

    by_unit: {property_id: [detail rows..., TOTAL row]} for member pids that have any
      booking. Rows come from BOTH sources — the Guesty payment breakdown (fee waterfall)
      and LTR/deferred bookings (fee-free) — so a rollup's units interleave.
    grand: the across-units TOTAL row (dict), or None (only built for multi-unit rollups).

    `claimed_codes` (from ltr.records.ltr_claimed_codes) are confirmation codes the LTR
    source superseded — their stale Guesty breakdown rows are dropped (dedup after the
    Guesty pull), so Section 1 foots with the Net Revenue section.
    """
    members = set(member_pids)
    claimed = {str(c) for c in (claimed_codes or ())}

    unit_rows = {}
    if pb_df is not None and len(pb_df):
        seg = pb_df[pb_df["property_id"].isin(members)].copy()
        if claimed:
            seg = seg[~seg["confirmationCode"].astype(str).isin(claimed)]
        for pid in sorted(seg["property_id"].unique()):
            u = seg[seg["property_id"] == pid].sort_values(["checkIn", "confirmationCode"])
            unit_rows.setdefault(pid, []).extend(guesty_row(b) for _, b in u.iterrows())
    for rec in ltr_records:
        pid = ltr_pid(rec)
        if pid in members:
            unit_rows.setdefault(pid, []).append(ltr_row(rec))

    by_unit, g_acc, g_n = {}, {c: 0.0 for c in NUM + FEE_PARTS}, 0
    for pid in sorted(unit_rows):
        rows = unit_rows[pid]
        by_unit[pid] = rows + [_total_row(rows)]
        for c in NUM + FEE_PARTS:
            g_acc[c] += by_unit[pid][-1][c]
        g_n += len(rows)

    grand = None
    if len(unit_rows) > 1:
        grand = {c: round(g_acc[c], 2) for c in NUM + FEE_PARTS}
        grand.update({"Conf Code": "", "Bookings": f"{g_n} booking(s)", "_total": True})
    return by_unit, grand
