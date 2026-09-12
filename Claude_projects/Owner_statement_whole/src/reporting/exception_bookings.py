"""Exception-booking register — every booking that is NOT a clean, fully-paid stay.

    python -m src.reporting.exception_bookings --period 2026-06 2026-07
    python -m src.reporting.exception_bookings --period 2026-07 --no-qbo

Writes ``output/exception_bookings.{csv,xlsx}``: one row per booking carrying any of

* **REFUND**            the guest was refunded (``TOTAL REFUNDED`` > 0), split into
                        FULL REFUND (the owner kept nothing) and PARTIAL REFUND.
* **RESOLUTION**        an Airbnb Resolution Center amount (the payment breakdown's
                        ``refund`` column, which is ARC — display only, NOT the refund).
* **QBO ADJUSTMENT**    QuickBooks re-cut the commission: a ``Management Commission |
                        Cancelled | <code>`` bill (the re-cut charge, owner rule
                        2026-08-28 — see reporting.qbo_adjustments) or a
                        ``Management Commission | Refund | <code>`` bill (a credit back).
                        A ``… Deduction |`` line does NOT count: crediting the commission
                        on the channel/Stripe fee is routine on every VRBO/Expedia stay,
                        not an adjustment. It is reported in ``QBO Comm Credited`` anyway.
* **CANCELED W/ INCOME** ``status == 'canceled'`` yet the owner still earned money.
* **DEFERRED**          the Guesty row was superseded by an LTR/DEFERRED ledger row (a long
                        stay recognized month by month). QuickBooks commissions the whole
                        stay in the check-in month, so its commission will not match ours
                        in any single month — that is a timing difference, not a gap.
* **NO LEDGER ROW**     the booking is in the payment breakdown but has no income row at
                        all — genuinely missing revenue; investigate.
* **DROPPED**           a cancellation residue removed from the statement entirely
                        (``booking_breakdown.is_dropped_cancellation``).

WHY THE QBO COLUMNS COME FROM A LIVE PULL: ``qbo_sync`` used to store every Bill line as
``-abs(amount)``, which collapsed a commission CREDIT (positive in QuickBooks) into a
CHARGE (negative). That is fixed — the sync now keeps QuickBooks' own sign — but only
periods re-synced SINCE the fix carry it, so reading ``ledger_lines`` would silently
mis-read older months. The read-only Bill query below is correct for every period
regardless. Once the whole history has been re-synced this can read the ledger instead.
``--no-qbo`` skips the pull and leaves those columns blank.
"""
import argparse
import calendar
import collections
import sqlite3

import pandas as pd

from .. import paths
from ..common.config import load_config
from ..scope.pm_rate import resolve_pm_fee_rate
from .booking_breakdown import is_dropped_cancellation

COMMISSION_ACCOUNT = "Management Commissions Revenue"
# Descriptions are "<label> | <marker>? | <code> | <dates> | <n> nights".
_MARKERS = {"cancelled", "refund", "adjustment", "adjusted"}


def _period_bounds(period):
    y, m = map(int, period.split("-"))
    return f"{period}-01", f"{period}-{calendar.monthrange(y, m)[1]:02d}"


def _f(v, default=0.0):
    try:
        x = float(v)
    except (TypeError, ValueError):
        return default
    return default if x != x else x           # NaN -> default


def _code_and_kind(description):
    """(confirmation code, kind) out of a commission line description."""
    parts = [p.strip() for p in str(description or "").split("|")]
    if len(parts) < 2:
        return "", ""
    if parts[0].endswith("Deduction"):        # fee-basis credit, e.g. "Stripe fee Deduction"
        return parts[1], "deduction"
    if parts[1].lower() in _MARKERS:
        return (parts[2] if len(parts) > 2 else ""), parts[1].lower()
    return parts[1], "plain"


def qbo_commission(period):
    """{code: {charged, credited, net, kinds}} with QuickBooks' TRUE signs.

    On the commission account QBO writes a charge as a NEGATIVE line and a credit
    (money given back to the owner) as a POSITIVE one.
    """
    from ..expense.qbo_client import QBOClient
    q = load_config(str(paths.CONFIG_YML))["qbo"]
    client = QBOClient(realm_id=q["realm_id"], base_url=q["base_url"],
                       minorversion=int(q.get("minorversion", 75)))
    start, end = _period_bounds(period)

    out = collections.defaultdict(lambda: {"charged": 0.0, "credited": 0.0, "kinds": set()})
    pos = 1
    while True:
        res = client.query(
            f"SELECT * FROM Bill WHERE TxnDate >= '{start}' AND TxnDate <= '{end}'",
            start_position=pos, max_results=500)
        bills = res.get("QueryResponse", {}).get("Bill", []) or []
        if not bills:
            break
        for b in bills:
            for ln in b.get("Line", []) or []:
                d = ln.get("AccountBasedExpenseLineDetail") or {}
                if (d.get("AccountRef") or {}).get("name") != COMMISSION_ACCOUNT:
                    continue
                code, kind = _code_and_kind(ln.get("Description"))
                if not code:
                    continue
                amt = _f(ln.get("Amount"))
                rec = out[code]
                if amt < 0:
                    rec["charged"] += -amt
                else:
                    rec["credited"] += amt
                if kind and kind not in ("plain", "deduction"):
                    rec["kinds"].add(kind)      # deduction is routine, not an adjustment
        pos += len(bills)
        if len(bills) < 500:
            break
    return out


def _deferred_codes(conn, period):
    """Codes whose Guesty row was replaced by an LTR/DEFERRED row this period.

    ``import_ltr._replace_colliding`` DELETES the guesty income row a DEFERRED row
    supersedes, so the booking looks absent. The confirmation code survives in the QBO
    customer string ("<channel> - <guest> - <code>").
    """
    start, end = _period_bounds(period)
    props = {p for (p,) in conn.execute(
        """SELECT DISTINCT property_id FROM ledger_lines
             WHERE source_object IN ('LTR','DEFERRED') AND category='INCOME'
               AND posting_date BETWEEN ? AND ?""", (start, end))}
    if not props:
        return set()
    out = set()
    for pid, vc in conn.execute(
            """SELECT property_id, vendor_customer FROM ledger_lines
                 WHERE vendor_customer LIKE '% - %' AND posting_date BETWEEN ? AND ?""",
            (start, end)):
        if pid in props and vc:
            out.add((pid, str(vc).rsplit(" - ", 1)[-1].strip()))
    return out


def _ledger_nets(conn, period):
    """{code: (net in the statement, include_in_statement)} for the period's guesty income."""
    start, end = _period_bounds(period)
    return {str(c): (round(_f(a), 2), int(inc)) for c, a, inc in conn.execute(
        """SELECT source_txn_id, amount, include_in_statement FROM ledger_lines
             WHERE source='guesty' AND category='INCOME' AND source_txn_id IS NOT NULL
               AND posting_date BETWEEN ? AND ?""", (start, end))}


COLUMNS = ["Period", "Flags", "Property ID", "Listing", "Conf Code", "Channel", "Guest",
           "Check-in", "Check-out", "Status", "Invoice Amount", "Guest Pay", "Cleaning Fee",
           "Tax", "Channel Fee", "Guesty Fee", "Stripe Fee", "Total Refunded", "Resolution",
           "Net (breakdown)", "Net (statement)", "In Statement", "PM Rate", "Our Commission",
           "QBO Comm Charged", "QBO Comm Credited", "QBO Comm Net", "Comm Diff"]

# Flags whose commission cannot line up with QuickBooks within a single month.
def build(conn, periods, with_qbo=True):
    rows, rates = [], {}
    for period in periods:
        try:
            pb = pd.read_csv(paths.payment_breakdown_csv(period))
        except FileNotFoundError:
            print(f"  {period}: no payment_breakdown CSV — skipped")
            continue
        # TOTAL REFUNDED lives only in the converted export; the breakdown's `refund`
        # column is the Airbnb Resolution Center amount (display only).
        refunded = {}
        conv_path = paths.inputs_dir(period) / "guesty_converted.csv"
        if conv_path.exists():
            conv = pd.read_csv(conv_path)
            refunded = {str(r["booking_id"]): _f(r.get("refund"))
                        for _, r in conv.iterrows()}

        nets = _ledger_nets(conn, period)
        deferred = _deferred_codes(conn, period)
        comm = qbo_commission(period) if with_qbo else {}
        start = f"{period}-01"

        for _, b in pb.iterrows():
            code = str(b["confirmationCode"])
            status = str(b.get("status") or "")
            invoice = _f(b.get("InvoiceItem"))
            net_bd = round(_f(b.get("net_revenue")), 2)
            refund = round(refunded.get(code, 0.0), 2)
            arc = round(_f(b.get("refund")), 2)
            net_st, included = nets.get(code, (None, None))
            c = comm.get(code)

            flags = []
            if refund > 0.005:
                flags.append("FULL REFUND" if net_bd <= 0.005 else "PARTIAL REFUND")
            if abs(arc) > 0.005:
                flags.append("RESOLUTION")
            if c and c["kinds"]:
                flags.append("QBO " + "/".join(sorted(c["kinds"])).upper())
            if status == "canceled":
                flags.append("DROPPED" if is_dropped_cancellation(status, invoice, code)
                             else ("CANCELED W/ INCOME" if net_bd > 0.005 else "CANCELED"))
            if net_st is None:
                flags.append("DEFERRED" if (str(b["property_id"]), code) in deferred
                             else "NO LEDGER ROW")
            if not flags:
                continue

            pid = str(b["property_id"])
            if pid not in rates:
                rates[pid] = resolve_pm_fee_rate(conn, pid, start)
            rate = rates[pid]
            # What we ACTUALLY charge: nothing on a row kept out of the statement
            # (a dropped cancellation residue) or one with no ledger row at all.
            if net_st is None or not included:
                ours = 0.0
            else:
                ours = round(net_st * float(rate), 2) if rate else None
            qnet = round(c["charged"] - c["credited"], 2) if c else None
            rows.append([
                period, " + ".join(flags), pid, b.get("listing"), code, b.get("channel"),
                b.get("guest"), b.get("checkIn"), b.get("checkOut"), status,
                round(invoice, 2), round(_f(b.get("guest_pay")), 2),
                round(_f(b.get("cleaning_fee")), 2), round(_f(b.get("tax")), 2),
                round(_f(b.get("channel_fee")), 2), round(_f(b.get("guesty_fee")), 2),
                round(_f(b.get("stripe_fee")), 2), refund, arc, net_bd, net_st,
                "" if included is None else ("Y" if included else "N"),
                rate, ours,
                round(c["charged"], 2) if c else None,
                round(c["credited"], 2) if c else None, qnet,
                round(ours - qnet, 2) if (ours is not None and qnet is not None) else None,
            ])

    return pd.DataFrame(rows, columns=COLUMNS).sort_values(
        ["Period", "Property ID", "Conf Code"]).reset_index(drop=True)


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--period", nargs="+", required=True, help="YYYY-MM …")
    ap.add_argument("--no-qbo", action="store_true",
                    help="skip the read-only QuickBooks pull (leaves the QBO columns blank)")
    args = ap.parse_args()

    conn = sqlite3.connect(str(paths.DB_PATH))
    df = build(conn, args.period, with_qbo=not args.no_qbo)

    out_csv = paths.OUTPUT_DIR / "exception_bookings.csv"
    out_xlsx = paths.OUTPUT_DIR / "exception_bookings.xlsx"
    df.to_csv(out_csv, index=False)
    counts = (df["Flags"].str.split(" + ", regex=False).explode().value_counts()
              .rename_axis("Flag").reset_index(name="Bookings"))
    with pd.ExcelWriter(out_xlsx, engine="openpyxl") as xl:
        df.to_excel(xl, sheet_name="Exception bookings", index=False)
        counts.to_excel(xl, sheet_name="Summary", index=False)

    print(f"\n{len(df)} exception booking(s) across {len(args.period)} period(s)")
    print(f"  -> {out_csv}\n  -> {out_xlsx}\n")
    print(counts.to_string(index=False))


if __name__ == "__main__":
    main()
