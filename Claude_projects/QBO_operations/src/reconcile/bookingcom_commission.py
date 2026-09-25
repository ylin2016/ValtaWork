"""Three-way reconciliation of Booking.com commission. READ-ONLY -- writes nothing.

Booking.com commission touches the books three times, and all three should agree:

    billed to the owner   9Z5X_<reservation> Bill, DEBIT 1A - Net Earnings:Channel Fees
                          (a $0.00 rebill: the owner is charged, Valta books the matching
                          Billable Expense Income - Channel Fee)
    invoiced by Booking   the monthly commission invoice, ONE ROW PER PROPERTY
    paid in cash          Purchase line, DEBIT Fee - Processing & Commission:
                          Fee - Booking.com Commission, out of Chase Trust 9967

Valta nets to zero on the pass-through, so a gap is real money: a commission charged to an
owner that Booking.com never billed, or one Booking.com billed that no owner was charged.

    python -m src.reconcile.bookingcom_commission
    python -m src.reconcile.bookingcom_commission --period 2026-08 --invoice <path.xlsx>

**The invoice is per PROPERTY, not per reservation**, so this cannot be a per-reservation
match however much one would like it: Booking.com bills a month of commission for a whole
property on one line.  The per-reservation detail exists only on OUR side, in the
`9Z5X_<reservation>` DocNumbers, so the report gives the property-level comparison and then
lists the reservations behind each one.

Joining the two sides takes two hops, because the invoice names a Booking.com Property ID
and QuickBooks names a class:

    Property ID -> NICKNAME     from the payout CSVs under payment/
    NICKNAME    -> property_id  via ltr.labels.to_property_id, through bridge.py

That second hop is why `Cottage 3` on the invoice and `OSBR 3` in QuickBooks are the same
unit.  Never hardcode that map here -- it is the statement project's, and a copy goes stale
the first time a listing is renamed.
"""
from __future__ import annotations

import argparse
import calendar
import csv
import glob
import sys
from collections import defaultdict
from pathlib import Path

import openpyxl

from ..bridge import to_property_id
from ..config import client
from ..paths import PROJECT_ROOT, review_csv

CHANNEL_FEES = "1636"     # Trust Liabilities:Owner Payables:1A - Net Earnings:Channel Fees
BCOM_COGS = "1602"        # Fee - Processing & Commission:Fee - Booking.com Commission
BILL_PREFIX = "9Z5X_"     # the generator's prefix for Booking.com channel-fee bills

BCOM_DIR = PROJECT_ROOT / "inputs" / "JE" / "bookingcom"
BOOKING_IDS = PROJECT_ROOT / "config" / "booking_Id.csv"


def invoice_files() -> list[Path]:
    found = sorted((BCOM_DIR / "commission invoices").glob("*.xlsx"))
    if not found:
        sys.exit(f"no commission invoice found in {BCOM_DIR / 'commission invoices'}")
    return [f for f in found if not f.name.startswith("~$")]


def read_invoice(path: Path) -> tuple[list[dict], str]:
    """Rows and the STAY month they cover.

    Every `Invoice Type` is kept, not just `Commission`.  A file can also carry
    `Customer complaint costs`, and those are booked to the same COGS account -- Purchase
    114697 has one at $201.84 sitting beside a commission line.  Dropping them made the
    invoice look $296.70 smaller than the cash that paid it.

    The period comes from the invoice DATE, not the filename: Booking.com bills in arrears,
    so an invoice dated 2026-09-06 is August's commission.  Filenames are not a contract --
    the same file has arrived as `..._2026-08.xlsx`, `..._20260919.xlsx` and
    `20260815_Booking.xlsx`, naming the stay month, the due date and the issue date.
    """
    ws = openpyxl.load_workbook(path, data_only=True, read_only=True).worksheets[0]
    rows = list(ws.iter_rows(values_only=True))
    hdr = list(rows[0])
    out = []
    for r in rows[1:]:
        if not r or not r[0]:
            continue                       # trailing blank rows: one file claims 999
        d = dict(zip(hdr, r))
        out.append({"Invoice": str(d.get("Invoice") or "").strip(),
                    "PropertyId": str(int(float(d["ID"]))) if d.get("ID") else "",
                    "PropertyName": str(d.get("Property Name") or "").strip(),
                    "InvoiceDate": str(d.get("Invoice Date") or "")[:10],
                    "DueDate": str(d.get("Due Date") or "")[:10],
                    "Type": str(d.get("Invoice Type") or "").strip(),
                    "Amount": round(float(d.get("Amount") or 0), 2),
                    "Status": str(d.get("Status") or "").strip()})
    dates = sorted({r["InvoiceDate"] for r in out if r["InvoiceDate"]})
    return out, (prev_month(dates[0][:7]) if dates else "")


def prev_month(period: str) -> str:
    y, m = int(period[:4]), int(period[5:7])
    return f"{y - (m == 1)}-{12 if m == 1 else m - 1:02d}"


def property_nicknames() -> dict[str, list[str]]:
    """Booking.com Property.ID -> listing nickname, from `config/booking_Id.csv`.

    That file is the owner-maintained map and wins outright.  Deriving this from the payout
    CSVs was only ever a stand-in, and it is genuinely ambiguous: a nickname changes over
    time, so ID 7214982 appeared as both `Elektra 1108` and `Elektra 1203` and the
    reconciliation had to guess.  The map has one nickname per ID and covers every ID in the
    invoices and the payout files.

    A nickname legitimately has MORE THAN ONE id -- `Microsoft 14615-D303` has two, one per
    Booking.com listing of the same unit -- so the map is many ids to one listing, never the
    reverse.

    The payout files are still read, but only to report ids the config has not caught up
    with; they never override it.
    """
    m: dict[str, set] = defaultdict(set)
    if BOOKING_IDS.exists():
        with BOOKING_IDS.open(encoding="utf-8-sig") as fh:
            for r in csv.DictReader(fh):
                pid = (r.get("ID") or "").strip()
                nk = (r.get("NICKNAME") or "").strip()
                if pid and nk:
                    m[pid].add(nk)
    if m:
        return {k: sorted(v) for k, v in m.items()}
    for f in glob.glob(str(BCOM_DIR / "payment" / "*.csv")):
        with open(f, encoding="utf-8-sig") as fh:
            for r in csv.DictReader(fh):
                pid = (r.get("Property.ID") or "").strip().strip('"')
                nk = (r.get("NICKNAME") or "").strip().strip('"')
                if pid and nk:
                    m[pid].add(nk)
    print(f"  NOTE: {BOOKING_IDS} not found; falling back to the payout files, which give "
          f"ambiguous nicknames.")
    return {k: sorted(v) for k, v in m.items()}


def month_end(period: str) -> str:
    """Last day of the month. A hardcoded `-31` is rejected by QBO for a 30-day month."""
    y, m = int(period[:4]), int(period[5:7])
    return f"{period}-{calendar.monthrange(y, m)[1]:02d}"


def next_month(period: str) -> str:
    y, m = int(period[:4]), int(period[5:7])
    return f"{y + (m == 12)}-{(m % 12) + 1:02d}"


def qbo_sides(period: str, paid_period: str) -> tuple[dict, dict, dict]:
    """(billed_by_property, paid_by_property, reservations_by_property).

    The two sides sit in DIFFERENT months on purpose.  A 9Z5X_ bill is dated by the stay;
    Booking.com invoices that month's commission in arrears (2026-08 stays -> invoice dated
    2026-09-06, due 2026-09-19) and the cash leaves later still.  Comparing cash from the
    same month as the bills lines August's payment of JULY's commission up against August's
    bills, which tells you nothing.
    """
    qbo = client()
    to_pid = to_property_id()
    billed: dict[str, float] = defaultdict(float)
    paid: dict[str, float] = defaultdict(float)
    resv: dict[str, list] = defaultdict(list)

    for b in qbo.query_all(
            f"select * from Bill where TxnDate >= '{period}-01' "
            f"and TxnDate <= '{month_end(period)}'", "Bill"):
        doc = b.get("DocNumber") or ""
        if not doc.startswith(BILL_PREFIX):
            continue
        for ln in b.get("Line", []):
            d = ln.get("AccountBasedExpenseLineDetail") or {}
            if (d.get("AccountRef") or {}).get("value") != CHANNEL_FEES:
                continue
            cls = (d.get("ClassRef") or {}).get("name", "").rsplit(":", 1)[-1]
            pid = to_pid(cls) or cls
            amt = round(float(ln.get("Amount") or 0), 2)
            billed[pid] += amt
            resv[pid].append((b.get("TxnDate"), doc.split("_", 1)[1], amt))

    for p in qbo.query_all(
            f"select * from Purchase where TxnDate >= '{paid_period}-01' "
            f"and TxnDate <= '{month_end(paid_period)}'", "Purchase"):
        for ln in p.get("Line", []):
            d = ln.get("AccountBasedExpenseLineDetail") or {}
            if (d.get("AccountRef") or {}).get("value") != BCOM_COGS:
                continue
            cls = (d.get("ClassRef") or {}).get("name", "").rsplit(":", 1)[-1]
            paid[to_pid(cls) or cls or "(no class)"] += round(float(ln.get("Amount") or 0), 2)
    return dict(billed), dict(paid), dict(resv)


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--invoice", default=None, help="the commission .xlsx; default = newest")
    ap.add_argument("--period", default=None,
                    help="YYYY-MM of the BILLS to compare; default = the invoice's period")
    ap.add_argument("--paid-period", default=None,
                    help="YYYY-MM the cash went out; default = the month after --period")
    ap.add_argument("--out", default=None)
    args = ap.parse_args()

    path = Path(args.invoice) if args.invoice else invoice_files()[-1]
    inv, inv_period = read_invoice(path)
    period = args.period or inv_period
    if not period:
        sys.exit("could not infer the period; pass --period YYYY-MM")
    print(f"invoice : {path.name}")
    print(f"          {len(inv)} rows, ${sum(r['Amount'] for r in inv):,.2f}, "
          f"dated {sorted({r['InvoiceDate'] for r in inv})}")
    print(f"          statuses: {sorted({r['Status'] for r in inv})}")
    by_type: dict[str, list] = defaultdict(lambda: [0, 0.0])
    for r in inv:
        by_type[r["Type"]][0] += 1
        by_type[r["Type"]][1] += r["Amount"]
    for t, (n, a) in sorted(by_type.items()):
        print(f"          {t:<28} {n:>3} rows  ${a:>10,.2f}")
    if len(invoice_files()) > 1 and not args.invoice:
        others = [f.name for f in invoice_files() if f != path]
        print(f"          (also present, pass --invoice to use: {', '.join(others)})")
    print(f"bills   : {BILL_PREFIX}* dated {period}")

    nicks = property_nicknames()
    to_pid = to_property_id()
    paid_period = args.paid_period or next_month(period)
    print(f"cash    : Fee - Booking.com Commission dated {paid_period} "
          f"(Booking.com invoices in arrears)")
    billed, paid, resv = qbo_sides(period, paid_period)

    rows, unmapped, ambiguous = [], [], []
    inv_by_pid: dict[str, float] = defaultdict(float)
    for r in inv:
        nk = nicks.get(r["PropertyId"], [])
        if not nk:
            unmapped.append(r)
            continue
        if len(nk) > 1:
            ambiguous.append((r, nk))
        pid = to_pid(nk[0]) or nk[0]
        inv_by_pid[pid] += r["Amount"]
        r["_pid"], r["_nick"] = pid, nk[0]

    print(f"\n{'property':<26}{'billed owner':>14}{'Bcom invoice':>14}"
          f"{'paid ' + paid_period:>15}{'bill-inv':>11}")
    tb = ti = tp = 0.0
    for pid in sorted(set(inv_by_pid) | set(billed) | set(paid)):
        b, i, p = (round(billed.get(pid, 0.0), 2), round(inv_by_pid.get(pid, 0.0), 2),
                   round(paid.get(pid, 0.0), 2))
        tb += b; ti += i; tp += p
        gap = round(b - i, 2)
        print(f"{pid:<26}{b:>14,.2f}{i:>14,.2f}{p:>15,.2f}{gap:>11,.2f}"
              + ("  <-" if abs(gap) >= 0.01 else ""))
        rows.append({"property_id": pid, "billed_to_owner": f"{b:.2f}",
                     "bcom_invoiced": f"{i:.2f}", "paid_cash": f"{p:.2f}",
                     "gap_billed_minus_invoiced": f"{gap:.2f}",
                     "reservations": len(resv.get(pid, [])),
                     "reservation_numbers": " ".join(x[1] for x in resv.get(pid, []))})
    print(f"{'TOTAL':<26}{tb:>14,.2f}{ti:>14,.2f}{tp:>15,.2f}{tb - ti:>11,.2f}")

    if ambiguous:
        print(f"\n{len(ambiguous)} Property ID(s) filed under more than one nickname "
              f"-- the FIRST was used:")
        for r, nk in ambiguous:
            print(f"   {r['PropertyId']:<12} ${r['Amount']:>9,.2f}  {' | '.join(nk)}")
    if unmapped:
        print(f"\n{len(unmapped)} invoice row(s) with no nickname in the payout files:")
        for r in unmapped:
            print(f"   {r['PropertyId']:<12} ${r['Amount']:>9,.2f}  {r['PropertyName'][:44]}")

    out = Path(args.out) if args.out else review_csv(f"bookingcom_commission_{period}")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    print(f"\n-> {out}")
    print("READ-ONLY: nothing was written to QuickBooks.")


if __name__ == "__main__":
    main()
