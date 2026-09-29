"""Post a MULTI-LINE vendor Bill and its payment from a review CSV.

    python -m src.invoices.post_cleaning_bill --all  --csv review/siren_cleaning_bill.csv
    python -m src.invoices.post_cleaning_bill --doc  SirenCL_2026-06 --csv ...      # one
    python -m src.invoices.post_cleaning_bill --all  --csv ... --confirm            # WRITES

Two objects per DocNumber, in this order:

    Bill          service month-end   Dr per (listing, category)   Cr Accounts Payable
    BillPayment   the ACH date        Dr Accounts Payable          Cr the bank   (Check)

The BILL FIRST, always.  A payment cannot link to a bill that does not exist, and a bill with
no payment is a visible open A/P balance the next run picks up -- where a payment with no bill
is money out of the trust against nothing.  If the payment fails, the run STOPS and names the
Bill Id, because half a pair is the one state worth interrupting for.  (Same rule as
`invoices/post_owner_payout`; see its docstring.)

**This is not `post_owner_payout`.**  That one is one line per Bill, one Bill per property, and
its key is (Vendor, TxnDate, amount).  Here one Bill carries a whole month of cleaning -- 9 to
18 lines across listings and categories -- so the rows are GROUPED by `DocNumber` and every row
of a group must agree on the header fields.  A disagreement is fatal: it means two months, or
two banks, were flattened into one document.

## The replaced Purchase is EXPECTED, and that breaks the usual duplicate check

`post_owner_payout` skips a row whose (Vendor, amount) already exists as a Purchase, because a
direct-ACH payout Expense is the same cash a second time.  Here the Purchase being replaced is
the *point*: the Bill supersedes it and the owner deletes it afterwards, so the naive check
would refuse every single row.

So the check is narrowed rather than dropped.  The review CSV names the Purchase in
`ReplacesPurchase`; that one is reported and allowed.  **Any OTHER Siren's Purchase matching the
amount is still a hard skip**, because that is the case the check exists for -- and an amount
that matches two Purchases is exactly how the same month gets paid twice.

Three further layers, all kept:
  * `DocNumber` already a Bill in QBO;
  * (vendor, TxnDate, total) already a Bill -- the same month billed under another DocNumber;
  * (vendor, PayDate, total) already a BillPayment -- the cash already went out that way.

## What is verified after the write

The Bill is read back and compared to the CSV on its total AND its per-(account, class)
subtotals -- **by account and class Id, never by name.**  QuickBooks renders the same class
differently depending on which object is asked (Bill 116623 reports
`Listings:Yacinde NuGrowth:Yacinde B6` where a Purchase built from it reports `Yacinde B6`,
both Id 1000000030), so comparing names once rejected a Purchase that was correct in every
respect -- after it had already been written.  The BillPayment is read back for its total and
its link to the Bill.

Nothing here deletes anything.  The run ends by printing the Purchases and JEs the owner
deletes now that the replacements exist, and the bank lines to re-match.
"""
from __future__ import annotations

import argparse
import csv
import json
import sys
from collections import defaultdict

from ..config import acct_id, client, name as cfg_name
from ..qbo_client import QBOClient
from ..resolver import Resolver, esc

AP_ACCOUNT_ID = acct_id("accounts_payable")
DOCNUMBER_MAX = 21

# Every row of a group describes ONE document, so these must be identical across it.  A
# builder bug that merged two months would otherwise post a Bill dated one month carrying
# another month's lines, which reads as correct in every total.
HEADER_FIELDS = ("TxnDate", "PayDate", "Vendor", "Location", "PaidFrom", "APAccount", "Memo")


def group_rows(rows: list[dict]) -> dict[str, list[dict]]:
    groups: dict[str, list[dict]] = defaultdict(list)
    for r in rows:
        groups[r["DocNumber"]].append(r)
    for doc, rs in groups.items():
        for f in HEADER_FIELDS:
            vals = {(r.get(f) or "").strip() for r in rs}
            if len(vals) > 1:
                sys.exit(f"ABORT -- {doc} has {len(vals)} different {f} values ({sorted(vals)}); "
                         f"one Bill cannot carry two.  Nothing was written.")
        rs.sort(key=lambda r: int(r["LineNum"]))
    return dict(groups)


def existing(qbo: QBOClient, vendor: str, start: str, end: str):
    """Bills, BillPayments and Purchases for this vendor that a post could duplicate.

    Bills and payments key on (TxnDate, total); Purchases key on amount ALONE and keep every
    Id, because the one we are replacing carries the bank's date while the Bill carries the
    service month's -- and because the caller has to tell the expected Purchase from a
    stranger, which it cannot do from a date.
    """
    v = vendor.casefold()

    def mine(o):     # the vendor half of every key is case-folded (the ARVIND VISVANATHAN lesson)
        return v in ((o.get("VendorRef") or {}).get("name") or "").casefold()

    bills = {(b["TxnDate"], round(float(b.get("TotalAmt") or 0), 2)): b["Id"]
             for b in qbo.query_all(f"SELECT * FROM Bill WHERE TxnDate >= '{start}' "
                                    f"AND TxnDate <= '{end}'", "Bill") if mine(b)}
    pays = {(p["TxnDate"], round(float(p.get("TotalAmt") or 0), 2)): p["Id"]
            for p in qbo.query_all(f"SELECT * FROM BillPayment WHERE TxnDate >= '{start}' "
                                   f"AND TxnDate <= '{end}'", "BillPayment") if mine(p)}
    purch: dict[float, list[tuple[str, str]]] = defaultdict(list)
    for e in qbo.query_all(f"SELECT * FROM Purchase WHERE TxnDate >= '{start}' "
                           f"AND TxnDate <= '{end}'", "Purchase"):
        if v not in ((e.get("EntityRef") or {}).get("name") or "").casefold():
            continue
        purch[round(float(e.get("TotalAmt") or 0), 2)].append((e["TxnDate"], e["Id"]))
    return bills, pays, purch


def build_bill(doc: str, rs: list[dict], res: Resolver) -> tuple[dict, list[str], float]:
    h, errs = rs[0], []
    vendor = res.vendor(h["Vendor"])
    if vendor is None:
        errs.append(f"vendor not found: {h['Vendor']!r}")
    ap = res.account(h["APAccount"])
    if ap is None:
        errs.append(f"A/P account not found: {h['APAccount']!r}")
    elif ap != AP_ACCOUNT_ID:
        errs.append(f"A/P account {h['APAccount']!r} is Id {ap}, not the configured "
                    f"{AP_ACCOUNT_ID} -- accounts.yml and the CSV disagree")
    dept = res.department(h["Location"]) if h.get("Location") else None
    if h.get("Location") and dept is None:
        errs.append(f"location not found: {h['Location']!r}")

    lines, total = [], 0.0
    for r in rs:
        acct, klass = res.account(r["Account"]), res.klass(r["Class"])
        if acct is None:
            errs.append(f"line {r['LineNum']}: account not found: {r['Account']!r}")
        if klass is None:
            errs.append(f"line {r['LineNum']}: class not found: {r['Class']!r}")
        amt = round(float(r["Amount"]), 2)
        if amt <= 0:
            errs.append(f"line {r['LineNum']}: amount is {amt:,.2f}; a cost must be positive")
        total = round(total + amt, 2)
        lines.append({
            "DetailType": "AccountBasedExpenseLineDetail",
            "Amount": amt,
            "Description": r["Description"][:4000],
            "AccountBasedExpenseLineDetail": {
                "AccountRef": {"value": acct},
                "ClassRef": {"value": klass},
                "BillableStatus": "NotBillable",
                "TaxCodeRef": {"value": "NON"},
            },
        })

    payload = {
        "VendorRef": {"value": vendor},
        "APAccountRef": {"value": ap},
        "DocNumber": doc[:DOCNUMBER_MAX],
        "TxnDate": h["TxnDate"],
        # Raised at the service month-end, due when the ACH actually goes -- which reads
        # correctly and is what the A/P aging should show.  Nothing here groups by DueDate.
        "DueDate": h["PayDate"] or h["TxnDate"],
        "PrivateNote": h["Memo"][:4000],
        "Line": lines,
    }
    if dept:
        payload["DepartmentRef"] = {"value": dept}
    return payload, errs, total


def build_payment(rs: list[dict], bill: dict, res: Resolver) -> tuple[dict, list[str]]:
    h, errs = rs[0], []
    bank_name = (h.get("PaidFrom") or "").strip() or cfg_name("bank_str")
    bank = res.account(bank_name)
    if bank is None:
        errs.append(f"paid-from account not found: {bank_name!r}")
    dept = res.department(h["Location"]) if h.get("Location") else None
    amount = round(float(bill["TotalAmt"]), 2)
    payload = {
        "VendorRef": bill["VendorRef"],
        "PayType": "Check",
        "CheckPayment": {"BankAccountRef": {"value": bank}},
        # The BANK's own date, never the Bill's.  This is the object the feed matches, and a
        # Bill dated before its payment is ordinary where the reverse is not.
        "TxnDate": h["PayDate"] or bill["TxnDate"],
        "TotalAmt": amount,
        "Line": [{"Amount": amount,
                  "LinkedTxn": [{"TxnId": bill["Id"], "TxnType": "Bill"}]}],
    }
    if dept:
        payload["DepartmentRef"] = {"value": dept}
    return payload, errs


def subtotals(lines: list[dict]) -> dict[tuple[str, str], float]:
    """Per (account Id, class Id) subtotals -- Ids, never names.  See the module docstring."""
    out: dict[tuple[str, str], float] = defaultdict(float)
    for ln in lines:
        d = ln.get("AccountBasedExpenseLineDetail") or {}
        key = ((d.get("AccountRef") or {}).get("value"), (d.get("ClassRef") or {}).get("value"))
        out[key] = round(out[key] + round(float(ln.get("Amount") or 0), 2), 2)
    return dict(out)


def verify(qbo: QBOClient, bill_id: str, pay_id: str, want: dict, total: float) -> list[str]:
    bad = []
    got = qbo.query(f"SELECT * FROM Bill WHERE Id = '{esc(bill_id)}'") \
        .get("QueryResponse", {}).get("Bill", [])
    if not got:
        return [f"Bill {bill_id} could not be read back"]
    b = got[0]
    if round(float(b.get("TotalAmt") or 0), 2) != total:
        bad.append(f"Bill {bill_id} total {b.get('TotalAmt')} != {total:,.2f}")
    if subtotals(b.get("Line", [])) != want:
        bad.append(f"Bill {bill_id} per-(account, class) subtotals differ from the CSV")
    got = qbo.query(f"SELECT * FROM BillPayment WHERE Id = '{esc(pay_id)}'") \
        .get("QueryResponse", {}).get("BillPayment", [])
    if not got:
        return bad + [f"BillPayment {pay_id} could not be read back"]
    pm = got[0]
    if round(float(pm.get("TotalAmt") or 0), 2) != total:
        bad.append(f"BillPayment {pay_id} total {pm.get('TotalAmt')} != {total:,.2f}")
    linked = {t.get("TxnId") for l in pm.get("Line", []) for t in l.get("LinkedTxn", [])}
    if bill_id not in linked:
        bad.append(f"BillPayment {pay_id} is not linked to Bill {bill_id} (linked: {linked})")
    return bad


def post_one(doc: str, rs: list[dict], qbo: QBOClient, res: Resolver, bills: dict, pays: dict,
             purch: dict, confirm: bool, verbose: bool) -> tuple[str, str]:
    """Returns (status, detail).  status in {skipped, error, ready, posted}."""
    h = rs[0]
    flat = " ".join(f"{r.get('Account')}|{r.get('Class')}|{r.get('Vendor')}" for r in rs)
    if "***" in flat:
        return "error", "unresolved marker (***) in the review CSV"

    already = qbo.query(f"SELECT * FROM Bill WHERE DocNumber = '{esc(doc[:DOCNUMBER_MAX])}'") \
        .get("QueryResponse", {}).get("Bill", [])
    if already:
        return "skipped", f"DocNumber {doc} already in QBO as Bill {already[0]['Id']}"

    total = round(sum(float(r["Amount"]) for r in rs), 2)
    if (h["TxnDate"], total) in bills:
        return "skipped", (f"a {total:,.2f} Bill for {h['Vendor']} already sits on "
                           f"{h['TxnDate']} (Bill {bills[(h['TxnDate'], total)]})")
    if (h["PayDate"], total) in pays:
        return "skipped", (f"{h['Vendor']} was already paid {total:,.2f} on {h['PayDate']} "
                           f"(BillPayment {pays[(h['PayDate'], total)]})")

    # The Purchase this replaces is expected; any OTHER one at the same amount is not.
    expected = (h.get("ReplacesPurchase") or "").strip()
    others = [(d, i) for d, i in purch.get(total, []) if i != expected]
    if others:
        where = ", ".join(f"{d} (Purchase {i})" for d, i in sorted(others))
        return "skipped", (f"{h['Vendor']} has an unexpected {total:,.2f} Purchase at {where} "
                           f"-- the CSV only accounts for Purchase {expected or '(none)'}")

    payload, errs, built = build_bill(doc, rs, res)
    if errs:
        return "error", "; ".join(errs)
    if abs(built - total) > 0.005:
        return "error", f"lines build to {built:,.2f} but the CSV rows sum to {total:,.2f}"
    if verbose:
        print(json.dumps(payload, indent=2))
    if not confirm:
        seen = "on the books" if expected in {i for v in purch.values() for _, i in v} else "GONE"
        return "ready", (f"Bill {total:,.2f} over {len(rs)} line(s) dated {h['TxnDate']}, "
                         f"paid {h['PayDate']} from {h['PaidFrom']}  "
                         f"[Purchase {expected or '?'} {seen}]")

    want = subtotals(payload["Line"])
    bill = qbo.post("bill", payload)
    bills[(h["TxnDate"], total)] = bill["Id"]
    if round(float(bill["TotalAmt"]), 2) != total:
        sys.exit(f"Bill {bill['Id']} posted as {bill['TotalAmt']} but the CSV said {total:,.2f}. "
                 f"STOPPING -- no payment was made against it.")

    pay_payload, errs = build_payment(rs, bill, res)
    if errs:
        sys.exit(f"Bill {bill['Id']} is POSTED but its payment could not be built: "
                 f"{'; '.join(errs)}.  Pay or delete it before re-running.")
    pay = qbo.post("billpayment", pay_payload)
    pays[(h["PayDate"], total)] = pay["Id"]

    bad = verify(qbo, bill["Id"], pay["Id"], want, total)
    if bad:
        sys.exit(f"Bill {bill['Id']} + BillPayment {pay['Id']} are POSTED but do not verify: "
                 f"{'; '.join(bad)}.  STOPPING -- do NOT delete anything.")
    return "posted", (f"Bill {bill['Id']} + Payment {pay['Id']}  {total:,.2f}  "
                      f"{len(rs)} line(s)")


def cleanup_guide(groups: dict[str, list[dict]], tally: dict) -> None:
    """What the owner does once the replacements exist.

    Printed even on a dry run, because the decision to convert is a decision to delete
    afterwards -- and until the delete happens the cash is on the books twice.
    """
    todo = [(d, rs) for d, rs in sorted(groups.items())
            if (rs[0].get("ReplacesPurchase") or rs[0].get("ReplacesJE"))]
    if not todo:
        return
    print("\nNow the replacements exist, DELETE the originals (this project does not delete):")
    for d, rs in todo:
        h = rs[0]
        # A month converted with --paid has no Purchase left to delete, so do not print an
        # empty one -- a delete list naming a blank object invites deleting the wrong thing.
        bits = [f"Purchase {h['ReplacesPurchase']}"] if h.get("ReplacesPurchase") else []
        if h.get("ReplacesJE"):
            bits.append(f"JE {h['ReplacesJE']}")
        # The GROUP's total, never rs[0]["Amount"] -- that is one line of a 9-to-18 line month,
        # and a delete list quoting the wrong amount is how the wrong object gets deleted.
        total = round(sum(float(r["Amount"]) for r in rs), 2)
        print(f"   {d:<16} {', '.join(bits):<34} ({total:,.2f} paid {h['PayDate']})")
    print("\nThen re-match in the bank feed: deleting each Expense releases its line back to "
          "For Review,\n  where the new BillPayment of the same amount is offered under "
          "Find match.  The feed\n  cannot be read from here, so only you can confirm that.")


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.invoices.post_cleaning_bill")
    g = ap.add_mutually_exclusive_group(required=True)
    g.add_argument("--doc", help="one DocNumber from the review CSV")
    g.add_argument("--all", action="store_true")
    ap.add_argument("--csv", required=True)
    ap.add_argument("--confirm", action="store_true", help="actually POST to QuickBooks")
    ap.add_argument("--start", default=None, help="window scanned for what is already booked")
    ap.add_argument("--end", default=None)
    args = ap.parse_args()

    rows = list(csv.DictReader(open(args.csv, encoding="utf-8")))
    if args.doc:
        rows = [r for r in rows if r["DocNumber"] == args.doc]
        if not rows:
            sys.exit(f"No row with DocNumber {args.doc!r} in {args.csv}")
    groups = group_rows(rows)

    vendors = {r["Vendor"] for r in rows}
    if len(vendors) > 1:
        sys.exit(f"ABORT -- {len(vendors)} vendors in one CSV ({sorted(vendors)}); the "
                 f"duplicate check is per vendor.  Split the file.")
    vendor = next(iter(vendors))

    # The window must cover the BILL dates and the PAY dates: the replaced Purchase carries
    # the pay date, and a Bill for the same month could sit on either.
    dates = sorted({r["TxnDate"] for r in rows} | {r["PayDate"] for r in rows if r["PayDate"]})
    start = args.start or f"{dates[0][:8]}01"
    end = args.end or dates[-1]

    qbo = client()
    res = Resolver(qbo)
    bills, pays, purch = existing(qbo, vendor, start, end)
    print(f"{start}..{end} for {vendor}: {len(bills)} Bill(s), {len(pays)} BillPayment(s), "
          f"{sum(len(v) for v in purch.values())} Purchase(s) already on the books\n")

    tally: dict[str, int] = {}
    failures, posted_total = [], 0.0
    for doc, rs in sorted(groups.items()):
        status, detail = post_one(doc, rs, qbo, res, bills, pays, purch, args.confirm,
                                  bool(args.doc))
        tally[status] = tally.get(status, 0) + 1
        amt = round(sum(float(r["Amount"]) for r in rs), 2)
        if status == "posted":
            posted_total += amt
        print(f"  {status.upper():8} {doc:<16} {detail}")
        if status == "error":
            failures.append(doc)

    if args.confirm:
        cleanup_guide(groups, tally)

    print("\n" + "  ".join(f"{k}={v}" for k, v in sorted(tally.items())))
    if args.confirm:
        print(f"posted {posted_total:,.2f}")
    else:
        ready = round(sum(float(r["Amount"]) for r in rows), 2)
        print(f"DRY RUN -- nothing posted.  {ready:,.2f} across {len(groups)} Bill(s) / "
              f"{len(rows)} line(s).  Re-run with --confirm to POST.")
    if failures:
        sys.exit(f"{len(failures)} Bill(s) could not be built: {', '.join(failures)}")


if __name__ == "__main__":
    main()
