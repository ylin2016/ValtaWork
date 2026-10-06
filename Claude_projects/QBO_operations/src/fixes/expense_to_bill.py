"""Recreate an unpaid Expense (Purchase) as a Bill, line for line.

The opposite of `fixes/bill_to_expense`, and for the opposite situation: that one converts a
Bill the trust has already PAID into the Expense shape; this one converts an Expense that was
posted before the money moved into the payable it actually is.

    python -m src.fixes.expense_to_bill --ids 120062 --date 2026-09-30     # dry run
    python -m src.fixes.expense_to_bill --ids 120062 --date 2026-09-30 --confirm

Why it exists: AA invoice 1204 covers cleaning done 09/16-09/30 but was billed on 10/01 and not
yet paid, so its Expense had to be dated in OCTOBER to leave the bank honest -- which puts a
September cost in an October period for every owner statement and every September report.  A
Bill separates the two dates: the cost lands on the Bill's TxnDate and the cash moves later,
when the Bill Payment is entered.  An Expense whose cash HAS left needs the payment half too, or the bank line
it matched is orphaned: `--with-payment` then also writes the Bill Payment (Check) from the same
bank account, on the Expense's own date and carrying its DocNumber, so the released bank-feed
line re-matches exactly as it did before. That is the shape the July and August AA cleaning
already has -- Bill + BillPayment -- and what `bill_to_expense` converted away from.

Same order as its opposite number, and for the same reason:

    1. this script CREATES the Bill                    <- reversible, adds nothing else
    2. the owner DELETES the Expense in the UI         <- deleting is not something this
                                                          project does
    3. `--payment-only --bill <Id> --ids <Expense Id>` writes the Bill Payment
    4. re-run with --verify

The payment CANNOT be written before step 2 when it is to carry the Expense's own DocNumber --
QBO rejects it as `Duplicate Document Number Error` while the Expense still owns that number --
and the point of carrying it is that the released bank-feed line re-matches on the reference it
already has.  So `--with-payment` is for an Expense being replaced in one pass under a different
number; after a delete, use `--payment-only`, which reads the deleted Expense's details from the
Bill this script wrote.

Creating first leaves the amount visibly on the books twice rather than leaving a month with
cost missing from it.  When the Expense being replaced is dated in a DIFFERENT month from the
Bill -- which is the whole point here -- the duplicate sits in the later month only, so the
month you are closing is right as soon as step 1 runs.

**Every line is mirrored exactly**: Amount, AccountRef, ClassRef, Description, CustomerRef,
BillableStatus, TaxCodeRef.  Yacinde cleaning carries who bears each clean in the ACCOUNT and
the CLASS (`fixes/yacinde_clean_class_align`), and a Bill that loses them re-charges ten owners
for the HOA's cleans.  The run aborts unless the new total and the per-account, per-class
subtotals match the Expense exactly.

`Owner_statement_whole`'s `qbo_sync` ingests Bill lines as well as Expense lines, so the owners'
share reaches their statements from a Bill exactly as it did from the Expense -- by class, with
the flat `Yacinde HOA` class still reaching none, which is what keeps the HOA's share off them.
"""
from __future__ import annotations

import argparse
import sys
from collections import defaultdict

from ..config import client
from ..qbo_client import QBOClient
from ..resolver import esc

# A Bill carries no PaymentType, bank account or PrintStatus, and QBO owns the rest.
DROP = ("Id", "SyncToken", "MetaData", "domain", "sparse", "PaymentType", "PaymentMethodRef",
        "AccountRef", "PurchaseEx", "PrintStatus", "Credit", "TotalAmt", "EntityRef")
# `YacindeCL_<inv#>` is what the earlier AA cleaning bills used.
DOC_PREFIX = "YacindeCL_"


def subtotals(lines: list[dict]) -> dict:
    """(account, class) -> amount, which is where the HOA/owner split actually lives."""
    out: dict[tuple[str, str], float] = defaultdict(float)
    for ln in lines:
        d = ln.get("AccountBasedExpenseLineDetail") or {}
        out[((d.get("AccountRef") or {}).get("value"),
             (d.get("ClassRef") or {}).get("value"))] += round(float(ln.get("Amount") or 0), 2)
    return {k: round(v, 2) for k, v in out.items()}


def purchase(qbo: QBOClient, pid: str) -> dict:
    got = qbo.query_all(f"SELECT * FROM Purchase WHERE Id = '{esc(pid)}'", "Purchase")
    if not got:
        sys.exit(f"ABORT — Purchase {pid} not found")
    return got[0]


def docnumber(cur: dict) -> str:
    """`YacindeCL_1204` from the Expense's `20261002_INV1204_3860`, else its own DocNumber."""
    doc = cur.get("DocNumber") or ""
    for part in doc.split("_"):
        if part.upper().startswith("INV") and part[3:].isdigit():
            return f"{DOC_PREFIX}{part[3:]}"
    return doc


def build(cur: dict, txndate: str, duedate: str | None, doc: str) -> dict:
    body = {k: v for k, v in cur.items() if k not in DROP}
    body["VendorRef"] = cur.get("EntityRef")        # a Bill names the vendor, not an entity
    body["TxnDate"] = txndate
    if duedate:
        body["DueDate"] = duedate
    body["DocNumber"] = doc
    body["Line"] = [{k: v for k, v in ln.items() if k not in ("Id", "CustomExtensions")}
                    for ln in cur.get("Line", [])]
    body["PrivateNote"] = ((cur.get("PrivateNote") or "").strip()
                           + f" Recreated as a Bill dated {txndate} so the cost falls in that "
                             f"month; replaces Expense {cur['Id']} ({cur.get('DocNumber')}), "
                             f"which is to be deleted and whose cash had not left.")
    return body


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.fixes.expense_to_bill")
    ap.add_argument("--ids", required=True, help="Purchase Id(s), comma separated")
    ap.add_argument("--date", required=True, help="the Bill's TxnDate (YYYY-MM-DD)")
    ap.add_argument("--due", default=None, help="the Bill's DueDate (YYYY-MM-DD)")
    ap.add_argument("--docnumber", default=None,
                    help="with --payment-only: the payment's DocNumber, normally the deleted "
                         "Expense's own so the bank-feed line re-matches on its reference")
    ap.add_argument("--with-payment", action="store_true",
                    help="the Expense's cash HAS left: also write the Bill Payment (Check) from "
                         "the same bank account, dated as the Expense was")
    ap.add_argument("--payment-only", action="store_true",
                    help="the Expense is already deleted: write only the Bill Payment, from the "
                         "bank account and date recorded in --bill's note")
    ap.add_argument("--bill", default=None, help="with --payment-only: the Bill's Id")
    ap.add_argument("--bank", default=None,
                    help="with --payment-only: the paying bank account (default: bank_str)")
    ap.add_argument("--verify", action="store_true",
                    help="report what is on the books for these Ids and stop")
    ap.add_argument("--confirm", action="store_true", help="actually WRITE")
    args = ap.parse_args()
    ids = [i.strip() for i in args.ids.split(",") if i.strip()]

    qbo = client()

    if args.verify:
        for pid in ids:
            got = qbo.query_all(f"SELECT * FROM Purchase WHERE Id = '{esc(pid)}'", "Purchase")
            print(f"Expense {pid}: " + ("still on the books — "
                  f"{got[0]['TxnDate']} {got[0].get('DocNumber')} "
                  f"{float(got[0]['TotalAmt']):,.2f}" if got else "deleted"))
        for b in qbo.query_all(f"SELECT * FROM Bill WHERE DocNumber LIKE '{DOC_PREFIX}%'", "Bill"):
            print(f"Bill {b['Id']}: {b['TxnDate']} {b.get('DocNumber')} "
                  f"{float(b['TotalAmt']):,.2f}  balance {b.get('Balance')}")
        return

    if args.payment_only:
        if not args.bill:
            sys.exit("--payment-only needs --bill")
        from ..config import name as cfg_name
        from ..resolver import Resolver
        res = Resolver(qbo)
        bill = qbo.query_all(f"SELECT * FROM Bill WHERE Id = '{esc(args.bill)}'", "Bill")[0]
        left = round(float(bill.get("Balance") or 0), 2)
        if not left:
            sys.exit(f"Bill {args.bill} ({bill.get('DocNumber')}) is already paid in full")
        for pid in ids:                       # the Expense must be gone, or its DocNumber blocks
            if qbo.query_all(f"SELECT * FROM Purchase WHERE Id = '{esc(pid)}'", "Purchase"):
                sys.exit(f"Expense {pid} is still on the books -- delete it first, or its "
                         f"DocNumber will be refused as a duplicate")
        bank_name = args.bank or cfg_name("bank_str")
        bank_id = res.account(bank_name)
        if bank_id is None:
            sys.exit(f"bank account not found: {bank_name!r}")
        pay = {
            "VendorRef": bill.get("VendorRef"),
            "TxnDate": args.date,
            "PayType": "Check",
            "CheckPayment": {"BankAccountRef": {"value": bank_id, "name": bank_name}},
            "TotalAmt": left,
            "PrivateNote": f"Pays Bill {bill['Id']} ({bill.get('DocNumber')}).",
            "Line": [{"Amount": left,
                      "LinkedTxn": [{"TxnId": bill["Id"], "TxnType": "Bill"}]}],
        }
        if args.docnumber:
            pay["DocNumber"] = args.docnumber[:21]
        if bill.get("DepartmentRef"):
            pay["DepartmentRef"] = bill["DepartmentRef"]
        print(f"  Bill {bill['Id']} {bill.get('DocNumber')} balance {left:,.2f} -> BillPayment "
              f"{args.date} from {bank_name}"
              + (f", DocNumber {args.docnumber}" if args.docnumber else "")
              + ("" if args.confirm else "   (dry run)"))
        if not args.confirm:
            print("\nDRY RUN — nothing written. Re-run with --confirm.")
            return
        got = qbo.post("billpayment", pay)
        back = qbo.query_all(f"SELECT * FROM Bill WHERE Id = '{esc(bill['Id'])}'", "Bill")[0]
        rest = round(float(back.get("Balance") or 0), 2)
        print(f"    -> BillPayment {got['Id']}  {got['TxnDate']}  {float(got['TotalAmt']):,.2f}; "
              f"bill balance now {rest:,.2f}")
        if rest:
            sys.exit(f"ABORT — Bill {bill['Id']} still shows {rest:,.2f} outstanding")
        return

    made = []
    for pid in ids:
        cur = purchase(qbo, pid)
        total, sub = round(float(cur.get("TotalAmt") or 0), 2), subtotals(cur.get("Line", []))
        doc = docnumber(cur)
        print(f"  Expense {pid}  {cur.get('DocNumber')}  {cur['TxnDate']}  {total:>9,.2f}  "
              f"-> Bill {doc}  {args.date}" + ("" if args.confirm else "   (dry run)"))
        for (acct, cls), amt in sorted(sub.items(), key=lambda kv: -kv[1]):
            print(f"      {amt:>9,.2f}  account {acct}  class {cls}")
        bank = (cur.get("AccountRef") or {}).get("name")
        if bank and args.with_payment:
            print(f"      + Bill Payment (Check) {total:,.2f} from {bank} dated {cur['TxnDate']}, "
                  f"DocNumber {cur.get('DocNumber')} — the released bank line re-matches on it")
        elif bank:
            print(f"      was paid from {bank} — no payment is written, so the Bill stays open; "
                  f"pass --with-payment if that cash really left")
        if not args.confirm:
            continue
        body = build(cur, args.date, args.due, doc)
        res = qbo.post("bill", body)
        after = round(float(res.get("TotalAmt") or 0), 2)
        if abs(after - total) > 0.005 or subtotals(res.get("Line", [])) != sub:
            sys.exit(f"ABORT — new Bill {res.get('Id')} does not reproduce Expense {pid}: "
                     f"{after:,.2f} vs {total:,.2f}. Delete nothing; investigate.")
        made.append((pid, cur, res))
        print(f"    -> Bill {res['Id']}  {res.get('DocNumber')}  {res['TxnDate']}  {after:,.2f}  "
              f"({len(res.get('Line', []))} lines, subtotals match)  balance {res.get('Balance')}")
        if bank and args.with_payment:
            pay = {
                "VendorRef": cur.get("EntityRef"),
                "TxnDate": cur["TxnDate"],
                "PayType": "Check",
                "CheckPayment": {"BankAccountRef": cur.get("AccountRef")},
                "TotalAmt": after,
                "DocNumber": (cur.get("DocNumber") or "")[:21],
                "PrivateNote": (f"Pays Bill {res['Id']} ({res.get('DocNumber')}); the payment half "
                                f"of Expense {pid}, which is to be deleted."),
                "Line": [{"Amount": after,
                          "LinkedTxn": [{"TxnId": res["Id"], "TxnType": "Bill"}]}],
            }
            if cur.get("DepartmentRef"):
                pay["DepartmentRef"] = cur["DepartmentRef"]
            got_pay = qbo.post("billpayment", pay)
            back = qbo.query_all(f"SELECT * FROM Bill WHERE Id = '{esc(res['Id'])}'", "Bill")[0]
            left = round(float(back.get("Balance") or 0), 2)
            print(f"    -> BillPayment {got_pay['Id']}  {got_pay['TxnDate']}  "
                  f"{float(got_pay['TotalAmt']):,.2f} from {bank}; bill balance now {left:,.2f}")
            if left:
                sys.exit(f"ABORT — Bill {res['Id']} still shows {left:,.2f} outstanding after its "
                         f"payment. Delete nothing; investigate.")

    if not args.confirm:
        print("\nDRY RUN — nothing written. Re-run with --confirm.")
        return
    if made:
        print(f"\ncreated {len(made)} bill(s). Now delete the Expense(s) in QuickBooks:")
        for pid, cur, bill in made:
            print(f"  delete Expense {pid} ({cur.get('DocNumber')}, {cur['TxnDate']})  "
                  f"— replaced by Bill {bill['Id']} {bill.get('DocNumber')} ({bill['TxnDate']})")
        print("\nUntil then the amount is on the books twice, in both months.")


if __name__ == "__main__":
    main()
