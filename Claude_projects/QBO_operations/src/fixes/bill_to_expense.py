"""Recreate a paid Bill as an Expense (Purchase), line for line.

QuickBooks has no "convert to expense".  A Bill plus its Bill Payment and an Expense post
the same net entry -- the Bill just parks the amount in A/P in between -- so converting is
delete-and-recreate, and only the recreate half belongs here:

    1. this script CREATES the replacement Expense(s)           <- reversible, adds nothing
       else to the books until step 2
    2. the owner DELETES the Bill and its Bill Payment in the UI (deleting is not something
       this project does, and QBO refuses to delete a paid Bill until its payment goes first)
    3. re-run with --verify to confirm the books landed where they started

Creating BEFORE deleting is deliberate.  In between, the amount is on the books twice and
plainly visible; the other order leaves the HOA receivable short and the HOA invoices
clearing debits that no longer exist, which is a much worse place to stop half way.

**Every line is mirrored exactly** -- Amount, AccountRef, ClassRef, Description,
CustomerRef, BillableStatus, TaxCodeRef.  The AA cleaning bills carry the HOA split
(`fixes/yacinde_hoa_split`) in those very fields, and an Expense that loses them silently
re-charges ten owners for the HOA's cleans.  The run aborts unless the new total and the
per-account, per-class subtotals match the Bill exactly.

    python -m src.fixes.bill_to_expense --ids 116622,116623,116789,116696
    python -m src.fixes.bill_to_expense --ids ... --date payment --confirm
    python -m src.fixes.bill_to_expense --ids ... --verify

--date decides what the Expense is dated, and it is not cosmetic:

    payment  the day the cash actually left (default).  The bank-feed line released by
             deleting the Bill Payment re-matches on this date, and it is what the
             September expense (20260917_INV1200_4500) already uses.  But an owner-borne
             line MOVES MONTH if the bill was dated month-end and paid later.
    bill     keeps every line in the month the Bill assigned it, so no owner statement
             changes period -- at the cost of a bank line that must match a few days off.

The dry run prints the per-month movement of owner-borne cost under each choice.  Pick
with that in front of you, not from the flag name.
"""
from __future__ import annotations

import argparse
import csv
import sys
from collections import defaultdict
from pathlib import Path

from ..config import client
from ..paths import review_csv
from ..qbo_client import QBOClient
from ..resolver import Resolver

OWNER_PREFIX = "Trust Liabilities:Owner Payables:"


def detail(ln: dict) -> dict | None:
    return ln.get("AccountBasedExpenseLineDetail")


def subtotals(lines: list[dict]) -> dict:
    """(account Id, class Id) -> amount, the fingerprint an Expense must reproduce.

    Keyed on Ids, never names.  QuickBooks renders the SAME class differently depending on
    the object asking: Bill 116623 reports its class as
    `Listings:Yacinde NuGrowth:Yacinde B6` and the Purchase built from it reports the leaf
    `Yacinde B6`, both Id 1000000030.  Comparing names failed a Purchase that was correct
    in every respect and aborted the run mid-batch.
    """
    out: dict[tuple[str, str], float] = defaultdict(float)
    for ln in lines:
        d = detail(ln)
        if not d:
            continue
        out[((d.get("AccountRef") or {}).get("value", ""),
             (d.get("ClassRef") or {}).get("value", ""))] += round(float(ln.get("Amount") or 0), 2)
    return {k: round(v, 2) for k, v in out.items()}


def display_subtotals(lines: list[dict]) -> dict:
    """The same grouping keyed on names, for the dry run a human reads."""
    out: dict[tuple[str, str], float] = defaultdict(float)
    for ln in lines:
        d = detail(ln)
        if not d:
            continue
        out[((d.get("AccountRef") or {}).get("name", ""),
             (d.get("ClassRef") or {}).get("name", ""))] += round(float(ln.get("Amount") or 0), 2)
    return {k: round(v, 2) for k, v in out.items()}


def payment_of(qbo: QBOClient, bill: dict) -> dict | None:
    for t in bill.get("LinkedTxn") or []:
        if t.get("TxnType") in ("BillPaymentCheck", "BillPayment"):
            got = qbo.query(
                f"select * from BillPayment where Id = '{t['TxnId']}'"
            ).get("QueryResponse", {}).get("BillPayment", [])
            if got:
                return got[0]
    return None


def doc_number(bill: dict, txndate: str, total: float) -> str:
    """The September expense's scheme: <yyyymmdd>_INV<aa invoice>_<total>."""
    inv = (bill.get("DocNumber") or "").rsplit("_", 1)[-1]
    return f"{txndate.replace('-', '')}_INV{inv}_{total:.0f}"


def build(bill: dict, pay: dict, txndate: str, bank_id: str, paytype: str) -> dict:
    """A Purchase's PaymentType is what QuickBooks DISPLAYS it as, and it is FINAL.

    `Cash` renders as **Expense**, `Check` renders as **Check**, `CreditCard` renders as
    an Expense on a card.  Mirroring the original BillPaymentCheck therefore produced four
    Checks when the whole point was to have Expenses matching `20260917_INV1200_4500`.

    **It cannot be corrected afterwards.**  Check and Expense are different transaction
    types underneath (`PurchaseEx` TxnType 3 vs 54), so a full update answers 610 "Object
    Not Found ... has been made inactive" -- which names no field and reads like a dead
    account -- while a SPARSE update returns 200 and silently leaves the old value.  Forcing
    `PurchaseEx.TxnType` fails too.  The only fix is to delete and re-create, so get this
    right in the dry run: `--paytype` is not cosmetic once the object exists.

    The posting is identical either way; only the label and the register icon differ.
    """
    lines = []
    for n, ln in enumerate(bill.get("Line", []), start=1):
        d = detail(ln)
        if not d:
            continue
        det = {"AccountRef": dict((d.get("AccountRef") or {}))}
        for k in ("ClassRef", "CustomerRef", "TaxCodeRef", "BillableStatus"):
            if d.get(k):
                det[k] = d[k]
        lines.append({"LineNum": n, "Amount": round(float(ln.get("Amount") or 0), 2),
                      "Description": ln.get("Description", ""),
                      "DetailType": "AccountBasedExpenseLineDetail",
                      "AccountBasedExpenseLineDetail": det})
    body = {
        "PaymentType": paytype,
        "AccountRef": {"value": bank_id},
        "EntityRef": dict(bill.get("VendorRef") or {}),
        "TxnDate": txndate,
        "DocNumber": doc_number(bill, txndate, float(bill.get("TotalAmt") or 0)),
        "PrivateNote": (f"Replaces Bill {bill['Id']} {bill.get('DocNumber')} "
                        f"(dated {bill.get('TxnDate')}) and Bill Payment {pay['Id']} "
                        f"(dated {pay.get('TxnDate')}), converted to an Expense so AA "
                        f"cleaning is entered one way throughout."),
        "Line": lines,
    }
    if bill.get("DepartmentRef"):
        body["DepartmentRef"] = dict(bill["DepartmentRef"])
    return body


def clone(qbo: QBOClient, ids: list[str], paytype: str, confirm: bool) -> None:
    """Re-create existing Purchases under a different PaymentType.

    PaymentType is fixed once an object exists (see `build`), so correcting a Check into an
    Expense means making a new object.  Used when the Bills these came from have already
    been deleted and there is nothing left to rebuild from but the Purchases themselves.

    Everything is carried over EXCEPT the server-owned fields: Id, SyncToken, MetaData,
    PurchaseEx (it holds the old TxnType) and PrintStatus (check-only).  Line Ids go too --
    they belong to the old object, and QBO assigns fresh ones.
    """
    made = []
    for pid in ids:
        got = qbo.query(f"select * from Purchase where Id = '{pid}'").get(
            "QueryResponse", {}).get("Purchase", [])
        if not got:
            sys.exit(f"ABORT — Purchase {pid} not found")
        cur = got[0]
        if cur.get("PaymentType") == paytype:
            print(f"  Purchase {pid}  already {paytype} — skipped")
            continue
        total, sub = round(float(cur.get("TotalAmt") or 0), 2), subtotals(cur.get("Line", []))
        body = {k: v for k, v in cur.items()
                if k not in ("Id", "SyncToken", "MetaData", "PurchaseEx", "PrintStatus",
                             "domain", "sparse")}
        body["PaymentType"] = paytype
        body["Line"] = [{k: v for k, v in ln.items() if k not in ("Id", "CustomExtensions")}
                        for ln in cur.get("Line", [])]
        body["PrivateNote"] = ((cur.get("PrivateNote") or "").strip()
                               + f" Re-created as an Expense (PaymentType {paytype}); "
                                 f"replaces Check {pid}, which is to be deleted.")
        print(f"  Purchase {pid}  {cur.get('DocNumber')}  {cur.get('TxnDate')}  "
              f"{total:,.2f}  {cur.get('PaymentType')} -> {paytype}"
              + ("" if confirm else "   (dry run)"))
        if not confirm:
            continue
        res_p = qbo.post("purchase", body)
        after = round(float(res_p.get("TotalAmt") or 0), 2)
        if abs(after - total) > 0.005 or subtotals(res_p.get("Line", [])) != sub:
            sys.exit(f"ABORT — new Purchase {res_p.get('Id')} does not reproduce {pid}: "
                     f"{after:,.2f} vs {total:,.2f}. Delete nothing; investigate.")
        made.append((pid, res_p))
        print(f"    -> Purchase {res_p['Id']}  {res_p.get('PaymentType')}  {after:,.2f}  "
              f"({len(res_p.get('Line', []))} lines, subtotals match)")
    if not confirm:
        print("\nDRY RUN — nothing written. Re-run with --confirm.")
        return
    if made:
        print(f"\ncreated {len(made)} expense(s). Now delete the old Check(s) in QuickBooks:")
        for pid, new_p in made:
            print(f"  delete Check {pid}   (replaced by Expense {new_p['Id']} "
                  f"{new_p.get('DocNumber')})")
        print("\nUntil then the amount is on the books twice.")


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--ids", default="", help="comma-separated Bill Ids")
    ap.add_argument("--clone", default=None, metavar="PURCHASE_IDS",
                    help="re-create these Purchases under --paytype (the Bills are gone)")
    ap.add_argument("--date", choices=["payment", "bill"], default="payment")
    ap.add_argument("--paytype", choices=["Cash", "Check", "CreditCard"], default="Cash",
                    help="Cash renders as an Expense (default), Check renders as a Check")
    ap.add_argument("--out", default=None)
    ap.add_argument("--verify", action="store_true",
                    help="report what exists now; creates nothing")
    ap.add_argument("--confirm", action="store_true", help="actually WRITE to QuickBooks")
    args = ap.parse_args()

    qbo = client()
    res = Resolver(qbo)

    if args.clone:
        clone(qbo, [i.strip() for i in args.clone.split(",") if i.strip()],
              args.paytype, args.confirm)
        return

    ids = [i.strip() for i in args.ids.split(",") if i.strip()]
    bills = qbo.query_all(
        f"select * from Bill where Id in ({','.join(repr(i) for i in ids)})", "Bill")
    found = {b["Id"]: b for b in bills}
    if missing := [i for i in ids if i not in found]:
        sys.exit(f"ABORT — Bill Id(s) not found (already deleted?): {', '.join(missing)}")

    existing = qbo.query_all(
        "select * from Purchase where TxnDate >= '2026-07-01'", "Purchase")
    # CONTENT key, never DocNumber alone: (paid-from account, date, amount).
    seen = {((p.get("AccountRef") or {}).get("value"), p.get("TxnDate"),
             round(float(p.get("TotalAmt") or 0), 2)) for p in existing}
    seen_doc = {p.get("DocNumber") for p in existing if p.get("DocNumber")}

    rows, plans = [], []
    month_move: dict[str, float] = defaultdict(float)
    for i in ids:
        b = found[i]
        pay = payment_of(qbo, b)
        if not pay:
            sys.exit(f"ABORT — Bill {i} has no linked Bill Payment; an unpaid Bill is not "
                     f"a paid Expense and must not be converted blind")
        bank = (pay.get("CheckPayment") or pay.get("CreditCardPayment") or {})
        bank_ref = bank.get("BankAccountRef") or {}
        bank_id = bank_ref.get("value")
        if not bank_id:
            sys.exit(f"ABORT — Bill Payment {pay['Id']} has no bank account")
        txndate = pay.get("TxnDate") if args.date == "payment" else b.get("TxnDate")
        total = round(float(b.get("TotalAmt") or 0), 2)
        paytype = args.paytype
        body = build(b, pay, txndate, bank_id, paytype)

        dup = ""
        if (bank_id, txndate, total) in seen:
            dup = f"DUPLICATE content ({txndate}, {total:,.2f} from {bank_ref.get('name')})"
        elif body["DocNumber"] in seen_doc:
            dup = f"DUPLICATE DocNumber {body['DocNumber']}"

        sub = subtotals(b.get("Line", []))
        shown = display_subtotals(b.get("Line", []))
        owner = round(sum(a for (ac, _), a in shown.items() if ac.startswith(OWNER_PREFIX)), 2)
        if owner and (b.get("TxnDate") or "")[:7] != (txndate or "")[:7]:
            month_move[f"{(b.get('TxnDate') or '')[:7]} -> {txndate[:7]}"] += owner

        print(f"\nBill {i}  {b.get('DocNumber')}  {b.get('TxnDate')}  {total:,.2f}"
              + (f"   *** {dup} ***" if dup else ""))
        print(f"  payment {pay['Id']} {pay.get('TxnDate')} from {bank_ref.get('name')} "
              f"({paytype})")
        print(f"  -> Expense {body['DocNumber']}  dated {txndate}  "
              f"{len(body['Line'])} lines  {total:,.2f}")
        for (ac, cl), amt in sorted(shown.items()):
            print(f"       {amt:>9,.2f}  {ac.rsplit(':', 1)[-1]:<28} {cl}")
        if owner:
            print(f"     owner-borne on this bill: {owner:,.2f}"
                  + (f"   MOVES {(b.get('TxnDate') or '')[:7]} -> {txndate[:7]}"
                     if (b.get('TxnDate') or '')[:7] != txndate[:7] else "   (same month)"))

        for ln in body["Line"]:
            d = ln["AccountBasedExpenseLineDetail"]
            rows.append({"BillId": i, "BillDoc": b.get("DocNumber", ""),
                         "BillDate": b.get("TxnDate", ""), "PaymentId": pay["Id"],
                         "PaymentDate": pay.get("TxnDate", ""), "NewDocNumber": body["DocNumber"],
                         "NewTxnDate": txndate, "PaidFrom": bank_ref.get("name", ""),
                         "LineNum": ln["LineNum"], "Amount": f"{ln['Amount']:.2f}",
                         "Account": (d.get("AccountRef") or {}).get("name", ""),
                         "Class": (d.get("ClassRef") or {}).get("name", ""),
                         "Description": (ln.get("Description") or "").replace("\n", " "),
                         "Duplicate": dup})
        plans.append((b, body, sub, total, dup))

    if args.verify:
        print("\n--verify: nothing created. Bills above still exist; delete each Bill AND its "
              "Bill Payment in the UI once the replacement Expense is on the books.")
        return

    out = Path(args.out) if args.out else review_csv("bill_to_expense")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    grand = sum(t for _, _, _, t, _ in plans)
    print(f"\n{len(plans)} expense(s), {len(rows)} lines, ${grand:,.2f} -> {out}")
    if month_move:
        print("\n  OWNER-BORNE COST CHANGING MONTH under --date "
              f"{args.date}:")
        for k, v in sorted(month_move.items()):
            print(f"    {k}   {v:,.2f}")
        print("    Those lines sit on 1C - Owner Expenses and land on an owner statement,")
        print("    so a statement already issued for either month would no longer reproduce.")
    else:
        print(f"\n  no owner-borne cost changes month under --date {args.date}.")

    if not args.confirm:
        print("\nDRY RUN — nothing written. Review the CSV, then re-run with --confirm.")
        return

    print(f"\ncreating {len(plans)} expense(s) …")
    made = []
    for b, body, sub, total, dup in plans:
        if dup:
            print(f"  SKIP Bill {b['Id']}: {dup}")
            continue
        got = qbo.post("purchase", body)
        after = round(float(got.get("TotalAmt") or 0), 2)
        if abs(after - total) > 0.005:
            sys.exit(f"ABORT — Purchase {got.get('Id')}: posted {after:,.2f}, "
                     f"expected {total:,.2f}")
        if subtotals(got.get("Line", [])) != sub:
            sys.exit(f"ABORT — Purchase {got.get('Id')}: account/class subtotals do not "
                     f"match Bill {b['Id']}. The HOA split did not survive; investigate "
                     f"before deleting anything.")
        made.append((b, got))
        print(f"  Expense {got['Id']}  {got.get('DocNumber')}  {got.get('TxnDate')}  "
              f"{after:,.2f}  ({len(got.get('Line', []))} lines, subtotals match)")

    print(f"\ncreated {len(made)} expense(s).  The amounts are now on the books TWICE.")
    print("Delete these in QuickBooks to finish — the Bill Payment first, then the Bill:")
    for b, got in made:
        pay = payment_of(qbo, b)
        print(f"  Bill Payment {pay['Id'] if pay else '?':<8} {b.get('TxnDate')}  "
              f"then Bill {b['Id']:<8} {b.get('DocNumber')}")
    print("\nDeleting a Bill Payment releases its bank-feed line back to For Review; "
          "match it to the new Expense there.")


if __name__ == "__main__":
    main()
