"""Net an owner-charge Bill to $0.00 by adding the credit it was missing.

    python -m src.fixes.bill_offset_ap --ids 116480,116481 \
        --charge "...:Cleaning Expense - Owner" --credit "Cleaning Fee Revenue:Cleaning Payout"
    ...  --confirm                                                              # WRITES

## The problem this fixes

An owner-charge Bill debits an owner-payable account -- charging the owner for a cost Valta
laid out -- and its CREDIT ought to relieve whatever account carries that cost.  Some of these
Bills carry the charge line only, so the credit fell to `APAccountRef` by default and the Bill
sits open forever: a payable to a vendor who was already paid another way, or (worse) to a
person who is not a vendor at all but the property's OWNER.

**The credit cannot be repointed on the object.**  A Bill's credit leg IS `APAccountRef`, and
QBO requires that to be an Accounts Payable account -- `Cleaning Payout` (1534) is
Income/ServiceFeeIncome, so the write is refused.  The credit has to become a LINE.

That is the house pattern, not a workaround: every `Valta Realty - *` fee rebill is a $0.00
Bill carrying both legs as lines, one negative and one positive (Bill 119704: -396.00 on
Management Commissions Revenue against +396.00 on 1A - Net Earnings), and 4,806 Bills since
2026-06 carry a negative line.  A NEGATIVE Bill line credits its account -- the same convention
`Owner_statement_whole`'s Bill ingest relies on, where it stores `-amount` and warns never to
use `-abs(amount)` because the sign is the only thing separating a charge from a credit.

So: one negative line per CLASS, on the credit account, for what that class's charge lines
total.  Per class rather than per line because the class is what reporting keys on, and per
class rather than one lump because a Bill can span several.

## What it refuses

  * a Bill already netting to 0.00 -- nothing to do, and a second offset would invert it;
  * a Bill with no charge-account line, or carrying a line on the credit account already;
  * any Bill whose `Balance` does not equal its `TotalAmt` -- that means a payment has been
    applied, so the payable was real and settling it this way would contradict the cash.

## What survives

Verified around every write: `DocNumber`, vendor, `APAccountRef`, `TxnDate`, `DueDate`,
`LinkedTxn`, and every ORIGINAL line's account / class / customer / amount / description,
byte-identical.  Only new lines are added.  Afterwards the object is re-read and must show
`TotalAmt` 0.00 and `Balance` 0.00.

A Bill is FULL-updated by reading it back and echoing it whole -- never hand-built.  QBO blanks
every field the payload omits, and a Bill carries `APAccountRef`, `DocNumber`, `DueDate`,
`LinkedTxn` and per-line `CustomerRef`/`ClassRef` that are invisible until they are gone.

**The owner is still charged.**  The charge lines are untouched, and the credit account is
outside the owner-statement gate while the charge account is inside it, so statements do not
move -- the credit is Valta relieving its own cost, which was the point.
"""
from __future__ import annotations

import argparse
import copy
import sys
from collections import defaultdict

from ..config import client
from ..resolver import Resolver, esc


def fingerprint(bill: dict, n_lines: int) -> tuple:
    """Everything that must NOT change.  Only the first n_lines are the original ones."""
    lines = []
    for ln in bill.get("Line", [])[:n_lines]:
        d = ln.get("AccountBasedExpenseLineDetail") or {}
        lines.append((round(float(ln.get("Amount") or 0), 2), ln.get("Description"),
                      (d.get("AccountRef") or {}).get("value"),
                      (d.get("ClassRef") or {}).get("value"),
                      (d.get("CustomerRef") or {}).get("value")))
    return ((bill.get("DocNumber"), (bill.get("VendorRef") or {}).get("value"),
             (bill.get("APAccountRef") or {}).get("value"), bill.get("TxnDate"),
             bill.get("DueDate"),
             tuple(sorted((t.get("TxnId"), t.get("TxnType")) for t in bill.get("LinkedTxn", []))),
             tuple(lines)))


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.fixes.bill_offset_ap")
    ap.add_argument("--ids", required=True, help="comma-separated Bill Ids")
    ap.add_argument("--charge", required=True, help="the owner-charge account (full name)")
    ap.add_argument("--credit", required=True, help="the account the credit line lands on")
    ap.add_argument("--note", default="offset to {credit}: cost already borne by Valta",
                    help="description for each credit line ({credit} = the leaf account name)")
    ap.add_argument("--confirm", action="store_true", help="actually WRITE")
    args = ap.parse_args()

    qbo = client()
    res = Resolver(qbo)
    charge_id, credit_id = res.account(args.charge), res.account(args.credit)
    for label, name, val in (("--charge", args.charge, charge_id), ("--credit", args.credit, credit_id)):
        if val is None:
            sys.exit(f"{label}: account not found: {name!r}")
    if charge_id == credit_id:
        sys.exit("--charge and --credit are the same account; that offsets nothing.")
    note = args.note.replace("{credit}", args.credit.rsplit(":", 1)[-1])

    ok, skip = [], []
    for bid in [i.strip() for i in args.ids.split(",") if i.strip()]:
        got = qbo.query(f"SELECT * FROM Bill WHERE Id = '{esc(bid)}'") \
            .get("QueryResponse", {}).get("Bill", [])
        if not got:
            skip.append((bid, "not found")); continue
        b = got[0]
        total = round(float(b.get("TotalAmt") or 0), 2)
        bal = round(float(b.get("Balance") or 0), 2)
        if abs(total) < 0.005:
            skip.append((bid, "already nets to 0.00")); continue
        if abs(bal - total) > 0.005:
            skip.append((bid, f"Balance {bal:,.2f} != TotalAmt {total:,.2f} -- a payment has been "
                              f"applied, so this payable was real")); continue
        per = defaultdict(float)
        other = 0.0
        for ln in b.get("Line", []):
            d = ln.get("AccountBasedExpenseLineDetail") or {}
            if not d:
                continue
            amt = round(float(ln.get("Amount") or 0), 2)
            a = (d.get("AccountRef") or {}).get("value")
            if a == credit_id:
                other = None; break
            if a == charge_id:
                per[(d.get("ClassRef") or {}).get("value")] = round(
                    per[(d.get("ClassRef") or {}).get("value")] + amt, 2)
            else:
                other = round(other + amt, 2)
        if other is None:
            skip.append((bid, f"already carries a line on {args.credit.rsplit(':', 1)[-1]}")); continue
        if not per:
            skip.append((bid, f"no line on {args.charge.rsplit(':', 1)[-1]}")); continue
        if other:
            skip.append((bid, f"{other:,.2f} sits on other accounts; offsetting would not "
                              f"net the Bill to 0.00")); continue
        ok.append((b, dict(per), total))

    print(f"{'Bill':<8}{'vendor':<24}{'date':<12}{'total':>9}  classes -> credit line(s)")
    for b, per, total in ok:
        print(f"{b['Id']:<8}{((b.get('VendorRef') or {}).get('name') or '')[:22]:<24}"
              f"{b['TxnDate']:<12}{total:>9,.2f}  "
              + ", ".join(f"{-v:,.2f}" for v in per.values()))
    for bid, why in skip:
        print(f"{bid:<8}SKIP -- {why}")
    print(f"\n{len(ok)} Bill(s) to net to 0.00, {round(sum(t for _, _, t in ok), 2):,.2f} of "
          f"phantom A/P cleared; {len(skip)} skipped")
    if not args.confirm:
        print("\nDRY RUN -- nothing written.  Re-run with --confirm to WRITE.")
        return

    print()
    failed = []
    for b, per, total in ok:
        n = len(b.get("Line", []))
        want = fingerprint(b, n)
        body = copy.deepcopy(b)
        for cls, amt in per.items():
            det = {"AccountRef": {"value": credit_id}, "BillableStatus": "NotBillable",
                   "TaxCodeRef": {"value": "NON"}}
            if cls:
                det["ClassRef"] = {"value": cls}
            body["Line"].append({"DetailType": "AccountBasedExpenseLineDetail",
                                 "Amount": round(-amt, 2), "Description": note[:4000],
                                 "AccountBasedExpenseLineDetail": det})
        body["sparse"] = False
        try:
            qbo.post("bill", body)
        except Exception as exc:                                     # noqa: BLE001
            print(f"   {b['Id']}  FAILED: {str(exc)[:220]}")
            failed.append(b["Id"]); continue
        back = qbo.query(f"SELECT * FROM Bill WHERE Id = '{esc(b['Id'])}'") \
            .get("QueryResponse", {}).get("Bill", [])[0]
        t2 = round(float(back.get("TotalAmt") or 0), 2)
        b2 = round(float(back.get("Balance") or 0), 2)
        if fingerprint(back, n) != want:
            print(f"   {b['Id']}  *** something other than the new line(s) changed ***")
            failed.append(b["Id"]); continue
        if abs(t2) > 0.005 or abs(b2) > 0.005:
            print(f"   {b['Id']}  *** total {t2:,.2f} balance {b2:,.2f}, expected 0.00 ***")
            failed.append(b["Id"]); continue
        print(f"   {b['Id']}  {((b.get('VendorRef') or {}).get('name') or '')[:22]:<24}"
              f"{total:>9,.2f} -> 0.00, balance 0.00, {n} lines + {len(per)}")
    if failed:
        sys.exit(f"\n{len(failed)} Bill(s) did not land: {', '.join(failed)}")
    print(f"\n{len(ok)} Bill(s) netted to 0.00.  "
          f"{round(sum(t for _, _, t in ok), 2):,.2f} of phantom A/P cleared.")


if __name__ == "__main__":
    main()
