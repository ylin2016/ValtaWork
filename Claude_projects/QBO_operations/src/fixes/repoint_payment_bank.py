"""Move a BillPayment (Check) onto the bank account the money actually left.

    python -m src.fixes.repoint_payment_bank --ids 118819,118821 --to "Chase Trust Checking 3038 - monthly"
    python -m src.fixes.repoint_payment_bank --ids 118819,118821 --to "..." --confirm   # WRITES

**The bank account on a payout payment is a guess until the feed confirms it, and the
owner is the only one who can confirm it.**  `Valta Beachwood LLC`'s ACH came out of
Chase Trust 9967 - STR through 2026-05, moved to 3038 - monthly for June-August, and
went back to 9967 in September.  The register is therefore not evidence for the current
month -- and the payout sheet does not carry the account at all, so `build_owner_payout`
writes its default (9967) into every row's `PaidFrom`.  When the debit turns up in the
OTHER feed, the payments are right in every respect except the one that decides whether
the bank line can ever match them.

Only `CheckPayment.BankAccountRef` changes.  The Bill it pays, the vendor, the date, the
amount and the A/P account are untouched, so nothing moves between owners or periods --
this is a cash-location fix, not an accounting one.

Unlike a Purchase's `PaymentType`, which is final and lies about it (see
`fixes/bill_to_expense`), a BillPayment's bank account DOES take a full update.  The run
proves it rather than trusting it: every object is read back after the write and the run
aborts unless the new bank, the TotalAmt and the LinkedTxn Bill Id all come back as
expected.

The payment is read back and echoed whole.  QBO full-updates blank every field the
payload omits, and a BillPayment carries `VendorRef`, `APAccountRef`, `PayType`,
`DepartmentRef`, `CheckPayment.PrintStatus` and the per-line `LinkedTxn` that binds it to
its Bill -- lose that last one and the Bill silently reopens as unpaid.
"""
from __future__ import annotations

import argparse
import csv
import json
import sys

from ..config import client
from ..paths import review_csv
from ..resolver import Resolver

FIELDS = ["Id", "TxnDate", "Vendor", "Amount", "FromBank", "ToBank", "PaysBill", "Status"]


def repoint(pay: dict, bank_id: str) -> dict:
    """The whole object back, with one field changed."""
    body = json.loads(json.dumps(pay))          # never mutate what we verify against
    body["CheckPayment"] = dict(body.get("CheckPayment") or {})
    body["CheckPayment"]["BankAccountRef"] = {"value": bank_id}
    body["sparse"] = False
    return body


def linked_bills(pay: dict) -> list[str]:
    out = []
    for l in pay.get("Line", []):
        for t in l.get("LinkedTxn", []):
            if t.get("TxnType") == "Bill":
                out.append(t.get("TxnId"))
    return out


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--ids", required=True, help="comma-separated BillPayment Ids")
    ap.add_argument("--to", required=True, help="bank account name the money actually left")
    ap.add_argument("--confirm", action="store_true", help="actually WRITE")
    ap.add_argument("--out", default=None)
    args = ap.parse_args()

    qbo = client()
    res = Resolver(qbo)
    bank_id = res.account(args.to)
    if bank_id is None:
        sys.exit(f"bank account not found: {args.to!r}")

    ids = [i.strip() for i in args.ids.split(",") if i.strip()]
    out = args.out or str(review_csv("repoint_payment_bank"))
    rows, failures = [], []

    for i in ids:
        got = qbo.query(f"SELECT * FROM BillPayment WHERE Id = '{i}'") \
                 .get("QueryResponse", {}).get("BillPayment", [])
        if not got:
            failures.append(f"{i}: not found"); continue
        pay = got[0]
        if pay.get("PayType") != "Check":
            failures.append(f"{i}: PayType is {pay.get('PayType')!r}, not Check "
                            f"-- only a Check payment carries CheckPayment.BankAccountRef")
            continue
        cur = (pay.get("CheckPayment") or {}).get("BankAccountRef") or {}
        bills = linked_bills(pay)
        row = {
            "Id": i, "TxnDate": pay["TxnDate"],
            "Vendor": (pay.get("VendorRef") or {}).get("name", ""),
            "Amount": f"{float(pay.get('TotalAmt') or 0):.2f}",
            "FromBank": cur.get("name", ""), "ToBank": args.to,
            "PaysBill": ",".join(bills),
        }
        if cur.get("value") == bank_id:
            row["Status"] = "already on that bank -- nothing to do"
            rows.append(row); continue
        if not bills:
            failures.append(f"{i}: pays no Bill; refusing to rewrite an unlinked payment")
            continue
        if not args.confirm:
            row["Status"] = "ready"
            rows.append(row); continue

        qbo.post("billpayment", repoint(pay, bank_id))
        back = qbo.query(f"SELECT * FROM BillPayment WHERE Id = '{i}'") \
                  .get("QueryResponse", {}).get("BillPayment", [])[0]
        nb = (back.get("CheckPayment") or {}).get("BankAccountRef") or {}
        same_total = abs(float(back.get("TotalAmt") or 0) - float(pay.get("TotalAmt") or 0)) < 0.005
        # A sparse update returns 200 with the old value intact -- read back, never assume.
        if nb.get("value") != bank_id or not same_total or linked_bills(back) != bills:
            sys.exit(f"{i}: read-back does not agree -- bank={nb.get('name')!r} "
                     f"total={back.get('TotalAmt')} bills={linked_bills(back)}; "
                     f"STOPPING before the rest are touched")
        row["FromBank"] = cur.get("name", "")
        row["ToBank"] = nb.get("name", "")
        row["Status"] = "moved"
        rows.append(row)

    with open(out, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=FIELDS)
        w.writeheader(); w.writerows(rows)

    for r in rows:
        print(f"  {r['Status']:34} {r['Id']:>7} {r['TxnDate']} {r['Vendor']:24} "
              f"{float(r['Amount']):>11,.2f}  {r['FromBank']} -> {r['ToBank']}")
    total = sum(float(r["Amount"]) for r in rows if r["Status"] in ("ready", "moved"))
    print(f"\n{len(rows)} payment(s), {total:,.2f} -> {out}")
    if not args.confirm:
        print("DRY RUN -- nothing written. Re-run with --confirm to WRITE.")
    if failures:
        sys.exit("\n".join(["refused:"] + [f"  {x}" for x in failures]))


if __name__ == "__main__":
    main()
