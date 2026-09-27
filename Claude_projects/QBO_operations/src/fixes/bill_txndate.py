"""Move a batch of Bills to a different TxnDate, changing nothing else.

The September owner payout was booked on the dates the ACH left the bank (2026-09-15/16),
but the payable it settles belongs to the August statement period.  Re-dating the BILL puts
the distribution in the right month.

**The BillPayment is deliberately not touched.**  It carries its own date, it is the object
the bank feed matches (CLAUDE.md, "The monthly owner payout"), and moving it would break a
match that is already made.  A Bill dated before its payment is ordinary; the reverse is not,
so this refuses a target date AFTER any linked payment rather than creating one.

A Bill is FULL-updated by reading it back and echoing it with one field changed.  Never
hand-build the payload: QBO blanks every field omitted, and a Bill carries `APAccountRef`,
`DocNumber`, `DueDate`, `LinkedTxn` and per-line `CustomerRef`/`ClassRef` that are invisible
until they are gone.  Every one is re-read afterwards and checked against what it was.

`DueDate` moves only with `--due`.  Left alone, a bill dated 08-31 and due 09-15 reads
correctly -- raised at month end, paid mid-month -- and the due date is not what any report
here groups by.

    python -m src.fixes.bill_txndate --from 2026-09-01 --to 2026-09-30 --date 2026-08-31
    python -m src.fixes.bill_txndate ... --confirm                       # write
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import json
import sys
from collections import defaultdict
from pathlib import Path

from ..config import acct_name, client
from ..paths import review_csv
from ..resolver import Resolver


def bill_accounts(b: dict) -> set[str]:
    return {(l.get("AccountBasedExpenseLineDetail") or {}).get("AccountRef", {}).get("name", "")
            for l in b.get("Line", [])}


def fingerprint(b: dict) -> tuple:
    """What must survive the re-date: totals, AP account, doc number, links, and every
    line's account / class / customer / amount."""
    return (
        round(float(b.get("TotalAmt") or 0), 2),
        round(float(b.get("Balance") or 0), 2),
        b.get("APAccountRef", {}).get("value"),
        b.get("DocNumber"),
        b.get("VendorRef", {}).get("value"),
        tuple(sorted((t.get("TxnId"), t.get("TxnType")) for t in b.get("LinkedTxn", []))),
        tuple((round(float(l.get("Amount") or 0), 2),
               (l.get("AccountBasedExpenseLineDetail") or {}).get("AccountRef", {}).get("value"),
               ((l.get("AccountBasedExpenseLineDetail") or {}).get("ClassRef") or {}).get("value"),
               ((l.get("AccountBasedExpenseLineDetail") or {}).get("CustomerRef") or {}).get("value"))
              for l in b.get("Line", [])),
    )


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--from", dest="frm", required=True, help="select Bills from this TxnDate")
    ap.add_argument("--to", dest="to", required=True, help="... to this TxnDate")
    ap.add_argument("--date", required=True, help="the new TxnDate, e.g. 2026-08-31")
    ap.add_argument("--account", default=None,
                    help="only Bills touching this account (default: owner distributions)")
    ap.add_argument("--created-after", default=None, help="only Bills created on/after this date")
    ap.add_argument("--due", action="store_true", help="move DueDate to the same date")
    ap.add_argument("--out", default=None)
    ap.add_argument("--confirm", action="store_true", help="actually WRITE to QuickBooks")
    args = ap.parse_args()

    try:
        new_date = dt.date.fromisoformat(args.date)
    except ValueError:
        sys.exit(f"ABORT — --date {args.date!r} is not YYYY-MM-DD")

    qbo = client()
    acct = args.account or acct_name("owner_distributions")
    print(f"selecting Bills {args.frm} .. {args.to} touching:\n  {acct}")

    bills = qbo.query_all(
        f"SELECT * FROM Bill WHERE TxnDate >= '{args.frm}' AND TxnDate <= '{args.to}'", "Bill")
    picked = [b for b in bills if any(a.endswith(acct.rsplit(":", 1)[-1]) for a in bill_accounts(b))]
    if args.created_after:
        picked = [b for b in picked
                  if (b.get("MetaData", {}).get("CreateTime", "")[:10] or "") >= args.created_after]
    if not picked:
        sys.exit("no Bills matched")

    # A payment BEFORE its bill is not a thing; refuse rather than create one.
    ids = {b["Id"] for b in picked}
    pays: dict[str, list[dict]] = defaultdict(list)
    for bp in qbo.query_all(
            f"SELECT * FROM BillPayment WHERE TxnDate >= '{args.date}'", "BillPayment"):
        for l in bp.get("Line", []):
            for t in l.get("LinkedTxn", []):
                if t.get("TxnId") in ids:
                    pays[t["TxnId"]].append(bp)

    rows, jobs, bad = [], [], []
    by_old = defaultdict(lambda: [0, 0.0])
    for b in picked:
        old = b.get("TxnDate")
        by_old[old][0] += 1
        by_old[old][1] += float(b.get("TotalAmt") or 0)
        earliest = min((p["TxnDate"] for p in pays.get(b["Id"], [])), default=None)
        if earliest and earliest < args.date:
            bad.append(f"Bill {b['Id']} {b.get('DocNumber')}: payment {earliest} "
                       f"is BEFORE the new bill date {args.date}")
        body = json.loads(json.dumps(b))
        body["TxnDate"] = args.date
        if args.due:
            body["DueDate"] = args.date
        rows.append({"BillId": b["Id"], "DocNumber": b.get("DocNumber") or "",
                     "Vendor": b.get("VendorRef", {}).get("name", ""),
                     "OldTxnDate": old, "NewTxnDate": args.date,
                     "DueDate": body.get("DueDate") or "",
                     "DueMoved": "yes" if args.due else "no",
                     "TotalAmt": f"{float(b.get('TotalAmt') or 0):.2f}",
                     "Balance": f"{float(b.get('Balance') or 0):.2f}",
                     "Payments": ",".join(sorted(p["TxnDate"] for p in pays.get(b["Id"], []))),
                     "Lines": len(b.get("Line", []))})
        if old != args.date:
            jobs.append((b, body, fingerprint(b)))

    print(f"\n{len(picked)} Bill(s) selected:")
    for d in sorted(by_old):
        print(f"   {d}  {by_old[d][0]:>3} bills  ${by_old[d][1]:>13,.2f}  ->  {args.date}")
    print(f"   {'TOTAL':<10} {len(picked):>3} bills  "
          f"${sum(float(b.get('TotalAmt') or 0) for b in picked):>13,.2f}")
    print(f"\n   DueDate: {'moved to ' + args.date if args.due else 'LEFT AS IS'}")
    print(f"   BillPayments: untouched ({sum(len(v) for v in pays.values())} linked)")

    if bad:
        print(f"\n*** {len(bad)} problem(s) ***")
        for m in bad:
            print(f"   {m}")
        sys.exit("ABORT — a bill would be dated after its own payment")

    out = Path(args.out) if args.out else review_csv("bill_txndate")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    print(f"\n{len(jobs)} Bill(s) to re-date, {len(rows)} row(s) -> {out}")

    if not jobs:
        print("\nalready on that date — nothing to write.")
        return
    if not args.confirm:
        print("\nDRY RUN — nothing written. Review the CSV, then re-run with --confirm.")
        return

    done = 0
    for b, body, before in jobs:
        got = qbo.post("bill", body)
        if got.get("TxnDate") != args.date:
            sys.exit(f"ABORT — Bill {b['Id']}: TxnDate is {got.get('TxnDate')}, "
                     f"expected {args.date}")
        if fingerprint(got) != before:
            sys.exit(f"ABORT — Bill {b['Id']}: something other than the date changed")
        done += 1
        if done <= 5 or done % 20 == 0:
            print(f"  Bill {got['Id']:<8} {got.get('DocNumber','') :<28} "
                  f"{got['TxnDate']}  ${float(got['TotalAmt']):>11,.2f}  "
                  f"balance {float(got.get('Balance') or 0):,.2f}")
    print(f"\nre-dated {done} Bill(s) to {args.date}; totals, links and lines all unchanged")


if __name__ == "__main__":
    main()
