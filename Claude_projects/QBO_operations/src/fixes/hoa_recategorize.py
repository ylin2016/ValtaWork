"""Repoint a purchase the HOA bears onto `HOA Receivable - Yacinde`.

Cleaning arrives on one AA Professional Cleaners bill and is split by the allocation
workbook (`fixes/yacinde_hoa_split`).  Everything else the HOA bears -- pool chemicals, a
pool hook, common-area maintenance -- arrives one Amazon purchase at a time, categorised
to an owner expense account by whoever entered it.  That charges the Yacinde owners for a
cost the HOA reimburses:

    from   1C - Owner Expenses:Maintenance - Owner     class = the listing
    to     HOA Receivable - Yacinde                    class = Yacinde HOA

The receivable is an ASSET until the HOA is invoiced and pays; the owner payable is not
the HOA's to charge.  `invoices/hoa_maintenance` then bills whatever sits here, so this
step is what makes a cost billable at all -- and skipping it while invoicing anyway would
credit an asset that was never debited, driving the receivable negative.

**Which purchases the HOA bears is never inferred, and `pool` in a description is not a
rule.** The Ids come from the owner, exactly as the workbook decides which cleans are the
HOA's.  A guess here moves real money off an owner's statement.

    python -m src.fixes.hoa_recategorize --ids 117982,117983            # dry run + CSV
    python -m src.fixes.hoa_recategorize --ids 117982,117983 --confirm  # write
    python -m src.fixes.hoa_recategorize --ids 117982 --to owner \
        --account "Trust Liabilities:Owner Payables:1C - Owner Expenses:Maintenance - Owner" \
        --class "Listings:Yacinde NuGrowth" --confirm                   # undo

The Purchase is read back and echoed whole: QBO full-updates blank every field the payload
omits, and a Purchase carries PaymentType, EntityRef, CreditCardPayment and per-line
BillableStatus that are invisible until they are gone.  The run aborts on the first
TotalAmt that moves -- only AccountRef and ClassRef change.
"""
from __future__ import annotations

import argparse
import csv
import json
import sys
from pathlib import Path

from ..config import client, name as acct_name_of
from ..paths import review_csv
from ..resolver import Resolver

HOA_CLASS = "Yacinde HOA"


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--ids", required=True, help="comma-separated Purchase Ids")
    ap.add_argument("--to", choices=["hoa", "owner"], default="hoa")
    ap.add_argument("--account", default=None, help="with --to owner: the account to restore")
    ap.add_argument("--class", dest="klass", default=None,
                    help="with --to owner: the class to restore")
    ap.add_argument("--out", default=None)
    ap.add_argument("--confirm", action="store_true", help="actually WRITE to QuickBooks")
    args = ap.parse_args()

    if args.to == "owner" and not (args.account and args.klass):
        sys.exit("ABORT — --to owner needs both --account and --class; a restore is not guessed")

    qbo = client()
    res = Resolver(qbo)
    acct = acct_name_of("hoa_receivable_yacinde") if args.to == "hoa" else args.account
    klass = HOA_CLASS if args.to == "hoa" else args.klass
    acct_id, class_id = res.account(acct), res.klass(res.klass_fqn(klass) or klass)
    print(f"target account {acct!r} -> {'Id ' + acct_id if acct_id else '*** MISSING ***'}")
    print(f"target class   {klass!r} -> {'Id ' + class_id if class_id else '*** MISSING ***'}")
    if not acct_id or not class_id:
        sys.exit("ABORT — resolve the account and class first")

    ids = [i.strip() for i in args.ids.split(",") if i.strip()]
    got = qbo.query_all(
        f"select * from Purchase where Id in ({','.join(repr(i) for i in ids)})", "Purchase")
    found = {p["Id"]: p for p in got}
    if missing := [i for i in ids if i not in found]:
        sys.exit(f"ABORT — Purchase Id(s) not found: {', '.join(missing)}")

    rows, edits = [], {}
    for i in ids:
        p = found[i]
        per = []
        for n, ln in enumerate(p.get("Line", [])):
            det = ln.get("AccountBasedExpenseLineDetail")
            if not det:
                continue
            cur_a = (det.get("AccountRef") or {}).get("name", "")
            cur_c = (det.get("ClassRef") or {}).get("name", "")
            if (det.get("AccountRef") or {}).get("value") == acct_id and \
                    (det.get("ClassRef") or {}).get("value") == class_id:
                print(f"  Purchase {i} line {n+1}: already there — skipped")
                continue
            per.append(n)
            rows.append({"Id": i, "TxnDate": p.get("TxnDate", ""), "LineNum": n + 1,
                         "Payee": (p.get("EntityRef") or {}).get("name", ""),
                         "PaidFrom": (p.get("AccountRef") or {}).get("name", ""),
                         "Amount": f"{float(ln.get('Amount') or 0):.2f}",
                         "FromAccount": cur_a, "ToAccount": acct,
                         "FromClass": cur_c, "ToClass": klass,
                         "Description": (ln.get("Description") or "").replace("\n", " ")})
        if per:
            edits[i] = per

    if not rows:
        print("\nnothing to change — already in that state.")
        return

    out = Path(args.out) if args.out else review_csv(f"hoa_recategorize_{args.to}")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)

    print(f"\n{len(rows)} line(s) on {len(edits)} purchase(s) -> {out}")
    for r in rows:
        print(f"  {r['TxnDate']}  Purchase {r['Id']:<7} {r['Amount']:>9}  "
              f"{r['FromAccount'].rsplit(':', 1)[-1]} / {r['FromClass']}")
        print(f"  {'':<11} {'':<16} {'->':>9}  {r['ToAccount'].rsplit(':', 1)[-1]} / {r['ToClass']}")
        print(f"  {'':<11} {r['Description'][:72]}")
    print(f"\n  TOTAL {sum(float(r['Amount']) for r in rows):,.2f}")

    if not args.confirm:
        print("\nDRY RUN — nothing written. Review the CSV, then re-run with --confirm.")
        return

    print(f"\nupdating {len(edits)} purchase(s) …")
    for i, per in edits.items():
        p = found[i]
        body = json.loads(json.dumps(p))
        for n in per:
            det = body["Line"][n]["AccountBasedExpenseLineDetail"]
            det["AccountRef"] = {"value": acct_id}
            det["ClassRef"] = {"value": class_id}
        body["sparse"] = False
        before = round(float(p.get("TotalAmt") or 0), 2)
        res_p = qbo.post("purchase", body)
        after = round(float(res_p.get("TotalAmt") or 0), 2)
        if abs(before - after) > 0.005:
            sys.exit(f"ABORT — Purchase {i}: total changed {before:,.2f} -> {after:,.2f}")
        print(f"  Purchase {i}  {len(per)} line(s), total unchanged at {after:,.2f}")
    print(f"\nupdated {len(edits)} purchase(s)")


if __name__ == "__main__":
    main()
