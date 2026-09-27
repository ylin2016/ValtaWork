"""Move Purchases onto the right Location, changing nothing else.

Location is a transaction-level field (`DepartmentRef`), so this is a one-field edit -- but
it is the field that decides which side of the business a cost is reported on, and it gets
set from whatever the last similar transaction did.  That is how a wrong one spreads: a note
in CLAUDE.md claimed every Booking.com commission Expense carried `Valta Realty`, which was
read off the most recent ones.  They were the anomaly -- 490 of 685 commission purchases are
`Trust` and have been since 2025-04 -- and the claim then set the default in
`invoices/build_bcom_commission`, which put 32 more on the wrong Location.

    python -m src.fixes.purchase_location --account "...Fee - Booking.com Commission" \
        --from "Valta Realty" --to Trust --start 2026-08-01          # dry run
    ... --confirm

Selection is by ACCOUNT, current Location and date, never by Id list, because the point is to
catch every one a bad default produced rather than the handful anyone remembers.  The dry run
prints them grouped by month so an unexpected month is visible before the write.

Only `DepartmentRef` moves.  The Purchase is read back and echoed whole with `sparse` false,
and a fingerprint of the total, payee, bank, payment type and every line's account / class /
amount is compared after the write -- a Location change that alters a line is not a Location
change.
"""
from __future__ import annotations

import argparse
import csv
import json
import sys
from collections import defaultdict
from pathlib import Path

from ..config import client
from ..paths import review_csv
from ..resolver import Resolver


def fingerprint(p: dict) -> tuple:
    return (round(float(p.get("TotalAmt") or 0), 2),
            (p.get("EntityRef") or {}).get("value"),
            (p.get("AccountRef") or {}).get("value"),
            p.get("PaymentType"), p.get("DocNumber"), p.get("PrivateNote"),
            tuple((round(float(l.get("Amount") or 0), 2),
                   ((l.get("AccountBasedExpenseLineDetail") or {}).get("AccountRef") or {}).get("value"),
                   ((l.get("AccountBasedExpenseLineDetail") or {}).get("ClassRef") or {}).get("value"))
                  for l in p.get("Line", [])))


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.fixes.purchase_location")
    ap.add_argument("--account", required=True, help="fully-qualified account a line must use")
    ap.add_argument("--from", dest="from_loc", required=True)
    ap.add_argument("--to", dest="to_loc", required=True)
    ap.add_argument("--start", default="2026-01-01")
    ap.add_argument("--end", default="2027-12-31")
    ap.add_argument("--out", default=None)
    ap.add_argument("--confirm", action="store_true")
    args = ap.parse_args()

    qbo = client()
    res = Resolver(qbo)
    acct_id = res.account(args.account)
    if not acct_id:
        sys.exit(f"account not found: {args.account!r}")
    to_id = res.department(args.to_loc)
    if to_id is None:
        sys.exit(f"location not found: {args.to_loc!r}")

    found, rows = {}, []
    for p in qbo.query_all(f"SELECT * FROM Purchase WHERE TxnDate >= '{args.start}' "
                           f"AND TxnDate <= '{args.end}' MAXRESULTS 1000", "Purchase"):
        if (p.get("DepartmentRef") or {}).get("name") != args.from_loc:
            continue
        amt = sum(float(l.get("Amount") or 0) for l in p.get("Line", [])
                  if ((l.get("AccountBasedExpenseLineDetail") or {}).get("AccountRef") or {}
                      ).get("value") == acct_id)
        if not amt:
            continue
        found[p["Id"]] = p
        rows.append({"Id": p["Id"], "TxnDate": p["TxnDate"], "DocNumber": p.get("DocNumber") or "",
                     "Payee": (p.get("EntityRef") or {}).get("name", ""),
                     "TotalAmt": f"{float(p.get('TotalAmt') or 0):.2f}",
                     "OnAccount": f"{amt:.2f}",
                     "FromLocation": args.from_loc, "ToLocation": args.to_loc})

    if not rows:
        print(f"nothing on {args.from_loc!r} touching that account in the window.")
        return

    per = defaultdict(lambda: [0, 0.0])
    for r in rows:
        per[r["TxnDate"][:7]][0] += 1
        per[r["TxnDate"][:7]][1] += float(r["OnAccount"])
    print(f"{len(rows)} purchase(s) on {args.from_loc!r} -> {args.to_loc!r}:")
    for m in sorted(per):
        print(f"   {m}  {per[m][0]:>3} purchases  {per[m][1]:>10,.2f}")
    print(f"   {'TOTAL':<9} {len(rows):>3} purchases  "
          f"{sum(float(r['OnAccount']) for r in rows):>10,.2f}")

    out = Path(args.out) if args.out else review_csv("purchase_location")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    print(f"\n-> {out}")

    if not args.confirm:
        print("\nDRY RUN — nothing written. Review the CSV, then re-run with --confirm.")
        return

    print(f"\nupdating {len(found)} purchase(s) …")
    bad = 0
    for pid, p in found.items():
        before = fingerprint(p)
        body = json.loads(json.dumps(p))
        body["DepartmentRef"] = {"value": to_id}
        body["sparse"] = False
        got = qbo.post("purchase", body)
        if fingerprint(got) != before:
            sys.exit(f"ABORT — Purchase {pid}: something other than the Location changed")
        if (got.get("DepartmentRef") or {}).get("value") != to_id:
            sys.exit(f"ABORT — Purchase {pid}: Location did not land on {args.to_loc!r}")
        bad += 0
    print(f"updated {len(found)} purchase(s); totals, payee, bank and every line verified unchanged")


if __name__ == "__main__":
    main()
