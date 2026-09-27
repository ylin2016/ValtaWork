"""Put a cleaner's payout lines on the listing class that bears them.

Some cleaners' payouts sit on the flat `Valta Realty` class, so no listing carries their
cleaning cost.  Where the cleaner works a SINGLE property the fix is a class repoint on the
expense line itself -- no journal entry, because there is nothing to spread.  (A cleaner
covering many properties from one lump payment needs a JE instead; that is what
`je/build_maria_cleaning` does.)

    python -m src.fixes.reclass_cleaning_payout --payee "Brittany Perry" \
        --to "Listings:OSBR" --year 2026                    # dry run + review CSV
    ... --confirm                                            # write

**Only lines on a cleaning-payout account whose class is NOT already under `Listings:` are
touched.** That restraint is not cosmetic here.  Brittany Perry is a cleaner who is also a
TENANT: every payment nets her cleaning against the rent she owes, so one Purchase carries

    + cleaning payout        class Valta Realty      <- the only line this script moves
    - Monthly Rent           class Listings:OSBR 8   <- her unit; already right
    - Security Deposits Pay. class Listings:OSBR 8   <- likewise

A blanket reclass of "her lines" would move the rent offsets off the unit that owes them and
break the netting.  `Wages - Cohost/PM` lines are left alone too: they are wages, not
cleaning, and share only the flat class.

**The property class and the unit class are different classes.** Her rent belongs to
`Listings:OSBR 8` because that is where she lives; her cleaning covers the property, so it
goes to `Listings:OSBR` -- which is what every other OSBR cleaner already uses (Olga,
Guadalupe, Carol Koch, all Id 2100000000001547055).  Pass `--to` explicitly; nothing here
guesses a class from a payee.

Only `ClassRef` moves.  The Purchase is read back and echoed whole with `sparse` false, and
the run aborts on the first TotalAmt that shifts.  Idempotent -- a line already under
`Listings:` is skipped, so a re-run touches only what is new.
"""
from __future__ import annotations

import argparse
import csv
import json
import sys
from pathlib import Path

from ..config import client
from ..paths import review_csv
from ..resolver import Resolver

CLEANING_LEAVES = (":Cleaning Payout", ":Destiny's cleaning")


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.fixes.reclass_cleaning_payout")
    ap.add_argument("--payee", required=True, help="exact vendor DisplayName (case-insensitive)")
    ap.add_argument("--to", required=True, dest="to_class",
                    help="target class, e.g. 'Listings:OSBR' -- never inferred")
    ap.add_argument("--year", default=None, help="restrict to one calendar year; default all")
    ap.add_argument("--out", default=None)
    ap.add_argument("--confirm", action="store_true", help="WRITE (default is a dry run)")
    args = ap.parse_args()

    qbo = client()
    res = Resolver(qbo)
    target_id = res.klass(args.to_class) if hasattr(res, "klass") else None
    fqn = {c["Id"]: c.get("FullyQualifiedName", "")
           for c in qbo.query_all("SELECT * FROM Class MAXRESULTS 500", "Class")}
    if target_id is None:
        hit = [cid for cid, f in fqn.items() if f == args.to_class]
        if len(hit) != 1:
            sys.exit(f"class {args.to_class!r} resolved to {len(hit)} classes — refusing to guess")
        target_id = hit[0]
    print(f"target class: {args.to_class} -> Id {target_id}")

    start = f"{args.year}-01-01" if args.year else "2024-01-01"
    end = f"{args.year}-12-31" if args.year else "2027-12-31"
    found, edits, rows, skipped = {}, {}, [], []
    q = (f"SELECT * FROM Purchase WHERE TxnDate >= '{start}' AND TxnDate <= '{end}' "
         f"MAXRESULTS 1000")
    for p in qbo.query_all(q, "Purchase"):
        if (p.get("EntityRef") or {}).get("name", "").casefold() != args.payee.casefold():
            continue
        per = []
        for n, ln in enumerate(p.get("Line", [])):
            det = ln.get("AccountBasedExpenseLineDetail")
            if not det:
                continue
            acct = (det.get("AccountRef") or {}).get("name", "")
            cid = (det.get("ClassRef") or {}).get("value")
            cur = fqn.get(cid, "") if cid else ""
            amt = float(ln.get("Amount") or 0)
            if not acct.endswith(CLEANING_LEAVES):
                skipped.append((p["TxnDate"], p["Id"], amt, acct.rsplit(":", 1)[-1], cur,
                                "not a cleaning-payout account"))
                continue
            if cur.startswith("Listings:"):
                skipped.append((p["TxnDate"], p["Id"], amt, acct.rsplit(":", 1)[-1], cur,
                                "already on a listing"))
                continue
            per.append(n)
            rows.append({"Id": p["Id"], "TxnDate": p["TxnDate"], "Payee": args.payee,
                         "Amount": f"{amt:.2f}", "Account": acct,
                         "FromClass": cur or "(none)", "ToClass": args.to_class,
                         "Description": (ln.get("Description") or "").replace("\n", " ")})
        if per:
            found[p["Id"]] = p
            edits[p["Id"]] = per

    if skipped:
        print(f"\nleft alone ({len(skipped)} line(s)) — these are why this script is narrow:")
        for s in sorted(skipped)[:12]:
            print(f"   {s[0]}  P{s[1]:<8} {s[2]:>9,.2f}  {s[3]:<26} cls={s[4] or '(none)':<18} {s[5]}")
        if len(skipped) > 12:
            print(f"   … and {len(skipped) - 12} more")

    if not rows:
        print("\nnothing to change.")
        return

    out = Path(args.out) if args.out else review_csv(
        f"reclass_cleaning_{args.payee.split()[0].lower()}{'_' + args.year if args.year else ''}")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)

    print(f"\n{len(rows)} line(s) on {len(edits)} purchase(s) -> {out}\n")
    for r in sorted(rows, key=lambda x: x["TxnDate"]):
        print(f"  {r['TxnDate']}  P{r['Id']:<8} {float(r['Amount']):>9,.2f}  "
              f"{r['FromClass']} -> {r['ToClass']}")
        print(f"  {'':<12} {r['Description'][:66]}")
    print(f"\n  TOTAL {sum(float(r['Amount']) for r in rows):,.2f}")

    if not args.confirm:
        print("\nDRY RUN — nothing written. Review the CSV, then re-run with --confirm.")
        return

    print(f"\nupdating {len(edits)} purchase(s) …")
    for pid, per in edits.items():
        p = found[pid]
        body = json.loads(json.dumps(p))
        for n in per:
            body["Line"][n]["AccountBasedExpenseLineDetail"]["ClassRef"] = {"value": target_id}
        body["sparse"] = False
        before = round(float(p.get("TotalAmt") or 0), 2)
        got = qbo.post("purchase", body)
        after = round(float(got.get("TotalAmt") or 0), 2)
        if abs(before - after) > 0.005:
            sys.exit(f"ABORT — Purchase {pid}: total changed {before:,.2f} -> {after:,.2f}")
        print(f"  Purchase {pid}  {len(per)} line(s), total unchanged at {after:,.2f}")
    print(f"\nupdated {len(edits)} purchase(s)")


if __name__ == "__main__":
    main()
