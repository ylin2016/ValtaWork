"""Move a balance between LOCATIONS without touching the account, the class or the total.

Location is a TRANSACTION-level field on a Purchase (`DepartmentRef`), not a per-line one.
So when one Purchase carries lines belonging to two sides of the business, no edit to that
Purchase can put them in the right places -- the transaction has exactly one Location and
something has to be wrong.  A journal entry is the only way to express it.

That is the case this exists for.  Brittany Perry is a cleaner who is also a tenant, so a
single Purchase nets her cleaning pay against the rent she owes:

    Cleaning Payout            Valta Realty   <- company money, company Location: right
    Monthly Rent (credit)      Valta Realty   <- a TRUST liability sitting on the company
    Security Deposits Payable  Valta Realty   <- likewise

Both those accounts are already trust accounts by name (`Trust Liabilities:...`), so nothing
is miscoded; only the Location is wrong, and it is wrong because the Purchase could not be
two Locations at once.

    python -m src.je.build_location_move --from "Valta Realty" --to Trust     # dry run
    python -m src.je.post --all --csv review/location_move.csv [--confirm]

The JE names ONE account on both sides of every pair, so no money moves between accounts and
no balance changes -- the same invariant `build_maria_cleaning` keeps for classes, applied to
Locations.  The class is carried through from the source lines, so per-listing reporting is
unaffected.

**Direction is derived from the balance, never assumed.**  A line's Amount on a Purchase is
debit-positive, so Brittany's rent lines are NEGATIVE: they CREDIT the liability, which is
what collecting rent does.  Moving a credit balance means debiting where it sits and
crediting where it belongs; a debit balance moves the other way.  Getting this backwards
would double the liability on one Location and negate it on the other, which still balances
and still reconciles in total -- so it would not be caught by any check that only looks at
the sum.

**The Location move is NOT a payment.**  Location is a reporting dimension, not a party:
both sides name one account, so no cash moves and no balance changes.  It only stops a trust
liability being reported under the company.

`--settle-cash` is the separate half that DOES move money.  Brittany's rent was never
collected as cash -- it was WITHHELD, each Purchase paying her cleaning minus the rent she
owed, so Chase Checking 7197 kept money the trust is owed.  Settling it is a real bank
transfer, and it is a SEPARATE JE on the date the money actually moves, because backdating a
cash movement into the period the liability arose would misstate both bank balances for every
day in between.  The two entries together read:

    (backdated)  Dr Monthly Rent @ Valta Realty   Cr Monthly Rent @ Trust      the liability
    (today)      Dr Chase Trust 9967 @ Trust      Cr Chase 7197 @ Valta Realty the cash

Each Location then nets to zero: the company loses a liability and the cash behind it, the
trust gains both.

**One JE per source month**, dated month-end.  A single catch-up JE dated today would leave
every Location-filtered balance sheet before today still wrong, which is the whole thing
being fixed.  `--single <date>` collapses it into one if that is preferred.
"""
from __future__ import annotations

import argparse
import calendar
import csv
import sys
from collections import defaultdict
from datetime import date
from pathlib import Path

from ..config import client
from ..paths import review_csv
from ..resolver import Resolver

DEFAULT_ACCOUNTS = ("Trust Liabilities:Owner Payables:1B - Long Term Rent:Monthly Rent",
                    "Trust Liabilities:Security Deposits Payable")
DOC = "LOCMOVE_{}"


def month_end(period: str) -> str:
    y, m = int(period[:4]), int(period[5:7])
    return date(y, m, calendar.monthrange(y, m)[1]).isoformat()


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.je.build_location_move")
    ap.add_argument("--from", dest="from_loc", required=True, help="the Location to move OFF")
    ap.add_argument("--to", dest="to_loc", required=True, help="the Location to move ON TO")
    ap.add_argument("--accounts", default=",".join(DEFAULT_ACCOUNTS),
                    help="comma-separated fully-qualified account names")
    ap.add_argument("--start", default="2025-01-01")
    ap.add_argument("--per", default="month", choices=("month", "year"),
                    help="one JE per source month (default) or per calendar year")
    ap.add_argument("--single", default=None, metavar="YYYY-MM-DD",
                    help="one JE on this date instead of one per source month")
    ap.add_argument("--settle-cash", action="store_true",
                    help="also emit the bank transfer that actually pays the balance over")
    ap.add_argument("--settle-from", default="Chase Checking 7197")
    ap.add_argument("--settle-to", default="Chase Trust Checking 9967 - STR")
    ap.add_argument("--settle-date", default=date.today().isoformat(),
                    help="when the money actually moves; default today")
    ap.add_argument("--out", default=None)
    args = ap.parse_args()

    qbo = client()
    res = Resolver(qbo)
    wanted = {}
    for name in [a.strip() for a in args.accounts.split(",") if a.strip()]:
        aid = res.account(name)
        if not aid:
            sys.exit(f"account not found: {name!r}")
        wanted[aid] = name
    for loc in (args.from_loc, args.to_loc):
        if res.department(loc) is None:
            sys.exit(f"location not found: {loc!r}")
    print(f"moving {args.from_loc!r} -> {args.to_loc!r} on {len(wanted)} account(s)")

    fqn = {c["Id"]: c.get("FullyQualifiedName", "")
           for c in qbo.query_all("SELECT * FROM Class MAXRESULTS 500", "Class")}

    # (period, account, class) -> net DEBIT-positive amount sitting on the from-Location
    net: dict[tuple, float] = defaultdict(float)
    sources: dict[tuple, list] = defaultdict(list)
    for p in qbo.query_all(
            f"SELECT * FROM Purchase WHERE TxnDate >= '{args.start}' MAXRESULTS 1000", "Purchase"):
        if (p.get("DepartmentRef") or {}).get("name") != args.from_loc:
            continue
        for ln in p.get("Line", []):
            det = ln.get("AccountBasedExpenseLineDetail")
            if not det:
                continue
            aid = (det.get("AccountRef") or {}).get("value")
            if aid not in wanted:
                continue
            amt = float(ln.get("Amount") or 0)
            key = (p["TxnDate"][:7], wanted[aid],
                   fqn.get((det.get("ClassRef") or {}).get("value"), ""))
            net[key] += amt
            sources[key].append((p["Id"], p["TxnDate"], amt))

    net = {k: round(v, 2) for k, v in net.items() if abs(round(v, 2)) >= 0.005}
    if not net:
        print("nothing on that Location to move.")
        return

    rows = []
    groups = defaultdict(list)
    for key in sorted(net):
        if args.single:
            bucket = args.single
        elif args.per == "year":
            bucket = key[0][:4]
        else:
            bucket = key[0]
        groups[bucket].append(key)

    for when, keys in sorted(groups.items()):
        doc = DOC.format(when)
        if args.single:
            txn = args.single
        else:
            # Date a grouped JE at the LAST source month it covers, never at the calendar
            # year end: 2026 is not over, and a future-dated entry would sit outside every
            # report run before it.
            txn = month_end(max(k[0] for k in keys))
        # Collapse the same (account, class) across the months in this bucket, or a yearly
        # JE would carry one pair per month and defeat the point of asking for one.
        merged: dict[tuple, float] = defaultdict(float)
        srcn: dict[tuple, int] = defaultdict(int)
        for k in keys:
            merged[(k[1], k[2])] += net[k]
            srcn[(k[1], k[2])] += len(sources[k])
        n = 0
        for (acct, klass), amount in sorted(merged.items()):
            amount = round(amount, 2)
            period = when
            # Debit-positive: a NEGATIVE net is a credit balance, which moves by debiting
            # where it sits and crediting where it belongs.  A positive net moves the other way.
            off_is_debit = amount < 0
            mag = abs(amount)
            for loc, is_debit in ((args.from_loc, off_is_debit), (args.to_loc, not off_is_debit)):
                n += 1
                rows.append({
                    "JournalNo": doc, "JournalDate": txn, "LineNum": n,
                    "Account": acct,
                    "Debits": f"{mag:.2f}" if is_debit else "",
                    "Credits": "" if is_debit else f"{mag:.2f}",
                    "Description": f"{period} move {acct.rsplit(':', 1)[-1]} "
                                   f"{args.from_loc} -> {args.to_loc}",
                    "Name": "", "EntityType": "", "Location": loc, "Class": klass,
                })
            print(f"  {doc}  {txn}  {acct.rsplit(':', 1)[-1]:<26} {klass or '(no class)':<18} "
                  f"{'Cr' if off_is_debit else 'Dr'} {mag:>10,.2f} moves  "
                  f"({srcn[(acct, klass)]} source line(s))")

    if args.settle_cash:
        for b in (args.settle_from, args.settle_to):
            if res.account(b) is None:
                sys.exit(f"bank account not found: {b!r}")
        cash = round(sum(abs(v) for v in net.values()), 2)
        doc = f"TRUSTSETTLE_{args.settle_date.replace('-', '')}"
        # Dated when the money moves, NOT in the period the liability arose.
        pairs = ((args.settle_to, args.to_loc, True), (args.settle_from, args.from_loc, False))
        for i, (bank, loc, is_debit) in enumerate(pairs, 1):
            rows.append({
                "JournalNo": doc, "JournalDate": args.settle_date, "LineNum": i,
                "Account": bank,
                "Debits": f"{cash:.2f}" if is_debit else "",
                "Credits": "" if is_debit else f"{cash:.2f}",
                "Description": f"{args.from_loc} settles withheld rent/deposits to {args.to_loc}",
                "Name": "", "EntityType": "", "Location": loc, "Class": "",
            })
        print(f"\n  {doc}  {args.settle_date}  {cash:,.2f} cash: "
              f"Cr {args.settle_from} -> Dr {args.settle_to}")
        print("     this is a REAL bank movement -- both feeds will offer it under Find match")

    out = Path(args.out) if args.out else review_csv("location_move")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    dr = sum(float(r["Debits"] or 0) for r in rows)
    cr = sum(float(r["Credits"] or 0) for r in rows)
    print(f"\n{len({r['JournalNo'] for r in rows})} JE(s), {len(rows)} lines -> {out}")
    print(f"  debits {dr:,.2f}  credits {cr:,.2f}  diff {dr - cr:,.2f}")
    if abs(dr - cr) > 0.005:
        sys.exit("ABORT -- debits and credits disagree")
    print("\nNOTHING WAS WRITTEN. Review the CSV, then:\n"
          f"    python -m src.je.post --all --csv {out}            # dry run\n"
          f"    python -m src.je.post --all --csv {out} --confirm  # WRITES")


if __name__ == "__main__":
    main()
