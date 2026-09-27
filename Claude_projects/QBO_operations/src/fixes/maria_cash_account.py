"""Move Maria's cleaning CASH onto the account that year's JE posts to.

`je/build_maria_cleaning` writes one JE per service month that debits each listing and
credits the flat class WITHIN A SINGLE ACCOUNT -- the one holding that month's cash.  That
invariant only holds if the cash is actually there.  When a payment lands on the parent
`Cleaning Payout` (1534) but its month's JE posts to `Destiny's cleaning` (1588), the two
accounts drift apart in equal and opposite directions: 1588 carries a credit with no cash
behind it and 1534 carries cash with nothing offsetting it.  Neither balance is wrong by
itself, which is why it goes unnoticed -- the P&L parent nets to the right number because
1588 is a CHILD of 1534.

    python -m src.fixes.maria_cash_account --year 2026              # dry run + review CSV
    python -m src.fixes.maria_cash_account --year 2026 --confirm     # write

The correct account is not a constant here: it comes from `build_maria_cleaning.
posting_account`, the same function the builder uses, so this script cannot disagree with
the JEs it is reconciling to.  `DESTINY_FROM` there is the one place the cutover month
lives.

Only `AccountRef` moves.  Class, amount, date, vendor, bank and PaymentType are left alone
-- a payment's class is `Valta Realty` (or the listing it was booked to) and that is a
separate question from which cleaning account holds it.  The Purchase is read back and
echoed whole with `sparse` false, because a full update blanks every field the payload
omits, and the run aborts on the first TotalAmt that moves.

Idempotent: a line already on its year's account is skipped, so re-running after next
January's payments only touches the new ones.
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
from ..je.build_maria_cleaning import PARENT_ACCT, DESTINY_ACCT, posting_account

# The payee is matched on the WHOLE DisplayName, case-folded -- not as a substring.
# `maria` also matches **Maria - OSBR**, a different vendor paid out of Chase Checking 7197
# whose single 2026 line is already classed to OSBR and is offset by no MariaCL JE at all.
# Sweeping it in would move company-account cash into the account whose per-class movement
# the JEs are responsible for.  Case-folding both sides is the Arvind Visvanathan lesson:
# QuickBooks resolves a DisplayName case-insensitively but returns its own capitalisation.
VENDOR_MATCH = "Maria Rangel"


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.fixes.maria_cash_account")
    ap.add_argument("--year", default="2026", help="calendar year of the payments to check")
    ap.add_argument("--vendor", default=VENDOR_MATCH,
                    help="exact payee DisplayName (case-insensitive); default Maria Rangel")
    ap.add_argument("--out", default=None)
    ap.add_argument("--confirm", action="store_true", help="WRITE (default is a dry run)")
    args = ap.parse_args()

    qbo = client()
    res = Resolver(qbo)
    # Both cleaning accounts, by Id, so a rename cannot silently narrow the search.
    ids = {res.account(PARENT_ACCT): PARENT_ACCT, res.account(DESTINY_ACCT): DESTINY_ACCT}
    want_id = {name: aid for aid, name in ids.items()}

    found, edits, rows = {}, {}, []
    near = {}            # payees that LOOK like the target but are not it -- printed, never used
    q = (f"SELECT * FROM Purchase WHERE TxnDate >= '{args.year}-01-01' "
         f"AND TxnDate <= '{args.year}-12-31' MAXRESULTS 1000")
    for p in qbo.query_all(q, "Purchase"):
        payee = (p.get("EntityRef") or {}).get("name", "")
        if payee.casefold() != args.vendor.casefold():
            # Flag a near miss rather than silently ignoring it: a second vendor sharing a
            # first name is exactly how the wrong cash gets moved.
            if args.vendor.split()[0].casefold() in payee.casefold():
                for ln in p.get("Line", []):
                    det = ln.get("AccountBasedExpenseLineDetail") or {}
                    if (det.get("AccountRef") or {}).get("value") in ids:
                        near.setdefault(payee, [0, 0.0, set()])
                        near[payee][0] += 1
                        near[payee][1] += float(ln.get("Amount") or 0)
                        near[payee][2].add((p.get("AccountRef") or {}).get("name", "?"))
            continue
        per = []
        for n, ln in enumerate(p.get("Line", [])):
            det = ln.get("AccountBasedExpenseLineDetail")
            if not det:
                continue
            cur = (det.get("AccountRef") or {}).get("value")
            if cur not in ids:
                continue
            target = posting_account(p["TxnDate"][:7])
            if ids[cur] == target:
                continue                      # already right for its month
            per.append(n)
            rows.append({"Id": p["Id"], "TxnDate": p["TxnDate"], "Payee": payee,
                         "DocNumber": p.get("DocNumber") or "",
                         "Amount": f"{float(ln.get('Amount') or 0):.2f}",
                         "Class": (det.get("ClassRef") or {}).get("name", ""),
                         "FromAccount": ids[cur], "ToAccount": target,
                         "Description": (ln.get("Description") or "").replace("\n", " ")})
        if per:
            found[p["Id"]] = p
            edits[p["Id"]] = per

    for who, (n, amt, banks) in sorted(near.items()):
        print(f"NOTE  {who!r} is a DIFFERENT payee and is NOT touched: {n} cleaning line(s), "
              f"{amt:,.2f}, from {'/'.join(sorted(banks))}")
    if near:
        print()

    if not rows:
        print(f"nothing to change — every {args.year} '{args.vendor}' cleaning line is already "
              f"on the account its month's JE posts to.")
        return

    out = Path(args.out) if args.out else review_csv(f"maria_cash_account_{args.year}")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)

    print(f"\n{len(rows)} line(s) on {len(edits)} purchase(s) -> {out}\n")
    for r in rows:
        print(f"  {r['TxnDate']}  Purchase {r['Id']:<7} {float(r['Amount']):>11,.2f}  "
              f"class {r['Class'] or '(none)'}")
        print(f"  {'':<12} {r['FromAccount'].rsplit(':', 1)[-1]} -> {r['ToAccount'].rsplit(':', 1)[-1]}")
    print(f"\n  TOTAL {sum(float(r['Amount']) for r in rows):,.2f}")

    if not args.confirm:
        print("\nDRY RUN — nothing written. Review the CSV, then re-run with --confirm.")
        return

    print(f"\nupdating {len(edits)} purchase(s) …")
    for pid, per in edits.items():
        p = found[pid]
        body = json.loads(json.dumps(p))
        for n in per:
            tgt = posting_account(p["TxnDate"][:7])
            body["Line"][n]["AccountBasedExpenseLineDetail"]["AccountRef"] = {"value": want_id[tgt]}
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
