"""Make ALL Yacinde cleaning owner-borne: off the HOA receivable, onto owner cleaning.

    python -m src.fixes.yacinde_cleaning_all_owner              # dry run
    python -m src.fixes.yacinde_cleaning_all_owner --confirm    # WRITES

Owner decision 2026-09-28: the HOA no longer reimburses cleaning, so every Yacinde cleaning line
belongs on `1C - Owner Expenses:Cleaning Expense - Owner` carrying its UNIT class, and the
Yacinde owners bear it.  This reverses the split that `fixes/yacinde_hoa_split` and
`fixes/yacinde_hoa_resplit` maintain; those read an allocation workbook to decide who bears each
clean, and this decision overrides the workbook wholesale, which is why neither of them fits.

**It also supersedes `fixes/yacinde_hoa_classes` for cleaning.**  That script queries `Bill`
objects (`YacindeCL_` DocNumbers) and Yacinde HOA Invoices only, and AA cleaning became a
PURCHASE on 2026-09-19 -- so it cannot see any of this.  Same staleness CLAUDE.md records for
`yacinde_hoa_split`.

Two line shapes are corrected, and they are not the same defect:

    on HOA Receivable + a `Listings:Yacinde*` class   -> AccountRef to owner cleaning
                                                        (the HOA's share becoming the owner's)
    on owner cleaning + the flat `Yacinde HOA` class  -> ClassRef to the unit's listing class
                                                        (already the right ACCOUNT, but a flat
                                                        class reaches no owner statement at all,
                                                        so the charge lands on nobody)

**What is deliberately NOT touched: receivable lines carrying the flat `Yacinde HOA` class.**
Those are not cleaning -- they are the Derrick Holm maintenance wage and the pool chemicals, which
the HOA still does reimburse.  Selecting by account alone would sweep them up and silently make
the owners bear common-area cost.  The class is what separates them.

The unit comes from the line's own DESCRIPTION (`07/23/2026 B3 Cleaning Fee`), resolved to a real
class by leaf name, and a line whose unit cannot be read or resolved BLOCKS its document rather
than being guessed at.

## The invoice half has to follow, and this script does not do it

`invoices/hoa_cleaning_update` is the other half.  Until it runs, HOA-2026-07 and HOA-2026-08
still bill the HOA for cleaning the owners now bear -- so the cost is recovered twice.  Running
the EXPENSE side first is the documented safe order: it leaves the receivable temporarily
negative, which reads as "we owe the HOA" rather than as an asset that does not exist.
**If the receivable does not move by what the invoices come down by, one half is missing.**

Only `AccountRef` and `ClassRef` change.  Every document's `TotalAmt` and every line's amount and
description are fingerprinted before and after, and the run aborts on the first total that moves.
A Purchase is FULL-updated by reading it back and echoing it whole -- never hand-built.
"""
from __future__ import annotations

import argparse
import copy
import re
import sys
from collections import defaultdict

from ..config import client, name as cfg_name
from ..resolver import Resolver, esc

VENDOR = "AA Professional Cleaners LLC"
HOA_CLASS = "Yacinde HOA"
UNIT_RE = re.compile(r"\d{2}/\d{2}/\d{4}\s+(?:Yacinde\s+)?([A-Z]\d)\b")


def fingerprint(p: dict) -> tuple:
    return (round(float(p.get("TotalAmt") or 0), 2), p.get("DocNumber"), p.get("TxnDate"),
            (p.get("EntityRef") or {}).get("value"), (p.get("AccountRef") or {}).get("value"),
            p.get("PaymentType"),
            tuple((round(float(l.get("Amount") or 0), 2), l.get("Description"))
                  for l in p.get("Line", [])))


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.fixes.yacinde_cleaning_all_owner")
    ap.add_argument("--ids", default=None, help="limit to these Purchase Ids")
    ap.add_argument("--confirm", action="store_true", help="actually WRITE")
    args = ap.parse_args()

    qbo = client()
    res = Resolver(qbo)
    recv_id = res.account(cfg_name("hoa_receivable_yacinde"))
    owner_id = res.account(cfg_name("cleaning_expense_owner"))
    if recv_id is None or owner_id is None:
        sys.exit("HOA receivable or owner-cleaning account not found")
    fqn = {c["Id"]: c.get("FullyQualifiedName", "") for c in
           qbo.query_all("SELECT * FROM Class MAXRESULTS 500", "Class")}
    hoa_cls = {i for i, n in fqn.items() if n == HOA_CLASS}

    vid = res.vendor(VENDOR)
    if vid is None:
        sys.exit(f"vendor not found: {VENDOR!r}")
    purchases = [p for p in qbo.query_all(
        f"SELECT * FROM Purchase WHERE TxnDate >= '2026-01-01' MAXRESULTS 1000", "Purchase")
        if (p.get("EntityRef") or {}).get("value") == vid]
    if args.ids:
        keep = {i.strip() for i in args.ids.split(",")}
        purchases = [p for p in purchases if p["Id"] in keep]
    purchases.sort(key=lambda p: p["TxnDate"])

    unit_cls: dict[str, str] = {}

    def class_for(unit: str) -> str | None:
        if unit not in unit_cls:
            full = res.klass_fqn(f"Yacinde {unit}")
            unit_cls[unit] = res.klass(full) if full else None
        return unit_cls[unit]

    plan, blocked = [], []
    for p in purchases:
        moves, bad = [], []
        for idx, ln in enumerate(p.get("Line", [])):
            d = ln.get("AccountBasedExpenseLineDetail") or {}
            if not d:
                continue
            a = (d.get("AccountRef") or {}).get("value")
            cid = (d.get("ClassRef") or {}).get("value")
            amt = round(float(ln.get("Amount") or 0), 2)
            desc = ln.get("Description") or ""
            if a == recv_id and cid not in hoa_cls and fqn.get(cid, "").startswith("Listings:"):
                moves.append((idx, "account", amt, fqn.get(cid, ""), desc))
            elif a == owner_id and cid in hoa_cls:
                unit = (UNIT_RE.search(desc) or [None, None])[1] if UNIT_RE.search(desc) else None
                if not unit:
                    bad.append(f"line {idx}: {amt:,.2f} on owner cleaning with the flat class and "
                               f"no readable unit in {desc[:48]!r}")
                    continue
                target = class_for(unit)
                if not target:
                    bad.append(f"line {idx}: unit {unit!r} resolves to no class")
                    continue
                moves.append((idx, "class", amt, f"-> Yacinde {unit}", desc))
        if bad:
            blocked.append((p, bad)); continue
        if moves:
            plan.append((p, moves))

    acct_total = round(sum(a for _, mv in plan for _, k, a, _, _ in mv if k == "account"), 2)
    cls_total = round(sum(a for _, mv in plan for _, k, a, _, _ in mv if k == "class"), 2)
    for p, moves in plan:
        print(f"Purchase {p['Id']}  {p['TxnDate']}  {p.get('DocNumber') or '-':<24} "
              f"{float(p.get('TotalAmt') or 0):>9,.2f}")
        for idx, kind, amt, where, desc in moves:
            what = ("receivable -> owner cleaning" if kind == "account"
                    else f"class {HOA_CLASS} {where}")
            print(f"    line {idx:>2}  {amt:>8,.2f}  {what:<34} {desc[:38]}")
    for p, bad in blocked:
        print(f"BLOCKED Purchase {p['Id']} {p.get('DocNumber') or '-'}:")
        for b in bad:
            print(f"    {b}")
    print(f"\n{len(plan)} Purchase(s): {acct_total:,.2f} moves off the HOA receivable onto owner "
          f"cleaning, {cls_total:,.2f} gets its unit class.  {len(blocked)} blocked.")
    if not args.confirm:
        print("\nDRY RUN -- nothing written.  Re-run with --confirm to WRITE.")
        return

    print()
    failed = []
    for p, moves in plan:
        want = fingerprint(p)
        body = copy.deepcopy(p)
        for idx, kind, _, _, desc in moves:
            det = body["Line"][idx]["AccountBasedExpenseLineDetail"]
            if kind == "account":
                det["AccountRef"] = {"value": owner_id}
            else:
                unit = UNIT_RE.search(desc).group(1)
                det["ClassRef"] = {"value": class_for(unit)}
        body["sparse"] = False
        try:
            qbo.post("purchase", body)
        except Exception as exc:                                     # noqa: BLE001
            print(f"   {p['Id']}  FAILED: {str(exc)[:200]}")
            failed.append(p["Id"]); continue
        back = qbo.query(f"SELECT * FROM Purchase WHERE Id = '{esc(p['Id'])}'") \
            .get("QueryResponse", {}).get("Purchase", [])[0]
        if fingerprint(back) != want:
            print(f"   {p['Id']}  *** an amount, description or header field moved ***")
            failed.append(p["Id"]); continue
        still = sum(1 for l in back.get("Line", [])
                    if (d := l.get("AccountBasedExpenseLineDetail") or {})
                    and ((d.get("AccountRef") or {}).get("value") == recv_id
                         and fqn.get((d.get("ClassRef") or {}).get("value"), "").startswith("Listings:")
                         or (d.get("AccountRef") or {}).get("value") == owner_id
                         and (d.get("ClassRef") or {}).get("value") in hoa_cls))
        print(f"   {p['Id']}  {p.get('DocNumber') or '-':<24} {len(moves)} line(s) moved, "
              f"total {float(back.get('TotalAmt') or 0):,.2f} unchanged"
              + (f"   *** {still} still to fix ***" if still else ""))
        if still:
            failed.append(p["Id"])
    if failed:
        sys.exit(f"\n{len(failed)} Purchase(s) did not land cleanly: {', '.join(failed)}")
    print(f"\ndone.  {acct_total:,.2f} moved to owner cleaning, {cls_total:,.2f} re-classed."
          f"\nTHE INVOICE HALF IS STILL OUTSTANDING -- HOA-2026-07 and HOA-2026-08 still bill the "
          f"HOA for cleaning the owners now bear.")


if __name__ == "__main__":
    main()
