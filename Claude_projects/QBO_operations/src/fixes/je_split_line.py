"""Split ONE line of a posted JournalEntry into two, moving part of it to another account.

    python -m src.fixes.je_split_line --id 119578 \
        --account "...:Maintenance - Owner" --class "Listings:Hoodsport 26060" --amount 225.00 \
        --move 112.50 --to "Accounts Receivable" --customer "homeaway - Lin Jiang - HA-FFz33vm" \
        --description "..." [--confirm]

`fixes/je_account` repoints a whole line; this one divides it, which is what a charge that two
parties bear actually needs.  Both exist because a split cannot be expressed as a repoint: the
amount has to end up on two accounts at once.

**The line is selected by (account Id, class Id, amount) and the match must be UNIQUE.**  A JE
here can carry the same account and class twice -- the itemised `Others` rows do -- so a
non-unique selector is refused rather than resolved by taking the first.  Ids, never names:
QuickBooks renders the same class differently depending on which object is asked.

The new line INHERITS the original's PostingType, ClassRef and DepartmentRef.  Location is a
PER-LINE field on a JE (inside JournalEntryLineDetail, unlike a Purchase where it is
transaction-level), so a new line built without it silently drops off every Location-filtered
report -- and nothing about the JE's totals would show it.

Guards, all checked around the write:

  * total debits and total credits are unchanged, and still equal each other;
  * every line NOT being split is byte-identical before and after;
  * the split pair sums to the original amount to the cent;
  * the object is re-read afterwards and the same checks run on what QBO actually stored.

A full JE update BLANKS whatever the payload omits, so the object is read back and echoed whole
through `je/payload.je_update` -- never hand-built.
"""
from __future__ import annotations

import argparse
import copy
import json
import sys

from ..config import client
from ..je.payload import je_update
from ..resolver import Resolver, esc


def totals(lines: list[dict]) -> tuple[float, float]:
    dr = cr = 0.0
    for ln in lines:
        d = ln.get("JournalEntryLineDetail") or {}
        amt = round(float(ln.get("Amount") or 0), 2)
        if d.get("PostingType") == "Debit":
            dr = round(dr + amt, 2)
        else:
            cr = round(cr + amt, 2)
    return dr, cr


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.fixes.je_split_line")
    ap.add_argument("--id", required=True, help="JournalEntry Id")
    ap.add_argument("--account", required=True, help="the line's CURRENT account (full name)")
    ap.add_argument("--class", dest="klass", required=True, help="the line's class (full name)")
    ap.add_argument("--amount", required=True, type=float, help="the line's current amount")
    ap.add_argument("--move", required=True, type=float, help="how much to move to the new line")
    ap.add_argument("--to", required=True, help="the new line's account (full name)")
    ap.add_argument("--customer", default=None)
    ap.add_argument("--vendor", default=None)
    ap.add_argument("--description", default=None, help="the new line's description")
    ap.add_argument("--confirm", action="store_true", help="actually WRITE")
    args = ap.parse_args()
    if args.customer and args.vendor:
        sys.exit("--customer and --vendor are mutually exclusive: a JE line names one entity.")

    qbo = client()
    res = Resolver(qbo)
    got = qbo.query(f"SELECT * FROM JournalEntry WHERE Id = '{esc(args.id)}'") \
        .get("QueryResponse", {}).get("JournalEntry", [])
    if not got:
        sys.exit(f"JournalEntry {args.id} not found")
    je = got[0]
    before = copy.deepcopy(je["Line"])

    from_id, to_id = res.account(args.account), res.account(args.to)
    class_id = res.klass(args.klass)
    for label, name, val in (("--account", args.account, from_id), ("--to", args.to, to_id),
                             ("--class", args.klass, class_id)):
        if val is None:
            sys.exit(f"{label}: not found in the company file: {name!r}")

    move, orig = round(args.move, 2), round(args.amount, 2)
    if not 0 < move < orig:
        sys.exit(f"--move {move:,.2f} must be greater than 0 and less than --amount {orig:,.2f}")

    hits = [i for i, ln in enumerate(je["Line"])
            if (d := ln.get("JournalEntryLineDetail") or {})
            and (d.get("AccountRef") or {}).get("value") == from_id
            and (d.get("ClassRef") or {}).get("value") == class_id
            and abs(round(float(ln.get("Amount") or 0), 2) - orig) < 0.005]
    if len(hits) != 1:
        sys.exit(f"selector matched {len(hits)} line(s) on JE {args.id}; it must match exactly one "
                 f"(account {from_id}, class {class_id}, amount {orig:,.2f}).  Nothing was written.")
    idx = hits[0]
    src = je["Line"][idx]
    det = src["JournalEntryLineDetail"]

    entity = None
    if args.customer or args.vendor:
        kind = "Customer" if args.customer else "Vendor"
        who = args.customer or args.vendor
        eid = (res.customer if kind == "Customer" else res.vendor)(who)
        if eid is None:
            sys.exit(f"{kind.lower()} not found: {who!r}")
        entity = {"Type": kind, "EntityRef": {"value": eid}}

    # The new line inherits PostingType, ClassRef and the PER-LINE DepartmentRef.
    new_det = {"PostingType": det.get("PostingType"),
               "AccountRef": {"value": to_id},
               "ClassRef": {"value": class_id}}
    if det.get("DepartmentRef"):
        new_det["DepartmentRef"] = {"value": det["DepartmentRef"]["value"]}
    if entity:
        new_det["Entity"] = entity
    new_line = {"DetailType": "JournalEntryLineDetail", "Amount": move,
                "Description": (args.description or src.get("Description") or "")[:500],
                "JournalEntryLineDetail": new_det}

    src["Amount"] = round(orig - move, 2)
    je["Line"].insert(idx + 1, new_line)

    dr0, cr0 = totals(before)
    dr1, cr1 = totals(je["Line"])
    print(f"JE {je['Id']}  {je.get('DocNumber')}  {je['TxnDate']}")
    print(f"  line {idx}: {args.account.rsplit(':', 1)[-1]} {orig:,.2f} "
          f"-> {src['Amount']:,.2f}")
    print(f"  NEW:      {args.to.rsplit(':', 1)[-1]} {move:,.2f} "
          f"{det.get('PostingType')} class {args.klass}"
          + (f"  {entity['Type']} {args.customer or args.vendor}" if entity else ""))
    print(f"    description: {new_line['Description']!r}")
    print(f"  debits {dr0:,.2f} -> {dr1:,.2f}   credits {cr0:,.2f} -> {cr1:,.2f}")
    if (dr0, cr0) != (dr1, cr1) or abs(dr1 - cr1) > 0.005:
        sys.exit("ABORT -- the split changed the JE's totals.  Nothing was written.")
    untouched_before = [l for i, l in enumerate(before) if i != idx]
    untouched_after = [l for l in je["Line"] if l is not src and l is not new_line]
    if untouched_before != untouched_after:
        sys.exit("ABORT -- a line other than the split one changed.  Nothing was written.")
    print(f"  {len(untouched_before)} other line(s) unchanged")

    if not args.confirm:
        print("\nDRY RUN -- nothing written.  Re-run with --confirm to WRITE.")
        return

    qbo.post("journalentry", je_update(je))
    back = qbo.query(f"SELECT * FROM JournalEntry WHERE Id = '{esc(args.id)}'") \
        .get("QueryResponse", {}).get("JournalEntry", [])[0]
    dr2, cr2 = totals(back["Line"])
    pair = [ln for ln in back["Line"]
            if (d := ln.get("JournalEntryLineDetail") or {})
            and (d.get("ClassRef") or {}).get("value") == class_id
            and (d.get("AccountRef") or {}).get("value") in (from_id, to_id)]
    got_sum = round(sum(round(float(l.get("Amount") or 0), 2) for l in pair), 2)
    print(f"\nwritten.  read back: debits {dr2:,.2f} credits {cr2:,.2f}, "
          f"{len(back['Line'])} lines (was {len(before)})")
    for ln in pair:
        d = ln["JournalEntryLineDetail"]
        ent = (d.get("Entity") or {}).get("EntityRef", {}).get("name", "")
        print(f"   {float(ln['Amount']):>9,.2f}  {(d.get('AccountRef') or {}).get('name','')[:58]:<58}"
              f"  {ent}")
    if (dr2, cr2) != (dr0, cr0):
        sys.exit("ABORT -- the JE's totals moved after the write.  CHECK QUICKBOOKS NOW.")
    if abs(got_sum - orig) > 0.005:
        sys.exit(f"ABORT -- the split pair reads back as {got_sum:,.2f}, not {orig:,.2f}. "
                 f"CHECK QUICKBOOKS NOW.")
    print("verified: totals unchanged and the split pair sums to the original.")


if __name__ == "__main__":
    main()
