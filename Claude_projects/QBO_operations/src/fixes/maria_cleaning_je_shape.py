"""Make a `MariaCL_*` JE a pure CLASS move inside Destiny's cleaning.

Owner rule, 2026-09-26: **Maria's JEs debit and credit Destiny's cleaning (1588); the only
thing that changes is the class.**

    Dr ...:Destiny's cleaning (1588)   class = the listing    per listing
    Cr ...:Destiny's cleaning (1588)   class = Valta Realty   one line, the total

Her lump Zelle transfers land on 1588 under Valta Realty, so no listing carries its own
cleaning cost.  Redistributing INSIDE the account attributes that cost per listing without
moving a cent between accounts: the account total is unchanged by construction and only the
class split moves.  `MariaCL_2026-08` is the worked example, corrected by hand.

August also carries the third line the earlier months are missing:

    Dr Accounts Receivable (807)   class Valta Realty, Customer **Valta Home**
                                   = the sheet's `Residential cleaning`

`Residential cleaning` is not a listing and is not Valta's cost -- it is rebilled -- so it
leaves the JE on A/R rather than sitting in the listings credit.  The amount is READ FROM THE
WORKBOOK, the same sheet the JE was built from, never inferred from the JE.

So this brings a posted JE to the August shape in three moves:

    1. listing debits          -> Destiny's cleaning (1588)
    2. residential             -> a new A/R debit line, if the JE has none and the sheet has one
    3. the flat Valta Realty credit  -> the sum of the debits, so the whole sheet clears

**The workbook is the authority for the A/R amount**, not what is on the JE: an existing line
is corrected to the sheet, not preserved.  An early version kept whatever was posted, reading
a difference as a deliberate hand correction -- it is not, it is the JE being out of date.
`--keep-ar` restores the preserving behaviour for a month where the posted figure really is
the intended one.

**The workbook must be current.**  `inputs/JE/Cleaning payouts/...xlsx` is a snapshot of a
live Google Sheet, and a stale snapshot silently produces wrong residential figures -- a
blank cell reads as "no residential this month" and writes no line at all.  The script prints
the file's own modification time so an old export is visible before anything is posted, never
after.

Listing lines have only their `AccountRef` touched -- posting types, amounts, classes and
descriptions are left alone.  That restraint matters: an earlier pass tried to derive every
line's role from its class (listing means debit, flat means credit) and broke
`MariaCL_2026-06`, because the A/R line is neither a listing nor the credit.  A rule that
reads roles off the class cannot see that line; naming each of the three moves never has to.

    python -m src.fixes.maria_cleaning_je_shape --from 2026-01            # dry run + CSV
    python -m src.fixes.maria_cleaning_je_shape --docs MariaCL_2026-08
    python -m src.fixes.maria_cleaning_je_shape --from 2026-01 --confirm  # write
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import json
import re
import sys
from collections import defaultdict
from pathlib import Path

import openpyxl

from ..config import acct_name, client
from ..je.build_maria_cleaning import DEFAULT_SRC, period_of, read_month
from ..je.payload import je_update
from ..paths import review_csv
from ..resolver import Resolver

FLAT_CLASS = "Valta Realty"
AR_CUSTOMER = "Valta Home"
DOC_RE = re.compile(r"^MariaCL_(\d{4}-\d{2})$")


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--docs", default=None, help="comma-separated DocNumbers")
    ap.add_argument("--from", dest="frm", default=None, help="first period, e.g. 2026-01")
    ap.add_argument("--to", dest="to", default=None, help="last period, e.g. 2026-08")
    ap.add_argument("--src", default=str(DEFAULT_SRC), help="Maria's workbook")
    ap.add_argument("--keep-ar", action="store_true",
                    help="leave an existing A/R amount alone instead of matching the sheet")
    ap.add_argument("--out", default=None)
    ap.add_argument("--confirm", action="store_true", help="actually WRITE to QuickBooks")
    args = ap.parse_args()

    qbo = client()
    res = Resolver(qbo)
    acct = acct_name("destiny_cleaning")
    other = acct_name("cleaning_payout")
    ar_acct = acct_name("accounts_receivable")
    aid = res.account(acct)
    ar_id = res.account(ar_acct)
    cust_id = res.customer(AR_CUSTOMER)
    print("resolving:")
    for nm, got in ((acct, aid), (ar_acct, ar_id), (f"Customer {AR_CUSTOMER!r}", cust_id)):
        print(f"  {nm[-62:]:<62} {'Id ' + got if got else '*** MISSING ***'}")
    if not all((aid, ar_id, cust_id)):
        sys.exit("ABORT — an account or customer does not exist")

    src = Path(args.src)
    if not src.exists():
        sys.exit(f"ABORT — no workbook at {src}")
    age = dt.datetime.now() - dt.datetime.fromtimestamp(src.stat().st_mtime)
    print(f"\n  workbook {src.name}\n    modified {dt.datetime.fromtimestamp(src.stat().st_mtime):%Y-%m-%d %H:%M}"
          f"  ({age.days} day(s) ago)"
          + ("   *** this is a snapshot of a live Google Sheet -- re-export it before posting"
             " if it is behind ***" if age.days >= 1 else ""))
    wb = openpyxl.load_workbook(src, data_only=True, read_only=True)
    sheets = {p: sh for sh in wb.sheetnames if (p := period_of(sh))}

    jes = qbo.query_all("SELECT * FROM JournalEntry WHERE DocNumber LIKE 'MariaCL_%'",
                        "JournalEntry")
    want = {d.strip() for d in args.docs.split(",")} if args.docs else None
    picked = []
    for j in jes:
        doc = j.get("DocNumber") or ""
        m = DOC_RE.match(doc)
        if not m:
            continue
        p = m.group(1)
        if want is not None and doc not in want:
            continue
        if args.frm and p < args.frm:
            continue
        if args.to and p > args.to:
            continue
        picked.append((p, j))
    picked.sort(key=lambda x: x[0])
    if not picked:
        sys.exit("no MariaCL JEs matched")
    if want:
        for d in sorted(want - {j.get("DocNumber") for _p, j in picked}):
            print(f"  *** {d} not found ***")

    rows, jobs = [], []
    move = defaultdict(float)   # (account, class-kind) -> signed net debit change
    print(f"\n{'DocNumber':<18} {'Id':<8} {'lines':>5} {'new total':>12} {'changing':>9}  "
          f"{'listing debits on':<18} residential")
    for period, je in picked:
        body = json.loads(json.dumps(je))
        changed = 0
        for l in body["Line"]:
            det = l.get("JournalEntryLineDetail") or {}
            amt = round(float(l.get("Amount") or 0), 2)
            klass = (det.get("ClassRef") or {}).get("name", "")
            was_acct = det.get("AccountRef", {}).get("name", "")
            was_type = det.get("PostingType", "")
            if not klass.startswith("Listings:"):
                # The flat credit AND the Accounts Receivable debit for `Residential
                # cleaning` both live here.  Neither is ours to touch.
                rows.append({"DocNumber": je.get("DocNumber"), "JournalEntryId": je["Id"],
                             "TxnDate": je["TxnDate"], "Class": klass, "Amount": f"{amt:.2f}",
                             "FromAccount": was_acct, "FromPosting": was_type,
                             "ToAccount": was_acct, "ToPosting": was_type,
                             "Action": "left alone (not a listing)",
                             "Description": l.get("Description") or ""})
                continue
            if was_type != "Debit":
                sys.exit(f"ABORT — {je.get('DocNumber')}: listing line {klass!r} "
                         f"${amt:,.2f} is a {was_type}, expected a Debit")
            sgn = 1 if was_type == "Debit" else -1
            if det.get("AccountRef", {}).get("value") != aid:
                changed += 1
                move[(was_acct, "per listing")] -= sgn * amt
                move[(acct, "per listing")] += sgn * amt
            det["AccountRef"] = {"value": aid}
            rows.append({"DocNumber": je.get("DocNumber"), "JournalEntryId": je["Id"],
                         "TxnDate": je["TxnDate"], "Class": klass, "Amount": f"{amt:.2f}",
                         "FromAccount": was_acct, "FromPosting": was_type,
                         "ToAccount": acct, "ToPosting": was_type,
                         "Action": "MOVE" if was_acct != acct else "already correct",
                         "Description": l.get("Description") or ""})
        # --- 2. the residential A/R line -------------------------------------------
        if period not in sheets:
            sys.exit(f"ABORT — {je.get('DocNumber')}: no {period!r} sheet in {args.src}")
        _rows, residential, _tot = read_month(wb[sheets[period]])
        ar = [l for l in body["Line"]
              if (l.get("JournalEntryLineDetail") or {}).get("AccountRef", {}).get("value") == ar_id]
        if len(ar) > 1:
            sys.exit(f"ABORT — {je.get('DocNumber')}: {len(ar)} A/R lines, expected at most one")
        if ar:
            have = round(float(ar[0]["Amount"]), 2)
            same = abs(have - residential) <= 0.005
            if same:
                ar_note = f"A/R {have:,.2f} matches the sheet"
            elif args.keep_ar:
                ar_note = (f"A/R {have:,.2f} KEPT (sheet says {residential:,.2f}, "
                           f"diff {have - residential:+,.2f})")
            else:
                ar[0]["Amount"] = residential
                changed += 1
                move[(ar_acct, FLAT_CLASS)] += round(residential - have, 2)
                ar_note = f"A/R {have:,.2f} -> {residential:,.2f} ({residential - have:+,.2f})"
                rows.append({"DocNumber": je.get("DocNumber"), "JournalEntryId": je["Id"],
                             "TxnDate": je["TxnDate"], "Class": FLAT_CLASS,
                             "Amount": f"{residential:.2f}",
                             "FromAccount": f"A/R was {have:,.2f}", "FromPosting": "Debit",
                             "ToAccount": ar_acct, "ToPosting": "Debit",
                             "Action": "CORRECT A/R to the sheet",
                             "Description": ar[0].get("Description") or ""})
        elif residential:
            body["Line"].append({
                "Amount": residential,
                "Description": f"{period} Residential Cleaning",
                "DetailType": "JournalEntryLineDetail",
                "JournalEntryLineDetail": {
                    "PostingType": "Debit",
                    "AccountRef": {"value": ar_id},
                    "ClassRef": {"value": res.klass(FLAT_CLASS)},
                    "DepartmentRef": {"value": res.department(FLAT_CLASS)},
                    "Entity": {"Type": "Customer", "EntityRef": {"value": cust_id}},
                }})
            changed += 1
            ar_note = f"A/R {residential:,.2f} ADDED"
            move[(ar_acct, FLAT_CLASS)] += residential
            rows.append({"DocNumber": je.get("DocNumber"), "JournalEntryId": je["Id"],
                         "TxnDate": je["TxnDate"], "Class": FLAT_CLASS,
                         "Amount": f"{residential:.2f}", "FromAccount": "(none)",
                         "FromPosting": "", "ToAccount": ar_acct, "ToPosting": "Debit",
                         "Action": f"ADD A/R line (Customer {AR_CUSTOMER})",
                         "Description": f"{period} Residential Cleaning"})
        else:
            ar_note = "no residential this month"

        # --- 3. the flat credit clears the whole sheet ------------------------------
        d = round(sum(float(l["Amount"]) for l in body["Line"]
                      if l["JournalEntryLineDetail"]["PostingType"] == "Debit"), 2)
        cred = [l for l in body["Line"]
                if l["JournalEntryLineDetail"]["PostingType"] == "Credit"]
        if len(cred) != 1:
            sys.exit(f"ABORT — {je.get('DocNumber')}: {len(cred)} credit lines, expected one")
        was_credit = round(float(cred[0]["Amount"]), 2)
        if abs(was_credit - d) > 0.005:
            cred[0]["Amount"] = d
            changed += 1
            move[(acct, FLAT_CLASS)] -= round(d - was_credit, 2)
            rows.append({"DocNumber": je.get("DocNumber"), "JournalEntryId": je["Id"],
                         "TxnDate": je["TxnDate"], "Class": FLAT_CLASS, "Amount": f"{d:.2f}",
                         "FromAccount": f"credit was {was_credit:,.2f}", "FromPosting": "Credit",
                         "ToAccount": acct, "ToPosting": "Credit",
                         "Action": "RAISE credit to the sheet total",
                         "Description": cred[0].get("Description") or ""})
        c = round(sum(float(l["Amount"]) for l in body["Line"]
                      if l["JournalEntryLineDetail"]["PostingType"] == "Credit"), 2)
        if abs(d - c) > 0.005:
            sys.exit(f"ABORT — {je.get('DocNumber')} would not balance: {d:,.2f} vs {c:,.2f}")
        lis = {ln["JournalEntryLineDetail"]["AccountRef"]["name"].rsplit(":", 1)[-1]
               for ln in je["Line"]
               if (ln["JournalEntryLineDetail"].get("ClassRef") or {})
                  .get("name", "").startswith("Listings:")}
        print(f"{je.get('DocNumber'):<18} {je['Id']:<8} {len(body['Line']):>5} {d:>12,.2f} "
              f"{changed:>9}  {'/'.join(sorted(lis)) or '(none)':<18} {ar_note}")
        if changed:
            jobs.append((je.get("DocNumber"), body, d, c))

    print(f"\nnet DEBIT movement by account and class (what the P&L by Class will show):")
    for (a, k) in sorted(move, key=lambda x: (x[0], x[1])):
        v = move[(a, k)]
        if abs(v) < 0.005:
            continue
        print(f"   {a[-52:]:<52} {k:<14} {v:>13,.2f}")

    out = Path(args.out) if args.out else review_csv("maria_cleaning_je_shape")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    print(f"\n{len(jobs)} JE(s) to change, {len(rows)} line(s) -> {out}")
    print(f"  each JE now clears its sheet's own Total payment: adding the A/R line RAISES the"
          f"\n  total, which is the point -- the listings subtotal never covered residential.")
    print(f"  {other.rsplit(':',1)[-1]!r} gives up every per-listing debit to "
          f"{acct.rsplit(':',1)[-1]!r}; no other account changes except A/R.")

    if not jobs:
        print("\nall already correct — nothing to write.")
        return
    if not args.confirm:
        print("\nDRY RUN — nothing written. Review the CSV, then re-run with --confirm.")
        return

    for doc, body, d, c in jobs:
        got = qbo.post("journalentry", je_update(body))
        nd = round(sum(float(l["Amount"]) for l in got["Line"]
                       if l["JournalEntryLineDetail"]["PostingType"] == "Debit"), 2)
        nc = round(sum(float(l["Amount"]) for l in got["Line"]
                       if l["JournalEntryLineDetail"]["PostingType"] == "Credit"), 2)
        if abs(nd - d) > 0.005 or abs(nc - c) > 0.005:
            sys.exit(f"ABORT — {doc}: posted {nd:,.2f}/{nc:,.2f}, expected {d:,.2f}/{c:,.2f}")
        print(f"  JE {got['Id']}  {got.get('DocNumber')}  {nd:,.2f} / {nc:,.2f}  "
              f"SyncToken {got['SyncToken']}")
    print(f"\ncorrected {len(jobs)} JE(s)")


if __name__ == "__main__":
    main()
