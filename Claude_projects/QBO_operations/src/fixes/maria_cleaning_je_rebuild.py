"""Rebuild posted `MariaCL_*` JEs from a corrected workbook, in place.

`fixes/maria_cleaning_je_shape` re-points accounts and adds the missing A/R line, but it
cannot help when the SHEET ITSELF changes: a corrected workbook can move a listing's amount,
drop a listing, or add one.  2026-03 went from 40 listings / $22,237.00 to 38 / $22,620.00
that way.  Patching a JE line by line cannot express that; the lines have to come from the
workbook again.

So this takes the builder's own output -- `build_maria_cleaning --rebuild`, which emits rows
for months already in QBO instead of skipping them -- and FULL-UPDATES each existing JE with
it, keeping the Id, the DocNumber and the date.  One source of truth for what a Maria JE
looks like, and it is the builder; nothing here constructs a line.

    python -m src.je.build_maria_cleaning --rebuild                  # refresh the review CSV
    python -m src.fixes.maria_cleaning_je_rebuild                    # dry run, diff per JE
    python -m src.fixes.maria_cleaning_je_rebuild --confirm          # write

A JE with no matching DocNumber is reported and skipped -- raising it is `je/post`'s job, not
this one's.  Updating rather than deleting and re-creating keeps the Id, so anything already
pointing at it still resolves, and it is the house rule: create before deleting, and never
delete to avoid an update.
"""
from __future__ import annotations

import argparse
import csv
import json
import sys
from collections import defaultdict
from pathlib import Path

from ..config import client
from ..je.build_maria_cleaning import DEFAULT_CSV
from ..je.payload import je_update
from ..je.post import build_payload
from ..resolver import Resolver


def totals(lines: list[dict]) -> tuple[float, float]:
    d = round(sum(float(l["Amount"]) for l in lines
                  if l["JournalEntryLineDetail"]["PostingType"] == "Debit"), 2)
    c = round(sum(float(l["Amount"]) for l in lines
                  if l["JournalEntryLineDetail"]["PostingType"] == "Credit"), 2)
    return d, c


def shape_je(lines: list[dict]) -> tuple[int, float, float, float]:
    """(listing line count, listings, A/R, credit) for a JE READ BACK from QBO."""
    n = lis = ar = cr = 0.0
    for l in lines:
        det = l["JournalEntryLineDetail"]
        amt = round(float(l["Amount"]), 2)
        if det["PostingType"] == "Credit":
            cr += amt
        elif (det.get("ClassRef") or {}).get("name", "").startswith("Listings:"):
            lis += amt
            n += 1
        else:
            ar += amt
    return int(n), round(lis, 2), round(ar, 2), round(cr, 2)


def shape_rows(rows: list[dict]) -> tuple[int, float, float, float]:
    """The same, for the builder's review-CSV rows.

    It cannot be read off `build_payload`'s output: that resolves every name to an Id and
    drops the name, so a listing line and the A/R line become indistinguishable.  The CSV
    still carries `Class` and `Name`, so the classification happens here instead.
    """
    n = lis = ar = cr = 0.0
    for r in rows:
        amt = round(float(r["Debits"] or r["Credits"]), 2)
        if r["Credits"]:
            cr += amt
        elif r["Name"]:
            ar += amt
        elif r["Class"].startswith("Listings:"):
            lis += amt
            n += 1
        else:
            ar += amt
    return int(n), round(lis, 2), round(ar, 2), round(cr, 2)


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--csv", default=str(DEFAULT_CSV))
    ap.add_argument("--months", help="comma list of YYYY-MM; default every JE in the CSV")
    ap.add_argument("--confirm", action="store_true", help="actually WRITE to QuickBooks")
    args = ap.parse_args()

    path = Path(args.csv)
    if not path.exists():
        sys.exit(f"ABORT — no {path}; run `python -m src.je.build_maria_cleaning --rebuild` first")
    rows = list(csv.DictReader(path.open(encoding="utf-8")))
    if not rows:
        sys.exit(f"ABORT — {path} is empty; did you pass --rebuild to the builder?")

    by_doc: dict[str, list[dict]] = defaultdict(list)
    for r in rows:
        by_doc[r["JournalNo"]].append(r)
    if args.months:
        want = {f"MariaCL_{m.strip()}" for m in args.months.split(",")}
        by_doc = {k: v for k, v in by_doc.items() if k in want}
    if not by_doc:
        sys.exit("no JEs selected")

    qbo = client()
    res = Resolver(qbo)
    print(f"{path.name}: {len(rows)} line(s) over {len(by_doc)} JE(s)\n")
    print(f"{'DocNumber':<17} {'Id':<7} {'listings':>22} {'A/R':>21} {'credit':>23}")

    jobs = []
    for doc in sorted(by_doc):
        payload, errs = build_payload(by_doc[doc], res)
        if errs:
            for e in errs:
                print(f"  {doc}: *** {e} ***")
            sys.exit(f"ABORT — {doc} does not resolve; fix the review CSV first")
        found = qbo.query_all(
            f"SELECT * FROM JournalEntry WHERE DocNumber = '{doc}'", "JournalEntry")
        if len(found) != 1:
            print(f"{doc:<17} *** {len(found)} JE(s) on the books — skipped ***")
            continue
        je = found[0]
        on, ol, oa, oc = shape_je(je["Line"])
        nn, nl, na, nc = shape_rows(by_doc[doc])
        nd, ncr = totals(payload["Line"])
        if abs(nd - ncr) > 0.005:
            sys.exit(f"ABORT — {doc} would not balance: {nd:,.2f} vs {ncr:,.2f}")

        def cell(o, n, w=10):
            return (f"{o:>{w},.2f} -> {n:>{w},.2f}" if abs(o - n) > 0.005
                    else f"{'':>{w}}    {n:>{w},.2f}")
        mark = "  CHANGED" if (on, ol, oa, oc) != (nn, nl, na, nc) else ""
        print(f"{doc:<17} {je['Id']:<7} {cell(ol, nl)} {cell(oa, na, 8)} {cell(oc, nc)}"
              f"   {on}->{nn} listings{mark}")

        body = json.loads(json.dumps(je))
        body["Line"] = payload["Line"]
        jobs.append((doc, je, body, nd))

    if not jobs:
        sys.exit("\nnothing to update")
    print(f"\n{len(jobs)} JE(s) would be full-updated in place (Id, DocNumber and date kept)")

    if not args.confirm:
        print("\nDRY RUN — nothing written. Review the CSV, then re-run with --confirm.")
        return

    for doc, je, body, want in jobs:
        got = qbo.post("journalentry", je_update(body))
        d, c = totals(got["Line"])
        if abs(d - want) > 0.005 or abs(d - c) > 0.005:
            sys.exit(f"ABORT — {doc}: posted {d:,.2f}/{c:,.2f}, expected {want:,.2f}")
        print(f"  JE {got['Id']}  {got.get('DocNumber')}  {len(got['Line'])} lines  "
              f"{d:,.2f} / {c:,.2f}  SyncToken {got['SyncToken']}")
    print(f"\nrebuilt {len(jobs)} JE(s)")


if __name__ == "__main__":
    main()
