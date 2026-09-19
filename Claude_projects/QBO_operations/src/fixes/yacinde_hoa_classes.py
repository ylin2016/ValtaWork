"""Move the HOA cleaning lines between the flat `Yacinde HOA` class and listing classes.

`fixes/yacinde_hoa_split` and `invoices/hoa_cleaning` both stamp `Yacinde HOA` on their
lines, because that flat class is the ONLY thing keeping HOA-borne cost off owner
statements: `Owner_statement_whole`'s `qbo_sync` gates on the class alone -- any class in
`mapping_classes.yml` is ingested, with no account filter -- and it categorises
`HOA Receivable - Yacinde` as EXPENSE/"HOA" via the `(?i)hoa` rule.

So the direction matters, and it is not symmetric:

    --to listing   per-unit reporting in QBO.  Owner statements then charge the Yacinde
                   owners for the HOA's cleans until `qbo_sync` gates on the ACCOUNT
                   (only `1A - Net Earnings`, `1C - Owner Expenses` and
                   `2 - Owner Distributions` belong on a statement).
    --to hoa       back to the flat class; owner statements are safe again.

Both directions are one `ClassRef` per line.  Descriptions already carry the unit
(`08/17/2026 B3 Cleaning Fee`, `07/13/2026  Yacinde C1  cleaning …`) and are left alone,
so a flip back and forth is lossless and the unit survives either way.

    python -m src.fixes.yacinde_hoa_classes --to listing            # dry run + review CSV
    python -m src.fixes.yacinde_hoa_classes --to listing --confirm  # write
    python -m src.fixes.yacinde_hoa_classes --to hoa --confirm      # undo

Amounts never move: only ClassRef changes, and the run aborts on the first document whose
TotalAmt shifts.
"""
from __future__ import annotations

import argparse
import csv
import json
import re
import sys
from pathlib import Path

from ..config import client, name as acct_name_of
from ..paths import review_csv
from ..qbo_client import QBOClient
from ..resolver import Resolver

HOA_CLASS = "Yacinde HOA"
VENDOR = "AA Professional Cleaners LLC"
CUSTOMER = "Yacinde HOA"
DOC_PREFIX = "YacindeCL_"

# `07/13/2026 C1 Cleaning Fee` (bill) and `07/13/2026  Yacinde C1  cleaning …` (invoice).
UNIT_RE = re.compile(r"\d{2}/\d{2}/\d{4}\s+(?:Yacinde\s+)?([A-Z]\d)\b")


def unit_of(desc: str) -> str | None:
    m = UNIT_RE.search(desc or "")
    return m.group(1) if m else None


def line_detail(ln: dict) -> dict | None:
    return (ln.get("AccountBasedExpenseLineDetail") or ln.get("SalesItemLineDetail")
            or ln.get("ItemBasedExpenseLineDetail"))


def targets(qbo: QBOClient) -> list[tuple[str, dict]]:
    """(entity, document) for every AA cleaning Bill and Yacinde HOA Invoice."""
    res = Resolver(qbo)
    vid, cid = res.vendor(VENDOR), res.customer(CUSTOMER)
    out: list[tuple[str, dict]] = []
    if vid:
        out += [("bill", b) for b in
                qbo.query_all(f"SELECT * FROM Bill WHERE VendorRef = '{vid}'", "Bill")
                if (b.get("DocNumber") or "").startswith(DOC_PREFIX)]
    if cid:
        out += [("invoice", i) for i in
                qbo.query_all(f"SELECT * FROM Invoice WHERE CustomerRef = '{cid}'", "Invoice")]
    return sorted(out, key=lambda t: (t[0], t[1].get("DocNumber") or ""))


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--to", choices=["listing", "hoa"], required=True)
    ap.add_argument("--out", default=None)
    ap.add_argument("--confirm", action="store_true", help="actually WRITE to QuickBooks")
    args = ap.parse_args()

    qbo = client()
    res = Resolver(qbo)
    hoa_id = res.klass(HOA_CLASS)
    if not hoa_id:
        sys.exit(f"ABORT — class {HOA_CLASS!r} not found")
    recv = acct_name_of("hoa_receivable_yacinde")
    recv_id = res.account(recv)

    docs = targets(qbo)
    print(f"{len(docs)} document(s): "
          f"{sum(1 for e, _ in docs if e == 'bill')} bills, "
          f"{sum(1 for e, _ in docs if e == 'invoice')} invoices")

    rows, edits = [], {}
    unresolved: list[str] = []
    for ent, d in docs:
        per: list[tuple[int, str]] = []
        for n, ln in enumerate(d.get("Line", [])):
            det = line_detail(ln)
            if not det:
                continue
            cur = (det.get("ClassRef") or {}).get("name") or ""
            acct = (det.get("AccountRef") or det.get("ItemAccountRef") or {}).get("name", "")
            on_hoa_class = cur == HOA_CLASS
            # Going back to the flat class, the HOA lines are the ones on the receivable.
            on_receivable = (det.get("AccountRef") or {}).get("value") == recv_id or (
                ent == "invoice")
            want = on_hoa_class if args.to == "listing" else (on_receivable and not on_hoa_class)
            if not want:
                continue
            unit = unit_of(ln.get("Description") or "")
            if args.to == "listing":
                if not unit:
                    unresolved.append(f"{d.get('DocNumber')} line {n+1}: no unit in "
                                      f"{ln.get('Description')!r}")
                    continue
                fqn = res.klass_fqn(f"Yacinde {unit}")
                new_id = res.klass(fqn) if fqn else None
                if not new_id:
                    unresolved.append(f"{d.get('DocNumber')} line {n+1}: no class for "
                                      f"'Yacinde {unit}'")
                    continue
            else:
                fqn, new_id = HOA_CLASS, hoa_id
            per.append((n, new_id))
            rows.append({"Entity": ent, "Id": d["Id"], "DocNumber": d.get("DocNumber", ""),
                         "TxnDate": d.get("TxnDate", ""), "LineNum": n + 1,
                         "Amount": f"{float(ln.get('Amount') or 0):.2f}",
                         "Account": acct, "FromClass": cur, "ToClass": fqn,
                         "Description": ln.get("Description", "")})
        if per:
            edits[(ent, d["Id"])] = per

    if unresolved:
        print(f"\n*** {len(unresolved)} line(s) could not be resolved:")
        for u in unresolved:
            print("   ", u)

    if not rows:
        print("\nnothing to change — already in that state.")
        return

    out = Path(args.out) if args.out else review_csv(f"yacinde_hoa_classes_{args.to}")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)

    print(f"\n{len(rows)} lines on {len(edits)} documents -> {out}")
    for (ent, did), per in edits.items():
        d = next(x for e, x in docs if e == ent and x["Id"] == did)
        amt = sum(float(d["Line"][n].get("Amount") or 0) for n, _ in per)
        print(f"  {ent:<8} {d.get('DocNumber'):<16} {len(per):>2} lines  ${amt:>9,.2f}")
    by_class: dict[str, list[float]] = {}
    for r in rows:
        by_class.setdefault(r["ToClass"], []).append(float(r["Amount"]))
    print("\n  destination classes:")
    for c in sorted(by_class):
        print(f"    {c:<45} {len(by_class[c]):>3} lines  ${sum(by_class[c]):>9,.2f}")
    print(f"    {'TOTAL':<45} {len(rows):>3} lines  "
          f"${sum(float(r['Amount']) for r in rows):>9,.2f}")

    if args.to == "listing":
        print(f"\n  NOTE: these lines sit on {recv}.")
        print("  qbo_sync gates on the CLASS, not the account, and categorises that account")
        print("  as EXPENSE/'HOA' -- so the next statement build charges the Yacinde owners")
        print("  for the HOA's cleans until qbo_sync gates on the account instead.")

    if not args.confirm:
        print("\nDRY RUN — nothing written. Review the CSV, then re-run with --confirm.")
        return
    if unresolved:
        sys.exit(f"ABORT — {len(unresolved)} unresolved line(s); fix them first")

    print(f"\nupdating {len(edits)} documents …")
    for (ent, did), per in edits.items():
        d = next(x for e, x in docs if e == ent and x["Id"] == did)
        body = json.loads(json.dumps(d))
        for n, new_id in per:
            line_detail(body["Line"][n])["ClassRef"] = {"value": new_id}
        body["sparse"] = False
        before = round(float(d.get("TotalAmt") or 0), 2)
        got = qbo.post(ent, body)
        after = round(float(got.get("TotalAmt") or 0), 2)
        if abs(before - after) > 0.005:
            sys.exit(f"ABORT — {ent} {did}: total changed {before:,.2f} -> {after:,.2f}")
        print(f"  {ent:<8} {got.get('DocNumber'):<16} {len(per):>2} lines, "
              f"total unchanged at {after:,.2f}")
    print(f"\nupdated {len(edits)} documents")


if __name__ == "__main__":
    main()
