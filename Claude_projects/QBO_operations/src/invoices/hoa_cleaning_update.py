"""Re-point an existing HOA invoice's CLEANING lines at the allocation workbook.

`invoices/hoa_cleaning` raises the month's invoice once.  When the owner then revises the
allocation -- which is the normal case, the workbook is hand-corrected after it is generated
-- the invoice on the books is stale and `hoa_cleaning` refuses it as a DUPLICATE DocNumber.
This replaces the cleaning lines in place:

    cleaning lines (item `HOA - Cleaning Fee`)   REBUILT from the workbook's `Invoice - HOA`
    every other line (maintenance, pool, wages)  KEPT VERBATIM

Keeping the rest is the point.  Maintenance reaches an invoice from `invoices/hoa_maintenance`,
which reads the RECEIVABLE rather than a workbook, so the workbook cannot reproduce those
lines and rebuilding the whole invoice from it would silently drop them.

The invoice is full-updated: read back and echoed whole, `sparse` false.  An Invoice carries
`BillAddr`, `TxnTaxDetail`, `CustomField`, `LinkedTxn` and per-line `ItemAccountRef` that QBO
blanks if the payload omits them, so only `Line` is touched.

Both halves of the split have to move together.  Lowering the HOA's share here does NOT move
the cost back onto the owner -- the expense lines still sit on `HOA Receivable - Yacinde`, and
the receivable simply grows by the difference.  That growth is the control account doing its
job: it says money is laid out and not billed on.  The matching expense-side repoint is
`fixes/yacinde_hoa_resplit`, and this script prints what the receivable will become so the gap
is visible before it is created, never after.

    python -m src.invoices.hoa_cleaning_update --period 202607            # dry run + CSV
    python -m src.invoices.hoa_cleaning_update                            # every workbook
    python -m src.invoices.hoa_cleaning_update --period 202607 --confirm  # write
"""
from __future__ import annotations

import argparse
import csv
import json
import sys
from pathlib import Path

from ..config import client, name as acct_name_of
from ..paths import review_csv
from ..qbo_client import QBOClient
from ..resolver import Resolver
from .hoa_cleaning import CUSTOMER, HOA_CLASS, describe, month_end, read_period, workbooks

# The items were renamed after the first invoices were posted (`HOA Reimbursement - Cleaning`
# -> this).  Both still credit the receivable.  Match on the LEAF so a further re-parent
# under `Owner Charges:` does not break the partition.
CLEANING_ITEM = "Owner Charges:HOA - Cleaning Fee"
CLEANING_LEAF = "HOA - Cleaning Fee"


def is_cleaning(line: dict, item_id: str) -> bool:
    d = line.get("SalesItemLineDetail")
    if not d:
        return False
    ref = d.get("ItemRef", {})
    return ref.get("value") == item_id or str(ref.get("name", "")).endswith(CLEANING_LEAF)


def amount(line: dict) -> float:
    return round(float(line.get("Amount") or 0), 2)


def unit_classes(qbo: QBOClient, res: Resolver, units: list[str]) -> dict[str, str | None]:
    """unit -> class Id, by leaf name.  The cleaning lines carry the LISTING class (owner
    decision 2026-09-17, for per-unit reporting), not the flat `Yacinde HOA` one."""
    out = {}
    for u in units:
        fqn = res.klass_fqn(u)
        out[u] = res.klass(fqn) if fqn else None
    return out


def rebuild(inv: dict, cleans: list[dict], item_id: str, classes: dict[str, str | None]) -> dict:
    """The invoice echoed back with cleaning lines replaced and everything else untouched."""
    body = json.loads(json.dumps(inv))
    old = body["Line"]
    kept = [l for l in old if not is_cleaning(l, item_id)]
    subtotal = [l for l in kept if l.get("DetailType") == "SubTotalLineDetail"]
    kept = [l for l in kept if l.get("DetailType") != "SubTotalLineDetail"]

    made = []
    for n, c in enumerate(cleans, start=1):
        made.append({
            "LineNum": n,
            "Description": describe(c),
            "Amount": c["Amount"],
            "DetailType": "SalesItemLineDetail",
            "SalesItemLineDetail": {
                "ItemRef": {"value": item_id},
                "ClassRef": {"value": classes[c["Unit"]]},
                "Qty": 1,
                "UnitPrice": c["Amount"],
                "TaxCodeRef": {"value": "NON"},
            },
        })
    for k, l in enumerate(kept, start=len(made) + 1):
        l["LineNum"] = k
    body["Line"] = made + kept + subtotal
    body["sparse"] = False
    return body, kept


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--period", default=None, help="YYYYMM; default = every workbook found")
    ap.add_argument("--out", default=None)
    ap.add_argument("--confirm", action="store_true", help="actually WRITE to QuickBooks")
    args = ap.parse_args()

    books = workbooks(args.period)
    if not books:
        sys.exit(f"no workbooks matching period {args.period!r}")

    qbo = client()
    res = Resolver(qbo)
    acct = acct_name_of("hoa_receivable_yacinde")
    cust_id = res.customer(CUSTOMER)
    item_id = res.item(CLEANING_ITEM)
    recv_id = res.account(acct)
    print("resolving:")
    for label, got in ((f"Customer {CUSTOMER!r}", cust_id), (f"Item {CLEANING_ITEM!r}", item_id),
                       (f"Account {acct!r}", recv_id)):
        print(f"  {label:<62} {'Id ' + got if got else '*** MISSING ***'}")
    for k, v in (("customer", cust_id), ("item", item_id), ("account", recv_id)):
        if not v:
            sys.exit(f"ABORT — {k} not found; nothing to update against")

    recv_before = float(qbo.query(f"SELECT * FROM Account WHERE Id = '{recv_id}'")
                        ["QueryResponse"]["Account"][0].get("CurrentBalance") or 0)
    print(f"\n{acct}\n  balance now  {recv_before:>12,.2f}")

    rows, jobs, swing = [], [], 0.0
    for b in books:
        period, cleans = read_period(b)
        doc = f"HOA-{period}"
        found = qbo.query_all(
            f"SELECT * FROM Invoice WHERE CustomerRef = '{cust_id}' AND DocNumber = '{doc}'",
            "Invoice")
        print(f"\n{'='*78}\n{b.name}  ->  {doc}")
        if len(found) != 1:
            print(f"  *** SKIP — {len(found)} invoice(s) with DocNumber {doc}; "
                  f"use invoices.hoa_cleaning to raise it ***")
            continue
        inv = found[0]
        want = round(sum(c["Amount"] for c in cleans), 2)

        classes = unit_classes(qbo, res, sorted({c["Unit"] for c in cleans}))
        bad = [u for u, v in classes.items() if not v]
        for u in sorted(classes):
            print(f"    class {u:<14} {'Id ' + classes[u] if classes[u] else '*** UNMAPPED ***'}")
        if bad:
            print(f"  *** SKIP — unmapped class(es): {', '.join(bad)} ***")
            continue

        body, kept = rebuild(inv, cleans, item_id, classes)
        old_lines = inv["Line"]
        old_clean = [l for l in old_lines if is_cleaning(l, item_id)]
        old_clean_tot = round(sum(map(amount, old_clean)), 2)
        kept_tot = round(sum(map(amount, kept)), 2)
        old_tot = round(float(inv.get("TotalAmt") or 0), 2)
        new_tot = round(want + kept_tot, 2)
        delta = round(new_tot - old_tot, 2)
        swing += delta

        print(f"\n  TxnDate {inv['TxnDate']}   Balance {float(inv.get('Balance') or 0):,.2f}"
              f"   SyncToken {inv['SyncToken']}"
              + (f"   LinkedTxn {[t.get('TxnType') for t in inv.get('LinkedTxn', [])]}"
                 if inv.get("LinkedTxn") else ""))
        print(f"    cleaning   {len(old_clean):>3} lines {old_clean_tot:>11,.2f}"
              f"   ->  {len(cleans):>3} lines {want:>11,.2f}"
              f"   ({want - old_clean_tot:+,.2f})")
        print(f"    kept       {len(kept):>3} lines {kept_tot:>11,.2f}   (unchanged)")
        for l in kept:
            print(f"        KEEP  {amount(l):>9,.2f}  "
                  f"{l.get('SalesItemLineDetail',{}).get('ItemRef',{}).get('name')}"
                  f"  | {(l.get('Description') or '')[:58]}")
        print(f"    TOTAL          {old_tot:>11,.2f}   ->      {new_tot:>11,.2f}   ({delta:+,.2f})")

        # line-level diff on (date, unit, amount) -- what the owner actually re-decided
        def key(c):
            return (c["Date"], c["Unit"], c["Amount"])
        was = {}
        for l in old_clean:
            d = (l.get("Description") or "")
            dt_ = d[6:10] + "-" + d[:2] + "-" + d[3:5] if len(d) > 10 and d[2] == "/" else "?"
            u = " ".join(d[12:].split()[:2]) if len(d) > 12 else "?"
            was.setdefault((dt_, u, amount(l)), 0)
            was[(dt_, u, amount(l))] += 1
        now = {}
        for c in cleans:
            now.setdefault(key(c), 0)
            now[key(c)] += 1
        removed = {k: v - now.get(k, 0) for k, v in was.items() if v > now.get(k, 0)}
        added = {k: v - was.get(k, 0) for k, v in now.items() if v > was.get(k, 0)}
        if removed:
            print(f"\n    dropped from the HOA ({sum(removed.values())} clean(s), "
                  f"${sum(k[2]*v for k, v in removed.items()):,.2f}) — these now sit on owners:")
            for k in sorted(removed):
                print(f"        - {k[0]}  {k[1]:<14} {k[2]:>8,.2f}" +
                      (f"  x{removed[k]}" if removed[k] > 1 else ""))
        if added:
            print(f"\n    added to the HOA ({sum(added.values())} clean(s), "
                  f"${sum(k[2]*v for k, v in added.items()):,.2f}):")
            for k in sorted(added):
                print(f"        + {k[0]}  {k[1]:<14} {k[2]:>8,.2f}" +
                      (f"  x{added[k]}" if added[k] > 1 else ""))

        for n, c in enumerate(cleans, start=1):
            rows.append({"DocNumber": doc, "InvoiceId": inv["Id"], "TxnDate": inv["TxnDate"],
                         "Action": "CLEANING (rebuilt)", "LineNum": n, "CleanDate": c["Date"],
                         "Unit": c["Unit"], "Amount": f"{c['Amount']:.2f}",
                         "AAInvoice": c["Invoice"], "Item": CLEANING_ITEM,
                         "Class": res.klass_fqn(c["Unit"]), "CreditsAccount": acct,
                         "Description": describe(c), "OldTotal": f"{old_tot:.2f}",
                         "NewTotal": f"{new_tot:.2f}", "Delta": f"{delta:.2f}",
                         "Source": b.name})
        for n, l in enumerate(kept, start=len(cleans) + 1):
            rows.append({"DocNumber": doc, "InvoiceId": inv["Id"], "TxnDate": inv["TxnDate"],
                         "Action": "KEPT (untouched)", "LineNum": n, "CleanDate": "",
                         "Unit": "", "Amount": f"{amount(l):.2f}", "AAInvoice": "",
                         "Item": l.get("SalesItemLineDetail", {}).get("ItemRef", {}).get("name", ""),
                         "Class": l.get("SalesItemLineDetail", {}).get("ClassRef", {}).get("name", ""),
                         "CreditsAccount": acct, "Description": l.get("Description") or "",
                         "OldTotal": f"{old_tot:.2f}", "NewTotal": f"{new_tot:.2f}",
                         "Delta": f"{delta:.2f}", "Source": "on the invoice already"})
        jobs.append((doc, inv, body, new_tot, delta))

    if not rows:
        sys.exit("\nnothing to update")

    out = Path(args.out) if args.out else review_csv("hoa_cleaning_update")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)

    print(f"\n{'='*78}\n{len(jobs)} invoice(s), {len(rows)} lines -> {out}")
    print(f"  invoiced to the HOA changes by {swing:+,.2f}")
    print(f"  {acct}:  {recv_before:,.2f}  ->  {recv_before - swing:,.2f}")
    if swing < 0:
        # Stated as a condition, not a fact: this script cannot see the expense side, and
        # asserting the repoint is outstanding reads as an error when it has already run.
        print(f"\n  *** {-swing:,.2f} of cleaning stops being the HOA's here. It reaches an owner\n"
              f"      only if the matching expense lines have been repointed off the receivable.\n"
              f"      If fixes/yacinde_hoa_resplit has NOT been run for these periods, run it --\n"
              f"      otherwise that amount strands on the receivable. The balance above is the\n"
              f"      projection for THIS script alone; the two together leave it unchanged. ***")

    if not args.confirm:
        print("\nDRY RUN — nothing written. Review the CSV, then re-run with --confirm.")
        return

    for doc, inv, body, new_tot, delta in jobs:
        got = qbo.post("invoice", body)
        after = round(float(got.get("TotalAmt") or 0), 2)
        if abs(after - new_tot) > 0.005:
            sys.exit(f"ABORT — {doc}: posted {after:,.2f}, expected {new_tot:,.2f}")
        print(f"  Invoice {got['Id']}  {got.get('DocNumber')}  "
              f"${after:,.2f}  ({len(got.get('Line', []))} lines)  SyncToken {got['SyncToken']}")
    print(f"\nupdated {len(jobs)} invoice(s)")


if __name__ == "__main__":
    main()
