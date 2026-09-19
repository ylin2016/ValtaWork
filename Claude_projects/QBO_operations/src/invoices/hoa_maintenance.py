"""Add non-cleaning HOA costs (pool chemicals, maintenance) to a Yacinde HOA invoice.

`invoices/hoa_cleaning` bills the cleaning the HOA bears, off the allocation workbook.
This is the other kind of HOA cost -- pool chlorine, muriatic acid, a pool hook -- which
has no workbook behind it and arrives one Amazon purchase at a time.

**The receivable IS the source of truth here.**  There is no list to maintain: you
categorise the purchase to `HOA Receivable - Yacinde` in QuickBooks, and that line is by
definition money laid out on the HOA's behalf and not yet billed on.  This script reads
the account's open debits and turns them into invoice lines:

    Purchase  Dr HOA Receivable - Yacinde   Cr the card/bank that paid   (you, in the UI)
    Invoice   Dr Accounts Receivable        Cr HOA Receivable            (here)
    Payment   Dr the bank                   Cr Accounts Receivable       (you, in the UI)

so the account nets to zero once everything laid out has been invoiced.  A line still on
`Maintenance - Owner` is NOT picked up, and should not be: until it is repointed it is an
owner's cost, and invoicing it would credit an asset that was never debited -- driving the
receivable negative while the owners keep carrying the money.

Cleaning is skipped explicitly (vendor AA Professional Cleaners, `YacindeCL_` bills):
`invoices/hoa_cleaning` owns those and double-billing them is the obvious failure mode.

    python -m src.invoices.hoa_maintenance --into HOA-2026-08            # dry run + CSV
    python -m src.invoices.hoa_maintenance --into HOA-2026-08 --confirm  # write
    python -m src.invoices.hoa_maintenance --start 2026-09-01 --end 2026-09-30

`--into` APPENDS to an existing invoice, read back and echoed whole: QBO full-updates
blank every field the payload omits, and an Invoice carries BillAddr, TxnTaxDetail,
CustomField and per-line ItemAccountRef that are invisible until they are gone.  The run
aborts unless the new total is the old total plus exactly what was added.

The duplicate check is CONTENT-based, per this project's rule: a source line whose
description is already on one of the HOA's invoices is skipped, whatever invoice it is on.
"""
from __future__ import annotations

import argparse
import csv
import json
import sys
from pathlib import Path

from ..config import client, location, name as acct_name_of
from ..paths import review_csv
from ..qbo_client import QBOClient
from ..resolver import Resolver

CUSTOMER = "Yacinde HOA"
HOA_CLASS = "Yacinde HOA"
ITEM = "Owner Charges:HOA Reimbursement - Maintenance"

# Cleaning belongs to invoices/hoa_cleaning; billing it from here too would double it.
CLEANING_VENDOR = "AA Professional Cleaners LLC"
CLEANING_DOC_PREFIX = "YacindeCL_"


def line_detail(ln: dict) -> dict | None:
    return (ln.get("AccountBasedExpenseLineDetail") or ln.get("ItemBasedExpenseLineDetail")
            or ln.get("JournalEntryLineDetail") or ln.get("SalesItemLineDetail"))


def open_debits(qbo: QBOClient, recv_id: str, start: str, end: str) -> list[dict]:
    """Lines that DEBIT the receivable in the window -- money laid out, cleaning excluded."""
    out = []
    for ent in ("Purchase", "Bill", "JournalEntry"):
        q = (f"select * from {ent} where TxnDate >= '{start}' and TxnDate <= '{end}'")
        for d in qbo.query_all(q, ent):
            payee = (d.get("EntityRef") or d.get("VendorRef") or {}).get("name", "")
            if payee == CLEANING_VENDOR or (d.get("DocNumber") or "").startswith(
                    CLEANING_DOC_PREFIX):
                continue
            for n, ln in enumerate(d.get("Line", [])):
                det = line_detail(ln)
                if not det or (det.get("AccountRef") or {}).get("value") != recv_id:
                    continue
                # A JE can sit on either side; only a DEBIT is money laid out.
                if ent == "JournalEntry" and det.get("PostingType") != "Debit":
                    continue
                amt = round(float(ln.get("Amount") or 0), 2)
                if amt <= 0:
                    continue
                out.append({"Entity": ent, "Id": d["Id"], "DocNumber": d.get("DocNumber") or "",
                            "TxnDate": d.get("TxnDate", ""), "LineNum": n + 1, "Payee": payee,
                            "Amount": amt, "PaidFrom": (d.get("AccountRef") or {}).get("name", ""),
                            "Class": (det.get("ClassRef") or {}).get("name", ""),
                            "Description": (ln.get("Description") or "").replace("\n", " ").strip()})
    out.sort(key=lambda r: (r["TxnDate"], r["Amount"]))
    return out


def invoiced_descriptions(qbo: QBOClient, customer_id: str) -> tuple[set[str], list[dict]]:
    invs = qbo.query_all(f"select * from Invoice where CustomerRef = '{customer_id}'", "Invoice")
    seen = {(l.get("Description") or "").strip()
            for i in invs for l in i.get("Line", []) if l.get("SalesItemLineDetail")}
    return seen - {""}, invs


def append_lines(inv: dict, new: list[dict], item_id: str, class_id: str) -> dict:
    """Echo the invoice back with the new lines inserted before its SubTotal line."""
    body = json.loads(json.dumps(inv))
    lines = body["Line"]
    at = next((i for i, l in enumerate(lines)
               if l.get("DetailType") == "SubTotalLineDetail"), len(lines))
    nextnum = max((l.get("LineNum") or 0) for l in lines) + 1
    made = []
    for k, r in enumerate(new):
        made.append({
            "LineNum": nextnum + k,
            "Description": r["Description"],
            "Amount": r["Amount"],
            "DetailType": "SalesItemLineDetail",
            "SalesItemLineDetail": {
                "ItemRef": {"value": item_id},
                "ClassRef": {"value": class_id},
                "Qty": 1,
                "UnitPrice": r["Amount"],
                "TaxCodeRef": {"value": "NON"},
            },
        })
    body["Line"] = lines[:at] + made + lines[at:]
    body["sparse"] = False
    return body


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--start", default="2026-01-01")
    ap.add_argument("--end", default="2026-12-31")
    ap.add_argument("--into", default=None,
                    help="DocNumber of an existing HOA invoice to append to, e.g. HOA-2026-08")
    ap.add_argument("--out", default=None)
    ap.add_argument("--create-missing", action="store_true",
                    help="with --confirm, create the Item if it does not exist")
    ap.add_argument("--confirm", action="store_true", help="actually WRITE to QuickBooks")
    args = ap.parse_args()

    qbo = client()
    res = Resolver(qbo)
    recv = acct_name_of("hoa_receivable_yacinde")
    recv_id = res.account(recv)
    if not recv_id:
        sys.exit(f"ABORT — {recv!r} does not exist")
    cust_id = res.customer(CUSTOMER)
    item_id = res._one("Item", "FullyQualifiedName", ITEM)
    class_id = res.klass(HOA_CLASS)

    print("resolving:")
    for label, got in ((f"Account {recv!r}", recv_id), (f"Customer {CUSTOMER!r}", cust_id),
                       (f"Item {ITEM!r}", item_id), (f"Class {HOA_CLASS!r}", class_id),
                       (f"Location {location()!r}", res.department(location()))):
        print(f"  {label:<64} {'Id ' + got if got else '*** MISSING ***'}")
    if not cust_id:
        sys.exit(f"ABORT — Customer {CUSTOMER!r} does not exist")

    debits = open_debits(qbo, recv_id, args.start, args.end)
    seen, invs = invoiced_descriptions(qbo, cust_id)
    fresh = [r for r in debits if r["Description"] not in seen]
    dupes = [r for r in debits if r["Description"] in seen]

    print(f"\n{len(debits)} line(s) debit {recv.rsplit(':', 1)[-1]} "
          f"between {args.start} and {args.end} (cleaning excluded)")
    for r in debits:
        mark = "ALREADY INVOICED" if r["Description"] in seen else ""
        print(f"  {r['TxnDate']}  {r['Entity']:<9} {r['Id']:<7} {r['Amount']:>9,.2f}  "
              f"{r['Class'] or '(no class)':<14}  {r['Description'][:56]}  {mark}")
    if dupes:
        print(f"\n  {len(dupes)} skipped as already on an HOA invoice")
    if not fresh:
        print("\nnothing to bill — the receivable has no uninvoiced debits in this window.")
        return

    total = round(sum(r["Amount"] for r in fresh), 2)
    print(f"\n{len(fresh)} line(s) to bill, ${total:,.2f}")

    target = None
    if args.into:
        target = next((i for i in invs if i.get("DocNumber") == args.into), None)
        if not target:
            sys.exit(f"ABORT — no invoice {args.into!r} on {CUSTOMER}")
        before = round(float(target.get("TotalAmt") or 0), 2)
        print(f"  appending to {args.into} (Id {target['Id']}, {before:,.2f} -> "
              f"{before + total:,.2f}), balance now {float(target.get('Balance') or 0):,.2f}")
        if float(target.get("Balance") or 0) < before - 0.005:
            print("  NOTE: this invoice is partly or fully PAID — changing a paid invoice "
                  "alters what the HOA already settled.")
    else:
        print("  no --into given; pass --into <DocNumber> to append to an existing invoice")

    rows = [{**r, "Item": ITEM, "ToClass": HOA_CLASS, "CreditsAccount": recv,
             "IntoInvoice": args.into or ""} for r in fresh]
    out = Path(args.out) if args.out else review_csv("hoa_maintenance")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    print(f"\nreview CSV -> {out}")
    print(f"each line debits Accounts Receivable and credits {recv}")

    if not args.confirm:
        print("\nDRY RUN — nothing written. Review the CSV, then re-run with --confirm.")
        if not item_id:
            print(f"\n  Item {ITEM!r} does not exist — create it "
                  f"(Service, income account = {recv}) or pass --create-missing.")
        return

    if not target:
        sys.exit("ABORT — --confirm needs --into <DocNumber>")
    if not class_id:
        sys.exit(f"ABORT — class {HOA_CLASS!r} must exist before posting")
    if not item_id:
        if not args.create_missing:
            sys.exit(f"ABORT — Item {ITEM!r} does not exist; create it or pass --create-missing")
        item_id = qbo.post("item", {
            "Name": ITEM.rsplit(":", 1)[-1], "Type": "Service", "Taxable": False,
            "ParentRef": {"value": res._one("Item", "FullyQualifiedName", "Owner Charges")},
            "SubItem": True,
            "IncomeAccountRef": {"value": recv_id},
            "Description": "Maintenance paid by Valta on the HOA's behalf, rebilled at cost.",
        })["Id"]
        print(f"created Item {ITEM!r}: Id {item_id}")

    before = round(float(target.get("TotalAmt") or 0), 2)
    got = qbo.post("invoice", append_lines(target, fresh, item_id, class_id))
    after = round(float(got.get("TotalAmt") or 0), 2)
    if abs(after - (before + total)) > 0.005:
        sys.exit(f"ABORT — {args.into}: total {before:,.2f} + {total:,.2f} should be "
                 f"{before + total:,.2f}, QuickBooks returned {after:,.2f}")
    print(f"\nInvoice {got['Id']}  {got.get('DocNumber')}  "
          f"{before:,.2f} + {total:,.2f} = {after:,.2f}  "
          f"({len([l for l in got.get('Line', []) if l.get('SalesItemLineDetail')])} lines)")


if __name__ == "__main__":
    main()
