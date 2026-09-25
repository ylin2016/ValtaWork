"""Build the Booking.com commission Expenses from the monthly commission invoice.

    python -m src.invoices.build_bcom_commission                      # newest invoice
    python -m src.invoices.build_bcom_commission --invoice <x.xlsx> --status all
    python -m src.invoices.build_bcom_commission --group batch --date 2026-09-16

Writes `review/bcom_commission_expenses_<period>.csv` and nothing else.  The ordinary
`src.invoices.post` posts it, so the DocNumber check, the content-based duplicate check
and the `--confirm` rule all apply unchanged.

WHY THIS EXISTS -- the bank feed cannot be categorised through the API.  QuickBooks has
no public endpoint for the *For Review* list, so nothing here can reach a downloaded bank
line.  What it can do is put the matching Expense on the books first: the feed then offers
it under **Find match** and the owner accepts it with one click instead of picking an
account and a class 27 times.  Matching keeps the Expense's own date, so a debit that
lands a few days off the invoice's due date still matches cleanly.

    Expense   Dr Fee - Processing & Commission:Fee - Booking.com Commission   Cr Chase Trust 9967

That account is COGS.  A Booking.com bank charge is NEVER a `1C - Owner Expenses` account:
the owner was already charged at reservation time through the `9Z5X_` $0.00 rebill Bill, so
booking it as an owner expense charges them twice.

Three things this gets from the books rather than from a constant:

  * **Booking.com is a CUSTOMER here (Id 471), not a Vendor**, and the Location is
    `Valta Realty`, not Trust.  Every commission Expense already posted carries both;
    the review CSV says so and `invoices/post` honours it.
  * **Only `Paid` rows have left the bank.**  An `Overdue` row is a debit that has not
    happened, so posting it invents cash out.  `--status all` overrides, deliberately.
  * **The class comes from the listing, in two hops** -- Booking.com Property ID ->
    NICKNAME (`config/booking_Id.csv`) -> property_id (`ltr.labels.to_property_id`) ->
    QBO class (the statement project's `mapping_classes.yml`), every hop verified against
    the live company file.  `Cottage 3` and `OSBR 3` are one unit and only that chain
    knows it.  A nickname that reaches no class is written as `*** UNMAPPED ***`, which
    blocks the post -- never guessed.
"""
from __future__ import annotations

import argparse
import csv
import sys
from collections import defaultdict
from pathlib import Path

import yaml

from ..bridge import mapping_classes, to_property_id
from ..config import client, name as cfg_name
from ..paths import review_csv
from ..reconcile.bookingcom_commission import invoice_files, property_nicknames, read_invoice
from ..resolver import Resolver

BCOM_ACCOUNT = "Fee - Processing & Commission:Fee - Booking.com Commission"   # Id 1602
PAYEE = "Booking.com"          # a Customer in this company file, not a Vendor
PAYEE_TYPE = "Customer"
LOCATION = "Valta Realty"      # what the commission Expenses already on the books carry
BANK = cfg_name("bank_str")
UNMAPPED = "*** UNMAPPED: {} ***"

FIELDS = ["PayRef", "DocNumber", "TxnDate", "Vendor", "EntityType", "PaidFrom", "Location",
          "LineNum", "Amount", "Account", "Class", "LineCustomer", "Description",
          "BcomInvoice", "PropertyId", "Nickname", "Status", "Type", "Memo", "LumpTotal"]


def class_resolver(res: Resolver, statements_root: str | None):
    """nickname -> QBO class FullyQualifiedName, or None.

    Direct leaf lookup first, because most nicknames ARE the class name.  Only when that
    misses does it go through the statement project's map -- which is where the
    `Cottage n` -> `OSBR n` rename lives.  The map is never trusted on its own: it
    records some listings one level too shallow, so whatever it returns is put back
    through the company file before it is used.
    """
    to_pid = to_property_id(statements_root)
    by_pid = {e["property_id"]: e["qbo_class_name"]
              for e in yaml.safe_load(mapping_classes(statements_root).read_text())
              if e.get("property_id") and e.get("qbo_class_name")}

    def resolve(nickname: str) -> str | None:
        direct = res.klass_fqn(nickname)
        if direct:
            return direct
        mapped = by_pid.get(to_pid(nickname) or "")
        return res.klass_fqn(mapped) if mapped else None

    return resolve


def description(r: dict, period: str) -> str:
    kind = "" if r["Type"].lower() == "commission" else f" {r['Type']}"
    return f"Booking.com commission{kind} {period} inv {r['Invoice']}"


def build(inv: list[dict], period: str, res: Resolver, statements_root: str | None,
          txndate: str, group: str) -> tuple[list[dict], list[dict]]:
    """(review rows, rows that could not be classed)."""
    nicks = property_nicknames()
    to_class = class_resolver(res, statements_root)
    stamp = txndate.replace("-", "")
    rows, unmapped = [], []

    priced = []
    for r in sorted(inv, key=lambda r: (-r["Amount"], r["Invoice"])):
        nk = (nicks.get(r["PropertyId"]) or [None])[0]
        klass = to_class(nk) if nk else None
        if not klass:
            unmapped.append({**r, "_nick": nk})
        priced.append({**r, "_nick": nk or "", "_class": klass or UNMAPPED.format(nk or r["PropertyName"][:30])})

    if group == "batch":
        total = round(sum(r["Amount"] for r in priced), 2)
        ref = f"{stamp}_BCOM_{total:.2f}"
        for i, r in enumerate(priced, 1):
            rows.append(row(ref, ref[:21], txndate, i, r, period, total,
                            memo=f"Booking.com commission {period}, {len(priced)} properties"))
        return rows, unmapped

    for r in priced:
        ref = f"{stamp}_{r['Invoice']}"
        rows.append(row(ref, ref[:21], txndate, 1, r, period, r["Amount"],
                        memo=f"Booking.com commission {period} "
                             f"{r['_nick'] or r['PropertyName'][:24]} inv {r['Invoice']}"))
    return rows, unmapped


def row(ref: str, doc: str, txndate: str, linenum: int, r: dict, period: str,
        lump: float, memo: str) -> dict:
    return {
        "PayRef": ref, "DocNumber": doc, "TxnDate": txndate,
        "Vendor": PAYEE, "EntityType": PAYEE_TYPE, "PaidFrom": BANK, "Location": LOCATION,
        "LineNum": linenum, "Amount": f"{r['Amount']:.2f}",
        "Account": BCOM_ACCOUNT, "Class": r["_class"], "LineCustomer": PAYEE,
        "Description": description(r, period),
        "BcomInvoice": r["Invoice"], "PropertyId": r["PropertyId"], "Nickname": r["_nick"],
        "Status": r["Status"], "Type": r["Type"], "Memo": memo, "LumpTotal": f"{lump:.2f}",
    }


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--invoice", default=None, help="the commission .xlsx; default = newest")
    ap.add_argument("--status", default="Paid",
                    help="'Paid' (default: only what has actually left the bank), "
                         "'all', or any substring of the invoice's Status column")
    ap.add_argument("--group", default="property", choices=("property", "batch"),
                    help="one Expense per property (default, which is how Booking.com "
                         "debits) or one Expense for the whole invoice")
    ap.add_argument("--date", default=None,
                    help="TxnDate for the Expenses; default = the invoice's Due Date")
    ap.add_argument("--period", default=None, help="override the stay month in descriptions")
    ap.add_argument("--statements-root", default=None)
    ap.add_argument("--out", default=None)
    args = ap.parse_args()

    path = Path(args.invoice) if args.invoice else invoice_files()[-1]
    inv, inv_period = read_invoice(path)
    period = args.period or inv_period
    due = sorted({r["DueDate"] for r in inv if r["DueDate"]})
    txndate = args.date or (due[0] if due else "")
    if not txndate:
        sys.exit("no Due Date on the invoice and no --date given")

    print(f"invoice : {path.name}  ({len(inv)} rows, ${sum(r['Amount'] for r in inv):,.2f})")
    print(f"stays   : {period}   (Booking.com bills in arrears)")
    by_status: dict[str, list] = defaultdict(lambda: [0, 0.0])
    for r in inv:
        by_status[r["Status"]][0] += 1
        by_status[r["Status"]][1] += r["Amount"]
    for s, (n, a) in sorted(by_status.items()):
        print(f"          {s:<32} {n:>3} rows  ${a:>10,.2f}")

    want = [r for r in inv
            if args.status.lower() == "all" or args.status.lower() in r["Status"].lower()]
    if not want:
        sys.exit(f"no invoice rows with status matching {args.status!r}")
    print(f"posting : status ~ {args.status!r} -> {len(want)} rows, "
          f"${sum(r['Amount'] for r in want):,.2f}")
    print(f"dated   : {txndate}" + ("" if args.date else "  (the invoice's Due Date -- pass "
          "--date if the bank debited on another day)"))

    res = Resolver(client())
    rows, unmapped = build(want, period, res, args.statements_root, txndate, args.group)

    exp = len({r["PayRef"] for r in rows})
    total = round(sum(float(r["Amount"]) for r in rows), 2)
    print(f"\n{exp} Expense(s), {len(rows)} line(s), ${total:,.2f} out of {BANK}\n")
    print(f"{'ref':<22}{'class':<30}{'amount':>10}  description")
    for r in rows:
        print(f"{r['PayRef']:<22}{r['Class'][:29]:<30}{float(r['Amount']):>10,.2f}  "
              f"{r['Description']}")

    if unmapped:
        print(f"\n{len(unmapped)} row(s) reach no QuickBooks class -- these BLOCK the post:")
        for r in unmapped:
            print(f"   id={r['PropertyId']:<10} ${r['Amount']:>9,.2f}  "
                  f"nickname={r['_nick']!r}  {r['PropertyName'][:40]}")
        print("   Fix the NICKNAME in config/booking_Id.csv, or add the listing to the "
              "statement project's class map.")

    if args.group == "property":
        print(f"\nThis assumes Booking.com debited each property separately -- {exp} bank "
              f"lines totalling ${total:,.2f},")
        print(f"which is how 2026-08-15 came through.  If the feed shows ONE line of "
              f"${total:,.2f} instead, re-run with --group batch.")

    out = Path(args.out) if args.out else review_csv(f"bcom_commission_expenses_{period}")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=FIELDS)
        w.writeheader()
        w.writerows(rows)
    print(f"\n-> {out}")
    print("NOTHING WAS WRITTEN. Review the CSV, then:")
    print(f"    python -m src.invoices.post --all --csv {out}            # dry run")
    print(f"    python -m src.invoices.post --all --csv {out} --confirm  # WRITES")


if __name__ == "__main__":
    main()
