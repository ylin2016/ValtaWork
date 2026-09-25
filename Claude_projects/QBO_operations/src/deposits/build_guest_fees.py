"""Build the QuickBooks drafts for guest pet / parking fees recorded on the expense form.

The form (../expense_form) records each fee a guest paid by Zelle, Cash App or cash, with the
reservation's confirmation code, and after review writes CSV copies to billing@'s Drive:

    Expense_processing/exports/submissions.csv        kind=deposit, status=approved rows
    Expense_processing/exports/bank_deposits.csv      one row per bank-feed line into 9967
    Expense_processing/exports/config/deposit_types.csv   fee type -> QBO item
    Expense_processing/exports/config/properties.csv      property -> QBO class

Per fee, the reservation's customer is found by its confirmation code (customers are named
`<Guest> - <code>`), then its invoices decide the treatment:

    PAY_EXISTING   an invoice carries the fee item and its Balance covers the fee
                   -> a Payment against that invoice
    NEW_INVOICE    no invoice carries the fee (it was agreed after booking)
                   -> an Invoice for the fee (item Pet Fee (Owner) / Parking Fee, classed to
                      the property: an owner pass-through, no commission), then a Payment
    HELD           the invoice carries the fee but is already paid -- the guest paid twice,
                   or the channel collected it.  Nothing is drafted; the owner decides.
    ERROR          no customer / item / class could be resolved.  Nothing is drafted.

Every Payment goes to **Undeposited Funds**, and `post_guest_fees` then makes ONE Deposit per
bank_deposits row, linking its payments, into Chase Trust Checking 9967 -- the object the bank
feed matches.  A Payment is never deposited straight to 9967 (the $55,972.41 double count).

    python -m src.deposits.build_guest_fees --exports "<Drive>/Expense_processing/exports"
        -> review/guest_fees.csv   (read it, then post_guest_fees --confirm)

Only approved rows with a bank deposit are built.  Cash waits until it is batched into a bank
deposit on the form (not built yet), and a row the form flagged `typed_in` waits until the
reviewer maps it.
"""
from __future__ import annotations

import argparse
import csv
from collections import defaultdict
from pathlib import Path

from ..config import client, name as cfg_name
from ..paths import review_csv
from ..resolver import Resolver, esc

DEFAULT_EXPORTS = Path.home() / "Library/CloudStorage/GoogleDrive-billing@valtarealty.com/My Drive/Expense_processing/exports"
OUT = review_csv("guest_fees")

FIELDS = ["BankDepositId", "BankDate", "BankTotal", "SubmissionId", "TxnDate", "Code", "CheckIn",
          "Customer", "CustomerId", "FeeType", "Item", "Amount", "Treatment", "InvoiceId",
          "InvoiceDoc", "InvoiceBalance", "Property", "Class", "Method", "Payer", "Note"]


def read_csv(path: Path) -> list[dict]:
    with open(path, encoding="utf-8-sig", newline="") as fh:
        return [r for r in csv.DictReader(fh) if any((v or "").strip() for v in r.values())]


def load_exports(exports: Path) -> tuple[list[dict], dict, dict, dict]:
    subs = read_csv(exports / "submissions.csv")
    banks = {b["id"]: b for b in read_csv(exports / "bank_deposits.csv")}
    items = {d["code"]: d for d in read_csv(exports / "config" / "deposit_types.csv")}
    props = {p["code"]: p for p in read_csv(exports / "config" / "properties.csv")}
    return subs, banks, items, props


def customer_invoices(qbo, cid: str) -> list[dict]:
    return qbo.query_all(f"SELECT * FROM Invoice WHERE CustomerRef = '{esc(cid)}'", "Invoice")


def fee_on(invoices: list[dict], item_id: str) -> tuple[dict | None, float]:
    """The invoice carrying the fee item, and the fee's amount on it."""
    for inv in invoices:
        amt = sum(float(l.get("Amount") or 0) for l in inv.get("Line", [])
                  if (l.get("SalesItemLineDetail") or {}).get("ItemRef", {}).get("value") == item_id)
        if amt > 0.005:
            return inv, round(amt, 2)
    return None, 0.0


def treat(sub: dict, qbo, res: Resolver, items: dict, props: dict) -> dict:
    """One review row for one approved deposit submission."""
    code = sub["reservation_code"].strip().upper()
    amount = round(float(sub["amount"]), 2)
    dtype = items.get(sub["deposit_type"], {})
    item_name = (dtype.get("qbo_item") or "").strip()
    prop = props.get(sub["property_code"], {})
    row = {"SubmissionId": sub["id"], "TxnDate": sub["txn_date"], "Code": code,
           "CheckIn": sub.get("check_in_date", ""), "FeeType": dtype.get("label_en") or sub["deposit_type"],
           "Item": item_name, "Amount": f"{amount:.2f}", "Property": sub["property_name"],
           "Method": sub["collection_method"], "Payer": sub["payer_name"], "InvoiceId": "",
           "InvoiceDoc": "", "InvoiceBalance": "", "Note": ""}

    problems = []
    cls = (prop.get("qbo_class_name") or "").strip()
    row["Class"] = cls if cls and res.klass(cls) else f"*** UNMAPPED {sub['property_name'] or '(blank)'} ***"
    if row["Class"].startswith("***"):
        problems.append(f"no QBO class for property {sub['property_name']!r}")
    item_id = res.item(item_name) if item_name else None
    if not item_id:
        problems.append(f"QBO item {item_name or '(none)'!r} for fee type {sub['deposit_type']!r} not found")
    cid = res.customer(f"x - {code}") if code else None
    row["CustomerId"] = cid or ""
    row["Customer"] = "" if not cid else (qbo.query(f"SELECT DisplayName FROM Customer WHERE Id = '{esc(cid)}'")
                                          .get("QueryResponse", {}).get("Customer", [{}])[0].get("DisplayName", ""))
    if not cid:
        problems.append(f"no single QBO customer with confirmation code {code!r}")
    if problems:
        return {**row, "Treatment": "ERROR", "Note": "; ".join(problems)}

    inv, on_invoice = fee_on(customer_invoices(qbo, cid), item_id)
    if inv is None:
        return {**row, "Treatment": "NEW_INVOICE", "Note": f"{item_name} not on the reservation invoice"}
    bal = round(float(inv.get("Balance") or 0), 2)
    row.update({"InvoiceId": inv["Id"], "InvoiceDoc": inv.get("DocNumber", ""), "InvoiceBalance": f"{bal:.2f}"})
    if bal + 0.005 < amount:
        return {**row, "Treatment": "HELD",
                "Note": f"invoice {inv.get('DocNumber') or inv['Id']} already carries {item_name} "
                        f"${on_invoice:.2f} and its open balance is ${bal:.2f} -- paid already? owner decides"}
    note = "" if abs(on_invoice - amount) < 0.005 else f"invoice fee line is ${on_invoice:.2f}, guest paid ${amount:.2f}"
    return {**row, "Treatment": "PAY_EXISTING", "Note": note}


def build(exports: Path, qbo) -> tuple[list[dict], list[str]]:
    subs, banks, items, props = load_exports(exports)
    res = Resolver(qbo)
    rows, warnings = [], []
    for s in subs:
        if s.get("kind") != "deposit" or s.get("status") != "approved":
            continue
        if s.get("qbo_payment_id") or s.get("qbo_posted_at"):
            continue                                        # already written back as posted
        if not s.get("bank_deposit_id"):
            warnings.append(f"submission {s['id']}: {s['collection_method']} ${s['amount']} has no bank "
                            f"deposit yet (cash is batched when it is taken to the bank) -- skipped")
            continue
        if (s.get("typed_in") or "").strip():
            warnings.append(f"submission {s['id']}: {s['typed_in']} typed in on the form, not mapped -- skipped")
            continue
        b = banks.get(s["bank_deposit_id"])
        if not b:
            warnings.append(f"submission {s['id']}: bank deposit {s['bank_deposit_id']} not in bank_deposits.csv -- skipped")
            continue
        r = treat(s, qbo, res, items, props)
        r.update({"BankDepositId": b["id"], "BankDate": b["bank_date"], "BankTotal": f"{float(b['total']):.2f}"})
        rows.append(r)
        if r["Treatment"] in ("ERROR", "HELD"):
            warnings.append(f"submission {s['id']} {r['Code']}: {r['Treatment']} -- {r['Note']}")

    groups = defaultdict(list)
    for r in rows:
        groups[r["BankDepositId"]].append(r)
    for bid, rs in groups.items():
        total = round(sum(float(r["Amount"]) for r in rs), 2)
        if abs(total - float(rs[0]["BankTotal"])) > 0.005:
            warnings.append(f"bank deposit {bid}: fees total ${total:.2f} but the bank line is "
                            f"${rs[0]['BankTotal']} -- it will not be deposited")
    if res.account(cfg_name("undeposited_funds")) is None:
        warnings.append(f"account {cfg_name('undeposited_funds')!r} not found -- fix names.undeposited_funds "
                        f"in config/accounts.yml (python -m src.verify_accounts)")
    return rows, warnings


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--exports", default=str(DEFAULT_EXPORTS),
                    help="the form's exports folder (Drive for desktop path)")
    ap.add_argument("--out", default=str(OUT))
    args = ap.parse_args()
    rows, warnings = build(Path(args.exports), client())
    out = Path(args.out)
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=FIELDS)
        w.writeheader()
        w.writerows(rows)
    by = defaultdict(int)
    for r in rows:
        by[r["Treatment"]] += 1
    print(f"{len(rows)} fee(s) -> {out}   " + ", ".join(f"{k} {v}" for k, v in sorted(by.items())))
    for w_ in warnings:
        print(f"  WARN {w_}")


if __name__ == "__main__":
    main()
