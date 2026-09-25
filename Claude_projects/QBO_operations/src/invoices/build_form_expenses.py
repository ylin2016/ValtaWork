"""Build QuickBooks Expenses for card / bank purchases recorded on the expense form.

The form (../expense_form) records each purchase with its receipt, and after review writes
CSV copies to billing@'s Drive:

    Expense_processing/exports/submissions.csv             kind=expense, status=approved rows
    Expense_processing/exports/attachments.csv             the receipt's final Drive name
    Expense_processing/exports/config/money_accounts.csv   paid-with -> QBO bank / card account
    Expense_processing/exports/config/expense_categories.csv   category -> QBO account
    Expense_processing/exports/config/properties.csv       property -> QBO class

One approved purchase = one Expense (`Purchase`), one line, the shape the card feed's own
Expenses already have (Purchase on CitiCostco3104-Supplies, vendor Costco Wholesale, memo
= the receipt name):

    paid from   money_accounts.qb_account          PaymentType CreditCard for a card, else Cash
    vendor      the store (`store_stuff` before the `_`), through config/payees.yml
                ("Costco" -> "Costco Wholesale"); not a vendor -> *** NO VENDOR *** (blocks)
    line        category account, classed to the property
    location    Trust for a `Trust ...` account, else Valta Realty (what the books do)
    DocNumber   EF<submission id> -- the re-run guard; the poster also refuses the same
                amount from the same account within 3 days (the card feed may have it already)

Not built here: `personal` (Zelle reimbursement -> build_zelle), accounts with in_qbo FALSE
(owner-paid, Valta Homes, Baselane), rows the form flagged `typed_in`, and rows already
written back with a qbo_txn_id.

    python -m src.invoices.build_form_expenses [--exports "<Drive>/Expense_processing/exports"]
        -> review/form_expenses.csv
    python -m src.invoices.post --all --csv review/form_expenses.csv            # dry run
    python -m src.invoices.post --all --csv review/form_expenses.csv --confirm  # WRITES
"""
from __future__ import annotations

import argparse
import csv
from pathlib import Path

import yaml

from ..config import client, payees
from ..paths import CONFIG_DIR, review_csv
from ..resolver import Resolver, esc

DEFAULT_EXPORTS = Path.home() / "Library/CloudStorage/GoogleDrive-billing@valtarealty.com/My Drive/Expense_processing/exports"
OUT = review_csv("form_expenses")
PROPERTY_CLASSES_YML = CONFIG_DIR / "property_classes.yml"

TRUST_LOCATION, COMPANY_LOCATION = "Trust", "Valta Realty"

# post.py's columns, plus PaymentType / Memo and the form's own ids for the reader.
FIELDS = ["PayRef", "DocNumber", "TxnDate", "Vendor", "PaidFrom", "PaymentType", "Location",
          "LineNum", "Amount", "Account", "Class", "Description", "Memo", "Category",
          "Property", "Invoice", "LumpTotal", "SubmissionId", "Store", "Submitter", "ReceiptUrl"]


def read_csv(path: Path) -> list[dict]:
    with open(path, encoding="utf-8-sig", newline="") as fh:
        return [r for r in csv.DictReader(fh) if any((v or "").strip() for v in r.values())]


def truthy(v) -> bool:
    return str(v or "").strip().upper() in ("TRUE", "YES", "1")


def property_aliases() -> dict[str, str]:
    if not PROPERTY_CLASSES_YML.exists():
        return {}
    return (yaml.safe_load(PROPERTY_CLASSES_YML.read_text()) or {}).get("properties", {}) or {}


def vendor_for(store: str, res: Resolver) -> str | None:
    """The store as typed on the form -> a QBO Vendor DisplayName, or None.

    payees.yml first (case-insensitive: the form has "Costco" and "costco"), then the
    store itself as a DisplayName ("target" -> "Target"; QBO's `=` ignores case, so the
    name is read back as QuickBooks spells it).  Never a fuzzy match: a wrong vendor is
    invisible once it is on the books."""
    table = {k.lower(): v for k, v in payees().items()}
    for cand in (table.get(store.lower()), store):
        if cand and res.vendor(cand) is not None:
            got = res.qbo.query(f"SELECT DisplayName FROM Vendor WHERE DisplayName = '{esc(cand)}'")
            return (got.get("QueryResponse", {}).get("Vendor") or [{}])[0].get("DisplayName") or cand
    return None


def class_for(sub: dict, props: dict, aliases: dict, res: Resolver) -> str | None:
    prop = props.get(sub.get("property_code") or "", {})
    name = (sub.get("property_name") or "").strip()
    cands = [(prop.get("qbo_class_name") or "").strip()]
    cands += [aliases[name]] if name in aliases else []
    cands += [f"Listings:{name}", name] if name else []
    for c in cands:
        if c and res.klass(c) is not None:
            return c
    return None


def build(exports: Path, qbo) -> tuple[list[dict], list[str]]:
    subs = read_csv(exports / "submissions.csv")
    files = read_csv(exports / "attachments.csv")
    accounts = {a["code"]: a for a in read_csv(exports / "config" / "money_accounts.csv")}
    cats = {c["code"]: c for c in read_csv(exports / "config" / "expense_categories.csv")}
    props = {p["code"]: p for p in read_csv(exports / "config" / "properties.csv")}
    aliases = property_aliases()
    res = Resolver(qbo)

    first_file: dict[str, dict] = {}
    for f in sorted(files, key=lambda f: int(f.get("seq") or 0)):
        if f.get("upload_status", "uploaded") in ("", "uploaded"):
            first_file.setdefault(f["submission_id"], f)

    rows, warnings, skipped = [], [], {}
    for s in subs:
        if s.get("kind") != "expense" or s.get("status") != "approved":
            continue
        if s.get("qbo_txn_id") or s.get("qbo_posted_at"):
            continue                                        # already written back as posted
        acct = accounts.get(s.get("money_account") or "", {})
        kind = acct.get("kind", "")
        if kind == "personal":
            skipped["paid personally (build_zelle)"] = skipped.get("paid personally (build_zelle)", 0) + 1
            continue
        if not truthy(acct.get("in_qbo")):
            label = acct.get("label") or s.get("money_account") or "(blank)"
            skipped[f"{label} (not in this QBO)"] = skipped.get(f"{label} (not in this QBO)", 0) + 1
            continue
        sid = s["id"]
        if (s.get("typed_in") or "").strip():
            warnings.append(f"submission {sid}: {s['typed_in']} typed in on the form, not mapped -- skipped")
            continue

        amount = round(float(s["amount"]), 2)
        store = (s.get("vendor") or "").strip()
        f = first_file.get(sid, {})
        receipt = Path(f.get("file_name") or "").stem
        desc = receipt or s.get("description") or ""
        if not f:
            warnings.append(f"submission {sid}: no uploaded receipt in attachments.csv")

        paid_from = (acct.get("qb_account") or "").strip()
        if not paid_from:
            paid_from = f"*** NEEDS ACCOUNT paid with {s.get('money_account')} ***"
            warnings.append(f"submission {sid}: money account {s.get('money_account')!r} has no qb_account "
                            f"(Expense Config money_accounts)")
        elif res.account(paid_from) is None:
            warnings.append(f"submission {sid}: paid-from {paid_from!r} is not an account in this company file")
            paid_from = f"*** NEEDS ACCOUNT {paid_from} ***"

        cat = cats.get(s.get("category") or "", {})
        line_acct = (cat.get("qb_account") or "").strip()
        if not line_acct or res.account(line_acct) is None:
            warnings.append(f"submission {sid}: category {s.get('category')!r} -> {line_acct or '(no qb_account)'!r} "
                            f"is not an account in this company file")
            line_acct = f"*** NEEDS ACCOUNT {s.get('category') or '(blank)'} ***"

        cls = class_for(s, props, aliases, res)
        if cls is None:
            warnings.append(f"submission {sid}: no QBO class for property {s.get('property_name')!r} "
                            f"-- add it to config/property_classes.yml")
            cls = f"*** UNMAPPED {s.get('property_name') or '(blank)'} ***"

        vendor = vendor_for(store, res) if store else None
        if vendor is None:
            warnings.append(f"submission {sid}: store {store or '(blank)'!r} is not a QuickBooks vendor "
                            f"-- add `{store}: \"<Vendor DisplayName>\"` to config/payees.yml")
            vendor = f"*** NO VENDOR {store} ***"

        rows.append({
            "PayRef": f"EF{sid}", "DocNumber": f"EF{sid}", "TxnDate": s["txn_date"],
            "Vendor": vendor, "PaidFrom": paid_from,
            "PaymentType": "CreditCard" if kind == "card" else "Cash",
            "Location": TRUST_LOCATION if line_acct.startswith("Trust ") else COMPANY_LOCATION,
            "LineNum": 1, "Amount": f"{amount:.2f}", "Account": line_acct, "Class": cls,
            "Description": desc, "Memo": desc, "Category": s.get("category", ""),
            "Property": s.get("property_name", ""), "Invoice": f.get("file_name", ""),
            "LumpTotal": f"{amount:.2f}", "SubmissionId": sid, "Store": store,
            "Submitter": s.get("submitter", ""), "ReceiptUrl": f.get("drive_url", "")})

    for why, n in sorted(skipped.items()):
        warnings.append(f"not drafted here: {n} x {why}")
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
    blocked = sum(1 for r in rows if "***" in r["Vendor"] + r["Account"] + r["Class"] + r["PaidFrom"])
    print(f"{len(rows)} expense(s), total {sum(float(r['Amount']) for r in rows):,.2f} -> {out}"
          f"{f'   ({blocked} blocked by *** until fixed)' if blocked else ''}")
    for w_ in warnings:
        print(f"  WARN {w_}")
    if rows:
        print(f"next: python -m src.invoices.post --all --csv {out}   (dry run; add --confirm to write)")


if __name__ == "__main__":
    main()
