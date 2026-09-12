"""Post Zelle lump-sum Expenses from a review CSV into QuickBooks.

    python -m src.invoices.post --all                        # dry run, every transfer
    python -m src.invoices.post --ref 20260908_Andea_455.93  # dry run, one
    python -m src.invoices.post --all --confirm              # WRITES

Refuses to post when a line is still unresolved in the review CSV, when the lines
do not add up to the transfer total the owner recorded, or when the transfer is
already on the books.

The duplicate check is CONTENT-based -- (TxnDate, amount paid out of the bank
account) -- not the reference.  The bank feed books the same Zelle transfer as its
own Expense carrying no reference of ours, so a reference check would miss it and
pay the same invoices a second time.  A transfer can also post a day or two after
the date on the sheet, so a near-date hit is reported rather than ignored.
"""
from __future__ import annotations

import argparse
import csv
import json
import sys
from datetime import datetime, timedelta

from ..config import client, location, name as cfg_name
from ..paths import review_csv
from ..qbo_client import QBOClient
from ..resolver import Resolver, esc

DEFAULT_CSV = review_csv("zelle_expenses")

BANK_ACCT = cfg_name("bank_str")
LOCATION = location()
DOCNUMBER_MAX = 21
NEAR_DAYS = 3


def existing_zelle(qbo: QBOClient, start: str = "2026-01-01",
                   end: str = "2026-12-31") -> dict[tuple[str, str, float], tuple[str, str]]:
    """(paid-from account, TxnDate, amount) -> (reference, Id) for every Expense.

    Keyed on the bank as well as date and amount: a transfer can leave the STR trust
    account or the operating account, and the same amount on the same day from the
    OTHER account is a different payment, not a duplicate."""
    index: dict[tuple[str, str, float], tuple[str, str]] = {}
    for p in qbo.query_all(f"SELECT * FROM Purchase WHERE TxnDate >= '{start}' "
                           f"AND TxnDate <= '{end}'", "Purchase"):
        bank = (p.get("AccountRef") or {}).get("name") or ""
        ref = p.get("DocNumber") or p.get("PrivateNote") or "(no reference)"
        index[(bank, p["TxnDate"], round(float(p.get("TotalAmt") or 0), 2))] = (ref[:60], p["Id"])
    return index


def near(bank: str, txndate: str, amount: float,
         booked: dict[tuple[str, str, float], tuple[str, str]]) -> list[tuple[str, str, str]]:
    """Payments of the same amount from the same account within a few days of this one."""
    d0 = datetime.strptime(txndate, "%Y-%m-%d").date()
    hits = []
    for n in range(-NEAR_DAYS, NEAR_DAYS + 1):
        d = (d0 + timedelta(days=n)).isoformat()
        if (bank, d, amount) in booked:
            ref, pid = booked[(bank, d, amount)]
            hits.append((d, ref, pid))
    return hits


def paid_from(rows: list[dict]) -> str:
    return (rows[0].get("PaidFrom") or "").strip() or BANK_ACCT


def docnumber(ref: str, rows: list[dict]) -> str:
    return ((rows[0].get("DocNumber") or "").strip() or ref)[:DOCNUMBER_MAX]


def build_payload(ref: str, rows: list[dict], res: Resolver) -> tuple[dict, list[str]]:
    errs: list[str] = []

    bank_name = paid_from(rows)
    bank = res.account(bank_name)
    if bank is None:
        errs.append(f"paid-from account not found: {bank_name!r}")
    dept = res.department(rows[0]["Location"]) if rows[0]["Location"] else None
    if rows[0]["Location"] and dept is None:
        errs.append(f"location not found: {rows[0]['Location']!r}")

    vendor_name = rows[0]["Vendor"]
    vendor = res.vendor(vendor_name)
    if vendor is None:
        errs.append(f"vendor not found: {vendor_name!r}")

    lines = []
    for r in rows:
        acct = res.account(r["Account"])
        if acct is None:
            errs.append(f"line {r['LineNum']}: account not found: {r['Account']!r}")
        detail: dict = {
            "AccountRef": {"value": acct},
            "BillableStatus": "NotBillable",
            "TaxCodeRef": {"value": "NON"},
        }
        if r["Class"]:
            cid = res.klass(r["Class"])
            if cid is None:
                errs.append(f"line {r['LineNum']}: class not found: {r['Class']!r}")
            detail["ClassRef"] = {"value": cid}
        lines.append({
            "DetailType": "AccountBasedExpenseLineDetail",
            "Amount": round(float(r["Amount"]), 2),
            "Description": r["Description"],
            "AccountBasedExpenseLineDetail": detail,
        })

    total = round(sum(l["Amount"] for l in lines), 2)
    stated = rows[0]["LumpTotal"]
    if not stated:
        errs.append(f"no transfer total (LumpTotal) on the review CSV; lines total {total:,.2f}")
    elif abs(total - round(float(stated), 2)) > 0.005:
        errs.append(f"lines total {total:,.2f} but the transfer was {float(stated):,.2f}")

    payload = {
        "PaymentType": "Cash",
        "AccountRef": {"value": bank},
        "EntityRef": {"value": vendor, "type": "Vendor"},
        "DocNumber": docnumber(ref, rows),
        "TxnDate": rows[0]["TxnDate"],
        "PrivateNote": f"Zelle payment to {vendor_name} {ref}",
        "Line": lines,
    }
    if dept:
        payload["DepartmentRef"] = {"value": dept}
    return payload, errs


def post_one(ref: str, rows: list[dict], qbo: QBOClient, res: Resolver,
             booked: dict[tuple[str, float], tuple[str, str]],
             confirm: bool, verbose: bool = True) -> tuple[str, str]:
    """Returns (status, detail).  status in {skipped, error, ready, posted}."""
    bad = [r for r in rows
           if "NEEDS ACCOUNT" in r["Account"] or "UNMAPPED" in r["Class"]
           or "NO VENDOR" in r["Vendor"]]
    if bad:
        return "error", f"{len(bad)} unresolved line(s) in the review CSV"

    doc = docnumber(ref, rows)
    bank_name = paid_from(rows)
    already = qbo.query(
        f"SELECT * FROM Purchase WHERE DocNumber = '{esc(doc)}'"
    ).get("QueryResponse", {}).get("Purchase", [])
    if already:
        return "skipped", f"DocNumber already in QBO as Id {already[0]['Id']}"

    total = round(sum(float(r["Amount"]) for r in rows), 2)
    hits = near(bank_name, rows[0]["TxnDate"], total, booked)
    if hits:
        d, seen_ref, pid = hits[0]
        same_day = d == rows[0]["TxnDate"]
        return "skipped", (f"${total:,.2f} already paid from {bank_name} on {d}"
                           f"{'' if same_day else ' (within %d days)' % NEAR_DAYS}"
                           f" as {seen_ref} (Id {pid})")

    payload, errs = build_payload(ref, rows, res)
    if errs:
        return "error", "; ".join(errs)
    if verbose:
        print(json.dumps(payload, indent=2))
    if not confirm:
        return "ready", f"{len(payload['Line'])} lines, {total:,.2f}"

    p = qbo.post("purchase", payload)
    booked[(bank_name, p["TxnDate"], round(float(p["TotalAmt"]), 2))] = (doc, p["Id"])
    return "posted", f"Id={p['Id']} TxnDate={p['TxnDate']} lines={len(p['Line'])} ${p['TotalAmt']:,.2f}"


def main() -> None:
    ap = argparse.ArgumentParser()
    g = ap.add_mutually_exclusive_group(required=True)
    g.add_argument("--ref", help="one transfer's 'Zelle content' reference")
    g.add_argument("--all", action="store_true", help="every transfer in the review CSV")
    ap.add_argument("--csv", default=str(DEFAULT_CSV))
    ap.add_argument("--confirm", action="store_true", help="actually POST to QuickBooks")
    ap.add_argument("--start", default="2026-01-01",
                    help="window scanned for already-booked payments (duplicate check)")
    ap.add_argument("--end", default="2026-12-31")
    args = ap.parse_args()

    all_rows = list(csv.DictReader(open(args.csv, encoding="utf-8")))
    by_ref: dict[str, list[dict]] = {}
    for r in all_rows:
        by_ref.setdefault(r["PayRef"], []).append(r)

    if args.ref:
        if args.ref not in by_ref:
            sys.exit(f"No rows for reference {args.ref!r} in {args.csv}")
        by_ref = {args.ref: by_ref[args.ref]}

    qbo = client()
    res = Resolver(qbo)
    booked = existing_zelle(qbo, args.start, args.end)
    print(f"{len(booked)} expenses already on the books (all accounts)\n")

    seen: dict[tuple[str, str, float], str] = {}
    for ref, rows in by_ref.items():
        k = (paid_from(rows), rows[0]["TxnDate"], round(sum(float(r["Amount"]) for r in rows), 2))
        if k in seen:
            print(f"  NOTE {ref} and {seen[k]} share date+amount {k} — "
                  f"the second will be treated as a duplicate and skipped.")
        seen[k] = ref

    tally: dict[str, int] = {}
    failures = []
    for ref, rows in by_ref.items():
        status, detail = post_one(ref, rows, qbo, res, booked, args.confirm,
                                  verbose=bool(args.ref))
        tally[status] = tally.get(status, 0) + 1
        print(f"  {status.upper():8} {ref:24} {detail}")
        if status == "error":
            failures.append(ref)

    print("\n" + "  ".join(f"{k}={v}" for k, v in sorted(tally.items())))
    if not args.confirm:
        print("DRY RUN — nothing posted. Re-run with --confirm to POST.")
    if failures:
        sys.exit(f"{len(failures)} expense(s) could not be built: {', '.join(failures)}")


if __name__ == "__main__":
    main()
