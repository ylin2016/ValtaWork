"""Post journal entries from a review CSV into QuickBooks.

Channel-agnostic: it posts whatever review CSV it is given (Airbnb or Booking.com),
resolving every name to a QBO Id.  Refuses to post when anything is unresolved, when
a line still says NEEDS ACCOUNT, or when the payout is already on the books.

    python -m src.je.post --all                                    # dry run, every JE
    python -m src.je.post --docnum M-JO5Z2IITTUAKU                 # dry run, one
    python -m src.je.post --docnum M-JO5Z2IITTUAKU --confirm       # POST
    python -m src.je.post --csv review/bookingcom_je.csv --all --confirm

The duplicate check is CONTENT-based -- (TxnDate, bank debit) -- not DocNumber.
See `existing_payouts`.
"""
from __future__ import annotations

import argparse
import csv
import json
import sys

from ..config import client, name
from ..paths import review_csv
from ..qbo_client import QBOClient
from ..resolver import Resolver, esc

DEFAULT_CSV = review_csv("airbnb_je")


BANK_ACCT = name("bank_str")


def existing_payouts(qbo: QBOClient, start: str = "2026-01-01",
                     end: str = "2026-12-31") -> dict[tuple[str, float], tuple[str, str]]:
    """(TxnDate, bank debit) -> (DocNumber, Id) for every payout already booked.

    DocNumber alone is NOT a sufficient duplicate check: the same Airbnb payouts have
    also been posted by another system under a YYMMDD-XXXX number, so matching on the
    Airbnb reference code would miss them and double-post the cash.
    """
    index: dict[tuple[str, float], tuple[str, str]] = {}
    for j in qbo.query_all(f"SELECT * FROM JournalEntry WHERE TxnDate >= '{start}' "
                           f"AND TxnDate <= '{end}'", "JournalEntry"):
        for ln in j.get("Line", []):
            det = ln.get("JournalEntryLineDetail")
            if not det:
                continue
            if (det["AccountRef"].get("name") == BANK_ACCT
                    and det["PostingType"] == "Debit"):
                index[(j["TxnDate"], round(ln["Amount"], 2))] = (
                    j.get("DocNumber") or "(no doc no.)", j["Id"])
    return index


def build_payload(rows: list[dict], res: Resolver) -> tuple[dict, list[str]]:
    errs: list[str] = []
    lines = []
    for r in rows:
        acct = res.account(r["Account"])
        if acct is None:
            errs.append(f"line {r['LineNum']}: account not found: {r['Account']!r}")

        detail: dict = {
            "PostingType": "Debit" if r["Debits"] else "Credit",
            "AccountRef": {"value": acct},
        }
        if r["Class"]:
            cid = res.klass(r["Class"])
            if cid is None:
                errs.append(f"line {r['LineNum']}: class not found: {r['Class']!r}")
            detail["ClassRef"] = {"value": cid}
        if r["Location"]:
            did = res.department(r["Location"])
            if did is None:
                errs.append(f"line {r['LineNum']}: location not found: {r['Location']!r}")
            detail["DepartmentRef"] = {"value": did}
        if r["Name"]:
            cust = res.customer(r["Name"])
            if cust is None:
                errs.append(f"line {r['LineNum']}: customer not found: {r['Name']!r}")
            detail["Entity"] = {"Type": "Customer", "EntityRef": {"value": cust}}

        lines.append({
            "DetailType": "JournalEntryLineDetail",
            "Amount": round(float(r["Debits"] or r["Credits"]), 2),
            "Description": r["Description"],
            "JournalEntryLineDetail": detail,
        })

    payload = {
        "DocNumber": rows[0]["JournalNo"],
        "TxnDate": rows[0]["JournalDate"],
        "Adjustment": False,
        "Line": lines,
    }
    dr = sum(l["Amount"] for l in lines if l["JournalEntryLineDetail"]["PostingType"] == "Debit")
    cr = sum(l["Amount"] for l in lines if l["JournalEntryLineDetail"]["PostingType"] == "Credit")
    if abs(dr - cr) > 0.005:
        errs.append(f"UNBALANCED: debits {dr:.2f} vs credits {cr:.2f}")
    return payload, errs


def post_one(docnum: str, rows: list[dict], qbo: QBOClient, res: Resolver,
             posted: dict[tuple[str, float], tuple[str, str]],
             confirm: bool, verbose: bool = True) -> tuple[str, str]:
    """Returns (status, detail).  status in {skipped, error, ready, posted}."""
    bad = [r for r in rows if "NEEDS ACCOUNT" in r["Account"] or "UNMAPPED" in r["Class"]]
    if bad:
        return "error", f"{len(bad)} unresolved line(s) in the review CSV"

    existing = qbo.query(
        f"SELECT * FROM JournalEntry WHERE DocNumber = '{esc(docnum)}'"
    ).get("QueryResponse", {}).get("JournalEntry", [])
    if existing:
        return "skipped", f"DocNumber already in QBO as Id {existing[0]['Id']}"

    bank = [r for r in rows if r["Account"] == BANK_ACCT]
    key = (rows[0]["JournalDate"], round(sum(float(r["Debits"]) for r in bank), 2))
    if key in posted:
        doc, jid = posted[key]
        return "skipped", (f"same payout already booked: {key[0]} ${key[1]:,.2f} "
                           f"on {BANK_ACCT} as {doc} (Id {jid})")

    payload, errs = build_payload(rows, res)
    if errs:
        return "error", "; ".join(errs)
    if verbose:
        print(json.dumps(payload, indent=2))
    if not confirm:
        return "ready", f"{len(payload['Line'])} lines, {sum(l['Amount'] for l in payload['Line'] if l['JournalEntryLineDetail']['PostingType'] == 'Debit'):.2f} Dr"

    je = qbo.post("journalentry", payload)
    posted[key] = (je.get("DocNumber") or docnum, je["Id"])
    return "posted", f"Id={je['Id']} TxnDate={je['TxnDate']} lines={len(je['Line'])}"


def main() -> None:
    ap = argparse.ArgumentParser()
    g = ap.add_mutually_exclusive_group(required=True)
    g.add_argument("--docnum", help="Airbnb reference code / JE DocNumber")
    g.add_argument("--all", action="store_true", help="every JE in the review CSV")
    ap.add_argument("--csv", default=str(DEFAULT_CSV))
    ap.add_argument("--confirm", action="store_true", help="actually POST to QuickBooks")
    ap.add_argument("--start", default="2026-01-01",
                    help="window scanned for already-booked payouts (duplicate check)")
    ap.add_argument("--end", default="2026-12-31")
    args = ap.parse_args()

    all_rows = list(csv.DictReader(open(args.csv, encoding="utf-8")))
    by_doc: dict[str, list[dict]] = {}
    for r in all_rows:
        by_doc.setdefault(r["JournalNo"], []).append(r)

    if args.docnum:
        if args.docnum not in by_doc:
            sys.exit(f"No rows for DocNumber {args.docnum!r} in {args.csv}")
        by_doc = {args.docnum: by_doc[args.docnum]}

    qbo = client()
    res = Resolver(qbo)
    posted = existing_payouts(qbo, args.start, args.end)
    print(f"{len(posted)} payouts already booked on {BANK_ACCT}\n")

    seen_keys: dict[tuple[str, float], str] = {}
    for doc, rows in by_doc.items():
        bank = [r for r in rows if r["Account"] == BANK_ACCT]
        k = (rows[0]["JournalDate"], round(sum(float(r["Debits"]) for r in bank), 2))
        if k in seen_keys:
            print(f"  NOTE {doc} and {seen_keys[k]} share date+amount {k} — "
                  f"the second will be treated as a duplicate and skipped.")
        seen_keys[k] = doc

    tally: dict[str, int] = {}
    failures = []
    for doc, rows in by_doc.items():
        status, detail = post_one(doc, rows, qbo, res, posted, args.confirm,
                                  verbose=bool(args.docnum))
        tally[status] = tally.get(status, 0) + 1
        print(f"  {status.upper():8} {doc:18} {detail}")
        if status == "error":
            failures.append(doc)

    print("\n" + "  ".join(f"{k}={v}" for k, v in sorted(tally.items())))
    if not args.confirm:
        print("DRY RUN — nothing posted. Re-run with --confirm to POST.")
    if failures:
        sys.exit(f"{len(failures)} JE(s) could not be built: {', '.join(failures)}")


if __name__ == "__main__":
    main()
