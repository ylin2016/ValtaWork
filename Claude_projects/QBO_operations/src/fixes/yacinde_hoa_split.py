"""Split the AA Professional Cleaners bills between the owner and the Yacinde HOA.

Every line of every `YacindeCL_*` Bill posts to
`Trust Liabilities:Owner Payables:1C - Owner Expenses:Cleaning Expense - Owner`, so
100% of the cleaning is charged to owners.  The HOA pays a share of it (4 cleans per
unit per month on whole-owner units, 1 per fraction week on fractional ones), and the
HOA is a third party that REIMBURSES -- not an owner whose payable can be charged.
Its share is an asset until the HOA is invoiced and pays:

    owner's clean   1C - Owner Expenses:Cleaning Expense - Owner   class = the listing
    HOA's clean     HOA Receivable - Yacinde                       class = Yacinde HOA

The class matters as much as the account.  `Owner_statement_whole/src/expense/qbo_sync.py`
ingests EVERY Bill line carrying a mapped class and filters on nothing else -- no account
filter -- so a listing class on an HOA line charges that owner for a clean the HOA is
paying for, whatever account it points at.  `Yacinde HOA` (Id 1000000041) is deliberately
not under `Listings:` and not in the statement project's `mapping_classes.yml`, so those
lines land in its exceptions table as CLASS_NOT_MAPPED and reach no statement.  The unit
goes into the Description instead, the way the two already-converted lines on Bill 116622
write it: `07/13/2026 C1 Cleaning Fee`.

Which cleans are the HOA's comes from the allocation workbooks' `Invoice - HOA` tab --
that project's own output, after the owner's hand corrections.  Nothing is inferred here.

    python -m src.fixes.yacinde_hoa_split                       # dry run + review CSV
    python -m src.fixes.yacinde_hoa_split --confirm             # write
    python -m src.fixes.yacinde_hoa_split --workbook a.xlsx --workbook b.xlsx

Amounts never move: only AccountRef, ClassRef and Description change, and the run aborts
on the first Bill whose TotalAmt shifts.  Re-running is safe -- a line already pointing at
the receivable account is left alone, so an interrupted run just picks up the rest.
"""
from __future__ import annotations

import argparse
import csv
import json
import re
import sys
from collections import Counter
from pathlib import Path

import openpyxl

from ..config import client, name as acct_name_of
from ..paths import review_csv
from ..qbo_client import QBOClient
from ..resolver import Resolver

VENDOR = "AA Professional Cleaners LLC"
DOC_PREFIX = "YacindeCL_"

# The class every HOA line is stamped with.  Flat on purpose: anything under `Listings:`
# is mapped to a property by the statement project and would reach an owner's statement.
HOA_CLASS = "Yacinde HOA"

# The allocation workbooks, newest run of the yacinde_expense project.
DEFAULT_WORKBOOKS = sorted(
    (Path(__file__).resolve().parents[3] / "yacinde_expense" / "output").glob(
        "yacinde_expense_allocation_2026??.xlsx"))

HOA_SHEET = "Invoice - HOA"

# `07/13/2026 Cleaning Fee`, and the converted form `07/13/2026 C1 Cleaning Fee`.
#
# DOTALL and no `$`: one description carries an embedded newline
# ("07/24/2026 Cleaning_Intensive cleaning do to marijuana\nsmoked"), and `.*$` stopped
# at it, so the whole match failed and that line's date came back None -- the $250 B6
# clean then matched no bill line at all.
DESC_RE = re.compile(r"^\s*(\d{2})/(\d{2})/(\d{4})\s*(?:([A-Z]\d)\s+)?(.*)", re.S)


def parse_desc(desc: str) -> tuple[str | None, str | None, str]:
    """(iso date, unit code already in the text, the rest) from a bill line description."""
    m = DESC_RE.match(desc or "")
    if not m:
        return None, None, (desc or "").strip()
    return f"{m.group(3)}-{m.group(1)}-{m.group(2)}", m.group(4), m.group(5).strip()


def line_unit(det: dict, desc_unit: str | None) -> str:
    """`Yacinde C1` for a line, from its listing class or -- once converted -- its text."""
    leaf = ((det.get("ClassRef") or {}).get("name") or "").rsplit(":", 1)[-1].strip()
    if leaf and leaf != HOA_CLASS:
        return leaf
    return f"Yacinde {desc_unit}" if desc_unit else ""


def read_hoa_rows(paths: list[Path]) -> list[dict]:
    """The HOA-paid cleans: one row per clean off each workbook's `Invoice - HOA` tab.

    The tab's last row is a TOTAL; it is not a clean and must not be matched to a line.
    """
    out: list[dict] = []
    for p in paths:
        wb = openpyxl.load_workbook(p, data_only=True, read_only=True)
        if HOA_SHEET not in wb.sheetnames:
            sys.exit(f"{p.name}: no {HOA_SHEET!r} sheet")
        ws = wb[HOA_SHEET]
        rows = ws.iter_rows(values_only=True)
        hdr = [c for c in next(rows)]
        for r in rows:
            d = dict(zip(hdr, r))
            unit = str(d.get("unit") or "").strip()
            if not unit or unit.upper() == "TOTAL" or d.get("date") is None:
                continue
            amt = round(float(d.get("amount") or 0), 2)
            if amt <= 0:                       # the $0 "fraction week, no clean" markers
                continue
            out.append({
                "Source": p.name,
                "Invoice": str(d.get("aa_invoice") or "").strip(),
                "Unit": unit,
                "Date": str(d.get("date"))[:10],
                "Amount": amt,
                "Notes": str(d.get("notes") or "").strip(),
            })
    return out


def scan_bills(qbo: QBOClient) -> list[dict]:
    """Every AA Professional Cleaners bill, newest first, with its lines indexed."""
    vid = Resolver(qbo).vendor(VENDOR)
    if vid is None:
        sys.exit(f"vendor {VENDOR!r} not found")
    bills = [b for b in qbo.query_all(f"SELECT * FROM Bill WHERE VendorRef = '{vid}'", "Bill")
             if (b.get("DocNumber") or "").startswith(DOC_PREFIX)]
    return sorted(bills, key=lambda b: b.get("TxnDate") or "")


def match(hoa: list[dict], bills: list[dict], target_acct_id: str | None) -> tuple[list[dict], list[dict]]:
    """Pair each HOA clean with one bill line.  Returns (rows, unmatched).

    Exact (invoice, unit, date, amount) first.  A leftover then relaxes the UNIT only:
    the allocation workbook and the bill disagree on which unit two August cleans belong
    to (08/20 F1-vs-C1, 08/22 F5-vs-B3), and the two-way reconciliation is otherwise
    exact -- 83 lines and $14,760.00 on both sides -- so the clean is the same clean and
    the HOA pays it either way.  Only the description text is in doubt, so the pairing is
    made and flagged rather than dropped.  The date is never relaxed.
    """
    avail: dict[tuple, list[tuple[dict, int]]] = {}
    for b in bills:
        inv = (b.get("DocNumber") or "")[len(DOC_PREFIX):]
        for n, ln in enumerate(b.get("Line", [])):
            det = ln.get("AccountBasedExpenseLineDetail")
            if not det:
                continue
            date, desc_unit, _ = parse_desc(ln.get("Description") or "")
            amt = round(float(ln.get("Amount") or 0), 2)
            item = (b, n)
            avail.setdefault((inv, line_unit(det, desc_unit), date, amt), []).append(item)
            avail.setdefault((inv, None, date, amt), []).append(item)

    taken: set[tuple[str, int]] = set()

    def take(key) -> tuple[dict, int] | None:
        for b, n in avail.get(key, []):
            if (b["Id"], n) not in taken:
                taken.add((b["Id"], n))
                return b, n
        return None

    rows, unmatched = [], []
    for h in hoa:
        flag = ""
        hit = take((h["Invoice"], h["Unit"], h["Date"], h["Amount"]))
        if hit is None:
            hit = take((h["Invoice"], None, h["Date"], h["Amount"]))
            flag = "UNIT MISMATCH"
        if hit is None:
            unmatched.append(h)
            continue
        b, n = hit
        ln = b["Line"][n]
        det = ln["AccountBasedExpenseLineDetail"]
        _, desc_unit, _ = parse_desc(ln.get("Description") or "")
        bill_unit = line_unit(det, desc_unit)
        from_acct = (det.get("AccountRef") or {}).get("name", "")
        if flag == "UNIT MISMATCH":
            flag = f"UNIT MISMATCH: workbook {h['Unit']}, bill {bill_unit}"
        if target_acct_id and (det.get("AccountRef") or {}).get("value") == target_acct_id:
            flag = ("already repointed" + (f"; {flag}" if flag else ""))
        rows.append({
            "BillId": b["Id"], "DocNumber": b.get("DocNumber", ""), "TxnDate": b.get("TxnDate", ""),
            "LineNum": n + 1, "CleanDate": h["Date"], "Amount": f"{h['Amount']:.2f}",
            "WorkbookUnit": h["Unit"], "BillUnit": bill_unit,
            "FromAccount": from_acct, "ToAccount": acct_name_of("hoa_receivable_yacinde"),
            "FromClass": (det.get("ClassRef") or {}).get("name", ""), "ToClass": HOA_CLASS,
            "Description": ln.get("Description", ""),
            # The WORKBOOK unit, not the bill's: where the two disagree the allocation
            # workbook is the corrected record (owner decision, 2026-09-17), and the
            # HOA class no longer carries the unit, so the text is the only place it
            # survives.  They are identical on every line that is not flagged.
            "NewDescription": new_description(ln.get("Description") or "", h["Unit"]),
            "Flag": flag, "Source": h["Source"], "Notes": h["Notes"],
        })
    return rows, unmatched


def new_description(desc: str, unit: str) -> str:
    """Put the unit in the text, since the HOA class no longer carries it."""
    date, desc_unit, rest = parse_desc(desc)
    code = (unit or "").replace("Yacinde ", "").strip()
    if date is None or not code:
        return desc
    if desc_unit:                       # already converted -- leave the wording alone
        return desc
    d = f"{date[5:7]}/{date[8:10]}/{date[:4]}"
    return f"{d} {code} {rest}".rstrip()


def apply_to_bill(bill: dict, edits: list[dict], acct_id: str, class_id: str) -> dict:
    """Echo the Bill back with only the HOA lines' account, class and text changed.

    A FULL update: QBO blanks every field the payload omits, and a Bill carries
    APAccountRef, DueDate, LinkedTxn and per-line refs that are invisible until gone.
    """
    out = json.loads(json.dumps(bill))
    for e in edits:
        det = out["Line"][e["LineNum"] - 1]["AccountBasedExpenseLineDetail"]
        det["AccountRef"] = {"value": acct_id}
        det["ClassRef"] = {"value": class_id}
        out["Line"][e["LineNum"] - 1]["Description"] = e["NewDescription"]
    out["sparse"] = False
    return out


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--workbook", action="append", default=None,
                    help="allocation workbook (repeatable); default = yacinde_expense/output")
    ap.add_argument("--out", default=None)
    ap.add_argument("--confirm", action="store_true", help="actually WRITE to QuickBooks")
    args = ap.parse_args()

    books = [Path(w) for w in args.workbook] if args.workbook else list(DEFAULT_WORKBOOKS)
    if not books:
        sys.exit("no allocation workbooks found -- pass --workbook")
    for b in books:
        if not b.exists():
            sys.exit(f"no such workbook: {b}")
    print("workbooks:")
    for b in books:
        print(f"  {b}")

    qbo = client()
    res = Resolver(qbo)

    target_name = acct_name_of("hoa_receivable_yacinde")
    acct_id = res.account(target_name)
    class_id = res.klass(HOA_CLASS)
    print(f"\ntarget account  {target_name}")
    print(f"                {'Id ' + acct_id if acct_id else '*** DOES NOT EXIST YET ***'}")
    print(f"target class    {HOA_CLASS}")
    print(f"                {'Id ' + class_id if class_id else '*** DOES NOT EXIST ***'}")

    hoa = read_hoa_rows(books)
    bills = scan_bills(qbo)
    print(f"\n{len(hoa)} HOA-paid cleans in the workbooks, "
          f"${sum(h['Amount'] for h in hoa):,.2f}")
    print(f"{len(bills)} {DOC_PREFIX}* bills in QuickBooks, "
          f"${sum(float(b.get('TotalAmt') or 0) for b in bills):,.2f}")

    rows, unmatched = match(hoa, bills, acct_id)

    out = Path(args.out) if args.out else review_csv("yacinde_hoa_split")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)

    by_bill: dict[str, list[dict]] = {}
    for r in rows:
        by_bill.setdefault(r["BillId"], []).append(r)

    print(f"\n{len(rows)} lines on {len(by_bill)} bills -> {out}")
    for b in bills:
        edits = by_bill.get(b["Id"], [])
        moved = sum(float(e["Amount"]) for e in edits)
        total = float(b.get("TotalAmt") or 0)
        print(f"  {b.get('DocNumber'):<16} {b.get('TxnDate')}  total ${total:>9,.2f}   "
              f"HOA {len(edits):>2} lines ${moved:>9,.2f}   owner ${total - moved:>9,.2f}")
    print(f"  {'':16} {'':10}  {'':16}   "
          f"HOA {len(rows):>2} lines ${sum(float(r['Amount']) for r in rows):>9,.2f}")

    flags = Counter(r["Flag"].split(":")[0] for r in rows if r["Flag"])
    for f, n in flags.items():
        print(f"\n  {n} line(s) flagged {f!r}:")
        for r in rows:
            if r["Flag"].startswith(f):
                print(f"    {r['DocNumber']} line {r['LineNum']} {r['CleanDate']} "
                      f"${r['Amount']}  {r['Flag']}")

    if unmatched:
        print(f"\n  *** {len(unmatched)} HOA clean(s) with NO bill line -- NOT fixed:")
        for h in unmatched:
            print(f"    inv {h['Invoice']} {h['Unit']} {h['Date']} ${h['Amount']:.2f}")

    todo = {bid: e for bid, e in by_bill.items()
            if any(not x["Flag"].startswith("already repointed") for x in e)}

    if not args.confirm:
        print("\nDRY RUN — nothing written. Review the CSV, then re-run with --confirm.")
        if not acct_id:
            print(f"\n  Create the account first: {target_name}")
            print("  (Other Current Asset, under 'Trust Assets other than Cash & A/R')")
        return

    if not acct_id:
        sys.exit(f"ABORT — account {target_name!r} does not exist; create it first")
    if not class_id:
        sys.exit(f"ABORT — class {HOA_CLASS!r} does not exist")
    if unmatched:
        sys.exit(f"ABORT — {len(unmatched)} HOA clean(s) matched no bill line; resolve first")

    print(f"\nupdating {len(todo)} bills …")
    ok = 0
    for bid, edits in todo.items():
        bill = next(b for b in bills if b["Id"] == bid)
        before = round(float(bill.get("TotalAmt") or 0), 2)
        got = qbo.post("bill", apply_to_bill(bill, edits, acct_id, class_id))
        after = round(float(got.get("TotalAmt") or 0), 2)
        if abs(before - after) > 0.005:
            sys.exit(f"ABORT — Bill {bid}: total changed {before:,.2f} -> {after:,.2f}")
        ok += 1
        print(f"  {got.get('DocNumber')}: {len(edits)} lines moved, "
              f"total unchanged at {after:,.2f}")
    print(f"\nupdated {ok} bills")


if __name__ == "__main__":
    main()
