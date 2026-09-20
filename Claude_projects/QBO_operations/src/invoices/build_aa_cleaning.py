"""Build the QuickBooks Expense for an AA Professional Cleaners invoice.

Valta pays the cleaner first; the money is recovered afterwards.  One Expense records
the cash leaving the trust account, split line by line between the two parties that
bear it:

    HOA's clean     HOA Receivable - Yacinde (1150040013)      an ASSET until invoiced
    owner's clean   1C - Owner Expenses:Cleaning Expense - Owner (1678)

Both carry the unit's LISTING class, which is how every HOA-receivable line already on
the books is classed (owner decision 2026-09-17, `fixes/yacinde_hoa_classes --to listing`).
The owner's share needs no separate bill: `Cleaning Expense - Owner` is an owner payable,
so the statement pipeline charges it on its own.  The HOA's share is invoiced by
`invoices/hoa_cleaning`, and its payment deposits back to the account that paid the
cleaner -- Chase Trust 9967.

**Which cleans are the HOA's is never inferred here.**  Two sources state it and
`--split` picks which one wins; the other is always reconciled against and every
disagreement is printed, so neither is applied silently.

    --split sheet        the expense sheet's own `Category` column  (DEFAULT)
    --split allocation   `hoa_paid` / `owner_paid` in the allocation workbook

The sheet is the default because the owner fills that column in by hand, per invoice,
knowing what they intend to recover -- owner decision 2026-09-19, on INV1200, where the
sheet put all 25 cleans on the receivable and the allocation would have given 2 of them
($360, B3 on 09/03 and 09/07) to Yacinde Holdings.  Be aware of what that costs: the HOA
is then invoiced for the full amount, and anything it declines to reimburse strands on
the receivable instead of reaching the owner who bore it.  `--split allocation` is the
reverse call, and follows `fixes/yacinde_hoa_split`.

The allocation workbook is still required -- it is what the sheet is reconciled against,
and `--split sheet` changes only which side wins a disagreement.

    python -m src.invoices.build_aa_cleaning --invoice 1200
    python -m src.invoices.post --all --csv review/aa_cleaning_1200.csv --confirm

The output is the review CSV that `invoices/post` already reads, so nothing about
posting, the duplicate check or the change log is special-cased for this vendor.
"""
from __future__ import annotations

import argparse
import csv
import re
import sys
from collections import defaultdict
from datetime import date, datetime, timedelta
from pathlib import Path

import openpyxl

from ..config import client, location, name as cfg_name
from ..paths import INVOICE_INPUTS, review_csv
from ..resolver import Resolver

VENDOR = "AA Professional Cleaners LLC"
BANK_ACCT = cfg_name("bank_str")
LOCATION = location()

HOA_ACCT = cfg_name("hoa_receivable_yacinde")
OWNER_ACCT = "Trust Liabilities:Owner Payables:1C - Owner Expenses:Cleaning Expense - Owner"

ALLOC_DIR = Path(__file__).resolve().parents[3] / "yacinde_expense" / "output"
ALLOC_GLOB = "yacinde_expense_allocation_*.xlsx"
ALLOC_SHEET = "Cleaning Allocation"

DOCNUMBER_MAX = 21

# `2026-09-01 Yacinde B1 Cleaning Fee` -- the form the expense sheet writes.
#
# DOTALL and no `$` for the same reason `fixes/yacinde_hoa_split` needs it: a clean's
# description can carry an embedded newline, and `.*$` stops at it, failing the whole
# match so the line's date comes back None and it pairs with nothing.
DESC_RE = re.compile(r"^\s*(\d{4})-(\d{2})-(\d{2})\s+Yacinde\s+([A-Z]\d)\b(.*)", re.S)


def iso(v) -> str:
    if isinstance(v, datetime):
        return v.date().isoformat()
    if isinstance(v, date):
        return v.isoformat()
    if isinstance(v, (int, float)):
        return (date(1899, 12, 30) + timedelta(days=float(v))).isoformat()
    return str(v).strip()[:10]


def rows_of(ws) -> list[dict]:
    it = ws.iter_rows(values_only=True)
    header = [str(h).strip() if h is not None else "" for h in next(it)]
    out = []
    for raw in it:
        if all(c is None or str(c).strip() == "" for c in raw):
            continue
        out.append({h: v for h, v in zip(header, raw) if h})
    return out


def parse_desc(desc: str) -> tuple[str | None, str | None]:
    """(iso date, unit) from `2026-09-01 Yacinde B1 Cleaning Fee`."""
    m = DESC_RE.match(desc or "")
    if not m:
        return None, None
    return f"{m.group(1)}-{m.group(2)}-{m.group(3)}", m.group(4)


def sheet_rows(src: Path, invoice: str | None) -> tuple[list[dict], str, dict]:
    """The AA lines of the expense workbook, with the transfer's own header fields."""
    wb = openpyxl.load_workbook(src, data_only=True, read_only=True)
    records = rows_of(wb.worksheets[0])
    wb.close()

    out, header = [], {}
    for i, r in enumerate(records, start=2):
        if str(r.get("Pay") or "").strip() != VENDOR:
            continue
        inv = str(r.get("fileName") or "").strip().upper().removeprefix("INV")
        if invoice and inv != invoice:
            continue
        d, unit = parse_desc(str(r.get("filename") or ""))
        out.append({
            "_row": i, "Invoice": inv, "Date": d, "Unit": unit,
            "Amount": round(float(r.get("Invoice amount") or 0), 2),
            "Desc": str(r.get("filename") or "").strip(),
            "SheetAccount": str(r.get("Category") or "").strip(),
        })
        header = {
            "TxnDate": iso(r.get("Date")),
            "Ref": str(r.get("Zelle content") or "").strip(),
            "JEAmount": round(float(r.get("JE Amount") or 0), 2),
        }
    invs = {r["Invoice"] for r in out}
    if len(invs) > 1:
        sys.exit(f"rows span invoices {sorted(invs)} -- pass --invoice to pick one")
    return out, (invs.pop() if invs else ""), header


def allocation(paths: list[Path], invoice: str) -> list[dict]:
    """Every clean on this AA invoice, with the party that bears it.

    `$0` rows are the "fraction week, no clean" markers and are not cleans.
    """
    out = []
    for p in paths:
        wb = openpyxl.load_workbook(p, data_only=True, read_only=True)
        if ALLOC_SHEET not in wb.sheetnames:
            wb.close()
            continue
        for d in rows_of(wb[ALLOC_SHEET]):
            if str(d.get("invoice") or "").strip() != invoice:
                continue
            amt = round(float(d.get("Amount") or 0), 2)
            if amt <= 0:
                continue
            hoa = round(float(d.get("hoa_paid") or 0), 2)
            own = round(float(d.get("owner_paid") or 0), 2)
            out.append({
                "Source": p.name,
                "Unit": str(d.get("unit") or "").strip().removeprefix("Yacinde").strip(),
                "Date": iso(d.get("date")),
                "Amount": amt, "HOA": hoa, "Owner": own,
                "Party": str(d.get("party") or "").strip(),
            })
        wb.close()
    return out


def match(sheet: list[dict], alloc: list[dict]) -> tuple[list[tuple[dict, dict]], list[dict], list[dict], list[str]]:
    """Pair each sheet line with one allocation clean.

    Exact (date, unit, amount) first; a leftover then relaxes the UNIT, and only then
    the DATE.  Both relaxations are flagged, never silent.

    The unit relaxation is the older one: the allocation workbook and the cleaner's own
    paperwork have disagreed on which unit a clean belongs to (08/20 F1-vs-C1, 08/22
    F5-vs-B3) while reconciling exactly on date, amount and count -- one clean under two
    labels, borne by the same party either way.

    The date relaxation is owner decision 2026-09-19, and it is deliberately last.  A
    clean can be RESCHEDULED: on INV1200 the allocation carries F1 at 09/08 noting
    "checkout at 9/8, rescheduled cleaning" while AA invoiced it on 09/11.  Refusing to
    pair them left the expense $180 short of the money that actually left the bank, so
    the bank feed would not have matched.  Unit and amount must still agree exactly.
    """
    warnings: list[str] = []
    avail: dict[tuple, list[dict]] = defaultdict(list)
    for a in alloc:
        avail[(a["Date"], a["Unit"], a["Amount"])].append(a)

    pairs, unmatched = [], []
    for s in sheet:
        k = (s["Date"], s["Unit"], s["Amount"])
        if avail.get(k):
            pairs.append((s, avail[k].pop(0)))
        else:
            unmatched.append(s)

    still = []
    for s in unmatched:
        loose = [k for k, v in avail.items() if v and k[0] == s["Date"] and k[2] == s["Amount"]]
        if loose:
            a = avail[loose[0]].pop(0)
            warnings.append(f"row {s['_row']}: sheet says {s['Unit']} on {s['Date']}, the "
                            f"allocation says {a['Unit']} -- same date and amount, so it is "
                            f"one clean under two labels; using the allocation's unit")
            pairs.append((s, a))
            continue
        # Last resort: the same unit and amount on a different day -- a rescheduled clean.
        loose = [k for k, v in avail.items() if v and k[1] == s["Unit"] and k[2] == s["Amount"]]
        if loose:
            a = avail[loose[0]].pop(0)
            warnings.append(f"row {s['_row']}: {s['Unit']} ${s['Amount']:,.2f} is dated "
                            f"{s['Date']} on the sheet and {a['Date']} in the allocation "
                            f"-- treated as ONE rescheduled clean, booked under the sheet's "
                            f"date (AA's invoice date) and the allocation's party")
            pairs.append((s, a))
        else:
            still.append(s)

    leftover = [a for v in avail.values() for a in v]
    return pairs, still, leftover, warnings


def compact_docnumber(txndate: str, invoice: str, total: float) -> str:
    """`20260917_INV1200_4500` -- date, AA invoice, total, within QuickBooks' 21 chars."""
    amt = f"{total:.2f}".rstrip("0").rstrip(".")
    return f"{txndate.replace('-', '')}_INV{invoice}_{amt}"[:DOCNUMBER_MAX]


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--src", default=str(INVOICE_INPUTS / "20260919_Expense2qbo.xlsx"))
    ap.add_argument("--invoice", default=None, help="AA invoice number, e.g. 1200")
    ap.add_argument("--workbook", action="append", default=None,
                    help="allocation workbook (repeatable); default = yacinde_expense/output")
    ap.add_argument("--out", default=None)
    ap.add_argument("--split", choices=("sheet", "allocation"), default="sheet",
                    help="who bears each clean: the expense sheet's own Category column "
                         "(default), or the allocation workbook's hoa_paid/owner_paid")
    args = ap.parse_args()

    sheet, invoice, hdr = sheet_rows(Path(args.src), args.invoice)
    if not sheet:
        sys.exit(f"no {VENDOR} rows in {args.src}")
    books = [Path(p) for p in args.workbook] if args.workbook else sorted(ALLOC_DIR.glob(ALLOC_GLOB))
    books = [p for p in books if not p.name.startswith("~$")]
    if not books:
        sys.exit("no allocation workbooks found -- pass --workbook")

    alloc = allocation(books, invoice)
    print(f"AA invoice {invoice}: {len(sheet)} sheet lines, {len(alloc)} allocation cleans")
    print(f"  allocation: HOA {sum(a['HOA'] for a in alloc):,.2f} + "
          f"owner {sum(a['Owner'] for a in alloc):,.2f} = "
          f"{sum(a['Amount'] for a in alloc):,.2f}")

    pairs, unmatched, leftover, warnings = match(sheet, alloc)
    for s in unmatched:
        warnings.append(f"row {s['_row']}: {s['Unit']} {s['Date']} ${s['Amount']:,.2f} is on the "
                        f"expense sheet but NOT in the allocation -- line DROPPED")
    for a in leftover:
        warnings.append(f"allocation has {a['Unit']} {a['Date']} ${a['Amount']:,.2f} "
                        f"({a['Party']}) with no line on the expense sheet -- NOT booked")

    qbo = client()
    res = Resolver(qbo)
    if res.vendor(VENDOR) is None:
        sys.exit(f"vendor {VENDOR!r} is not in this company file")
    # Every account that could reach a line, including the ones the sheet names itself:
    # a typo in that column must fail loudly here, never post to the wrong account.
    for acct in {HOA_ACCT, OWNER_ACCT, BANK_ACCT} | {s["SheetAccount"] for s in sheet if s["SheetAccount"]}:
        if res.account(acct) is None:
            sys.exit(f"account {acct!r} is not in this company file")

    docnum = compact_docnumber(hdr["TxnDate"], invoice, sum(s["Amount"] for s, _ in pairs))
    rows = []
    for n, (s, a) in enumerate(sorted(pairs, key=lambda p: (p[0]["Date"] or p[1]["Date"],
                                                            p[1]["Unit"])), start=1):
        # The clean's date as AA invoiced it; the unit and the party from the allocation.
        when = s["Date"] or a["Date"]
        if a["HOA"] > 0 and a["Owner"] > 0:
            warnings.append(f"{a['Unit']} {a['Date']}: the allocation splits one clean across "
                            f"both parties (HOA {a['HOA']}, owner {a['Owner']}) -- not supported, "
                            f"line DROPPED")
            continue
        from_alloc = HOA_ACCT if a["HOA"] > 0 else OWNER_ACCT
        acct = (s["SheetAccount"] or from_alloc) if args.split == "sheet" else from_alloc
        if s["SheetAccount"] and s["SheetAccount"] != from_alloc:
            kept, other = ((s["SheetAccount"], from_alloc) if args.split == "sheet"
                           else (from_alloc, s["SheetAccount"]))
            warnings.append(f"row {s['_row']} ({a['Unit']} {a['Date']}): the sheet's Category and "
                            f"the allocation ({a['Party']}) disagree -- booking "
                            f"{kept.rsplit(':', 1)[-1]}, not {other.rsplit(':', 1)[-1]} "
                            f"(--split {args.split})")
        # The class map is not proof a class exists -- resolve the real FQN, which for
        # these units is two levels deep (`Listings:Yacinde NuGrowth:Yacinde E1`).
        cls = res.klass_fqn(f"Yacinde {a['Unit']}")
        if cls is None:
            cls = f"*** UNMAPPED Yacinde {a['Unit']} ***"
            warnings.append(f"{a['Unit']} {a['Date']}: no class resolves for "
                            f"'Yacinde {a['Unit']}'")
        rows.append({
            "PayRef": hdr["Ref"], "DocNumber": docnum, "TxnDate": hdr["TxnDate"],
            "Vendor": VENDOR, "PaidFrom": BANK_ACCT, "Location": LOCATION,
            "LineNum": n, "Amount": f"{a['Amount']:.2f}", "Account": acct, "Class": cls,
            "Description": f"{when} Yacinde {a['Unit']} Cleaning Fee (AA INV{invoice})",
            "Category": a["Party"], "Property": f"Yacinde {a['Unit']}",
            "Invoice": f"INV{invoice}", "LumpTotal": "",
        })

    total = round(sum(float(r["Amount"]) for r in rows), 2)
    for r in rows:
        r["LumpTotal"] = f"{total:.2f}"
    if hdr["JEAmount"] and abs(total - hdr["JEAmount"]) > 0.005:
        warnings.append(f"lines total {total:,.2f} but the sheet's JE Amount says "
                        f"{hdr['JEAmount']:,.2f} -- off by {total - hdr['JEAmount']:,.2f}")

    out = Path(args.out) if args.out else review_csv(f"aa_cleaning_{invoice}")
    out.parent.mkdir(parents=True, exist_ok=True)
    fields = ["PayRef", "DocNumber", "TxnDate", "Vendor", "PaidFrom", "Location", "LineNum",
              "Amount", "Account", "Class", "Description", "Category", "Property", "Invoice",
              "LumpTotal"]
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=fields)
        w.writeheader()
        w.writerows(rows)

    split = defaultdict(float)
    for r in rows:
        split[r["Account"].rsplit(":", 1)[-1]] += float(r["Amount"])
    print(f"\n{len(rows)} lines -> {out}")
    print(f"  DocNumber {docnum}  TxnDate {hdr['TxnDate']}  paid from {BANK_ACCT}")
    for k, v in sorted(split.items(), key=lambda x: -x[1]):
        print(f"  {v:>9,.2f}  {k}")
    print(f"  {total:>9,.2f}  TOTAL")
    for w_ in warnings:
        print(f"  WARN {w_}")
    print("\nDRY RUN -- review the CSV, then post it with:")
    print(f"  python -m src.invoices.post --all --csv {out} --confirm")


if __name__ == "__main__":
    main()
