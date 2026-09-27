"""Build the monthly JEs that move Maria's cleaning onto each listing's class.

Maria Rangel's team is paid by lump Zelle transfers, booked to
`Cleaning Fee Revenue:Cleaning Payout:Destiny's cleaning` (1588) under class Valta
Realty -- so no listing carries its own cleaning cost.  Her workbook has one sheet per
service month (`2026-08`, `2025-7`, ...) listing `Total for cleaner` per listing.  Each
month becomes ONE JE:

    Dr ...:Destiny's cleaning (1588)   class = the listing    per listing
    Dr Accounts Receivable (807)       class = Valta Realty   `Residential cleaning`,
                                                              Customer = Valta Home
    Cr ...:Destiny's cleaning (1588)   class = Valta Realty   the sheet total

**Both sides sit on the account that holds that month's cash, so the JE is a pure CLASS
move and no money crosses accounts** (owner rule, 2026-09-26).  That account is Destiny's
cleaning from 2026-01 and Cleaning Payout itself before it -- through 2025 Maria's payments
were booked to the parent, and touching Destiny's for them would strand a balance there.
So a 2025 JE is a pure class move within 1534 and a 2026 one within 1588; the rule is the
same, only the account it names differs.

`fixes/maria_cleaning_je_shape` re-points JEs already posted with the listing debits on the
wrong account.

`Residential cleaning` is not a listing and is not Valta's cost either -- it is rebilled to
**Valta Home**, so it leaves the JE as a DEBIT TO A/R carrying that customer on the line,
and the credit is then the sheet's own `Total payment` rather than the listings subtotal.
The shape follows `MariaCL_2026-08`, which the owner corrected by hand (2026-09-26).

The residential figure is READ FROM THE SHEET, never carried forward: it moves every month
($10,870.76 / $9,045.78 / $7,249.03 for 2026-06/07/08) and a month can have none at all
(2026-03), in which case the line is omitted rather than written as zero.

    python -m src.je.build_maria_cleaning                           # 2026-01 .. latest
    python -m src.je.build_maria_cleaning --months 2026-07,2026-08
    python -m src.je.build_maria_cleaning --months 2025-07,2025-12 --out review/maria_cleaning_je_2025.csv
    python -m src.je.post --all --csv review/maria_cleaning_je.csv  # dry run
    python -m src.je.post --all --csv review/maria_cleaning_je.csv --confirm

Duplicate check: a month whose DocNumber is already in QBO, or with a JE already
crediting the same account the same amount on the same date, is left out.
"""
from __future__ import annotations

import argparse
import calendar
import csv
import re
import sys
from collections import defaultdict
from pathlib import Path

import openpyxl
import yaml

from ..config import acct_name, client
from ..paths import CONFIG_DIR, JE_INPUTS, review_csv
from ..resolver import Resolver, esc

DEFAULT_SRC = JE_INPUTS / "Cleaning payouts" / "Maria cleaning payment process_copied20260117.xlsx"
DEFAULT_CSV = review_csv("maria_cleaning_je")
PROPERTY_CLASSES_YML = CONFIG_DIR / "property_classes.yml"

# Both sides of the JE use ONE account -- whichever holds that month's cash.  These two
# names are that choice, not a debit/credit split; see `posting_account` below.
PARENT_ACCT = acct_name("cleaning_payout")
DESTINY_ACCT = acct_name("destiny_cleaning")
AR_ACCT = acct_name("accounts_receivable")
AR_CUSTOMER = "Valta Home"          # `Residential cleaning` is rebilled to them
CREDIT_CLASS = "Valta Realty"
LOCATION = "Valta Realty"    # what every Maria payment on Destiny's cleaning carries

# From this month Maria's payments are booked to Destiny's cleaning; before it, to
# Cleaning Payout itself.  The credit side follows the money.
DESTINY_FROM = "2026-01"
DEFAULT_FIRST = DESTINY_FROM


def posting_account(period: str) -> str:
    """The one account BOTH sides of the JE use -- where that month's cash landed."""
    return DESTINY_ACCT if period >= DESTINY_FROM else PARENT_ACCT


# Kept as aliases so a caller asking for either side gets the same account, which is the
# invariant: a Maria JE that names two accounts is wrong.
credit_account = posting_account
debit_account = posting_account

NOT_LISTINGS = {"residential cleaning"}


def period_of(sheet: str) -> str | None:
    m = re.fullmatch(r"(20\d\d)-(\d{1,2})", sheet.strip())
    return f"{m[1]}-{int(m[2]):02d}" if m else None


def month_end(period: str) -> str:
    y, m = map(int, period.split("-"))
    return f"{period}-{calendar.monthrange(y, m)[1]:02d}"


def num(v) -> float | None:
    return float(v) if isinstance(v, (int, float)) else None


def read_month(ws) -> tuple[list[dict], float, float | None]:
    """(listing rows, residential total, sheet's Total payment) for one month."""
    header = [str(c or "").strip().lower() for c in next(ws.iter_rows(max_row=1, values_only=True))]
    try:
        i_price = header.index("price for cleaner")
        i_times = header.index("times")
        i_total = header.index("total for cleaner")
    except ValueError:
        raise SystemExit(f"sheet {ws.title!r}: unrecognised header {header}")
    rows, residential, sheet_total = [], 0.0, None
    for r in ws.iter_rows(min_row=2, values_only=True):
        label = str(r[0] or "").strip()
        if not label:
            continue
        if label.lower() == "total payment":
            sheet_total = num(r[i_total])
            break
        amt = num(r[i_total]) or 0.0
        if label.lower() in NOT_LISTINGS:
            residential += amt
            continue
        if not amt:
            continue
        rows.append({"listing": label, "price": num(r[i_price]),
                     "times": num(r[i_times]), "amount": round(amt, 2)})
    return rows, round(residential, 2), sheet_total


def listing_class(label: str, res: Resolver, aliases: dict[str, str]) -> str | None:
    """Sheet label -> the class's real FullyQualifiedName, or None.

    The sheet spells listings several ways over the years: `Beachwood #2`,
    `Mercer Island 3627 Main`, `Redmond Gull val 7(Redmond 7579)`.  Normalise those
    spellings only; anything still unresolved blocks the post rather than guessing.
    """
    tries = [label]
    m = re.search(r"\(([^)]+)\)", label)
    if m:
        tries.append(m[1])
    norm = re.sub(r"\s+", " ", label.replace("#", "").replace("Mercer Island", "Mercer")).strip()
    tries.append(norm)
    for t in tries:
        if t in aliases:
            return res.klass_fqn(aliases[t])
        hit = res.klass_fqn(f"Listings:{t}")
        if hit:
            return hit
    return None


def already_booked(qbo, docnum: str, date: str, credit: str, total: float) -> str | None:
    got = qbo.query(f"SELECT Id FROM JournalEntry WHERE DocNumber = '{esc(docnum)}'"
                    ).get("QueryResponse", {}).get("JournalEntry", [])
    if got:
        return f"DocNumber {docnum} already in QBO as Id {got[0]['Id']}"
    for j in qbo.query_all(f"SELECT * FROM JournalEntry WHERE TxnDate = '{date}'", "JournalEntry"):
        for ln in j.get("Line", []):
            d = ln.get("JournalEntryLineDetail") or {}
            if (d.get("PostingType") == "Credit"
                    and d.get("AccountRef", {}).get("name", "") == credit
                    and abs(ln["Amount"] - total) < 0.005):
                return (f"JE {j.get('DocNumber') or '(no doc no.)'} (Id {j['Id']}) already credits "
                        f"{credit.rsplit(':', 1)[-1]} {total:,.2f} on {date}")
    return None


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--src", default=str(DEFAULT_SRC))
    ap.add_argument("--months", help="comma list of YYYY-MM (default: every sheet from 2026-01)")
    ap.add_argument("--out", default=str(DEFAULT_CSV))
    ap.add_argument("--rebuild", action="store_true",
                    help="emit rows for months already in QBO too, so fixes/"
                         "maria_cleaning_je_rebuild can full-update them from a corrected sheet")
    args = ap.parse_args()

    wb = openpyxl.load_workbook(args.src, data_only=True, read_only=True)
    sheets = {p: s for s in wb.sheetnames if (p := period_of(s))}
    if args.months:
        want = [m.strip() for m in args.months.split(",")]
        missing = [m for m in want if m not in sheets]
        if missing:
            sys.exit(f"no sheet for {missing}; have {sorted(sheets)}")
    else:
        want = sorted(p for p in sheets if p >= DEFAULT_FIRST)

    qbo = client()
    res = Resolver(qbo)
    aliases = (yaml.safe_load(PROPERTY_CLASSES_YML.read_text()) or {}).get("properties", {}) or {}

    out, warns = [], 0
    print(f"{'month':8} {'listings':>8} {'to listings':>12} {'residential':>12} {'sheet total':>12}  status")
    for period in sorted(want):
        rows, residential, sheet_total = read_month(wb[sheets[period]])
        total = round(sum(r["amount"] for r in rows), 2)
        docnum = f"MariaCL_{period}"
        date = month_end(period)
        notes = []
        if sheet_total is not None and abs(total + residential - sheet_total) > 0.005:
            notes.append(f"WARN listings+residential {total + residential:,.2f} != Total payment {sheet_total:,.2f}")
        credit = credit_account(period)
        dup = already_booked(qbo, docnum, date, credit, round(total + residential, 2))
        status = ("ok" if not dup else
                  (f"REBUILD {dup}" if args.rebuild else f"SKIP {dup}"))
        lines = []
        for r in rows:
            fq = listing_class(r["listing"], res, aliases)
            if fq is None:
                notes.append(f"WARN no class for {r['listing']!r}")
            calc = (r["price"] or 0) * (r["times"] or 0)
            desc = f"{period} Maria cleaning {r['listing']}"
            if r["times"]:
                desc += f" {r['times']:g} x ${r['price']:g}"
            if abs(calc - r["amount"]) > 0.005:
                notes.append(f"note {r['listing']}: {r['times']} x {r['price']} = {calc:g}, sheet says {r['amount']:g}")
            lines.append({"Account": credit, "Debits": f"{r['amount']:.2f}", "Credits": "",
                          "Description": desc, "Class": fq or f"*** UNMAPPED: {r['listing']} ***"})
        # `Residential cleaning` is rebilled, so it leaves on A/R against Valta Home.  A
        # month with none gets no line -- a $0.00 JE line is noise QBO will not even keep.
        if residential:
            lines.append({"Account": AR_ACCT, "Debits": f"{residential:.2f}", "Credits": "",
                          "Description": f"{period} Residential Cleaning",
                          "Class": CREDIT_CLASS, "Name": AR_CUSTOMER})
        # The credit is the whole sheet, not the listings subtotal: both debits clear it.
        credit_amt = round(total + residential, 2)
        if sheet_total is not None and abs(credit_amt - sheet_total) > 0.005:
            notes.append(f"WARN credit {credit_amt:,.2f} != Total payment {sheet_total:,.2f}")
        lines.append({"Account": credit, "Debits": "", "Credits": f"{credit_amt:.2f}",
                      "Description": f"{period} Maria cleaning reclassed to listings ({len(rows)} listings)",
                      "Class": CREDIT_CLASS})
        print(f"{period:8} {len(rows):>8} {total:>12,.2f} {residential:>12,.2f} "
              f"{sheet_total if sheet_total is not None else float('nan'):>12,.2f}  {status}")
        for n in notes:
            print(f"         {n}")
            warns += n.startswith("WARN")
        if dup and not args.rebuild:
            continue
        for i, ln in enumerate(lines, 1):
            out.append({"JournalNo": docnum, "JournalDate": date, "LineNum": i,
                        "Name": "", "Location": LOCATION, **ln})

    cols = ["JournalNo", "JournalDate", "LineNum", "Account", "Debits", "Credits",
            "Description", "Name", "Location", "Class"]
    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    with open(args.out, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=cols)
        w.writeheader()
        w.writerows(out)
    jes = len({r["JournalNo"] for r in out})
    print(f"\n{jes} JE(s), {len(out)} lines -> {args.out}")
    if warns:
        print(f"{warns} WARN(s) -- an UNMAPPED class blocks je/post until fixed.")


if __name__ == "__main__":
    main()
