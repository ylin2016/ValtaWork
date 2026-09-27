"""Build one JE per service month spreading Siren's Cleaning Crew's payout across listings.

Siren's is paid one lump ACH per service month, booked to
`Cleaning Fee Revenue:Cleaning Payout` (1534) under the flat `Valta Realty` class, so no
listing carries its own cleaning cost:

    Dr Cleaning Fee Revenue:Cleaning Payout          class = listing   per (listing, category)
    Dr 1C - Owner Expenses:Maintenance - Owner       class = listing   the `Hot tub` category
    Cr Cleaning Fee Revenue:Cleaning Payout          class = Valta Realty   the payment total

**Unlike `build_maria_cleaning` this is NOT a pure class move**, and the difference matters.
Hot tub servicing is the owner's cost, not Valta's (owner decision 2026-09-26), so it leaves
1534 for `Maintenance - Owner` -- the same account the Zelle builder's hot-tub split uses.
That account IS in the owner statement's gate (`1A - Net Earnings:`, `1C - Owner Expenses:`,
`2 - Owner Distributions`), where 1534 is not, so **hot tub reaches owner statements and the
cleaning categories do not**.  Adding a category here without deciding its account silently
decides it is Valta's.

    python -m src.je.build_siren_cleaning                      # every unbooked month
    python -m src.je.build_siren_cleaning --month 2026-04       # one
    python -m src.je.post --all --csv review/siren_cleaning_je.csv [--confirm]

Source: `inputs/JE/Cleaning payouts/Cl2024-12gSheet_Siren.xlsx`, one `YYYYMM` sheet per
service month, one row per clean.  Five things about that sheet are load-bearing:

- **Column roles come from the HEADER, never position.**  The layout changed mid-year:
  202601 carries `CheckIn, CheckOut, Guests, Nights, Cleaner.lead`, 202604 onward carries
  `Confirmation Code, Guest Name, Guests, CheckIn, CheckOut`.  Only `Month`, `Listing`,
  `Siren's Fee` and `Category` are relied on, and a missing one is fatal rather than guessed.
- **`Siren's Fee` is the payment, not `Cleaning.fee`.**  They differ -- Lilliwaup 28610 is
  charged 173.00 and Siren's is paid 170.00 -- and it is the payout being spread here.
- **Do NOT trust the sheet's own total row.**  202602 carries two (1,970.00 and 2,092.69) and
  most months carry none; 1,970.00 is stale.  The listing rows are what tie to the bank.
  A no-listing row with NO category IS such a total row and is skipped.
- **A no-listing row WITH a category is a real charge whose listing was left blank.**
  202604 has 46 of them, 1,725.00, and without them April is short by exactly that.
  `CATEGORY_LISTING` places the two categories that are provably single-property; any other
  category arriving without a listing is a WARN that blocks the post, never a guess.
- **The classes are PROPERTY level** (owner decision 2026-09-26), matching the one month
  already entered by hand: Purchase 110628 put all twelve cottages into a single
  `Listings:OSBR` line of 4,762.50.  So `Cottage N` -> `Listings:OSBR`, not `OSBR N`.

**The credit is the PAYMENT ON THE BOOKS, and the debits must equal it.**  The payment is
found by AMOUNT among Siren's flat-class lines on 1534 rather than by the one-month lag
(service month paid mid-following-month), because an amount match is evidence and date
arithmetic is an assumption.  If the sheet's rows do not sum to a real payment the month is
blocked -- deriving the total from the rows instead would make a missing row invisible, which
is the lesson the Zelle builder's `JE Amount` check exists to enforce.
"""
from __future__ import annotations

import argparse
import calendar
import warnings
import csv
import re
import sys
from collections import defaultdict
from datetime import date
from pathlib import Path

from openpyxl import load_workbook

# The workbook carries pivot caches with malformed rels; openpyxl warns 23 times and reads fine.
warnings.filterwarnings("ignore", module="openpyxl")

from ..config import client, location, name as cfg_name
from ..paths import JE_INPUTS, review_csv
from ..resolver import Resolver

WORKBOOK = JE_INPUTS / "Cleaning payouts" / "Cl2024-12gSheet_Siren.xlsx"
VENDOR = "Siren's Cleaning Crew"
ACCOUNT = "Cleaning Fee Revenue:Cleaning Payout"          # Id 1534 -- the credit, and most debits
MAINT_OWNER = "Trust Liabilities:Owner Payables:1C - Owner Expenses:Maintenance - Owner"   # 1678
SUPPLIES_OWNER = "Trust Liabilities:Owner Payables:1C - Owner Expenses:Supplies - Owner"   # 1680
COHOST_WAGE = "0_Employees 1099:Wages - Cohost/PM"                                         # 1689
REPAIRS_OWNER = "Trust Liabilities:Owner Payables:1C - Owner Expenses:Repairs - Owner"     # 1679


# Categories that are NOT Valta's cost.  The owner bears hot tub servicing, so it is charged
# to them rather than absorbed in the cleaning payout.
CATEGORY_ACCOUNT = {"Hot tub": MAINT_OWNER}

# `Others` is a catch-all the sheet fills in by hand -- special trips, supply reimbursements,
# a run to buy bolt cutters.  Aggregating it would post a line that says only "Others", so
# these stay ONE LINE PER ROW carrying the sheet's own note.
ITEMISED = {"Others"}

# Splitting `Others` by its note (owner decisions 2026-09-26).  Supplies are the owner's, a
# cleaner's travel time is Valta's, and three one-off tasks were ruled on individually.
# Note the asymmetry: SUPPLIES_OWNER, MAINT_OWNER and REPAIRS_OWNER are under
# `1C - Owner Expenses:` so they reach owner statements; COHOST_WAGE does not.
#
# This is a derivation from FREE TEXT, which nothing else in this project does, so it is
# built to be audited rather than trusted: the FIRST matching rule wins, every classification
# is printed, and a note matching NO rule blocks the month instead of falling back to a
# default.  Order matters twice over --
#   * `dump` is tested first of all.  A disposal run is owner maintenance whether the sheet
#     wrote "Extra fee for TRIP for dump", "Extra TASK fee for dump run" or "REIMBURSEMENT
#     for dump run" (owner decision 2026-09-26) -- the activity decides, not the wording.
#     This is the one place a later rule is deliberately overridden.
#   * `trip` is otherwise tested first.  A cleaner's trip is their time whatever the errand was for,
#     and the errand words all appear inside trip notes -- 2026-04 reads "Special trip ... so
#     item can get mailed.  Actual handling fee for mailing + shipping will be paid direct
#     from guest", which is a wage line, not a receivable, however often it says "mailing".
#   * `reimburs` is tested LAST, so "special trip to buy supplies" is a trip while
#     "reimbursement for buying bolt cutters" is a purchase.
#
# KNOWN INCONSISTENCY, kept because the owner ruled per note: a dump run written "Extra fee
# for trip for dump" is cohost wage while one written "Extra task fee for dump run" is owner
# maintenance.  Same activity, different account, decided by wording.
# Each rule carries its own entity: a wage line names the Vendor it paid.  `je/post` takes
# the optional `EntityType` column for this, and would take a Customer the same way if a
# future rule ever needed an Accounts Receivable line.
#
# Mailing guests' belongings back is COHOST WAGE, not a receivable (owner decision
# 2026-09-26) -- including the postage itself, so the fee and its shipping stay together
# rather than splitting across two accounts on the word "reimbursement".
WAGE_VENDOR = (VENDOR, "Vendor")

OTHERS_RULES = (("dump",         MAINT_OWNER,         None),         # a disposal run, however worded
                ("trip",         COHOST_WAGE,         WAGE_VENDOR),  # see below
                ("mailing",      COHOST_WAGE,         WAGE_VENDOR),  # 2025-08 cottage 5, incl. postage
                ("extra task",   MAINT_OWNER,         None),         # 2025-09 dump run
                ("installation", REPAIRS_OWNER,       None),         # 2025-11 toilet + garage door
                ("reimburs",     SUPPLIES_OWNER,      None))


def others_account(note: str):
    """(account, entity) for an `Others` note, or (None, None)."""
    low = note.lower()
    for token, acct, ent in OTHERS_RULES:
        if token in low:
            return acct, ent
    return None, None
FLAT_CLASS = "Valta Realty"

DOC = "SirenCL_{}"
SHEET_RE = re.compile(r"^(20\d{2})(0[1-9]|1[0-2])$")

REQUIRED = ("Month", "Listing", "Siren's Fee", "Category")

# Property-level classes.  Every `Cottage *` is an OSBR cottage and the register groups them:
# Purchase 110628 booked the whole block as one `Listings:OSBR` line.
CLASS_MAP = {
    "OSBR": "Listings:OSBR",
    "Lilliwaup": "Listings:Lilliwaup 28610",      # the sheet writes it both ways
}

# Categories that are provably single-property, used ONLY to place a row whose Listing cell
# was left blank.  Verified 2026-09-26 across 202603/05/06/07: every Common Cleaning row
# (49) and every Laundry row (57) whose listing IS filled sits on OSBR, with no exceptions.
CATEGORY_LISTING = {
    "Common Cleaning": "Listings:OSBR",
    "Laundry": "Listings:OSBR",
}


def month_end(period: str) -> str:
    y, m = int(period[:4]), int(period[5:7])
    return date(y, m, calendar.monthrange(y, m)[1]).isoformat()


def klass_for(listing: str) -> str | None:
    if listing in CLASS_MAP:
        return CLASS_MAP[listing]
    if listing.lower().startswith("cottage"):
        return "Listings:OSBR"
    return f"Listings:{listing}"


def read_month(wb, sheet: str):
    """(listing, category) -> amount, plus warnings and the ignored total-row sum.

    Returns None when the sheet's header is not one this builder understands -- the 2024
    sheets carry `Siren's cleaning invoice` / `Shaya's $` / `Who` instead, a different
    arrangement of a different arrangement of money.  Refusing to GUESS the columns must not
    mean refusing to run: the month is skipped by name and everything else still builds.
    """
    ws = wb[sheet]
    rows = list(ws.iter_rows(values_only=True))
    hdr = [("" if c is None else str(c)).strip() for c in rows[0]]
    missing = [c for c in REQUIRED if c not in hdr]
    if missing:
        return None, [f"{sheet}: header lacks {missing} -- not this builder's layout"], 0.0
    li, fi, ci = hdr.index("Listing"), hdr.index("Siren's Fee"), hdr.index("Category")
    ni = hdr.index("Notes") if "Notes" in hdr else None

    per: dict[tuple[str, str, str], float] = defaultdict(float)
    warns: list[str] = []
    totals = 0.0
    for n, r in enumerate(rows[1:], 2):
        try:
            fee = float(r[fi])
        except (TypeError, ValueError):
            continue
        listing = str(r[li]).strip() if r[li] else ""
        cat = str(r[ci]).strip() if r[ci] else ""
        note = " ".join(str(r[ni]).split()) if ni is not None and r[ni] else ""
        # An itemised category keys on its note as well, so two special trips to the same
        # property stay two lines and each says what it was for.
        key_note = note if cat in ITEMISED else ""
        if not listing and not cat:
            totals += fee                      # the sheet's own total row
            continue
        if not listing:
            placed = CATEGORY_LISTING.get(cat)
            if not placed:
                warns.append(f"{sheet} row {n}: {fee:,.2f} category {cat!r} has NO listing "
                             f"and no rule places it -- *** NEEDS LISTING ***")
                continue
            per[(placed, cat, key_note)] += fee
            continue
        kl = klass_for(listing)
        per[(kl, cat or "(uncategorised)", key_note)] += fee
    return per, warns, totals


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.je.build_siren_cleaning")
    ap.add_argument("--src", default=str(WORKBOOK))
    ap.add_argument("--month", default=None, help="one service month, YYYY-MM")
    ap.add_argument("--from", dest="from_m", default=None, help="earliest service month, YYYY-MM")
    ap.add_argument("--to", dest="to_m", default=None, help="latest service month, YYYY-MM")
    ap.add_argument("--out", default=None)
    ap.add_argument("--absorb-cents", type=float, default=0.0, metavar="N",
                    help="accept a payment up to N off the sheet, adding the difference to the "
                         "largest debit line (owner decision per run; 0 = refuse, the default)")
    ap.add_argument("--rebuild", action="store_true",
                    help="emit months whose JE is already in QBO")
    args = ap.parse_args()

    src = Path(args.src)
    if not src.exists():
        sys.exit(f"workbook not found: {src}")
    print(f"source: {src}")
    print(f"  modified {date.fromtimestamp(src.stat().st_mtime).isoformat()} "
          f"-- a stale export reproduces corrected months silently; re-export before a run.")
    wb = load_workbook(src, data_only=True)

    qbo = client()
    res = Resolver(qbo)
    acct_id = res.account(ACCOUNT)
    for a in set(CATEGORY_ACCOUNT.values()) | {r[1] for r in OTHERS_RULES}:
        res.account(a)                 # resolve now: a bad name must fail before any CSV exists
    fqn = {c["Id"]: c.get("FullyQualifiedName", "")
           for c in qbo.query_all("SELECT * FROM Class MAXRESULTS 500", "Class")}
    # Case-folded, because the sheet writes `shelton 310` for `Listings:Shelton 310`.  A class
    # that differs only in case is the SAME class; refusing it would block a good month.
    by_name = {v.casefold(): k for k, v in fqn.items()}

    # Siren's flat-class lines on 1534 -- the payments these JEs are spreading.
    payments = []
    # Wide enough to cover every sheet the workbook carries.  A window that starts after the
    # earliest payment reports "no payment of that amount" for a month that IS paid -- 2025-04
    # blocked that way against a payment sitting at 2025-05-12.
    for p in qbo.query_all("SELECT * FROM Purchase WHERE TxnDate >= '2024-06-01' MAXRESULTS 1000",
                           "Purchase"):
        if VENDOR.casefold() not in (p.get("EntityRef") or {}).get("name", "").casefold():
            continue
        for ln in p.get("Line", []):
            det = ln.get("AccountBasedExpenseLineDetail") or {}
            if (det.get("AccountRef") or {}).get("value") != acct_id:
                continue
            cid = (det.get("ClassRef") or {}).get("value")
            if fqn.get(cid, "").startswith("Listings:"):
                continue                      # already spread by hand
            payments.append({"Id": p["Id"], "TxnDate": p["TxnDate"],
                             "Amount": round(float(ln.get("Amount") or 0), 2)})
    print(f"  {len(payments)} unspread Siren's payment line(s) on {ACCOUNT.rsplit(':', 1)[-1]}")

    booked = {j.get("DocNumber") for j in qbo.query_all(
        "SELECT * FROM JournalEntry WHERE DocNumber LIKE 'SirenCL_%' MAXRESULTS 200",
        "JournalEntry")}

    sheets = sorted(s for s in wb.sheetnames if SHEET_RE.match(s))
    if args.month:
        sheets = [args.month.replace("-", "")]
    if args.from_m:
        sheets = [s for s in sheets if s >= args.from_m.replace("-", "")]
    if args.to_m:
        sheets = [s for s in sheets if s <= args.to_m.replace("-", "")]
    rows_out, skipped, blocked, all_others = [], [], [], []
    used: set[str] = set()

    for sheet in sheets:
        period = f"{sheet[:4]}-{sheet[4:]}"
        doc = DOC.format(period)
        per, warns, totals = read_month(wb, sheet)
        if per is None:
            skipped.extend(warns)
            continue
        # Round EACH line first, then sum -- never round the aggregate.  A month of 37.50s and
        # 56.25s sums to a half-cent and the credit then misses the debits by 0.01, which QBO
        # rejects and which reads as a data fault rather than an arithmetic one.
        per = {k: round(v, 2) for k, v in per.items()}
        debits = round(sum(per.values()), 2)
        if not debits:
            continue
        if doc in booked and not args.rebuild:
            skipped.append(f"{doc}: already in QBO ({debits:,.2f}) -- --rebuild to emit")
            continue

        match = [p for p in payments
                 if abs(p["Amount"] - debits) < 0.01 and p["Id"] not in used]
        # A sheet of 37.50s and 56.25s can land a cent from the bank.  Absorbing that is an
        # owner decision per run, never a default, and the adjusted line is named in the
        # output and in its own description so the cent is never silently invented.
        adjusted = None
        if not match and args.absorb_cents:
            close = sorted((abs(q["Amount"] - debits), q) for q in payments
                           if q["Id"] not in used and abs(q["Amount"] - debits) <= args.absorb_cents)
            if close:
                q = close[0][1]
                delta = round(q["Amount"] - debits, 2)
                biggest = max(per, key=lambda k: per[k])
                per[biggest] = round(per[biggest] + delta, 2)
                debits = round(sum(per.values()), 2)
                adjusted = (biggest, delta)
                match = [q]
        if warns:
            blocked.append((doc, debits, warns))
            continue
        if not match:
            # Name the CLOSEST unused payment when there is one.  "No payment of that amount"
            # sends you looking for a missing transaction when the real answer is often a
            # one-cent rounding difference or a known gap -- say which.
            near = sorted((abs(q["Amount"] - debits), q) for q in payments
                          if q["Id"] not in used)
            why = (f"sheet rows sum to {debits:,.2f} but no unspread Siren's payment of that "
                   f"amount is on the books")
            if near and near[0][0] <= 1.00:
                q = near[0][1]
                why += (f" -- closest is Purchase {q['Id']} {q['TxnDate']} at {q['Amount']:,.2f}, "
                        f"off by {debits - q['Amount']:+,.2f}")
            else:
                why += " -- a row is missing, or the month is unpaid"
            blocked.append((doc, debits, [why]))
            continue
        pay = match[0]
        if abs(pay["Amount"] - debits) > 0.005:
            blocked.append((doc, debits,
                            [f"rounded lines sum to {debits:,.2f} against payment "
                             f"{pay['Amount']:,.2f} (Purchase {pay['Id']}) -- {debits - pay['Amount']:+,.2f}"]))
            continue
        used.add(pay["Id"])
        if adjusted:
            (akl, acat, _), delta = adjusted
            print(f"  {doc}: ABSORBED {delta:+.2f} onto {akl} / {acat} to meet Purchase "
                  f"{pay['Id']} at {pay['Amount']:,.2f}")
        if totals:
            print(f"  {doc}: ignored {totals:,.2f} of no-listing/no-category total row(s)")

        # Build the month into its OWN list and commit only when every line resolves.  A
        # half-committed month is the one failure that breaks the JE: its debits would reach
        # the CSV while the credit never does, and the run then reports an imbalance whose
        # cause is several months away from the month that caused it.
        month_rows, bad = [], None
        others_seen: list[tuple] = []
        for n, ((kl, cat, note), amt) in enumerate(sorted(per.items()), 1):
            cid = by_name.get(kl.casefold())
            if not cid:
                bad = f"class {kl!r} does not exist in the company file"
                break
            acct, ent = CATEGORY_ACCOUNT.get(cat, ACCOUNT), None
            if cat in ITEMISED:
                acct, ent = others_account(note)
                if not acct:
                    bad = (f"{cat} line {amt:,.2f} on {kl}: note matches no rule "
                           f"({', '.join(r[0] for r in OTHERS_RULES)}) -- *** NEEDS A RULE ***: "
                           f"{note[:70]!r}")
                    break
                others_seen.append((doc, kl, amt, acct.rsplit(":", 1)[-1], note))
            who, who_type = ent if ent else ("", "")
            desc = f"{period} Siren's {cat}"
            if note:
                desc = f"{desc} -- {note}"
            if adjusted and (kl, cat, note) == adjusted[0]:
                desc = f"{desc} [incl. {adjusted[1]:+.2f} rounding to the payment]"
            month_rows.append({"JournalNo": doc, "JournalDate": month_end(period), "LineNum": n,
                               "Account": acct,
                               "Debits": f"{amt:.2f}", "Credits": "",
                               "Description": desc[:500],
                               "Name": who, "EntityType": who_type, "Location": location(),
                               "Class": fqn[cid]})       # the file's own spelling, not the sheet's
        if bad:
            blocked.append((doc, debits, [bad]))
            used.discard(pay["Id"])
            continue
        month_rows.append({"JournalNo": doc, "JournalDate": month_end(period),
                           "LineNum": len(month_rows) + 1,
                           "Account": ACCOUNT, "Debits": "", "Credits": f"{debits:.2f}",
                           "Description": f"{period} Siren's cleaning payout "
                                          f"(Purchase {pay['Id']} {pay['TxnDate']})",
                           "Name": "", "EntityType": "", "Location": location(),
                           "Class": FLAT_CLASS})
        rows_out.extend(month_rows)
        all_others.extend(others_seen)
        print(f"  {doc}  {len(month_rows) - 1} debit line(s) {debits:>10,.2f}  "
              f"<- Purchase {pay['Id']} {pay['TxnDate']}")

    if all_others:
        print("\n  `Others` split by note -- check these, they are derived from free text:")
        for doc, kl, amt, acct, note in all_others:
            print(f"     {doc}  {amt:>7,.2f}  {kl:<24} -> {acct:<20} {note[:52]}")
    for s in skipped:
        print(f"  SKIP  {s}")
    for doc, amt, ws in blocked:
        print(f"  BLOCKED {doc} ({amt:,.2f}):")
        for w in ws:
            print(f"     {w}")

    if not rows_out:
        print("\nnothing to build.")
        return
    out = Path(args.out) if args.out else review_csv("siren_cleaning_je")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows_out[0].keys()))
        w.writeheader()
        w.writerows(rows_out)
    dr = sum(float(r["Debits"] or 0) for r in rows_out)
    cr = sum(float(r["Credits"] or 0) for r in rows_out)
    print(f"\n{len({r['JournalNo'] for r in rows_out})} JE(s), {len(rows_out)} lines -> {out}")
    print(f"  debits {dr:,.2f}  credits {cr:,.2f}  diff {dr - cr:,.2f}")
    if abs(dr - cr) > 0.005:
        sys.exit("ABORT -- debits and credits disagree")
    print("\nNOTHING WAS WRITTEN. Review the CSV, then:\n"
          f"    python -m src.je.post --all --csv {out}            # dry run\n"
          f"    python -m src.je.post --all --csv {out} --confirm  # WRITES")


if __name__ == "__main__":
    main()
