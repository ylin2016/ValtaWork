"""Build one JE per VRBO invoice month moving its commission off the flat class onto listings.

    python -m src.je.build_vrbo_commission                      # every unbooked month
    python -m src.je.build_vrbo_commission --month 2026-06      # one
    python -m src.je.post --all --csv review/vrbo_commission_je.csv [--confirm]

VRBO charges one card payment per monthly invoice and it lands on
`Fee - Processing & Commission:Fee - VRBO Commission` (1606) under the flat `Valta Realty`
class, so no listing carries its own channel commission.  $43,913.67 sat that way on
2026-09-28.  This is the same shape as `je/build_maria_cleaning`:

    Dr Fee - VRBO Commission (1606)   class = the listing     per-listing commission
    Cr Fee - VRBO Commission (1606)   class = Valta Realty    the invoice's total

**Both sides are ONE account, so the JE moves no money between accounts -- only between
classes.**  A VRBO JE that names two accounts is wrong.  1606 is COGS and outside the owner
statement gate, exactly as `Fee - Booking.com Commission` (1602) is: the owner is charged for
channel fees at reservation time through the `9Z5X_`-style rebill Bills, so putting commission
on an owner-expense account would charge them twice.  This only fixes per-listing reporting.

**Dated by the PAYMENT, not the invoice month-end.**  The charge is dated early in the month
(2026-02-04 for the `202602` invoice), so a month-end JE would leave the flat class carrying
the whole amount for most of the month for no reason.  Crediting on the payment's own date
makes the two exactly offset.

## The invoice is the source, and it ties to the cent

`inputs/Invoice_payment/vrbo_invoices/VHA13B*-YYYYMM.csv`, one row per reservation.  Each
month's commission total equals its card payment exactly -- all nine did on 2026-09-28 -- and
**the month is BLOCKED if it does not**, the same rule the Siren builder uses: an amount match
against a real payment is evidence, where deriving the total from the rows would make a missing
row invisible.

Four things about that file are load-bearing:

- **A row's `Property` can be BLANK, and those rows are real money.**  8 of 715 rows, $1,060.10,
  and they are exactly why four months looked short.  Two kinds: a `UOM` of `Cancellation`
  (no guest, no gross, a 10% or 25% penalty rate) and ordinary reservations on a listing too new
  to appear by name.  Both still carry a **Listing Number**, so they are placed from the other
  rows -- `Property` is looked up from every row in every file that has both.  Only when a
  number is named nowhere does `config/vrbo_listings.yml` supply it, and a number in neither
  blocks the month.
- **The listing number is the identity, not the name.**  `2704155` carries two 202606
  cancellations and resolves to Elektra 703 from the files themselves.
- **Every `Cottage N` is an OSBR cottage** and rolls up to `Listings:OSBR` (owner decision
  2026-09-28), the same lump the Siren builder uses.
- **A bare building name is the unit the REGISTER already uses**, not a property-level class,
  which mostly does not exist: `Seattle 906` -> `Seattle 906 Lower` (65 existing lines,
  $8,362.15) and `Seattle 7434` -> `Seattle 7434 Whole` (10, $4,775.33).  Read off the very
  reservations in these files, by their `HA-` DocNumber -- not from a class map, which is not
  proof a class exists.

## Known, and deliberately not resolved here

**$138.26 is on the books twice and this JE does not fix it.**  The 202602 cancellation
`HA-HJNPBXM` is inside that month's $1,933.03 payment AND exists as its own Purchase 96254
(2026-02-08, class `Listings:Bellevue 10409`, description `homeaway - Akeylee West - HA-HjNpB…`).
Spreading February gives Bellevue 10409 that commission a second time.  Excluding the row would
break the amount tie, so it is included and reported: the duplicate is a question about
Purchase 96254, not about this split.

**$793.30 of small monthly VRBO charges is not in these invoices** -- a second card charge every
month ($13.60 … $261.90) on 1606 at Valta Realty that no invoice row accounts for.  Left alone;
no JE here touches it, and it stays on the flat class.
"""
from __future__ import annotations

import argparse
import csv
import re
import sys
from collections import defaultdict
from datetime import date
from pathlib import Path

import yaml

from ..config import client, location
from ..paths import CONFIG_DIR, INVOICE_INPUTS, review_csv
from ..resolver import Resolver

SRC_DIR = INVOICE_INPUTS / "vrbo_invoices"
ACCOUNT = "Fee - Processing & Commission:Fee - VRBO Commission"      # 1606
FLAT_CLASS = "Valta Realty"
CUSTOMER = "VRBO"
# There is MORE THAN ONE VRBO account and each bills its own monthly invoice, so the
# DocNumber has to name the account or two months collide.  The `vacation` account's JEs were
# posted before the second account was known and carry the unprefixed form; it is kept for them
# rather than renaming nine posted JEs.
DOC = "VrboCM_{}"
DOC_ACCT = "VrboCM_{}_{}"
DEFAULT_ACCOUNT = "vacation"
ACCOUNT_CODE = {"vacation": None, "elektra": "ELK"}
LISTINGS_YML = CONFIG_DIR / "vrbo_listings.yml"

# Owner decisions 2026-09-28 for names that are not a class, and register-derived answers for
# bare building names.  Everything else resolves against the live company file by leaf name.
PROPERTY_CLASS = {
    "Poulsbo Scandinavian Retreat, 2 blocks to DT": "Listings:Poulsbo 563",
    "Seattle 906": "Listings:Seattle 906 Lower",       # 65 existing lines, 8,362.15
    "Seattle 7434": "Listings:Seattle 7434 Whole",     # 10 existing lines, 4,775.33
}
# The `vacation` files carry a period; the `elektra` files do NOT -- so the period is taken
# from the PAYMENT the invoice matches, never from the filename.  That is better evidence
# anyway: the charge is what the books have to agree with, and it dates itself.
PERIOD_RE = re.compile(r"-(\d{4})(\d{2})\.csv$")


def money(s: str) -> float:
    s = (s or "").strip().replace("$", "").replace(",", "")
    try:
        return round(float(s), 2)
    except ValueError:
        return 0.0


def klass_for(prop: str) -> str:
    if prop in PROPERTY_CLASS:
        return PROPERTY_CLASS[prop]
    if prop.lower().startswith("cottage"):
        return "Listings:OSBR"          # every Cottage N is an OSBR cottage (owner, 2026-09-28)
    return prop                          # resolved by leaf against the company file


def read_files(src: Path):
    """One entry per INVOICE FILE, plus the listing# -> property map built across ALL of them.

    Files may sit directly under `src` or one level down in a per-account folder
    (`vacation/`, `elektra/`), and the folder name IS the account.  The listing# -> property map
    is deliberately global: a listing named in one account's file places a blank row in
    another's, and there is no reason to hold that evidence back.
    """
    num2prop: dict[str, set] = defaultdict(set)
    files: list[dict] = []
    for f in sorted(src.rglob("*.csv")):
        account = f.parent.name if f.parent != src else DEFAULT_ACCOUNT
        m = PERIOD_RE.search(f.name)
        rows = []
        with f.open(encoding="utf-8-sig", newline="") as fh:
            for n, r in enumerate(csv.DictReader(fh), 2):
                prop = (r.get("Property") or "").strip()
                num = (r.get("Listing Number") or "").strip()
                if prop and num:
                    num2prop[num].add(prop)
                rows.append((n, prop, num, money(r.get("Commission")),
                             (r.get("UOM") or "").strip(),
                             (r.get("Reservation External ID (Folio #)") or "").strip()))
        files.append({"path": f, "account": account, "rows": rows,
                      "label": f"{m.group(1)}-{m.group(2)}" if m else None})
    return files, num2prop


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.je.build_vrbo_commission")
    ap.add_argument("--src", default=str(SRC_DIR))
    ap.add_argument("--month", default=None, help="one invoice period, YYYY-MM")
    ap.add_argument("--out", default=None)
    ap.add_argument("--rebuild", action="store_true", help="emit months already in QBO")
    args = ap.parse_args()

    src = Path(args.src)
    if not src.is_dir():
        sys.exit(f"not a directory: {src}")
    files, num2prop = read_files(src)
    if not files:
        sys.exit(f"no .csv invoice files under {src} (looked in subfolders too)")
    by_acct = defaultdict(int)
    for fl in files:
        by_acct[fl["account"]] += 1
    print("  " + ", ".join(f"{k}: {v} invoice(s)" for k, v in sorted(by_acct.items())))
    override = (yaml.safe_load(LISTINGS_YML.read_text()) or {}).get("listings", {}) \
        if LISTINGS_YML.exists() else {}
    override = {str(k): v for k, v in override.items()}

    qbo = client()
    res = Resolver(qbo)
    acct_id = res.account(ACCOUNT)
    if acct_id is None:
        sys.exit(f"account not found: {ACCOUNT!r}")
    fqn = {c["Id"]: c.get("FullyQualifiedName", "")
           for c in qbo.query_all("SELECT * FROM Class MAXRESULTS 500", "Class")}
    flat_ids = {i for i, n in fqn.items() if n == FLAT_CLASS}

    # The card payments this JE is spreading: 1606 lines at the flat class.  An already-spread
    # line (a Listings: class) is not a candidate -- it has been dealt with.
    payments = []
    for p in qbo.query_all("SELECT * FROM Purchase WHERE TxnDate >= '2025-01-01' MAXRESULTS 1000",
                           "Purchase"):
        for ln in p.get("Line", []):
            d = ln.get("AccountBasedExpenseLineDetail") or {}
            if (d.get("AccountRef") or {}).get("value") != acct_id:
                continue
            if (d.get("ClassRef") or {}).get("value") not in flat_ids:
                continue
            payments.append({"Id": p["Id"], "TxnDate": p["TxnDate"],
                             "Amount": round(float(ln.get("Amount") or 0), 2)})
    print(f"  {len(payments)} unspread VRBO commission line(s) on the flat class, "
          f"{round(sum(p['Amount'] for p in payments), 2):,.2f}")

    booked = {j.get("DocNumber"): j["Id"] for j in qbo.query_all(
        "SELECT * FROM JournalEntry WHERE DocNumber LIKE 'VrboCM_%' MAXRESULTS 200",
        "JournalEntry")}

    rows_out, blocked, skipped, placed_log = [], [], [], []
    used: set[str] = set()

    # Biggest invoice first.  Two files can legitimately total the same amount, and matching the
    # large ones while the candidate pool is full makes a collision surface as a BLOCK on the
    # small one rather than as a silent swap between two accounts.
    for fl in sorted(files, key=lambda f: -round(sum(r[3] for r in f["rows"]), 2)):
        name = fl["path"].name
        per: dict[str, float] = defaultdict(float)
        bad = []
        for n, prop, num, amt, uom, code in fl["rows"]:
            if not amt:
                continue
            if not prop:
                named = sorted(num2prop.get(num, []))
                if len(named) == 1:
                    prop = named[0]
                    placed_log.append((name, n, code, uom, num, amt, prop, "from the files"))
                elif num in override:
                    prop = override[num]
                    placed_log.append((name, n, code, uom, num, amt, prop, "vrbo_listings.yml"))
                elif named:
                    bad.append(f"row {n}: {amt:,.2f} listing {num} is named {named} -- ambiguous")
                    continue
                else:
                    bad.append(f"row {n}: {amt:,.2f} listing {num!r} ({uom}) has no Property and "
                               f"no entry in vrbo_listings.yml -- *** NEEDS A LISTING ***")
                    continue
            per[klass_for(prop)] = round(per[klass_for(prop)] + amt, 2)
        # Round each line first, then sum -- never round the aggregate.
        per = {k: round(v, 2) for k, v in per.items()}
        debits = round(sum(per.values()), 2)
        if bad:
            blocked.append((name, debits, bad))
            continue
        if not debits:
            skipped.append(f"{name}: no commission rows")
            continue

        # The PAYMENT decides the period, not the filename -- the elektra files carry no period at
        # all, and the charge is what the books must agree with.  A non-unique amount match is
        # refused rather than resolved by taking the first: two accounts billing the same total
        # in one month would otherwise swap silently.
        match = [p for p in payments
                 if abs(p["Amount"] - debits) < 0.01 and p["Id"] not in used]
        if len(match) > 1:
            blocked.append((name, debits,
                            [f"{len(match)} unspread payments of {debits:,.2f} "
                             f"({', '.join(f'{m[chr(39)+chr(73)+chr(100)+chr(39)]} {m[chr(39)+chr(84)+chr(120)+chr(110)+chr(68)+chr(97)+chr(116)+chr(101)+chr(39)]}' for m in match)}) "
                             f"-- which month this invoice is cannot be decided from the amount"]))
            continue
        if not match:
            near = sorted((abs(p["Amount"] - debits), p) for p in payments if p["Id"] not in used)
            why = (f"invoice rows sum to {debits:,.2f} but no unspread VRBO payment of that "
                   f"amount is on the books")
            if near and near[0][0] <= 5.00:
                q = near[0][1]
                why += (f" -- closest is Purchase {q['Id']} {q['TxnDate']} at {q['Amount']:,.2f}, "
                        f"off by {debits - q['Amount']:+,.2f}")
            if fl["label"]:
                why += f"   (filename says {fl['label']})"
            blocked.append((name, debits, [why]))
            continue
        pay = match[0]
        period = pay["TxnDate"][:7]
        if fl["label"] and fl["label"] != period:
            print(f"  NOTE {name}: filename says {fl['label']} but its payment is "
                  f"{pay['TxnDate']} -- using {period}")
        code = ACCOUNT_CODE.get(fl["account"], fl["account"].upper()[:4])
        doc = DOC.format(period) if code is None else DOC_ACCT.format(code, period)
        if doc in booked and not args.rebuild:
            skipped.append(f"{doc} ({name}): already in QBO as JE {booked[doc]} "
                           f"({debits:,.2f}) -- --rebuild to emit")
            continue
        used.add(pay["Id"])

        month_rows, cls_bad = [], None
        for i, (kl, amt) in enumerate(sorted(per.items()), 1):
            full = res.klass_fqn(kl)
            cid = res.klass(full) if full else None
            if not cid:
                cls_bad = f"class {kl!r} does not exist in the company file"
                break
            month_rows.append({"JournalNo": doc, "JournalDate": pay["TxnDate"], "LineNum": i,
                               "Account": ACCOUNT, "Debits": f"{amt:.2f}", "Credits": "",
                               "Description": f"{period} VRBO commission"[:500],
                               "Name": "", "EntityType": "", "Location": location(),
                               "Class": full})
        if cls_bad:
            blocked.append((name, debits, [cls_bad]))
            used.discard(pay["Id"])
            continue
        month_rows.append({"JournalNo": doc, "JournalDate": pay["TxnDate"],
                           "LineNum": len(month_rows) + 1, "Account": ACCOUNT,
                           "Debits": "", "Credits": f"{debits:.2f}",
                           "Description": f"{period} VRBO commission "
                                          f"(Purchase {pay['Id']} {pay['TxnDate']})",
                           "Name": CUSTOMER, "EntityType": "Customer", "Location": location(),
                           "Class": FLAT_CLASS})
        rows_out.extend(month_rows)
        print(f"  {doc:<20} {len(month_rows) - 1:>2} listing(s) {debits:>10,.2f}  "
              f"<- Purchase {pay['Id']} {pay['TxnDate']}  ({fl['account']}/{name})")

    if placed_log:
        print("\n  rows with NO Property, placed by listing number -- check these:")
        for nm, n, cd, uom, num, amt, prop, how in placed_log:
            print(f"     {nm[:26]:<26} row {n:>4}  {amt:>8,.2f}  {uom:<15} {cd:<12} "
                  f"listing {num:<9} -> {prop:<22} ({how})")
    for s in skipped:
        print(f"  SKIP  {s}")
    for nm, amt, ws in blocked:
        print(f"  BLOCKED {nm} ({amt:,.2f}):")
        for w in ws:
            print(f"     {w}")

    if not rows_out:
        print("\nnothing to build.")
        return
    out = Path(args.out) if args.out else review_csv("vrbo_commission_je")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows_out[0].keys()))
        w.writeheader()
        w.writerows(rows_out)
    dr = round(sum(float(r["Debits"] or 0) for r in rows_out), 2)
    cr = round(sum(float(r["Credits"] or 0) for r in rows_out), 2)
    print(f"\n{len({r['JournalNo'] for r in rows_out})} JE(s), {len(rows_out)} lines -> {out}")
    print(f"  debits {dr:,.2f}  credits {cr:,.2f}  diff {round(dr - cr, 2):,.2f}")
    if abs(dr - cr) > 0.005:
        sys.exit("ABORT -- debits and credits disagree")
    print("\nNOTHING WAS WRITTEN. Review the CSV, then:\n"
          f"    python -m src.je.post --all --csv {out}            # dry run\n"
          f"    python -m src.je.post --all --csv {out} --confirm  # WRITES")


if __name__ == "__main__":
    main()
