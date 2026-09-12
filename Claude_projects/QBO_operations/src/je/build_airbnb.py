"""Build QuickBooks journal entries from Airbnb payout CSVs.

One JE per Airbnb payout:
  * one CREDIT line per detail row (reservation/adjustment) -> Payments Clearing - Airbnb,
    classed to the listing's QBO class (resolved conf code -> ledger property_id -> class)
  * one DEBIT line for the payout itself -> Chase Trust Checking 9967 - STR

Journal no. = Airbnb reference code.  Journal date = payout date.

Usage:  python -m src.je.build_airbnb --indir inputs/JE --out review/airbnb_je.csv
"""
from __future__ import annotations

import argparse
import csv
import glob
import os
import sqlite3
from pathlib import Path

import yaml

from .. import bridge
from ..config import acct_name, location, name
from ..paths import JE_INPUTS, review_csv

CLEARING_ACCT = acct_name("clearing_airbnb")
BANK_ACCT = name("bank_str")
LOCATION = location()
NEEDS_ACCOUNT = "*** NEEDS ACCOUNT ***"

# Detail-row types that are ordinary guest money and post to CLEARING_ACCT with the
# listing's class.  Anything else must be named here by the owner before it can post.
RESERVATION_TYPES = {"Reservation"}

# Owner-specified accounts for the non-Reservation detail rows.  Key = Airbnb "Type".
# Value = (account, class_override) -- class_override None means "use the listing's class".
RESOLUTIONS_ACCT = acct_name("resolutions")
COMMISSION_REVENUE_ACCT = name("commission_revenue")

SPECIAL_ACCOUNTS: dict[str, tuple[str, str | None]] = {
    # Rebates carry no listing; owner assigns them to the company class.
    "Misc Credit": (COMMISSION_REVENUE_ACCT, "Valta Realty"),
    # Resolutions/adjustments keep their listing's class.
    "Resolution Payout": (RESOLUTIONS_ACCT, None),
    "Resolution Adjustment": (RESOLUTIONS_ACCT, None),
    "Adjustment": (RESOLUTIONS_ACCT, None),
    # Hawaii transient-accommodation tax Airbnb passes through for us to remit.  It is part
    # of the payout and the invoice already recognises the tax under Guest Charges:Tax, so
    # the payout JE credits clearing for the FULL payout like every other line -- routing it
    # to Tax Liability here stranded it in clearing, which the payment cannot clear.
    "Pass Through Tot": (CLEARING_ACCT, None),
}

# Listings that carry no ledger history, so conf-code -> property_id fails.  The class
# here is what QuickBooks itself used for these same bookings on the other system's JEs.
LISTING_CLASS_OVERRIDES = {
    "Seattle Modern Townhouse + Rooftop, near Stadiums": "Listings:Seattle 1424C",
    "PNW Contemporary Retreat near Lake Sammamish": "Listings:Bellevue 242",
    "Zen & Spacious Mercer Island Guest Suite": "Listings:Mercer 3627 ADU",
    # mapping_classes.yml names class id 1000000011 "Listings:Bellevue 2323", but QBO
    # calls it "Listings:Bellevue 2323 Whole" -- and the Airbnb title says whole house.
    "Bellevue 2323, Whole house, Fenced Yard": "Listings:Bellevue 2323 Whole",
}

OUT_COLS = [
    "JournalNo", "JournalDate", "LineNum", "Account",
    "Debits", "Credits", "Description", "Name", "Location", "Class",
]


def num(s: str) -> float:
    s = (s or "").replace(",", "").strip()
    return float(s) if s else 0.0


def iso(d: str) -> str:
    """MM/DD/YYYY -> YYYY-MM-DD."""
    m, dd, y = d.split("/")
    return f"{y}-{m}-{dd}"


def load_class_map(root: Path | None = None) -> dict[str, str]:
    entries = yaml.safe_load(bridge.mapping_classes(root).read_text())
    return {e["property_id"]: e.get("qbo_class_name") for e in entries if e.get("qbo_class_name")}


def build(indir: Path, root: str | None = None) -> tuple[list[dict], list[str]]:
    conn = sqlite3.connect(bridge.ledger_db(root))
    classes = load_class_map(root)
    code_cache: dict[str, str | None] = {}

    def class_for(code: str) -> str:
        if code not in code_cache:
            row = conn.execute(
                "SELECT property_id FROM ledger_lines WHERE source_txn_id=? LIMIT 1", (code,)
            ).fetchone()
            code_cache[code] = classes.get(row[0]) if row else None
        return code_cache[code] or f"*** UNMAPPED {code} ***"

    out: list[dict] = []
    warnings: list[str] = []

    for path in sorted(glob.glob(str(indir / "*.csv"))):
        rows = list(csv.DictReader(open(path, encoding="utf-8-sig")))
        # The folder also holds other channels' payout exports (Booking.com), whose columns
        # are unrelated -- skip anything that is not an Airbnb transaction history.
        if not rows or "Type" not in rows[0] or "Reference code" not in rows[0]:
            warnings.append(f"skipped {os.path.basename(path)}: not an Airbnb payout CSV")
            continue
        payout: dict | None = None
        details: list[dict] = []

        def flush() -> None:
            if payout is None:
                return
            jno = payout["Reference code"]
            jdate = iso(payout["Date"])
            line = 0
            for d in details:
                line += 1
                amt = num(d["Amount"])
                typ = d["Type"]
                code = d["Confirmation code"]

                if typ in RESERVATION_TYPES:
                    acct, cls_over = CLEARING_ACCT, None
                elif typ in SPECIAL_ACCOUNTS:
                    acct, cls_over = SPECIAL_ACCOUNTS[typ]
                else:
                    acct, cls_over = NEEDS_ACCOUNT, None
                    warnings.append(f"{jno} line {line}: no account for type {typ!r} ({amt:+.2f})")

                if cls_over is not None:
                    cls = cls_over
                elif d["Listing"] in LISTING_CLASS_OVERRIDES:
                    cls = LISTING_CLASS_OVERRIDES[d["Listing"]]
                elif code:
                    cls = class_for(code)
                else:
                    cls = ""

                if d["Start date"] and d["End date"]:
                    span = f"{iso(d['Start date'])} to {iso(d['End date'])} | {d['Nights']} nights"
                    desc = f"{typ} | airbnb | {span}"
                else:
                    desc = f"{typ} | airbnb"
                if d["Details"]:
                    desc += f" | {d['Details']}"

                guest = (d["Guest"] or "").strip()
                name = f"airbnb - {guest} - {code}" if guest and code else ""

                out.append({
                    "JournalNo": jno, "JournalDate": jdate, "LineNum": line, "Account": acct,
                    "Debits": f"{-amt:.2f}" if amt < 0 else "",
                    "Credits": f"{amt:.2f}" if amt >= 0 else "",
                    "Description": desc, "Name": name, "Location": LOCATION, "Class": cls,
                })

            line += 1
            out.append({
                "JournalNo": jno, "JournalDate": jdate, "LineNum": line, "Account": BANK_ACCT,
                "Debits": f"{num(payout['Paid out']):.2f}", "Credits": "",
                "Description": f"Payout | {payout['Details']} | {jno}",
                "Name": "", "Location": LOCATION, "Class": "",
            })

            total = sum(num(d["Amount"]) for d in details)
            if abs(total - num(payout["Paid out"])) > 0.005:
                warnings.append(
                    f"{jno}: UNBALANCED payout {num(payout['Paid out']):.2f} vs details {total:.2f}"
                )

        for r in rows:
            if r["Type"] == "Payout":
                flush()
                payout, details = r, []
            else:
                details.append(r)
        flush()

    return out, warnings


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--indir", default=str(JE_INPUTS))
    ap.add_argument("--out", default=str(review_csv("airbnb_je")))
    ap.add_argument("--statements-root", default=None,
                    help="Owner_statement_whole checkout (class map + ledger); "
                         "defaults to the sibling folder")
    ap.add_argument("--skip-existing", action="store_true",
                    help="drop JEs whose payout is already in QuickBooks")
    args = ap.parse_args()

    rows, warnings = build(Path(args.indir), args.statements_root)

    if args.skip_existing:
        from ..config import client
        from .post import existing_payouts
        # Index every payout debit already on the bank account by (date, amount).
        # DocNumber alone is NOT enough: the same payouts were posted by another
        # system under a YYMMDD-XXXX number, so a DocNumber check would miss them
        # and double-post the cash.  Same index the poster itself checks against.
        posted = set(existing_payouts(client()))

        docs = sorted({r["JournalNo"] for r in rows})
        bank_of = {r["JournalNo"]: round(float(r["Debits"]), 2)
                   for r in rows if r["Account"] == BANK_ACCT}
        date_of = {r["JournalNo"]: r["JournalDate"] for r in rows}
        already = {d for d in docs if (date_of[d], bank_of[d]) in posted}
        rows = [r for r in rows if r["JournalNo"] not in already]
        warnings = [w for w in warnings if w.split(":")[0].split()[0] not in already]
        print(f"skip-existing: {len(already)} of {len(docs)} already in QBO, "
              f"{len(docs) - len(already)} new")
    os.makedirs(Path(args.out).parent, exist_ok=True)
    with open(args.out, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=OUT_COLS)
        w.writeheader()
        w.writerows(rows)

    jes = len({r["JournalNo"] for r in rows})
    dr = sum(num(r["Debits"]) for r in rows)
    cr = sum(num(r["Credits"]) for r in rows)
    print(f"{jes} journal entries, {len(rows)} lines -> {args.out}")
    print(f"  debits {dr:,.2f}   credits {cr:,.2f}   diff {dr - cr:,.2f}")
    for w_ in warnings:
        print(f"  WARN {w_}")


if __name__ == "__main__":
    main()
