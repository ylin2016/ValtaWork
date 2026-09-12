"""Build QuickBooks journal entries from a Booking.com payout CSV.

One JE per Statement Descriptor (the payout batch):
  * one line per reservation -> Payments Clearing - Booking.com, classed to the listing
    (RESERVATION = credit, REFUND = debit)
  * one DEBIT line for the batch total -> Chase Trust Checking 9967 - STR

Journal no. = Statement Descriptor.  Journal date = payout date.
Shape follows the 2024 Booking.com JEs already in QuickBooks (e.g. Id 51796).

    python -m src.je.build_bookingcom --out review/bookingcom_je.csv
"""
from __future__ import annotations

import argparse
import csv
import glob
import os
from collections import defaultdict
from datetime import datetime
from pathlib import Path

import yaml

from .. import bridge
from ..config import acct_name, location, name as cfg_name
from ..paths import JE_INPUTS, review_csv

CLEARING_ACCT = acct_name("clearing_bookingcom")
BANK_ACCT = cfg_name("bank_str")
LOCATION = location()

DEFAULT_IN = JE_INPUTS / "bookings_JE_input_jul_aug.csv"

# Nicknames `ltr.records.to_property_id` does not resolve.  Kept local rather than added to
# that shared map, which the statement pipeline depends on.
NICKNAME_OVERRIDES = {
    # to_property_id yields "cottage_11_tiny"; the real listing is osbr_11.
    "Cottage 11 (tiny)": "osbr_11",
    # "Seattle 906" is a statement-only parent with no QBO class.  Only the Lower unit has
    # ever carried a JE line (164 of them; Upper has none), so the booking is assumed Lower.
    # UNCONFIRMED -- one row, $551.48.
    "Seattle 906": "seattle_906_lower",
}

def load_guesty(statements_root: str | None = None) -> tuple[dict, dict, dict]:
    """Booking.com stays from the stored Guesty exports, indexed two ways.

    The Booking.com payout CSV carries no check-out date, so the stay comes from Guesty.
    Its reservation number is not Guesty's booking_id (Guesty uses BC-xxxx), so the join is
    (property_id, check-in); (check-in, amount) is the fallback that catches a row whose
    property disagrees.

    TWO sources, because they hold different things: `guesty_converted.csv` carries only
    CONFIRMED stays, while `Guesty_booking_<p>.csv` (the UI-shaped export) also carries the
    CANCELED ones -- which is where a cancelled booking's dates have to come from.
    """
    to_property_id = bridge.to_property_id(statements_root)
    inputs = bridge.guesty_inputs(statements_root)

    by_pid_ci: dict[tuple[str, str], list[dict]] = defaultdict(list)
    by_ci_amt: dict[tuple[str, float], list[dict]] = defaultdict(list)
    by_pid_amt: dict[tuple[str, float], list[dict]] = defaultdict(list)

    def add(pid: str, ci: str, checkout: str, nights: str, payout: str,
            status: str = "confirmed") -> None:
        rec = {"checkin": ci[:10], "checkout": checkout, "nights": nights, "status": status}
        by_pid_ci[(pid, ci[:10])].append(rec)
        try:
            amt = round(float(payout), 2)
        except ValueError:
            return
        by_ci_amt[(ci[:10], amt)].append(rec)
        by_pid_amt[(pid, amt)].append(rec)

    for f in sorted(glob.glob(str(inputs / "*" / "guesty_converted.csv"))):
        for r in csv.DictReader(open(f, encoding="utf-8-sig")):
            if r.get("channel") == "Booking.com":
                add(r["property_id"], r["checkin"], r["checkout"], r["nights"], r["total_payout"])

    for f in sorted(glob.glob(str(inputs / "*" / "Guesty_booking_*.csv"))):
        for r in csv.DictReader(open(f, encoding="utf-8-sig")):
            if r.get("SOURCE") != "Booking.com":
                continue
            pid = to_property_id(r["LISTING'S NICKNAME"])
            if pid:
                add(pid, r["CHECK-IN"], r["CHECK-OUT"], r["NUMBER OF NIGHTS"],
                    r["TOTAL PAYOUT"], r["STATUS"])

    return by_pid_ci, by_ci_amt, by_pid_amt


# Individual bookings where the Booking.com export's nickname disagrees with Guesty about
# the listing.  Guesty is authoritative (owner-confirmed): guests were moved between units.
# Keyed by reservation number.
RESERVATION_CLASS_OVERRIDES = {
    "5410000096": "Listings:Elektra 1203",        # file said Elektra 1108
    "6633094326": "Listings:Seattle 7434 Upper",  # file said Elektra 703
    "5065374340": "Listings:OSBR 11",             # file said Cottage 9
    "5359475020": "Listings:Seattle 7434 Upper",  # file said Seattle 7434 Lower
}

OUT_COLS = [
    "JournalNo", "JournalDate", "LineNum", "Account",
    "Debits", "Credits", "Description", "Name", "Location", "Class",
]


def iso(d: str) -> str:
    """Normalise a payout date.  Exports come as M/D/YY or already ISO."""
    d = d.strip()
    for fmt in ("%m/%d/%y", "%Y-%m-%d", "%m/%d/%Y"):
        try:
            return datetime.strptime(d, fmt).strftime("%Y-%m-%d")
        except ValueError:
            continue
    raise ValueError(f"unrecognised date {d!r}")


def build(src: Path, qbo, statements_root: str | None = None) -> tuple[list[dict], list[str]]:
    to_property_id = bridge.to_property_id(statements_root)
    classes = {e["property_id"]: e.get("qbo_class_name")
               for e in yaml.safe_load(bridge.mapping_classes(statements_root).read_text())}
    rows = list(csv.DictReader(open(src, encoding="utf-8-sig")))
    by_pid_ci, by_ci_amt, by_pid_amt = load_guesty(statements_root)

    cust_cache: dict[str, str | None] = {}

    def customer(resno: str) -> str | None:
        if resno not in cust_cache:
            got = qbo.query(
                f"SELECT Id, DisplayName FROM Customer WHERE DisplayName LIKE '%{resno}%'"
            ).get("QueryResponse", {}).get("Customer", [])
            names = sorted({c["DisplayName"] for c in got})
            # A reservation can match more than one customer record (e.g. a stray
            # "booking.com - ..." duplicate alongside the canonical "bookingcom - ...").
            # The canonical prefix is the one carrying the invoice.
            canon = [n for n in names if n.startswith("bookingcom - ")]
            if len(canon) == 1:
                cust_cache[resno] = canon[0]
            else:
                cust_cache[resno] = names[0] if len(names) == 1 else None
        return cust_cache[resno]

    batches: dict[str, list[dict]] = defaultdict(list)
    for r in rows:
        batches[r["Statement.Descriptor"]].append(r)

    out: list[dict] = []
    warnings: list[str] = []

    for desc, items in batches.items():
        jdate = iso(items[0]["Payout.date"])
        line = 0
        for r in items:
            line += 1
            amt = float(r["Total.amount..gross."])
            nick = r["NICKNAME"]
            resno = r["Reservation.number"]

            pid = NICKNAME_OVERRIDES.get(nick) or to_property_id(nick)
            cls = classes.get(pid) if pid else None
            # A per-reservation override outranks the nickname: Guesty is authoritative
            # about which unit the guest actually stayed in.
            if resno in RESERVATION_CLASS_OVERRIDES:
                cls = RESERVATION_CLASS_OVERRIDES[resno]
            if not cls:
                cls = f"*** UNMAPPED {nick} ***"
                warnings.append(f"{desc} line {line}: no class for nickname {nick!r}")

            name = customer(resno)
            if not name:
                name = f"*** NO CUSTOMER {resno} ***"
                warnings.append(f"{desc} line {line}: no QBO customer for reservation {resno}")

            kind = "Reservation" if r["Transaction.type"] == "RESERVATION" else "Refund"

            ci = iso(r["Check.in.date"])
            stay = None
            cand = by_pid_ci.get((pid, ci), [])
            # Duplicates that agree on check-out and nights describe the same stay.
            if cand and len({(c["checkout"][:10], c["nights"], c["status"]) for c in cand}) == 1:
                stay = cand[0]
            else:
                # Either no stay at this property/date, or several that disagree; the payout
                # amount picks the right one in both cases.
                alt = by_ci_amt.get((ci, round(abs(amt), 2)), [])
                if len({(a["checkout"][:10], a["nights"], a["status"]) for a in alt}) == 1:
                    stay = alt[0]
                    if not cand:
                        warnings.append(
                            f"{desc} line {line}: {nick!r} (Property.ID {r['Property.ID']}) "
                            f"matched a stay on a DIFFERENT property by amount — CLASS MAY BE WRONG")
            if stay is None:
                # Booking.com's check-in is sometimes a day off Guesty's, so fall back to
                # (property, payout amount) and take the stay closest to the stated date.
                near = by_pid_amt.get((pid, round(abs(amt), 2)), [])
                if near:
                    d0 = datetime.strptime(ci, "%Y-%m-%d")
                    near = sorted(near, key=lambda x: abs(
                        (datetime.strptime(x["checkin"], "%Y-%m-%d") - d0).days))
                    if abs((datetime.strptime(near[0]["checkin"], "%Y-%m-%d") - d0).days) <= 3:
                        stay = near[0]

            if stay:
                # Show Guesty's own check-in, which is authoritative for the stay.
                ci = stay.get("checkin", ci)
                span = f"{ci} to {stay['checkout'][:10]} | {stay['nights']} nights"
                # Match QuickBooks' own wording for a voided stay ("... | Cancelled | ...").
                if stay["status"] == "canceled":
                    kind = f"{kind} | Cancelled"
            else:
                span = ci
                warnings.append(f"{desc} line {line}: no Guesty stay for reservation {resno} "
                                f"— check-in only, no check-out/nights")

            out.append({
                "JournalNo": desc, "JournalDate": jdate, "LineNum": line,
                "Account": CLEARING_ACCT,
                "Debits": f"{-amt:.2f}" if amt < 0 else "",
                "Credits": f"{amt:.2f}" if amt >= 0 else "",
                "Description": f"{kind} | bookingcom | {span}",
                "Name": name, "Location": LOCATION, "Class": cls,
            })

        total = round(sum(float(r["Total.amount..gross."]) for r in items), 2)
        line += 1
        out.append({
            "JournalNo": desc, "JournalDate": jdate, "LineNum": line, "Account": BANK_ACCT,
            "Debits": f"{total:.2f}", "Credits": "",
            "Description": f"Payout | Bank Account | {desc}",
            "Name": "", "Location": LOCATION, "Class": "",
        })
        if total <= 0:
            warnings.append(f"{desc}: batch total is {total:.2f} — not a deposit")

    return out, warnings


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--src", default=str(DEFAULT_IN))
    ap.add_argument("--out", default=str(review_csv("bookingcom_je")))
    ap.add_argument("--statements-root", default=None,
                    help="Owner_statement_whole checkout (class map + Guesty exports); "
                         "defaults to the sibling folder")
    ap.add_argument("--skip-existing", action="store_true",
                    help="drop JEs whose payout is already booked on the bank account")
    args = ap.parse_args()

    from ..config import client
    qbo = client()

    rows, warnings = build(Path(args.src), qbo, args.statements_root)

    if args.skip_existing:
        # Match on (date, bank debit), NOT DocNumber: the same payout may already be
        # booked under another numbering scheme, which a DocNumber check would miss.
        from .post import existing_payouts
        posted = set(existing_payouts(qbo))
        bank_of = {r["JournalNo"]: round(float(r["Debits"]), 2)
                   for r in rows if r["Account"] == BANK_ACCT}
        date_of = {r["JournalNo"]: r["JournalDate"] for r in rows}
        already = {d for d in bank_of if (date_of[d], bank_of[d]) in posted}
        rows = [r for r in rows if r["JournalNo"] not in already]
        warnings = [w for w in warnings if w.split(":")[0].split()[0] not in already]
        print(f"skip-existing: {len(already)} of {len(bank_of)} already in QBO, "
              f"{len(bank_of) - len(already)} new")

    os.makedirs(Path(args.out).parent, exist_ok=True)
    with open(args.out, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=OUT_COLS)
        w.writeheader()
        w.writerows(rows)

    def num(s: str) -> float:
        return float(s) if s else 0.0

    jes = len({r["JournalNo"] for r in rows})
    dr = sum(num(r["Debits"]) for r in rows)
    cr = sum(num(r["Credits"]) for r in rows)
    print(f"{jes} journal entries, {len(rows)} lines -> {args.out}")
    print(f"  debits {dr:,.2f}   credits {cr:,.2f}   diff {dr - cr:,.2f}")
    for w_ in warnings:
        print(f"  WARN {w_}")


if __name__ == "__main__":
    main()
