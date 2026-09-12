"""Recategorise channel refund lines between the clearing accounts and Resolutions.

Every other refund/resolution/adjustment in the books lands on
`Trust Liabilities:Owner Payables:1A - Net Earnings:Resolutions`; the Booking.com payout
JEs were writing them against the clearing account instead.

    python -m src.fixes.refunds_to_resolutions                     # dry run
    python -m src.fixes.refunds_to_resolutions --confirm           # apply
    python -m src.fixes.refunds_to_resolutions --refund-for-charge # + deferred-revenue lines
    python -m src.fixes.refunds_to_resolutions --cancelled-back    # cancelled ones back to clearing
"""
from __future__ import annotations

import argparse

from ..config import acct_id, accounts, client
from ..je.payload import je_update


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--confirm", action="store_true")
    ap.add_argument("--refund-for-charge", action="store_true",
                    help="also move 'REFUND FOR CHARGE' lines off the deferred-revenue account")
    ap.add_argument("--cancelled-back", action="store_true",
                    help="reverse: put CANCELLED-booking refunds back on the clearing account")
    ap.add_argument("--start", default="2026-01-01")
    ap.add_argument("--end", default="2026-12-31")
    args = ap.parse_args()

    resolutions_id = acct_id("resolutions")
    clearing_bookingcom_id = acct_id("clearing_bookingcom")
    clearing_suffixes = tuple(accounts()["clearing_suffixes"])

    qbo = client()
    jes = qbo.query_all(f"SELECT * FROM JournalEntry WHERE TxnDate >= '{args.start}' "
                        f"AND TxnDate <= '{args.end}'", "JournalEntry")

    done = moved = 0
    for j in jes:
        targets = []
        if args.cancelled_back:
            # A cancelled booking's reservation and refund both belong in clearing, where
            # they wash: the stay never happened, so no owner payable arises.
            for l in j.get("Line", []):
                d = l.get("JournalEntryLineDetail")
                if not d:
                    continue
                desc = l.get("Description") or ""
                if (desc.startswith("Refund |") and "Cancelled" in desc
                        and "bookingcom" in desc
                        and "Resolutions" in (d["AccountRef"].get("name") or "")):
                    targets.append(l)
            if targets:
                amt = sum(l["Amount"] for l in targets)
                if not args.confirm:
                    print(f"  WOULD  {j.get('DocNumber',''):20} Id={j['Id']:7} {j['TxnDate']}  "
                          f"${amt:>9,.2f}  {len(targets)} line(s) -> Clearing Booking.com")
                    done += 1
                    moved += amt
                    continue
                for l in targets:
                    l["JournalEntryLineDetail"]["AccountRef"] = {"value": clearing_bookingcom_id}
                res = qbo.post("journalentry", je_update(j))
                print(f"  BACK   {res.get('DocNumber',''):20} Id={res['Id']:7} {res['TxnDate']}  ${amt:>9,.2f}")
                done += 1
                moved += amt
            continue

        for l in j.get("Line", []):
            d = l.get("JournalEntryLineDetail")
            if not d:
                continue
            # A JE line carries the FULLY QUALIFIED account name, so match the suffix.
            acct = d["AccountRef"].get("name") or ""
            desc = l.get("Description") or ""
            if acct.endswith(clearing_suffixes) and "Refund" in desc:
                targets.append(l)
            # The other system's Stripe payout JEs book guest refunds against
            # Accrued/Deferred TRUST Revenue/Expense; they belong in Resolutions.
            elif (args.refund_for_charge and "REFUND FOR CHARGE" in desc.upper()
                    and "Resolutions" not in acct):
                targets.append(l)
        if not targets:
            continue

        amt = sum(l["Amount"] for l in targets)
        if not args.confirm:
            print(f"  WOULD  {j.get('DocNumber',''):20} Id={j['Id']:7} {j['TxnDate']}  "
                  f"${amt:>9,.2f}  {len(targets)} line(s) -> Resolutions")
            done += 1
            moved += amt
            continue

        for l in targets:
            l["JournalEntryLineDetail"]["AccountRef"] = {"value": resolutions_id}
        res = qbo.post("journalentry", je_update(j))
        print(f"  MOVED  {res.get('DocNumber',''):20} Id={res['Id']:7} {res['TxnDate']}  ${amt:>9,.2f}")
        done += 1
        moved += amt

    dest = "clearing" if args.cancelled_back else "Resolutions"
    verb = "moved" if args.confirm else "would move"
    print(f"\n{verb} {done} JEs, ${moved:,.2f} to {dest}")
    if not args.confirm:
        print("DRY RUN — nothing written.")


if __name__ == "__main__":
    main()
