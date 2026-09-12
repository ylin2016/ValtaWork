"""Rebuild the Booking.com reservation payments onto the clearing account.

Undoing the bank-feed deposits left 149 zeroed Payment shells still pointing at the bank
and 149 reopened invoices.  This re-applies each payout to its invoice and deposits it to
Payments Clearing - Booking.com, so the payout JE (Dr bank / Cr clearing) is what the bank
deposit matches to.

    python -m src.payments.repoint            # dry run
    python -m src.payments.repoint --confirm  # write
"""
from __future__ import annotations

import argparse
import csv

from ..config import acct_id, client
from ..paths import review_csv
from ..resolver import esc

DEFAULT_CSV = review_csv("bookingcom_payment_rebuild")


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--csv", default=str(DEFAULT_CSV))
    ap.add_argument("--confirm", action="store_true", help="actually write to QuickBooks")
    args = ap.parse_args()

    qbo = client()
    default_clearing = acct_id("clearing_bookingcom")

    rows = list(csv.DictReader(open(args.csv, encoding="utf-8")))
    done = skipped = failed = 0
    applied = 0.0

    for r in rows:
        pid, inv_id, amt = r["PaymentId"], r["InvoiceId"], round(float(r["PayAmount"]), 2)
        # The CSV may target a different clearing account (Airbnb vs Booking.com).
        clearing = r.get("NewAcctId") or default_clearing

        # No payment shell exists for this booking -- create one instead of updating.
        if not pid:
            cust = qbo.query(
                f"SELECT Id FROM Customer WHERE DisplayName = '{esc(r['Customer'])}'"
            ).get("QueryResponse", {}).get("Customer", [])
            if not cust:
                print(f"  MISSING  customer {r['Customer']!r}")
                failed += 1
                continue
            body = {
                "CustomerRef": {"value": cust[0]["Id"]},
                "TxnDate": r["TxnDate"],
                "TotalAmt": amt,
                "DepositToAccountRef": {"value": clearing},
                "Line": [{"Amount": amt,
                          "LinkedTxn": [{"TxnId": inv_id, "TxnType": "Invoice"}]}],
            }
            if not args.confirm:
                print(f"  WOULD-NEW  {r['Customer'][:40]:40} ${amt:>9,.2f} -> inv {inv_id}")
                done += 1
                applied += amt
                continue
            res = qbo.post("payment", body)
            print(f"  CREATED  {res['Id']} {r['Customer'][:40]:40} ${res['TotalAmt']:>9,.2f} "
                  f"-> inv {inv_id}")
            done += 1
            applied += amt
            continue

        # Re-fetch: SyncToken must be current, and the invoice balance may have moved.
        got = qbo.query(f"SELECT * FROM Payment WHERE Id = '{pid}'").get(
            "QueryResponse", {}).get("Payment", [])
        if not got:
            print(f"  MISSING  payment {pid} ({r['Customer']})")
            failed += 1
            continue
        p = got[0]

        if round(p.get("TotalAmt", 0), 2) == amt and \
                p.get("DepositToAccountRef", {}).get("value") == clearing:
            print(f"  SKIP     {pid} {r['Customer'][:40]:40} already ${amt:,.2f} to clearing")
            skipped += 1
            continue

        payload = {
            "Id": p["Id"],
            "SyncToken": p["SyncToken"],
            "CustomerRef": p["CustomerRef"],
            "TxnDate": p["TxnDate"],
            "TotalAmt": amt,
            "DepositToAccountRef": {"value": clearing},
            "Line": [{
                "Amount": amt,
                "LinkedTxn": [{"TxnId": inv_id, "TxnType": "Invoice"}],
            }],
        }
        if p.get("PaymentRefNum"):
            payload["PaymentRefNum"] = p["PaymentRefNum"]

        if not args.confirm:
            print(f"  WOULD    {pid} {r['Customer'][:40]:40} ${amt:>9,.2f} -> inv {inv_id}")
            done += 1
            applied += amt
            continue

        try:
            res = qbo.post("payment", payload)
            print(f"  UPDATED  {res['Id']} {r['Customer'][:40]:40} ${res['TotalAmt']:>9,.2f} "
                  f"-> inv {inv_id}")
            done += 1
            applied += amt
        except Exception as e:  # noqa: BLE001 - report and keep going
            print(f"  FAILED   {pid} {r['Customer'][:40]:40} {e}")
            failed += 1

    verb = "updated" if args.confirm else "would update"
    print(f"\n{verb} {done}  skipped {skipped}  failed {failed}   ${applied:,.2f}")
    if not args.confirm:
        print("DRY RUN — nothing written. Re-run with --confirm.")


if __name__ == "__main__":
    main()
