"""Post guest pet / parking fees from review/guest_fees.csv (built by build_guest_fees).

    python -m src.deposits.post_guest_fees                 # dry run: what WOULD be created
    python -m src.deposits.post_guest_fees --confirm       # WRITES

Per bank deposit (one bank-feed line into 9967), in this order:

    1. per fee   NEW_INVOICE:  Invoice  <code>-PET / <code>-PARK, the fee item, classed to the property
    2. per fee   Payment       against the fee's invoice, DepositToAccount = Undeposited Funds
    3. once      Deposit       Undeposited Funds -> Chase Trust Checking 9967, linking the payments,
                               for exactly the bank line's total -- the object the feed matches

A bank deposit is posted only when EVERY fee in it is PAY_EXISTING or NEW_INVOICE and they add
up to the bank line; a HELD or ERROR fee stops its whole group, because a Deposit short of the
bank line cannot be matched.  If a later step fails after an earlier one posted, the run stops
and names what exists, so the next run picks up from there (each step finds its own earlier
work before creating anything):

    Invoice   same customer, DocNumber <code>-PET/-PARK
    Payment   same customer, same amount, TxnDate within 3 days, linked to that invoice
    Deposit   same TotalAmt into 9967 within 3 days (Deposit), or the same cash already in 9967
              another way (a Payment deposited straight to 9967, a JE debiting it) -- stop and ask

Every write is appended to review/CHANGE_LOG.csv, and review/guest_fees_posted.csv lists the
QBO Ids per submission (for the form's write-back, not built yet).
"""
from __future__ import annotations

import argparse
import csv
import sys
from collections import defaultdict
from datetime import date, datetime, timedelta
from pathlib import Path

from ..config import client, location, name as cfg_name
from ..paths import REVIEW_DIR, review_csv
from ..resolver import Resolver, esc

IN = review_csv("guest_fees")
POSTED = review_csv("guest_fees_posted")
CHANGE_LOG = REVIEW_DIR / "CHANGE_LOG.csv"
BANK = cfg_name("bank_str")
UF = cfg_name("undeposited_funds")
DOCNUMBER_MAX = 21
SUFFIX = {"Pet Fee (Owner)": "PET", "Parking Fee": "PARK"}


def near(d: str, days: int = 3) -> tuple[str, str]:
    d0 = datetime.strptime(d, "%Y-%m-%d").date()
    return (d0 - timedelta(days=days)).isoformat(), (d0 + timedelta(days=days)).isoformat()


def fee_doc(row: dict) -> str:
    return f"{row['Code']}-{SUFFIX.get(row['Item'], 'FEE')}"[:DOCNUMBER_MAX]


def log_change(action: str, typ: str, qid: str, ref: str, txn_date: str, amount: str, detail: str) -> None:
    new = not CHANGE_LOG.exists()
    with CHANGE_LOG.open("a", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        if new:
            w.writerow(["ChangedOn", "Action", "Type", "QBO_Id", "Ref", "TxnDate", "Amount", "Detail"])
        w.writerow([date.today().isoformat(), action, typ, qid, ref, txn_date, amount, detail])


def find_fee_invoice(qbo, row: dict) -> dict | None:
    got = qbo.query(f"SELECT * FROM Invoice WHERE CustomerRef = '{esc(row['CustomerId'])}' "
                    f"AND DocNumber = '{esc(fee_doc(row))}'").get("QueryResponse", {}).get("Invoice", [])
    return got[0] if got else None


def find_payment(qbo, row: dict, invoice_id: str) -> dict | None:
    lo, hi = near(row["TxnDate"])
    pays = qbo.query_all(f"SELECT * FROM Payment WHERE CustomerRef = '{esc(row['CustomerId'])}' "
                         f"AND TxnDate >= '{lo}' AND TxnDate <= '{hi}'", "Payment")
    amt = round(float(row["Amount"]), 2)
    for p in pays:
        linked = {t.get("TxnId") for l in p.get("Line", []) for t in l.get("LinkedTxn", [])
                  if t.get("TxnType") == "Invoice"}
        if round(float(p.get("TotalAmt") or 0), 2) == amt and invoice_id in linked:
            return p
    return None


def cash_already_in_bank(qbo, bank_id: str, bank_date: str, total: float) -> list[str]:
    """Anything already putting this amount into 9967 around that date (the content key)."""
    lo, hi = near(bank_date)
    hits = []
    for d in qbo.query_all(f"SELECT * FROM Deposit WHERE TxnDate >= '{lo}' AND TxnDate <= '{hi}'", "Deposit"):
        if (d.get("DepositToAccountRef") or {}).get("value") == bank_id and round(float(d.get("TotalAmt") or 0), 2) == total:
            hits.append(f"Deposit {d['Id']} ({d['TxnDate']})")
    for p in qbo.query_all(f"SELECT * FROM Payment WHERE TxnDate >= '{lo}' AND TxnDate <= '{hi}'", "Payment"):
        if (p.get("DepositToAccountRef") or {}).get("value") == bank_id and round(float(p.get("TotalAmt") or 0), 2) == total:
            hits.append(f"Payment {p['Id']} deposited straight to the bank ({p['TxnDate']})")
    for j in qbo.query_all(f"SELECT * FROM JournalEntry WHERE TxnDate >= '{lo}' AND TxnDate <= '{hi}'", "JournalEntry"):
        for l in j.get("Line", []):
            det = l.get("JournalEntryLineDetail") or {}
            if (det.get("PostingType") == "Debit" and (det.get("AccountRef") or {}).get("value") == bank_id
                    and round(float(l.get("Amount") or 0), 2) == total):
                hits.append(f"JournalEntry {j['Id']} ({j['TxnDate']})")
    return hits


def invoice_payload(row: dict, res: Resolver) -> dict:
    amt = round(float(row["Amount"]), 2)
    body = {"CustomerRef": {"value": row["CustomerId"]}, "TxnDate": row["TxnDate"], "DocNumber": fee_doc(row),
            "PrivateNote": f"expense form #{row['SubmissionId']}: {row['FeeType']} paid by {row['Method']} "
                           f"({row['Payer']}), stay {row['CheckIn']}",
            "Line": [{"LineNum": 1, "Amount": amt, "DetailType": "SalesItemLineDetail",
                      "Description": f"{row['FeeType']} - {row['Code']} - {row['Property']}",
                      "SalesItemLineDetail": {"ItemRef": {"value": res.item(row["Item"])},
                                              "ClassRef": {"value": res.klass(row["Class"])},
                                              "Qty": 1, "UnitPrice": amt, "TaxCodeRef": {"value": "NON"}}}]}
    dept = res.department(location())
    if dept:
        body["DepartmentRef"] = {"value": dept}
    return body


def payment_payload(row: dict, invoice_id: str, uf_id: str) -> dict:
    amt = round(float(row["Amount"]), 2)
    return {"CustomerRef": {"value": row["CustomerId"]}, "TxnDate": row["TxnDate"], "TotalAmt": amt,
            "DepositToAccountRef": {"value": uf_id},
            "PrivateNote": f"expense form #{row['SubmissionId']}: {row['FeeType']} by {row['Method']} ({row['Payer']})",
            "Line": [{"Amount": amt, "LinkedTxn": [{"TxnId": invoice_id, "TxnType": "Invoice"}]}]}


def deposit_payload(bank_date: str, bank_id: str, pays: list[tuple[str, float]], memo: str, dept: str | None) -> dict:
    body = {"TxnDate": bank_date, "DepositToAccountRef": {"value": bank_id}, "PrivateNote": memo,
            "Line": [{"Amount": a, "LinkedTxn": [{"TxnId": pid, "TxnType": "Payment", "TxnLineId": "0"}]}
                     for pid, a in pays]}
    if dept:
        body["DepartmentRef"] = {"value": dept}
    return body


def post_group(bid: str, rows: list[dict], qbo, res: Resolver, confirm: bool, out: list[dict]) -> str:
    """One bank deposit. Returns a one-line status."""
    bad = [r for r in rows if r["Treatment"] not in ("PAY_EXISTING", "NEW_INVOICE")]
    if bad:
        return f"bank deposit {bid}: SKIPPED -- {', '.join(r['SubmissionId'] + ' ' + r['Treatment'] for r in bad)}"
    total = round(sum(float(r["Amount"]) for r in rows), 2)
    bank_total = round(float(rows[0]["BankTotal"]), 2)
    if abs(total - bank_total) > 0.005:
        return f"bank deposit {bid}: SKIPPED -- fees ${total:.2f} != bank line ${bank_total:.2f}"
    bank_id, uf_id = res.account(BANK), res.account(UF)
    if not bank_id or not uf_id:
        return f"bank deposit {bid}: SKIPPED -- account {BANK if not bank_id else UF!r} not found"

    pays: list[tuple[str, float]] = []
    plan: list[str] = []
    for r in rows:
        amt = round(float(r["Amount"]), 2)
        inv_id = r["InvoiceId"]
        if r["Treatment"] == "NEW_INVOICE":
            inv = find_fee_invoice(qbo, r)
            if inv:
                inv_id = inv["Id"]
                plan.append(f"#{r['SubmissionId']} invoice {fee_doc(r)} exists ({inv_id})")
            elif not confirm:
                plan.append(f"#{r['SubmissionId']} WOULD create Invoice {fee_doc(r)} ${amt:.2f} {r['Item']} / {r['Class']}")
            else:
                inv = qbo.post("invoice", invoice_payload(r, res))
                inv_id = inv["Id"]
                log_change("Guest fee invoice created", "Invoice", inv_id, fee_doc(r), r["TxnDate"], f"{amt:.2f}",
                           f"form #{r['SubmissionId']} {r['Item']}")
                plan.append(f"#{r['SubmissionId']} CREATED Invoice {inv_id}")
        pay = find_payment(qbo, r, inv_id) if inv_id else None
        if pay:
            plan.append(f"#{r['SubmissionId']} payment exists ({pay['Id']})")
        elif not confirm:
            plan.append(f"#{r['SubmissionId']} WOULD create Payment ${amt:.2f} -> {UF}")
        else:
            try:
                pay = qbo.post("payment", payment_payload(r, inv_id, uf_id))
            except Exception as e:  # noqa: BLE001 -- name what already exists, then stop
                sys.exit(f"bank deposit {bid}: payment for #{r['SubmissionId']} FAILED ({e}). "
                         f"Already posted this run: {plan}. Fix and re-run -- existing objects are found, not duplicated.")
            log_change("Guest fee payment created", "Payment", pay["Id"], r["Code"], r["TxnDate"], f"{amt:.2f}",
                       f"form #{r['SubmissionId']} -> {UF}")
            plan.append(f"#{r['SubmissionId']} CREATED Payment {pay['Id']}")
        if pay:
            pays.append((pay["Id"], amt))
        out.append({"SubmissionId": r["SubmissionId"], "BankDepositId": bid, "InvoiceId": inv_id or "",
                    "PaymentId": (pay or {}).get("Id", ""), "DepositId": "", "Treatment": r["Treatment"]})

    bank_date = rows[0]["BankDate"]
    lo, hi = near(bank_date)
    ours = {pid for pid, _ in pays}
    for d in qbo.query_all(f"SELECT * FROM Deposit WHERE TxnDate >= '{lo}' AND TxnDate <= '{hi}'", "Deposit"):
        linked = {t.get("TxnId") for l in d.get("Line", []) for t in l.get("LinkedTxn", []) if t.get("TxnType") == "Payment"}
        if ours and linked == ours:                          # this group's own Deposit, from an earlier run
            for o in out:
                if o["BankDepositId"] == bid:
                    o["DepositId"] = d["Id"]
            return f"bank deposit {bid}: {'; '.join(plan)}; Deposit exists ({d['Id']}) -- nothing to do"
    hits = cash_already_in_bank(qbo, bank_id, bank_date, bank_total)
    if hits:
        return f"bank deposit {bid}: {'; '.join(plan)}; Deposit NOT made -- ${bank_total:.2f} already in 9967: {', '.join(hits)}"
    if not confirm:
        return f"bank deposit {bid}: {'; '.join(plan)}; WOULD create Deposit ${bank_total:.2f} on {bank_date} -> {BANK}"
    if len(pays) != len(rows):
        sys.exit(f"bank deposit {bid}: only {len(pays)} of {len(rows)} payments exist -- NOT depositing. {plan}")
    memo = "expense form bank deposit " + bid + ": " + ", ".join(f"{r['Code']} {r['FeeType']}" for r in rows)
    dep = qbo.post("deposit", deposit_payload(bank_date, bank_id, pays, memo, res.department(location())))
    log_change("Guest fee deposit created", "Deposit", dep["Id"], f"bank_deposit {bid}", bank_date,
               f"{bank_total:.2f}", f"{len(pays)} payment(s) from {UF}")
    for o in out:
        if o["BankDepositId"] == bid:
            o["DepositId"] = dep["Id"]
    return f"bank deposit {bid}: {'; '.join(plan)}; CREATED Deposit {dep['Id']} ${bank_total:.2f}"


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--csv", default=str(IN))
    ap.add_argument("--confirm", action="store_true", help="write to QuickBooks")
    args = ap.parse_args()
    with open(args.csv, encoding="utf-8-sig", newline="") as fh:
        rows = list(csv.DictReader(fh))
    groups = defaultdict(list)
    for r in rows:
        groups[r["BankDepositId"]].append(r)
    qbo = client()
    res = Resolver(qbo)
    out: list[dict] = []
    for bid in sorted(groups, key=lambda x: int(x) if x.isdigit() else 0):
        print(post_group(bid, groups[bid], qbo, res, args.confirm, out))
    if args.confirm and out:
        with open(POSTED, "a", newline="", encoding="utf-8") as fh:
            w = csv.DictWriter(fh, fieldnames=list(out[0]))
            if fh.tell() == 0:
                w.writeheader()
            w.writerows(out)
        print(f"Ids -> {POSTED}")
    if not args.confirm:
        print("\nDRY RUN -- nothing written. Re-run with --confirm.")


if __name__ == "__main__":
    main()
