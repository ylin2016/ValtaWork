"""Post the monthly owner-payout Bills and their payments from a review CSV.

    python -m src.invoices.post_owner_payout --all                       # dry run
    python -m src.invoices.post_owner_payout --ref <PayRef>              # dry run, one
    python -m src.invoices.post_owner_payout --all --confirm             # WRITES

Two objects per row, in this order:

    Bill          Dr 2 - Owner Distributions (Payouts)   Cr PMS Clearing - A/P
    BillPayment   Dr PMS Clearing - A/P                  Cr Chase Trust 9967   (Check)

The BILL FIRST, always.  A payment cannot be linked to a bill that does not exist, and a
bill with no payment is a visible open A/P balance that the next run picks up -- where a
payment with no bill is money out of the trust against nothing.  If the payment fails the
run stops and names the Bill Id, because the pair is only half made.

**A payout is not always a Bill, so a clean Bill ledger proves nothing.**  Some owners are
paid by a direct ACH booked as an Expense straight to 1633 -- Beachwood is, every month --
and that is a Purchase object, invisible to any query over Bill and BillPayment.  Checking
only those two is exactly how $55,972.41 of channel cash got counted twice once already.
So the check reads **Purchase as well**, keyed on (Vendor, amount) over the whole window
rather than an exact date: a hand-entered ACH carries the bank's own date, which is not
the one on the payout sheet.  A hit is skipped and named, never posted "because the dates
differ".

This is NOT `invoices/post`.  That one writes Purchase objects and its duplicate key is
(bank, TxnDate, amount); a payout is a Bill + BillPayment and the same amount can leave
the same bank on the same day for two different owners, so the key here is
(Vendor, TxnDate, amount) and it is checked against BOTH objects:

  * the same owner already has a payout Bill that day -- the Bill would be duplicated;
  * the same owner was already PAID that day -- the cash would go out twice, which is
    the failure that matters, and it survives the Bill being deleted and rebuilt.

**The bank debits in BATCHES, not one line per property.**  A `Batch` column on the review
CSV names which bank debit each payout left in, and the run ends by printing each batch's
BillPayment Ids and their total, because that is the group the owner ticks under *Find
match*: QuickBooks matches one bank line against several transactions, and only if they
sum to the debit exactly.  The Ids are printed for skipped rows too -- a row already on the
books is still part of its batch, and leaving it out of the tick list breaks the match.

**The vendor half of every key is case-folded.**  QuickBooks resolves `DisplayName =
'Arvind Visvanathan'` case-insensitively but stores and returns `ARVIND VISVANATHAN`, so
a key built from the review CSV's spelling never matches one built from the object read
back -- and the duplicate check silently stops working for that owner, leaving only the
DocNumber, which is the one layer this project does not rely on.

A near-date window is deliberately NOT used.  Unlike a Zelle transfer, a payout is dated
by the batch and every row in a batch shares one date, so a hit a day either side is a
different batch, not the same one landing late.
"""
from __future__ import annotations

import argparse
import csv
import json
import sys
from collections import defaultdict

from ..config import acct_id, client, name as cfg_name
from ..paths import review_csv
from ..qbo_client import QBOClient
from ..resolver import Resolver, esc

PAYOUT_ACCOUNT_ID = acct_id("owner_distributions")
AP_ACCOUNT_ID = acct_id("pms_clearing_ap")
BANK = cfg_name("bank_str")
DOCNUMBER_MAX = 21


def existing(qbo: QBOClient, start: str, end: str) -> tuple[dict, dict, dict]:
    """Everything already on the books that a payout could duplicate.

    Returns (bills, payments, purchases).  The first two are keyed
    (vendor, TxnDate, amount); purchases are keyed (vendor, amount) ONLY, because a
    direct-ACH payout Expense carries the bank's date and the sheet carries another.
    """
    bills: dict[tuple[str, str, float], str] = {}
    for b in qbo.query_all(f"SELECT * FROM Bill WHERE TxnDate >= '{start}' "
                           f"AND TxnDate <= '{end}'", "Bill"):
        on_payout = any((l.get("AccountBasedExpenseLineDetail") or {})
                        .get("AccountRef", {}).get("value") == PAYOUT_ACCOUNT_ID
                        for l in b.get("Line", []))
        if not on_payout:
            continue
        key = (b["VendorRef"]["name"].casefold(), b["TxnDate"],
               round(float(b.get("TotalAmt") or 0), 2))
        bills[key] = b["Id"]

    pays: dict[tuple[str, str, float], str] = {}
    for p in qbo.query_all(f"SELECT * FROM BillPayment WHERE TxnDate >= '{start}' "
                           f"AND TxnDate <= '{end}'", "BillPayment"):
        key = (p["VendorRef"]["name"].casefold(), p["TxnDate"],
               round(float(p.get("TotalAmt") or 0), 2))
        pays[key] = p["Id"]

    purch: dict[tuple[str, float], list[tuple[str, str]]] = defaultdict(list)
    for e in qbo.query_all(f"SELECT * FROM Purchase WHERE TxnDate >= '{start}' "
                           f"AND TxnDate <= '{end}'", "Purchase"):
        payout = sum(float(l.get("Amount") or 0) for l in e.get("Line", [])
                     if (l.get("AccountBasedExpenseLineDetail") or {})
                     .get("AccountRef", {}).get("value") == PAYOUT_ACCOUNT_ID)
        if not payout:
            continue
        who = (e.get("EntityRef") or {}).get("name") or "(no payee)"
        purch[(who.casefold(), round(payout, 2))].append((e["TxnDate"], e["Id"]))
    return bills, pays, purch


def build_bill(row: dict, res: Resolver) -> tuple[dict, list[str]]:
    errs: list[str] = []
    vendor = res.vendor(row["Vendor"])
    if vendor is None:
        errs.append(f"vendor not found: {row['Vendor']!r}")
    klass = res.klass(row["Class"])
    if klass is None:
        errs.append(f"class not found: {row['Class']!r}")
    dept = res.department(row["Location"]) if row["Location"] else None
    if row["Location"] and dept is None:
        errs.append(f"location not found: {row['Location']!r}")
    acct = res.account(row["Account"])
    if acct is None:
        errs.append(f"account not found: {row['Account']!r}")
    elif acct != PAYOUT_ACCOUNT_ID:
        errs.append(f"account {row['Account']!r} is Id {acct}, not the payout account "
                    f"{PAYOUT_ACCOUNT_ID}")

    amount = round(float(row["Amount"]), 2)
    if amount <= 0:
        errs.append(f"amount is {amount:,.2f}; a payout is money out, so it must be positive")

    payload = {
        "VendorRef": {"value": vendor},
        "APAccountRef": {"value": AP_ACCOUNT_ID},
        "DocNumber": row["DocNumber"][:DOCNUMBER_MAX],
        "TxnDate": row["TxnDate"],
        "DueDate": row["TxnDate"],
        "PrivateNote": row["Memo"],
        "Line": [{
            "DetailType": "AccountBasedExpenseLineDetail",
            "Amount": amount,
            "Description": row["Description"],
            "AccountBasedExpenseLineDetail": {
                "AccountRef": {"value": acct},
                "ClassRef": {"value": klass},
                "BillableStatus": "NotBillable",
                "TaxCodeRef": {"value": "NON"},
            },
        }],
    }
    if dept:
        payload["DepartmentRef"] = {"value": dept}
    return payload, errs


def build_payment(row: dict, bill: dict, res: Resolver) -> tuple[dict, list[str]]:
    errs: list[str] = []
    bank_name = (row.get("PaidFrom") or "").strip() or BANK
    bank = res.account(bank_name)
    if bank is None:
        errs.append(f"paid-from account not found: {bank_name!r}")
    dept = res.department(row["Location"]) if row["Location"] else None
    payload = {
        "VendorRef": bill["VendorRef"],
        "PayType": "Check",
        "CheckPayment": {"BankAccountRef": {"value": bank}},
        "TxnDate": row["TxnDate"],
        "TotalAmt": round(float(bill["TotalAmt"]), 2),
        "Line": [{
            "Amount": round(float(bill["TotalAmt"]), 2),
            "LinkedTxn": [{"TxnId": bill["Id"], "TxnType": "Bill"}],
        }],
    }
    if dept:
        payload["DepartmentRef"] = {"value": dept}
    return payload, errs


def post_one(row: dict, qbo: QBOClient, res: Resolver, bills: dict, pays: dict,
             purch: dict, confirm: bool, verbose: bool) -> tuple[str, str]:
    """Returns (status, detail).  status in {skipped, error, ready, posted}."""
    if ("NEEDS VENDOR" in row["Vendor"] or "UNMAPPED" in row["Class"]
            or "NEEDS BANK" in (row.get("PaidFrom") or "")):
        return "error", "unresolved in the review CSV"

    doc = row["DocNumber"][:DOCNUMBER_MAX]
    already = qbo.query(f"SELECT * FROM Bill WHERE DocNumber = '{esc(doc)}'") \
        .get("QueryResponse", {}).get("Bill", [])
    if already:
        return "skipped", f"DocNumber {doc} already in QBO as Bill {already[0]['Id']}"

    amount = round(float(row["Amount"]), 2)
    key = (row["Vendor"].casefold(), row["TxnDate"], amount)
    if key in pays:
        return "skipped", (f"{row['Vendor']} was already paid ${amount:,.2f} on "
                           f"{row['TxnDate']} (BillPayment {pays[key]})")
    if key in bills:
        return "skipped", (f"{row['Vendor']} already has a ${amount:,.2f} payout Bill on "
                           f"{row['TxnDate']} (Bill {bills[key]})")
    hits = purch.get((row["Vendor"].casefold(), amount))
    if hits:
        where = ", ".join(f"{d} (Purchase {i})" for d, i in sorted(hits))
        return "skipped", (f"{row['Vendor']} was already paid ${amount:,.2f} by direct "
                           f"Expense on {where} -- posting this would pay it twice")

    payload, errs = build_bill(row, res)
    if errs:
        return "error", "; ".join(errs)
    if verbose:
        print(json.dumps(payload, indent=2))
    if not confirm:
        return "ready", f"Bill ${amount:,.2f} {row['Property']} -> {row['Class']}"

    bill = qbo.post("bill", payload)
    bills[key] = bill["Id"]
    if round(float(bill["TotalAmt"]), 2) != amount:
        sys.exit(f"Bill {bill['Id']} posted as ${bill['TotalAmt']} but the review CSV said "
                 f"${amount:,.2f}.  STOPPING -- no payment was made against it.")

    pay_payload, errs = build_payment(row, bill, res)
    if errs:
        sys.exit(f"Bill {bill['Id']} is POSTED but its payment could not be built: "
                 f"{'; '.join(errs)}.  Pay or delete it before re-running.")
    pay = qbo.post("billpayment", pay_payload)
    pays[key] = pay["Id"]
    return "posted", (f"Bill {bill['Id']} + Payment {pay['Id']}  ${amount:,.2f}  "
                      f"{row['Property']}")


def matching_guide(rows: list[dict], pays: dict) -> None:
    """What to tick under *Find match*, one group per bank debit.

    The bank pays a lump covering many properties, so the feed line is matched against the
    whole group.  A row that was SKIPPED (already on the books) still belongs to its group
    -- its payment is looked up by the same (Vendor, TxnDate, amount) key, and omitting it
    would leave the group short of the debit and unmatchable.
    """
    if not any(r.get("Batch") for r in rows):
        return
    groups: dict[str, list[dict]] = defaultdict(list)
    for r in rows:
        groups[r.get("Batch") or "(no batch named)"].append(r)
    print("\nWhat to tick under Find match, one group per bank debit:")
    for nm, rs in sorted(groups.items()):
        ids, missing, total = [], [], 0.0
        for r in rs:
            amt = round(float(r["Amount"]), 2)
            total += amt
            pid = pays.get((r["Vendor"].casefold(), r["TxnDate"], amt))
            (ids if pid else missing).append(pid or r["DocNumber"])
        print(f"\n  {nm}: {len(rs)} payment(s), ${round(total, 2):,.2f}")
        if ids:
            print(f"    BillPayment Ids: {', '.join(sorted(ids, key=int))}")
        if missing:
            print(f"    NOT ON THE BOOKS -- the group is short of the debit without them: "
                  f"{', '.join(missing)}")


def main() -> None:
    ap = argparse.ArgumentParser()
    g = ap.add_mutually_exclusive_group(required=True)
    g.add_argument("--ref", help="one PayRef from the review CSV")
    g.add_argument("--all", action="store_true")
    ap.add_argument("--csv", required=True)
    ap.add_argument("--confirm", action="store_true", help="actually POST to QuickBooks")
    ap.add_argument("--start", default=None, help="window scanned for already-booked payouts")
    ap.add_argument("--end", default=None)
    args = ap.parse_args()

    rows = list(csv.DictReader(open(args.csv, encoding="utf-8")))
    if args.ref:
        rows = [r for r in rows if r["PayRef"] == args.ref]
        if not rows:
            sys.exit(f"No row with PayRef {args.ref!r} in {args.csv}")

    dates = sorted({r["TxnDate"] for r in rows})
    start = args.start or f"{dates[0][:8]}01"
    end = args.end or dates[-1]

    qbo = client()
    res = Resolver(qbo)
    bills, pays, purch = existing(qbo, start, end)
    print(f"{start}..{end}: {len(bills)} payout Bill(s), {len(pays)} BillPayment(s) and "
          f"{len(purch)} direct-ACH payout Expense(s) already on the books\n")

    # Two rows for the same owner, date and amount are indistinguishable once posted:
    # the second would be skipped as a duplicate of the first.  Say so before, not after.
    seen: dict[tuple[str, str, float], list[str]] = defaultdict(list)
    for r in rows:
        seen[(r["Vendor"].casefold(), r["TxnDate"],
              round(float(r["Amount"]), 2))].append(r["Property"])
    for k, v in seen.items():
        if len(v) > 1:
            print(f"  NOTE {k[0]} has {len(v)} rows at ${k[2]:,.2f} on {k[1]} "
                  f"({', '.join(v)}) -- only the first will post.")

    tally: dict[str, int] = {}
    failures, posted_total = [], 0.0
    for r in rows:
        status, detail = post_one(r, qbo, res, bills, pays, purch, args.confirm,
                                  bool(args.ref))
        tally[status] = tally.get(status, 0) + 1
        if status == "posted":
            posted_total += round(float(r["Amount"]), 2)
        print(f"  {status.upper():8} {r['DocNumber']:<14} {r['Vendor'][:28]:<28} {detail}")
        if status == "error":
            failures.append(r["Property"])

    matching_guide(rows, pays)

    print("\n" + "  ".join(f"{k}={v}" for k, v in sorted(tally.items())))
    if args.confirm:
        print(f"posted ${posted_total:,.2f}")
    else:
        ready = round(sum(float(r["Amount"]) for r in rows), 2)
        print(f"DRY RUN -- nothing posted.  ${ready:,.2f} across {len(rows)} row(s). "
              f"Re-run with --confirm to POST.")
    if failures:
        sys.exit(f"{len(failures)} payout(s) could not be built: {', '.join(failures)}")


if __name__ == "__main__":
    main()
