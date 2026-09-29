"""Delete a JournalEntry that a Bill of the SAME DocNumber has replaced.

    python -m src.fixes.delete_superseded_je --doc-like 'SirenCL_%'              # dry run
    python -m src.fixes.delete_superseded_je --ids 119583,119584 --confirm       # DELETES

**This is the one place this project deletes anything, and the exception is narrow.**  The
standing rule is create-before-delete: a script writes the replacement and the owner removes
the original, because a delete cannot be undone and because a half-finished conversion is far
safer when the duplicate is visible than when the original is gone.  That rule's PURPOSE is
served once the replacement exists and ties out -- which is a thing a script can check better
than a human reading a register, and check against the live books rather than against the CSV
that drove the conversion.

So the precondition is not "the owner said so".  For every Id, ALL of these must hold or that
Id is refused and nothing about it is sent:

  * a Bill exists carrying the JE's own DocNumber;
  * that Bill's total equals the JE's total DEBITS;
  * the Bill's per-(account Id, class Id) subtotals equal the JE's DEBIT subtotals, exactly --
    **by Id, never by name.**  QuickBooks renders the same class differently depending on which
    object is asked (`Listings:Yacinde NuGrowth:Yacinde B6` from a Bill, `Yacinde B6` from a
    Purchase, both Id 1000000030), so a name comparison rejects correct objects and, worse,
    could accept mismatched ones across two object types;
  * the Bill's Balance is 0.00, which is the evidence its BillPayment exists too -- deleting the
    JE while only half the replacement is posted would leave the cash unrecorded.

A JE whose Bill is missing, unpaid, or differs by a cent is LEFT ALONE and named.  The refusal
is per Id, because each of these is a separate month and one bad month must not block the rest
-- but nothing partial happens WITHIN an Id: the checks all run before the delete is sent.

The JE's CREDIT side is deliberately not compared.  A Bill has no credit line -- its credit is
A/P, implicit on the object -- so there is nothing to compare it against.  That asymmetry is
the conversion, not a gap in the check: the JE credited the cash account the payment had
already hit, and the Bill's payment is what hits the bank now.

Every delete is verified by re-querying the Id, because QBO's delete response is not proof the
object is gone -- and an unverified delete is the one kind this project must not report as done.
"""
from __future__ import annotations

import argparse
import sys
from collections import defaultdict

from ..config import client
from ..qbo_client import QBOClient
from ..resolver import esc


def debit_subtotals(je: dict) -> tuple[dict, float]:
    """Per (account Id, class Id) DEBIT subtotals and their total."""
    out: dict[tuple, float] = defaultdict(float)
    total = 0.0
    for ln in je.get("Line", []):
        d = ln.get("JournalEntryLineDetail") or {}
        if d.get("PostingType") != "Debit":
            continue
        amt = round(float(ln.get("Amount") or 0), 2)
        total = round(total + amt, 2)
        key = ((d.get("AccountRef") or {}).get("value"), (d.get("ClassRef") or {}).get("value"))
        out[key] = round(out[key] + amt, 2)
    return dict(out), total


def bill_subtotals(bill: dict) -> tuple[dict, float]:
    out: dict[tuple, float] = defaultdict(float)
    total = 0.0
    for ln in bill.get("Line", []):
        d = ln.get("AccountBasedExpenseLineDetail") or {}
        if not d:
            continue
        amt = round(float(ln.get("Amount") or 0), 2)
        total = round(total + amt, 2)
        key = ((d.get("AccountRef") or {}).get("value"), (d.get("ClassRef") or {}).get("value"))
        out[key] = round(out[key] + amt, 2)
    return dict(out), total


def check(je: dict, bill: dict | None) -> list[str]:
    doc = je.get("DocNumber") or "(no DocNumber)"
    if bill is None:
        return [f"no Bill carries DocNumber {doc} -- nothing has replaced this JE"]
    errs = []
    je_sub, je_total = debit_subtotals(je)
    bl_sub, bl_total = bill_subtotals(bill)
    if abs(round(float(bill.get("TotalAmt") or 0), 2) - je_total) > 0.005:
        errs.append(f"Bill {bill['Id']} total {float(bill.get('TotalAmt') or 0):,.2f} != JE debits "
                    f"{je_total:,.2f}")
    if abs(bl_total - je_total) > 0.005:
        errs.append(f"Bill {bill['Id']} lines {bl_total:,.2f} != JE debits {je_total:,.2f}")
    if je_sub != bl_sub:
        only_je = {k: v for k, v in je_sub.items() if bl_sub.get(k) != v}
        only_bl = {k: v for k, v in bl_sub.items() if je_sub.get(k) != v}
        errs.append(f"Bill {bill['Id']} per-(account, class) subtotals differ: "
                    f"JE-only {only_je}, Bill-only {only_bl}")
    if abs(round(float(bill.get("Balance") or 0), 2)) > 0.005:
        errs.append(f"Bill {bill['Id']} still has Balance "
                    f"{float(bill.get('Balance') or 0):,.2f} -- its payment is not posted, so "
                    f"deleting this JE would leave the cash unrecorded")
    return errs


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.fixes.delete_superseded_je")
    g = ap.add_mutually_exclusive_group(required=True)
    g.add_argument("--ids", help="comma-separated JournalEntry Ids")
    g.add_argument("--doc-like", dest="doc_like",
                   help="SQL LIKE over DocNumber, e.g. 'SirenCL_%%'")
    ap.add_argument("--confirm", action="store_true", help="actually DELETE")
    args = ap.parse_args()

    qbo = client()
    if args.ids:
        want = [i.strip() for i in args.ids.split(",") if i.strip()]
        jes = []
        for i in want:
            got = qbo.query(f"SELECT * FROM JournalEntry WHERE Id = '{esc(i)}'") \
                .get("QueryResponse", {}).get("JournalEntry", [])
            if not got:
                print(f"  NOT FOUND  JE {i} -- already deleted, or never existed")
                continue
            jes.append(got[0])
    else:
        jes = qbo.query_all(f"SELECT * FROM JournalEntry WHERE DocNumber LIKE "
                            f"'{esc(args.doc_like)}' MAXRESULTS 500", "JournalEntry")
    jes.sort(key=lambda j: j.get("DocNumber") or "")
    if not jes:
        print("nothing to consider.")
        return

    docs = [j.get("DocNumber") for j in jes if j.get("DocNumber")]
    bills: dict[str, dict] = {}
    for d in set(docs):
        for b in qbo.query(f"SELECT * FROM Bill WHERE DocNumber = '{esc(d)}'") \
                .get("QueryResponse", {}).get("Bill", []):
            bills[d] = b

    go, refuse = [], []
    for je in jes:
        errs = check(je, bills.get(je.get("DocNumber") or ""))
        (refuse if errs else go).append((je, errs))

    print(f"{'JE':<8}{'DocNumber':<17}{'date':<12}{'debits':>10}  verdict")
    for je, _ in go:
        _, t = debit_subtotals(je)
        b = bills[je["DocNumber"]]
        print(f"{je['Id']:<8}{je['DocNumber']:<17}{je['TxnDate']:<12}{t:>10,.2f}  "
              f"replaced by Bill {b['Id']} -- verified, DELETE")
    for je, errs in refuse:
        _, t = debit_subtotals(je)
        print(f"{je['Id']:<8}{(je.get('DocNumber') or '-'):<17}{je['TxnDate']:<12}{t:>10,.2f}  "
              f"KEEP:")
        for e in errs:
            print(f"{'':<47}{e}")

    total = round(sum(debit_subtotals(j)[1] for j, _ in go), 2)
    if not args.confirm:
        print(f"\nDRY RUN -- nothing deleted.  {len(go)} JE(s) / {total:,.2f} would be deleted, "
              f"{len(refuse)} kept.\nRe-run with --confirm to DELETE.  This cannot be undone.")
        return

    print(f"\nDELETING {len(go)} JE(s), {total:,.2f}:")
    failed = []
    for je, _ in go:
        try:
            qbo.request("POST", f"/v3/company/{qbo.realm_id}/journalentry",
                        params={"operation": "delete"},
                        json_body={"Id": je["Id"], "SyncToken": je["SyncToken"]})
        except Exception as exc:                                  # noqa: BLE001
            print(f"   {je['Id']}  {je['DocNumber']:<17} FAILED: {str(exc)[:200]}")
            failed.append(je["Id"])
            continue
        # The delete RESPONSE is not proof.  Re-query: an Id that still returns is still posted.
        still = qbo.query(f"SELECT * FROM JournalEntry WHERE Id = '{esc(je['Id'])}'") \
            .get("QueryResponse", {}).get("JournalEntry", [])
        if still:
            print(f"   {je['Id']}  {je['DocNumber']:<17} *** STILL ON THE BOOKS after delete ***")
            failed.append(je["Id"])
        else:
            print(f"   {je['Id']}  {je['DocNumber']:<17} deleted and verified gone")
    if failed:
        sys.exit(f"\n{len(failed)} delete(s) did not land: {', '.join(failed)}")
    print(f"\n{len(go)} JE(s) deleted, {total:,.2f}.  {len(refuse)} left alone.")


if __name__ == "__main__":
    main()
