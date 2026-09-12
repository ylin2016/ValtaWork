"""Repoint fee-rebill Bills off the Supplies account and onto their own.

Every `Valta Realty - Channel Fee` and `- Stripe Fee` Bill credited
`Billable Expense Income - Supplies Owner`, whatever the cost actually was.  The owner
statement categorises on the ACCOUNT NAME (`(?i)suppl`), so the offsetting half of every
fee Bill was filed under the owner's *Supplies* line while its matching charge went to
Other Expense -- 718 lines and $22,828.07 across 73 properties.  The correctly named
accounts already existed and were unused.

    Valta Realty - Channel Fee   ->  Billable Expense Income - Channel Fee
    Valta Realty - Stripe Fee    ->  Billable Expense Income - Stripe Fee

Only the AccountRef on that one line changes.  Amount, class, memo, every other line and
every header field are carried through untouched, so no figure moves -- both accounts sit
under the same `Billable Expense Income` parent, so even the P&L subtotal is unchanged.

    python -m src.fixes.fee_rebill_accounts                      # dry run + review CSV
    python -m src.fixes.fee_rebill_accounts --limit 1 --confirm  # prove it on one Bill
    python -m src.fixes.fee_rebill_accounts --confirm            # the whole run

    # the minority that credited plain `Billable Expense Income` instead:
    python -m src.fixes.fee_rebill_accounts --from billable_expense_income --vendor channel

The run is resumable: every updated Bill Id is appended to review/fee_rebill_done.csv and
skipped on the next pass, so an interrupted run picks up where it stopped.

A Bill is FULL-updated: QBO blanks any field the payload omits, so the whole object is
read back and echoed with one AccountRef changed -- never a hand-built payload.
"""
from __future__ import annotations

import argparse
import csv
import json
import sys
import time
from pathlib import Path

from ..config import acct_id, acct_name, client
from ..paths import review_csv
from ..qbo_client import QBOClient

# The account being moved OFF, as an accounts.yml key.  Set by --from; matched on the Id,
# not the name -- `Billable Expense Income` is also the leading segment of every child
# account's full name, so a name comparison is one careless edit away from matching all.
SOURCE_KEY = "billable_supplies_owner"

# Bill vendor -> the account its rebill line should credit.
TARGETS = {
    "Valta Realty - Channel Fee": ("billable_channel_fee", "Channel Fee"),
    "Valta Realty - Stripe Fee": ("billable_stripe_fee", "Stripe Fee"),
}



def done_log() -> Path:
    """One resume log per SOURCE account: a Bill finished in one pass must not be skipped
    by a pass moving a different line off it."""
    if SOURCE_KEY == "billable_supplies_owner":
        return review_csv("fee_rebill_done")          # the original run's log, kept as-is
    return review_csv(f"fee_rebill_done_{SOURCE_KEY}")
# QuickBooks throttles at 500 requests/minute per realm; stay comfortably under.
PAUSE = 0.12


def lines_to_fix(bill: dict) -> list[int]:
    """Indexes of the lines crediting the Supplies account."""
    out = []
    for n, ln in enumerate(bill.get("Line", [])):
        det = ln.get("AccountBasedExpenseLineDetail") or {}
        if (det.get("AccountRef") or {}).get("value") == acct_id(SOURCE_KEY):
            out.append(n)
    return out


def scan(qbo: QBOClient, start: str, end: str) -> tuple[list[dict], dict[str, dict]]:
    """Every fee Bill in the window that still points at the Supplies account.

    The Bill objects are kept, not just described: the update re-uses the one already
    read here instead of fetching it again, which halves the API calls over a run this
    size.  A SyncToken that has gone stale in the meantime is caught and re-read.
    """
    rows = []
    keep: dict[str, dict] = {}
    for b in qbo.query_all(f"SELECT * FROM Bill WHERE TxnDate >= '{start}' "
                           f"AND TxnDate <= '{end}'", "Bill"):
        vendor = (b.get("VendorRef") or {}).get("name")
        if vendor not in TARGETS:
            continue
        idxs = lines_to_fix(b)
        if not idxs:
            continue
        key, label = TARGETS[vendor]
        keep[b["Id"]] = b
        for n in idxs:
            ln = b["Line"][n]
            det = ln["AccountBasedExpenseLineDetail"]
            rows.append({
                "BillId": b["Id"], "TxnDate": b["TxnDate"], "Vendor": vendor,
                "LineNum": n + 1, "Amount": f"{float(ln.get('Amount', 0)):.2f}",
                "FromAccount": acct_name(SOURCE_KEY), "ToAccount": acct_name(key),
                "ToAcctId": acct_id(key),
                "Class": (det.get("ClassRef") or {}).get("name", ""),
                "Memo": (b.get("PrivateNote") or "")[:60],
            })
    return rows, keep


def load_done() -> set[str]:
    DONE_LOG = done_log()
    if not DONE_LOG.exists():
        return set()
    with DONE_LOG.open(encoding="utf-8") as fh:
        # An ERROR row is not done -- it must be retried, not skipped for ever.
        return {r["BillId"] for r in csv.DictReader(fh)
                if not (r.get("Detail") or "").startswith("ERROR")}


def update_one(qbo: QBOClient, bill_id: str, target_id: str,
               bill: dict | None = None, _retry: bool = True) -> tuple[int, str]:
    """Repoint a Bill's Supplies lines and full-update it.  Returns (lines, detail)."""
    if bill is None:
        got = qbo.query(f"SELECT * FROM Bill WHERE Id = '{bill_id}'") \
                 .get("QueryResponse", {}).get("Bill", [])
        if not got:
            return 0, "bill not found"
        bill = got[0]
    bill = json.loads(json.dumps(bill))     # never mutate the caller's copy
    idxs = lines_to_fix(bill)
    if not idxs:
        return 0, "already repointed"

    for n in idxs:
        # Replace the ref outright: a stale `name` alongside a new `value` is how QBO
        # ends up honouring neither.
        bill["Line"][n]["AccountBasedExpenseLineDetail"]["AccountRef"] = {"value": target_id}

    before = round(float(bill.get("TotalAmt") or 0), 2)
    bill["sparse"] = False
    try:
        res = qbo.post("bill", bill)      # post() already unwraps the entity
    except Exception as e:                # noqa: BLE001
        # Stale SyncToken: the cached copy lost a race. Re-read once and redo.
        if _retry and ("Stale" in str(e) or "SyncToken" in str(e) or "5010" in str(e)):
            return update_one(qbo, bill_id, target_id, bill=None, _retry=False)
        raise
    after = round(float(res.get("TotalAmt") or 0), 2)
    if abs(before - after) > 0.005:
        return len(idxs), f"TOTAL CHANGED {before:,.2f} -> {after:,.2f}"
    return len(idxs), f"{len(idxs)} line(s), total unchanged at {after:,.2f}"


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--start", default="2024-01-01")
    ap.add_argument("--end", default="2027-12-31")
    ap.add_argument("--from", dest="source", default="billable_supplies_owner",
                    help="accounts.yml key of the account to move lines OFF")
    ap.add_argument("--vendor", choices=["channel", "stripe", "both"], default="both")
    ap.add_argument("--out", default=None)
    ap.add_argument("--limit", type=int, default=0, help="stop after N bills (trial run)")
    ap.add_argument("--confirm", action="store_true", help="actually WRITE to QuickBooks")
    args = ap.parse_args()

    global SOURCE_KEY
    SOURCE_KEY = args.source
    acct_id(SOURCE_KEY)                       # an unknown key fails here, before any read
    keep = {"channel": ["Valta Realty - Channel Fee"], "stripe": ["Valta Realty - Stripe Fee"],
            "both": list(TARGETS)}[args.vendor]
    for v in list(TARGETS):
        if v not in keep:
            del TARGETS[v]
    if args.out is None:
        args.out = str(review_csv("fee_rebill" if SOURCE_KEY == "billable_supplies_owner"
                                  else f"fee_rebill_{SOURCE_KEY}_{args.vendor}"))
    print(f"moving lines OFF {acct_name(SOURCE_KEY)!r} on: {', '.join(TARGETS)}")

    qbo = client()
    print(f"scanning Bills {args.start} .. {args.end} …")
    rows, cached = scan(qbo, args.start, args.end)

    out = Path(args.out)
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()) if rows else
                           ["BillId", "TxnDate", "Vendor", "LineNum", "Amount",
                            "FromAccount", "ToAccount", "ToAcctId", "Class", "Memo"])
        w.writeheader()
        w.writerows(rows)

    by_bill: dict[str, str] = {}
    for r in rows:
        by_bill[r["BillId"]] = r["ToAcctId"]
    nonzero = sum(1 for r in rows if abs(float(r["Amount"])) > 0.005)
    print(f"{len(rows)} lines on {len(by_bill)} bills -> {out}")
    print(f"  {nonzero} lines carry an amount; {len(rows) - nonzero} are $0.00 (cancelled bookings)")
    for vendor in TARGETS:
        n = sum(1 for r in rows if r["Vendor"] == vendor)
        if n:
            print(f"  {vendor:30} {n:>6} lines")

    done = load_done()
    todo = [(bid, tid) for bid, tid in by_bill.items() if bid not in done]
    if done:
        print(f"  {len(done)} bills already done in a previous run; {len(todo)} left")

    if not args.confirm:
        print("\nDRY RUN — nothing written. Re-run with --confirm to UPDATE.")
        return

    if args.limit:
        todo = todo[:args.limit]
    print(f"\nupdating {len(todo)} bills …")

    DONE_LOG = done_log()
    fresh = not DONE_LOG.exists()
    log = DONE_LOG.open("a", newline="", encoding="utf-8")
    logw = csv.DictWriter(log, fieldnames=["BillId", "ToAcctId", "Lines", "Detail"])
    if fresh:
        logw.writeheader()

    ok = fail = 0
    failures: list[str] = []
    for i, (bid, tid) in enumerate(todo, start=1):
        try:
            n, detail = update_one(qbo, bid, tid, bill=cached.get(bid))
            logw.writerow({"BillId": bid, "ToAcctId": tid, "Lines": n, "Detail": detail})
            ok += 1
            if "TOTAL CHANGED" in detail:
                sys.exit(f"ABORT — Bill {bid}: {detail}")
        except Exception as e:                      # noqa: BLE001 -- keep going, record it
            fail += 1
            failures.append(f"{bid}: {e}")
            logw.writerow({"BillId": bid, "ToAcctId": tid, "Lines": 0, "Detail": f"ERROR {e}"})
        if i % 25 == 0:
            log.flush()
            print(f"    {i}/{len(todo)}  ok={ok} failed={fail}")
        time.sleep(PAUSE)

    log.close()
    print(f"\nupdated {ok} bills, {fail} failed")
    for f in failures[:10]:
        print(f"  FAILED {f}")
    if fail:
        sys.exit(f"{fail} bill(s) failed; re-run to retry them")


if __name__ == "__main__":
    main()
