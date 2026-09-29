"""Change the VENDOR on a posted Bill, and nothing else.

    python -m src.fixes.bill_revendor --set 105803="AFN Shelton Advisor LLC" \
                                      --set 113026="Suet Ki Poon"  [--confirm]

An owner-charge Bill debits an owner-payable account, so the party it concerns is the property's
OWNER -- and that is what the house's other such Bills name (Wenge Wei's, Chengcen Zhao's).  Four
named the CLEANER instead, which reads as a debt to someone who had already been paid.

**The vendor is not derivable from the Bill**, so it is passed in per Id.  Which entity owns a
listing is a fact about the payout register, not about the Bill, and an owner can CHANGE: OSBR's
payouts went to `Valta SeaVac Fund | LP` through 2026-08-20 and to `Valta OceanSpray LLC` from
2026-08-31, so the right owner for a Bill depends on its DATE.  Nothing here guesses that; the
caller resolves it and this script records what it was told.

Guards, around every write:

  * the new vendor exists and is not the one already on the Bill;
  * `DocNumber`, `APAccountRef`, `TxnDate`, `DueDate`, `TotalAmt`, `Balance`, `LinkedTxn` and
    EVERY line's account / class / customer / amount / description are byte-identical after;
  * the object is re-read and must show the new vendor.

A Bill is FULL-updated by reading it back and echoing it whole -- QBO blanks every field the
payload omits, and a Bill carries `APAccountRef`, `DocNumber`, `DueDate`, `LinkedTxn` and
per-line `CustomerRef`/`ClassRef` that are invisible until they are gone.

This does NOT move money: a Bill's vendor is a party, not an account.  If the Bill still carries
an open balance the change merely moves that payable to the new vendor -- so net it to $0.00
first (`fixes/bill_offset_ap`) and this script warns when it has not been.
"""
from __future__ import annotations

import argparse
import copy
import sys

from ..config import client
from ..resolver import Resolver, esc


def fingerprint(bill: dict) -> tuple:
    lines = []
    for ln in bill.get("Line", []):
        d = ln.get("AccountBasedExpenseLineDetail") or {}
        lines.append((round(float(ln.get("Amount") or 0), 2), ln.get("Description"),
                      (d.get("AccountRef") or {}).get("value"),
                      (d.get("ClassRef") or {}).get("value"),
                      (d.get("CustomerRef") or {}).get("value")))
    return (bill.get("DocNumber"), (bill.get("APAccountRef") or {}).get("value"),
            bill.get("TxnDate"), bill.get("DueDate"),
            round(float(bill.get("TotalAmt") or 0), 2),
            round(float(bill.get("Balance") or 0), 2),
            tuple(sorted((t.get("TxnId"), t.get("TxnType")) for t in bill.get("LinkedTxn", []))),
            tuple(lines))


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.fixes.bill_revendor")
    ap.add_argument("--set", action="append", required=True, metavar="ID=VENDOR",
                    help='e.g. --set 105803="AFN Shelton Advisor LLC" (repeatable)')
    ap.add_argument("--confirm", action="store_true", help="actually WRITE")
    args = ap.parse_args()

    pairs = []
    for spec in args.set:
        if "=" not in spec:
            sys.exit(f"--set needs ID=VENDOR, got {spec!r}")
        bid, who = spec.split("=", 1)
        pairs.append((bid.strip(), who.strip()))

    qbo = client()
    res = Resolver(qbo)
    plan, skip = [], []
    for bid, who in pairs:
        got = qbo.query(f"SELECT * FROM Bill WHERE Id = '{esc(bid)}'") \
            .get("QueryResponse", {}).get("Bill", [])
        if not got:
            skip.append((bid, who, "Bill not found")); continue
        b = got[0]
        vid = res.vendor(who)
        if vid is None:
            skip.append((bid, who, f"vendor not found: {who!r}")); continue
        cur = (b.get("VendorRef") or {})
        if cur.get("value") == vid:
            skip.append((bid, who, "already this vendor")); continue
        plan.append((b, vid, who, cur.get("name") or "?"))

    print(f"{'Bill':<8}{'date':<12}{'bal':>8}  {'from':<26} -> to")
    for b, vid, who, was in plan:
        bal = round(float(b.get("Balance") or 0), 2)
        warn = "   *** still OPEN: this moves the payable, it does not clear it ***" if bal else ""
        print(f"{b['Id']:<8}{b['TxnDate']:<12}{bal:>8,.2f}  {was[:26]:<26} -> {who}{warn}")
    for bid, who, why in skip:
        print(f"{bid:<8}SKIP -- {why}")
    if not plan:
        print("\nnothing to do.")
        return
    if not args.confirm:
        print(f"\nDRY RUN -- nothing written.  {len(plan)} Bill(s) would be re-vendored.")
        return

    print()
    failed = []
    for b, vid, who, was in plan:
        want = fingerprint(b)
        body = copy.deepcopy(b)
        body["VendorRef"] = {"value": vid}
        body["sparse"] = False
        try:
            qbo.post("bill", body)
        except Exception as exc:                                    # noqa: BLE001
            print(f"   {b['Id']}  FAILED: {str(exc)[:200]}")
            failed.append(b["Id"]); continue
        back = qbo.query(f"SELECT * FROM Bill WHERE Id = '{esc(b['Id'])}'") \
            .get("QueryResponse", {}).get("Bill", [])[0]
        now = (back.get("VendorRef") or {}).get("name")
        if fingerprint(back) != want:
            print(f"   {b['Id']}  *** something other than the vendor changed ***")
            failed.append(b["Id"]); continue
        if (back.get("VendorRef") or {}).get("value") != vid:
            print(f"   {b['Id']}  *** vendor did not change (still {now}) ***")
            failed.append(b["Id"]); continue
        print(f"   {b['Id']}  {was[:26]:<26} -> {now}")
    if failed:
        sys.exit(f"\n{len(failed)} Bill(s) did not land: {', '.join(failed)}")
    print(f"\n{len(plan)} Bill(s) re-vendored; nothing else changed.")


if __name__ == "__main__":
    main()
