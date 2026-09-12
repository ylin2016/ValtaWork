"""Move Pass Through Tot lines from Tax Liability onto the Airbnb clearing account.

The Hawaii transient-accommodation tax is part of the Airbnb payout, and the reservation
invoice already recognises it under Guest Charges:Tax.  Crediting Tax Liability in the
payout JE therefore left that amount stranded in Payments Clearing - Airbnb, because the
reservation payment debits clearing for the FULL payout.

    python -m src.fixes.passthrough_tot                    # dry run, all
    python -m src.fixes.passthrough_tot --mine-only        # only JEs from this work
    python -m src.fixes.passthrough_tot --confirm          # apply
"""
from __future__ import annotations

import argparse

from ..config import acct_id, client
from ..je.payload import je_update

MINE = {"M-KRKY322GITYXL", "M-BRZ6TB5OGAOOU", "M-PCCP6WZZHJKL5"}


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--mine-only", action="store_true")
    ap.add_argument("--to-account", default=None,
                    help="account Id to move Pass Through Tot lines onto "
                         "(default: the Airbnb clearing account)")
    ap.add_argument("--start", default="2026-01-01")
    ap.add_argument("--end", default="2026-12-31")
    ap.add_argument("--confirm", action="store_true")
    args = ap.parse_args()

    to_account = args.to_account or acct_id("clearing_airbnb")
    qbo = client()
    jes = qbo.query_all(f"SELECT * FROM JournalEntry WHERE TxnDate >= '{args.start}' "
                        f"AND TxnDate <= '{args.end}'", "JournalEntry")

    done = moved = 0
    for j in jes:
        if args.mine_only and j.get("DocNumber") not in MINE:
            continue
        targets = [l for l in j.get("Line", [])
                   if "Pass Through Tot" in (l.get("Description") or "")
                   and (l.get("JournalEntryLineDetail") or {})
                   .get("AccountRef", {}).get("value") != to_account]
        if not targets:
            continue

        amt = sum(l["Amount"] for l in targets)
        if not args.confirm:
            print(f"  WOULD  {j.get('DocNumber',''):20} Id={j['Id']:7} {j['TxnDate']}  "
                  f"${amt:>9,.2f}  {len(targets)} line(s) -> acct {to_account}")
            done += 1
            moved += amt
            continue

        for l in targets:
            l["JournalEntryLineDetail"]["AccountRef"] = {"value": to_account}
        res = qbo.post("journalentry", je_update(j))
        print(f"  FIXED  {res.get('DocNumber',''):20} Id={res['Id']:7} {res['TxnDate']}  ${amt:>9,.2f}")
        done += 1
        moved += amt

    verb = "fixed" if args.confirm else "would fix"
    print(f"\n{verb} {done} JEs, ${moved:,.2f} moved to account {to_account}")
    if not args.confirm:
        print("DRY RUN — nothing written.")


if __name__ == "__main__":
    main()
