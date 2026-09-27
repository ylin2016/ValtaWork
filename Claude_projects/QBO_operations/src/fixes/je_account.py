"""Repoint one account on an existing journal entry, changing nothing else.

A JE posted to the wrong account cannot be corrected by re-running its builder: the
DocNumber check refuses it as already posted, and deleting is not something this project
does.  The fix is a FULL UPDATE -- read the object back, change the one `AccountRef`, and
echo the rest whole.

    python -m src.fixes.je_account --docnum TRUSTSETTLE_20260926 \
        --from "Chase Trust Checking 9967 - STR" --to "Chase Trust Checking 3038 - monthly"
    ... --confirm     # WRITES

`je/payload.je_update` is the payload shape, and it exists because QBO blanks whatever the
payload omits: `DocNumber` has to be carried explicitly and `SyncToken` must be the one just
read back or the write is rejected as stale.

The guard is a fingerprint taken before the write and compared after it -- every line's
posting type, amount, class and location, plus the entry's date and total.  Only the account
may differ, and only on the lines that matched `--from`.  A JE that balances after the change
is not evidence the change was right: swapping the wrong line still balances.
"""
from __future__ import annotations

import argparse
import json
import sys

from ..config import client
from ..je.payload import je_update
from ..resolver import Resolver


def fingerprint(je: dict, ignore_account: bool = False) -> tuple:
    out = []
    for l in je.get("Line", []):
        d = l.get("JournalEntryLineDetail") or {}
        out.append((
            round(float(l.get("Amount") or 0), 2),
            d.get("PostingType"),
            (d.get("ClassRef") or {}).get("value"),
            (d.get("DepartmentRef") or {}).get("value"),
            (d.get("Entity") or {}).get("Type"),
            l.get("Description"),
            None if ignore_account else (d.get("AccountRef") or {}).get("value"),
        ))
    return (je.get("DocNumber"), je.get("TxnDate"), round(float(je.get("TotalAmt") or 0), 2),
            tuple(out))


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.fixes.je_account")
    ap.add_argument("--docnum", required=True)
    ap.add_argument("--from", dest="from_acct", required=True)
    ap.add_argument("--to", dest="to_acct", required=True)
    ap.add_argument("--confirm", action="store_true")
    args = ap.parse_args()

    qbo = client()
    res = Resolver(qbo)
    from_id, to_id = res.account(args.from_acct), res.account(args.to_acct)
    for name, aid in ((args.from_acct, from_id), (args.to_acct, to_id)):
        if not aid:
            sys.exit(f"account not found: {name!r}")

    found = qbo.query_all(
        f"SELECT * FROM JournalEntry WHERE DocNumber = '{args.docnum}' MAXRESULTS 10",
        "JournalEntry")
    if len(found) != 1:
        sys.exit(f"{len(found)} journal entries carry DocNumber {args.docnum!r} -- refusing")
    je = found[0]

    hits = [n for n, l in enumerate(je.get("Line", []))
            if ((l.get("JournalEntryLineDetail") or {}).get("AccountRef") or {}).get("value") == from_id]
    if not hits:
        print(f"no line on {args.from_acct!r} -- nothing to change.")
        return

    print(f"JE {je['Id']} {je.get('DocNumber')} {je['TxnDate']}")
    for n in hits:
        l = je["Line"][n]
        d = l["JournalEntryLineDetail"]
        print(f"  line {n + 1}: {d.get('PostingType')} {float(l.get('Amount') or 0):,.2f}  "
              f"{args.from_acct} -> {args.to_acct}")
        print(f"     Location={(d.get('DepartmentRef') or {}).get('name')} "
              f"Class={(d.get('ClassRef') or {}).get('name', '')}")
    for n, l in enumerate(je.get("Line", [])):
        if n in hits:
            continue
        d = l["JournalEntryLineDetail"]
        print(f"  line {n + 1}: {d.get('PostingType')} {float(l.get('Amount') or 0):,.2f}  "
              f"{(d.get('AccountRef') or {}).get('name')}   (unchanged)")

    if not args.confirm:
        print("\nDRY RUN — nothing written. Re-run with --confirm.")
        return

    before = fingerprint(je, ignore_account=True)
    body = json.loads(json.dumps(je))
    for n in hits:
        body["Line"][n]["JournalEntryLineDetail"]["AccountRef"] = {"value": to_id}
    got = qbo.post("journalentry", je_update(body))

    after_read = qbo.query_all(
        f"SELECT * FROM JournalEntry WHERE Id = '{je['Id']}'", "JournalEntry")[0]
    if fingerprint(after_read, ignore_account=True) != before:
        sys.exit("ABORT — something other than the account changed; inspect JE "
                 f"{je['Id']} in QuickBooks before doing anything else")
    now = {((l.get("JournalEntryLineDetail") or {}).get("AccountRef") or {}).get("value")
           for n, l in enumerate(after_read.get("Line", [])) if n in hits}
    if now != {to_id}:
        sys.exit(f"ABORT — lines did not land on {args.to_acct!r}: {now}")
    print(f"\nupdated JE {got.get('Id')} — {len(hits)} line(s) now on {args.to_acct}, "
          f"everything else verified unchanged")


if __name__ == "__main__":
    main()
