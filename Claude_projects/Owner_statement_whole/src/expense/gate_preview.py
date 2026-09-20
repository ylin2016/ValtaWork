"""What the account gate would remove from owner statements -- read-only, no writes.

The gate (see the `gate:` block in config/mapping_accounts.yml) is the fix for a design
inversion: until 2026-09-17 the CLASS alone decided whether a line reached an owner's
statement, and the ACCOUNT only picked a label.  Turning it on changes real statements, so
this prints the delta first, by account and by property, from rows already in the ledger.

    python -m src.expense.gate_preview --start 2026-07-01 --end 2026-08-31
    python -m src.expense.gate_preview --by-property        # add the per-owner breakdown

Invoice rows store the ITEM name in `qbo_account`, not an account, so the item's income
account is resolved from QuickBooks (a read) before the gate is applied to them.  Deposit
rows carry no account at all and are classified from their description; the gate cannot
speak to them and they are reported separately rather than silently passed.
"""
from __future__ import annotations

import argparse
import sqlite3
from pathlib import Path
import sys
from collections import defaultdict

from ..common.mappings import account_is_owner_side, load_account_gate
from ..paths import DB_PATH, MAPPING_ACCOUNTS


def item_accounts() -> dict[str, str]:
    """QBO Item FullyQualifiedName -> its income account's FullyQualifiedName.

    Through THIS project's own read-only client: it raises ReadOnlyError on any method
    but GET, and the dependency between the two projects runs one way (QBO_operations
    reads here, never the reverse), so importing its client would invert that -- and
    both projects name their package `src`, so the import would not resolve anyway.
    """
    import yaml                                        # noqa: PLC0415
    from dotenv import load_dotenv                     # noqa: PLC0415

    from ..paths import CONFIG_YML, ENV_FILE           # noqa: PLC0415
    from .qbo_client import QBOClient                  # noqa: PLC0415

    load_dotenv(ENV_FILE)

    q = (yaml.safe_load(Path(CONFIG_YML).read_text(encoding="utf-8")) or {})["qbo"]
    qbo = QBOClient(realm_id=q["realm_id"], base_url=q["base_url"],
                    minorversion=int(q.get("minorversion", 75)))

    def all_of(entity: str, select: str) -> list[dict]:
        out, pos = [], 1
        while True:
            page = (qbo.query(select, start_position=pos, max_results=500)
                       .get("QueryResponse", {}).get(entity, []) or [])
            out += page
            if len(page) < 500:
                return out
            pos += 500

    accts = {a["Id"]: a["FullyQualifiedName"]
             for a in all_of("Account", "select Id, FullyQualifiedName from Account")}
    out = {}
    for i in all_of("Item", "select * from Item"):
        ref = (i.get("IncomeAccountRef") or {}).get("value")
        if ref and ref in accts:
            out[i["FullyQualifiedName"]] = accts[ref]
    return out


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--start", default="2026-07-01")
    ap.add_argument("--end", default="2026-08-31")
    ap.add_argument("--by-property", action="store_true")
    ap.add_argument("--db", default=str(DB_PATH))
    args = ap.parse_args()

    gate = load_account_gate(str(MAPPING_ACCOUNTS))
    if not gate["enabled"]:
        sys.exit("gate is disabled in mapping_accounts.yml — nothing to preview")
    print("include_prefixes:")
    for p in gate["include_prefixes"]:
        print(f"  {p}")
    print(f"include_exact: {sorted(gate['include_exact']) or '(none)'}\n")

    items = item_accounts()
    print(f"resolved {len(items)} item -> income account mappings\n")

    conn = sqlite3.connect(args.db)
    conn.row_factory = sqlite3.Row
    rows = conn.execute(
        """SELECT source_object, qbo_account, property_id, category, subcategory,
                  COUNT(*) n, ROUND(SUM(amount), 2) amt
             FROM ledger_lines
            WHERE source='qbo' AND include_in_statement=1
              AND posting_date >= ? AND posting_date <= ?
            GROUP BY 1, 2, 3, 4, 5""", (args.start, args.end)).fetchall()

    keep_a, drop_a = defaultdict(lambda: [0, 0.0]), defaultdict(lambda: [0, 0.0])
    keep_p, drop_p = defaultdict(float), defaultdict(float)
    no_account = [0, 0.0]
    for r in rows:
        if r["source_object"] == "Deposit" or not r["qbo_account"]:
            no_account[0] += r["n"]
            no_account[1] += r["amt"]
            continue
        acct = r["qbo_account"]
        if r["source_object"] == "Invoice":
            acct = items.get(acct, acct)          # item name -> its income account
        bucket_a, bucket_p = ((keep_a, keep_p) if account_is_owner_side(acct, gate)
                              else (drop_a, drop_p))
        key = (r["source_object"], acct)
        bucket_a[key][0] += r["n"]
        bucket_a[key][1] += r["amt"]
        bucket_p[r["property_id"]] += r["amt"]

    def total(d):
        return sum(v[0] for v in d.values()), sum(v[1] for v in d.values())

    kn, ka = total(keep_a)
    dn, da = total(drop_a)
    print(f"period {args.start} .. {args.end}\n")
    print(f"  KEPT     {kn:>6} lines  {ka:>15,.2f}")
    print(f"  DROPPED  {dn:>6} lines  {da:>15,.2f}")
    print(f"  no account (Deposit etc, gate cannot speak) "
          f"{no_account[0]:>6} lines  {no_account[1]:>15,.2f}\n")

    print("DROPPED, by account:")
    for (obj, acct), (n, amt) in sorted(drop_a.items(), key=lambda kv: kv[1][1]):
        print(f"  {obj:<13} {n:>5} {amt:>14,.2f}  {acct}")

    print("\nKEPT, by account:")
    for (obj, acct), (n, amt) in sorted(keep_a.items(), key=lambda kv: kv[1][1]):
        print(f"  {obj:<13} {n:>5} {amt:>14,.2f}  {acct}")

    if args.by_property:
        print("\nper-property effect (negative = the owner was being charged this):")
        print(f"  {'property_id':<26} {'kept':>14} {'dropped':>14}")
        for pid in sorted(set(keep_p) | set(drop_p),
                          key=lambda p: drop_p.get(p, 0.0)):
            d = drop_p.get(pid, 0.0)
            if not d:
                continue
            print(f"  {pid:<26} {keep_p.get(pid, 0.0):>14,.2f} {d:>14,.2f}")


if __name__ == "__main__":
    main()
