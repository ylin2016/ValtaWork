"""One-time backfill: re-derive the subcategory of existing 'Repairs & Maintenance'
ledger rows now that mapping_accounts.yml splits Repairs and Maintenance.

    python -m src.onetime.split_repairs_maintenance [--dry-run]

The subcategory is stamped onto ledger_lines at qbo-sync time, so a mapping change
only reaches NEW rows. Re-syncing 20 months from QuickBooks just to relabel them
would be a lot of API traffic for a pure display change, so this relabels the rows in
place using the SAME `apply_account_rules` the sync uses — no hardcoded account names,
so it stays correct if the owner edits the rules again.

Money is untouched: only `subcategory` changes, so `total_expenses` and every payout
stay identical. Re-run `build` afterwards to regenerate the Excel/PDF expense sections.
Idempotent — running it twice is a no-op.
"""
import argparse
import sqlite3

from .. import paths
from ..common.mappings import apply_account_rules, load_account_rules

STALE = "Repairs & Maintenance"


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--dry-run", action="store_true", help="report the split without writing.")
    args = ap.parse_args()

    rules = load_account_rules(str(paths.MAPPING_ACCOUNTS))
    conn = sqlite3.connect(str(paths.DB_PATH))
    rows = conn.execute(
        "SELECT ledger_id, qbo_account, vendor_customer FROM ledger_lines WHERE subcategory=?",
        (STALE,)).fetchall()
    if not rows:
        print(f"No rows left with subcategory '{STALE}' — nothing to do.")
        return

    moves, unchanged = {}, 0
    for ledger_id, acct, vendor in rows:
        _, sub = apply_account_rules(acct, vendor, rules)
        if sub == STALE:
            unchanged += 1
            continue
        moves.setdefault(sub, []).append(ledger_id)

    for sub, ids in sorted(moves.items()):
        print(f"  {STALE} -> {sub}: {len(ids)} row(s)")
    if unchanged:
        print(f"  !! {unchanged} row(s) still map to '{STALE}' — check mapping_accounts.yml")

    if args.dry_run:
        print("\n--dry-run: nothing written.")
        return
    for sub, ids in moves.items():
        conn.executemany("UPDATE ledger_lines SET subcategory=? WHERE ledger_id=?",
                         [(sub, i) for i in ids])
    conn.commit()
    print(f"\nRelabelled {sum(len(v) for v in moves.values())} row(s). "
          f"Re-run `build` for each period to refresh the Excel/PDF expense sections.")


if __name__ == "__main__":
    main()
