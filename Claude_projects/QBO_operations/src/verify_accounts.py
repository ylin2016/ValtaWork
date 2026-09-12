"""Check every Id and name in config/accounts.yml against the live company file.

A wrong account Id in a write payload posts real money to the wrong account, and
nothing in QuickBooks will complain.  This is a pure read -- run it after editing
accounts.yml, and before a posting session.

    python -m src.verify_accounts       # exits 1 if anything does not resolve
"""
from __future__ import annotations

import sys

from .config import accounts, client


def main() -> int:
    qbo = client()
    a = accounts()
    ok = True

    print(f"{'key':24} {'id':6} {'live FullyQualifiedName':70} match")
    print("-" * 110)
    for key, spec in a["accounts"].items():
        got = qbo.query(f"SELECT Id, FullyQualifiedName FROM Account WHERE Id = '{spec['id']}'") \
                 .get("QueryResponse", {}).get("Account", [])
        live = got[0]["FullyQualifiedName"] if got else "*** NO SUCH ACCOUNT ***"
        good = live == spec["name"]
        ok &= good
        print(f"{key:24} {spec['id']:6} {live:70} {'ok' if good else 'MISMATCH'}")
        if not good and got:
            print(f"{'':31} accounts.yml says: {spec['name']}")

    print()
    for key, nm in a["names"].items():
        got = qbo.query(f"SELECT Id FROM Account WHERE FullyQualifiedName = '{nm}'") \
                 .get("QueryResponse", {}).get("Account", [])
        print(f"names.{key:20} {nm:60} -> "
              f"{'Id ' + got[0]['Id'] if got else '*** NOT FOUND ***'}")
        ok &= bool(got)

    got = qbo.query(f"SELECT Id FROM Department WHERE FullyQualifiedName = '{a['location']}'") \
             .get("QueryResponse", {}).get("Department", [])
    print(f"{'location':26} {a['location']:60} -> "
          f"{'Id ' + got[0]['Id'] if got else '*** NOT FOUND ***'}")
    ok &= bool(got)

    print("\nALL RESOLVE" if ok else "\n*** SOMETHING DOES NOT RESOLVE — fix accounts.yml ***")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
