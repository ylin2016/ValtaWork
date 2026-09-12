"""QuickBooks OAuth -- this project owns the token store.

    python -m src.auth url                  # print the consent URL, open it
    python -m src.auth exchange --code XXX  # trade the redirect code for tokens

Tokens land in config/secrets/qbo_tokens.json, which Owner_statement_whole reads
too.  Intuit rotates the refresh token on every refresh, so there is exactly one
copy on disk and both projects share it -- never duplicate this file.
"""
from __future__ import annotations

import argparse
import uuid

from .config import client
from .paths import QBO_TOKENS


def main() -> None:
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("url", help="print the OAuth consent URL")
    ex = sub.add_parser("exchange", help="exchange the redirect code for tokens")
    ex.add_argument("--code", required=True)
    args = ap.parse_args()

    qbo = client()
    if args.cmd == "url":
        print(qbo.auth_url(uuid.uuid4().hex))
        print("\nOpen it, approve, then run:")
        print("  python -m src.auth exchange --code <CODE_FROM_REDIRECT_URL>")
        return

    qbo.exchange_code_for_tokens(args.code)
    print(f"Tokens saved to {QBO_TOKENS}")


if __name__ == "__main__":
    main()
