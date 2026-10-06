"""Read-only snapshot of the QBO Invoices + fee Bills that already exist for the
reservations in a stored Guesty pull.

    python -m src.qbo_snapshot                 # latest inputs/guesty pull

Two uses: the duplicate guard (VRP posted most reservations up to 2026-09-28; never
create a second Invoice/Bill for a code that has one) and the parity check (build a
reservation VRP already posted, diff ours against theirs line by line).
Writes inputs/qbo/snapshot_<guesty stamp>.json. Pure reads.
"""
from __future__ import annotations

import json

from . import bridge
from .codes import bill_doc, invoice_doc
from .guesty_store import latest_pull
from .paths import QBO_INPUTS
from .resolver_util import esc

BILL_PREFIXES = ("5W4Y", "RR26", "9Z5X", "8M10", "YKY6", "RZ3O", "X9M5")
CHUNK = 40


def _in(values):
    return ", ".join(f"'{esc(v)}'" for v in values)


def snapshot(resvs: list[dict]) -> dict:
    qbo = bridge.qbo_client()
    invoices, bills = [], []
    for i in range(0, len(resvs), CHUNK):
        part = resvs[i:i + CHUNK]
        inv_docs = sorted({invoice_doc(r) for r in part})
        invoices += qbo.query_all(f"SELECT * FROM Invoice WHERE DocNumber IN ({_in(inv_docs)})", "Invoice")
        docs = sorted({bill_doc(p, r) for r in part for p in BILL_PREFIXES})
        bills += qbo.query_all(f"SELECT * FROM Bill WHERE DocNumber IN ({_in(docs)})", "Bill")
        print(f"  {min(i + CHUNK, len(resvs))}/{len(resvs)} reservations: "
              f"{len(invoices)} invoices, {len(bills)} bills")
    return {"invoices": invoices, "bills": bills}


def main():
    path, pull = latest_pull()
    resvs = [r for r in pull["results"] if r.get("confirmationCode")]
    snap = snapshot(resvs)
    QBO_INPUTS.mkdir(parents=True, exist_ok=True)
    out = QBO_INPUTS / f"snapshot_{pull['pulled_at']}.json"
    out.write_text(json.dumps({"guesty_pull": path.name, **snap}, indent=1))
    print(f"-> {out}")


if __name__ == "__main__":
    main()
