"""Book the Booking.com commission rows that the per-property debits left behind.

Booking.com debits most properties individually around an invoice's due date and then
SWEEPS THE REMAINDER into a single later debit -- weeks later, and across more than one
invoice.  Such a line matches no invoice total and no single property, so it reads in the
bank feed as an unfamiliar charge:

    01/23/2026  163.20   Ocean Spray Resort Cottage, November 92.75 + December 70.45
    01/23/2026  664.90   three more November rows + two December rows
    02/25/2026  447.70   all five January stragglers
    03/23/2026  232.55   both February stragglers

`build_bcom_commission` cannot express that: it builds ONE invoice at a time and assumes the
whole file is being booked.  This reads every `documentsummary_<period>.xlsx`, subtracts what
is already on `Fee - Booking.com Commission`, and fits the remainder to the debits the owner
read off the feed.

    python -m src.invoices.build_bcom_catchup \
        --debit 2026-01-23=163.20 --debit 2026-01-23=664.90 \
        --debit 2026-02-25=447.70 --debit 2026-03-23=232.55
    python -m src.invoices.post --all --csv review/bcom_catchup.csv [--confirm]

**The fit must be UNIQUE or the debit is refused.**  A subset-sum over a dozen candidate rows
will often find several combinations that hit the same total, and picking one arbitrarily
would put a real cost on the wrong owner's listing while still reconciling in total -- the
failure this project keeps meeting, where the sum is right and the attribution is not.  When
more than one combination fits, the alternatives are printed and nothing is written.

**Already-booked rows are matched by AMOUNT, which is the weakest link here.**  The invoice is
per property and carries no reference our side records, so two properties charged the same
amount in one month are indistinguishable.  The dry run prints what it treated as booked; a
wrong call there shows up as a debit that will not fit.
"""
from __future__ import annotations

import argparse
import csv
import glob
import re
import sys
from itertools import combinations
from pathlib import Path

import warnings

from openpyxl import load_workbook

warnings.filterwarnings("ignore", module="openpyxl")

from ..config import client
from ..paths import INVOICE_INPUTS, review_csv
from ..resolver import Resolver
from .build_bcom_commission import (BCOM_ACCOUNT, UNMAPPED, class_resolver, row)
from ..reconcile.bookingcom_commission import property_nicknames

INVOICE_DIR = INVOICE_INPUTS / "bookingcom_invoices"
# A compute guard only -- 2**20 combinations is still under a second, and SAFETY comes from
# refusing a non-unique fit, not from keeping the candidate set small.
MAX_SUBSET = 20


def read_summary(path: Path) -> tuple[list[dict], str]:
    ws = load_workbook(path, data_only=True).worksheets[0]
    rows = list(ws.iter_rows(values_only=True))
    hdr = [str(c).strip() if c else "" for c in rows[0]]
    need = ("Invoice", "ID", "Property Name", "Amount", "Status", "Invoice Type")
    miss = [c for c in need if c not in hdr]
    if miss:
        sys.exit(f"{path.name}: header lacks {miss}")
    ix = {c: hdr.index(c) for c in need}
    period = re.search(r"(\d{4})-(\d{2})", path.name).group(0)
    out = []
    for r in rows[1:]:
        if r[ix["Amount"]] is None:
            continue
        pid = r[ix["ID"]]
        out.append({"Invoice": str(r[ix["Invoice"]]),
                    "PropertyId": str(int(pid)) if isinstance(pid, float) else str(pid),
                    "PropertyName": str(r[ix["Property Name"]] or ""),
                    "Amount": round(float(r[ix["Amount"]]), 2),
                    "Status": str(r[ix["Status"]] or ""), "Type": str(r[ix["Invoice Type"]] or ""),
                    "_period": period})
    return out, period


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.invoices.build_bcom_catchup")
    ap.add_argument("--debit", action="append", required=True, metavar="YYYY-MM-DD=AMOUNT",
                    help="a bank-feed line to explain; repeat for each")
    ap.add_argument("--dir", default=str(INVOICE_DIR))
    ap.add_argument("--statements-root", default=None)
    ap.add_argument("--out", default=None)
    args = ap.parse_args()

    debits = []
    for d in args.debit:
        when, _, amt = d.partition("=")
        debits.append((when.strip(), round(float(amt), 2)))

    inv = []
    for f in sorted(glob.glob(str(Path(args.dir) / "documentsummary_*.xlsx"))):
        got, period = read_summary(Path(f))
        inv.extend(got)
        print(f"  {Path(f).name}: {len(got)} rows {sum(r['Amount'] for r in got):,.2f} ({period})")
    if not inv:
        sys.exit(f"no documentsummary_*.xlsx in {args.dir}")

    qbo = client()
    res = Resolver(qbo)
    booked = []
    for p in qbo.query_all("SELECT * FROM Purchase WHERE TxnDate >= '2025-11-01' "
                           "AND TxnDate <= '2026-06-30' MAXRESULTS 900", "Purchase"):
        for l in p.get("Line", []):
            det = l.get("AccountBasedExpenseLineDetail") or {}
            if (det.get("AccountRef") or {}).get("name", "").endswith(BCOM_ACCOUNT.rsplit(":", 1)[-1]):
                booked.append(round(float(l.get("Amount") or 0), 2))

    pool = list(booked)
    todo = []
    for r in sorted(inv, key=lambda x: (x["_period"], -x["Amount"])):
        if r["Amount"] in pool:
            pool.remove(r["Amount"])
        else:
            todo.append(r)
    print(f"\n{len(inv)} invoice rows; {len(inv) - len(todo)} already on the books by amount; "
          f"{len(todo)} outstanding = {sum(r['Amount'] for r in todo):,.2f}")

    if len(todo) > MAX_SUBSET:
        sys.exit(f"{len(todo)} outstanding rows would need 2**{len(todo)} combinations per "
                 f"debit -- narrow the invoice set with --dir")

    nicks = property_nicknames()
    to_class = class_resolver(res, args.statements_root)
    out_rows, unmapped, remaining = [], [], list(todo)

    for when, amount in debits:
        fits = []
        for k in range(1, len(remaining) + 1):
            for c in combinations(range(len(remaining)), k):
                if abs(sum(remaining[i]["Amount"] for i in c) - amount) < 0.005:
                    fits.append(c)
        if not fits:
            sys.exit(f"ABORT — no combination of the outstanding rows sums to {amount:,.2f} "
                     f"for {when}. Either a row is already booked that should not be, or this "
                     f"debit is not commission.")
        if len(fits) > 1:
            print(f"\nABORT — {len(fits)} combinations sum to {amount:,.2f} for {when}:")
            for c in fits[:6]:
                print("   " + " + ".join(f"{remaining[i]['PropertyName'][:22]} "
                                         f"{remaining[i]['Amount']:,.2f}" for i in c))
            sys.exit("attribution is not unique; nothing written")
        pick = [remaining[i] for i in fits[0]]
        for r in pick:
            remaining.remove(r)

        stamp = when.replace("-", "")
        ref = f"{stamp}_BCOMCU_{amount:.2f}"
        periods = sorted({r["_period"] for r in pick})
        memo = (f"Booking.com commission catch-up {'/'.join(periods)}, "
                f"{len(pick)} propert{'y' if len(pick) == 1 else 'ies'}")
        print(f"\n{ref}  {when}  {amount:,.2f}  covering {', '.join(periods)}")
        for i, r in enumerate(sorted(pick, key=lambda x: -x["Amount"]), 1):
            nk = (nicks.get(r["PropertyId"]) or [None])[0]
            klass = to_class(nk) if nk else None
            if not klass:
                unmapped.append({**r, "_nick": nk})
            r2 = {**r, "_nick": nk or "",
                  "_class": klass or UNMAPPED.format(nk or r["PropertyName"][:30])}
            out_rows.append(row(ref, ref[:21], when, i, r2, r["_period"], amount, memo))
            print(f"   {r['Amount']:>8,.2f}  {r['_period']}  {r['PropertyName'][:30]:<32} "
                  f"{klass or '*** UNMAPPED ***'}")

    if remaining:
        print(f"\nNOTE {len(remaining)} outstanding row(s) are explained by no debit given "
              f"({sum(r['Amount'] for r in remaining):,.2f}):")
        for r in remaining:
            print(f"   {r['Amount']:>8,.2f}  {r['_period']}  {r['PropertyName'][:40]}")

    if unmapped:
        print(f"\n{len(unmapped)} row(s) reached no class -- these BLOCK the post:")
        for u in unmapped:
            print(f"   {u['Amount']:>8,.2f}  {u['PropertyName'][:40]}  nickname={u.get('_nick')}")

    out = Path(args.out) if args.out else review_csv("bcom_catchup")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(out_rows[0].keys()))
        w.writeheader()
        w.writerows(out_rows)
    print(f"\n{len({r['PayRef'] for r in out_rows})} expense(s), {len(out_rows)} lines -> {out}")
    print(f"  total {sum(float(r['Amount']) for r in out_rows):,.2f}")
    print("\nNOTHING WAS WRITTEN. Review the CSV, then:\n"
          f"    python -m src.invoices.post --all --csv {out}            # dry run\n"
          f"    python -m src.invoices.post --all --csv {out} --confirm  # WRITES")


if __name__ == "__main__":
    main()
