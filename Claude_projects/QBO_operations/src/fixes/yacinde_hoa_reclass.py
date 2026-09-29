"""Put the flat `Yacinde HOA` class back on the HOA's cleaning lines.

    python -m src.fixes.yacinde_hoa_reclass \
        --from-invoice 118602=118386:1930.00 --from-invoice 118604=118387:850.00 \
        --from-invoice 118605=118387:2340.00 --all-lines 118594:4500.00  [--confirm]

Owner decision 2026-09-28, the second half of making Yacinde cleaning owner-borne: the HOA's
cleans move to `Cleaning Expense - Owner` by ACCOUNT (see `yacinde_cleaning_all_owner`) but keep
the flat `Yacinde HOA` CLASS, so they stay identifiable as the HOA's and stay off individual
owners' statements.  Only the cleans that were always the owner's keep their unit class.

**Know what that combination does.**  `Cleaning Expense - Owner` IS inside the owner-statement
account gate, but `Yacinde HOA` is deliberately absent from the statement project's
`mapping_classes.yml`, so these lines land in its exceptions table as CLASS_NOT_MAPPED and reach
NO statement at all.  The class is the binding constraint, not the account -- which is the same
property the 2026-09-17 note relies on, used here on purpose rather than as a side effect.

## Why the lines are identified from the INVOICES

Once `yacinde_cleaning_all_owner` has run, every cleaning line on these Purchases carries the
same account and a unit class, so nothing on the expense itself distinguishes a clean that was
the HOA's from one that was the owner's.  **The HOA invoices are the independent record**: what
was billed to the HOA is by definition what the HOA bore.  Lines are matched on
(date, unit, amount) parsed from the description, and the match must hit an EXPECTED TOTAL passed
in per Purchase -- a partial or over-wide match aborts rather than re-classing the wrong cleans.
`--all-lines` is for a Purchase that was wholly the HOA's and has no invoice yet (INV1200).

The description formats differ between documents -- `07/13/2026 C1 Cleaning Fee` on the older
ones, `2026-09-01 Yacinde B1 Cleaning Fee (AA …)` on INV1200 -- so both are parsed, and a line
whose date or unit cannot be read simply does not match, which the expected-total check then
catches.

Only `ClassRef` changes.  Every Purchase's `TotalAmt` and every line's amount, description and
ACCOUNT are fingerprinted before and after; a Purchase is FULL-updated by reading it back and
echoing it whole.
"""
from __future__ import annotations

import argparse
import copy
import re
import sys
from collections import Counter

from ..config import client
from ..resolver import Resolver, esc

HOA_CLASS = "Yacinde HOA"
# `07/13/2026 C1 Cleaning Fee` / `07/13/2026  Yacinde C1  cleaning` / `2026-09-01 Yacinde B1 …`
KEY_RE = re.compile(r"(\d{2}/\d{2}/\d{4}|\d{4}-\d{2}-\d{2})\s+(?:Yacinde\s+)?([A-Z]\d)\b")


def norm_date(s: str) -> str:
    if "/" in s:
        m, d, y = s.split("/")
        return f"{y}-{m}-{d}"
    return s


def line_key(ln: dict):
    m = KEY_RE.search(ln.get("Description") or "")
    if not m:
        return None
    return (norm_date(m.group(1)), m.group(2), round(float(ln.get("Amount") or 0), 2))


def fingerprint(p: dict) -> tuple:
    out = []
    for ln in p.get("Line", []):
        d = ln.get("AccountBasedExpenseLineDetail") or {}
        out.append((round(float(ln.get("Amount") or 0), 2), ln.get("Description"),
                    (d.get("AccountRef") or {}).get("value")))
    return (round(float(p.get("TotalAmt") or 0), 2), p.get("DocNumber"), p.get("TxnDate"),
            (p.get("EntityRef") or {}).get("value"), tuple(out))


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.fixes.yacinde_hoa_reclass")
    ap.add_argument("--from-invoice", action="append", default=[], metavar="PURCHASE=INVOICE:TOTAL",
                    help="re-class the Purchase lines that match that Invoice's cleaning lines")
    ap.add_argument("--all-lines", action="append", default=[], metavar="PURCHASE:TOTAL",
                    help="re-class EVERY cleaning line on that Purchase")
    ap.add_argument("--to", default=HOA_CLASS, help=f"target class (default {HOA_CLASS!r})")
    ap.add_argument("--to-units", action="append", default=[], metavar="PURCHASE:TOTAL",
                    help="the REVERSE: put each line back on its OWN unit class, for cleans the "
                         "HOA turns out not to bear.  Combine with --keep-units.")
    ap.add_argument("--keep-units", default="", metavar="B1,B2,...",
                    help="with --to-units: units left on the flat class because the HOA DOES "
                         "bear them")
    ap.add_argument("--confirm", action="store_true", help="actually WRITE")
    args = ap.parse_args()
    if not args.from_invoice and not args.all_lines and not args.to_units:
        sys.exit("nothing selected: pass --from-invoice, --all-lines or --to-units")

    qbo = client()
    res = Resolver(qbo)
    target = res.klass(args.to)
    if target is None:
        sys.exit(f"class not found: {args.to!r}")

    # --to-units is the reverse direction and shares everything except which class each line
    # lands on, so it runs as its own pass rather than being bolted onto the forward one: mixing
    # them in one call would make "--to" ambiguous per line.
    if args.to_units:
        keep = {u.strip().upper() for u in args.keep_units.split(",") if u.strip()}
        unit_cls: dict[str, str | None] = {}

        def class_of(unit: str) -> str | None:
            if unit not in unit_cls:
                full = res.klass_fqn(f"Yacinde {unit}")
                unit_cls[unit] = res.klass(full) if full else None
            return unit_cls[unit]

        pending = []
        for spec in args.to_units:
            try:
                pid, total = spec.split(":", 1)
                pid, expect = pid.strip(), round(float(total), 2)
            except ValueError:
                sys.exit(f"--to-units needs PURCHASE:TOTAL, got {spec!r}")
            got = qbo.query(f"SELECT * FROM Purchase WHERE Id = '{esc(pid)}'") \
                .get("QueryResponse", {}).get("Purchase", [])
            if not got:
                sys.exit(f"Purchase {pid} not found")
            pp = got[0]
            picks, moved = [], 0.0
            for i, ln in enumerate(pp.get("Line", [])):
                d = ln.get("AccountBasedExpenseLineDetail") or {}
                if not d or (d.get("ClassRef") or {}).get("value") != target:
                    continue          # only lines currently on the flat class are candidates
                m = KEY_RE.search(ln.get("Description") or "")
                if not m:
                    sys.exit(f"ABORT -- Purchase {pid} line {i}: no readable unit in "
                             f"{(ln.get('Description') or '')[:50]!r}.  Nothing was written.")
                unit = m.group(2)
                if unit in keep:
                    continue
                cid = class_of(unit)
                if not cid:
                    sys.exit(f"ABORT -- unit {unit!r} resolves to no class.  Nothing was written.")
                picks.append((i, unit, cid))
                moved = round(moved + round(float(ln.get("Amount") or 0), 2), 2)
            if abs(moved - expect) > 0.005:
                sys.exit(f"ABORT -- Purchase {pid}: {len(picks)} line(s) totalling {moved:,.2f} "
                         f"would move but {expect:,.2f} was expected.  Nothing was written.")
            pending.append((pp, picks, moved))

        for pp, picks, moved in pending:
            byu: dict[str, float] = {}
            for _, u, _ in picks:
                byu[u] = 0.0
            for i, u, _ in picks:
                byu[u] = round(byu[u] + round(float(pp["Line"][i].get("Amount") or 0), 2), 2)
            print(f"Purchase {pp['Id']}  {pp.get('DocNumber') or '-':<24} "
                  f"{len(picks)} line(s) off {args.to} -> their own unit   {moved:>9,.2f}")
            for u, v in sorted(byu.items()):
                print(f"      Yacinde {u:<4} {v:>9,.2f}")
            if keep:
                print(f"      kept on {args.to}: {', '.join(sorted(keep))}")
        if not args.confirm:
            print("\nDRY RUN -- nothing written.  Re-run with --confirm to WRITE.")
            return
        print()
        bad = []
        for pp, picks, moved in pending:
            want_fp = fingerprint(pp)
            body = copy.deepcopy(pp)
            for i, _, cid in picks:
                body["Line"][i]["AccountBasedExpenseLineDetail"]["ClassRef"] = {"value": cid}
            body["sparse"] = False
            try:
                qbo.post("purchase", body)
            except Exception as exc:                              # noqa: BLE001
                print(f"   {pp['Id']}  FAILED: {str(exc)[:200]}")
                bad.append(pp["Id"]); continue
            back = qbo.query(f"SELECT * FROM Purchase WHERE Id = '{esc(pp['Id'])}'") \
                .get("QueryResponse", {}).get("Purchase", [])[0]
            if fingerprint(back) != want_fp:
                print(f"   {pp['Id']}  *** an amount, description, account or header field moved ***")
                bad.append(pp["Id"]); continue
            left = round(sum(round(float(l.get("Amount") or 0), 2) for l in back.get("Line", [])
                             if (d := l.get("AccountBasedExpenseLineDetail") or {})
                             and (d.get("ClassRef") or {}).get("value") == target), 2)
            print(f"   {pp['Id']}  {pp.get('DocNumber') or '-':<24} {len(picks)} re-classed, "
                  f"{left:,.2f} still on {args.to}, total {float(back['TotalAmt']):,.2f} unchanged")
        if bad:
            sys.exit(f"\n{len(bad)} Purchase(s) did not land: {', '.join(bad)}")
        print(f"\ndone.  {round(sum(m for _, _, m in pending), 2):,.2f} moved to unit classes.")
        return

    jobs = []
    for spec in args.from_invoice:
        try:
            pid, rest = spec.split("=", 1)
            iid, total = rest.split(":", 1)
            jobs.append((pid.strip(), iid.strip(), round(float(total), 2)))
        except ValueError:
            sys.exit(f"--from-invoice needs PURCHASE=INVOICE:TOTAL, got {spec!r}")
    for spec in args.all_lines:
        try:
            pid, total = spec.split(":", 1)
            jobs.append((pid.strip(), None, round(float(total), 2)))
        except ValueError:
            sys.exit(f"--all-lines needs PURCHASE:TOTAL, got {spec!r}")

    inv_cache: dict[str, Counter] = {}

    def invoice_keys(iid: str) -> Counter:
        if iid not in inv_cache:
            got = qbo.query(f"SELECT * FROM Invoice WHERE Id = '{esc(iid)}'") \
                .get("QueryResponse", {}).get("Invoice", [])
            if not got:
                sys.exit(f"Invoice {iid} not found")
            c: Counter = Counter()
            for ln in got[0].get("Line", []):
                d = ln.get("SalesItemLineDetail") or {}
                if not d or "Cleaning" not in ((d.get("ItemRef") or {}).get("name") or ""):
                    continue
                k = line_key(ln)
                if k:
                    c[k] += 1
            inv_cache[iid] = c
        return inv_cache[iid]

    plan = []
    for pid, iid, expect in jobs:
        got = qbo.query(f"SELECT * FROM Purchase WHERE Id = '{esc(pid)}'") \
            .get("QueryResponse", {}).get("Purchase", [])
        if not got:
            sys.exit(f"Purchase {pid} not found")
        p = got[0]
        want = invoice_keys(iid) if iid else None
        budget = Counter(want) if want is not None else None
        idxs, moved = [], 0.0
        for i, ln in enumerate(p.get("Line", [])):
            d = ln.get("AccountBasedExpenseLineDetail") or {}
            if not d:
                continue
            if (d.get("ClassRef") or {}).get("value") == target:
                continue                      # already on the target class
            if budget is not None:
                k = line_key(ln)
                if not k or budget[k] <= 0:
                    continue
                budget[k] -= 1
            idxs.append(i)
            moved = round(moved + round(float(ln.get("Amount") or 0), 2), 2)
        if abs(moved - expect) > 0.005:
            sys.exit(f"ABORT -- Purchase {pid}: matched {len(idxs)} line(s) totalling {moved:,.2f} "
                     f"but {expect:,.2f} was expected.  Nothing was written.")
        plan.append((p, idxs, moved, iid))

    for p, idxs, moved, iid in plan:
        src = f"matching Invoice {iid}" if iid else "every cleaning line"
        print(f"Purchase {p['Id']}  {p.get('DocNumber') or '-':<24} "
              f"{len(idxs):>3} of {len([l for l in p.get('Line', []) if l.get('AccountBasedExpenseLineDetail')])} "
              f"line(s) -> class {args.to}   {moved:>9,.2f}   ({src})")
    grand = round(sum(m for _, _, m, _ in plan), 2)
    print(f"\n{len(plan)} Purchase(s), {sum(len(i) for _, i, _, _ in plan)} line(s), {grand:,.2f} "
          f"-> class {args.to}")
    print("NOTE: these lines then reach NO owner statement -- `Yacinde HOA` is absent from "
          "mapping_classes.yml, so they land in the exceptions table as CLASS_NOT_MAPPED.")
    if not args.confirm:
        print("\nDRY RUN -- nothing written.  Re-run with --confirm to WRITE.")
        return

    print()
    failed = []
    for p, idxs, moved, _ in plan:
        want_fp = fingerprint(p)
        body = copy.deepcopy(p)
        for i in idxs:
            body["Line"][i]["AccountBasedExpenseLineDetail"]["ClassRef"] = {"value": target}
        body["sparse"] = False
        try:
            qbo.post("purchase", body)
        except Exception as exc:                                    # noqa: BLE001
            print(f"   {p['Id']}  FAILED: {str(exc)[:200]}")
            failed.append(p["Id"]); continue
        back = qbo.query(f"SELECT * FROM Purchase WHERE Id = '{esc(p['Id'])}'") \
            .get("QueryResponse", {}).get("Purchase", [])[0]
        if fingerprint(back) != want_fp:
            print(f"   {p['Id']}  *** an amount, description, account or header field moved ***")
            failed.append(p["Id"]); continue
        n = sum(1 for l in back.get("Line", [])
                if (d := l.get("AccountBasedExpenseLineDetail") or {})
                and (d.get("ClassRef") or {}).get("value") == target)
        print(f"   {p['Id']}  {p.get('DocNumber') or '-':<24} {len(idxs)} re-classed, "
              f"{n} line(s) now on {args.to}, total {float(back['TotalAmt']):,.2f} unchanged")
    if failed:
        sys.exit(f"\n{len(failed)} Purchase(s) did not land: {', '.join(failed)}")
    print(f"\ndone.  {grand:,.2f} across {sum(len(i) for _, i, _, _ in plan)} line(s).")


if __name__ == "__main__":
    main()
