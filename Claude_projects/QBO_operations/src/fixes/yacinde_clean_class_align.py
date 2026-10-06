"""Align an AA cleaning Purchase's line CLASSES with the allocation workbook.

    python -m src.fixes.yacinde_clean_class_align --purchase 118594            # dry run
    python -m src.fixes.yacinde_clean_class_align --purchase 118594 --confirm  # WRITES

Every Yacinde cleaning line sits on `Cleaning Expense - Owner` since the 2026-09-28 decision
(`fixes/yacinde_cleaning_all_owner`), so the CLASS is the only thing that says who bears a clean:

    the HOA's clean    the flat `Yacinde HOA` class  -- reaches no owner statement; the HOA is
                                                       invoiced for it by `invoices/hoa_cleaning`
    an owner's clean   that unit's `Listings:Yacinde <unit>` class -- the statement pipeline
                                                       charges the owner who bore it

`fixes/yacinde_cleaning_all_owner` moves every flat-classed line to its unit class and
`fixes/yacinde_hoa_reclass` moves a named total the other way; neither can follow a workbook
line by line, which is what a corrected allocation needs.  This one reads the workbook and sets
each line's class to the one its own party implies -- in both directions, per line.

**The workbook is the authority here**, deliberately unlike `invoices/build_aa_cleaning`, whose
`--split sheet` default lets the expense sheet's hand-filled `Category` column win when the
expense is first built.  Once the owner has corrected the allocation, that sheet is the older
statement of intent: INV1200 was posted with all 25 cleans as the HOA's, where the corrected
workbook gives 3 of them ($540.00, B3 on 09/03, 09/05 and 09/13) to Yacinde Holdings.

Pairing is EXACT on (date, unit, amount) -- no relaxation.  `build_aa_cleaning` relaxes the unit
and then the date when it builds an expense, because a line dropped there would leave the
expense short of the cash that left the bank; here an unpaired line means the workbook and the
books disagree about what the clean IS, which is not something a class change should paper over.
Any unpaired line on either side blocks the whole Purchase, as does a party the workbook names
but this project has no class for.

Only `ClassRef` changes.  Accounts are left exactly as they are and a line on neither the owner
cleaning account nor the receivable is printed and skipped.  `TotalAmt`, every line's amount and
description, and the header refs are fingerprinted before and after; a Purchase is FULL-updated
by reading it back and echoing it whole, never hand-built.
"""
from __future__ import annotations

import argparse
import copy
import re
import sys
from pathlib import Path

import openpyxl

from ..config import client, name as cfg_name
from ..resolver import Resolver, esc

VENDOR = "AA Professional Cleaners LLC"
HOA_CLASS = "Yacinde HOA"                 # flat: the HOA's share, off every owner statement
ALLOC_DIR = Path(__file__).resolve().parents[3] / "yacinde_expense" / "output"
ALLOC_GLOB = "yacinde_expense_allocation_*.xlsx"
ALLOC_SHEET = "Cleaning Allocation"

# `2026-09-01 Yacinde B1 Cleaning Fee (AA INV1200)`, and the older `07/13/2026 C1 Cleaning Fee`.
# re.S throughout: a description can carry an embedded newline, and `.*$` would stop at it.
ISO_RE = re.compile(r"^\s*(\d{4})-(\d{2})-(\d{2})\s+(?:Yacinde\s+)?([A-Z]\d)\b", re.S)
US_RE = re.compile(r"^\s*(\d{2})/(\d{2})/(\d{4})\s+(?:Yacinde\s+)?([A-Z]\d)\b", re.S)
DOC_INV_RE = re.compile(r"INV(\d{3,})", re.I)


def parse_desc(desc: str) -> tuple[str | None, str | None]:
    """(iso date, unit) from a cleaning line's description."""
    if m := ISO_RE.match(desc or ""):
        return f"{m.group(1)}-{m.group(2)}-{m.group(3)}", m.group(4)
    if m := US_RE.match(desc or ""):
        return f"{m.group(3)}-{m.group(1)}-{m.group(2)}", m.group(4)
    return None, None


def iso(v) -> str:
    return v.date().isoformat() if hasattr(v, "date") else str(v).strip()[:10]


def workbook_cleans(paths: list[Path], invoice: str) -> list[dict]:
    """Every clean on this AA invoice, with the party the workbook says bears it.

    `$0` rows are the "fraction week, no clean" markers, not cleans.  `Amount` is what the
    party is charged, which `cleaning_caps` can hold below what the cleaner billed; the class
    follows the party either way, so only the paired amount has to agree with the line.
    """
    out = []
    for p in paths:
        wb = openpyxl.load_workbook(p, data_only=True, read_only=True)
        if ALLOC_SHEET not in wb.sheetnames:
            wb.close()
            continue
        ws = wb[ALLOC_SHEET]
        it = ws.iter_rows(values_only=True)
        header = [str(h).strip() if h is not None else "" for h in next(it)]
        for raw in it:
            d = {h: v for h, v in zip(header, raw) if h}
            if str(d.get("invoice") or "").strip() != invoice:
                continue
            amt = round(float(d.get("Amount") or 0), 2)
            if amt <= 0:
                continue
            out.append({"source": p.name, "date": iso(d.get("date")),
                        "unit": str(d.get("unit") or "").removeprefix("Yacinde").strip(),
                        "amount": amt, "party": str(d.get("cleaning_paid_by") or "").strip()})
        wb.close()
    return out


def newest_workbook_cleans(paths: list[Path], invoice: str) -> tuple[list[dict], Path | None]:
    """This invoice's cleans from the most recently written workbook that carries it.

    Several workbooks hold the same invoice -- a month's report and the full-period file it
    was split from -- and they can genuinely disagree: the superseded Jul-Sep 15 file has
    INV1200's F1 clean on 09/08 where the corrected Jul-Sep 30 file has it on 09/11.  Merging
    them would turn one clean into two, so the newest file wins outright and the others are
    only named, never mixed in.  Pass --workbook to choose.
    """
    found = []
    for p in sorted(paths):
        cleans = workbook_cleans([p], invoice)
        if cleans:
            found.append((p, cleans))
    if not found:
        return [], None
    found.sort(key=lambda fc: fc[0].stat().st_mtime)
    book, cleans = found[-1]
    print(f"INV{invoice}: {len(cleans)} cleans from {book.name}"
          + (f" (also in {', '.join(p.name for p, _ in found[:-1])})" if len(found) > 1 else ""))
    return cleans, book


def fingerprint(p: dict) -> tuple:
    return (round(float(p.get("TotalAmt") or 0), 2), p.get("DocNumber"), p.get("TxnDate"),
            (p.get("EntityRef") or {}).get("value"), (p.get("AccountRef") or {}).get("value"),
            p.get("PaymentType"),
            tuple((round(float(l.get("Amount") or 0), 2), l.get("Description"))
                  for l in p.get("Line", [])))


def main() -> None:
    ap = argparse.ArgumentParser(prog="python -m src.fixes.yacinde_clean_class_align")
    ap.add_argument("--purchase", action="append", required=True, help="Purchase Id (repeatable)")
    ap.add_argument("--invoice", default=None,
                    help="AA invoice number; default = read it from the DocNumber")
    ap.add_argument("--workbook", action="append", default=None,
                    help="allocation workbook (repeatable); default = yacinde_expense/output")
    ap.add_argument("--confirm", action="store_true", help="actually WRITE")
    args = ap.parse_args()

    books = ([Path(w) for w in args.workbook] if args.workbook
             else sorted(ALLOC_DIR.glob(ALLOC_GLOB)))
    if not books:
        sys.exit("no allocation workbooks found -- pass --workbook")

    qbo = client()
    res = Resolver(qbo)
    owner_id = res.account(cfg_name("cleaning_expense_owner"))
    if owner_id is None:
        sys.exit("owner-cleaning account not found")
    # The HOA receivable no longer exists in the chart of accounts (it went when the
    # 2026-09-28 decision made cleaning owner-borne), so it is optional: a cleaning line
    # still sitting on it would be class-aligned too, anything else is printed and skipped.
    recv_id = res.account(cfg_name("hoa_receivable_yacinde"))
    cleaning_accts = {owner_id} | ({recv_id} if recv_id else set())
    fqn = {c["Id"]: c.get("FullyQualifiedName", "") for c in
           qbo.query_all("SELECT * FROM Class MAXRESULTS 500", "Class")}
    hoa_cls = res.klass(HOA_CLASS)
    if hoa_cls is None:
        sys.exit(f"class not found: {HOA_CLASS!r}")

    unit_cls: dict[str, str | None] = {}

    def class_for(party: str, unit: str) -> tuple[str | None, str]:
        """(class Id, its name) for the party that bears a clean in this unit."""
        if party == "HOA":
            return hoa_cls, HOA_CLASS
        if unit not in unit_cls:
            full = res.klass_fqn(f"Yacinde {unit}")
            unit_cls[unit] = res.klass(full) if full else None
        return unit_cls[unit], f"Yacinde {unit}"

    plan, blocked = [], []
    for pid in args.purchase:
        rows = qbo.query_all(f"SELECT * FROM Purchase WHERE Id = '{esc(pid)}'", "Purchase")
        if not rows:
            blocked.append((pid, "-", [f"no Purchase with Id {pid}"])); continue
        p = rows[0]
        doc = p.get("DocNumber") or "-"
        payee = (p.get("EntityRef") or {}).get("name") or ""
        bad = []
        if payee != VENDOR:
            bad.append(f"payee is {payee!r}, not {VENDOR!r}")
        inv = args.invoice or (m.group(1) if (m := DOC_INV_RE.search(doc)) else None)
        if not inv:
            bad.append(f"no invoice number in DocNumber {doc!r} -- pass --invoice")
        if bad:
            blocked.append((pid, doc, bad)); continue

        cleans, book = newest_workbook_cleans(books, inv)
        if not cleans:
            blocked.append((pid, doc, [f"invoice {inv} appears in no allocation workbook"])); continue

        lines, skipped = [], []
        for idx, ln in enumerate(p.get("Line", [])):
            det = ln.get("AccountBasedExpenseLineDetail") or {}
            if not det:
                continue
            acct = (det.get("AccountRef") or {}).get("value")
            amt = round(float(ln.get("Amount") or 0), 2)
            desc = ln.get("Description") or ""
            if acct not in cleaning_accts:
                skipped.append(f"line {idx}: {amt:,.2f} on "
                               f"{(det.get('AccountRef') or {}).get('name')} -- left alone")
                continue
            d, unit = parse_desc(desc)
            if not d or not unit:
                bad.append(f"line {idx}: no date/unit in {desc[:48]!r}")
                continue
            lines.append({"idx": idx, "date": d, "unit": unit, "amount": amt, "desc": desc,
                          "class_id": (det.get("ClassRef") or {}).get("value"),
                          "class": (det.get("ClassRef") or {}).get("name")})

        pool = list(cleans)                                 # exact (date, unit, amount), one-to-one
        moves = []
        for ln in lines:
            hit = next((c for c in pool if (c["date"], c["unit"], c["amount"])
                        == (ln["date"], ln["unit"], ln["amount"])), None)
            if hit is None:
                bad.append(f"line {ln['idx']}: {ln['date']} {ln['unit']} {ln['amount']:,.2f} "
                           f"pairs with no clean in the workbook")
                continue
            pool.remove(hit)
            cid, cname = class_for(hit["party"], ln["unit"])
            if cid is None:
                bad.append(f"line {ln['idx']}: party {hit['party']!r} resolves to no class")
                continue
            if cid != ln["class_id"]:
                moves.append({**ln, "party": hit["party"], "to_id": cid, "to": cname})
        for c in pool:
            bad.append(f"workbook clean {c['date']} {c['unit']} {c['amount']:,.2f} "
                       f"({c['party']}) pairs with no line")
        if bad:
            blocked.append((pid, doc, bad)); continue
        plan.append((p, inv, moves, skipped))

    for p, inv, moves, skipped in plan:
        print(f"Purchase {p['Id']}  {p['TxnDate']}  {p.get('DocNumber') or '-':<24} "
              f"{float(p.get('TotalAmt') or 0):>9,.2f}  INV{inv}")
        for s in skipped:
            print(f"    {s}")
        for m in moves:
            print(f"    line {m['idx']:>2}  {m['amount']:>8,.2f}  {m['class']:<14} -> "
                  f"{m['to']:<14} {m['party']:<17} {m['desc'][:34]}")
        if not moves:
            print("    every line already carries the class the workbook implies")
    for pid, doc, bad in blocked:
        print(f"BLOCKED Purchase {pid} {doc}:")
        for b in bad:
            print(f"    {b}")

    total = round(sum(m["amount"] for _, _, mv, _ in plan for m in mv), 2)
    print(f"\n{len(plan)} Purchase(s) read, "
          f"{sum(len(mv) for _, _, mv, _ in plan)} line(s) to re-class, {total:,.2f}. "
          f"{len(blocked)} blocked.")
    if blocked:
        sys.exit("nothing written -- resolve the blocked Purchase(s) first")
    if not args.confirm:
        print("\nDRY RUN -- nothing written.  Re-run with --confirm to WRITE.")
        return

    print()
    failed = []
    for p, inv, moves, _ in plan:
        if not moves:
            continue
        want = fingerprint(p)
        body = copy.deepcopy(p)
        for m in moves:
            body["Line"][m["idx"]]["AccountBasedExpenseLineDetail"]["ClassRef"] = {"value": m["to_id"]}
        body["sparse"] = False
        try:
            qbo.post("purchase", body)
        except Exception as exc:                                     # noqa: BLE001
            print(f"   {p['Id']}  FAILED: {str(exc)[:200]}")
            failed.append(p["Id"]); continue
        back = qbo.query_all(f"SELECT * FROM Purchase WHERE Id = '{esc(p['Id'])}'", "Purchase")[0]
        if fingerprint(back) != want:
            print(f"   {p['Id']}  *** an amount, description or header field moved ***")
            failed.append(p["Id"]); continue
        got = {l.get("Description"): ((l.get("AccountBasedExpenseLineDetail") or {})
                                      .get("ClassRef") or {}).get("value")
               for l in back.get("Line", [])}
        off = [m for m in moves if got.get(m["desc"]) != m["to_id"]]
        print(f"   {p['Id']}  {p.get('DocNumber') or '-':<24} {len(moves)} line(s) re-classed, "
              f"total {float(back.get('TotalAmt') or 0):,.2f} unchanged"
              + (f"   *** {len(off)} did not take ***" if off else ""))
        if off:
            failed.append(p["Id"])
    if failed:
        sys.exit(f"\n{len(failed)} Purchase(s) did not land cleanly: {', '.join(failed)}")
    print(f"\ndone.  {total:,.2f} re-classed to match the allocation workbook."
          f"\nTHE INVOICE HALF DOES NOT FOLLOW AUTOMATICALLY -- a clean that moved off the flat "
          f"{HOA_CLASS} class must come off that month's HOA invoice too "
          f"(`invoices/hoa_cleaning_update`), or it is recovered twice.")


if __name__ == "__main__":
    main()
