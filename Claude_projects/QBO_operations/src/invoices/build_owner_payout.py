"""Build the monthly owner-payout Bills from the bank's own payout sheet.

    python -m src.invoices.build_owner_payout                          # newest sheet
    python -m src.invoices.build_owner_payout --sheet inputs/owner_payout/Payout_202608.xlsx
    python -m src.invoices.build_owner_payout --expect-total 270868.91

Writes `review/owner_payout_<period>.csv` and nothing else.  `invoices/post_owner_payout`
posts it, and only with `--confirm`.

THE SHAPE, which is the register's own and not invented here:

    Bill          Dr 2 - Owner Distributions (Payouts)   Cr PMS Clearing - A/P    per property
    BillPayment   Dr PMS Clearing - A/P                  Cr Chase Trust 9967      per property

One Bill and one Check per bank line, because the bank pays one ACH per property.  The
BillPayment is the object the feed matches; a Bill alone would leave the bank line
uncategorised and the owner picking an account 52 times.

THE SHEET is one column of the same filename-style reference the expense sheets use,
`<yyyymmdd>_<Property>_<payee>_Owner Payout_<amount>`.  Only the two ends are fixed, so
the property is read from field 2 and the amount from the last field.  An optional SECOND
column names the bank BATCH each payout left in.

**The bank does not debit one line per property.**  2026-09-15 came through as two lump
debits ($270,868.91 and $23,191.62) plus a few individual ones, so a bank line is matched
against the WHOLE GROUP of payments behind it -- QuickBooks lets one line match several
transactions, and it will only balance if the group sums exactly.  That is why
`--batch <name>=<amount>` exists and why a batch that does not add up is fatal here: the
batch total is the bank's own record of what left the account, and a payout missing from
the sheet is invisible in any total derived from the sheet itself.  $14,342.10 of the
2026-09-15 second batch had no row on the first two versions of the sheet, and only the
batch figure said so.

Three things it will not guess:

  * **The payee is the BANK's ACH label**, not a QuickBooks DisplayName -- `VALTA COJI
    LLC`, `Jing Zhou Chase`, `AFN SheltonAdvisorLLC`.  `config/payees.yml` carries the
    spelling-only differences; a name that reaches no Vendor is written
    `*** NEEDS VENDOR ***` and blocks the post.  A row naming TWO owners (`Huijing Tao
    Jing Zhou`) is never resolved to one of them here -- the owner says which.
  * **A property is not always a class.**  Five properties are paid as one ACH but have
    no class of their own; `config/payout_classes.yml` records which unit class the
    register already put each one on.  Anything else is `*** UNMAPPED ***`.
  * **The statement figure is reported, never substituted.**  The bank is the truth for
    cash: the Bill carries what left the account, and `StatementAmount`/`Diff` say how
    that compares to `amount_due_to_owner` for the period so a payout that went out at
    the wrong figure is visible BEFORE it is on the books, not after.

The DocNumber is `PAY-<yyyymmdd>-<amount>`, keyed to the PAYOUT -- not to the row's
position in the sheet.  An ordinal (`PAY-202609-08`) renumbers the moment a row is added,
which would hand an already-posted DocNumber to a different property and make the
duplicate check skip a payout that had never been booked.  The sheet grew from 52 rows to
55 between two runs, so this is not hypothetical.  A collision or an over-long DocNumber
is fatal rather than truncated, for the same reason the Zelle scheme dropped the long
channel reference: two payouts sharing one DocNumber make the second look already posted.
"""
from __future__ import annotations

import argparse
import csv
import re
import sqlite3
import sys
from collections import defaultdict
from datetime import date
from pathlib import Path

import openpyxl
import yaml

from ..bridge import ledger_db
from ..config import acct_name, client, location, name as cfg_name, payees
from ..paths import OWNER_PAYOUT_INPUTS, PAYOUT_CLASSES_YML, review_csv
from ..resolver import Resolver

PAYOUT_ACCOUNT = acct_name("owner_distributions")
BANK = cfg_name("bank_str")
LOCATION = location()
NEEDS_VENDOR = "*** NEEDS VENDOR: {} ***"
NEEDS_BANK = "*** NEEDS BANK: {} ***"
UNMAPPED = "*** UNMAPPED: {} ***"
MARKER = "Owner Payout"
DOCNUMBER_MAX = 21

FIELDS = ["PayRef", "DocNumber", "TxnDate", "Vendor", "Property", "Class", "Account",
          "Amount", "Batch", "StatementAmount", "StatementSource", "Diff", "PaidFrom",
          "Location", "Memo", "Description", "SheetRow"]


def payout_classes() -> dict[str, str]:
    if not PAYOUT_CLASSES_YML.exists():
        return {}
    return (yaml.safe_load(PAYOUT_CLASSES_YML.read_text()) or {}).get("properties", {})


def sheets() -> list[Path]:
    # `~$Payout_*.xlsx` is Excel's lock file for a workbook that is still open; it is not
    # a workbook and openpyxl cannot read it.
    return sorted(p for p in OWNER_PAYOUT_INPUTS.glob("Payout_*.xlsx")
                  if not p.name.startswith("~$"))


def parse(ref: str) -> dict:
    """`20260915_Seattle 7434_Dai Li _Owner Payout_4336.22` -> its parts.

    Only the ENDS are fixed, exactly as on the expense sheets: date and property first,
    amount last, `Owner Payout` second from last.  Everything between the property and
    that marker is the payee, in one piece -- `Redmond 14707_Yuhui Mao_Zhongyan Qiu_Owner
    Payout_9884.6` names two owners with an underscore between them, and taking field 3
    alone would silently drop the second and bill only the first.
    """
    f = [p.strip() for p in ref.split("_")]
    if len(f) < 5:
        raise ValueError(f"expected <date>_<property>_<payee>_{MARKER}_<amount>, got {ref!r}")
    if not re.fullmatch(r"\d{8}", f[0]):
        raise ValueError(f"field 1 is not a yyyymmdd date: {ref!r}")
    try:
        amount = round(float(f[-1]), 2)
    except ValueError:
        raise ValueError(f"last field is not an amount: {ref!r}") from None
    payee = "_".join(x for x in f[2:-2] if x)
    if not payee:
        raise ValueError(f"no payee between the property and {MARKER!r}: {ref!r}")
    return {
        "raw": ref,
        "txndate": f"{f[0][:4]}-{f[0][4:6]}-{f[0][6:]}",
        "prop": f[1],
        "payee": " ".join(payee.split()),          # 'Dai Li ' -> 'Dai Li'
        "marker_ok": f[-2].lower() == MARKER.lower(),
        "amount": amount,
    }


def resolve_bank(name: str, banks: dict[str, str]) -> str | None:
    """A bank as the sheet writes it -> its full account name, or None if not exactly one.

    The sheet is filled in by hand from a bank statement, so it says `Chase Trust 9967 -
    STR` or just `3038` where the company file says `Chase Trust Checking 9967 - STR`.
    Three passes, each requiring a UNIQUE hit: the exact name, then containment, then
    every word of the spelling appearing somewhere in the account name -- which is what
    bridges the missing `Checking`.

    Ambiguity is a refusal, never a pick.  `Chase Trust` matches both trust accounts, and
    guessing between them puts a payout where no feed will ever find it.
    """
    n = " ".join(name.split())
    if not n:
        return None
    for full in banks:
        if full.casefold() == n.casefold():
            return full
    hits = [full for full in banks if n.casefold() in full.casefold()]
    if len(hits) == 1:
        return hits[0]
    words = [w for w in re.split(r"[^0-9a-z]+", n.casefold()) if w]
    if words:
        hits = [full for full in banks
                if all(w in full.casefold() for w in words)]
        if len(hits) == 1:
            return hits[0]
    return None


def read_sheet(path: Path) -> list[dict]:
    """Column A is the payout reference; the OTHER columns are named by the header row.

    Which column means what is read from the header, never from its position.  The sheet
    began as one column, grew a batch column, and then grew a `Paid From` column IN THE
    SAME POSITION the batch had occupied -- so a builder counting columns silently read
    every bank name as a batch label, found no batch that summed to anything, and carried
    on.  The header is the owner's own statement of what they filled in.

    Recognised, case- and space-insensitively:

        paid from / bank / account   the bank account that payout left
        batch / bank batch          which bank debit it was part of

    An unrecognised header is a refusal, not a shrug: a column the owner took the trouble
    to fill in is not something to ignore.
    """
    BANKCOL = {"paidfrom", "bank", "account", "bankaccount"}
    BATCHCOL = {"batch", "bankbatch", "bankline"}

    wb = openpyxl.load_workbook(path, data_only=True)
    ws = wb.active
    header = next(ws.iter_rows(min_row=1, max_row=1, values_only=True), ()) or ()
    roles: dict[int, str] = {}
    for i, h in enumerate(header):
        if i == 0 or h is None or not str(h).strip():
            continue
        key = "".join(str(h).split()).casefold()
        if key in BANKCOL:
            roles[i] = "bank"
        elif key in BATCHCOL:
            roles[i] = "batch"
        else:
            sys.exit(f"{path.name}: column {i + 1} is headed {str(h).strip()!r}, which this "
                     f"builder does not know.  Expected one of: "
                     f"{', '.join(sorted(BANKCOL | BATCHCOL))} -- or rename it.")

    rows, bad = [], []
    for raw in ws.iter_rows(min_row=2, values_only=True):
        v = raw[0] if raw else None
        if v is None or not str(v).strip():
            continue
        try:
            r = parse(str(v).strip())
        except ValueError as e:
            bad.append(str(e))
            continue
        r["batch"] = r["bank"] = ""
        for i, role in roles.items():
            cell = raw[i] if len(raw) > i else None
            if cell is not None and str(cell).strip():
                r[role] = str(cell).strip()
        rows.append(r)
    if bad:
        sys.exit("Unparseable row(s) in the payout sheet:\n  " + "\n  ".join(bad))
    return rows


def statement_totals(period: str, root: str | None) -> tuple[dict[str, float], str | None]:
    """property_name -> amount_due_to_owner for the period's most recent run.

    A property that is only a roll-up (Seattle 10057 has no class of its own) still has
    a row here, which is exactly what the sheet pays, so the name is the join key and
    the class is not consulted.
    """
    db = ledger_db(root)
    if not db.exists():
        return {}, None
    con = sqlite3.connect(f"file:{db}?mode=ro", uri=True)
    start, end = f"{period}-01", f"{period}-31"
    run = con.execute(
        "SELECT run_id FROM statement_runs WHERE period_start >= ? AND period_start <= ? "
        "ORDER BY created_at DESC LIMIT 1", (start, end)).fetchone()
    if not run:
        return {}, None
    names = {pid: nm for pid, nm in con.execute("SELECT property_id, property_name FROM properties")}
    out: dict[str, float] = {}
    for pid, amt in con.execute(
            "SELECT property_id, amount_due_to_owner FROM statement_property_totals "
            "WHERE run_id = ?", (run[0],)):
        out[names.get(pid, pid)] = round(amt or 0.0, 2)
    return out, run[0]


def statement_for(prop: str, totals: dict[str, float],
                  paid: list[str]) -> tuple[float | None, str]:
    """The statement figure the bank line should equal, and where it came from.

    Three ways round, in this order:

      * an exact property name -- always wins, or `Seattle 10057` (which has a roll-up
        row of its OWN as well as unit rows) would be counted twice;
      * the sheet pays a property whose statements are per unit, so the units sum;
      * the sheet pays a UNIT (`Seattle 906 Lower`) whose statements are only kept at the
        parent, so the parent answers -- but ONLY when this is the sole row under that
        parent.  Two units both claiming the parent's whole figure would manufacture a
        difference on one of them, so the comparison is dropped and said to be dropped.
    """
    if prop in totals:
        return totals[prop], prop
    kids = {n: a for n, a in totals.items() if n.startswith(prop + " ")}
    if kids:
        return round(sum(kids.values()), 2), " + ".join(sorted(kids))
    parents = sorted((n for n in totals if prop.startswith(n + " ")), key=len, reverse=True)
    if parents:
        parent = parents[0]
        siblings = [q for q in paid if q == parent or q.startswith(parent + " ")]
        if len(siblings) > 1:
            return None, f"{parent} covers {len(siblings)} rows -- not compared"
        return totals[parent], f"{parent} (parent)"
    return None, ""


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--sheet", default=None, help="the payout .xlsx; default = newest")
    ap.add_argument("--period", default=None,
                    help="statement period YYYY-MM the payout settles; default = from the filename")
    ap.add_argument("--expect-total", type=float, default=None,
                    help="the sheet's whole total from the bank; a mismatch means a missing row")
    ap.add_argument("--only", action="append", default=[], metavar="PROPERTY",
                    help="build ONLY these properties.  Repeatable.  The clean way to carve "
                         "one bank batch out of a sheet covering several -- and --batch is "
                         "what proves the selection is that batch.")
    ap.add_argument("--extra", action="append", default=[], metavar="REF",
                    help="a payout reference not on the sheet, in the sheet's own format. "
                         "Repeatable.  For a payout the bank made that the workbook has not "
                         "caught up with; it is parsed, resolved and totalled exactly like a "
                         "sheet row, and the review CSV records that it came from here.")
    ap.add_argument("--exclude", action="append", default=[], metavar="PROPERTY",
                    help="leave this property out of the build.  Repeatable.  Use it with "
                         "--expect-total or --batch to carve ONE bank batch out of a sheet "
                         "that covers several: the total is what proves the selection.")
    ap.add_argument("--batch", action="append", default=[], metavar="NAME=AMOUNT[@BANK]",
                    help="a bank batch and what it debited, e.g. --batch B1=270868.91 . "
                         "Repeatable.  Rows say which batch they are in via the sheet's "
                         "second column.  A batch that does not add up is fatal.  Append "
                         "@<bank> to say which account that debit is on -- @3038 is enough "
                         "if it names one bank account.")
    ap.add_argument("--bank", default=None, metavar="BANK",
                    help="the account these payouts left, when the sheet does not say. "
                         "Overrides the built-in default for every row that has no bank of "
                         "its own.")
    ap.add_argument("--statements-root", default=None)
    ap.add_argument("--out", default=None)
    args = ap.parse_args()

    found = sheets()
    path = Path(args.sheet) if args.sheet else (found[-1] if found else None)
    if path is None:
        sys.exit(f"No Payout_*.xlsx in {OWNER_PAYOUT_INPUTS}")
    period = args.period
    if not period:
        m = re.search(r"(\d{4})(\d{2})", path.stem)
        if not m:
            sys.exit(f"Cannot read a period from {path.name}; pass --period YYYY-MM")
        period = f"{m.group(1)}-{m.group(2)}"

    rows = read_sheet(path)
    if args.only:
        keep = {x.strip() for x in args.only}
        missing = keep - {r["prop"] for r in rows}
        if missing:
            sys.exit(f"--only names propert(ies) not on the sheet: {', '.join(sorted(missing))}")
        rows = [r for r in rows if r["prop"] in keep]
    for ref in args.extra:
        try:
            r = parse(ref.strip())
        except ValueError as e:
            sys.exit(f"--extra {ref!r}: {e}")
        r["batch"] = ""
        r["bank"] = ""
        r["extra"] = True
        rows.append(r)
        print(f"{path.name}: + 1 row from --extra -- {r['prop']} ${r['amount']:,.2f} "
              f"{r['txndate']} (not on the sheet)")
    if args.exclude:
        drop = {x.strip() for x in args.exclude}
        missing = drop - {r["prop"] for r in rows}
        if missing:
            sys.exit(f"--exclude names propert(ies) not on the sheet: {', '.join(sorted(missing))}")
        kept = [r for r in rows if r["prop"] not in drop]
        print(f"{path.name}: excluding {len(rows) - len(kept)} row(s) -- "
              f"{', '.join(sorted(drop))}")
        rows = kept
    print(f"{path.name}: {len(rows)} payout(s), period {period}, "
          f"total ${sum(r['amount'] for r in rows):,.2f}")
    for r in rows:
        if not r["marker_ok"]:
            print(f"  WARN  row does not say {MARKER!r}: {r['raw']}")

    dates = sorted({r["txndate"] for r in rows})
    print(f"  bank dates: {', '.join(dates)}")
    seen: dict[tuple[str, str], list[str]] = defaultdict(list)
    for r in rows:
        seen[(r["prop"], r["txndate"])].append(r["raw"])
    for k, v in seen.items():
        if len(v) > 1:
            print(f"  WARN  {k[0]} appears {len(v)} times on {k[1]}")

    total = round(sum(r["amount"] for r in rows), 2)
    if args.expect_total is not None and abs(total - round(args.expect_total, 2)) > 0.005:
        sys.exit(f"Sheet totals ${total:,.2f} but the bank paid out "
                 f"${args.expect_total:,.2f} -- ${total - args.expect_total:,.2f} out.")

    # A bank batch is one debit covering many payouts, and the feed matches it against the
    # WHOLE group -- so a group that does not sum to the debit cannot be matched at all.
    # The bank's figure is the only independent record of how many payouts there were;
    # any total taken from the sheet cannot see a payout the sheet never mentioned.
    banked, batch_bank = {}, {}
    for spec in args.batch:
        if "=" not in spec:
            sys.exit(f"--batch wants NAME=AMOUNT, got {spec!r}")
        nm, _, amt = spec.partition("=")
        amt, _, bank_of_batch = amt.partition("@")
        try:
            banked[nm.strip()] = round(float(amt.replace(",", "").strip()), 2)
        except ValueError:
            sys.exit(f"--batch {spec!r}: {amt!r} is not an amount")
        if bank_of_batch.strip():
            batch_bank[nm.strip()] = bank_of_batch.strip()

    # One batch named and no row labelled: the whole build IS that batch, which is what
    # --exclude was used to carve out.  The sum check below is what makes that safe.
    if len(banked) == 1 and not any(r.get("batch") for r in rows):
        only = next(iter(banked))
        for r in rows:
            r["batch"] = only

    grouped: dict[str, list[dict]] = defaultdict(list)
    for r in rows:
        grouped[r.get("batch", "")].append(r)
    if banked or len(grouped) > 1 or "" not in grouped:
        print("\n  batch                 rows        on the sheet          the bank")
        short = []
        for nm in sorted(set(grouped) | set(banked)):
            got = round(sum(x["amount"] for x in grouped.get(nm, [])), 2)
            want = banked.get(nm)
            label = nm or "(no batch named)"
            if want is None:
                print(f"  {label:<20} {len(grouped.get(nm, [])):>5} {got:>18,.2f}   (not given)")
            else:
                d = round(got - want, 2)
                print(f"  {label:<20} {len(grouped.get(nm, [])):>5} {got:>18,.2f} {want:>17,.2f}"
                      + ("" if abs(d) < 0.005 else f"   OUT BY {d:,.2f}"))
                if abs(d) >= 0.005:
                    short.append((label, d))
        if short:
            sys.exit("\n" + "\n".join(
                f"Batch {nm} is ${d:,.2f} {'over' if d > 0 else 'short'} -- "
                f"{'a row on the sheet does not belong to it' if d > 0 else 'a payout it paid is missing from the sheet'}."
                for nm, d in short)
                + "\nThe feed matches a bank line against the whole group, so it cannot be "
                  "matched until the group sums to the debit.  Nothing was written.")
    elif args.expect_total is None:
        print("  NOTE  no --batch and no --expect-total: the sheet is then the only record "
              "of how many payouts there were, so one missing from it is invisible here.")

    qbo = client()
    res = Resolver(qbo)
    pmap = payees()
    cmap = payout_classes()

    # THE BANK IS NOT DERIVABLE FROM ANYTHING ELSE ON THE SHEET.  It decides which feed
    # can offer the payment under Find match, and a payment on the wrong one is correct
    # in every respect that shows on screen -- right Bill, right vendor, right amount,
    # Balance 0 -- while being unmatchable forever.  2026-09 used TWO accounts at once.
    # Precedence: the row's own column C, then its batch's @BANK, then --bank, then the
    # built-in default.
    banks = {a["Name"]: a["Id"]
             for a in qbo.query_all("SELECT * FROM Account WHERE AccountType = 'Bank'", "Account")
             if a.get("Active", True)}
    default_bank = BANK
    if args.bank:
        default_bank = resolve_bank(args.bank, banks)
        if default_bank is None:
            sys.exit(f"--bank {args.bank!r} names no single bank account.  Known: "
                     + ", ".join(sorted(banks)))
    for nm, spelling in list(batch_bank.items()):
        full = resolve_bank(spelling, banks)
        if full is None:
            sys.exit(f"--batch {nm}=...@{spelling}: names no single bank account.  Known: "
                     + ", ".join(sorted(banks)))
        batch_bank[nm] = full
    for r in rows:
        spelling = r.get("bank") or batch_bank.get(r.get("batch", ""), "")
        if not spelling:
            r["paid_from"] = default_bank
            continue
        full = resolve_bank(spelling, banks)
        r["paid_from"] = full if full else NEEDS_BANK.format(spelling)
    totals, run_id = statement_totals(period, args.statements_root)
    print(f"  statement run {run_id or '(none found)'}: {len(totals)} properties\n")

    out_rows, unresolved, diffs, nostmt = [], [], [], []
    for r in sorted(rows, key=lambda x: (x["txndate"], x["prop"])):
        doc = f"PAY-{r['txndate'].replace('-', '')}-{r['amount']:.2f}"
        if len(doc) > DOCNUMBER_MAX:
            sys.exit(f"DocNumber {doc!r} is {len(doc)} characters; QuickBooks allows "
                     f"{DOCNUMBER_MAX}.  {r['prop']} ${r['amount']:,.2f} needs a scheme of "
                     f"its own -- truncating would collide with another payout.")
        vendor = pmap.get(r["payee"], r["payee"])
        if res.vendor(vendor) is None:
            vendor = NEEDS_VENDOR.format(r["payee"])
            unresolved.append(f"{r['prop']}: payee {r['payee']!r}")

        if "NEEDS BANK" in (r.get("paid_from") or ""):
            unresolved.append(f"{r['prop']}: bank {r['bank']!r} matches no single account")

        klass = cmap.get(r["prop"]) or res.klass_fqn(f"Listings:{r['prop']}") \
            or res.klass_fqn(r["prop"])
        if klass is None or res.klass(klass) is None:
            klass = UNMAPPED.format(r["prop"])
            unresolved.append(f"{r['prop']}: no class")

        stmt, src = statement_for(r["prop"], totals, [x["prop"] for x in rows])
        diff = None if stmt is None else round(r["amount"] - stmt, 2)
        if stmt is None:
            nostmt.append(f"{r['prop']}{' -- ' + src if src else ''}")
        elif abs(diff) >= 0.02:
            diffs.append((r["prop"], r["amount"], stmt, diff, src))

        d = date.fromisoformat(r["txndate"])
        memo = f"{MARKER} {d.day} {d:%b %Y}"
        out_rows.append({
            "PayRef": r["raw"],
            "DocNumber": doc,
            "TxnDate": r["txndate"],
            "Vendor": vendor,
            "Property": r["prop"],
            "Class": klass,
            "Account": PAYOUT_ACCOUNT,
            "Amount": f"{r['amount']:.2f}",
            "Batch": r.get("batch", ""),
            "StatementAmount": "" if stmt is None else f"{stmt:.2f}",
            "StatementSource": src,
            "Diff": "" if diff is None else f"{diff:.2f}",
            "PaidFrom": r.get("paid_from") or BANK,
            "Location": LOCATION,
            "Memo": memo,
            "Description": memo,
            "SheetRow": "--extra (command line)" if r.get("extra") else path.name,
        })

    dupes = {d for d in (x["DocNumber"] for x in out_rows)
             if [x["DocNumber"] for x in out_rows].count(d) > 1}
    if dupes:
        sys.exit("Two payouts share a DocNumber, so the second would look already posted: "
                 + ", ".join(sorted(dupes))
                 + "\nSame date and same amount to two owners -- give one of them its own "
                   "date or resolve it by hand before posting.")

    # What the feed will be asked to match, per account.  This is the check that catches a
    # payment on the wrong bank: every group here has to equal a real debit in THAT feed.
    per_bank: dict[str, list[float]] = defaultdict(list)
    for x in out_rows:
        per_bank[x["PaidFrom"]].append(float(x["Amount"]))
    if len(per_bank) > 1 or any(k != BANK for k in per_bank):
        print("  paid from                              rows            total")
        for nm in sorted(per_bank):
            v = per_bank[nm]
            print(f"  {nm:<38} {len(v):>4} {sum(v):>16,.2f}")
        print("  ^ each of these must be a real debit in THAT account's feed.\n")

    out = Path(args.out) if args.out else review_csv(f"owner_payout_{period}")
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=FIELDS)
        w.writeheader()
        w.writerows(out_rows)

    if diffs:
        print(f"{len(diffs)} payout(s) differ from the {period} statement "
              f"(the BANK amount is what gets posted):")
        for p, bank, stmt, d, src in sorted(diffs, key=lambda x: -abs(x[3])):
            note = "" if src == p else f"   [{src}]"
            print(f"    {p:<22} bank {bank:>11,.2f}  stmt {stmt:>11,.2f}  diff {d:>10,.2f}{note}")
    if nostmt:
        print(f"\n{len(nostmt)} paid with no {period} statement row: {', '.join(sorted(nostmt))}")
    # Only meaningful for a WHOLE sheet.  When --only/--exclude carved one bank batch out,
    # every property in the other batches would be listed as unpaid, which is false and
    # would bury the ones that really are.
    paid = {r["prop"] for r in rows}
    owed = [] if (args.only or args.exclude) else [
        (n, a) for n, a in totals.items() if abs(a) >= 0.02
            and n not in paid and not any(n.startswith(p + " ") for p in paid)
            and not any(p.startswith(n + " ") for p in paid)]
    if args.only or args.exclude:
        print("\n  NOTE  this build is one batch, not the whole sheet, so the "
              "'balance but no payout' check is skipped -- run it on the full sheet.")
    if owed:
        print(f"\n{len(owed)} propert(ies) with a {period} balance and NO payout on the sheet:")
        for n, a in sorted(owed, key=lambda x: -x[1]):
            print(f"    {n:<28} {a:>11,.2f}")

    print(f"\n{len(out_rows)} payout(s), ${total:,.2f} -> {out}")
    if unresolved:
        print(f"\n{len(unresolved)} unresolved -- the post is BLOCKED until each is named:")
        for u in sorted(set(unresolved)):
            print(f"    {u}")
    print("\nReview the CSV, then:")
    print(f"    python -m src.invoices.post_owner_payout --all --csv {out}            # dry run")
    print(f"    python -m src.invoices.post_owner_payout --all --csv {out} --confirm  # WRITES")


if __name__ == "__main__":
    main()
