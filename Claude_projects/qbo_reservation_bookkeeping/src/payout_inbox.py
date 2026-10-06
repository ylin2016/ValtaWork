"""Turn payout exports dropped in inputs/inbox/ into verified, de-duplicated payout files.

Routine (weekly is enough):
  1. Airbnb, per host account: Earnings -> View all paid -> Export CSV (no filters needed --
     the export is always full history). Save it into inputs/inbox/ with the account alias
     or id in the name (`vacation.csv`, `vacation2.csv`, `lucia.csv` ...).
  2. Booking.com: Finance -> Payouts -> download each new `Payout_Statement__<date>__...csv`
     into inputs/inbox/.
  3. python -m src.payout_inbox            (or --dry-run to only report)

For every payout dated on/after `start_date` (config/payout_accounts.yml) that is not yet in
inputs/payout_ledger.csv, it checks the payout equals the sum of its rows, writes the new
payouts to inputs/airbnb/ or inputs/bookingcom/, and records them in the ledger. Overlapping
or repeated exports are harmless. Processed inbox files move to inputs/<channel>/raw/.
Nothing here talks to QBO.

    python -m src.payout_inbox --rebuild-ledger   # re-register what is already in inputs/
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import re
import shutil
import sys
from decimal import Decimal
from pathlib import Path

import yaml

from .paths import CONFIG_DIR, INPUTS_DIR

INBOX = INPUTS_DIR / "inbox"
AIRBNB_DIR = INPUTS_DIR / "airbnb"
BOOKING_DIR = INPUTS_DIR / "bookingcom"
LEDGER = INPUTS_DIR / "payout_ledger.csv"
LEDGER_COLS = ["channel", "account", "payout_id", "payout_date", "amount", "rows",
               "file", "registered_at"]


def _cfg() -> dict:
    return yaml.safe_load((CONFIG_DIR / "payout_accounts.yml").read_text())


def _money(s: str) -> Decimal:
    s = (s or "").replace(",", "").replace("$", "").strip()
    return Decimal(s) if s else Decimal(0)


def _read(path: Path) -> tuple[list[str], list[dict]]:
    with open(path, encoding="utf-8-sig", newline="") as f:
        r = csv.DictReader(f)
        return list(r.fieldnames or []), list(r)


def _kind(header: list[str]) -> str | None:
    if {"Paid out", "Amount", "Type"} <= set(header):
        return "airbnb"
    if {"Payout date", "Reservation number", "Total amount (gross)"} <= set(header):
        return "bookingcom"
    return None


# ---------------------------------------------------------------- ledger

def load_ledger() -> list[dict]:
    if not LEDGER.exists():
        return []
    return _read(LEDGER)[1]


def _append_ledger(entries: list[dict]) -> None:
    new = not LEDGER.exists()
    with open(LEDGER, "a", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=LEDGER_COLS)
        if new:
            w.writeheader()
        w.writerows(entries)


def _airbnb_key(account: str, p: dict) -> str:
    # Reference code is the payout id (M-...). An export without that column falls back to
    # date + amount, which is what the bank sees anyway.
    return p["ref"] or f"{p['date']}|{p['amount']}"


# ---------------------------------------------------------------- Airbnb

def airbnb_payouts(rows: list[dict]) -> tuple[list[dict], int]:
    """Group detail rows under the Payout row that precedes them (export order)."""
    out, cur, orphans = [], None, 0
    for r in rows:
        if r["Type"] == "Payout":
            cur = {"date": dt.datetime.strptime(r["Date"], "%m/%d/%Y").date(),
                   "ref": (r.get("Reference code") or "").strip(),
                   "amount": _money(r["Paid out"]), "rows": [r]}
            out.append(cur)
        elif cur is None:
            orphans += 1          # not yet paid out -- belongs to a future payout
        else:
            cur["rows"].append(r)
    for p in out:
        p["sum"] = sum((_money(r["Amount"]) for r in p["rows"][1:]), Decimal(0))
    return out, orphans


def airbnb_account(path: Path, payouts: list[dict], ledger: list[dict], cfg: dict) -> str | None:
    accounts = {k.lower(): str(v) for k, v in cfg["airbnb_accounts"].items()}
    name = path.stem.lower()
    for acct_id in accounts.values():
        if acct_id in name:
            return acct_id
    # longest alias first so `vacation2` is not taken for `vacation`
    for alias in sorted(accounts, key=len, reverse=True):
        if re.search(rf"(?<![a-z0-9]){re.escape(alias)}(?![a-z0-9])", name):
            return accounts[alias]
    # infer from listings seen in earlier files of each account
    seen: dict[str, set[str]] = {}
    for p in sorted(AIRBNB_DIR.glob("airbnb_*_*.csv")):
        m = re.match(r"airbnb_(\d+)_", p.name)
        if m:
            seen.setdefault(m.group(1), set()).update(
                r["Listing"] for r in _read(p)[1] if r.get("Listing"))
    # Listings have moved between accounts over the years, so only recent payouts count, and
    # the account with clearly the most overlap wins.
    recent = [p for p in payouts if p["date"] >= cfg["start_date"]] or payouts
    listings = {r["Listing"] for p in recent for r in p["rows"] if r.get("Listing")}
    score = sorted(((len(listings & ls), a) for a, ls in seen.items()), reverse=True)
    if score and score[0][0] and (len(score) == 1 or score[0][0] > 2 * score[1][0]):
        return score[0][1]
    return None


def process_airbnb(path, header, rows, ledger, cfg, start, dry) -> tuple[list[dict], list[str]]:
    msgs: list[str] = []
    payouts, orphans = airbnb_payouts(rows)
    account = airbnb_account(path, payouts, ledger, cfg)
    if not account:
        return [], [f"  ! cannot tell which Airbnb account {path.name} is -- rename it with "
                    f"the alias or id ({', '.join(cfg['airbnb_accounts'])}) and re-run"]
    done = {r["payout_id"] for r in ledger if r["channel"] == "airbnb" and r["account"] == account}
    done |= {f"{r['payout_date']}|{r['amount']}" for r in ledger
             if r["channel"] == "airbnb" and r["account"] == account}
    fresh, bad = [], []
    for p in payouts:
        if p["date"] < start or _airbnb_key(account, p) in done:
            continue
        (fresh if p["sum"] == p["amount"] else bad).append(p)
    msgs.append(f"  Airbnb {account}: {len(payouts)} payouts in file, {len(fresh)} new"
                + (f", {orphans} rows not yet paid out (ignored)" if orphans else ""))
    for p in bad:
        msgs.append(f"  ! {p['date']} {p['ref'] or '(no ref)'} paid {p['amount']} but rows sum "
                    f"{p['sum']} -- NOT written, check the export")
    if not fresh:
        return [], msgs
    fresh.sort(key=lambda p: p["date"])
    lo, hi = fresh[0]["date"], fresh[-1]["date"]
    out = AIRBNB_DIR / f"airbnb_{account}_{lo}_{hi}.csv"
    n = 2
    while out.exists():
        out = AIRBNB_DIR / f"airbnb_{account}_{lo}_{hi}_{n}.csv"
        n += 1
    entries = []
    for p in fresh:
        msgs.append(f"    {p['date']}  {p['ref'] or '(no ref code!)':<17} {p['amount']:>11,.2f}"
                    f"  {len(p['rows']) - 1} rows")
        entries.append({"channel": "airbnb", "account": account,
                        "payout_id": _airbnb_key(account, p), "payout_date": p["date"],
                        "amount": p["amount"], "rows": len(p["rows"]) - 1, "file": out.name})
    msgs.append(f"    total {sum(p['amount'] for p in fresh):,.2f} -> {out.name}")
    if not dry:
        with open(out, "w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=header)
            w.writeheader()
            for p in sorted(fresh, key=lambda p: p["date"], reverse=True):  # export order
                w.writerows(p["rows"])
    return entries, msgs


# ---------------------------------------------------------------- Booking.com

def process_booking(path, header, rows, ledger, cfg, start, dry) -> tuple[list[dict], list[str]]:
    msgs: list[str] = []
    by_date: dict[str, list[dict]] = {}
    for r in rows:
        by_date.setdefault(r["Payout date"], []).append(r)
    done = {r["payout_id"]: r for r in ledger if r["channel"] == "bookingcom"}
    entries = []
    for d, rs in sorted(by_date.items()):
        total = sum((_money(r["Total amount (gross)"]) for r in rs), Decimal(0))
        if dt.date.fromisoformat(d) < start:
            continue
        if d in done:
            if Decimal(done[d]["amount"]) != total:
                msgs.append(f"  ! Booking.com {d}: this file totals {total} but the ledger has "
                            f"{done[d]['amount']} -- not replaced, check which is right")
            else:
                msgs.append(f"  Booking.com {d}: already have it ({total:,.2f})")
            continue
        out = BOOKING_DIR / (path.name if len(by_date) == 1 else
                             f"Payout_Statement__{d}__Valta_Realty.csv")
        if out.exists():
            out = out.with_stem(out.stem + "_2")
        msgs.append(f"  Booking.com {d}: NEW  {len(rs)} rows  {total:,.2f} -> {out.name}")
        entries.append({"channel": "bookingcom", "account": "bookingcom", "payout_id": d,
                        "payout_date": d, "amount": total, "rows": len(rs), "file": out.name})
        if not dry:
            with open(out, "w", newline="", encoding="utf-8") as f:
                w = csv.DictWriter(f, fieldnames=header)
                w.writeheader()
                w.writerows(rs)
    return entries, msgs


# ---------------------------------------------------------------- main

def rebuild_ledger() -> None:
    """Register the payout files already in inputs/ (no inbox, nothing written there)."""
    entries = []
    for p in sorted(AIRBNB_DIR.glob("airbnb_*_*.csv")):
        m = re.match(r"airbnb_(\d+)_", p.name)
        if not m:
            continue
        payouts, _ = airbnb_payouts(_read(p)[1])
        for po in payouts:
            entries.append({"channel": "airbnb", "account": m.group(1),
                            "payout_id": _airbnb_key(m.group(1), po), "payout_date": po["date"],
                            "amount": po["amount"], "rows": len(po["rows"]) - 1, "file": p.name})
    for p in sorted(BOOKING_DIR.glob("*.csv")):
        rows = _read(p)[1]
        for d in sorted({r["Payout date"] for r in rows}):
            rs = [r for r in rows if r["Payout date"] == d]
            entries.append({"channel": "bookingcom", "account": "bookingcom", "payout_id": d,
                            "payout_date": d, "rows": len(rs), "file": p.name,
                            "amount": sum((_money(r["Total amount (gross)"]) for r in rs),
                                          Decimal(0))})
    now = dt.datetime.now().isoformat(timespec="seconds")
    for e in entries:
        e["registered_at"] = now
    LEDGER.unlink(missing_ok=True)
    _append_ledger(entries)
    print(f"ledger rebuilt: {len(entries)} payouts -> {LEDGER.relative_to(INPUTS_DIR.parent)}")


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--dry-run", action="store_true", help="report only; write and move nothing")
    ap.add_argument("--rebuild-ledger", action="store_true",
                    help="re-register the payout files already in inputs/airbnb + bookingcom")
    a = ap.parse_args(argv)
    if a.rebuild_ledger:
        rebuild_ledger()
        return 0

    cfg = _cfg()
    start = cfg["start_date"]
    INBOX.mkdir(exist_ok=True)
    files = sorted(p for p in INBOX.iterdir() if p.suffix.lower() == ".csv")
    if not files:
        print(f"inbox empty: drop the exports into {INBOX}")
        return 0
    ledger = load_ledger()
    problems = 0
    stamp = dt.datetime.now().strftime("%Y%m%dT%H%M%S")
    for path in files:
        header, rows = _read(path)
        kind = _kind(header)
        print(f"{path.name}")
        if kind is None:
            print("  ! not an Airbnb or Booking.com payout export -- left in the inbox")
            problems += 1
            continue
        fn = process_airbnb if kind == "airbnb" else process_booking
        entries, msgs = fn(path, header, rows, ledger, cfg, start, a.dry_run)
        print("\n".join(msgs))
        bad = any(m.lstrip().startswith("!") for m in msgs)
        problems += bad
        if a.dry_run:
            continue
        now = dt.datetime.now().isoformat(timespec="seconds")
        for e in entries:
            e["registered_at"] = now
        _append_ledger(entries)
        ledger += entries
        if not bad:   # a file with problems stays in the inbox to be looked at
            raw = (AIRBNB_DIR if kind == "airbnb" else BOOKING_DIR) / "raw"
            raw.mkdir(exist_ok=True)
            shutil.move(path, raw / f"{stamp}_{path.name}")
    if a.dry_run:
        print("\n(dry run -- nothing written)")
    if problems:
        print(f"\n{problems} file(s) need attention (marked !)")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
