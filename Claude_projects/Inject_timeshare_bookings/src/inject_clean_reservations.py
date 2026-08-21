"""Inject the iCal-sourced Yacinde bookings as Guesty owner reservations.

This is the second data source for the project. Where `inject_reservations.py`
loads the *timeshare owner weeks* (`Yacinde_timeshares.csv`), this loads
`Yacinde_Reservations_Clean.xlsx` — the 118 actual guest bookings (Airbnb / VRBO
/ Website / Owner-Guest) that were previously pushed into Guesty as **iCal
blocks**. The goal is to replace those blocks with real owner reservations that
carry the guest's name, email and phone.

Per the user's instruction, **every booking becomes an owner reservation
regardless of its channel/source** (`source: "owner"`).

Prerequisite: the old iCal feeds must be disconnected first (see
`remove_ical_feeds.py`), otherwise each date is still blocked by its own iCal
block and the create returns 400 "no availability". Availability conflicts are
skipped-and-logged (not fatal) by the shared create loop's caller here.

Filters:
  * check-in on/after the cutoff (default 2026-07-30);
  * rows already present in the shared done-log are skipped, which automatically
    excludes the ~30 owner-weeks already created from the timeshare CSV.

Missing contact data (Airbnb hides guest email; a couple of rows lack one):
per the user's choice we fill a **placeholder** so all rows can be created —
`email` -> res<Res#>@guests.valtarealty.com, `phone` -> a placeholder E.164.

Usage:
    python -m src.inject_clean_reservations --dry-run
    python -m src.inject_clean_reservations --limit 1
    python -m src.inject_clean_reservations
"""
from __future__ import annotations

import argparse
import datetime as dt

import openpyxl

# Reuse the shared client, helpers and the create loop from the sibling module.
from .inject_reservations import (
    PROJECT_ROOT,
    GuestyClient,
    build_owner_payload,
    load_done_keys,
    load_yacinde_items,
    normalize_phones,
    run_create_loop,
)

CLEAN_XLSX = PROJECT_ROOT / "data" / "Yacinde_Reservations_Clean.xlsx"
DEFAULT_CUTOFF = dt.date(2026, 7, 30)

# Placeholders for rows the source can't supply (the API requires both fields).
PLACEHOLDER_PHONE = "+10000000000"


def placeholder_email(res_num: str) -> str:
    return f"res{res_num}@guests.valtarealty.com"


def read_clean_rows() -> list[dict]:
    wb = openpyxl.load_workbook(CLEAN_XLSX, read_only=True, data_only=True)
    ws = wb["Reservations"]
    rows = list(ws.iter_rows(values_only=True))
    header = rows[0]
    return [dict(zip(header, r)) for r in rows[1:] if r and r[1]]  # r[1] = Unit


def build_plans(cutoff: dt.date) -> list[dict]:
    # Read listings.json once; derive both the id-map and the accommodates-map
    # (this file has no guest-count column, so accommodates is used as guestsCount).
    items = load_yacinde_items()
    listing_map = {it["nickname"]: it["_id"] for it in items}
    accommodates = {it["nickname"]: int(it.get("accommodates") or 2) for it in items}
    plans: list[dict] = []
    problems: list[str] = []
    for r in read_clean_rows():
        unit = str(r["Unit"]).strip()
        checkin = dt.date.fromisoformat(str(r["Check In"]).strip())
        checkout = dt.date.fromisoformat(str(r["Check Out"]).strip())
        if checkin < cutoff:
            continue
        listing_id = listing_map.get(unit)
        if not listing_id:
            problems.append(f"no listingId for {unit!r} (Res #{r['Res #']})")
            continue

        res_num = str(r["Res #"]).strip()
        first = (r["Guest First"] or "").strip()
        last = (r["Guest Last"] or "").strip()
        email = (r["Email"] or "").strip() or placeholder_email(res_num)
        phones = normalize_phones(str(r["Phone"] or "")) or [PLACEHOLDER_PHONE]

        guest = {"firstName": first, "lastName": last, "email": email, "phones": phones}
        guests_count = accommodates.get(unit, 2)
        payload = build_owner_payload(
            listing_id=listing_id,
            checkin=checkin,
            checkout=checkout,
            guest=guest,
            guests_count=guests_count,
            notes_other=f"Migrated from iCal. Orig source: {r['Source']}. "
            f"Res #{res_num}. Guest: {first} {last}.",
        )
        plans.append(
            {
                "key": f"{unit}|{checkin.isoformat()}",
                "listing": unit,
                "label": f"{unit}  {checkin.isoformat()}→{checkout.isoformat()}  "
                f"{first} {last} [{r['Source']}]",
                "payload": payload,
            }
        )

    if problems:
        print("WARNING — rows skipped for missing listing mapping:")
        for p in problems:
            print("  -", p)
    return plans


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--limit", type=int, default=None)
    ap.add_argument("--cutoff", type=lambda s: dt.date.fromisoformat(s), default=DEFAULT_CUTOFF)
    args = ap.parse_args()

    plans = build_plans(args.cutoff)
    done = load_done_keys()
    pending = [p for p in plans if p["key"] not in done]
    print(
        f"\nEligible (check-in ≥ {args.cutoff}): {len(plans)} | "
        f"already created (shared log): {len(plans) - len(pending)} | to do now: {len(pending)}"
    )
    if args.limit is not None:
        pending = pending[: args.limit]
        print(f"--limit {args.limit} -> processing {len(pending)} this run")

    if args.dry_run:
        print("\n=== DRY RUN (no writes) ===")
        for p in pending:
            g = p["payload"]["guest"]
            ph = "  ⚠ placeholder-email" if g["email"].endswith("@guests.valtarealty.com") else ""
            print(f"\n• {p['label']}{ph}")
            print(f"    guest : {g['firstName']} {g['lastName']} | {g['email']} | {g['phones']}"
                  f" | guests {p['payload']['guestsCount']}")
        print(f"\nWould create {len(pending)} owner reservation(s).")
        return 0

    client = GuestyClient()
    run_create_loop(client, pending, extra_rec={"source": "clean_xlsx"})
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
