"""Inject Yacinde timeshare bookings into Guesty as confirmed OWNER reservations.

Unlike an iCal feed (which only creates calendar *blocks* with no contact
details), this uses the Guesty Open API endpoint

    POST /reservations-v3/owner/confirmed

which creates a real owner reservation and attaches a guest record carrying the
occupant's name, email and phone. These Yacinde weeks are owner-use ("In Rental
Program = No"), so `source: "owner"` is the correct reservation type — it blocks
the calendar with no revenue/commission/channel.

Auth + rate-limit handling is reused from the sibling GuestyAccess project so we
share its .env credentials and its cached 24h token (Guesty allows only
5 tokens / 24h / client — do NOT mint fresh tokens per run).

Usage (run from the project root, venv active):

    python -m src.inject_reservations --dry-run              # preview all, no writes
    python -m src.inject_reservations --dry-run --limit 3    # preview first 3
    python -m src.inject_reservations --limit 1              # create ONE for real, then stop
    python -m src.inject_reservations                        # create all remaining

Safety:
  * --dry-run performs zero writes and prints each payload.
  * Every successful create is appended to logs/created_reservations.jsonl keyed
    by (listing, checkInDate). Re-runs skip anything already in that log, so the
    script is idempotent and safe to resume after an interruption.
  * Bookings with a check-in date before --cutoff (default 2026-07-15) are skipped.
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import importlib
import importlib.util
import json
import re
import sys
import time
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
CLAUDE_PROJECTS = PROJECT_ROOT.parent
GUESTY_ACCESS = CLAUDE_PROJECTS / "GuestyAccess"


def _load_guesty_client():
    """Import GuestyAccess's Open API client without a package-name clash.

    Both this project and GuestyAccess have a top-level ``src`` package, so a
    plain ``from src.guesty_client import ...`` would resolve to the wrong one.
    We register GuestyAccess's ``src`` under a unique module name so its own
    relative imports (``from .config import ...``) still resolve, then pull the
    client class out. This also reuses its ``.env`` credentials and its cached
    24h token (Guesty allows only 5 tokens / 24h / client).
    """
    ga_src = GUESTY_ACCESS / "src"
    pkg_name = "guestyaccess_src"
    if pkg_name not in sys.modules:
        spec = importlib.util.spec_from_file_location(
            pkg_name,
            ga_src / "__init__.py",
            submodule_search_locations=[str(ga_src)],
        )
        module = importlib.util.module_from_spec(spec)
        sys.modules[pkg_name] = module
        spec.loader.exec_module(module)
    client_mod = importlib.import_module(f"{pkg_name}.guesty_client")
    return client_mod.GuestyClient


GuestyClient = _load_guesty_client()

# --- constants ---------------------------------------------------------------
DATA_CSV = PROJECT_ROOT / "data" / "Yacinde_timeshares.csv"
LISTINGS_JSON = GUESTY_ACCESS / "data" / "listings.json"
DONE_LOG = PROJECT_ROOT / "logs" / "created_reservations.jsonl"
BLOCKED_LOG = PROJECT_ROOT / "logs" / "blocked_reservations.jsonl"

OWNER_CONFIRMED_PATH = "/reservations-v3/owner/confirmed"
SOURCE = "owner"  # owner-use weeks (not friends & family -> not "owner-guest")

# All 5 Yacinde units share this Guesty owner (Deb Twyford). Used for the
# required creationInfo.owner._id — attribution of who created the reservation.
CREATION_OWNER_ID = "6a39a6f8167b645287a59143"

DEFAULT_CUTOFF = dt.date(2026, 7, 30)
CREATE_DELAY_SECONDS = 1.0  # politeness between POSTs

# Only owner-use weeks are injected as owner reservations. In the source these
# are Ownership.Type == "Individual" (the Developer/LLC weeks are in the rental
# program and must stay open, not blocked).
OWNERSHIP_TYPE = "Individual"


# --- helpers -----------------------------------------------------------------
def parse_date(s: str) -> dt.date:
    """Parse a check-in/out date. Source uses ISO 'YYYY-MM-DD'."""
    return dt.date.fromisoformat(s.strip())


def split_name(raw: str) -> tuple[str, str]:
    """Split an owner name into (firstName, lastName), last word = last name.

    Strips a trailing parenthetical (e.g. 'Melody Ann Chairez (Snell)' ->
    'Melody Ann Chairez') before splitting so it doesn't become the surname.
    """
    name = re.sub(r"\([^)]*\)", "", raw).strip()
    name = re.sub(r"\s+", " ", name)
    parts = name.split(" ")
    if len(parts) == 1:
        # No given data has this, but stay safe: duplicate into both fields.
        return parts[0], parts[0]
    return " ".join(parts[:-1]), parts[-1]


def normalize_phones(*raws: str) -> list[str]:
    """Normalize one or more phone fields to a de-duplicated E.164 list.

    Accepts the separate `Phone` / `Phone.2` columns; each may itself hold more
    than one number (e.g. '253-680-9287 / 253-209-4517'). A US 10-digit number
    becomes '+1XXXXXXXXXX'; anything else is kept (digits + '+') rather than
    dropped, so nothing is silently lost.
    """
    out: list[str] = []
    for raw in raws:
        for chunk in re.split(r"[/;,]", raw or ""):
            digits = re.sub(r"\D", "", chunk)
            if not digits:
                continue
            num = "+1" + digits if len(digits) == 10 else "+" + digits
            if num not in out:
                out.append(num)
    return out


def unwrap_items(data) -> list:
    """Items from either a bare list or a {results|data: [...]} envelope."""
    return data if isinstance(data, list) else (data.get("results") or data.get("data") or [])


def load_yacinde_items() -> list[dict]:
    """Every Yacinde listing object from the GuestyAccess pull (listings.json).

    Read/parsed once here; callers project the field they need (listingId,
    accommodates, ...) so the source-shape unwrap and the "Yacinde" filter live
    in exactly one place.
    """
    items = unwrap_items(json.loads(LISTINGS_JSON.read_text()))
    return [it for it in items if (it.get("nickname") or "").startswith("Yacinde")]


def load_listing_map() -> dict[str, str]:
    """nickname -> listingId for the Yacinde units, from GuestyAccess pull."""
    return {it["nickname"]: it["_id"] for it in load_yacinde_items()}


def load_done_keys() -> set[str]:
    """Keys (listing|checkin) already created, for idempotent resume."""
    if not DONE_LOG.exists():
        return set()
    keys = set()
    for line in DONE_LOG.read_text().splitlines():
        line = line.strip()
        if not line:
            continue
        rec = json.loads(line)
        keys.add(rec["key"])
    return keys


def append_done(rec: dict) -> None:
    DONE_LOG.parent.mkdir(parents=True, exist_ok=True)
    with DONE_LOG.open("a") as f:
        f.write(json.dumps(rec) + "\n")


def append_blocked(rec: dict) -> None:
    BLOCKED_LOG.parent.mkdir(parents=True, exist_ok=True)
    with BLOCKED_LOG.open("a") as f:
        f.write(json.dumps(rec) + "\n")


def is_availability_conflict(msg: str) -> bool:
    """True when a create failed because the dates are already blocked/booked.

    Guesty returns a 400 for this (an existing reservation or a still-connected
    iCal feed). The shared GuestyClient raises with the status + body stringified,
    so we detect it from the message. This is a data conflict to skip-and-log,
    not a script fault.
    """
    return "(400)" in msg and ("availability" in msg or "VALIDATION_ERROR" in msg)


def build_owner_payload(*, listing_id, checkin, checkout, guest, guests_count, notes_other) -> dict:
    """Assemble the owner-confirmed reservation payload — the API contract, in one place.

    `checkin`/`checkout` are ``datetime.date``; both injectors share this exact
    skeleton (only their value derivations and the notes text differ).
    """
    return {
        "listingId": listing_id,
        "checkInDateLocalized": checkin.isoformat(),
        "checkOutDateLocalized": checkout.isoformat(),
        "source": SOURCE,
        "guest": guest,
        "guestsCount": guests_count,
        "numberOfGuests": {"numberOfAdults": guests_count},
        "creationInfo": {"owner": {"_id": CREATION_OWNER_ID}},
        "notes": {"other": notes_other},
    }


def run_create_loop(client, pending: list[dict], *, extra_rec: dict | None = None) -> tuple[int, int]:
    """POST each plan as an owner reservation; the shared live-run loop.

    Used by both entry points. Every success is appended to the done-log (merging
    `extra_rec`, e.g. a source tag); every availability-400 is skip-and-logged to
    the blocked-log and the run continues. Any OTHER error is unexpected, so we
    stop (``SystemExit(1)``) rather than half-run blind. Returns (created, blocked).
    """
    created = blocked = 0
    for i, p in enumerate(pending, 1):
        print(f"\n[{i}/{len(pending)}] creating: {p['label']}")
        try:
            resp = client.post(OWNER_CONFIRMED_PATH, p["payload"])
        except Exception as e:  # noqa: BLE001
            msg = str(e)
            if is_availability_conflict(msg):
                print("    SKIP — dates already blocked (logged to blocked_reservations.jsonl)")
                append_blocked({"key": p["key"], "listing": p["listing"],
                                "label": p["label"], "error": msg[:500]})
                blocked += 1
                if i < len(pending):
                    time.sleep(CREATE_DELAY_SECONDS)
                continue
            print(f"    ERROR: {e}")
            print("    Stopping. Fix the issue and re-run — already-created rows are skipped.")
            raise SystemExit(1)
        rec = {
            "key": p["key"],
            "listing": p["listing"],
            "reservationId": resp.get("reservationId"),
            "confirmationCode": resp.get("confirmationCode"),
            "status": resp.get("status"),
            "guestId": resp.get("guestId"),
        }
        if extra_rec:
            rec.update(extra_rec)
        append_done(rec)
        created += 1
        print(
            f"    OK reservationId={rec['reservationId']} "
            f"code={rec['confirmationCode']} status={rec['status']}"
        )
        if i < len(pending):
            time.sleep(CREATE_DELAY_SECONDS)

    print(f"\nDone. Created {created} owner reservation(s)."
          + (f" Skipped {blocked} already-blocked (see {BLOCKED_LOG})." if blocked else ""))
    return created, blocked


# --- core --------------------------------------------------------------------
def build_row_payloads(cutoff: dt.date) -> list[dict]:
    """Read the CSV and build one owner-reservation plan per eligible booking."""
    listing_map = load_listing_map()
    rows = [r for r in csv.DictReader(DATA_CSV.open()) if (r.get("Listing") or "").strip()]

    plans: list[dict] = []
    problems: list[str] = []
    for r in rows:
        # Only owner-use (Individual) weeks become owner reservations; the
        # Developer/LLC rental-program weeks must stay open.
        if (r.get("Ownership.Type") or "").strip() != OWNERSHIP_TYPE:
            continue
        listing = r["Listing"].strip()
        checkin = parse_date(r["checkin"])
        checkout = parse_date(r["checkout"])
        if checkin < cutoff:
            continue
        listing_id = listing_map.get(listing)
        if not listing_id:
            problems.append(f"no listingId for {listing!r} ({r['Owner']} {r['checkin']})")
            continue

        first, last = split_name(r["Owner"])
        phones = normalize_phones(r.get("Phone", ""), r.get("Phone.2", ""))
        email = (r.get("Email.1") or "").strip()

        # guestsCount is required by the API (>= 1). Prefer the source MaxGuests;
        # fall back to inferring a couple from the name ("A & B", "A and B").
        try:
            guests_count = int((r.get("MaxGuests") or "").strip())
        except ValueError:
            guests_count = 0
        if guests_count < 1:
            name = r["Owner"]
            guests_count = 2 if ("&" in name or re.search(r"\band\b", name, re.I)) else 1

        guest = {"firstName": first, "lastName": last, "email": email, "phones": phones}
        payload = build_owner_payload(
            listing_id=listing_id,
            checkin=checkin,
            checkout=checkout,
            guest=guest,
            guests_count=guests_count,
            notes_other=f"Timeshare owner week — {r['Year']} wk {r['Week.#']} "
            f"seg {r['Segment']}. Owner: {r['Owner']}.",
        )
        plans.append(
            {
                "key": f"{listing}|{checkin.isoformat()}",
                "listing": listing,
                "label": f"{listing}  {checkin.isoformat()}→{checkout.isoformat()}  {r['Owner']}",
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
    ap.add_argument("--dry-run", action="store_true", help="preview only, no API writes")
    ap.add_argument("--limit", type=int, default=None, help="process at most N bookings")
    ap.add_argument(
        "--cutoff",
        type=lambda s: dt.date.fromisoformat(s),
        default=DEFAULT_CUTOFF,
        help="skip bookings with check-in before this date (YYYY-MM-DD)",
    )
    args = ap.parse_args()

    plans = build_row_payloads(args.cutoff)
    done = load_done_keys()
    pending = [p for p in plans if p["key"] not in done]

    print(
        f"\nEligible (check-in ≥ {args.cutoff}): {len(plans)} | "
        f"already created: {len(plans) - len(pending)} | to do now: {len(pending)}"
    )
    if args.limit is not None:
        pending = pending[: args.limit]
        print(f"--limit {args.limit} -> processing {len(pending)} this run")

    if args.dry_run:
        print("\n=== DRY RUN (no writes) ===")
        for p in pending:
            g = p["payload"]["guest"]
            print(f"\n• {p['label']}")
            print(f"    listingId : {p['payload']['listingId']}")
            print(f"    guest     : {g['firstName']} {g['lastName']} | {g['email']} | {g['phones']}")
            print(f"    guests    : {p['payload']['guestsCount']}")
        print(f"\nWould create {len(pending)} owner reservation(s).")
        return 0

    client = GuestyClient()
    run_create_loop(client, pending)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
