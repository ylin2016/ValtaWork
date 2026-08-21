# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A backfill tool that loads Yacinde bookings into the Guesty calendar as
**confirmed owner reservations** — each carrying the guest's name, email, and
phone. It has **two data sources / entry points**:

1. `src/inject_reservations.py` — the **timeshare owner weeks**
   (`data/Yacinde_timeshares.csv`).
2. `src/inject_clean_reservations.py` — the **iCal-migrated guest bookings**
   (`data/Yacinde_Reservations_Clean.xlsx`): the actual Airbnb/VRBO/Website/
   Owner-Guest reservations that used to live as iCal blocks. Per the user, ALL
   become owner reservations regardless of channel.
   Its prerequisite, `src/remove_ical_feeds.py`, disconnects the old iCal feeds
   first (see the migration workflow below).

The design decision that shaped everything: bookings are represented as **owner
reservations via the Guesty Open API** (`POST /reservations-v3/owner/confirmed`,
`source: "owner"`), *not* iCal feeds. An iCal feed only creates calendar *blocks*
with no guest contact details or reservation record; an owner reservation blocks
the calendar with no revenue/commission/channel but stores a real reservation +
guest. See `README.md` for the user-facing walkthrough.

## Commands

```bash
source /Users/ylin/ValtaWork/.venv/bin/activate      # shared workspace venv
cd /Users/ylin/ValtaWork/Claude_projects/Inject_timeshare_bookings

# Source 1 — timeshare owner weeks
python -m src.inject_reservations --dry-run           # preview payloads, zero writes
python -m src.inject_reservations --limit 1           # create ONE for real, then stop
python -m src.inject_reservations                      # create all remaining

# Source 2 — iCal-migrated guest bookings (remove feeds FIRST, then create)
python -m src.remove_ical_feeds --dry-run             # list the 8 Yacinde_*_pre feeds
python -m src.remove_ical_feeds                        # delete them (clears future blocks)
python -m src.inject_clean_reservations --dry-run     # preview; ⚠ marks placeholder emails
python -m src.inject_clean_reservations               # create all remaining
```

All three take `--dry-run`; the injectors also take `--limit N` and
`--cutoff YYYY-MM-DD` (**default 2026-07-30**). There is no test suite;
**`--dry-run` is the verification mechanism** — always run it before a live run.

### iCal → owner-reservation migration workflow

The two sources share the done-log, so running source 2 after source 1
auto-skips the ~30 owner-weeks that appear in both files. Order matters:

1. `remove_ical_feeds` DELETEs each `Yacinde_*_pre` imported calendar with
   strategy `remove_future_channel_events` (clears future iCal blocks, keeps
   past as history, stops re-syncing). **Block-clearing is ASYNC per listing** —
   some units free up within seconds, others lag a few minutes.
2. `inject_clean_reservations` then creates owner reservations on the freed
   dates. Because clearing is async, a first run may skip-and-log a few
   still-blocked dates; just re-run (it's resumable) once they clear. A
   still-blocked date backed by an existing `o` (owner) reservation is a genuine
   pre-existing booking — leave it.

## Architecture / things that require reading multiple files

**Auth is borrowed from the sibling `GuestyAccess` project, not reimplemented.**
`_load_guesty_client()` in `src/inject_reservations.py` imports GuestyAccess's
Open API client (OAuth2 client-credentials + disk-cached token + 429 retry) and
reuses its `../GuestyAccess/.env` credentials **and its token cache**. This
matters: Guesty allows only **5 tokens / 24h / client**, so both projects share
one cached token — never mint fresh tokens per run.

**Why the import is done via `importlib`, not a plain `import`:** both this
project and GuestyAccess have a top-level `src` package, so
`from src.guesty_client import ...` would resolve to the wrong one. The loader
registers GuestyAccess's `src` under a unique module name (`guestyaccess_src`)
so its own relative imports (`from .config import ...`) still resolve. If you
move either project, update `GUESTY_ACCESS` at the top of the script.

**Listing IDs come from GuestyAccess's pulled data**, not a live call:
`load_listing_map()` reads `../GuestyAccess/data/listings.json` and maps unit
nickname → `listingId`. If a Yacinde unit is missing there, re-pull listings in
GuestyAccess first.

**Idempotency / resume is via an append-only log.** Every successful create is
written to `logs/created_reservations.jsonl` keyed by `listing|checkInDate`.
Re-runs skip anything already logged, so an interrupted run is safe to resume
and never double-books. To genuinely re-create a booking you must remove its
line from that log. **Both injectors share this one log** — that's deliberate:
it's how source 2 (clean xlsx) auto-skips owner weeks already created by source
1. Availability conflicts (a 400 "no availability", i.e. the date is already
blocked/booked) are skip-and-logged to `logs/blocked_reservations.jsonl` rather
than aborting the run; any *other* error stops the run so it never half-runs blind.

**`inject_clean_reservations.py` reuses source 1's helpers** — it imports the
client, `load_done_keys`/`load_yacinde_items`, `normalize_phones`, the payload
builder (`build_owner_payload`), and the shared live-run loop (`run_create_loop`)
from `inject_reservations` (importing that module is safe; its `__main__` guard
means nothing runs). The create loop, the availability-conflict detection
(`is_availability_conflict`), and the owner-reservation payload skeleton live
**once** in `inject_reservations.py`; both entry points call them, so keep all
shared logic there rather than re-copying it.

## Guesty Open API gotchas (learned the hard way)

- `guestsCount` (≥ 1) is **required despite the schema marking it optional** —
  omitting it returns a 400. The script infers 2 for couples (name contains `&`
  or `and`), else 1, and sends a matching `numberOfGuests.numberOfAdults`.
- `creationInfo.owner._id` is **required** — it's the "who created this"
  attribution and must be a real Guesty owner `_id`. All 5 Yacinde units share
  one owner (`6a39a6f8167b645287a59143`, Deb Twyford), hardcoded as
  `CREATION_OWNER_ID`.
- The `guest` object requires `firstName`, `lastName`, `phones` (array), `email`.
  Phones go in as E.164 (`+1…`); Guesty stores them back without the leading `+`.

## Source-data mapping (`data/Yacinde_timeshares.csv`)

One row per week, ISO dates. The CSV contains **all 762 weeks** across the 5
units; only the **`Ownership.Type = Individual`** rows (212) are owner-use and
get injected — the Developer/LLC weeks are in the rental program and are skipped
so they stay bookable. Non-obvious transforms in `build_row_payloads()`:
- **Name split is last-word-as-surname**, after stripping a trailing
  parenthetical (`Melody Ann Chairez (Snell)` → first `Melody Ann`, last
  `Chairez`).
- **Two phone columns** (`Phone`, `Phone.2`) are merged and de-duplicated into
  the E.164 `phones` array (each field may itself hold `A / B`).
- **`guestsCount` comes from `MaxGuests`** (the unit capacity, 4 or 8), falling
  back to couple-inference from the name only if it's blank.
- Only `Email.1` is sent — the owner-confirmed guest object has no `emails`
  array, so `Email.2` is not attached.

## Source-data mapping (`data/Yacinde_Reservations_Clean.xlsx`, source 2)

118 real guest bookings (sheet `Reservations`), variable nights, ISO dates,
columns `Res # / Unit / Guest First / Guest Last / Source / Status / Check In /
Check Out / Nights / Total Rent / Email / Phone`. In `build_plans()`:
- First/Last are already split in the file — no name-splitting needed.
- **Missing contact → placeholder** (the API requires both): Airbnb hides guest
  emails, so ~13 rows have none → `res<Res#>@guests.valtarealty.com`; a missing
  phone → `PLACEHOLDER_PHONE`. This was the user's explicit choice over skipping.
- `guestsCount` = the listing's `accommodates` (this file has no guest-count
  column), read from `listings.json`.
- All rows use `source: "owner"` regardless of their original channel.
