# Inject Yacinde timeshare bookings into Guesty (owner reservations)

Adds the Yacinde timeshare weeks to the Guesty calendar as **confirmed owner
reservations**, each with the occupant's **name, email, and phone** attached.

## Why owner reservations (not iCal)

An earlier approach fed these dates in via iCal, which only creates calendar
*blocks* — no guest contact details, no reservation record. These weeks are
owner-use (`In Rental Program = No`), so the correct Guesty type is an **owner
reservation**: it blocks the calendar with no revenue/commission/channel, and
unlike a block it stores a real reservation + guest record.

We use the Guesty Open API endpoint:

    POST /reservations-v3/owner/confirmed      (source: "owner")

Docs: <https://open-api-docs.guesty.com/reference/reservationsopenapicontroller_createownerconfirmed>

## How it works

`src/inject_reservations.py`:

1. Reads `data/Yacinde_timeshares.csv` (one row per week, ISO dates).
2. Keeps only **`Ownership.Type = Individual`** rows (the owner-use weeks); the
   Developer/LLC weeks are in the rental program and stay open.
3. Skips rows with check-in **before 2026-07-30** (`--cutoff` to change).
4. Maps each row → an owner-reservation payload:
   - `listingId` from the unit nickname (via `GuestyAccess/data/listings.json`).
   - `guest`: name split as *last word = last name* (parentheticals stripped);
     `Email.1` → email; `Phone` + `Phone.2` → de-duplicated E.164 `phones` array.
   - `source: "owner"`, `guestsCount` from `MaxGuests` (falls back to 2 for
     couples / 1 otherwise), `creationInfo.owner._id` = the Yacinde owner
     (`6a39a…9143`, uniform across all 5 units).
5. POSTs to Guesty, pausing ~1s between calls.

Auth (OAuth2 + cached 24h token + 429 handling) and credentials are **reused
from the sibling `GuestyAccess` project** — the same `.env` and the same token
cache, so we stay under Guesty's 5-tokens/24h limit.

### Idempotent & resumable

Every successful create is appended to `logs/created_reservations.jsonl`, keyed
by `listing|checkInDate`. Re-runs skip anything already logged, so an
interrupted run is safe to resume — it never double-books.

## Usage

```bash
source /Users/ylin/ValtaWork/.venv/bin/activate
cd /Users/ylin/ValtaWork/Claude_projects/Inject_timeshare_bookings

python -m src.inject_reservations --dry-run            # preview everything, no writes
python -m src.inject_reservations --dry-run --limit 3  # preview first 3
python -m src.inject_reservations --limit 1            # create ONE for real, then stop
python -m src.inject_reservations                      # create all remaining
```

Flags: `--dry-run` (no writes), `--limit N` (cap this run), `--cutoff YYYY-MM-DD`
(skip earlier check-ins; default 2026-07-30).

## Replacing the old iCal blocks (second source)

The earlier attempt pushed the actual guest bookings
(`data/Yacinde_Reservations_Clean.xlsx`) into Guesty as **iCal blocks** via
netlify feeds. To upgrade those to real owner reservations with contact details:

```bash
python -m src.remove_ical_feeds --dry-run          # list the 8 Yacinde_*_pre feeds
python -m src.remove_ical_feeds                     # disconnect them (clears future blocks)
python -m src.inject_clean_reservations --dry-run   # preview (⚠ marks placeholder emails)
python -m src.inject_clean_reservations             # create all remaining
```

Remove the feeds **first** — otherwise each date is still blocked by its own iCal
block and the create is skipped. Feed block-clearing is async, so if a first run
skips a few, just re-run once they clear (it's resumable). All rows become owner
reservations regardless of channel; Airbnb rows with no email get a placeholder.

## Scope / results

- **Timeshare weeks** (`Yacinde_timeshares.csv`): 212 `Ownership.Type = Individual`,
  170 with check-in ≥ 7/30 → **168 created** (2 already existed).
- **iCal-migrated bookings** (`Yacinde_Reservations_Clean.xlsx`): 118 total, 68
  after cutoff+dedup → **66 created**, 8 old iCal feeds removed. The 2 skipped
  already existed as owner-guest reservations for the same guests.
- Units: B1, B2, B3, B4, F1 (timeshare) plus B6, E1, F5 (iCal bookings).

## Files

| Path | Purpose |
|------|---------|
| `src/inject_reservations.py` | Injector for the timeshare owner weeks. |
| `src/inject_clean_reservations.py` | Injector for the iCal-migrated guest bookings. |
| `src/remove_ical_feeds.py` | Disconnects the old `Yacinde_*_pre` iCal feeds. |
| `data/Yacinde_timeshares.csv` | Timeshare owner weeks (name/email/phone per week). |
| `data/Yacinde_Reservations_Clean.xlsx` | The 118 actual guest bookings behind the iCal feeds. |
| `logs/created_reservations.jsonl` | Shared audit/resume log of created reservations. |
| `logs/blocked_reservations.jsonl` | Dates skipped because already blocked/booked. |
