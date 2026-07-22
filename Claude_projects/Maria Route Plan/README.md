# Maria Route Plan

A Google Apps Script that scans **Maria's** Google Calendars once a day and texts
her (via **Twilio**) her next-day jobs, **plus** builds a daily, address-by-address
**route plan** across those same calendars (see *Maria's route plan* below). Maria
can have several phone numbers — every number gets its own 1:1 SMS copy.

It runs *as* the vacation@valtarealty.com account, so calendars are read
natively — no key files or OAuth tokens to manage.

## Which calendars are read

Only **three calendars, all Maria's**:

| Calendar | Cleaning type(s) | What it holds |
|----------|------------------|---------------|
| `ValtaAuto_Maria` | Back-to-back / Next-day | turnover shifts — purple 11am–4pm (back-to-back) and yellow 4–10pm (next-day) |
| `Residential Cleaning` | Residential | residential jobs (address only) |
| `move in/out cleaning` | Move-in/out | move-in/out jobs (address only) |

The turnover types come from the `ValtaAuto_Maria` shift's start time; the other
two are typed by which calendar the event sits on. All three are configured on the
single **Maria** row in `Cleaners.gs` (`calendar` + `extraCalendars`).

## How the calendar is read

- **A job is a titled event, and one event can hold many units.** The title is a
  comma-separated list like
  `Bellevue 2243(6->0),Elektra 1212(4->0),Seattle 8415(3->0)` where each unit is
  `Property Unit(guests_out->guests_in)`. The script splits on commas and lists
  each unit on its own line. Empty `(No title)` 11am–4pm / 4–10pm blocks are shift
  placeholders and are ignored (`CONFIG.SKIP_UNTITLED`).
- **Back-to-back is a property of the shift (event), not the unit.** The purple
  morning event (11am–4pm) is the back-to-back turn — a guest checks in that day,
  so it must finish before arrival — and its whole section is tagged
  `⚠️ BACK-TO-BACK`. The yellow evening event (4pm–10pm) is not. Shifts are told
  apart by start time (`CONFIG.BACKTOBACK_BEFORE_HOUR`, default 3pm), so the
  `(out->in)` arrows on individual units don't affect this.
- Maria's titled jobs for the day (all three calendars) are combined into **one** text.

Each unit is forwarded **as written** (Maria's own shorthand). If you'd
rather expand `Property Unit(out->in)` into words, say so.

## Files

| File              | What it is                                                        |
|-------------------|-------------------------------------------------------------------|
| `Code.gs`         | Main logic: Maria's calendars → titled jobs → compose → send.     |
| `Cleaners.gs`     | The Maria row: name, turnover calendar, extra calendars, phone.   |
| `Config.gs`       | Settings: days-ahead, `DRY_RUN` safety switch, skip-untitled, `ROUTING`. |
| `Routes.gs`       | Maria's daily route plan: geocode → pack into cars → order stops. |
| `Listings.gs`     | Listing → address / bedrooms / bathrooms (from `data/Listings.xlsx`). |
| `appsscript.json` | Manifest (timezone + OAuth scopes).                              |

## One-time setup

### 1. Create the Apps Script project
Signed in as **vacation@valtarealty.com**:
1. <https://script.google.com> → **New project**.
2. Create the four files above and paste in their contents (or push with
   [`clasp`](https://github.com/google/clasp)).
3. **Project Settings** → tick *"Show appsscript.json manifest file"*.

### 2. Confirm Maria's calendars
Run **`listCleanerCalendars`** once and check the log — it prints every calendar
name/ID the account can see. Make sure the three names on the Maria row of
`Cleaners.gs` (`ValtaAuto_Maria`, `Residential Cleaning`, `move in/out cleaning`)
match exactly. (If any name isn't unique, put the calendar's ID in a `calendarId:`
field instead.)

### 3. Add Maria's phone(s)
Edit `Cleaners.gs` — fill `phones` in **E.164** (`+1` + 10 digits, no
spaces/dashes). List **all the numbers** (Maria plus any helpers):
`phones: ['+18283311782', '+12065551234']`. **Each number gets its own 1:1 SMS
copy** of the schedule.

### 4. Add Twilio credentials (secrets — not in code)
1. Create a Twilio account and an **SMS-enabled** phone number.
2. Apps Script → **Project Settings → Script properties → Add**:
   | Property               | Value                                   |
   |------------------------|-----------------------------------------|
   | `TWILIO_ACCOUNT_SID`   | Account SID (starts `AC…`)              |
   | `TWILIO_AUTH_TOKEN`    | Auth Token                              |
   | `TWILIO_FROM_NUMBER`   | Twilio number, e.g. `+15125550100`      |

### How messages are sent
Every number in Maria's `phones` list receives its **own separate 1:1 SMS** copy
of the schedule — Maria and any helpers each get the same text individually (there
is no shared group thread; replies are private to each sender). Any US A2P 10DLC
registration your Twilio account requires for SMS still applies.

**Leader / dispatcher oversight.** Put the leader's number in
`CONFIG.LEADER_PHONES` and they'll get a **1:1 copy of the schedule**, exactly like
Maria does.

### 5. Check the timezone
`appsscript.json` is `America/Los_Angeles` (Pacific — matches the Seattle-area
properties and the GMT-07 grid in your calendar). This controls what counts as
"tomorrow" and the times printed in the message.

## Try it (safe — no texts sent)

`Config.gs` ships with `DRY_RUN: true`. Run **`previewTomorrow`** (first run asks
you to authorize Calendar + external-request access). Open **Executions / Logs**:
you'll see the **exact SMS** that would be sent to Maria — no texts go
out. To check a specific day, set `CONFIG.PREVIEW_DATE` and run **`previewDate`**
(the Run button can't pass an argument, so it reads the date from Config). Share
that log with me and we'll tune the parsing to your real events if anything looks off.

## Go live

1. Set `DRY_RUN: false` in `Config.gs`.
2. Run **`runDaily`** once manually — real texts send. Confirm receipt.

## Cleaning types & extra calendars

Every cleaning has a **type**:

| Type | Where it comes from |
|------|---------------------|
| Back-to-back | `ValtaAuto_Maria` calendar, purple 11am–4pm shift (start before `BACKTOBACK_BEFORE_HOUR`) |
| Next-day | `ValtaAuto_Maria` calendar, yellow 4–10pm shift |
| Residential | the **`Residential Cleaning`** extra calendar |
| Move-in/out | the **`move in/out cleaning`** extra calendar |

Maria's extra calendars beyond her turnover shifts are listed in
`Cleaners.gs` under `extraCalendars`, each tagged with its `type`. Their events
are folded into Maria's **daily message** (as their own labeled sections)
and **weekly summary**. Maria is set up with Residential + Move-in/out.

**Residential** renders differently: **one row per cleaning** under a single
`Residential:` section — no time, no unit splitting. Each row shows the
**address only**. It comes from the event's structured *location* field when set
(deduped); when that's empty, the address is dug out of the freeform title — the
first `<number> <street>` segment plus the city that follows, dropping price,
names, and phone numbers. So

```
Monthly : $180, 14701 SE 42nd ST, Bellevue, Isabelle WFH, 4252833210 Max4.5ph
```

renders as

```
Residential:
 • 14701 SE 42nd ST, Bellevue
```

It counts as one cleaning per event in the totals. (Back-to-back / next-day /
move-in-out still split their comma-separated unit lists.)

> Calendar names are matched **exactly** (case-sensitive). If an extra calendar
> "not found" shows in the log, run `listCleanerCalendars` and copy the name
> verbatim (or use `calendarId`).

## Weekly summary (Mondays)

`runWeekly` texts Maria a summary of one **Monday–Sunday** week: totals per
type, plus a per-day breakdown. Counts are by unit, same as the daily reminder.
Only the types Maria actually covers are shown.

**Choose which week** with `CONFIG.WEEKLY_TARGET`:

| Value | `runWeekly` sends… |
|-------|--------------------|
| `'upcoming'` (default) | today's week if it's **Monday**, else **next** week — so the Monday trigger covers the week starting that day, and a manual mid-week run looks ahead |
| `'this'` | always the Mon–Sun week **containing today** |
| `'next'` | always the week **after** this one |

To just *look* without changing the setting, run **`previewThisWeek`** or
**`previewNextWeek`** (neither ever sends). **`previewWeekly`** shows exactly what
`runWeekly` would send under the current `WEEKLY_TARGET`. Example:

```
Valta Realty Cleaning Schedule — Weekly Summary
Mon, Jul 13 – Sun, Jul 19 (49 units)

Totals:
 • Back-to-back: 12
 • Next-day: 34
 • Residential: 3

By day:
 Mon Jul 13 — B2B 1, Next 4, Res 1
 Tue Jul 14 — none
 Wed Jul 15 — B2B 2, Next 5
 …

Thanks, Maria!
```

The weekly summary goes to **Maria** (no leader CC).

## Maria's route plan (daily)

Maria's day spans turnovers **plus** the Residential and Move-in/out calendars, so
she gets an extra **route plan** message: the per-type counts *and* the day's jobs
packed into cars with an ordered, address-by-address route. Other cleaners are
unaffected. Preview with **`previewMariaRoutes`** (tomorrow) or **`previewMariaRoutesDate`**
(`CONFIG.PREVIEW_DATE`); send with **`runMariaRoutes`**.

**How it works.** Each cleaning becomes a *stop*. A turnover event splits into one
stop per unit — each unit's address, bedrooms and bathrooms come from `Listings.gs`
(a nickname like `Bellevue 2243(6->0)` is matched by stripping the `(…)`).
Residential / move-in/out stops use the address on the calendar. Every address is
geocoded once (cached) via the built-in **Maps service** — no API key.

**Cars and crews (workload-driven).** The planner first sizes the day's work to
decide **how many cars** are needed — the fewest (up to **3**) that hold the units
and the labor — then staffs **each car with 2–4 cleaners**: the *fewest* people who
can finish that car's route within the **8-hour** day. Each car still holds at most
**6 units**. So a light day might be one 2-person car; a heavy day, three 4-person
cars. **A bigger crew cleans proportionally faster** — a job that's 1h for 2 people
is ~40m for 3 and 30m for 4 — so the crew size is shown per car and the per-stop
times reflect it.

**Back-to-back comes first.** B2B turnovers have a same-day check-in deadline, so
the packer places **every B2B before any other job**:
- **Done first in each car** — a car's B2B stops are ordered ahead of its
  Residential / next-day / move-in-out stops.
- **Spread across the crews** — when the day needs more than one car, B2B are
  distributed so each crew starts its morning on a deadline job.
- **Never over capacity** — only non-B2B work can spill to **over capacity**; a
  B2B is never deferred. (In the rare case B2B alone exceed every crew's capacity,
  the plan flags a loud `🚨 B2B OVER CAPACITY — add a crew`.)

Non-B2B jobs then fill the leftover capacity by nearest-next stop; anything past
3 cars is flagged as **over capacity** (non-B2B only).

**Cleaning-time model** (`CONFIG.ROUTING`, all tunable): turnover = `CLEAN_BASE_MIN`
(25) + `CLEAN_PER_BEDROOM_MIN` (15) × beds + `CLEAN_PER_BATHROOM_MIN` (20) × baths,
in clock-minutes for a **`CALIBRATION_CREW`** (2)-person crew — anchored so a
**1BR/1BA = 1 hour** at 2 people; residential & move-in/out = a flat 2h. The planner
treats these as labor (time × 2 person-min) and divides by the car's actual crew
size. Travel = straight-line miles × `ROAD_FACTOR` ÷ `AVG_SPEED_MPH`.

**Set these before going live:**
1. `CONFIG.ROUTING.BASE_ADDRESS` — the office/home base crews start and end at.
   Without it the plan still runs but can't count the first/last drive.
2. Keep `Listings.gs` current (or set `CONFIG.ROUTING.LISTINGS_SHEET_ID` to a Google
   Sheet with columns *Listing, Address, Bedrooms, Bathrooms* — the sheet overrides
   the embedded table). Run **`checkListings`** to see which of tomorrow's units
   resolve to an address; fix any "UNKNOWN"/"GEOCODE FAILED" before relying on it.

Example:

```
Valta Realty Cleaning Schedule — Maria's Route Plan
Mon, Jul 20 (7 units, 2 cars)
Counts: B2B 3, Next 2, Res 2

🚗 Car 1 — 3 cleaners, 4 units, 7h10m (5h7m clean + 2h3m drive · 57 mi):
 1. Elektra 1212 — 1400 Hubbell Pl, Seattle (2BR/1BA) · B2B 50m
 2. Bellevue 2243 — 2243 W Lake Sammamish Pkwy SE, Bellevue (4BR/3BA) · B2B 1h37m
 3. 14701 SE 42nd — 14701 SE 42nd St, Bellevue · Res 1h20m
 4. 1938 Riva Ln NW — 1938 Riva Ln NW, Issaquah · Res 1h20m
    ↩ back to base
🚗 Car 2 — 2 cleaners, 3 units, 6h28m (4h25m clean + 2h3m drive · 57 mi):
 1. Seattle 8415 — 8415 Linden Ave N, Seattle (3BR/2BA) · B2B 1h50m
 2. Ballard 55 — 5501 8th Ave NW, Seattle (1BR/1BA) · Next 1h
 3. Kirkland 9 — 9 Central Way, Kirkland (2BR/2BA) · Next 1h35m
    ↩ back to base
```

Each car shows its crew size and leads with its B2B job; per-stop times reflect the
crew (Car 1's 3 cleaners clean the 2BR/1BA in 50m, Car 2's 2 clean the 1BR/1BA in 1h).

**Trigger:** function `runMariaRoutes`, **Time-driven → Day timer**, morning (or the
evening before, matching your daily reminder). Geocoding uses the Maps-service quota
(a few hundred lookups/day; results are cached, so repeat addresses are free).

## Automate the scans (when ready)

Currently manual, by design. To automate, Apps Script → **Triggers** (clock icon)
→ **Add Trigger**:
- **Daily:** function `runDaily`, **Time-driven → Day timer**, pick an hour (e.g.
  5–6pm so Maria gets tomorrow's list the evening before).
- **Weekly:** function `runWeekly`, **Time-driven → Week timer → Every Monday**,
  pick an hour (e.g. Monday morning).
- **Maria's route plan:** function `runMariaRoutes`, **Time-driven → Day timer**,
  same cadence as the daily reminder.

## Assumptions to confirm

- A **titled** event = a job; `(No title)` = ignore.
- Only **Maria's three calendars** are read — `ValtaAuto_Maria` (turnovers),
  `Residential Cleaning`, and `move in/out cleaning`. All other cleaners are out of
  scope for this project.
- Twilio charges per message (~1¢ US SMS); US A2P registration may be required
  depending on your account type.
