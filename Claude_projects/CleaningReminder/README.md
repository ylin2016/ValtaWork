# CleaningReminder

A Google Apps Script that scans each cleaner's Google Calendar once a day, finds
their **next-day** cleaning jobs, and texts them (via **Twilio**) the list of
units, with times and any notes.

It runs *as* the vacation@valtarealty.com account, so calendars are read
natively — no key files or OAuth tokens to manage.

## How the calendar is read

- **One calendar per cleaner** — `ValtaAuto_Angelina`, `ValtaAuto_Anna`,
  `ValtaAuto_Camilla`, `ValtaAuto_Crystal`, `ValtaAuto_Maria`. The calendar an
  event sits on is how we know whose job it is.
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
- Each cleaner's titled jobs for the day are combined into **one** text.

Each unit is forwarded **as written** (the cleaners' own shorthand). If you'd
rather expand `Property Unit(out->in)` into words, say so.

## Files

| File              | What it is                                                        |
|-------------------|-------------------------------------------------------------------|
| `Code.gs`         | Main logic: per-cleaner calendar → titled jobs → compose → send.  |
| `Cleaners.gs`     | One row per cleaner: name, calendar name, phone.                  |
| `Config.gs`       | Settings: days-ahead, `DRY_RUN` safety switch, skip-untitled.     |
| `appsscript.json` | Manifest (timezone + OAuth scopes).                              |

## One-time setup

### 1. Create the Apps Script project
Signed in as **vacation@valtarealty.com**:
1. <https://script.google.com> → **New project**.
2. Create the four files above and paste in their contents (or push with
   [`clasp`](https://github.com/google/clasp)).
3. **Project Settings** → tick *"Show appsscript.json manifest file"*.

### 2. Confirm the cleaner calendars
Run **`listCleanerCalendars`** once and check the log — it prints every calendar
name/ID the account can see. Make sure the `calendar:` values in `Cleaners.gs`
match exactly. (If any name isn't unique, put the calendar's ID in a
`calendarId:` field on that row instead.)

### 3. Add each cleaner's phone(s)
Edit `Cleaners.gs` — fill `phones` for each cleaner in **E.164** (`+1` + 10
digits, no spaces/dashes). List **all the numbers for that cleaner's group** (the
cleaner plus any helpers): `phones: ['+18283311782', '+12065551234']`. Leave
`phones: []` to skip that cleaner for now. See *Group messaging* below.

### 4. Add Twilio credentials (secrets — not in code)
1. Create a Twilio account and a phone number. For group messaging it must be an
   **MMS-enabled US/Canada number**; for plain 1:1 SMS any SMS number works.
2. Apps Script → **Project Settings → Script properties → Add**:
   | Property               | Value                                   |
   |------------------------|-----------------------------------------|
   | `TWILIO_ACCOUNT_SID`   | Account SID (starts `AC…`)              |
   | `TWILIO_AUTH_TOKEN`    | Auth Token                              |
   | `TWILIO_FROM_NUMBER`   | Twilio number, e.g. `+15125550100`      |

### Group messaging
With `CONFIG.GROUP_MESSAGING: true` (the default), each cleaner's `phones` form
**one shared group text** (Twilio Conversations *Group MMS*) — the cleaner and
their helpers all see each other's replies, like a normal group chat. The thread
is created once per cleaner and reused every day (its ID is cached in Script
Properties), so it stays one continuous conversation.

**Leader / dispatcher oversight.** Put the leader's number in
`CONFIG.LEADER_PHONES`. With a single Twilio number the leader **cannot** be a
member of every group (a number can only be in one group MMS thread per sending
number), so instead they receive a **1:1 copy** of each cleaner's schedule. They
see every schedule; they don't see the cleaners' in-thread replies. (To put the
leader *inside* every group you'd need one Twilio number per cleaner and a
`fromNumber` per cleaner in `Cleaners.gs`.)

Requirements & limits:
- **MMS-enabled US/Canada** sending number; **A2P 10DLC** registration for US.
- A cleaner needs **2+ numbers** to form a group; one number falls back to 1:1 SMS.
- A phone number can be in **only one group per sending number** — so don't put
  the same helper in two cleaners' groups on one number.
- After you **change a cleaner's `phones`**, run **`resetGroups`** once so the
  next run rebuilds the thread with the new members.

Set `GROUP_MESSAGING: false` to send every number (cleaner + leader) a plain 1:1 SMS.

### 5. Check the timezone
`appsscript.json` is `America/Los_Angeles` (Pacific — matches the Seattle-area
properties and the GMT-07 grid in your calendar). This controls what counts as
"tomorrow" and the times printed in the message.

## Try it (safe — no texts sent)

`Config.gs` ships with `DRY_RUN: true`. Run **`previewTomorrow`** (first run asks
you to authorize Calendar + external-request access). Open **Executions / Logs**:
for each cleaner you'll see the **exact SMS** that would be sent — no texts go
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
| Back-to-back | main `ValtaAuto_*` calendar, purple 11am–4pm shift (start before `BACKTOBACK_BEFORE_HOUR`) |
| Next-day | main `ValtaAuto_*` calendar, yellow 4–10pm shift |
| Residential | the cleaner's **`Residential Cleaning`** extra calendar |
| Move-in/out | the cleaner's **`move in/out cleaning`** extra calendar |

A cleaner can cover extra calendars beyond their own shifts — list them in
`Cleaners.gs` under `extraCalendars`, each tagged with its `type`. Their events
are folded into that cleaner's **daily message** (as their own labeled sections)
and **weekly summary**. Maria is set up with Residential + Move-in/out.

**Residential** renders differently: **one row per cleaning** under a single
`Residential:` section — no time, no unit splitting. Each row shows the
**address only** (the event's *location* field, deduped; the freeform title is
ignored) →

```
Residential:
 • 1938 Riva Ln NW, Issaquah, WA 98027, USA
```

It counts as one cleaning per event in the totals. (Back-to-back / next-day /
move-in-out still split their comma-separated unit lists.)

> Calendar names are matched **exactly** (case-sensitive). If an extra calendar
> "not found" shows in the log, run `listCleanerCalendars` and copy the name
> verbatim (or use `calendarId`).

## Weekly summary (Sundays)

`runWeekly` texts each cleaner a summary of their **coming week** (the 7 days
Sun–Sat): totals per type, plus a per-day breakdown. Counts are by unit, same as
the daily reminder. Only the types that cleaner actually covers are shown.
Preview safely with **`previewWeekly`** (never sends). Example:

```
Valta Realty Cleaning Schedule — week of Sun, Jul 12 (49 units):

Totals:
 • Back-to-back: 12
 • Next-day: 34
 • Residential: 3

By day:
 Sun Jul 12 — B2B 2, Next 5
 Mon Jul 13 — B2B 1, Next 4, Res 1
 Tue Jul 14 — none
 …

Thanks, Maria!
```

The weekly summary goes to **each cleaner** (no leader CC).

## Automate the scans (when ready)

Currently manual, by design. To automate, Apps Script → **Triggers** (clock icon)
→ **Add Trigger**:
- **Daily:** function `runDaily`, **Time-driven → Day timer**, pick an hour (e.g.
  5–6pm so cleaners get tomorrow's list the evening before).
- **Weekly:** function `runWeekly`, **Time-driven → Week timer → Every Sunday**,
  pick an hour (e.g. Sunday morning).

## Assumptions to confirm

- One cleaner per calendar; a **titled** event = a job; `(No title)` = ignore.
- The **five** `ValtaAuto_*` calendars above are the full cleaner list. Add/remove
  rows in `Cleaners.gs` as needed.
- Twilio charges per message (~1¢ US SMS); US A2P registration may be required
  depending on your account type.
