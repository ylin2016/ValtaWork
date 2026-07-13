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
digits, no spaces/dashes). You can list **more than one number** per cleaner and
the same text goes to each: `phones: ['+18283311782', '+12065551234']`. Leave
`phones: []` to skip that cleaner for now.

### 4. Add Twilio credentials (secrets — not in code)
1. Create a Twilio account and an SMS-capable number.
2. Apps Script → **Project Settings → Script properties → Add**:
   | Property               | Value                                   |
   |------------------------|-----------------------------------------|
   | `TWILIO_ACCOUNT_SID`   | Account SID (starts `AC…`)              |
   | `TWILIO_AUTH_TOKEN`    | Auth Token                              |
   | `TWILIO_FROM_NUMBER`   | Twilio number, e.g. `+15125550100`      |

### 5. Check the timezone
`appsscript.json` is `America/Los_Angeles` (Pacific — matches the Seattle-area
properties and the GMT-07 grid in your calendar). This controls what counts as
"tomorrow" and the times printed in the message.

## Try it (safe — no texts sent)

`Config.gs` ships with `DRY_RUN: true`. Run **`previewTomorrow`** (first run asks
you to authorize Calendar + external-request access). Open **Executions / Logs**:
for each cleaner you'll see the **exact SMS** that would be sent — no texts go
out. To check a specific day, run **`previewDate('2026-07-16')`**. Share that log
with me and we'll tune the parsing to your real events if anything looks off.

## Go live

1. Set `DRY_RUN: false` in `Config.gs`.
2. Run **`runDaily`** once manually — real texts send. Confirm receipt.

## Automate the daily scan (when ready)

Currently manual, by design. To automate:
- Apps Script → **Triggers** (clock icon) → **Add Trigger**
  - Function `runDaily`, **Time-driven → Day timer**, pick an hour (e.g. 5–6pm so
    cleaners get tomorrow's list the evening before).

## Assumptions to confirm

- One cleaner per calendar; a **titled** event = a job; `(No title)` = ignore.
- The **five** `ValtaAuto_*` calendars above are the full cleaner list. Add/remove
  rows in `Cleaners.gs` as needed.
- Twilio charges per message (~1¢ US SMS); US A2P registration may be required
  depending on your account type.
