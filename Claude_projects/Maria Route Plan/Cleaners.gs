/**
 * CleaningReminder — cleaners
 *
 * One entry per cleaner. Each cleaner has their OWN Google Calendar
 * (e.g. "ValtaAuto_Angelina"); that calendar is how we know the job is theirs.
 *
 *   name     — label used in logs / the message greeting.
 *   calendar — the calendar's NAME exactly as it appears in Google Calendar.
 *   phones     — one or more numbers in E.164 format ('+1' + 10 digits, no
 *                spaces/dashes). Each number is sent its OWN 1:1 SMS copy of the
 *                cleaner's schedule. A single `phone: '+1...'` string also works.
 *   extraCalendars — (optional) additional calendars this cleaner covers, each
 *                tagged with a fixed cleaning type. Their events are added to the
 *                cleaner's daily message and weekly summary. Types:
 *                'residential', 'moveinout'. (Main-calendar events are typed
 *                automatically as 'backtoback'/'nextday' by shift time.)
 *
 * If two calendars happen to share a name, use `calendarId` instead of
 * `calendar` (Calendar settings → "Integrate calendar" → Calendar ID).
 * Run `listCleanerCalendars` once to print the exact names/IDs you can access.
 */

const CLEANERS = [
  {
    name: 'Maria', calendar: 'ValtaAuto_Maria', phones: ['+18283311782'],
    extraCalendars: [
      { calendar: 'Residential Cleaning',  type: 'residential' },
      { calendar: 'move in/out cleaning',  type: 'moveinout' },
    ],
  },
];

// Example — a cleaner with two numbers; each gets its own 1:1 SMS copy:
//   { name: 'Maria', calendar: 'ValtaAuto_Maria',
//     phones: ['+18283311782', '+12065551234'] },
