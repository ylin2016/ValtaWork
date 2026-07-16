/**
 * CleaningReminder — cleaners
 *
 * One entry per cleaner. Each cleaner has their OWN Google Calendar
 * (e.g. "ValtaAuto_Angelina"); that calendar is how we know the job is theirs.
 *
 *   name     — label used in logs / the message greeting.
 *   calendar — the calendar's NAME exactly as it appears in Google Calendar.
 *   phones     — one or more numbers in E.164 format ('+1' + 10 digits, no
 *                spaces/dashes). With group messaging these are the group's
 *                members (plus CONFIG.LEADER_PHONES). A single `phone: '+1...'`
 *                string is also accepted.
 *   fromNumber — (optional) the Twilio MMS number this cleaner's group sends
 *                from. Give each cleaner a DIFFERENT one if the leader must be in
 *                every group (a number can only be in one group per sending
 *                number). Omit to use the default TWILIO_FROM_NUMBER.
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
  { name: 'Angelina', calendar: 'ValtaAuto_Angelina', phones: [], fromNumber: '' },
  { name: 'Anna',     calendar: 'ValtaAuto_Anna',     phones: [], fromNumber: '' },
  { name: 'Camilla',  calendar: 'ValtaAuto_Camilla',  phones: [], fromNumber: '' },
  { name: 'Crystal',  calendar: 'ValtaAuto_Crystal',  phones: [], fromNumber: '' },
  {
    name: 'Maria', calendar: 'ValtaAuto_Maria', phones: ['+18283311782'], fromNumber: '',
    extraCalendars: [
      { calendar: 'Residential Cleaning',  type: 'residential' },
      { calendar: 'move in/out cleaning',  type: 'moveinout' },
    ],
  },
];

// Example — a cleaner's own two numbers, its own Twilio sending number, and the
// leader (CONFIG.LEADER_PHONES) is added to every group automatically:
//   { name: 'Maria', calendar: 'ValtaAuto_Maria',
//     phones: ['+18283311782', '+12065551234'], fromNumber: '+12065550111' },
