/**
 * CleaningReminder — cleaners
 *
 * One entry per cleaner. Each cleaner has their OWN Google Calendar
 * (e.g. "ValtaAuto_Angelina"); that calendar is how we know the job is theirs.
 *
 *   name     — label used in logs / the message greeting.
 *   calendar — the calendar's NAME exactly as it appears in Google Calendar.
 *   phones   — one or more numbers in E.164 format ('+1' + 10 digits, no
 *              spaces/dashes). The same message is sent to every number listed.
 *              A single `phone: '+1...'` string is also accepted.
 *
 * If two calendars happen to share a name, use `calendarId` instead of
 * `calendar` (Calendar settings → "Integrate calendar" → Calendar ID).
 * Run `listCleanerCalendars` once to print the exact names/IDs you can access.
 */

const CLEANERS = [
  { name: 'Angelina', calendar: 'ValtaAuto_Angelina', phones: [] },
  { name: 'Anna',     calendar: 'ValtaAuto_Anna',     phones: [] },
  { name: 'Camilla',  calendar: 'ValtaAuto_Camilla',  phones: [] },
  { name: 'Crystal',  calendar: 'ValtaAuto_Crystal',  phones: [] },
  { name: 'Maria',    calendar: 'ValtaAuto_Maria',    phones: ['+18283311782'] },
];

// Example — two numbers for one cleaner (both get the text):
//   { name: 'Maria', calendar: 'ValtaAuto_Maria', phones: ['+18283311782', '+12065551234'] },
