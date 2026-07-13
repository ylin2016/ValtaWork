/**
 * CleaningReminder — cleaners
 *
 * One entry per cleaner. Each cleaner has their OWN Google Calendar
 * (e.g. "ValtaAuto_Angelina"); that calendar is how we know the job is theirs.
 *
 *   name     — label used in logs / the message greeting.
 *   calendar — the calendar's NAME exactly as it appears in Google Calendar.
 *   phone    — E.164 format: '+1' followed by the 10-digit number, no spaces.
 *
 * If two calendars happen to share a name, use `calendarId` instead of
 * `calendar` (Calendar settings → "Integrate calendar" → Calendar ID).
 * Run `listCleanerCalendars` once to print the exact names/IDs you can access.
 */

const CLEANERS = [
  { name: 'Angelina', calendar: 'ValtaAuto_Angelina', phone: '' },
  { name: 'Anna',     calendar: 'ValtaAuto_Anna',     phone: '' },
  { name: 'Camilla',  calendar: 'ValtaAuto_Camilla',  phone: '' },
  { name: 'Crystal',  calendar: 'ValtaAuto_Crystal',  phone: '' },
  { name: 'Maria',    calendar: 'ValtaAuto_Maria',    phone: '' },
];
