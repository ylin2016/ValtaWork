/**
 * CleaningReminder — configuration
 *
 * Secrets (Twilio SID/token/number) do NOT go here — they live in Script
 * Properties (Project Settings → Script properties). See README.md.
 */

const CONFIG = {
  // How many days ahead to scan. 1 = tomorrow (the next-day schedule).
  DAYS_AHEAD: 1,

  // SAFETY SWITCH.
  //   true  → parse events and LOG the messages that WOULD be sent (no texts).
  //   false → actually send SMS via Twilio.
  // Keep this true until the logs look right, then flip to false.
  DRY_RUN: true,

  // A calendar event is treated as a cleaning JOB only if it has a real title.
  // The empty "(No title)" 11am–4pm / 4–10pm shift blocks are ignored.
  SKIP_UNTITLED: true,

  // Google Calendar event color IDs that mean "back-to-back" (same-day turnover,
  // a new guest checks in that day → hard deadline). Purple ≈ Grape = '3'.
  // Events using the calendar's DEFAULT color report '' (empty). Run
  // previewTomorrow — it prints each event's real color id — and adjust this
  // list to match your purple events.
  BACKTOBACK_COLORS: ['3'],

  // Business name shown at the top of each text message.
  BRAND: 'Valta',
};
