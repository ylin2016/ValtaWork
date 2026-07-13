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

  // BACK-TO-BACK is a property of the EVENT (the shift), not the unit. The purple
  // morning shift (11am–4pm) is the back-to-back turn — a new guest checks in that
  // day, so it must finish before arrival. The yellow evening shift (4pm–10pm) is
  // not. We classify by the event's START hour (24h, calendar timezone): an event
  // that starts before this hour is back-to-back. 15 (3pm) cleanly splits the
  // 11am shift from the 4pm shift.
  BACKTOBACK_BEFORE_HOUR: 15,

  // Business name shown at the top of each text message.
  BRAND: 'Valta Realty Cleaning Schedule',
};
