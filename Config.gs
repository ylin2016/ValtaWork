/**
 * CleaningReminder — configuration
 *
 * Secrets (Twilio SID/token/number) do NOT go here — they live in Script
 * Properties (Project Settings → Script properties). See README.md.
 */

const CONFIG = {
  // How many days ahead to scan. 1 = tomorrow (the next-day schedule).
  DAYS_AHEAD: 1,

  // Date used by previewDate() when you Run it from the editor (the Run button
  // can't pass an argument). Set this, pick previewDate, and Run. Format YYYY-MM-DD.
  PREVIEW_DATE: '2026-07-16',

  // SAFETY SWITCH.
  //   true  → parse events and LOG the messages that WOULD be sent (no texts).
  //   false → actually send SMS via Twilio.
  // Keep this true until the logs look right, then flip to false.
  DRY_RUN: true,

  // Leader / dispatcher number(s) that should see EVERY cleaner's schedule. Each
  // gets a 1:1 copy of every cleaner's message (same as the cleaners themselves).
  // E.164, e.g. ['+12065550100'].
  LEADER_PHONES: [],

  // Which week runWeekly() summarizes. Weeks run MONDAY–SUNDAY.
  //   'upcoming' → today's week if it's Monday, else next week. Best for the
  //                Monday trigger: Monday covers the week starting that day, and a
  //                manual mid-week run looks ahead to next week.
  //   'this'     → always the Mon–Sun week that contains today.
  //   'next'     → always the week after this one.
  // To just LOOK without changing this, run previewThisWeek() / previewNextWeek().
  WEEKLY_TARGET: 'upcoming',

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

  // For non-back-to-back (yellow, evening) shifts there is no same-day check-in,
  // so the deadline is the next day's check-in. Show this as the end of the window
  // instead of the event's literal end time (e.g. "4:00 PM–11:00 AM (next day)").
  NONB2B_END_LABEL: '11:00 AM (next day)',

  // Business name shown at the top of each text message.
  BRAND: 'Valta Realty Cleaning Schedule',
};
