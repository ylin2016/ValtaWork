/**
 * CleaningReminder — main script
 *
 * For each cleaner (each has their own Google Calendar), scans the target day
 * (default: tomorrow), keeps events that are real cleaning jobs (have a title;
 * the empty "(No title)" shift blocks are ignored), composes one SMS per
 * cleaner listing their units + times, and sends it via Twilio.
 *
 * ENTRY POINTS (run from the editor's ▶ Run menu, or a time trigger):
 *   runDaily()              — the real job. Honors CONFIG.DRY_RUN.
 *   previewTomorrow()       — dry run for tomorrow + verbose log, never sends.
 *   previewDate('2026-07-16') — dry run for a specific date, never sends.
 *                               Handy for checking a day that has purple events.
 *   listCleanerCalendars()  — print every calendar name/ID this account can see
 *                             (use it to get the exact names for Cleaners.gs).
 *
 * See README.md for setup, Twilio keys, and the daily trigger.
 */

/** Main entry point. Safe to attach to a daily time-driven trigger. */
function runDaily() {
  return runForDay_(targetDate_(CONFIG.DAYS_AHEAD), CONFIG.DRY_RUN);
}

/** Non-sending preview of tomorrow — never texts anyone. */
function previewTomorrow() {
  return runForDay_(targetDate_(CONFIG.DAYS_AHEAD), true);
}

/** Non-sending preview of a specific calendar date 'YYYY-MM-DD'. */
function previewDate(dateStr) {
  return runForDay_(dayFromString_(dateStr), true);
}

/** Build + (optionally) send each cleaner's message for one day. */
function runForDay_(day, dryRun) {
  const label = formatDay_(day);
  Logger.log('CleaningReminder — schedule for %s%s', label, dryRun ? '  (DRY RUN — no texts)' : '');

  const results = [];

  CLEANERS.forEach(function (cleaner) {
    const cal = resolveCalendar_(cleaner);
    if (!cal) {
      Logger.log('%s: calendar "%s" not found — skipping.', cleaner.name, cleaner.calendar || cleaner.calendarId);
      return;
    }

    const jobs = getJobs_(cal, day);
    if (!jobs.length) {
      Logger.log('%s: no cleaning jobs.', cleaner.name);
      return;
    }

    const message = composeMessage_(cleaner.name, label, jobs);
    const phones = cleanerPhones_(cleaner);

    if (dryRun) {
      Logger.log('\n--- DRY RUN → %s (%s) — %s event(s) ---\n%s',
        cleaner.name, phones.length ? phones.join(', ') : '(no phone set)', jobs.length, message);
      Logger.log('   [debug] shifts: %s', jobs.map(function (e) {
        return formatTime_(e.getStartTime()) + (isBackToBackEvent_(e) ? ' B2B' : '');
      }).join(', '));
      results.push({ name: cleaner.name, events: jobs.length, recipients: phones.length, sent: false, dryRun: true });
    } else if (!phones.length) {
      Logger.log('%s: no phone number set — skipping.', cleaner.name);
      results.push({ name: cleaner.name, events: jobs.length, sent: false, error: 'no phone' });
    } else {
      phones.forEach(function (phone) {
        const res = sendSms_(phone, message);
        Logger.log('%s (%s): %s', cleaner.name, phone, res.ok ? 'SENT ✓' : 'FAILED — ' + res.error);
        results.push({ name: cleaner.name, phone: phone, events: jobs.length, sent: res.ok, error: res.error });
      });
    }
  });

  return results;
}

/** Normalize a cleaner's number(s) — accepts `phones: [...]` and/or `phone: ''`. */
function cleanerPhones_(cleaner) {
  const raw = [];
  if (Array.isArray(cleaner.phones)) Array.prototype.push.apply(raw, cleaner.phones);
  if (cleaner.phone) raw.push(cleaner.phone);
  return raw.map(function (s) { return String(s).trim(); })
    .filter(function (s) { return s.length; });
}

/** Print every calendar this account can access, with names and IDs. */
function listCleanerCalendars() {
  CalendarApp.getAllCalendars().forEach(function (c) {
    Logger.log('%s   |   %s', c.getName(), c.getId());
  });
}

/* ------------------------------------------------------------------ */
/* Calendars & jobs                                                   */
/* ------------------------------------------------------------------ */

/** Resolve a cleaner's calendar by id (preferred) or by name. */
function resolveCalendar_(cleaner) {
  if (cleaner.calendarId) {
    return CalendarApp.getCalendarById(cleaner.calendarId);
  }
  const byName = CalendarApp.getCalendarsByName(cleaner.calendar);
  return byName && byName.length ? byName[0] : null;
}

/** Midnight-to-midnight window for the day `daysAhead` from now. */
function targetDate_(daysAhead) {
  const now = new Date();
  const start = new Date(now.getFullYear(), now.getMonth(), now.getDate() + daysAhead, 0, 0, 0);
  const end = new Date(now.getFullYear(), now.getMonth(), now.getDate() + daysAhead + 1, 0, 0, 0);
  return { start: start, end: end };
}

/** Midnight-to-midnight window for a specific date string 'YYYY-MM-DD'. */
function dayFromString_(dateStr) {
  const p = String(dateStr).split('-');
  const y = +p[0], m = +p[1] - 1, d = +p[2];
  return { start: new Date(y, m, d, 0, 0, 0), end: new Date(y, m, d + 1, 0, 0, 0) };
}

/**
 * Cleaning job events on `cal` for the day (titled events; empty "(No title)"
 * shift placeholders are skipped).
 */
function getJobs_(cal, day) {
  return cal.getEvents(day.start, day.end).filter(function (event) {
    return !(CONFIG.SKIP_UNTITLED && !event.getTitle().trim());
  });
}

/* ------------------------------------------------------------------ */
/* Message composition                                                */
/* ------------------------------------------------------------------ */

/**
 * Build the SMS body for one cleaner. Each event is one shift whose title is a
 * comma-separated list of units; units are broken onto their own lines under a
 * time header. A back-to-back shift (the purple 11am–4pm turn) is tagged so the
 * cleaner knows those units must be finished before same-day check-in.
 */
function composeMessage_(name, dayLabel, events) {
  const sorted = events.slice().sort(function (a, b) {
    return a.getStartTime() - b.getStartTime();
  });

  const totalUnits = sorted.reduce(function (n, e) {
    return n + splitUnits_(e.getTitle()).length;
  }, 0);

  const lines = [];
  lines.push(CONFIG.BRAND + ' — ' + dayLabel +
    ' (' + totalUnits + ' unit' + (totalUnits === 1 ? '' : 's') + '):');

  sorted.forEach(function (event) {
    lines.push('');

    let header = event.isAllDayEvent()
      ? 'All day'
      : formatTime_(event.getStartTime()) + '–' + formatTime_(event.getEndTime());
    if (isBackToBackEvent_(event)) {
      header = '⚠️ BACK-TO-BACK · ' + header + ' (finish before check-in)';
    }
    lines.push(header + ':');

    splitUnits_(event.getTitle()).forEach(function (unit) {
      lines.push(' • ' + unit);
    });

    const loc = event.getLocation();
    if (loc) lines.push('   ' + loc);

    const notes = cleanNotes_(event.getDescription());
    if (notes) lines.push('   Notes: ' + notes);
  });

  lines.push('');
  lines.push('Reply here if you cannot make it. Thanks, ' + name + '!');
  return lines.join('\n');
}

/** Split a multi-unit event title on commas into trimmed unit strings. */
function splitUnits_(title) {
  return title.split(',').map(function (s) { return s.trim(); })
    .filter(function (s) { return s.length; });
}

/**
 * True if an event is the back-to-back (purple, morning) shift — it starts before
 * CONFIG.BACKTOBACK_BEFORE_HOUR. All-day events are treated as not back-to-back.
 */
function isBackToBackEvent_(event) {
  if (event.isAllDayEvent()) return false;
  return event.getStartTime().getHours() < CONFIG.BACKTOBACK_BEFORE_HOUR;
}

/** Strip HTML and collapse whitespace from a calendar description. */
function cleanNotes_(desc) {
  if (!desc) return '';
  return desc.replace(/<[^>]*>/g, ' ').replace(/\s+/g, ' ').trim();
}

/* ------------------------------------------------------------------ */
/* Date/time formatting                                               */
/* ------------------------------------------------------------------ */

function formatDay_(day) {
  return Utilities.formatDate(day.start, Session.getScriptTimeZone(), 'EEE, MMM d');
}

function formatTime_(d) {
  return Utilities.formatDate(d, Session.getScriptTimeZone(), 'h:mm a');
}

/* ------------------------------------------------------------------ */
/* Twilio                                                             */
/* ------------------------------------------------------------------ */

/**
 * Send one SMS via Twilio. Returns { ok: bool, error: string, sid: string }.
 * Credentials come from Script Properties:
 *   TWILIO_ACCOUNT_SID, TWILIO_AUTH_TOKEN, TWILIO_FROM_NUMBER
 */
function sendSms_(to, body) {
  if (!to || to.charAt(0) !== '+') {
    return { ok: false, error: 'Bad phone number (need E.164 like +15125550101): ' + to };
  }

  const props = PropertiesService.getScriptProperties();
  const sid = props.getProperty('TWILIO_ACCOUNT_SID');
  const token = props.getProperty('TWILIO_AUTH_TOKEN');
  const from = props.getProperty('TWILIO_FROM_NUMBER');

  if (!sid || !token || !from) {
    return { ok: false, error: 'Missing Twilio Script Properties (TWILIO_ACCOUNT_SID / TWILIO_AUTH_TOKEN / TWILIO_FROM_NUMBER).' };
  }

  const url = 'https://api.twilio.com/2010-04-01/Accounts/' + sid + '/Messages.json';
  const options = {
    method: 'post',
    headers: { Authorization: 'Basic ' + Utilities.base64Encode(sid + ':' + token) },
    payload: { To: to, From: from, Body: body },
    muteHttpExceptions: true,
  };

  const resp = UrlFetchApp.fetch(url, options);
  const code = resp.getResponseCode();
  const data = JSON.parse(resp.getContentText() || '{}');

  if (code >= 200 && code < 300) {
    return { ok: true, error: '', sid: data.sid };
  }
  return { ok: false, error: 'HTTP ' + code + ': ' + (data.message || resp.getContentText()) };
}
