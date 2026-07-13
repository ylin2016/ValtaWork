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
 *   previewTomorrow()       — force a dry run + verbose log, never sends.
 *   listCleanerCalendars()  — print every calendar name/ID this account can see
 *                             (use it to get the exact names for Cleaners.gs).
 *
 * See README.md for setup, Twilio keys, and the daily trigger.
 */

/** Main entry point. Safe to attach to a daily time-driven trigger. */
function runDaily() {
  const day = targetDate_(CONFIG.DAYS_AHEAD);
  const label = formatDay_(day);
  Logger.log('CleaningReminder — schedule for %s', label);

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

    if (CONFIG.DRY_RUN) {
      Logger.log('\n--- DRY RUN → %s (%s) — %s job(s) ---\n%s',
        cleaner.name, cleaner.phone || '(no phone set)', jobs.length, message);
      Logger.log('   [debug] colors: %s', jobs.map(function (j) {
        return '"' + j.event.getTitle() + '"=' + (j.colorId || 'default') + (j.backToBack ? '(B2B)' : '');
      }).join(', '));
      results.push({ name: cleaner.name, jobs: jobs.length, sent: false, dryRun: true });
    } else {
      const res = sendSms_(cleaner.phone, message);
      Logger.log('%s (%s): %s', cleaner.name, cleaner.phone, res.ok ? 'SENT ✓' : 'FAILED — ' + res.error);
      results.push({ name: cleaner.name, jobs: jobs.length, sent: res.ok, error: res.error });
    }
  });

  return results;
}

/** Force a non-sending, verbose preview of tomorrow — never texts anyone. */
function previewTomorrow() {
  const saved = CONFIG.DRY_RUN;
  CONFIG.DRY_RUN = true;
  try {
    runDaily();
  } finally {
    CONFIG.DRY_RUN = saved;
  }
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

/**
 * Cleaning jobs on `cal` for the day. Returns [{ event, colorId, backToBack }].
 * Empty "(No title)" shift placeholders are skipped.
 */
function getJobs_(cal, day) {
  const jobs = [];
  cal.getEvents(day.start, day.end).forEach(function (event) {
    if (CONFIG.SKIP_UNTITLED && !event.getTitle().trim()) {
      return; // empty "(No title)" shift placeholder
    }
    const colorId = event.getColor(); // '' when the event uses the calendar default
    jobs.push({
      event: event,
      colorId: colorId,
      backToBack: CONFIG.BACKTOBACK_COLORS.indexOf(colorId) !== -1,
    });
  });
  return jobs;
}

/* ------------------------------------------------------------------ */
/* Message composition                                                */
/* ------------------------------------------------------------------ */

/**
 * Build the SMS body for one cleaner. Each job (event) is one shift whose title
 * is a comma-separated list of units; units are broken onto their own lines and
 * grouped under a shift header that flags back-to-back turnovers.
 */
function composeMessage_(name, dayLabel, jobs) {
  const sorted = jobs.slice().sort(function (a, b) {
    return a.event.getStartTime() - b.event.getStartTime();
  });

  const totalUnits = sorted.reduce(function (n, j) {
    return n + splitUnits_(j.event.getTitle()).length;
  }, 0);

  const lines = [];
  lines.push(CONFIG.BRAND + ' cleaning — ' + dayLabel +
    ' (' + totalUnits + ' unit' + (totalUnits === 1 ? '' : 's') + '):');

  sorted.forEach(function (job) {
    const event = job.event;
    lines.push('');

    let header = '';
    if (!event.isAllDayEvent()) {
      header = formatTime_(event.getStartTime()) + '–' + formatTime_(event.getEndTime());
    }
    header += (header ? '  ' : '') +
      (job.backToBack ? '⚠️ BACK-TO-BACK (finish before check-in)' : 'Not back-to-back');
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
