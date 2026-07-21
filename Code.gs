/**
 * CleaningReminder — main script
 *
 * For each cleaner (each has their own Google Calendar, plus optional extra
 * calendars like Residential / Move-in-out), scans the target day, composes one
 * message listing their units by type, and sends it as a 1:1 SMS to each of the
 * cleaner's numbers via Twilio. A weekly summary tallies the coming week by type.
 *
 * ENTRY POINTS (run from the editor's ▶ Run menu, or a time trigger):
 *   runDaily()              — daily next-day reminder. Honors CONFIG.DRY_RUN.
 *   runWeekly()             — weekly summary (CONFIG.WEEKLY_TARGET; trigger Mondays).
 *   previewTomorrow()       — dry run for tomorrow + verbose log, never sends.
 *   previewWeekly()         — dry run of what runWeekly would send, never sends.
 *   previewThisWeek()       — dry run of THIS week's summary, never sends.
 *   previewNextWeek()       — dry run of NEXT week's summary, never sends.
 *   previewDate()           — dry run for CONFIG.PREVIEW_DATE (set it, then Run).
 *   listCleanerCalendars()  — print every calendar name/ID this account can see.
 *
 * Maria's route planning lives in Routes.gs (previewMariaRoutes / runMariaRoutes).
 * See README.md for setup, Twilio keys, and the daily/weekly triggers.
 */

/** Main entry point. Safe to attach to a daily time-driven trigger. */
function runDaily() {
  return runForDay_(targetDate_(CONFIG.DAYS_AHEAD), CONFIG.DRY_RUN);
}

/** Non-sending preview of tomorrow — never texts anyone. */
function previewTomorrow() {
  return runForDay_(targetDate_(CONFIG.DAYS_AHEAD), true);
}

/**
 * Non-sending preview of a specific calendar date.
 * Run from the editor (no argument) → uses CONFIG.PREVIEW_DATE.
 * Call in code → previewDate('2026-07-16').
 */
function previewDate(dateStr) {
  return runForDay_(dayFromString_(dateStr || CONFIG.PREVIEW_DATE), true);
}

/**
 * Weekly summary (Mon–Sun) for the week set by CONFIG.WEEKLY_TARGET
 * ('upcoming' | 'this' | 'next'). Honors CONFIG.DRY_RUN. Trigger on Mondays.
 */
function runWeekly() {
  return runWeekly_(CONFIG.DRY_RUN, CONFIG.WEEKLY_TARGET);
}

/** Non-sending preview of what runWeekly would send (uses CONFIG.WEEKLY_TARGET). */
function previewWeekly() {
  return runWeekly_(true, CONFIG.WEEKLY_TARGET);
}

/** Non-sending preview of THIS week (the Mon–Sun week containing today). */
function previewThisWeek() {
  return runWeekly_(true, 'this');
}

/** Non-sending preview of NEXT week (the Mon–Sun after this one). */
function previewNextWeek() {
  return runWeekly_(true, 'next');
}

/* ------------------------------------------------------------------ */
/* Daily reminder                                                     */
/* ------------------------------------------------------------------ */

/** Build + (optionally) send each cleaner's message for one day. */
function runForDay_(day, dryRun) {
  const label = formatDay_(day);
  Logger.log('CleaningReminder — schedule for %s%s', label, dryRun ? '  (DRY RUN — no texts)' : '');

  const results = [];

  CLEANERS.forEach(function (cleaner) {
    const jobs = collectJobs_(cleaner, day);
    if (!jobs.length) {
      Logger.log('%s: no cleaning jobs.', cleaner.name);
      return;
    }

    const message = composeMessage_(cleaner.name, label, jobs);
    if (dryRun) {
      Logger.log('   [debug] jobs: %s', jobs.map(function (j) {
        return (TYPES[j.type] ? TYPES[j.type].short : j.type) + '@' + formatTime_(j.event.getStartTime());
      }).join(', '));
    }
    Array.prototype.push.apply(results, deliverMessage_(cleaner, message, dryRun, /*ccLeader=*/ true));
  });

  return results;
}

/**
 * Deliver `message` to one cleaner as a separate 1:1 SMS to EACH of their numbers,
 * and optionally CC the leader(s) a 1:1 copy too. Honors dryRun (logs only).
 * Returns an array of result records.
 */
function deliverMessage_(cleaner, message, dryRun, ccLeader) {
  const phones = cleanerPhones_(cleaner);
  const leaders = ccLeader
    ? leaderPhones_().filter(function (p) { return phones.indexOf(p) === -1; })
    : [];

  if (dryRun) {
    Logger.log('\n--- DRY RUN → %s [SMS: %s]%s ---\n%s',
      cleaner.name,
      phones.length ? phones.join(', ') : '(no phone set)',
      leaders.length ? '  +CC leader: ' + leaders.join(', ') : '',
      message);
    return [{ name: cleaner.name, recipients: phones.length, cc: leaders.length, sent: false, dryRun: true }];
  }
  if (!phones.length && !leaders.length) {
    Logger.log('%s: no phone number set — skipping.', cleaner.name);
    return [{ name: cleaner.name, sent: false, error: 'no phone' }];
  }

  const out = [];
  phones.forEach(function (phone) {
    const res = sendSms_(phone, message);
    Logger.log('%s (%s): %s', cleaner.name, phone, res.ok ? 'SENT ✓' : 'FAILED — ' + res.error);
    out.push({ name: cleaner.name, phone: phone, sent: res.ok, error: res.error });
  });
  leaders.forEach(function (phone) {
    const res = sendSms_(phone, message);
    Logger.log('%s → leader %s: %s', cleaner.name, phone, res.ok ? 'SENT ✓' : 'FAILED — ' + res.error);
    out.push({ name: cleaner.name, leader: phone, sent: res.ok, error: res.error });
  });
  return out;
}

/** Leader/dispatcher number(s) that get a 1:1 copy of every cleaner's schedule. */
function leaderPhones_() {
  const list = Array.isArray(CONFIG.LEADER_PHONES) ? CONFIG.LEADER_PHONES : [];
  const seen = {};
  const out = [];
  list.forEach(function (p) {
    p = String(p).trim();
    if (p && !seen[p]) { seen[p] = true; out.push(p); }
  });
  return out;
}

/** Normalize a cleaner's number(s) — accepts `phones: [...]` and/or `phone: ''`. */
function cleanerPhones_(cleaner) {
  const raw = [];
  if (Array.isArray(cleaner.phones)) Array.prototype.push.apply(raw, cleaner.phones);
  if (cleaner.phone) raw.push(cleaner.phone);
  return raw.map(function (s) { return String(s).trim(); })
    .filter(function (s) { return s.length; });
}

/* ------------------------------------------------------------------ */
/* Cleaning types                                                     */
/* ------------------------------------------------------------------ */

const TYPES = {
  backtoback:  { label: 'Back-to-back', short: 'B2B' },
  nextday:     { label: 'Next-day',     short: 'Next' },
  residential: { label: 'Residential',  short: 'Res' },
  moveinout:   { label: 'Move-in/out',  short: 'Move' },
};

// Order used in weekly totals / per-day lines.
const WEEKLY_TYPE_ORDER = ['backtoback', 'nextday', 'residential', 'moveinout'];

/** The types relevant to a cleaner: back-to-back + next-day, plus any extra-calendar types. */
function cleanerTypes_(cleaner) {
  const set = ['backtoback', 'nextday'];
  (cleaner.extraCalendars || []).forEach(function (x) {
    if (x.type && set.indexOf(x.type) === -1) set.push(x.type);
  });
  return WEEKLY_TYPE_ORDER.filter(function (k) { return set.indexOf(k) !== -1; });
}

/* ------------------------------------------------------------------ */
/* Calendars & jobs                                                   */
/* ------------------------------------------------------------------ */

/** Resolve a calendar spec ({calendarId} preferred, else {calendar} by name). */
function resolveCalendarSpec_(spec) {
  if (spec.calendarId) return CalendarApp.getCalendarById(spec.calendarId);
  const byName = CalendarApp.getCalendarsByName(spec.calendar);
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

/** Titled events on `cal` for the day (empty "(No title)" shift blocks skipped). */
function getTitledEvents_(cal, day) {
  return cal.getEvents(day.start, day.end).filter(function (event) {
    return !(CONFIG.SKIP_UNTITLED && !event.getTitle().trim());
  });
}

/**
 * All of a cleaner's jobs for the day as [{ event, type }], sorted by start time.
 * Main calendar → back-to-back / next-day by shift time. Each extra calendar →
 * that calendar's fixed type (residential, moveinout, …).
 */
function collectJobs_(cleaner, day) {
  const jobs = [];

  const mainCal = resolveCalendarSpec_(cleaner);
  if (mainCal) {
    getTitledEvents_(mainCal, day).forEach(function (e) {
      jobs.push({ event: e, type: isBackToBackEvent_(e) ? 'backtoback' : 'nextday' });
    });
  } else {
    Logger.log('%s: calendar "%s" not found.', cleaner.name, cleaner.calendar || cleaner.calendarId);
  }

  (cleaner.extraCalendars || []).forEach(function (extra) {
    const cal = resolveCalendarSpec_(extra);
    if (!cal) {
      Logger.log('%s: extra calendar "%s" not found.', cleaner.name, extra.calendar || extra.calendarId);
      return;
    }
    getTitledEvents_(cal, day).forEach(function (e) {
      jobs.push({ event: e, type: extra.type });
    });
  });

  jobs.sort(function (a, b) { return a.event.getStartTime() - b.event.getStartTime(); });
  return jobs;
}

/**
 * True if an event is the back-to-back (purple, morning) shift — it starts before
 * CONFIG.BACKTOBACK_BEFORE_HOUR. All-day events are treated as not back-to-back.
 */
function isBackToBackEvent_(event) {
  if (event.isAllDayEvent()) return false;
  return event.getStartTime().getHours() < CONFIG.BACKTOBACK_BEFORE_HOUR;
}

/* ------------------------------------------------------------------ */
/* Daily message composition                                          */
/* ------------------------------------------------------------------ */

/** Build the daily SMS body for one cleaner from their typed jobs. */
function composeMessage_(name, dayLabel, jobs) {
  const residential = [];
  const timed = [];
  jobs.forEach(function (j) { (j.type === 'residential' ? residential : timed).push(j); });

  const totalUnits = jobs.reduce(function (n, j) { return n + unitCount_(j); }, 0);

  const lines = [];
  lines.push(CONFIG.BRAND + ' — ' + dayLabel +
    ' (' + totalUnits + ' unit' + (totalUnits === 1 ? '' : 's') + '):');

  // Timed shift jobs (back-to-back, next-day, move-in/out): section per event.
  timed.forEach(function (job) {
    lines.push('');
    lines.push(typeHeader_(job) + ':');

    splitUnits_(job.event.getTitle()).forEach(function (unit) {
      lines.push(' • ' + unit);
    });

    const loc = job.event.getLocation();
    if (loc) lines.push('   ' + loc);

    const notes = cleanNotes_(job.event.getDescription());
    if (notes) lines.push('   Notes: ' + notes);
  });

  // Residential: one row per cleaning, contact + address only.
  if (residential.length) {
    lines.push('');
    lines.push('Residential:');
    residential.forEach(function (job) { lines.push(' • ' + residentialRow_(job)); });
  }

  lines.push('');
  lines.push('Reply here if you cannot make it. Thanks, ' + name + '!');
  return lines.join('\n');
}

/**
 * One residential row — address only. Prefer the event's structured location
 * field; if it's empty, dig the street+city out of the freeform title
 * (e.g. "Monthly : $180, 14701 SE 42nd ST, Bellevue, Isabelle WFH, 4252833210"
 * → "14701 SE 42nd ST, Bellevue"). Full title is the last-ditch fallback.
 */
function residentialRow_(job) {
  const loc = dedupeAddress_(job.event.getLocation() || '');
  if (loc) return loc;
  const title = job.event.getTitle().trim();
  return addressFromTitle_(title) || title;
}

/**
 * Pull a street address out of a comma-separated freeform title. Finds the first
 * segment that starts like a street ("<1-6 digits> <text>", which excludes
 * 10-digit phone numbers and "$180" prices) and pairs it with the following
 * segment when that looks like a city/place name. Returns '' if none found.
 */
function addressFromTitle_(title) {
  const seg = splitUnits_(title);            // comma-split + trim + drop blanks
  const streetRe = /^\d{1,6}\s+\S/;          // "14701 SE 42nd ST" yes; "4252833210 Max" no
  const placeRe = /^[A-Za-z][A-Za-z .'-]*$/; // city = the letters-only segment right after the street
  for (var i = 0; i < seg.length; i++) {
    if (streetRe.test(seg[i])) {
      const next = seg[i + 1];
      return next && placeRe.test(next) ? seg[i] + ', ' + next : seg[i];
    }
  }
  return '';
}

/** Drop duplicate comma-separated segments from a location string, preserving order. */
function dedupeAddress_(loc) {
  const seen = {};
  return String(loc).split(',').map(function (s) { return s.trim(); })
    .filter(function (s) {
      if (!s) return false;
      const k = s.toLowerCase();
      if (seen[k]) return false;
      seen[k] = true;
      return true;
    })
    .join(', ');
}

/** Cleanings represented by a job: residential = 1 per event, else one per unit. */
function unitCount_(job) {
  return job.type === 'residential' ? 1 : splitUnits_(job.event.getTitle()).length;
}

/** The section header for a job: time window plus a type tag. */
function typeHeader_(job) {
  const e = job.event;
  const window = e.isAllDayEvent()
    ? 'All day'
    : formatTime_(e.getStartTime()) + '–' + formatTime_(e.getEndTime());

  switch (job.type) {
    case 'backtoback':
      return '⚠️ BACK-TO-BACK · ' + window + ' (finish before check-in)';
    case 'nextday':
      return e.isAllDayEvent() ? 'All day' : formatTime_(e.getStartTime()) + '–' + CONFIG.NONB2B_END_LABEL;
    case 'residential':
      return 'Residential · ' + window;
    case 'moveinout':
      return 'Move-in/out · ' + window;
    default:
      return window;
  }
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
/* Weekly summary                                                     */
/* ------------------------------------------------------------------ */

/** Build + (optionally) send each cleaner's week summary for the chosen week (see weekWindow_). */
function runWeekly_(dryRun, mode) {
  const week = weekWindow_(mode);
  const weekLabel = formatDay_({ start: week.start }) + ' – ' + formatDay_({ start: week.days[6].start });
  Logger.log('CleaningReminder — weekly summary, %s%s', weekLabel, dryRun ? '  (DRY RUN — no texts)' : '');

  const results = [];
  CLEANERS.forEach(function (cleaner) {
    const tally = tallyWeek_(cleaner, week);
    if (!tally.total) {
      Logger.log('%s: no cleanings this week.', cleaner.name);
      return;
    }
    const message = composeWeeklyMessage_(cleaner.name, weekLabel, tally, cleanerTypes_(cleaner));
    // Weekly goes to each cleaner (no leader CC).
    Array.prototype.push.apply(results, deliverMessage_(cleaner, message, dryRun, /*ccLeader=*/ false));
  });

  return results;
}

/**
 * Seven day-windows for the chosen week, running MONDAY–SUNDAY. `mode`:
 *   'this'     — the Mon–Sun week that contains today.
 *   'next'     — the week after this one.
 *   'upcoming' — (default) today's week if it's Monday, else next week. This is
 *                the trigger-day behavior: a Monday run covers the week starting
 *                that day; a mid-week run looks ahead to next week.
 */
function weekWindow_(mode) {
  const now = new Date();
  const dow = now.getDay();               // 0 = Sunday … 1 = Monday … 6 = Saturday
  const sinceMon = (dow + 6) % 7;         // days since this week's Monday (Mon=0 … Sun=6)
  var daysToMon;
  if (mode === 'this') daysToMon = -sinceMon;          // Monday of the current week
  else if (mode === 'next') daysToMon = 7 - sinceMon;  // Monday of next week
  else daysToMon = (8 - dow) % 7;                       // 'upcoming': today if Monday, else next Monday
  const mon = new Date(now.getFullYear(), now.getMonth(), now.getDate() + daysToMon, 0, 0, 0);
  const days = [];
  for (var i = 0; i < 7; i++) {
    days.push({
      start: new Date(mon.getFullYear(), mon.getMonth(), mon.getDate() + i, 0, 0, 0),
      end: new Date(mon.getFullYear(), mon.getMonth(), mon.getDate() + i + 1, 0, 0, 0),
    });
  }
  return { start: days[0].start, end: days[6].end, days: days };
}

/**
 * Count cleanings (by unit) per type over the week's days for a cleaner. Returns
 * { total, totals:{type:n}, perDay:[{ start, counts:{type:n}, total }] }.
 */
function tallyWeek_(cleaner, week) {
  const totals = {};
  WEEKLY_TYPE_ORDER.forEach(function (k) { totals[k] = 0; });
  var total = 0;

  const perDay = week.days.map(function (day) {
    const counts = {};
    WEEKLY_TYPE_ORDER.forEach(function (k) { counts[k] = 0; });
    var dayTotal = 0;

    collectJobs_(cleaner, day).forEach(function (job) {
      if (!(job.type in counts)) return;
      const n = unitCount_(job);
      counts[job.type] += n;
      totals[job.type] += n;
      dayTotal += n;
      total += n;
    });

    return { start: day.start, counts: counts, total: dayTotal };
  });

  return { total: total, totals: totals, perDay: perDay };
}

/** Build the weekly summary SMS body for the cleaner's relevant `types`. */
function composeWeeklyMessage_(name, weekLabel, tally, types) {
  const lines = [];
  lines.push(CONFIG.BRAND + ' — Weekly Summary');
  lines.push(weekLabel + ' (' + tally.total + ' unit' + (tally.total === 1 ? '' : 's') + ')');

  lines.push('');
  lines.push('Totals:');
  types.forEach(function (k) { lines.push(' • ' + TYPES[k].label + ': ' + tally.totals[k]); });

  lines.push('');
  lines.push('By day:');
  tally.perDay.forEach(function (d) {
    const parts = types.filter(function (k) { return d.counts[k]; })
      .map(function (k) { return TYPES[k].short + ' ' + d.counts[k]; });
    lines.push(' ' + Utilities.formatDate(d.start, Session.getScriptTimeZone(), 'EEE MMM d') +
      ' — ' + (parts.length ? parts.join(', ') : 'none'));
  });

  lines.push('');
  lines.push('Thanks, ' + name + '!');
  return lines.join('\n');
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
/* Diagnostics                                                        */
/* ------------------------------------------------------------------ */

/** Print every calendar this account can access, with names and IDs. */
function listCleanerCalendars() {
  CalendarApp.getAllCalendars().forEach(function (c) {
    Logger.log('%s   |   %s', c.getName(), c.getId());
  });
}

/* ------------------------------------------------------------------ */
/* Twilio                                                             */
/* ------------------------------------------------------------------ */

/**
 * Twilio credentials from Script Properties, plus a ready Basic-auth header.
 * Returns null if any of the three properties is missing.
 *   TWILIO_ACCOUNT_SID, TWILIO_AUTH_TOKEN, TWILIO_FROM_NUMBER
 */
function twilioCreds_() {
  const props = PropertiesService.getScriptProperties();
  const sid = props.getProperty('TWILIO_ACCOUNT_SID');
  const token = props.getProperty('TWILIO_AUTH_TOKEN');
  const from = props.getProperty('TWILIO_FROM_NUMBER');
  if (!sid || !token || !from) return null;
  return { sid: sid, token: token, from: from, auth: 'Basic ' + Utilities.base64Encode(sid + ':' + token) };
}

/** Generic Twilio POST → { ok, data, error }. */
function twilioPost_(url, payload, c) {
  const resp = UrlFetchApp.fetch(url, {
    method: 'post',
    headers: { Authorization: c.auth },
    payload: payload,
    muteHttpExceptions: true,
  });
  const code = resp.getResponseCode();
  const data = JSON.parse(resp.getContentText() || '{}');
  if (code >= 200 && code < 300) return { ok: true, data: data };
  return { ok: false, error: 'HTTP ' + code + ': ' + (data.message || resp.getContentText()) };
}

/** Send one 1:1 SMS via Twilio. Returns { ok, error, sid }. */
function sendSms_(to, body) {
  if (!to || to.charAt(0) !== '+') {
    return { ok: false, error: 'Bad phone number (need E.164 like +15125550101): ' + to };
  }
  const c = twilioCreds_();
  if (!c) return { ok: false, error: 'Missing Twilio Script Properties (TWILIO_ACCOUNT_SID / TWILIO_AUTH_TOKEN / TWILIO_FROM_NUMBER).' };

  const res = twilioPost_('https://api.twilio.com/2010-04-01/Accounts/' + c.sid + '/Messages.json',
    { To: to, From: c.from, Body: body }, c);
  return res.ok ? { ok: true, error: '', sid: res.data.sid } : { ok: false, error: res.error };
}
