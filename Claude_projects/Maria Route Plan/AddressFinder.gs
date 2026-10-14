/**
 * CleaningReminder — find addresses for listings missing from Listings.gs
 *
 * The 2026 backtest found dispatch-card properties that have no address in
 * Listings.gs (see data/Listings_missing_2026.xlsx). Their addresses are on the
 * calendar events, so this scans the calendars and collects them.
 *
 * ENTRY POINT:
 *   findListingAddresses() — scan CONFIG.ADDRESS_SCAN's window and print the results
 *                            to the log as tab-separated tables (paste into Excel).
 *                            Read-only; needs no permissions beyond the existing ones.
 *
 * HOW A NAME IS MATCHED. Names look like "<City> <street number>[ unit]", e.g.
 * "Bellevue 14701" = 14701 SE 42nd St, Bellevue. For each event the scanner gathers
 * candidate addresses from the location field, the freeform title (same parsing as
 * the residential rows, addressFromTitle_) and the description. A candidate that
 * starts with the street number is a hit:
 *   high   — the number matches AND the candidate (or the event) names the city,
 *            or the event title contains the listing name itself;
 *   low    — only the street number matches (check it).
 * Building units ("Elektra 203", "Beachwood 5") have a unit number, not a street
 * number, so they take the building's address from Listings.gs instead.
 *
 * BEDROOMS / BATHROOMS / SQFT come from the text of the same matched events (title
 * + description: "3BR/2BA", "3 bed 2 bath", "Studio", "1,450 sqft", …). The value
 * seen most often wins; blank if no event states it.
 *
 * It also lists turnover units seen in the scanned window that Listings.gs can't
 * resolve, with whatever location their events carry.
 */

/** Names from the 2026 backtest with no address in Listings.gs. */
const MISSING_LISTINGS = [
  'Medina 2213', 'Seattle 5544', 'Clyde Hill 8830', 'Seattle 6064', 'Seattle 1717', 'Bellevue 14701',
  'Bellevue 4616', 'Bothell 14908', 'Kirkland 10219', 'Bothell 18006', 'Bellevue 12520', 'Seattle 3821',
  'Sammamish 23718', 'Bellevue 15647', 'Issaquah 1938', 'Redmond 12408', 'Bellevue 1638', 'Seattle 1512',
  'Renton 18823', 'Issaquah 2048', 'Issaquah 2450', 'Seatac 12834 Upper', 'Sammamish 21223', 'Seattle 2409',
  'Bellevue 16237', 'Seattle 1615', 'Seatac 12834 Lower', 'Bellevue 15746', 'Issaquah 917', 'Seattle 3400',
  'Mercer 3627', 'Elektra 203', 'Issaquah 20916', 'Bellevue 6153', 'Seattle 819', 'Sammamish 22525',
  'Bellevue 2403', 'Sammamish 20916', 'Kirkland 13027', 'Bellevue 634', 'Issaquah 1627', 'Seattle 4220',
  'Issaquah 1789', 'Sammamish 1316', 'Bellevue 10333', 'Seattle 7739', 'Seattle 6510', 'Kirkland 10925',
  'Bellevue 14507U3', 'Sammamish 19649', 'Beachwood 3', 'Newcastle 8321', 'Seattle 98 U503', 'Redmond 2844',
  'Edmonds 7819', 'Kirkland 13805', 'Redmond 17310', 'Redmond 10212', 'Bellevue 15306', 'Seattle 7729',
  'Seattle 1123', 'Sammamish 24698', 'Kirkland 12211', 'Seattle 3511', 'Bellevue 14507U1', 'Beachwood 8',
  'Seatac 12834', 'Bothell 3528', 'Issaquah 2437', 'Redmond 15736', 'Beachwood 5', 'Bellevue 10446',
  'Bothell 19313', 'Mercer 3627 Main', 'Beachwood 2', 'Redmond 6638',
];

/** Dispatch-card city spellings → how the city appears in an address. */
const CITY_ALIASES_ = { mercer: 'mercer island', seatac: 'seatac', 'clyde hill': 'clyde hill' };

function findListingAddresses() {
  const S = CONFIG.ADDRESS_SCAN;
  const from = dayFromString_(S.FROM).start;
  const to = S.TO ? dayFromString_(S.TO).end : new Date();
  const cals = scanCalendars_();
  Logger.log('Address scan %s → %s over %s calendar(s): %s', S.FROM, S.TO || 'today', cals.length,
    cals.map(function (c) { return c.getName(); }).join(', '));

  const targets = MISSING_LISTINGS.map(parseListingName_);
  const found = {};          // listing → { address → { n, high, last, cal, title } }
  const details = {};        // listing → { beds: {value: n}, baths: {…}, sqft: {…} }
  const unknownUnits = {};   // unit name → { n, last, location }
  var events = 0;

  cals.forEach(function (cal) {
    cal.getEvents(from, to).forEach(function (e) {
      const title = e.getTitle().trim();
      if (!title) return;
      events++;
      const ev = eventText_(e, title);
      targets.forEach(function (t) {
        if (!t.num) return;
        if (t.building) {           // building unit: only events that name it carry its details
          if (t.nameRe.test(title)) addDetails_(details, t.name, ev.details);
          return;
        }
        const hits = matchEvent_(t, ev);
        if (hits.length) addDetails_(details, t.name, ev.details);
        hits.forEach(function (hit) {
          const bucket = found[t.name] = found[t.name] || {};
          const row = bucket[hit.address] = bucket[hit.address] ||
            { n: 0, high: false, last: ev.start, cal: cal.getName(), title: title };
          row.n++;
          row.high = row.high || hit.high;
          if (ev.start > row.last) { row.last = ev.start; row.title = title; row.cal = cal.getName(); }
        });
      });
      collectUnknownUnits_(ev, unknownUnits);
    });
  });

  const rows = targets.map(function (t) { return resultRow_(t, found[t.name] || {}, details[t.name] || {}); });
  const unknownRows = Object.keys(unknownUnits).sort().map(function (u) {
    const x = unknownUnits[u];
    return [u, x.location, x.n, dateStr_(x.last)];
  });

  const filled = rows.filter(function (r) { return r[1]; }).length;
  Logger.log('Scanned %s events. Address found for %s of %s listings; %s unknown turnover unit name(s).',
    events, filled, rows.length, unknownRows.length);
  printTable_('ADDRESSES',
    ['Listing', 'Address', 'Bedrooms', 'Bathrooms', 'SqFt', 'Confidence', 'Times seen', 'Last seen', 'Calendar',
      'Other addresses found', 'Latest event title'],
    rows);
  printTable_('UNKNOWN TURNOVER UNITS', ['Unit', 'Event location', 'Times seen', 'Last seen'], unknownRows);
}

/** Log a table as tab-separated lines, so it pastes straight into Excel / Sheets. */
function printTable_(label, header, rows) {
  const clean = function (v) { return String(v == null ? '' : v).replace(/[\t\r\n]+/g, ' '); };
  const lines = [header].concat(rows).map(function (r) { return r.map(clean).join('\t'); });
  Logger.log('===== %s (%s rows) — copy the lines below =====\n%s', label, rows.length, lines.join('\n'));
}

/** Maria's calendars (main + extras), or every accessible calendar if ALL_CALENDARS. */
function scanCalendars_() {
  if (CONFIG.ADDRESS_SCAN.ALL_CALENDARS) return CalendarApp.getAllCalendars();
  const m = maria_();
  return [m].concat(m.extraCalendars || []).map(function (spec) {
    const cal = resolveCalendarSpec_(spec);
    if (!cal) Logger.log('Calendar "%s" not found — skipped.', spec.calendar || spec.calendarId);
    return cal;
  }).filter(function (c) { return c; });
}

/**
 * "Bellevue 14701" → { name, city:'bellevue', num:'14701', unit:'' }.
 * "Elektra 203" / "Beachwood 5" → building unit, with the building's address taken
 * from Listings.gs (any listing sharing the prefix).
 */
function parseListingName_(name) {
  const m = String(name).match(/^(.*?)\s+(\d+)\s*(.*)$/);
  const t = { name: name, city: '', num: '', unit: '', building: '' };
  if (!m) return t;
  t.city = m[1].toLowerCase(); t.num = m[2]; t.unit = m[3];
  // A building prefix (Elektra, Beachwood): every listing with this prefix shares one
  // address, and that address doesn't start with the listing's own number.
  const prefix = m[1] + ' ';
  const sib = LISTINGS.filter(function (r) { return r[0].indexOf(prefix) === 0 && r[1]; });
  const isBuilding = sib.length && sib.every(function (r) {
    return r[1] === sib[0][1] && r[1].indexOf((r[0].match(/\d+/) || [''])[0] + ' ') !== 0;
  });
  if (isBuilding) t.building = sib[0][1];
  t.cityRe = new RegExp('\\b' + escapeRe_(CITY_ALIASES_[t.city] || t.city) + '\\b', 'i');
  t.numRe = new RegExp('(^|[\\s,])' + t.num + '\\s+[A-Za-z0-9]');
  t.nameRe = new RegExp('(^|[\\s,(])' + escapeRe_(name) + '(?![0-9A-Za-z])', 'i');
  return t;
}

/** Everything about an event the matcher needs, computed once per event. */
function eventText_(e, title) {
  const loc = dedupeAddress_(e.getLocation() || '');
  const notes = cleanNotes_(e.getDescription() || '');
  const cands = [];
  if (loc) cands.push(loc);
  const fromTitle = addressFromTitle_(title);
  if (fromTitle) cands.push(fromTitle);
  // Description: every "<number> <street>[, city][, WA][ zip]" run.
  const re = /\b\d{1,6}\s+[A-Za-z0-9#.\- ]{2,60}(?:,\s*[A-Za-z.'\-](?:[A-Za-z .'\-]{0,28}[A-Za-z.])?){0,2}(?:,?\s*WA)?(?:\s+\d{5})?/g;
  var m;
  while ((m = re.exec(notes)) !== null) cands.push(m[0].trim());
  return { title: title, location: loc, text: (title + ' ' + loc + ' ' + notes), cands: cands, start: e.getStartTime(),
    details: roomDetails_(title + ' ' + notes) };
}

/**
 * Bedrooms / bathrooms / square feet written in an event's title or description:
 * "3BR/2BA", "3 bd 2.5 ba", "4bds", "3 bed 2 bath", "3b/2b", "Studio", "1,450 sqft",
 * "1450 sq ft", "2700sft", "1450 sf", "SF 4,554". Missing or implausible values are left out.
 */
function roomDetails_(text) {
  const d = {};
  var m;
  if (/\bstudio\b/i.test(text)) d.beds = 0;
  else if ((m = text.match(/(\d{1,2}(?:\.\d)?)\s*-?\s*(?:br|bd|bds|bdr|bdrm|beds?|bedrooms?)\b/i))) d.beds = +m[1];
  if ((m = text.match(/(\d{1,2}(?:\.\d{1,2})?)\s*-?\s*(?:ba|bth|baths?|bathrooms?)\b/i))) d.baths = +m[1];
  if ((m = text.match(/\b(\d{1,2})\s*b\s*\/\s*(\d(?:\.\d{1,2})?)\s*b\b/i))) {   // "3b/2b"
    if (d.beds == null) d.beds = +m[1];
    if (d.baths == null) d.baths = +m[2];
  }
  if ((m = text.match(/\b(\d{1,2},\d{3}|\d{3,5})\s*(?:sq\.?\s*ft\.?|sqft|sft|sf|square\s*f(?:ee|oo)t)(?![a-z])/i))) {
    d.sqft = +m[1].replace(',', '');
  } else if ((m = text.match(/\bSF\s*(\d{1,2},\d{3}|\d{3,5})\b/))) {   // "SF 4,554"
    d.sqft = +m[1].replace(',', '');
  }
  if (d.beds > 10) delete d.beds;
  if (!(d.baths > 0 && d.baths <= 10)) delete d.baths;
  if (!(d.sqft >= 200 && d.sqft <= 15000)) delete d.sqft;
  return d;
}

/** Tally one event's details under a listing. */
function addDetails_(acc, name, d) {
  const x = acc[name] = acc[name] || { beds: {}, baths: {}, sqft: {} };
  ['beds', 'baths', 'sqft'].forEach(function (k) {
    if (d[k] != null) x[k][d[k]] = (x[k][d[k]] || 0) + 1;
  });
}

/** The value seen most often in a tally ({value: count}), or ''. */
function mostCommon_(tally) {
  var best = '', n = 0;
  Object.keys(tally || {}).forEach(function (v) { if (tally[v] > n) { n = tally[v]; best = Number(v); } });
  return best;
}

/** Candidate addresses on this event that belong to listing `t`. */
function matchEvent_(t, ev) {
  const hits = [], seen = {};
  const titleNamesIt = t.nameRe.test(ev.title);
  ev.cands.forEach(function (c) {
    const startsWithNum = t.numRe.test(' ' + c);
    if (!startsWithNum && !(titleNamesIt && c === ev.location)) return;
    if (seen[c]) return;
    seen[c] = true;
    hits.push({ address: c, high: titleNamesIt || t.cityRe.test(c) || t.cityRe.test(ev.text) });
  });
  return hits;
}

/** Turnover-style units ("Name 123(2->0)") that Listings.gs can't resolve. */
function collectUnknownUnits_(ev, acc) {
  if (!/\(\s*\d+\s*-\s*>\s*\d+\s*\)/.test(ev.title)) return;   // only turnover titles carry (out->in)
  const units = splitUnits_(ev.title);
  units.forEach(function (u) {
    if (lookupListing_(u)) return;
    const name = u.replace(/\([^)]*\)\s*$/, '').trim();
    const x = acc[name] = acc[name] || { n: 0, last: ev.start, location: '' };
    x.n++;
    if (ev.start >= x.last) x.last = ev.start;
    if (ev.location && units.length === 1) x.location = ev.location;   // a multi-unit event's location isn't this unit's
  });
}

/** [Listing, Address, Beds, Baths, SqFt, Confidence, Times seen, Last seen, Calendar, Other addresses, Latest title] */
function resultRow_(t, bucket, det) {
  const rooms = [mostCommon_(det.beds), mostCommon_(det.baths), mostCommon_(det.sqft)];
  if (t.building) {
    return [t.name, t.building].concat(rooms, ['building', '', '', 'Listings.gs', '',
      'Same building as the other ' + t.name.split(' ')[0] + ' units']);
  }
  const addrs = Object.keys(bucket).sort(function (a, b) {
    const x = bucket[a], y = bucket[b];
    return (y.high - x.high) || (y.n - x.n) || (y.last - x.last);
  });
  if (!addrs.length) return [t.name, ''].concat(rooms, ['not found', 0, '', '', '', '']);
  const best = bucket[addrs[0]];
  return [t.name, addrs[0]].concat(rooms, [best.high ? 'high' : 'low', best.n, dateStr_(best.last), best.cal,
    addrs.slice(1, 4).join(' | '), best.title]);
}

function dateStr_(d) { return Utilities.formatDate(d, Session.getScriptTimeZone(), 'yyyy-MM-dd'); }
function escapeRe_(s) { return String(s).replace(/[.*+?^${}()|[\]\\]/g, '\\$&'); }
