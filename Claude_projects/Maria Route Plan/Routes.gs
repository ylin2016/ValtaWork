/**
 * CleaningReminder — route planning (Maria)
 *
 * Builds a per-day route plan: each cleaning becomes a geocoded stop, stops are
 * packed into 1–MAX_CARS crews under the unit + 8-hour limits, and each crew's
 * stops are ordered by shortest travel from the base and back. Back-to-back jobs
 * are placed first (done first in every car, spread across crews, never left over
 * capacity — see planCars_). See CONFIG.ROUTING.
 *
 * ENTRY POINTS:
 *   previewMariaRoutes()      — dry-run route plan for tomorrow (never sends).
 *   previewMariaRoutesDate()  — dry-run for CONFIG.PREVIEW_DATE.
 *   runMariaRoutes()          — build + send (honors CONFIG.DRY_RUN). Trigger daily.
 *   checkListings()           — log which of tomorrow's units resolve to an address.
 *
 * Travel uses REAL driving time/distance from the built-in Maps DirectionFinder
 * (no API key), cached per address-pair in Script Properties (repeats are free); it
 * falls back to straight-line × ROAD_FACTOR ÷ AVG_SPEED_MPH if directions are
 * unavailable. Addresses are geocoded once (also cached) via the same Maps service.
 */

function previewMariaRoutes() {
  return runRoutesForCleaner_(maria_(), targetDate_(CONFIG.DAYS_AHEAD), true);
}

function previewMariaRoutesDate() {
  return runRoutesForCleaner_(maria_(), dayFromString_(CONFIG.PREVIEW_DATE), true);
}

function runMariaRoutes() {
  return runRoutesForCleaner_(maria_(), targetDate_(CONFIG.DAYS_AHEAD), CONFIG.DRY_RUN);
}

/** The cleaner named "Maria" (the one whose calendars get route planning). */
function maria_() {
  const m = CLEANERS.filter(function (c) { return c.name === 'Maria'; })[0];
  if (!m) throw new Error('No cleaner named "Maria" in Cleaners.gs');
  return m;
}

/* ------------------------------------------------------------------ */
/* Orchestration                                                      */
/* ------------------------------------------------------------------ */

function runRoutesForCleaner_(cleaner, day, dryRun) {
  const dayLabel = formatDay_(day);
  Logger.log('CleaningReminder — %s route plan for %s%s', cleaner.name, dayLabel,
    dryRun ? '  (DRY RUN — no texts)' : '');

  const built = buildStops_(cleaner, day);
  if (!built.stops.length && !built.unmapped.length) {
    Logger.log('%s: no cleanings to route.', cleaner.name);
    return [{ name: cleaner.name, sent: false, error: 'no jobs' }];
  }

  const base = CONFIG.ROUTING.BASE_ADDRESS ? geocode_(CONFIG.ROUTING.BASE_ADDRESS) : null;
  const geocoded = [], ungeocoded = [];
  built.stops.forEach(function (s) {
    s.coord = geocode_(s.address);
    (s.coord ? geocoded : ungeocoded).push(s);
  });

  const plan = planCars_(base, geocoded);
  const message = composeRoutePlan_(cleaner.name, dayLabel, plan, built.counts,
    built.unmapped, ungeocoded, !!base);

  return deliverMessage_(cleaner, message, dryRun, /*ccLeader=*/ true);
}

/* ------------------------------------------------------------------ */
/* Stops                                                              */
/* ------------------------------------------------------------------ */

/**
 * Turn a cleaner's jobs for the day into routable stops. Turnover events split
 * into one stop per unit (each unit can be a different building); residential /
 * move-in-out are one stop per event (address only, fixed time).
 * Returns { stops:[{name,address,type,beds,baths,cleanMin}], counts, unmapped:[unit] }.
 */
function buildStops_(cleaner, day) {
  const R = CONFIG.ROUTING;
  const stops = [];
  const unmapped = [];
  const counts = { backtoback: 0, nextday: 0, residential: 0, moveinout: 0 };

  collectJobs_(cleaner, day).forEach(function (job) {
    if (job.type === 'residential' || job.type === 'moveinout') {
      counts[job.type] += 1;
      const addr = residentialRow_(job); // address-only string
      stops.push({
        name: shortAddr_(addr), address: addr, type: job.type, beds: null, baths: null,
        cleanMin: job.type === 'residential' ? R.RESIDENTIAL_MIN : R.MOVEINOUT_MIN,
      });
      return;
    }
    // turnover event → one stop per unit
    splitUnits_(job.event.getTitle()).forEach(function (unit) {
      counts[job.type] += 1;
      const info = lookupListing_(unit);
      if (!info || !info.address) { unmapped.push(unit); return; }
      stops.push({
        name: info.name, address: info.address, type: job.type,
        beds: info.beds, baths: info.baths,
        cleanMin: R.CLEAN_BASE_MIN + info.beds * R.CLEAN_PER_BEDROOM_MIN + info.baths * R.CLEAN_PER_BATHROOM_MIN,
      });
    });
  });

  return { stops: stops, counts: counts, unmapped: unmapped };
}

/* ------------------------------------------------------------------ */
/* Geocoding + distance                                               */
/* ------------------------------------------------------------------ */

/** Geocode an address to { lat, lng }, cached in Script Properties. null on failure. */
function geocode_(address) {
  if (!address) return null;
  const props = PropertiesService.getScriptProperties();
  const key = 'GEO:' + String(address).slice(0, 240);
  const cached = props.getProperty(key);
  if (cached) { try { return JSON.parse(cached); } catch (e) { /* refetch */ } }
  try {
    const res = Maps.newGeocoder().geocode(address);
    if (res && res.status === 'OK' && res.results.length) {
      const loc = res.results[0].geometry.location;
      const coord = { lat: loc.lat, lng: loc.lng };
      props.setProperty(key, JSON.stringify(coord));
      return coord;
    }
    Logger.log('geocode "%s": %s', address, res && res.status);
  } catch (e) {
    Logger.log('geocode "%s" error: %s', address, e.message);
  }
  return null;
}

/** Straight-line miles between two { lat, lng } points (haversine). Fallback only. */
function milesBetween_(a, b) {
  if (!a || !b) return 0;
  const R = 3958.8;
  const rad = function (d) { return d * Math.PI / 180; };
  const dLat = rad(b.lat - a.lat), dLng = rad(b.lng - a.lng);
  const h = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
    Math.cos(rad(a.lat)) * Math.cos(rad(b.lat)) * Math.sin(dLng / 2) * Math.sin(dLng / 2);
  return 2 * R * Math.asin(Math.min(1, Math.sqrt(h)));
}

// Per-execution memo of driving legs (backed by Script Properties for reuse across runs).
var DRIVE_CACHE_ = {};

/**
 * Real driving { min, miles } from point a to b via the built-in Maps DirectionFinder
 * (no API key). Cached per leg in memory + Script Properties, so a repeated address
 * pair is free. Same point → 0. Falls back to straight-line × ROAD_FACTOR if the Maps
 * service is unavailable or returns no route (also lets the model run outside Apps Script).
 */
function legDrive_(a, b) {
  if (!a || !b) return { min: 0, miles: 0 };
  if (a.lat === b.lat && a.lng === b.lng) return { min: 0, miles: 0 };
  const key = 'DRV:' + a.lat.toFixed(5) + ',' + a.lng.toFixed(5) + '>' + b.lat.toFixed(5) + ',' + b.lng.toFixed(5);
  if (DRIVE_CACHE_[key]) return DRIVE_CACHE_[key];
  try {
    const props = PropertiesService.getScriptProperties();
    const cached = props.getProperty(key);
    if (cached) return (DRIVE_CACHE_[key] = JSON.parse(cached));
    const res = Maps.newDirectionFinder()
      .setOrigin(a.lat, a.lng).setDestination(b.lat, b.lng)
      .setMode(Maps.DirectionFinder.Mode.DRIVING).getDirections();
    if (res && res.routes && res.routes.length) {
      const leg = res.routes[0].legs[0];
      const out = { min: leg.duration.value / 60, miles: leg.distance.value / 1609.344 };
      props.setProperty(key, JSON.stringify(out));
      return (DRIVE_CACHE_[key] = out);
    }
  } catch (e) { /* Maps/Properties unavailable or no route → straight-line fallback below */ }
  const miles = milesBetween_(a, b) * CONFIG.ROUTING.ROAD_FACTOR;
  return (DRIVE_CACHE_[key] = { min: miles / CONFIG.ROUTING.AVG_SPEED_MPH * 60, miles: miles });
}

/** Real driving minutes between two points. */
function driveMin_(a, b) { return legDrive_(a, b).min; }

/** Real driving miles between two points. */
function driveMiles_(a, b) { return legDrive_(a, b).miles; }

/** Drive { min, miles } for base → s1 → … → sn → base (base legs skipped if base is
 *  null). One walk; each leg's min and miles come together from legDrive_. */
function routeDrive_(base, stops) {
  var min = 0, miles = 0, prev = base, leg;
  for (var i = 0; i < stops.length; i++) {
    if (prev) { leg = legDrive_(prev, stops[i].coord); min += leg.min; miles += leg.miles; }
    prev = stops[i].coord;
  }
  if (base && prev) { leg = legDrive_(prev, base); min += leg.min; miles += leg.miles; }
  return { min: min, miles: miles };
}

/** Drive minutes for base → s1 → … → sn → base. */
function routeDriveMin_(base, stops) { return routeDrive_(base, stops).min; }

/** Drive miles for base → s1 → … → sn → base. */
function routeDriveMiles_(base, stops) { return routeDrive_(base, stops).miles; }

/* ------------------------------------------------------------------ */
/* Car packing (greedy nearest-fit)                                   */
/* ------------------------------------------------------------------ */

/**
 * Labor content of a stop in person-minutes. Cleaning times in CONFIG.ROUTING are
 * clock-minutes for a CALIBRATION_CREW-person crew; labor = time × that crew, and a
 * car of N people cleans it in labor ÷ N clock-minutes. So a 1BR/1BA (60 min @ 2)
 * is 120 person-min → 60 min at 2, 40 at 3, 30 at 4.
 */
function stopLaborMin_(s) { return s.cleanMin * CONFIG.ROUTING.CALIBRATION_CREW; }

/** Clock-minutes to clean a whole route with `crew` people (drive not included). */
function cleanClockMin_(route, crew) {
  var labor = 0;
  for (var i = 0; i < route.length; i++) labor += stopLaborMin_(route[i]);
  return labor / crew;
}

/** Fewest people in [MIN,MAX]_CREW_PER_CAR that finish the route within the day; MAX if none. */
function minCrewFor_(base, route) {
  const R = CONFIG.ROUTING, cap = R.MAX_HOURS_PER_PERSON * 60, drive = routeDriveMin_(base, route);
  for (var c = R.MIN_CREW_PER_CAR; c <= R.MAX_CREW_PER_CAR; c++) {
    if (cleanClockMin_(route, c) + drive <= cap) return c;
  }
  return R.MAX_CREW_PER_CAR;
}

/**
 * Pack geocoded stops into ≤ MAX_CARS crews under MAX_UNITS_PER_CAR and
 * MAX_HOURS_PER_PERSON (clean + travel incl. return to base). Rules:
 *   1. Units at the same ADDRESS ride together — all become one cluster assigned to
 *      a SINGLE car, never split (e.g. every Elektra unit at 1400 Hubbell Pl). If
 *      that cluster alone exceeds the unit or hour caps, it still stays on one car
 *      (caps overridden). Unique-address stops are a cluster of one.
 *   2. Car count is driven by the day's WORKLOAD — the fewest cars (up to MAX_CARS)
 *      that hold the units and labor at MAX_CREW_PER_CAR. Each car is then staffed
 *      with the FEWEST people in [MIN,MAX]_CREW_PER_CAR that finish within the day
 *      (more people = proportionally faster; a forced over-cap building gets MAX).
 *   3. Back-to-back (B2B) jobs are done FIRST in every car and spread across the
 *      crews, and are NEVER left in `overflow` — only non-B2B can. A B2B that still
 *      can't be placed within caps is flagged in `b2bOverflow` (needs another crew).
 * Returns { cars, overflow, b2bOverflow }.
 */
function planCars_(base, stops) {
  const R = CONFIG.ROUTING;
  const capMin = R.MAX_HOURS_PER_PERSON * 60;

  const clusters = clusterByBuilding_(stops);

  // Car count from the workload: the fewest cars that hold the day's units and its
  // labor at full (MAX) crews, capped at MAX_CARS. Cars can still grow below.
  const totalLabor = cleanClockMin_(stops, 1);   // labor ÷ 1 crew = total person-minutes
  const laborBound = Math.ceil(totalLabor / (R.MAX_CREW_PER_CAR * capMin));
  const unitBound = Math.ceil(stops.length / R.MAX_UNITS_PER_CAR);
  const nCars = Math.min(R.MAX_CARS, Math.max(1, laborBound, unitBound));
  const cars = [];
  for (var i = 0; i < nCars; i++) cars.push({ clusters: [] });

  // Pass 0 — oversized single-building clusters: each stays together on ONE car,
  // overriding the unit/hour caps. Prefer an empty car, else open one, else pile on.
  clusters.forEach(function (cl) {
    if (withinCaps_([cl], base)) return;         // not oversized — handled in pass 1/2
    var idx = emptyCarIndex_(cars);
    if (idx === -1 && cars.length < R.MAX_CARS) { cars.push({ clusters: [] }); idx = cars.length - 1; }
    if (idx === -1) idx = leastLoadedCarIndex_(cars);
    cars[idx].clusters.push(cl);
    cl.placed = true;
  });

  // Pass 1 — B2B clusters, spread across crews (fewest units, tie-broken by nearest).
  // Grow up to MAX_CARS; a B2B cluster that still can't fit is flagged (never dropped
  // into the normal overflow).
  const b2bOverflow = [];
  placeClusters_(clusters.filter(function (cl) { return cl.hasB2B; }), cars, base, b2bOverflow);

  // Pass 2 — remaining (non-B2B) clusters fill leftover capacity; if none fits, the
  // cluster's stops go to overflow.
  const overflow = [];
  placeClusters_(clusters, cars, base, overflow);

  const summarized = cars.filter(function (car) { return car.clusters.length; })
    .map(function (car) {
      const route = optimizeRoute_(base, orderRoute_(base, flattenClusters_(car.clusters)));
      return summarizeRoute_(base, route, minCrewFor_(base, route));
    });
  return { cars: summarized, overflow: overflow, b2bOverflow: b2bOverflow };
}

/**
 * Group stops that share an ADDRESS into clusters. All units at one address (e.g.
 * every Elektra unit at 1400 Hubbell Pl) become ONE cluster that must ride in a
 * single car — never split. Stops with a unique address are a cluster of one.
 */
function clusterByBuilding_(stops) {
  const map = {}, order = [];
  stops.forEach(function (s, idx) {
    const key = addressKey_(s) || ('solo:' + idx);   // no address → its own cluster
    if (!map[key]) { map[key] = { key: key, stops: [], coord: s.coord, hasB2B: false }; order.push(key); }
    map[key].stops.push(s);
    if (s.type === 'backtoback') map[key].hasB2B = true;
  });
  return order.map(function (k) { return map[k]; });
}

/** Normalized address key for a stop (units in one building share an address), or ''. */
function addressKey_(s) { return String(s.address || '').trim().toLowerCase(); }

/** Flatten a list of clusters back into their stops. */
function flattenClusters_(clusters) {
  const out = [];
  clusters.forEach(function (cl) { for (var i = 0; i < cl.stops.length; i++) out.push(cl.stops[i]); });
  return out;
}

/** Would these clusters fit one car within the unit + hour caps (at the fastest crew)? */
function withinCaps_(clusters, base) {
  const R = CONFIG.ROUTING, cap = R.MAX_HOURS_PER_PERSON * 60;
  const stops = flattenClusters_(clusters);
  if (stops.length > R.MAX_UNITS_PER_CAR) return false;
  const route = orderRoute_(base, stops);
  return cleanClockMin_(route, R.MAX_CREW_PER_CAR) + routeDriveMin_(base, route) <= cap;
}

/** Place each not-yet-placed cluster into a car via assignCluster_; a cluster that
 *  fits nowhere spills its stops into `sink`. Marks every cluster it touches placed.
 *  Feed it a hasB2B-filtered list for Pass 1, the whole list for Pass 2. */
function placeClusters_(clusters, cars, base, sink) {
  clusters.forEach(function (cl) {
    if (cl.placed) return;
    var idx = assignCluster_(cl, cars, base);
    if (idx === -1) { pushStops_(sink, cl); cl.placed = true; return; }
    cars[idx].clusters.push(cl);
    cl.placed = true;
  });
}

/** Place a cluster on the fittable car with the fewest units (tie: nearest); open a
 *  new car up to MAX_CARS if needed. Returns the car index, or -1 if none can take it. */
function assignCluster_(cl, cars, base) {
  var pick = -1, fewest = Infinity, bestMiles = Infinity;
  cars.forEach(function (car, i) {
    if (!withinCaps_(car.clusters.concat([cl]), base)) return;
    const stops = flattenClusters_(car.clusters);
    const last = stops.length ? orderRoute_(base, stops).slice(-1)[0].coord : base;
    const mins = last ? driveMin_(last, cl.coord) : 0;
    if (stops.length < fewest || (stops.length === fewest && mins < bestMiles)) {
      fewest = stops.length; bestMiles = mins; pick = i;
    }
  });
  if (pick === -1 && cars.length < CONFIG.ROUTING.MAX_CARS) {
    cars.push({ clusters: [] });
    if (withinCaps_([cl], base)) pick = cars.length - 1;
  }
  return pick;
}

/** Index of the first empty car, or -1. */
function emptyCarIndex_(cars) {
  for (var i = 0; i < cars.length; i++) if (!cars[i].clusters.length) return i;
  return -1;
}

/** Index of the car with the fewest units so far. */
function leastLoadedCarIndex_(cars) {
  var idx = 0, fewest = Infinity;
  cars.forEach(function (car, i) {
    const u = flattenClusters_(car.clusters).length;
    if (u < fewest) { fewest = u; idx = i; }
  });
  return idx;
}

function pushStops_(sink, cl) { for (var i = 0; i < cl.stops.length; i++) sink.push(cl.stops[i]); }

/** Order a car's stops: all B2B first (nearest-neighbor), then the rest (nearest-
 *  neighbor). Same-building units share coordinates, so they stay grouped. */
function orderRoute_(base, stops) {
  const b2b = stops.filter(function (s) { return s.type === 'backtoback'; });
  const rest = stops.filter(function (s) { return s.type !== 'backtoback'; });
  const head = orderByNearest_(base, b2b);
  const from = head.length ? head[head.length - 1].coord : base;
  return head.concat(orderByNearest_(from, rest));
}

/** Greedy nearest-neighbor ordering of stops starting from `from` (by drive time). */
function orderByNearest_(from, stops) {
  const remaining = stops.slice(), ordered = [];
  var cur = from;
  while (remaining.length) {
    var best = 0, bestMin = Infinity;
    for (var i = 0; i < remaining.length; i++) {
      const mins = cur ? driveMin_(cur, remaining[i].coord) : 0;
      if (mins < bestMin) { bestMin = mins; best = i; }
    }
    const next = remaining.splice(best, 1)[0];
    ordered.push(next);
    cur = next.coord;
  }
  return ordered;
}

/**
 * Shorten a route with 2-opt (reverse segments while the total base→…→base DRIVE
 * TIME drops), so it stops zig-zagging between areas — e.g. it won't cross Lake
 * Washington twice when once will do. B2B-first is preserved: reversals never
 * straddle the B2B/non-B2B boundary, so every B2B still precedes every other stop.
 */
function optimizeRoute_(base, route) {
  if (route.length < 3) return route;
  var boundary = 0;
  for (var i = 0; i < route.length; i++) { if (route[i].type === 'backtoback') boundary++; }
  var best = route.slice(), bestMin = routeDriveMin_(base, best), improved = true;
  while (improved) {
    improved = false;
    for (var a = 0; a < best.length - 1; a++) {
      for (var b = a + 1; b < best.length; b++) {
        if (a < boundary && b >= boundary) continue;   // don't reverse across the B2B boundary
        const cand = best.slice(0, a).concat(best.slice(a, b + 1).reverse(), best.slice(b + 1));
        const candMin = routeDriveMin_(base, cand);
        if (candMin + 1e-9 < bestMin) { best = cand; bestMin = candMin; improved = true; }
      }
    }
  }
  return best;
}

function summarizeRoute_(base, route, crew) {
  const drive = routeDrive_(base, route);
  const cleanMin = cleanClockMin_(route, crew);
  const totalMin = drive.min + cleanMin;
  return {
    stops: route, units: route.length, crew: crew,
    driveMin: drive.min, cleanMin: cleanMin, totalMin: totalMin,
    driveMiles: drive.miles,
    overDay: totalMin > CONFIG.ROUTING.MAX_HOURS_PER_PERSON * 60 + 0.5,
  };
}

/* ------------------------------------------------------------------ */
/* Message                                                            */
/* ------------------------------------------------------------------ */

function composeRoutePlan_(name, dayLabel, plan, counts, unmapped, ungeocoded, baseSet) {
  const lines = [];
  const b2bOver = plan.b2bOverflow || [];
  const totalUnits = plan.cars.reduce(function (n, c) { return n + c.units; }, 0) +
    plan.overflow.length + b2bOver.length + ungeocoded.length + unmapped.length;

  lines.push(CONFIG.BRAND + ' — ' + name + "'s Route Plan");
  lines.push(dayLabel + ' (' + totalUnits + ' unit' + (totalUnits === 1 ? '' : 's') +
    ', ' + plan.cars.length + ' car' + (plan.cars.length === 1 ? '' : 's') + ')');

  const countParts = WEEKLY_TYPE_ORDER.filter(function (k) { return counts[k]; })
    .map(function (k) { return TYPES[k].short + ' ' + counts[k]; });
  if (countParts.length) lines.push('Counts: ' + countParts.join(', '));

  plan.cars.forEach(function (car, i) {
    lines.push('');
    lines.push('🚗 Car ' + (i + 1) + ' — ' + car.crew + ' cleaners, ' + car.units + ' unit' +
      (car.units === 1 ? '' : 's') + ', ' + hm_(car.totalMin) + (car.overDay ? ' ⚠️ over 8h' : '') +
      ' (' + hm_(car.cleanMin) + ' clean + ' + hm_(car.driveMin) + ' drive · ' +
      Math.round(car.driveMiles) + ' mi):');
    car.stops.forEach(function (s, j) { lines.push(' ' + (j + 1) + '. ' + stopLine_(s, car.crew)); });
    if (baseSet) lines.push('    ↩ back to base');
  });

  if (b2bOver.length) {
    lines.push('');
    lines.push('🚨 B2B OVER CAPACITY — ' + b2bOver.length + ' back-to-back unit' +
      (b2bOver.length === 1 ? '' : 's') + " can't fit and can't be deferred; add a crew:");
    b2bOver.forEach(function (s) { lines.push(' • ' + s.name); });
  }
  if (plan.overflow.length) {
    lines.push('');
    lines.push('⚠️ Over capacity — ' + plan.overflow.length + ' unit' +
      (plan.overflow.length === 1 ? '' : 's') + ' need another crew/day (non-B2B only):');
    plan.overflow.forEach(function (s) { lines.push(' • ' + s.name); });
  }
  if (ungeocoded.length) {
    lines.push('');
    lines.push("⚠️ No map location (couldn't geocode — check the address):");
    ungeocoded.forEach(function (s) { lines.push(' • ' + s.name + ' — ' + s.address); });
  }
  if (unmapped.length) {
    lines.push('');
    lines.push('⚠️ Unknown listing (add to Listings): ' + unmapped.join(', '));
  }
  if (!baseSet) {
    lines.push('');
    lines.push('(Set ROUTING.BASE_ADDRESS to include the first/last drive and round-trip time.)');
  }

  lines.push('');
  lines.push('Drive safe, ' + name + '!');
  return lines.join('\n');
}

/** One stop line: "Bellevue 2243 — 2243 W Lake Sammamish Pkwy SE, Bellevue (4BR/3BA) · B2B 1h2m".
 *  The time is this stop's clean time at the car's crew size (labor ÷ crew). */
function stopLine_(s, crew) {
  var line = typeIcon_(s.type) + s.name + ' — ' + shortAddr_(s.address);
  if (s.beds != null) line += ' (' + s.beds + 'BR/' + s.baths + 'BA)';
  const tag = TYPES[s.type] ? TYPES[s.type].short : s.type;
  return line + ' · ' + tag + ' ' + hm_(stopLaborMin_(s) / crew);
}

/** First two comma segments of an address (street + city). */
function shortAddr_(addr) {
  return String(addr).split(',').map(function (p) { return p.trim(); })
    .filter(function (p) { return p.length; }).slice(0, 2).join(', ');
}

/** Minutes → "1h50m" / "2h" / "45m". */
function hm_(min) {
  min = Math.round(min);
  const h = Math.floor(min / 60), m = min % 60;
  if (h && m) return h + 'h' + m + 'm';
  if (h) return h + 'h';
  return m + 'm';
}

/* ------------------------------------------------------------------ */
/* Diagnostics                                                        */
/* ------------------------------------------------------------------ */

/** Log each of tomorrow's units and whether it resolves to a known address + geocode. */
function checkListings() {
  const day = targetDate_(CONFIG.DAYS_AHEAD);
  const built = buildStops_(maria_(), day);
  Logger.log('Route-planning check for %s:', formatDay_(day));
  built.stops.forEach(function (s) {
    const c = geocode_(s.address);
    Logger.log('  %s  [%s]  %s  → %s', s.name, TYPES[s.type] ? TYPES[s.type].short : s.type,
      s.address, c ? c.lat.toFixed(4) + ',' + c.lng.toFixed(4) : 'GEOCODE FAILED');
  });
  if (built.unmapped.length) Logger.log('  UNKNOWN listings: %s', built.unmapped.join(', '));
}
