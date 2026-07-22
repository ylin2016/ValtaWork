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
 * Travel is estimated from geocoded coordinates (straight-line × ROAD_FACTOR ÷
 * AVG_SPEED_MPH), so it needs only one geocode per unique address — cached in
 * Script Properties. Uses the built-in Apps Script Maps service (no API key).
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

/** Straight-line miles between two { lat, lng } points (haversine). */
function milesBetween_(a, b) {
  if (!a || !b) return 0;
  const R = 3958.8;
  const rad = function (d) { return d * Math.PI / 180; };
  const dLat = rad(b.lat - a.lat), dLng = rad(b.lng - a.lng);
  const h = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
    Math.cos(rad(a.lat)) * Math.cos(rad(b.lat)) * Math.sin(dLng / 2) * Math.sin(dLng / 2);
  return 2 * R * Math.asin(Math.min(1, Math.sqrt(h)));
}

/** Estimated road miles between two points. */
function roadMiles_(a, b) { return milesBetween_(a, b) * CONFIG.ROUTING.ROAD_FACTOR; }

/** Estimated drive minutes between two points. */
function driveMin_(a, b) { return roadMiles_(a, b) / CONFIG.ROUTING.AVG_SPEED_MPH * 60; }

/** Drive minutes for base → s1 → … → sn → base (base legs skipped if base is null). */
function routeDriveMin_(base, stops) {
  var total = 0, prev = base;
  for (var i = 0; i < stops.length; i++) {
    if (prev) total += driveMin_(prev, stops[i].coord);
    prev = stops[i].coord;
  }
  if (base && prev) total += driveMin_(prev, base);
  return total;
}

/** Road miles for base → s1 → … → sn → base. */
function routeDriveMiles_(base, stops) {
  var total = 0, prev = base;
  for (var i = 0; i < stops.length; i++) {
    if (prev) total += roadMiles_(prev, stops[i].coord);
    prev = stops[i].coord;
  }
  if (base && prev) total += roadMiles_(prev, base);
  return total;
}

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
 *   1. Car count is driven by the day's WORKLOAD — the fewest cars (up to MAX_CARS)
 *      that can hold the units and labor when each is staffed at MAX_CREW_PER_CAR.
 *   2. Each car is then staffed with the FEWEST people in [MIN,MAX]_CREW_PER_CAR
 *      that finish its route within the day (more people = proportionally faster).
 *   3. Back-to-back (B2B) jobs are done FIRST in every car and SPREAD across the
 *      crews, and are NEVER left over capacity — pass 1 places every B2B before any
 *      non-B2B, so only non-B2B can land in `overflow`.
 * Fit tests assume MAX_CREW_PER_CAR (a stop fits if the fastest crew could do it),
 * then minCrewFor_ trims each car to the people it actually needs.
 * Returns { cars, overflow, b2bOverflow }. `b2bOverflow` is normally empty — it is
 * non-empty only if B2B alone exceed every crew's capacity (needs manual action).
 */
function planCars_(base, stops) {
  const R = CONFIG.ROUTING;
  const capMin = R.MAX_HOURS_PER_PERSON * 60;

  const isB2B = function (s) { return s.type === 'backtoback'; };
  const b2b = stops.filter(isB2B);
  const rest = stops.filter(function (s) { return !isB2B(s); });

  // A stop fits a car if it stays under the unit cap and the fastest crew (MAX) could
  // still finish the car's route (clean + drive incl. return) within the day.
  const fits = function (car, s) {
    const route = car.route.concat([s]);
    return car.route.length < R.MAX_UNITS_PER_CAR &&
      cleanClockMin_(route, R.MAX_CREW_PER_CAR) + routeDriveMin_(base, route) <= capMin;
  };
  const add = function (car, s) { car.route.push(s); };

  // Car count from the workload: the fewest cars that hold the day's units and its
  // labor at full (MAX) crews, capped at MAX_CARS. Cars can still grow below (up to
  // the cap) if drive time makes the estimate too tight.
  const totalLabor = stops.reduce(function (n, s) { return n + stopLaborMin_(s); }, 0);
  const laborBound = Math.ceil(totalLabor / (R.MAX_CREW_PER_CAR * capMin));
  const unitBound = Math.ceil(stops.length / R.MAX_UNITS_PER_CAR);
  const nCars = Math.min(R.MAX_CARS, Math.max(1, laborBound, unitBound));
  const cars = [];
  for (var c = 0; c < nCars; c++) cars.push({ route: [] });

  // Pass 1 — B2B, spread across crews. Send each B2B to the fittable car with the
  // fewest stops (load balance), tie-broken by least added drive. Open another crew
  // if none fits; only when all MAX_CARS are full does a B2B fall to b2bOverflow.
  const b2bOverflow = [];
  b2b.forEach(function (s) {
    var pick = -1, fewest = Infinity, bestMiles = Infinity;
    cars.forEach(function (car, i) {
      if (!fits(car, s)) return;
      const cur = car.route.length ? car.route[car.route.length - 1].coord : base;
      const miles = cur ? milesBetween_(cur, s.coord) : 0;
      if (car.route.length < fewest || (car.route.length === fewest && miles < bestMiles)) {
        fewest = car.route.length; bestMiles = miles; pick = i;
      }
    });
    if (pick === -1 && cars.length < R.MAX_CARS) {
      cars.push({ route: [] });
      pick = cars.length - 1;
    }
    if (pick === -1) { b2bOverflow.push(s); return; }
    add(cars[pick], s);
  });

  // Order each car's B2B into a sensible nearest-neighbor path from base.
  cars.forEach(function (car) { car.route = orderByNearest_(base, car.route); });

  // Pass 2 — fill leftover capacity with non-B2B, nearest-fit, appended AFTER the
  // B2B (so B2B stay first). Round-robin over cars keeps the crews balanced.
  var pool = rest.slice(), progressed = true;
  while (pool.length && progressed) {
    progressed = false;
    cars.forEach(function (car) {
      const idx = nearestFitIndex_(car, pool, base, fits);
      if (idx !== -1) { add(car, pool.splice(idx, 1)[0]); progressed = true; }
    });
  }
  // Leftover non-B2B: open any remaining crews for them (all B2B are already placed).
  while (pool.length && cars.length < R.MAX_CARS) {
    const car = { route: [] };
    var idx;
    while ((idx = nearestFitIndex_(car, pool, base, fits)) !== -1) add(car, pool.splice(idx, 1)[0]);
    if (!car.route.length) break;
    cars.push(car);
  }

  const summarized = cars.filter(function (car) { return car.route.length; })
    .map(function (car) { return summarizeRoute_(base, car.route, minCrewFor_(base, car.route)); });
  return { cars: summarized, overflow: pool, b2bOverflow: b2bOverflow };
}

/** Index of the nearest (to the car's current position) stop in `pool` that fits; -1 if none. */
function nearestFitIndex_(car, pool, base, fits) {
  const cur = car.route.length ? car.route[car.route.length - 1].coord : base;
  var best = -1, bestMiles = Infinity;
  for (var i = 0; i < pool.length; i++) {
    if (!fits(car, pool[i])) continue;
    const miles = cur ? milesBetween_(cur, pool[i].coord) : 0;
    if (miles < bestMiles) { bestMiles = miles; best = i; }
  }
  return best;
}

/** Greedy nearest-neighbor ordering of stops starting from base. */
function orderByNearest_(base, stops) {
  const remaining = stops.slice(), ordered = [];
  var cur = base;
  while (remaining.length) {
    var best = 0, bestMiles = Infinity;
    for (var i = 0; i < remaining.length; i++) {
      const miles = cur ? milesBetween_(cur, remaining[i].coord) : 0;
      if (miles < bestMiles) { bestMiles = miles; best = i; }
    }
    const next = remaining.splice(best, 1)[0];
    ordered.push(next);
    cur = next.coord;
  }
  return ordered;
}

function summarizeRoute_(base, route, crew) {
  const driveMin = routeDriveMin_(base, route);
  const cleanMin = cleanClockMin_(route, crew);
  return {
    stops: route, units: route.length, crew: crew,
    driveMin: driveMin, cleanMin: cleanMin, totalMin: driveMin + cleanMin,
    driveMiles: routeDriveMiles_(base, route),
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
      (car.units === 1 ? '' : 's') + ', ' + hm_(car.totalMin) + ' (' + hm_(car.cleanMin) +
      ' clean + ' + hm_(car.driveMin) + ' drive · ' + Math.round(car.driveMiles) + ' mi):');
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
