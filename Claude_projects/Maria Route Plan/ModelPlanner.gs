/**
 * CleaningReminder — learned route planner (uses MODEL from Model.gs)
 *
 * Plans a day the way the dispatchers did in the historical dispatch cards, using the
 * parameters train/train_model.py learned from them:
 *   1. Workload: each stop's minutes (2 people) from the learned weights — a fixed
 *      cost per visit plus per bedroom / bathroom; residential uses the client's own
 *      estimate (scaled) when known, else the learned residential default.
 *   2. How many cars: the day model (workload, stops, spread, weekday).
 *   3. Which jobs share a car: maximize the learned same-car affinity between jobs
 *      (pair history + distance), within the learned capacity. Units at one building
 *      always stay together; back-to-backs are placed first and never left over (a
 *      rule, not learned — history has too few known back-to-backs to learn from).
 *   4. Crew per car: the crew model, raised if the car would run past capacity.
 *   5. Route order: as the rule planner — back-to-backs first, then shortest driving.
 * Returns the same { cars, overflow, b2bOverflow } as planCars_ (Routes.gs), so the
 * message is unchanged. Choose it with CONFIG.ROUTING.PLANNER = 'model'.
 */

/** Plan `stops` (from buildStops_) for the day starting at `dayStart`. */
function planCarsModel_(base, stops, dayStart) {
  const M = MODEL;
  if (!stops.length) return { cars: [], overflow: [], b2bOverflow: [] };
  if (M.dayModel.features.join() !== 'person_hours,nodes,spread_km,sun_or_mon' ||
      M.pairModel.features.join() !== 'log_km,km,both_res,one_res,hist_logit,hist_seen') {
    throw new Error('Model.gs does not match ModelPlanner.gs — retrain and paste both files.');
  }
  const nodes = modelNodes_(stops);
  const dayX = modelDayFeatures_(nodes, dayStart);
  var nCars = Math.min(M.maxCars, Math.max(1, argmaxClass_(M.dayModel, dayX)));

  // Pairwise same-car scores (log-odds) between nodes.
  const S = nodes.map(function (a) { return nodes.map(function (b) { return a === b ? 0 : modelPairScore_(a, b); }); });

  // Seeds: the biggest job, then each next seed the job least like the seeds so far.
  const order = nodes.map(function (_, i) { return i; }).sort(function (i, j) {
    return (nodes[j].b2b - nodes[i].b2b) || (nodes[j].load - nodes[i].load);
  });
  const cars = [];
  const placed = {};
  const seedOf = function () {
    var best = -1, bestScore = Infinity;
    order.forEach(function (i) {
      if (placed[i]) return;
      var s = cars.length ? Math.max.apply(null, cars.map(function (c) { return sumScore_(S, i, c); })) : -nodes[i].load;
      if (s < bestScore) { bestScore = s; best = i; }
    });
    return best;
  };
  while (cars.length < Math.min(nCars, nodes.length)) {
    const i = seedOf();
    cars.push([i]); placed[i] = true;
  }

  // Greedy: each job (back-to-backs first, biggest first) joins the car it fits best.
  const overflow = [], b2bOverflow = [];
  order.forEach(function (i) {
    if (placed[i]) return;
    var pick = -1, best = -Infinity;
    cars.forEach(function (c, k) {
      if (!modelFits_(base, nodes, c.concat([i]))) return;
      const s = sumScore_(S, i, c);
      if (s > best) { best = s; pick = k; }
    });
    if (pick === -1 && cars.length < M.maxCars) { cars.push([]); pick = cars.length - 1; }
    if (pick === -1) {
      // No car has room within the learned day. The dispatchers never left a job undone —
      // they ran a long day — so join the best car that still has unit room (it will be
      // flagged "over 8h"); only when every car is at its unit cap is the job left over.
      cars.forEach(function (c, k) {
        if (modelStopsOf_(nodes, c).length + nodes[i].stops.length > M.maxUnits) return;
        const s = sumScore_(S, i, c);
        if (pick === -1 || s > best) { best = s; pick = k; }
      });
    }
    if (pick === -1) (nodes[i].b2b ? b2bOverflow : overflow).push(i);
    else cars[pick].push(i);
    placed[i] = true;
  });

  // Local search: move a job to another car when that raises total affinity and fits.
  for (var pass = 0; pass < 8; pass++) {
    var moved = false;
    for (var k = 0; k < cars.length; k++) {
      for (var p = cars[k].length - 1; p >= 0; p--) {
        const i = cars[k][p];
        const rest = cars[k].filter(function (x) { return x !== i; });
        if (!rest.length) continue;                       // never empty a car
        const here = sumScore_(S, i, rest);
        for (var t = 0; t < cars.length; t++) {
          if (t === k) continue;
          const gain = sumScore_(S, i, cars[t]) - here;
          if (gain > 1e-9 && modelFits_(base, nodes, cars[t].concat([i]))) {
            cars[k] = rest; cars[t].push(i); moved = true; break;
          }
        }
      }
    }
    if (!moved) break;
  }

  const out = cars.filter(function (c) { return c.length; }).map(function (c) {
    const route = optimizeRoute_(base, orderRoute_(base, modelStopsOf_(nodes, c)));
    return summarizeRoute_(base, route, modelCrew_(base, route));
  });
  const flat = function (ix) { return modelStopsOf_(nodes, ix); };
  return { cars: out, overflow: flat(overflow), b2bOverflow: flat(b2bOverflow) };
}

/* ------------------------------------------------------------------ */
/* Stops → nodes (one per building / address)                         */
/* ------------------------------------------------------------------ */

/** Group stops into nodes keyed like training (building, else listing / client key). */
function modelNodes_(stops) {
  const byKey = {}, list = [];
  stops.forEach(function (s) {
    const info = modelStopInfo_(s);
    s.cleanMin = info.minutes;                 // the route summary + per-stop times use the learned minutes
    var n = byKey[info.node];
    if (!n) {
      n = byKey[info.node] = { key: info.node, stops: [], load: 0, b2b: 0, res: true, lat: 0, lng: 0 };
      list.push(n);
    }
    n.stops.push(s);
    n.load += info.minutes;
    if (s.type === 'backtoback') n.b2b = 1;
    if (s.type === 'backtoback' || s.type === 'nextday') n.res = false;
    n.lat += s.coord.lat; n.lng += s.coord.lng;
  });
  list.forEach(function (n) { n.lat /= n.stops.length; n.lng /= n.stops.length; n.coord = { lat: n.lat, lng: n.lng }; });
  return list;
}

/** { node, minutes } for one stop. */
function modelStopInfo_(s) {
  const T = MODEL.theta;
  if (s.type === 'backtoback' || s.type === 'nextday') {
    const k = modelKey_(s.name);
    const b = MODEL.buildings.filter(function (p) { return k.indexOf(p) === 0; })[0];
    return { node: b || k, minutes: T.base + T.bed * (s.beds || 0) + T.bath * (s.baths || 0) };
  }
  const client = modelResidentialIndex_()[streetKey_(s.address)];
  const node = client ? client.key : 'addr:' + (streetKey_(s.address) || String(s.address).toLowerCase());
  if (s.type === 'moveinout') return { node: node, minutes: T.move };
  if (client && client.estMin) return { node: node, minutes: T.est * client.estMin };
  return { node: node, minutes: T.res };
}

var _MODEL_RES_INDEX = null;
function modelResidentialIndex_() {
  if (_MODEL_RES_INDEX) return _MODEL_RES_INDEX;
  _MODEL_RES_INDEX = {};
  MODEL.residential.forEach(function (r) {
    const k = streetKey_(r[1]);
    if (k) _MODEL_RES_INDEX[k] = { key: r[0], estMin: r[2] };
  });
  return _MODEL_RES_INDEX;
}

/** Same normalization as train/dataset.py key(). */
function modelKey_(name) {
  return String(name).replace(/#/g, ' ').replace(/\s+/g, ' ').trim().toLowerCase().replace(/,$/, '').trim()
    .replace(/^mercer island\b/, 'mercer').replace(/^kirland\b/, 'kirkland')
    .replace(/^issquah\b/, 'issaquah').replace(/^millcreek\b/, 'mill creek');
}

function modelStopsOf_(nodes, ix) {
  const out = [];
  ix.forEach(function (i) { nodes[i].stops.forEach(function (s) { out.push(s); }); });
  return out;
}

/* ------------------------------------------------------------------ */
/* Learned pieces                                                     */
/* ------------------------------------------------------------------ */

function modelDayFeatures_(nodes, dayStart) {
  var ph = 0, la = 0, ln = 0;
  nodes.forEach(function (n) { ph += n.load * 2 / 60; la += n.lat; ln += n.lng; });
  const cen = { lat: la / nodes.length, lng: ln / nodes.length };
  var spread = 0;
  nodes.forEach(function (n) { spread += kmBetween_(n.coord, cen); });
  const dow = new Date(dayStart).getDay();
  return [ph, nodes.length, nodes.length ? spread / nodes.length : 0, (dow === 0 || dow === 1) ? 1 : 0];
}

/** Log-odds that nodes a and b ride in the same car. */
function modelPairScore_(a, b) {
  const d = kmBetween_(a.coord, b.coord);
  const h = modelPairHistory_(a.key, b.key);
  const x = [Math.log(1 + d), d, a.res && b.res ? 1 : 0, a.res !== b.res ? 1 : 0, h.logit, h.seen];
  const m = MODEL.pairModel;
  var z = m.intercept[0];
  for (var i = 0; i < x.length; i++) z += m.coef[0][i] * (x[i] - m.mean[i]) / m.scale[i];
  return z;
}

var _MODEL_PAIRS = null;
function modelPairHistory_(a, b) {
  if (!_MODEL_PAIRS) {
    _MODEL_PAIRS = {};
    MODEL.pairs.forEach(function (p) { _MODEL_PAIRS[p[0] + '|' + p[1]] = p; });
  }
  const p = _MODEL_PAIRS[a < b ? a + '|' + b : b + '|' + a];
  const both = p ? p[2] : 0, same = p ? p[3] : 0;
  const q = (same + MODEL.pairAlpha * MODEL.pairPrior) / (both + MODEL.pairAlpha);
  return { logit: Math.log(q / (1 - q)), seen: both > 0 ? 1 : 0 };
}

/** Crew from the crew model, raised until the route fits the learned day. */
function modelCrew_(base, route) {
  var labor = 0;
  route.forEach(function (s) { labor += s.cleanMin * 2; });
  const drive = routeDriveMin_(base, route);
  var crew = argmaxClass_(MODEL.crewModel, [labor / 60, route.length, drive / 60]);
  crew = Math.max(MODEL.minCrew, Math.min(MODEL.maxCrew, crew));
  while (crew < MODEL.maxCrew && labor / crew + drive > MODEL.capMin) crew++;
  return crew;
}

/** Would these nodes fit one car: unit cap, and the learned day at the packing crew. */
function modelFits_(base, nodes, ix) {
  const stops = modelStopsOf_(nodes, ix);
  if (ix.length > 1 && stops.length > MODEL.maxUnits) return false;
  var labor = 0;
  stops.forEach(function (s) { labor += s.cleanMin * 2; });
  return ix.length === 1 || labor / MODEL.packCrew + routeDriveMin_(base, orderRoute_(base, stops)) <= MODEL.capMin;
}

function sumScore_(S, i, car) {
  var s = 0;
  for (var j = 0; j < car.length; j++) s += S[i][car[j]];
  return s;
}

/** Most likely class of a multinomial logistic model (exported by train_model.py). */
function argmaxClass_(m, x) {
  const z = x.map(function (v, i) { return (v - m.mean[i]) / m.scale[i]; });
  if (m.coef.length === 1) {                              // binary
    var s = m.intercept[0];
    z.forEach(function (v, i) { s += m.coef[0][i] * v; });
    return s > 0 ? m.classes[1] : m.classes[0];
  }
  var best = 0, bestZ = -Infinity;
  m.coef.forEach(function (w, c) {
    var s = m.intercept[c];
    z.forEach(function (v, i) { s += w[i] * v; });
    if (s > bestZ) { bestZ = s; best = c; }
  });
  return m.classes[best];
}

/** Straight-line km between two { lat, lng } points (training uses the same). */
function kmBetween_(a, b) {
  return milesBetween_(a, b) * 1.609344;
}
