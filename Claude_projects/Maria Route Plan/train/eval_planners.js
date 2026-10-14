// Run the real Apps Script planners (rules: Routes.gs planCars_; learned: ModelPlanner.gs
// planCarsModel_) on the holdout days written by train_model.py, plus score the real cars
// with the same driving model. Maps/Properties are absent in Node, so driving uses the
// planner's own straight-line fallback (identical for all three).
//   node train/eval_planners.js <project dir> <holdout_days.json> <out.json>
const fs = require('fs'), vm = require('vm');
const [P, IN, OUT] = process.argv.slice(2);
const ctx = { Logger: { log() {} } };
vm.createContext(ctx);
for (const f of ['Config.gs', 'Cleaners.gs', 'Code.gs', 'Listings.gs', 'Routes.gs', 'Model.gs', 'ModelPlanner.gs'])
  vm.runInContext(fs.readFileSync(`${P}/${f}`, 'utf8').replace(/^const /gm, 'var '), ctx, { filename: f });
const R = ctx.CONFIG.ROUTING, data = JSON.parse(fs.readFileSync(IN));

function toStops(raw) {
  return raw.map((s, idx) => {
    const type = s.kind === 'turnover' ? (s.b2b ? 'backtoback' : 'nextday') : s.kind;
    const cleanMin = s.kind === 'turnover'
      ? (R.CLEAN_TIME_SCALE || 1) * (R.CLEAN_BASE_MIN + R.CLEAN_PER_BEDROOM_MIN * (s.beds || 0) + R.CLEAN_PER_BATHROOM_MIN * (s.baths || 0))
      : (ctx.residentialMinutes_(s.address) || (s.kind === 'residential' ? R.RESIDENTIAL_MIN : R.MOVEINOUT_MIN));
    return { idx, name: s.key, address: s.address, type, beds: s.beds, baths: s.baths, coord: s.coord, cleanMin };
  });
}
const summarize = p => ({
  cars: p.cars.map(c => ({ stops: c.stops.map(s => s.idx), crew: c.crew, miles: c.driveMiles, totalMin: c.totalMin })),
  overflow: p.overflow.map(s => s.idx), b2bOverflow: p.b2bOverflow.map(s => s.idx),
});
const out = {};
for (const [day, raw] of Object.entries(data.days)) {
  const rules = ctx.planCars_(data.base, toStops(raw));
  const model = ctx.planCarsModel_(data.base, toStops(raw), new Date(day + 'T12:00:00'));
  const realCars = {};
  raw.forEach((s, idx) => (realCars[s.realCar] = realCars[s.realCar] || []).push({ ...s, idx }));
  const real = Object.values(realCars).map(c => {
    c.sort((a, b) => a.order - b.order);
    const d = ctx.routeDrive_(data.base, c);
    return { stops: c.map(s => s.idx), crew: c[0].realCrew, miles: d.miles };
  });
  out[day] = { n: raw.length, real: { cars: real }, rules: summarize(rules), model: summarize(model) };
}
fs.writeFileSync(OUT, JSON.stringify(out));
