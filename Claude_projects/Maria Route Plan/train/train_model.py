"""Train the dispatch-imitation planning model on the historical dispatch cards and
export it as Model.gs for the Apps Script planner (ModelPlanner.gs).

There are no finish times, so nothing here predicts real durations. What it learns
is how the dispatchers plan:

  1. Workload weights — minutes per base visit / bedroom / bathroom / residential job,
     fit so that real cars come out as close to one full day as possible (the
     dispatchers add a car when the work doesn't fit). DAY_TARGET_H sets the scale;
     capacity is the 90th percentile of the real cars' fitted load.
  2. Cars per day — multinomial logistic regression on the day's workload, stops,
     spread and weekday.
  3. Crew size per car — multinomial logistic regression on the car's workload,
     units and driving.
  4. Same-car affinity — logistic regression on pairs of jobs from the same day:
     distance, residential mix, and how often that pair shared a car before
     (smoothed, leave-one-day-out so a day never explains itself).

Back-to-back is NOT a learned feature: it is unknown for most history (no Guesty data
before 2026, delisted listings). The planner still uses the live calendar's
back-to-backs by rule — first in the car, never left over.

Usage (from the project folder):
    .venv/bin/python train/train_model.py            # train on everything up to the holdout, evaluate, export
    .venv/bin/python train/train_model.py --final    # retrain on all history before exporting
Outputs: Model.gs (the model) and train/out/ (evaluation inputs + report).
"""
import argparse
import json
import math
from datetime import date
from itertools import combinations
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.optimize import lsq_linear
from sklearn.linear_model import LogisticRegression

import dataset

PROJ = dataset.PROJ
OUT = Path(__file__).resolve().parent / 'out'
HOLDOUT_FROM = pd.Timestamp('2026-08-01')
DAY_TARGET_H = 7.0                            # scale assumption: a dispatched car ≈ a 7-hour day
ROAD_FACTOR, AVG_MPH = 1.3, 28                # same fallback as Routes.gs
PRIOR = dict(base=20, bed=12, bath=16, res=120, est=1.0, move=120)   # current rules (× 0.8 scale)
ALPHA = 2.0                                   # smoothing for pair history


# ---------------------------------------------------------------- geometry
def km(a, b):
    if a is None or b is None or a == b: return 0.0
    r = math.radians
    h = math.sin(r(b[0] - a[0]) / 2) ** 2 + math.cos(r(a[0])) * math.cos(r(b[0])) * math.sin(r(b[1] - a[1]) / 2) ** 2
    return 2 * 6371.0 * math.asin(min(1, math.sqrt(h)))


def drive_min(a, b): return km(a, b) / 1.609344 * ROAD_FACTOR / AVG_MPH * 60


def route_drive(base, coords):
    t, prev = 0.0, base
    for c in coords: t += drive_min(prev, c); prev = c
    return t + drive_min(prev, base)


# ---------------------------------------------------------------- labor
FEATS = ['base', 'bed', 'bath', 'res', 'est', 'move']


def stop_features(r):
    """Row → feature vector so that minutes(2 people) = features · theta."""
    f = dict.fromkeys(FEATS, 0.0)
    if r.kind == 'turnover':
        f['base'], f['bed'], f['bath'] = 1.0, r.beds or 0.0, r.baths or 0.0
    elif r.kind == 'moveinout':
        f['move'] = 1.0
    elif pd.notna(r.est_min):
        f['est'] = r.est_min
    else:
        f['res'] = 1.0
    return np.array([f[k] for k in FEATS])


def fit_labor(cars):
    """Least squares with a ridge pull toward the current rules: each multi-car-day car's
    clock time (2·Σ minutes / crew + drive) should be about DAY_TARGET_H."""
    T = DAY_TARGET_H * 60
    A = np.array([c['F'] * 2 / c['crew'] for c in cars])
    y = np.array([T - c['drive'] for c in cars])
    theta0 = np.array([PRIOR[k] for k in FEATS])
    # Prior pull: moving weight k by `scale[k]` from the rules costs as much as one car
    # being 42 min (a tenth of the target day) off — times sqrt(lam).
    scale = np.array([30, 10, 10, 60, 0.5, 60])
    lam = 4.0
    w = np.sqrt(lam) * (T / 10) / scale
    A2 = np.vstack([A, np.diag(w)])
    y2 = np.concatenate([y, w * theta0])
    lo = np.array([5, 0, 0, 30, 0.3, 30]); hi = np.array([60, 40, 40, 300, 2.0, 300])
    sol = lsq_linear(A2, y2, bounds=(lo, hi))
    return dict(zip(FEATS, sol.x))


def minutes(theta, F): return float(np.dot([theta[k] for k in FEATS], F))


# ---------------------------------------------------------------- tables
def build_tables(st, base):
    st = st[st.pk.notna() & st.lat.notna()].copy()
    st['node'] = st.building.fillna(st.pk)
    st['coord'] = list(zip(st.lat, st.lng))
    st['F'] = [stop_features(r) for r in st.itertuples()]
    st['crew_n'] = st.crew_n.clip(lower=1)
    cars = []
    for (day, car), C in st.sort_values('order', kind='stable').groupby(['day', 'car']):
        cars.append(dict(day=day, car=car, crew=int(C.crew_n.iloc[0]), F=np.sum(list(C.F), axis=0),
                         drive=route_drive(base, list(C.coord)), units=len(C), nodes=set(C.node),
                         b2b=int(C.b2b.sum())))
    ncars = pd.Series([c['day'] for c in cars]).value_counts()
    for c in cars: c['day_cars'] = int(ncars[c['day']])
    return st, cars


def day_features(D, theta, base):
    """[person-hours, nodes, spread km, sun/mon] for one day's stops."""
    ph = sum(minutes(theta, F) for F in D.F) * 2 / 60
    nodes = D.groupby('node').agg(lat=('lat', 'mean'), lng=('lng', 'mean'))
    cen = (nodes.lat.mean(), nodes.lng.mean())
    spread = float(np.mean([km((a, b), cen) for a, b in zip(nodes.lat, nodes.lng)])) if len(nodes) else 0.0
    return [ph, len(nodes), spread, int(D.day.iloc[0].dayofweek in (0, 6))]


DAY_FEATS = ['person_hours', 'nodes', 'spread_km', 'sun_or_mon']
CREW_FEATS = ['person_hours', 'units', 'drive_hours']
PAIR_FEATS = ['log_km', 'km', 'both_res', 'one_res', 'hist_logit', 'hist_seen']


def pair_history(st):
    """(nodeA, nodeB) → [days both present on a multi-car day, days in the same car], per day too."""
    multi = st.groupby('day').car.nunique()
    tot, per_day = {}, {}
    for day, D in st[st.day.isin(multi[multi > 1].index)].groupby('day'):
        car_of = D.groupby('node').car.agg(lambda s: s.mode().iloc[0])
        for a, b in combinations(sorted(car_of.index), 2):
            same = int(car_of[a] == car_of[b])
            t = tot.setdefault((a, b), [0, 0]); t[0] += 1; t[1] += same
            per_day[(day, a, b)] = same
    return tot, per_day


def hist_feats(a, b, tot, p0, exclude_same=None):
    k = (a, b) if a < b else (b, a)
    both, same = tot.get(k, (0, 0))
    if exclude_same is not None:
        both, same = both - 1, same - exclude_same
    p = (same + ALPHA * p0) / (both + ALPHA)
    return math.log(p / (1 - p)), int(both > 0)


def pair_rows(D, tot, p0, leave_out):
    nodes = D.groupby('node').agg(lat=('lat', 'mean'), lng=('lng', 'mean'),
                                  res=('kind', lambda s: int((s != 'turnover').all())),
                                  car=('car', lambda s: s.mode().iloc[0]))
    rows, ys = [], []
    for a, b in combinations(sorted(nodes.index), 2):
        A, B = nodes.loc[a], nodes.loc[b]
        d = km((A.lat, A.lng), (B.lat, B.lng))
        same = int(A.car == B.car)
        hl, hs = hist_feats(a, b, tot, p0, same if leave_out else None)
        rows.append([math.log1p(d), d, int(A.res and B.res), int(A.res != B.res), hl, hs])
        ys.append(same)
    return rows, ys


def logit_export(model, X, feats, classes=None):
    mu, sd = X.mean(0), X.std(0)
    sd[sd == 0] = 1
    return dict(features=feats, mean=mu.round(6).tolist(), scale=sd.round(6).tolist(),
                coef=np.round(model.coef_, 6).tolist(), intercept=np.round(model.intercept_, 6).tolist(),
                classes=[int(c) for c in (classes if classes is not None else model.classes_)])


def fit_logit(X, y, C=1.0, multi=False):
    X = np.asarray(X, float); y = np.asarray(y)
    mu, sd = X.mean(0), X.std(0); sd[sd == 0] = 1
    m = LogisticRegression(C=C, max_iter=2000)
    m.fit((X - mu) / sd, y)
    return m, X


# ---------------------------------------------------------------- main
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--final', action='store_true', help='retrain on all history before exporting')
    ap.add_argument('--train-end', help='train on days before this date (default: the holdout start)')
    ap.add_argument('--eval-from', help='first evaluation day (default: the holdout start)')
    ap.add_argument('--eval-to', help='last evaluation day (default: the last day)')
    ap.add_argument('--cap-q', type=float, default=0.90, help='capacity quantile of real car days')
    ap.add_argument('--pair-c', type=float, default=0.1, help='inverse regularization of the pair model')
    args = ap.parse_args()
    OUT.mkdir(exist_ok=True)

    st_all, props = dataset.build()
    base = dataset.properties()[dataset.key('Seatac 12834')]
    base = (base['lat'], base['lng'])
    st, cars = build_tables(st_all, base)
    train_end = (st.day.max() + pd.Timedelta(days=1) if args.final
                 else pd.Timestamp(args.train_end) if args.train_end else HOLDOUT_FROM)
    eval_from = pd.Timestamp(args.eval_from) if args.eval_from else HOLDOUT_FROM
    eval_to = pd.Timestamp(args.eval_to) if args.eval_to else st.day.max()
    tr = st[st.day < train_end]
    tr_cars = [c for c in cars if c['day'] < train_end and c['crew'] >= 2]

    # 1. workload weights + capacity
    theta = fit_labor([c for c in tr_cars if c['day_cars'] > 1])
    loads = np.array([minutes(theta, c['F']) * 2 / c['crew'] + c['drive'] for c in tr_cars])
    cap_min = float(np.quantile(loads, args.cap_q))

    # 2. cars per day
    dX, dy = [], []
    for day, D in tr.groupby('day'):
        dX.append(day_features(D, theta, base)); dy.append(min(D.car.nunique(), 3))
    day_m, dXa = fit_logit(dX, dy, C=0.5)

    # 3. crew per car
    cX, cy = [], []
    for c in tr_cars:
        ph = minutes(theta, c['F']) * 2 / 60
        cX.append([ph, c['units'], c['drive'] / 60]); cy.append(int(min(max(c['crew'], 2), 4)))
    crew_m, cXa = fit_logit(cX, cy, C=0.5)

    # 4. same-car affinity
    tot, _ = pair_history(tr)
    multi = tr.groupby('day').car.nunique()
    p0 = float(sum(v[1] for v in tot.values()) / max(1, sum(v[0] for v in tot.values())))
    pX, py = [], []
    for day, D in tr[tr.day.isin(multi[multi > 1].index)].groupby('day'):
        r, y = pair_rows(D, tot, p0, leave_out=True); pX += r; py += y
    pair_m, pXa = fit_logit(pX, py, C=args.pair_c)

    # ------------------------------------------------------------ export
    res_tab = pd.read_excel(dataset.DATA / 'Listings.xlsx', 'Residential')
    residential = []
    for r in res_tab.itertuples():
        k = dataset.key(r.Listing)
        if pd.isna(r.Address) or k not in props or props[k]['kind'] == 'turnover': continue
        residential.append([k, str(r.Address), None if pd.isna(r.EstMin_2ppl) else float(r.EstMin_2ppl),
                            props[k]['kind']])
    pairs = sorted([[a, b, v[0], v[1]] for (a, b), v in tot.items() if v[0] >= 2])
    model = dict(
        version=str(date.today()), trainedFrom=str(tr.day.min().date()), trainedTo=str(tr.day.max().date()),
        days=int(tr.day.nunique()), cars=len(tr_cars), dayTargetHours=DAY_TARGET_H,
        theta={k: round(v, 4) for k, v in theta.items()}, capMin=round(cap_min, 1),
        maxUnits=int(np.quantile([c['units'] for c in tr_cars], 0.95)),
        packCrew=int(np.quantile([c['crew'] for c in tr_cars], 0.75)),
        maxCars=3, minCrew=2, maxCrew=4, buildings=list(dataset.BUILDINGS),
        dayModel=logit_export(day_m, dXa, DAY_FEATS), crewModel=logit_export(crew_m, cXa, CREW_FEATS),
        pairModel=logit_export(pair_m, pXa, PAIR_FEATS, classes=[0, 1]), pairPrior=round(p0, 4), pairAlpha=ALPHA,
        pairs=pairs, residential=residential)
    write_model_gs(model)

    # ------------------------------------------------------------ evaluation inputs (holdout days)
    ev = st[(st.day >= eval_from) & (st.day <= eval_to)]
    days = {}
    for day, D in ev.groupby('day'):
        stops = []
        for r in D.itertuples():
            stops.append(dict(name=r.name, key=r.pk, node=r.node, address=r.address if r.building is None
                              else props[r.pk]['address'], kind=r.kind, beds=r.beds, baths=r.baths,
                              estMin=None if pd.isna(r.est_min) else r.est_min, b2b=None if pd.isna(r.b2b) else bool(r.b2b),
                              coord=dict(lat=r.lat, lng=r.lng), realCar=int(r.car), realCrew=int(r.crew_n),
                              order=float(r.order)))
        days[str(day.date())] = stops
    def clean(o):                                    # NaN → null (JSON has no NaN)
        if isinstance(o, dict): return {k: clean(v) for k, v in o.items()}
        if isinstance(o, list): return [clean(v) for v in o]
        if isinstance(o, float) and math.isnan(o): return None
        return o
    json.dump(clean(dict(base=dict(lat=base[0], lng=base[1]), days=days)), open(OUT / 'holdout_days.json', 'w'))

    print(f"trained on {model['trainedFrom']} → {model['trainedTo']}: {model['days']} days, {model['cars']} cars")
    print('workload weights (2-person minutes):', {k: round(v, 2) for k, v in theta.items()})
    print(f'capacity (90th pct car day): {cap_min / 60:.2f} h')
    tl = np.array([minutes(theta, c['F']) * 2 / c['crew'] + c['drive'] for c in tr_cars]) / 60
    pr = np.array([minutes(PRIOR, c['F']) * 2 / c['crew'] + c['drive'] for c in tr_cars]) / 60
    print(f'real-car day length, train: learned {np.median(tl):.2f} h median (CV {tl.std() / tl.mean():.2f}) '
          f'| rules {np.median(pr):.2f} h (CV {pr.std() / pr.mean():.2f})')
    print(f'pairs exported: {len(pairs)}; residential clients: {len(residential)}; pair base rate {p0:.2f}')
    print('pair model coef:', dict(zip(PAIR_FEATS, np.round(pair_m.coef_[0], 2))))


def write_model_gs(model):
    body = json.dumps(model, separators=(',', ':'))
    (PROJ / 'Model.gs').write_text(
        '/**\n * CleaningReminder — learned planning model (GENERATED by train/train_model.py — do not edit).\n'
        f" * Trained on the dispatch cards {model['trainedFrom']} → {model['trainedTo']} "
        f"({model['days']} days, {model['cars']} cars). Used by ModelPlanner.gs.\n"
        ' * Retrain: .venv/bin/python train/train_model.py --final, then paste this file.\n */\n'
        f'const MODEL = {body};\n')


if __name__ == '__main__':
    main()
