"""Score the learned planner and the rule planner against the real dispatch on the
holdout days (days the model never trained on).

    .venv/bin/python train/evaluate.py      (after train/train_model.py)

Agreement with the dispatchers:
  cars/day exact   share of days with the same number of cars as the real dispatch
  same-car F1      over every pair of jobs on a day: did the plan put them in the same car
                   exactly when the dispatchers did? (precision/recall → F1)
  ARI              adjusted Rand index of the day's grouping vs the real one (1 = identical,
                   0 = no better than chance), averaged over days with 2+ jobs
plus cleaners/day, driving miles/day (same straight-line model for all) and leftovers.
"""
import json
import subprocess
from itertools import combinations
from pathlib import Path

import numpy as np
from sklearn.metrics import adjusted_rand_score

HERE = Path(__file__).resolve().parent
PROJ = HERE.parent
OUT = HERE / 'out'


def labels(n, cars, overflow=()):
    lab = [-1] * n
    for k, c in enumerate(cars):
        for i in c['stops']: lab[i] = k
    for j, i in enumerate(overflow): lab[i] = 100 + j          # leftovers are singletons
    return lab


def main():
    subprocess.run(['node', str(HERE / 'eval_planners.js'), str(PROJ), str(OUT / 'holdout_days.json'),
                    str(OUT / 'holdout_plans.json')], check=True)
    res = json.load(open(OUT / 'holdout_plans.json'))
    rows = {p: dict(cars_exact=[], tp=0, fp=0, fn=0, ari=[], people=[], miles=[], left=0, left_days=0)
            for p in ('rules', 'model')}
    real_people, real_miles, real_cars = [], [], []
    for day, r in res.items():
        n = r['n']
        real = labels(n, r['real']['cars'])
        real_cars.append(len(r['real']['cars']))
        real_people.append(sum(c['crew'] for c in r['real']['cars']))
        real_miles.append(sum(c['miles'] for c in r['real']['cars']))
        for p in rows:
            plan = r[p]
            left = plan['overflow'] + plan['b2bOverflow']
            lab = labels(n, plan['cars'], left)
            m = rows[p]
            m['cars_exact'].append(len(plan['cars']) == len(r['real']['cars']))
            for i, j in combinations(range(n), 2):
                a, b = real[i] == real[j], lab[i] == lab[j]
                m['tp'] += a and b; m['fp'] += b and not a; m['fn'] += a and not b
            if n >= 2: m['ari'].append(adjusted_rand_score(real, lab))
            m['people'].append(sum(c['crew'] for c in plan['cars']))
            m['miles'].append(sum(c['miles'] for c in plan['cars']))
            m['left'] += len(left); m['left_days'] += bool(left)
    report = dict(days=len(res), first=min(res), last=max(res),
                  real=dict(cars_per_day=np.mean(real_cars), people_per_day=np.mean(real_people),
                            miles_per_day=np.mean(real_miles)))
    for p, m in rows.items():
        prec = m['tp'] / max(1, m['tp'] + m['fp']); rec = m['tp'] / max(1, m['tp'] + m['fn'])
        report[p] = dict(cars_exact=float(np.mean(m['cars_exact'])), same_car_precision=prec, same_car_recall=rec,
                         same_car_f1=2 * prec * rec / max(1e-9, prec + rec), ari=float(np.mean(m['ari'])),
                         people_per_day=float(np.mean(m['people'])),
                         people_abs_err=float(np.mean(np.abs(np.array(m['people']) - np.array(real_people)))),
                         miles_per_day=float(np.mean(m['miles'])), leftover_units=m['left'], leftover_days=m['left_days'])
    json.dump(report, open(OUT / 'holdout_report.json', 'w'), indent=2)
    print(f"holdout {report['first']} → {report['last']} ({report['days']} days) | real: "
          f"{report['real']['cars_per_day']:.2f} cars, {report['real']['people_per_day']:.2f} people, "
          f"{report['real']['miles_per_day']:.1f} mi per day")
    print(f"{'':10s}{'cars exact':>11s}{'same-car F1':>13s}{'precision':>11s}{'recall':>8s}{'ARI':>7s}"
          f"{'people/day':>12s}{'people err':>12s}{'miles/day':>11s}{'leftover':>10s}")
    for p in ('rules', 'model'):
        m = report[p]
        print(f"{p:10s}{m['cars_exact']:>11.0%}{m['same_car_f1']:>13.2f}{m['same_car_precision']:>11.2f}"
              f"{m['same_car_recall']:>8.2f}{m['ari']:>7.2f}{m['people_per_day']:>12.2f}{m['people_abs_err']:>12.2f}"
              f"{m['miles_per_day']:>11.1f}{m['leftover_units']:>6d} ({m['leftover_days']}d)")


if __name__ == '__main__':
    main()
