"""Weekly market data: market reports (daily time series + monthly distributions)
for a configured list of cities/markets.

Required query params (per the RM API help docs):
  /market_report                    -> country_code
  /market_report/{id}/time_series   -> start_date, end_date
  /market_report/{id}/distribution  -> month (first of month)
"""
from datetime import datetime, timezone

from . import db
from .transforms import (
    first, as_str, dumps, flatten_series, flatten_distribution,
    date_window, month_firsts,
)
from .wheelhouse_client import WheelhouseError


def download_markets(client, conn, cfg, snapshot_date, limit=None):
    """List accessible markets per country, then pull time_series + distribution
    for the cities selected in config.yml (params.market_names / market_ids)."""
    ep = cfg["endpoints"]
    p = cfg.get("params", {})
    pulled_at = datetime.now(timezone.utc).isoformat()

    # 1) List accessible markets per country (cheap: one call each). /market_report
    #    only returns markets where you have a Pro listing.
    listed = []   # (market_id, name)
    market_rows = []
    for country in p.get("country_codes", ["US"]):
        try:
            for item in client.get_paginated(
                ep["market_report"], params={"country_code": country},
                ref_id=f"market_report:{country}"):
                mid = first(item, "market_id", "id", "uuid")
                if mid is None:
                    continue
                name = as_str(first(item, "market_name", "name", "title"))
                listed.append((str(mid), name or ""))
                market_rows.append({
                    "snapshot_date": snapshot_date,
                    "market_id": str(mid),
                    "name": name,
                    "raw_json": dumps(item),
                    "pulled_at": pulled_at,
                })
        except WheelhouseError as exc:
            print(f"  market_report[{country}]: list failed ({exc})")
    db.upsert(conn, "markets", market_rows)

    # 2) Select which markets to pull detail for.
    targets = _select_markets(listed, p)
    if limit:
        targets = targets[:limit]

    id_to_name = dict(listed)
    picked = ", ".join(id_to_name.get(mid, mid) for mid in targets) or "(none)"
    print(f"  markets: {len(market_rows)} accessible; pulling detail for "
          f"{len(targets)}: {picked}")
    if not targets:
        print("    (no markets matched params.market_names/market_ids in config.yml)")
        return

    start_date, end_date = date_window(
        snapshot_date, p.get("history_days", 90), p.get("forward_days", 365))
    months = month_firsts(snapshot_date, p.get("distribution_months", 3))

    ts_total = dist_total = 0
    for mid in targets:
        ts_total += _pull_market_time_series(
            client, conn, cfg, snapshot_date, mid, start_date, end_date)
        for month in months:
            dist_total += _pull_market_distribution(
                client, conn, cfg, snapshot_date, mid, month)
    print(f"  market_time_series: {ts_total} rows; market_distributions: {dist_total} rows")


def _select_markets(listed, p):
    """Resolve config selectors to a de-duped, ordered list of market_ids.

    Priority: explicit market_ids > market_names substring match > all accessible.
    """
    ids = [str(m) for m in (p.get("market_ids") or [])]
    if ids:
        return list(dict.fromkeys(ids))

    names = [n.lower() for n in (p.get("market_names") or [])]
    if names:
        matched, unmatched = [], set(p.get("market_names") or [])
        for mid, mname in listed:
            low = (mname or "").lower()
            hit = next((orig for orig, n in zip(p["market_names"], names) if n in low), None)
            if hit is not None:
                matched.append(mid)
                unmatched.discard(hit)
        if unmatched:
            print(f"    note: no accessible market matched {sorted(unmatched)} "
                  "(need a Pro listing there)")
        return list(dict.fromkeys(matched))

    # neither selector set -> all accessible markets
    return list(dict.fromkeys(mid for mid, _ in listed))


def _pull_market_time_series(client, conn, cfg, snapshot_date, mid, start_date, end_date):
    path = cfg["endpoints"]["market_time_series"].format(market_id=mid)
    try:
        body = client.get_json(
            path, params={"start_date": start_date, "end_date": end_date},
            ref_id=f"market_ts:{mid}")
    except WheelhouseError as exc:
        print(f"    market {mid} time_series skipped: {exc}")
        return 0
    rows = [
        {"snapshot_date": snapshot_date, "market_id": mid, "date": d,
         "metric": m, "value": v, "raw_json": None}
        for (d, m, v) in flatten_series(body)
    ]
    return db.upsert(conn, "market_time_series", rows)


def _pull_market_distribution(client, conn, cfg, snapshot_date, mid, month):
    path = cfg["endpoints"]["market_distribution"].format(market_id=mid)
    try:
        body = client.get_json(path, params={"month": month}, ref_id=f"market_dist:{mid}:{month}")
    except WheelhouseError as exc:
        print(f"    market {mid} distribution ({month}) skipped: {exc}")
        return 0
    rows = [
        {"snapshot_date": snapshot_date, "market_id": mid, **row}
        for row in flatten_distribution(body)
    ]
    return db.upsert(conn, "market_distributions", rows)
