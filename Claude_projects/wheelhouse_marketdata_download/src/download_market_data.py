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
    year_window, month_firsts,
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
    listing_mids = _listing_market_ids(conn, snapshot_date)
    targets = _select_markets(listed, p, listing_mids)
    if limit:
        targets = targets[:limit]

    id_to_name = dict(listed)
    picked = ", ".join(id_to_name.get(mid, mid) for mid in targets) or "(none)"
    print(f"  markets: {len(market_rows)} accessible; pulling detail for "
          f"{len(targets)}: {picked}")
    if not targets:
        print("    (no markets matched params.market_names/market_ids in config.yml)")
        return

    start_date, end_date = year_window(snapshot_date, p.get("history_years", 2))
    months = month_firsts(snapshot_date, p.get("distribution_months", 3))
    print(f"    date range: {start_date} .. {end_date}")

    ts_total = dist_total = 0
    for mid in targets:
        ts_total += _pull_market_time_series(
            client, conn, cfg, snapshot_date, mid, start_date, end_date)
        for month in months:
            dist_total += _pull_market_distribution(
                client, conn, cfg, snapshot_date, mid, month)
    print(f"  market_time_series: {ts_total} rows; market_distributions: {dist_total} rows")


def _listing_market_ids(conn, snapshot_date):
    """Distinct market_ids across our harvested listings for this snapshot."""
    return {str(r[0]) for r in conn.execute(
        "SELECT DISTINCT market_id FROM listings "
        "WHERE snapshot_date = ? AND market_id IS NOT NULL", (snapshot_date,))}


def _select_markets(listed, p, listing_mids):
    """Resolve config selectors to a de-duped, ordered list of market_ids.

    markets_from_listings=True : markets where our listings are, PLUS any
                                 market_names / market_ids as additive extras.
    markets_from_listings=False: market_ids > market_names as the exact list,
                                 falling back to every accessible market.
    """
    accessible = [mid for mid, _ in listed]
    from_listings = p.get("markets_from_listings", True)

    explicit = _explicit_markets(listed, p)

    if from_listings:
        picked = [mid for mid in accessible if mid in listing_mids]
        extras = [mid for mid in explicit if mid not in picked]
        no_report = sorted(listing_mids - set(accessible))
        if no_report:
            print(f"    note: {len(no_report)} listing market(s) have no accessible "
                  f"report (demo/Free tier): {no_report}")
        if extras:
            names = dict(listed)
            print(f"    plus {len(extras)} extra market(s) from config: "
                  + ", ".join(names.get(m, m) for m in extras))
        # keep accessible ordering
        chosen = set(picked) | set(extras)
        return [mid for mid in accessible if mid in chosen]

    if explicit:
        return explicit
    return list(dict.fromkeys(accessible))


def _explicit_markets(listed, p):
    """market_ids (preferred) or market_names substring matches, as market_ids."""
    ids = [str(m) for m in (p.get("market_ids") or [])]
    if ids:
        return list(dict.fromkeys(ids))

    raw_names = p.get("market_names") or []
    if not raw_names:
        return []
    lowered = [n.lower() for n in raw_names]
    matched, unmatched = [], set(raw_names)
    for mid, mname in listed:
        low = (mname or "").lower()
        hit = next((orig for orig, n in zip(raw_names, lowered) if n in low), None)
        if hit is not None:
            matched.append(mid)
            unmatched.discard(hit)
    if unmatched:
        print(f"    note: no accessible market matched {sorted(unmatched)} "
              "(need a Pro listing there)")
    return list(dict.fromkeys(matched))


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
