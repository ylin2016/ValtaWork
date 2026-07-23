"""Weekly market data: market reports (time series + distributions) and
per-listing neighborhood pricing/occupancy.

Required query params (per the RM API help docs):
  /market_report                         -> country_code
  /market_report/{id}/time_series        -> start_date, end_date
  /market_report/{id}/distribution       -> month (first of month)
  /listings/{id}/neighborhood/pricing    -> channel
  /listings/{id}/neighborhood/occupancy  -> channel
"""
from datetime import datetime, timezone

from . import db
from .transforms import (
    first, as_str, dumps, flatten_series, flatten_distribution,
    date_window, month_firsts,
)
from .wheelhouse_client import WheelhouseError


def _channel_params(channel):
    return {"channel": channel} if channel else None


def download_markets(client, conn, cfg, snapshot_date, market_ids, limit=None):
    """List markets per country, then pull time_series + distribution for the
    markets relevant to us (listing-derived ids, or an explicit config override)."""
    ep = cfg["endpoints"]
    p = cfg.get("params", {})
    pulled_at = datetime.now(timezone.utc).isoformat()

    # 1) Market list per country (cheap: one call each). Also records geometry/postal
    #    codes in raw_json so markets can be matched to listings later if needed.
    market_rows = []
    for country in p.get("country_codes", ["US"]):
        try:
            for item in client.get_paginated(
                ep["market_report"], params={"country_code": country},
                ref_id=f"market_report:{country}"):
                mid = first(item, "market_id", "id", "uuid")
                if mid is None:
                    continue
                market_rows.append({
                    "snapshot_date": snapshot_date,
                    "market_id": str(mid),
                    "name": as_str(first(item, "market_name", "name", "title")),
                    "raw_json": dumps(item),
                    "pulled_at": pulled_at,
                })
        except WheelhouseError as exc:
            print(f"  market_report[{country}]: list failed ({exc})")
    db.upsert(conn, "markets", market_rows)

    # 2) Which markets to pull detail for: explicit override, else listing-derived.
    targets = [str(m) for m in (p.get("market_ids") or [])] or sorted(
        {str(m) for m in (market_ids or [])})
    if limit:
        targets = targets[:limit]
    print(f"  markets: {len(market_rows)} listed; pulling detail for {len(targets)}")
    if not targets:
        print("    (no listing-derived market_ids; set params.market_ids in config.yml "
              "to pull market reports)")
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


def download_neighborhood(client, conn, cfg, snapshot_date, listings, limit=None):
    """Per-listing neighborhood pricing + occupancy (requires channel)."""
    ep = cfg["endpoints"]
    targets = listings[:limit] if limit else listings
    pricing_total = occ_total = 0
    for lst in targets:
        lid = lst["listing_id"]
        params = _channel_params(lst.get("channel"))

        pricing_total += _pull_neighborhood(
            client, conn, cfg, snapshot_date, lid, params,
            ep["neighborhood_pricing"], "neighborhood_pricing", "nbhd_price")
        occ_total += _pull_neighborhood(
            client, conn, cfg, snapshot_date, lid, params,
            ep["neighborhood_occupancy"], "neighborhood_occupancy", "nbhd_occ")

    print(f"  neighborhood_pricing: {pricing_total} rows; "
          f"neighborhood_occupancy: {occ_total} rows (over {len(targets)} listings)")


def _pull_neighborhood(client, conn, cfg, snapshot_date, lid, params, path_tmpl, table, ref):
    path = path_tmpl.format(listing_id=lid)
    try:
        body = client.get_json(path, params=params, ref_id=f"{ref}:{lid}")
    except WheelhouseError as exc:
        print(f"    listing {lid} {table} skipped: {exc}")
        return 0
    rows = [
        {"snapshot_date": snapshot_date, "listing_id": lid, "date": d,
         "metric": m, "value": v, "raw_json": None}
        for (d, m, v) in flatten_series(body)
    ]
    return db.upsert(conn, table, rows)
