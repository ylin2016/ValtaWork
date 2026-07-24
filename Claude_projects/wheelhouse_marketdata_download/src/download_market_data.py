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

    segments = _segments(p)
    seg_desc = ", ".join(_seg_label(s) for s in segments)
    print(f"    segments ({len(segments)}): {seg_desc}")

    want_dist = (cfg.get("pull") or {}).get("market_distribution", False)
    ts_total = dist_total = 0
    for mid in targets:
        for seg in segments:
            ts_total += _pull_market_monthly(
                client, conn, cfg, snapshot_date, mid, start_date, end_date, seg)
            if want_dist:
                for month in months:
                    dist_total += _pull_market_distribution(
                        client, conn, cfg, snapshot_date, mid, month, seg)
    msg = f"  market_monthly: {ts_total} rows"
    if want_dist:
        msg += f"; market_distributions: {dist_total} rows"
    print(msg)


def _segments(p):
    """Build the list of (performance, bedrooms) segments to request.

    The API accepts one performance and one bedrooms value per call, so each
    combination is its own request. ('', '') means unsegmented.
    """
    seg_cfg = p.get("market_segments") or {}
    out = []
    if seg_cfg.get("include_overall", True):
        out.append(("", ""))
    for perf in (seg_cfg.get("performance") or []):
        beds = seg_cfg.get("bedrooms") or [""]
        for bed in beds:
            out.append((str(perf), str(bed)))
    if not out:
        out = [("", "")]
    return list(dict.fromkeys(out))


def _seg_label(seg):
    perf, bed = seg
    if not perf and not bed:
        return "overall"
    return f"{perf or 'all'}/{bed or 'all'}br"


def _seg_params(seg):
    perf, bed = seg
    p = {}
    if perf:
        p["performance"] = perf
    if bed:
        p["bedrooms"] = bed
    return p


def _metric_param(cfg, kind):
    """Metric filter from config (empty list -> no filter).

    NOTE: the param must be sent as `metric[]=a&metric[]=b`. Plain repeated
    `metric=` silently keeps only the LAST value, and a comma-separated list 400s.
    """
    metrics = ((cfg.get("params") or {}).get("metrics") or {}).get(kind) or []
    return {"metric[]": list(metrics)} if metrics else {}


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


def _pull_market_monthly(client, conn, cfg, snapshot_date, mid, start_date,
                         end_date, seg=("", "")):
    """Fetch the daily market series and store only the monthly averages.

    The API returns daily rows; we roll them up to a per-month mean per metric so
    the DB stays small (this is all any export needs)."""
    path = cfg["endpoints"]["market_time_series"].format(market_id=mid)
    params = {"start_date": start_date, "end_date": end_date}
    params.update(_metric_param(cfg, "time_series"))
    params.update(_seg_params(seg))
    try:
        body = client.get_json(path, params=params,
                               ref_id=f"market_ts:{mid}:{_seg_label(seg)}")
    except WheelhouseError as exc:
        print(f"    market {mid} monthly [{_seg_label(seg)}] skipped: {exc}")
        return 0
    if not body:
        return 0
    perf, bed = seg
    # (month, metric) -> [sum, count]
    agg = {}
    for (d, m, v) in flatten_series(body):
        if v is None:
            continue
        key = (str(d)[:7], m)
        acc = agg.setdefault(key, [0.0, 0])
        acc[0] += v
        acc[1] += 1
    rows = [
        {"snapshot_date": snapshot_date, "market_id": mid,
         "performance": perf, "bedrooms": bed, "month": month,
         "metric": m, "value": round(s / n, 4), "days_in_avg": n}
        for (month, m), (s, n) in agg.items()
    ]
    return db.upsert(conn, "market_monthly", rows)


def _pull_market_distribution(client, conn, cfg, snapshot_date, mid, month, seg=("", "")):
    path = cfg["endpoints"]["market_distribution"].format(market_id=mid)
    params = {"month": month}
    params.update(_metric_param(cfg, "distribution"))
    params.update(_seg_params(seg))
    try:
        body = client.get_json(path, params=params,
                               ref_id=f"market_dist:{mid}:{month}:{_seg_label(seg)}")
    except WheelhouseError as exc:
        print(f"    market {mid} distribution ({month}) [{_seg_label(seg)}] skipped: {exc}")
        return 0
    if not body:
        return 0
    perf, bed = seg
    rows = [
        {"snapshot_date": snapshot_date, "market_id": mid,
         "performance": perf, "bedrooms": bed, **row}
        for row in flatten_distribution(body)
    ]
    return db.upsert(conn, "market_distributions", rows)
