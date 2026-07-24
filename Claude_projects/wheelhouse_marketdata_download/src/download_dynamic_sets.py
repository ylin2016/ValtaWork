"""Dynamic set ("dynamite set") data: the comp-set groupings, their members,
which of our listings are associated with them, and their metrics over time.

Live under /sets (per the RM API help docs). Optional params:
  time_series  -> start_date, end_date, metric
  distribution -> month, metric
"""
from datetime import datetime, timezone

from . import db
from .transforms import (
    first, to_float, as_str, dumps,
    flatten_series, flatten_distribution, flatten_metrics, flatten_members,
    _extract_list, year_window, month_firsts, _listing_id, in_window,
)
from .wheelhouse_client import WheelhouseError


def collect_associated_listings(client, conn, cfg, snapshot_date, limit=None):
    """List sets, store each set + its associated (real) listings, and upsert those
    real listings into the `listings` table.

    GET /listings only returns a demo listing; our actual portfolio is exposed as
    the `associated_listings` on each dynamic set (with wheelhouse_id + channel).
    This harvests them so the per-listing neighborhood pulls cover real units.

    Returns (set_ids, real_listings, associated_row_count) where real_listings is a
    list of {listing_id, channel, market_id} dicts (listing_id = wheelhouse_id).
    """
    ep = cfg["endpoints"]
    pulled_at = datetime.now(timezone.utc).isoformat()

    set_rows, set_ids = [], []
    for item in client.get_paginated(ep["dynamic_sets"], ref_id="dynamic_sets"):
        sid = first(item, "id", "set_id", "uuid")
        if sid is None:
            continue
        sid = str(sid)
        set_ids.append(sid)
        set_rows.append({
            "snapshot_date": snapshot_date,
            "set_id": sid,
            "name": as_str(first(item, "name", "title", "label")),
            "listing_count": to_float(first(item, "listing_count", "count", "num_listings")),
            "raw_json": dumps(item),
            "pulled_at": pulled_at,
        })
    db.upsert(conn, "dynamic_sets", set_rows)

    if limit:
        set_ids = set_ids[:limit]

    real, assoc_total = {}, 0
    for sid in set_ids:
        cnt, light = _pull_associated(client, conn, cfg, snapshot_date, sid)
        assoc_total += cnt
        for rl in light:
            real.setdefault(rl["listing_id"], rl)

    listing_rows = [{
        "snapshot_date": snapshot_date,
        "listing_id": rl["listing_id"],
        "channel": rl.get("channel"),
        "name": rl.get("name"),
        "bedrooms": rl.get("bedrooms"),
        "market_id": rl.get("market_id"),
        "raw_json": None,
        "pulled_at": pulled_at,
    } for rl in real.values()]
    db.upsert(conn, "listings", listing_rows)

    print(f"  dynamic_sets: {len(set_rows)} listed; "
          f"real (associated) listings harvested: {len(real)}")
    return set_ids, list(real.values()), assoc_total


def download_dynamic_sets(client, conn, cfg, snapshot_date, limit=None):
    """Full dynamic-set pull. Returns the real listings harvested from the sets so
    the caller can drive per-listing neighborhood pulls off the real portfolio."""
    p = cfg.get("params", {})
    set_ids, real_listings, assoc_total = collect_associated_listings(
        client, conn, cfg, snapshot_date, limit=limit)
    print(f"  pulling detail for {len(set_ids)} sets")

    start_date, end_date = year_window(snapshot_date, p.get("history_years", 2))
    months = month_firsts(snapshot_date, p.get("distribution_months", 3))
    print(f"    date range: {start_date} .. {end_date}")

    pull = cfg.get("pull", {})
    want = {
        "members": pull.get("set_members", True),
        "time_series": pull.get("set_time_series", False),
        "distributions": pull.get("set_distributions", False),
        "changelog": pull.get("set_changelog", False),
    }
    totals = {"associated_listings": assoc_total, "members": 0,
              "aggregated_metrics": 0, "time_series": 0,
              "distributions": 0, "changelog": 0}

    for sid in set_ids:
        if want["members"]:
            totals["members"] += _pull_members(client, conn, cfg, snapshot_date, sid)
        # aggregated_metrics (monthly) is always pulled — it feeds a CSV.
        totals["aggregated_metrics"] += _pull_aggregated(
            client, conn, cfg, snapshot_date, sid, start_date, end_date)
        if want["time_series"]:
            totals["time_series"] += _pull_time_series(
                client, conn, cfg, snapshot_date, sid, start_date, end_date)
        if want["distributions"]:
            totals["distributions"] += _pull_distributions(
                client, conn, cfg, snapshot_date, sid, months)
        if want["changelog"]:
            totals["changelog"] += _pull_changelog(
                client, conn, cfg, snapshot_date, sid, start_date, end_date)

    print("  dynamic set detail rows: " +
          ", ".join(f"{k}={v}" for k, v in totals.items() if want.get(k, True)))
    return real_listings


def _metric_names(cfg, kind):
    """Configured metric list for an endpoint family ([] = keep everything)."""
    return list(((cfg.get("params") or {}).get("metrics") or {}).get(kind) or [])


def _metric_param(cfg, kind):
    """Server-side metric filter.

    Must be `metric[]=a&metric[]=b` — plain repeated `metric=` silently keeps only
    the LAST value, and a comma-separated list 400s.
    """
    metrics = _metric_names(cfg, kind)
    return {"metric[]": metrics} if metrics else {}


def _metric_filter(cfg, kind):
    """Set of metric names to keep when filtering client-side."""
    return set(_metric_names(cfg, kind))


def _safe(client, path, ref, params=None):
    try:
        return client.get_json(path, params=params, ref_id=ref)
    except WheelhouseError as exc:
        print(f"    {ref} skipped: {exc}")
        return None


def _pull_associated(client, conn, cfg, snapshot_date, sid):
    """Store the set's associated (our real) listings. Returns (row_count, light)
    where light lists {listing_id, channel, market_id, name, bedrooms} per listing,
    keyed by wheelhouse_id (the numeric id the per-listing endpoints expect)."""
    body = _safe(client, cfg["endpoints"]["dynamic_set_associated_listings"].format(set_id=sid),
                 f"ds_associated:{sid}")
    if body is None:
        return 0, []
    rows, light = [], []
    for item in _extract_list(body):
        lid = _listing_id(item)
        if lid is None:
            continue
        rows.append({
            "snapshot_date": snapshot_date, "set_id": sid,
            "user_listing_id": lid,
            "raw_json": dumps(item) if isinstance(item, dict) else None,
        })
        if isinstance(item, dict):
            # Per-listing endpoints (neighborhood/*) key on the channel listing id
            # (the `id` field, e.g. the Guesty id) + channel — NOT wheelhouse_id.
            light.append({
                "listing_id": lid,
                "channel": as_str(first(item, "channel", "channel_name", "platform")),
                "market_id": as_str(first(item, "market_id", "market")),
                "name": as_str(first(item, "nickname", "title", "name")),
                "bedrooms": to_float(first(item, "num_bedrooms", "bedrooms", "beds")),
            })
    return db.upsert(conn, "dynamic_set_associated_listings", rows), light


def _pull_members(client, conn, cfg, snapshot_date, sid):
    body = _safe(client, cfg["endpoints"]["dynamic_set_listings"].format(set_id=sid),
                 f"ds_members:{sid}")
    if body is None:
        return 0
    rows = [
        {"snapshot_date": snapshot_date, "set_id": sid, "member_listing_id": lid,
         "status": status, "raw_json": dumps(item) if isinstance(item, dict) else None}
        for (status, lid, item) in flatten_members(body)
    ]
    return db.upsert(conn, "dynamic_set_members", rows)


def _pull_aggregated(client, conn, cfg, snapshot_date, sid, start_date, end_date):
    body = _safe(client, cfg["endpoints"]["dynamic_set_aggregated_metrics"].format(set_id=sid),
                 f"ds_agg:{sid}")
    if body is None:
        return 0
    # aggregated_metrics is a monthly series: {data:[{start_date,end_date,occupancy,
    # adr,revenue,...}]}. Treat start_date as the period and each numeric column as a
    # metric. The endpoint returns the set's full history (back to ~2017), so trim it
    # to the configured window. Fall back to a flat metric dict if that yields nothing.
    series = flatten_series(body, date_keys=("start_date", "end_date", "date", "month"))
    keep = _metric_filter(cfg, "aggregated")
    rows = [
        {"snapshot_date": snapshot_date, "set_id": sid, "metric": m,
         "period": d, "value": v, "raw_json": None}
        for (d, m, v) in series
        if in_window(d, start_date, end_date) and (not keep or m in keep)
    ]
    if not rows:
        rows = [
            {"snapshot_date": snapshot_date, "set_id": sid, "metric": m,
             "period": p, "value": v, "raw_json": None}
            for (m, p, v) in flatten_metrics(body)
        ]
    return db.upsert(conn, "dynamic_set_aggregated_metrics", rows)


def _pull_time_series(client, conn, cfg, snapshot_date, sid, start_date, end_date):
    """Pull a set's daily time series, widest window first.

    Dynamic-set history is shallower than market history (~24 months). If the
    requested range starts before the set has data, the API answers 204 for the
    WHOLE range instead of clipping — so step the start forward a year at a time
    until we get data.
    """
    path = cfg["endpoints"]["dynamic_set_time_series"].format(set_id=sid)
    metric_p = _metric_param(cfg, "time_series")
    for start in _start_candidates(start_date, end_date):
        body = _safe(client, path, f"ds_ts:{sid}",
                     params={"start_date": start, "end_date": end_date, **metric_p})
        if not body:
            continue
        rows = [
            {"snapshot_date": snapshot_date, "set_id": sid, "date": d,
             "metric": m, "value": v, "raw_json": None}
            for (d, m, v) in flatten_series(body)
        ]
        if rows:
            return db.upsert(conn, "dynamic_set_time_series", rows)
    return 0


def _start_candidates(start_date, end_date):
    """[configured start, Jan 1 of each later year ... , end's year] — widest first."""
    from datetime import date as _date
    y0 = int(start_date[:4])
    y1 = int(end_date[:4])
    out = [start_date]
    out += [_date(y, 1, 1).isoformat() for y in range(y0 + 1, y1 + 1)]
    return list(dict.fromkeys(out))


def _pull_distributions(client, conn, cfg, snapshot_date, sid, months):
    total = 0
    for month in months:
        body = _safe(client, cfg["endpoints"]["dynamic_set_distribution"].format(set_id=sid),
                     f"ds_dist:{sid}:{month}",
                     params={"month": month, **_metric_param(cfg, "distribution")})
        if not body:
            continue
        rows = [
            {"snapshot_date": snapshot_date, "set_id": sid, **row}
            for row in flatten_distribution(body)
        ]
        total += db.upsert(conn, "dynamic_set_distributions", rows)
    return total


def _pull_changelog(client, conn, cfg, snapshot_date, sid, start_date, end_date):
    body = _safe(client, cfg["endpoints"]["dynamic_set_changelog"].format(set_id=sid),
                 f"ds_changelog:{sid}")
    if body is None:
        return 0
    rows = []
    for item in _extract_list(body):
        if not isinstance(item, dict):
            continue
        # keep only changes inside the configured window (undated rows are kept)
        when = as_str(first(item, "date", "changed_at", "created_at", "timestamp"))
        if when and not in_window(when, start_date, end_date):
            continue
        rows.append({
            "snapshot_date": snapshot_date, "set_id": sid,
            "change_date": as_str(first(item, "date", "changed_at", "created_at", "timestamp")) or "",
            "change_type": as_str(first(item, "type", "change_type", "action")) or "",
            "detail": as_str(first(item, "detail", "description", "message", "field")) or "",
            "raw_json": dumps(item),
        })
    return db.upsert(conn, "dynamic_set_changelog", rows)
