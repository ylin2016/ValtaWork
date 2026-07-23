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
    _extract_list, date_window, month_firsts, _listing_id,
)
from .wheelhouse_client import WheelhouseError


def download_dynamic_sets(client, conn, cfg, snapshot_date, limit=None):
    ep = cfg["endpoints"]
    p = cfg.get("params", {})
    pulled_at = datetime.now(timezone.utc).isoformat()

    # 1) List all dynamic sets.
    set_rows = []
    set_ids = []
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
    print(f"  dynamic_sets: {len(set_rows)} listed, pulling detail for {len(set_ids)}")

    start_date, end_date = date_window(
        snapshot_date, p.get("history_days", 90), p.get("forward_days", 365))
    months = month_firsts(snapshot_date, p.get("distribution_months", 3))

    totals = {k: 0 for k in (
        "associated_listings", "members", "aggregated_metrics",
        "time_series", "distributions", "changelog")}

    for sid in set_ids:
        totals["associated_listings"] += _pull_associated(client, conn, cfg, snapshot_date, sid)
        totals["members"] += _pull_members(client, conn, cfg, snapshot_date, sid)
        totals["aggregated_metrics"] += _pull_aggregated(client, conn, cfg, snapshot_date, sid)
        totals["time_series"] += _pull_time_series(
            client, conn, cfg, snapshot_date, sid, start_date, end_date)
        totals["distributions"] += _pull_distributions(
            client, conn, cfg, snapshot_date, sid, months)
        totals["changelog"] += _pull_changelog(client, conn, cfg, snapshot_date, sid)

    print("  dynamic set detail rows: " +
          ", ".join(f"{k}={v}" for k, v in totals.items()))


def _safe(client, path, ref, params=None):
    try:
        return client.get_json(path, params=params, ref_id=ref)
    except WheelhouseError as exc:
        print(f"    {ref} skipped: {exc}")
        return None


def _pull_associated(client, conn, cfg, snapshot_date, sid):
    body = _safe(client, cfg["endpoints"]["dynamic_set_associated_listings"].format(set_id=sid),
                 f"ds_associated:{sid}")
    if body is None:
        return 0
    rows = []
    for item in _extract_list(body):
        lid = _listing_id(item)
        if lid is None:
            continue
        rows.append({
            "snapshot_date": snapshot_date, "set_id": sid,
            "user_listing_id": lid,
            "raw_json": dumps(item) if isinstance(item, dict) else None,
        })
    return db.upsert(conn, "dynamic_set_associated_listings", rows)


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


def _pull_aggregated(client, conn, cfg, snapshot_date, sid):
    body = _safe(client, cfg["endpoints"]["dynamic_set_aggregated_metrics"].format(set_id=sid),
                 f"ds_agg:{sid}")
    if body is None:
        return 0
    rows = [
        {"snapshot_date": snapshot_date, "set_id": sid, "metric": m,
         "period": p, "value": v, "raw_json": None}
        for (m, p, v) in flatten_metrics(body)
    ]
    return db.upsert(conn, "dynamic_set_aggregated_metrics", rows)


def _pull_time_series(client, conn, cfg, snapshot_date, sid, start_date, end_date):
    body = _safe(client, cfg["endpoints"]["dynamic_set_time_series"].format(set_id=sid),
                 f"ds_ts:{sid}", params={"start_date": start_date, "end_date": end_date})
    if body is None:
        return 0
    rows = [
        {"snapshot_date": snapshot_date, "set_id": sid, "date": d,
         "metric": m, "value": v, "raw_json": None}
        for (d, m, v) in flatten_series(body)
    ]
    return db.upsert(conn, "dynamic_set_time_series", rows)


def _pull_distributions(client, conn, cfg, snapshot_date, sid, months):
    total = 0
    for month in months:
        body = _safe(client, cfg["endpoints"]["dynamic_set_distribution"].format(set_id=sid),
                     f"ds_dist:{sid}:{month}", params={"month": month})
        if body is None:
            continue
        rows = [
            {"snapshot_date": snapshot_date, "set_id": sid, **row}
            for row in flatten_distribution(body)
        ]
        total += db.upsert(conn, "dynamic_set_distributions", rows)
    return total


def _pull_changelog(client, conn, cfg, snapshot_date, sid):
    body = _safe(client, cfg["endpoints"]["dynamic_set_changelog"].format(set_id=sid),
                 f"ds_changelog:{sid}")
    if body is None:
        return 0
    rows = []
    for item in _extract_list(body):
        if not isinstance(item, dict):
            continue
        rows.append({
            "snapshot_date": snapshot_date, "set_id": sid,
            "change_date": as_str(first(item, "date", "changed_at", "created_at", "timestamp")) or "",
            "change_type": as_str(first(item, "type", "change_type", "action")) or "",
            "detail": as_str(first(item, "detail", "description", "message", "field")) or "",
            "raw_json": dumps(item),
        })
    return db.upsert(conn, "dynamic_set_changelog", rows)
