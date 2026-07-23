"""Tolerant helpers to normalize Wheelhouse JSON into flat table rows.

The public docs don't fully pin down field names, so these helpers look for a
handful of likely keys and always fall back to storing the raw JSON. Once the
live payload shapes are confirmed, tighten the key lists here.
"""
import json
from datetime import date, timedelta

DATE_KEYS = ("date", "day", "stay_date", "ds", "month")
METRIC_NAME_KEYS = ("metric", "name", "key", "metric_name")
VALUE_KEYS = ("value", "val", "amount", "y")
BUCKET_KEYS = ("bucket", "bin", "range", "label", "x")


def date_window(snapshot_date: str, back_days: int, forward_days: int):
    """Return (start_date, end_date) ISO strings around the snapshot date."""
    d = date.fromisoformat(snapshot_date)
    return (
        (d - timedelta(days=back_days)).isoformat(),
        (d + timedelta(days=forward_days)).isoformat(),
    )


def month_firsts(snapshot_date: str, n: int):
    """First-of-month ISO dates starting with the snapshot's month, n months forward."""
    d = date.fromisoformat(snapshot_date).replace(day=1)
    out = []
    y, m = d.year, d.month
    for _ in range(max(1, n)):
        out.append(date(y, m, 1).isoformat())
        m += 1
        if m > 12:
            m = 1
            y += 1
    return out


def first(d: dict, *keys, default=None):
    for k in keys:
        if isinstance(d, dict) and d.get(k) is not None:
            return d[k]
    return default


def to_float(v):
    if v is None or v == "":
        return None
    try:
        return float(v)
    except (TypeError, ValueError):
        return None


def as_str(v):
    return None if v is None else str(v)


def dumps(v) -> str:
    return json.dumps(v, default=str)


def flatten_series(records, date_keys=DATE_KEYS):
    """Turn a daily/period time series into (date, metric, value) tuples.

    Handles two shapes:
      A) row-per-date:   {"date": "2026-07-01", "adr": 210, "occupancy": 0.72, ...}
      B) series-per-metric: {"metric": "adr", "series": [{"date":..,"value":..}, ...]}
    """
    out = []  # list of (date, metric, value)
    if isinstance(records, dict):
        records = _extract_list(records)
    for rec in records or []:
        if not isinstance(rec, dict):
            continue
        metric_name = first(rec, *METRIC_NAME_KEYS)
        series = rec.get("series") or rec.get("values") or rec.get("data")
        if metric_name and isinstance(series, list):
            # shape B
            for pt in series:
                if not isinstance(pt, dict):
                    continue
                d = first(pt, *date_keys)
                v = to_float(first(pt, *VALUE_KEYS))
                if d is not None:
                    out.append((as_str(d), str(metric_name), v))
            continue
        # shape A: one row, date + every numeric column becomes a metric
        d = first(rec, *date_keys)
        if d is None:
            continue
        for k, val in rec.items():
            if k in date_keys:
                continue
            fv = to_float(val)
            if fv is not None:
                out.append((as_str(d), str(k), fv))
    return out


def flatten_distribution(body):
    """Turn a distribution payload into dist rows.

    Primary (documented) shape:
      {"currency":..., "month":"2026-07-01", "updated_at":...,
       "data": {"adr_w_fees": [{"bucket_min_incl":100,"bucket_max_excl":150,
                                 "probability":0.2,"percentile":0.4}, ...], ...}}
    Also tolerates the older list-of-{metric,buckets:[...]} shape.

    Returns dicts: month, metric, bucket, bucket_min, bucket_max, probability,
    percentile, value(=probability), raw.
    """
    out = []
    if isinstance(body, dict) and isinstance(body.get("data"), dict):
        month = as_str(first(body, "month", "date", "period", default="")) or ""
        for metric, buckets in body["data"].items():
            if not isinstance(buckets, list):
                continue
            for b in buckets:
                if isinstance(b, dict):
                    out.append(_dist_row(month, metric, b))
        return out

    # fallback: list-of-{metric, buckets:[...]} or flat rows
    for rec in _extract_list(body):
        if not isinstance(rec, dict):
            continue
        month = as_str(first(rec, "month", "date", "period", default="")) or ""
        metric = first(rec, *METRIC_NAME_KEYS, default="")
        buckets = rec.get("buckets") or rec.get("bins") or rec.get("distribution")
        if isinstance(buckets, list):
            for b in buckets:
                if isinstance(b, dict):
                    out.append(_dist_row(month, metric, b))
        else:
            out.append(_dist_row(month, metric, rec))
    return out


def _dist_row(month, metric, b):
    bmin = to_float(first(b, "bucket_min_incl", "bucket_min", "min"))
    bmax = to_float(first(b, "bucket_max_excl", "bucket_max", "max"))
    prob = to_float(first(b, "probability", "count", "frequency", *VALUE_KEYS))
    pct = to_float(first(b, "percentile"))
    bucket = first(b, *BUCKET_KEYS)
    if bucket is None:
        bucket = f"{bmin}-{bmax}"
    return {
        "month": month, "metric": str(metric), "bucket": as_str(bucket),
        "bucket_min": bmin, "bucket_max": bmax, "probability": prob,
        "percentile": pct, "value": prob, "raw_json": dumps(b),
    }


def flatten_members(body):
    """Parse set member listings into (status, listing_id, raw) tuples.

    Documented shape: {"active":[...], "hidden":[...], "review":[...], "removed":[...]}.
    Each entry is a listing id or an object with an id. Falls back to a flat list.
    """
    STATUSES = ("active", "hidden", "review", "removed")
    container = body.get("data", body) if isinstance(body, dict) else body
    out = []
    if isinstance(container, dict) and any(k in container for k in STATUSES):
        for status in STATUSES:
            for item in container.get(status) or []:
                lid = _listing_id(item)
                if lid is not None:
                    out.append((status, lid, item))
        return out
    for item in _extract_list(body):
        lid = _listing_id(item)
        if lid is not None:
            out.append(("", lid, item))
    return out


def _listing_id(item):
    if isinstance(item, (str, int)):
        return str(item)
    if isinstance(item, dict):
        v = first(item, "id", "listing_id", "uuid")
        return None if v is None else str(v)
    return None


def flatten_metrics(obj):
    """Turn an aggregated-metrics object into (metric, period, value) tuples.

    Handles {"adr": 210, "occupancy_30": 0.7} and
    {"metrics": [{"metric":"adr","period":"30","value":210}, ...]}.
    """
    out = []
    if isinstance(obj, dict):
        listy = obj.get("metrics") or obj.get("data")
        if isinstance(listy, list):
            for rec in listy:
                if not isinstance(rec, dict):
                    continue
                metric = first(rec, *METRIC_NAME_KEYS, default="")
                period = as_str(first(rec, "period", "window", "horizon", default="")) or ""
                value = to_float(first(rec, *VALUE_KEYS))
                out.append((str(metric), period, value))
            return out
        for k, v in obj.items():
            fv = to_float(v)
            if fv is not None:
                out.append((str(k), "", fv))
    return out


def _extract_list(body):
    if body is None:
        return []
    if isinstance(body, list):
        return body
    if not isinstance(body, dict):
        return [body]
    for key in ("data", "results", "items", "series", "time_series", "distribution",
                "distributions", "changelog", "listings", "members"):
        if isinstance(body.get(key), list):
            return body[key]
    return [body]
