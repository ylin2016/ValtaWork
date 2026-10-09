"""Scale normalization and shared config helpers.

The one non-obvious rule in this codebase: review scores arrive on different
scales per channel. Airbnb / Vrbo / Expedia report 1..5; Booking.com reports
1..10. Everything here is converted to a single 0..5 scale via the per-channel
`divisor` in config.yml so scores are comparable across sources.
"""
from __future__ import annotations

import os
import re
from functools import lru_cache

import pandas as pd
import yaml

# Project root = parent of this file's directory (src/..)
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def project_path(rel: str) -> str:
    """Resolve a config-relative path against the project root."""
    return rel if os.path.isabs(rel) else os.path.join(ROOT, rel)


@lru_cache(maxsize=1)
def load_config() -> dict:
    with open(os.path.join(ROOT, "config.yml")) as fh:
        return yaml.safe_load(fh)


def channel_family(channel_id: str, cfg: dict) -> str:
    info = cfg["channels"].get(channel_id)
    return info["family"] if info else "other"


def channel_divisor(channel_id: str, cfg: dict) -> float:
    info = cfg["channels"].get(channel_id)
    return float(info["divisor"]) if info else 1.0


def canon_listing(name) -> str | None:
    """Canonical join/group key for a listing nickname: trim, collapse internal
    whitespace, casefold. Merges stray variants like 'Burien 14407 top' /
    'Burien 14407 Top'. Used to match reviews <-> baseline `Listing`."""
    if name is None or (isinstance(name, float) and pd.isna(name)):
        return None
    s = re.sub(r"\s+", " ", str(name).strip())
    return s.casefold() or None


@lru_cache(maxsize=1)
def allowed_listing_keys() -> frozenset | None:
    """Canonicalized listing keys from the Listing_contacts allow-list.

    Returns None when `restrict_to_contacts` is off (or the file is missing) —
    callers treat None as 'no restriction'. The CSV's `Property` column is the
    listing name; it's canonicalized to match the review `listing_key`."""
    cfg = load_config()
    if not cfg.get("restrict_to_contacts"):
        return None
    path = project_path(cfg["paths"]["listing_contacts"])
    if not os.path.exists(path):
        return None
    contacts = pd.read_csv(path)
    keys = {canon_listing(p) for p in contacts["Property"]}
    keys.discard(None)
    return frozenset(keys)


def month_of(created_at) -> str | None:
    """YYYY-MM from a createdAt value (string or datetime)."""
    ts = pd.to_datetime(created_at, errors="coerce")
    return None if pd.isna(ts) else ts.strftime("%Y-%m")


def _add_listing_labels(out: pd.DataFrame) -> pd.DataFrame:
    """Per listing_key, pick a representative nickname spelling as the display
    `listing`, and the curated Property (if any row has one) as property_alias."""
    def _mode(s: pd.Series):
        s = s.dropna()
        return s.mode().iat[0] if not s.empty else None

    disp = out.groupby("listing_key")["nickname"].agg(_mode)
    alias = out.groupby("listing_key")["property"].agg(_mode)
    out["listing"] = out["listing_key"].map(disp)
    out["property_alias"] = out["listing_key"].map(alias)
    return out


def normalize_reviews(df: pd.DataFrame, cfg: dict) -> pd.DataFrame:
    """Return a tidy frame: one row per review with raw + 5-scale columns.

    Adds: channel_family, month, created_dt, and `<cat>_5` for each score
    column (overall_5, cleanliness_5, ...). Booking.com scores are halved.
    """
    out = pd.DataFrame()
    out["reservation_id"] = df["reservationId"]
    out["channel_id"] = df["channelId"]
    out["channel_family"] = df["channelId"].map(lambda c: channel_family(c, cfg))
    out["nickname"] = df["nickname"].astype("string").str.strip()
    out["property"] = df["Property"].astype("string").str.strip()  # curated alias, sparse
    # listing_key = canonical grouping/join key (always present, from nickname);
    # listing = a readable display label; property_alias = curated name if any.
    out["listing_key"] = out["nickname"].map(canon_listing)
    out = _add_listing_labels(out)
    out["guest_name"] = df.get("Guest name")
    out["created_dt"] = pd.to_datetime(df["createdAt"], errors="coerce")
    out["month"] = out["created_dt"].dt.strftime("%Y-%m")
    out["check_in"] = pd.to_datetime(df.get("Check in"), errors="coerce")
    out["check_out"] = pd.to_datetime(df.get("Check out"), errors="coerce")
    out["public_review"] = df.get("Public Review")
    out["booking_positive"] = df.get("Booking.com Positive content")
    out["booking_negative"] = df.get("Booking.com Negative content")

    divisor = df["channelId"].map(lambda c: channel_divisor(c, cfg))
    for raw_col, tidy in cfg["score_columns"].items():
        raw = pd.to_numeric(df[raw_col], errors="coerce")
        out[f"{tidy}_raw"] = raw
        out[f"{tidy}_5"] = (raw / divisor).round(4)

    return out
