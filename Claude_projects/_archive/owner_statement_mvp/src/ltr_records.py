"""Shared logic for sourcing LTR rent + deferred-revenue lines from the period's
LTR CSV (data/LTR_<period>.csv), used by BOTH the Excel writer (run_month_close)
and the Streamlit dashboard so they can't drift.

Each CSV row becomes one Net-Revenue line = the full monthly rent for that property
(the CSV already consolidates multiple deposits into one row per property). Deferred
bookings that already exist as Guesty bookings are skipped (they render as STR
bookings). `covered_pids` is the set of property_ids that DID get a line — callers
use it to drop those properties' rent from the "Other Credits" section without
hiding rent for properties that have no CSV line.
"""
import re
import pandas as pd
from pathlib import Path

_ALIASES = {"bellevue 14507u3": "bellevue_14507_unit_3"}


def to_property_id(listing) -> str:
    s = str(listing).strip().lower()
    if s in _ALIASES:
        return _ALIASES[s]
    s = re.sub(r"[^a-z0-9]+", "_", s)
    return re.sub(r"_+", "_", s).strip("_")


def _money(v) -> float:
    if v is None:
        return 0.0
    try:
        if pd.isna(v):
            return 0.0
    except (TypeError, ValueError):
        pass
    s = str(v).replace("$", "").replace(",", "").strip()
    try:
        return float(s) if s else 0.0
    except ValueError:
        return 0.0


def _date(v):
    try:
        return pd.to_datetime(v).strftime("%Y-%m-%d")
    except Exception:
        return None


def csv_path(base_dir, period) -> Path:
    return Path(base_dir) / f"data/LTR_{period}.csv"


def build_records(base_dir, period: str, members, is_guesty_code):
    """Return (records, covered_pids).

    records: one dict per qualifying CSV row, with keys
        property_id, booking_id, guest_name, checkin, checkout,
        net_revenue, gross_revenue, is_ltr.
    covered_pids: set of property_ids that produced at least one record.

    `members` is the property_id plus any rollup members. `is_guesty_code(code)`
    must return True when a deferred booking is already in the ledger as a Guesty
    booking (so it is not shown twice).
    """
    path = csv_path(base_dir, period)
    if not path.exists():
        return [], set()

    df = pd.read_csv(str(path), encoding="utf-8-sig")
    member_set = set(members)
    records, covered = [], set()

    for _, r in df.iterrows():
        pid = to_property_id(r.get("Listing"))
        if pid not in member_set:
            continue
        is_ltr = str(r.get("Source", "")).strip().upper() == "LTR"
        code = str(r.get("Confirmation.Code", "")).strip()
        if not is_ltr and is_guesty_code(code):
            continue
        net = _money(r.get("Net Revenue"))
        records.append({
            "property_id": pid,
            "booking_id": code,
            "guest_name": str(r.get("Tenant", "")),
            "checkin": _date(r.get("Checkin_date")),
            "checkout": _date(r.get("Checkout_date")),
            "net_revenue": net,
            "gross_revenue": _money(r.get("Total Payout")) or net,
            "is_ltr": is_ltr,
        })
        covered.add(pid)

    return records, covered


def is_rent_income(source_object, description) -> bool:
    """True for ledger income lines that represent rent (LTR/DEFERRED tag or 'rent'
    in the description) — i.e. candidates to move out of Other Credits."""
    if str(source_object or "") in ("LTR", "DEFERRED"):
        return True
    return "rent" in str(description or "").lower()
