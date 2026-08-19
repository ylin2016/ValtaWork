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
import calendar
import pandas as pd
from pathlib import Path

# Works both as a package module (run_month_close) and as a top-level import
# (the Streamlit dashboard inserts src/ on sys.path).
try:
    from ..paths import ltr_csv as _ltr_csv
except ImportError:
    from paths import ltr_csv as _ltr_csv

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
    # base_dir kept for signature compatibility; location now lives in paths.py.
    return _ltr_csv(period)


def build_records(base_dir, period: str, members, is_guesty_code):
    """Return (records, covered_pids).

    records: one dict per qualifying CSV row, with keys
        property_id, booking_id, guest_name, checkin, checkout,
        net_revenue, cleaning_fee, gross_revenue, is_ltr.
    net_revenue is the accommodation fare (commissioned rent); cleaning_fee is the
    (non-commissioned) cleaning fee from the CSV's `Cleaning.Fee` column (0 if blank);
    gross_revenue = net_revenue + cleaning_fee (the "Total Payout").
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
        cleaning = _money(r.get("Cleaning.Fee"))
        records.append({
            "property_id": pid,
            "booking_id": code,
            "guest_name": str(r.get("Tenant", "")),
            "checkin": _date(r.get("Checkin_date")),
            "checkout": _date(r.get("Checkout_date")),
            "net_revenue": net,
            "cleaning_fee": cleaning,
            "gross_revenue": round(net + cleaning, 2),   # Total Payout = fare + cleaning
            "is_ltr": is_ltr,
        })
        covered.add(pid)

    return records, covered


def ltr_claimed_codes(conn, period: str) -> set:
    """Confirmation codes that are now represented ONLY by an LTR/DEFERRED ledger row
    for `period` — i.e. the LTR source claimed the booking and no Guesty INCOME row
    survives for it (import_ltr's `_replace_colliding` deleted it, or guesty-import
    never kept it). This is the "dedup after the Guesty pull": the Guesty booking
    breakdown must DROP these codes so Section 1 reflects the post-dedup ledger (LTR
    wins) and foots with the Net Revenue section, where the booking shows once as an
    LTR/deferred line. Codes that still have a surviving Guesty row are NOT returned
    (the Guesty breakdown row stays, matching Section 2)."""
    y, m = map(int, period.split("-"))
    start = f"{y:04d}-{m:02d}-01"
    end = f"{y:04d}-{m:02d}-{calendar.monthrange(y, m)[1]:02d}"
    ltr_codes = {str(r[0]) for r in conn.execute(
        """SELECT DISTINCT source_txn_id FROM ledger_lines
           WHERE source_object IN ('LTR','DEFERRED') AND source_txn_id IS NOT NULL
             AND posting_date>=? AND posting_date<=?""", (start, end))}
    if not ltr_codes:
        return set()
    guesty_codes = {str(r[0]) for r in conn.execute(
        """SELECT DISTINCT source_txn_id FROM ledger_lines
           WHERE source='guesty' AND category='INCOME' AND source_txn_id IS NOT NULL
             AND posting_date>=? AND posting_date<=?""", (start, end))}
    return ltr_codes - guesty_codes


def is_rent_income(source_object, description) -> bool:
    """True for ledger income lines that represent rent (LTR/DEFERRED tag or 'rent'
    in the description) — i.e. candidates to move out of Other Credits."""
    if str(source_object or "") in ("LTR", "DEFERRED"):
        return True
    return "rent" in str(description or "").lower()
