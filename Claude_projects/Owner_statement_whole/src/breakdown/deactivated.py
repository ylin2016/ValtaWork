"""Fold deactivated-listing reservations into the monthly pull.

The Guesty Open API silently excludes reservations on **deactivated**
(active=false) listings, but the Guesty UI CSV export includes them (see
GuestyFinancials CLAUDE.md — July 2026 had ~8-14 such rows on a few listings).
`merge_deactivated` reads a Guesty UI export and appends ONLY the reservations
whose confirmation code the API did not return, mapping the UI columns into the
summary-frame shape so both adapters (UI export + payment breakdown) process
them uniformly. Each added row is tagged `data_source='CSV (deactivated listing)'`.

Note: the UI export carries a single TOTAL TAXES (not itemized), so it is placed
in one `tax_other` bucket — the tax TOTAL is correct (what net revenue needs);
the per-type split is not available for these few rows.
"""
import pandas as pd


def _f(v) -> float:
    if pd.isna(v):
        return 0.0
    s = str(v).replace("$", "").replace(",", "").strip()
    try:
        return float(s) if s else 0.0
    except ValueError:
        return 0.0


def merge_deactivated(S: pd.DataFrame, ui_csv_path) -> pd.DataFrame:
    """Return S with API-missing UI rows appended (mapped to summary shape)."""
    ui = pd.read_csv(ui_csv_path, encoding="utf-8-sig")
    have = set(S["confirmationCode"].astype(str)) if "confirmationCode" in S else set()

    rows = []
    for _, r in ui.iterrows():
        code = str(r.get("CONFIRMATION CODE", "")).strip()
        if not code or code in have:
            continue  # add-only-missing: skip anything the API already returned
        rows.append({
            "confirmationCode": code,
            "source": r.get("SOURCE"),
            "status": str(r.get("STATUS", "")).strip(),
            "listing": r.get("LISTING'S NICKNAME"),
            "guest": r.get("GUEST"),
            "checkIn": r.get("CHECK-IN"),
            "checkOut": r.get("CHECK-OUT"),
            "nights": _f(r.get("NUMBER OF NIGHTS")),
            "guestsCount": _f(r.get("NUMBER OF GUESTS")),
            "accommodation": _f(r.get("ACCOMMODATION FARE")),
            "cleaning": _f(r.get("CLEANING FARE")),
            "pet_fee": _f(r.get("PET FEE")),
            "total_fees": _f(r.get("TOTAL FEES")),
            "airbnb_resolution_center": _f(r.get("AIRBNB RESOLUTION CENTER")),
            "total_refunded": _f(r.get("TOTAL REFUNDED")),
            "total_paid": _f(r.get("TOTAL PAID")),
            "host_payout": _f(r.get("TOTAL PAYOUT")),
            # channel commission -> negative host_channel_fee line (drives has_pcm).
            "host_channel_fee": -abs(_f(r.get("CHANNEL COMMISSION"))),
            # single UI tax total -> one bucket (total is right; split unavailable).
            "tax_other": _f(r.get("TOTAL TAXES")),
            "service_fee": 0.0,
            "data_source": "CSV (deactivated listing)",
        })

    if not rows:
        print("deactivated merge: no API-missing rows found in the UI export")
        return S
    add = pd.DataFrame(rows)
    print(f"deactivated merge: added {len(add)} reservation(s) not returned by the API")
    return pd.concat([S, add], ignore_index=True)
