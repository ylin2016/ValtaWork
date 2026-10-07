"""Pull confirmed Guesty reservations via the Open API and write them in the
Guesty UI "export reservations" CSV layout, so DataProcessing.format_reservation
reads it unchanged (replaces the manual Guesty_bookings_2026-YYYYMMDD.csv).

Uses Owner_statement_whole's Guesty client and its cached token
(config/secrets/guesty_token.json; 5 tokens/24h/client — never force-refresh). Deactivated listings are silently excluded by the Open
API; they are filled from a Guesty UI export (data/guesty/Guesty_UI_bookings_*.csv,
newest used) for listings the API returned nothing for. See CLAUDE.md.

    python fetch_guesty.py                       # check-in >= 2026-01-01, as of today
    python fetch_guesty.py --asof 2026-09-29
    python fetch_guesty.py --supplement-only     # re-apply the UI export to an existing pull
"""
import argparse
import json
import sys
from datetime import date
from pathlib import Path

import pandas as pd

from paths import DATA_DIR, OWNER_STATEMENT

sys.path.insert(0, str(OWNER_STATEMENT))
from src.guesty.client import GuestyClient  # noqa: E402  (Owner_statement_whole)
from src.breakdown.fetch_month import build_summary_frame  # noqa: E402  (Owner_statement_whole)

PAGE = 100
FIELDS = ("confirmationCode source status checkIn checkOut checkInDateLocalized "
          "checkOutDateLocalized confirmedAt createdAt nightsCount guestsCount "
          "numberOfGuests listing.nickname listing.title listingId guest.fullName "
          "integration.platform money specialRequests")
LOCAL_TZ = "America/Los_Angeles"

# UI export column order (Guesty_bookings_2026-*.csv)
UI_COLUMNS = [
    "CHECK-IN", "CHECK-OUT", "CONFIRMATION CODE", "TOTAL PAYOUT", "CONFIRMATION DATE",
    "NUMBER OF NIGHTS", "NUMBER OF GUESTS", "NUMBER OF ADULTS", "NUMBER OF CHILDREN",
    "NUMBER OF INFANTS", "LISTING'S NICKNAME", "LISTING'S TITLE", "SOURCE", "TOTAL PAID",
    "ALTERATION DATE", "TOTAL REFUNDED", "CLEANING FARE", "PET FEE", "ACCOMMODATION FARE",
    "EXTRA PERSON FEE", "GUEST", "PLATFORM",
]


def fetch_confirmed(client: GuestyClient, checkin_from: str) -> list[dict]:
    filt = [
        {"field": "checkIn", "operator": "$gte", "value": checkin_from},
        {"field": "status", "operator": "$in", "value": ["confirmed"]},
    ]
    out, skip = [], 0
    while True:
        r = client.get("/reservations", params={
            "filters": json.dumps(filt), "fields": FIELDS,
            "limit": PAGE, "skip": skip, "sort": "checkIn",
        })
        batch = r.get("results", [])
        out.extend(batch)
        total = r.get("count", len(out))
        skip += len(batch)
        if not batch or skip >= total:
            print(f"  guesty: {len(out)} confirmed reservations (check-in >= {checkin_from})")
            return out


def _local(ts):
    """UTC ISO timestamp -> 'YYYY-MM-DD HH:MM AM' Pacific (UI export format)."""
    if not ts:
        return None
    t = pd.Timestamp(ts)
    t = t.tz_localize("UTC") if t.tzinfo is None else t
    return t.tz_convert(LOCAL_TZ).strftime("%Y-%m-%d %I:%M %p")


def _items_sum(money: dict, types: set[str]) -> float:
    return round(sum(i.get("amount") or 0 for i in money.get("invoiceItems") or []
                     if i.get("normalType") in types), 2)


def to_ui_row(r: dict) -> dict:
    m = r.get("money") or {}
    g = r.get("numberOfGuests") or {}
    # UI "ACCOMMODATION FARE" = fareAccommodation + Markup + Extra person fee +
    # Weekly/Monthly discount. It EXCLUDES length-of-stay (LOSD), channel (GCD)
    # and fare (AFD) discounts and fare adjustments (AFA). Verified against the
    # 2026-09-22 UI export — do not swap in fareAccommodationAdjusted.
    fare = round((m.get("fareAccommodation") or 0)
                 + _items_sum(m, {"MAR", "EPF", "AFWD"}), 2)
    pet = sum(i.get("amount") or 0 for i in m.get("invoiceItems") or []
              if "pet" in (i.get("title") or "").lower())
    return {
        # check-in/out: localized DATE (the UTC checkIn can fall on the next day)
        "CHECK-IN": r.get("checkInDateLocalized"),
        "CHECK-OUT": r.get("checkOutDateLocalized"),
        "CONFIRMATION CODE": r.get("confirmationCode"),
        "TOTAL PAYOUT": m.get("hostPayout"),
        "CONFIRMATION DATE": _local(r.get("confirmedAt") or r.get("createdAt")),
        "NUMBER OF NIGHTS": r.get("nightsCount"),
        "NUMBER OF GUESTS": r.get("guestsCount"),
        "NUMBER OF ADULTS": g.get("numberOfAdults"),
        "NUMBER OF CHILDREN": g.get("numberOfChildren"),
        "NUMBER OF INFANTS": g.get("numberOfInfants"),
        "LISTING'S NICKNAME": (r.get("listing") or {}).get("nickname"),
        "LISTING'S TITLE": (r.get("listing") or {}).get("title"),
        "SOURCE": r.get("source"),
        "TOTAL PAID": m.get("totalPaid"),
        "ALTERATION DATE": None,
        "TOTAL REFUNDED": m.get("totalRefunded"),
        "CLEANING FARE": m.get("fareCleaning"),
        "PET FEE": round(pet, 2),
        "ACCOMMODATION FARE": fare,
        "EXTRA PERSON FEE": _items_sum(m, {"EPF"}) or None,
        "GUEST": (r.get("guest") or {}).get("fullName"),
        "PLATFORM": (r.get("integration") or {}).get("platform"),
    }


def supplement_from_ui(df: pd.DataFrame, ui_path: Path, checkin_from: str, log_path: Path) -> pd.DataFrame:
    """Add UI-export rows for listings the API returned NOTHING for (deactivated
    listings). Listings the API does return are left alone, so a stale export
    can't resurrect a booking cancelled since. Logs every added row."""
    ui = pd.read_csv(ui_path, dtype=str)
    if list(ui.columns) != UI_COLUMNS:
        raise RuntimeError(f"{ui_path.name}: columns differ from the Guesty UI export layout")
    api_listings = set(df["LISTING'S NICKNAME"].dropna())
    add = ui[~ui["LISTING'S NICKNAME"].isin(api_listings)
             & ~ui["CONFIRMATION CODE"].isin(df["CONFIRMATION CODE"])
             & (ui["CHECK-IN"].str[:10] >= checkin_from)]
    if not add.empty:   # don't clobber the log on an idempotent re-run
        add.assign(ui_export=ui_path.name).to_csv(log_path, index=False)
    by = add["LISTING'S NICKNAME"].value_counts()
    print(f"  guesty: +{len(add)} rows from {ui_path.name} for {len(by)} listing(s) not in the API: "
          + ", ".join(f"{k} ({v})" for k, v in by.items()))
    return pd.concat([df, add], ignore_index=True)


def latest_ui_export() -> Path | None:
    files = sorted((DATA_DIR / "guesty").glob("Guesty_UI_bookings_*.csv"))
    return files[-1] if files else None


def save_locations(client: GuestyClient) -> None:
    """Listing coordinates for the dashboard's comp-set maps (comp_map.py)."""
    rows, skip = [], 0
    while True:
        d = client.get("/listings", params={"limit": 100, "skip": skip, "fields": "nickname address active"})
        res = d.get("results", [])
        for r in res:
            a = r.get("address") or {}
            rows.append({"nickname": r.get("nickname"), "lat": a.get("lat"), "lng": a.get("lng"),
                         "full": a.get("full"), "active": r.get("active")})
        skip += len(res)
        if not res or skip >= d.get("count", 0):
            break
    out = DATA_DIR / "guesty" / "listing_locations.csv"
    pd.DataFrame(rows).to_csv(out, index=False)
    print(f"  guesty: {len(rows)} listing locations -> {out}")


def main(argv=None) -> Path:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--asof", default=date.today().isoformat(), help="Run date (YYYY-MM-DD)")
    ap.add_argument("--checkin-from", default="2026-01-01")
    ap.add_argument("--ui-export", default=None, help="Guesty UI export (default: newest in data/guesty)")
    ap.add_argument("--supplement-only", action="store_true",
                    help="Skip the API; re-apply the UI export to the existing pull for --asof")
    args = ap.parse_args(argv)
    tag = args.asof.replace("-", "")
    out = DATA_DIR / "guesty" / f"Guesty_bookings_2026-{tag}.csv"
    ui_path = Path(args.ui_export) if args.ui_export else latest_ui_export()
    log_path = DATA_DIR / "guesty" / f"ui_supplement_{tag}.csv"

    if args.supplement_only:
        df = pd.read_csv(out, dtype=str)
        before = len(df)
        if ui_path:
            df = supplement_from_ui(df, ui_path, args.checkin_from, log_path)
        df.to_csv(out, index=False)
        print(f"  guesty: {before} -> {len(df)} rows, wrote {out}")
        return out

    client = GuestyClient()
    raw = fetch_confirmed(client, args.checkin_from)
    try:
        save_locations(client)
    except Exception as e:   # maps fall back to the previous file
        print(f"  guesty: listing locations not refreshed ({e})")
    rows = [to_ui_row(r) for r in raw]
    df = pd.DataFrame(rows, columns=UI_COLUMNS)
    # per-booking itemization (fee categories, Stripe payment count, virtual card) in
    # Owner_statement_whole's summary layout — what its payment model reads
    summary = build_summary_frame(raw)
    summary_out = DATA_DIR / "guesty" / f"Guesty_summary_2026-{tag}.csv"
    # The API filters on UTC checkIn, which lets 12-31 local check-ins through;
    # those belong to the static 2025 file.
    df = df[df["CHECK-IN"] >= args.checkin_from].reset_index(drop=True)
    summary = summary[summary["confirmationCode"].isin(df["CONFIRMATION CODE"])]
    dup = df["CONFIRMATION CODE"].duplicated().sum()
    if dup:
        raise RuntimeError(f"{dup} duplicate confirmation codes in the confirmed pull")

    if ui_path:
        df = supplement_from_ui(df, ui_path, args.checkin_from, log_path)
    else:
        print("  guesty: no Guesty_UI_bookings_*.csv — deactivated listings are missing")

    out.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(out, index=False)
    summary.to_csv(summary_out, index=False)
    print(f"  guesty: wrote {out} (+ itemization {summary_out.name})")
    return out


if __name__ == "__main__":
    main()
