"""Task 1 orchestrator: pull one month from the Guesty Open API and write the
two derived inputs the downstream pipeline consumes.

    python -m src.breakdown.fetch_month --period 2026-07
    python -m src.breakdown.fetch_month --period 2026-07 --deactivated-csv <ui_export.csv>

ONE API pull -> two files in inputs/<period>/:
  A) Guesty_booking_<period>.csv   — UI-export-shaped; fed to convert_guesty_export.py
                                     (owner-statement net revenue stays single-sourced there).
  B) payment_breakdown_<period>.csv — per-booking fee waterfall (guest pay -> channel/guesty/
                                     stripe/cleaning/tax -> host payout -> net) for dashboard Section 1.

Both are built from the SAME reservation objects. TOTAL TAXES in (A) is the SUM of
itemized tax_* lines, not money.totalTaxes (which undercounts — see GuestyFinancials
CLAUDE.md). The Open API silently excludes deactivated (active=false) listings; pass
--deactivated-csv (a Guesty UI export filtered to those listings) to fold them in.
"""
import argparse
import calendar

import pandas as pd

from .. import paths
from .convert_export import to_property_id
from ..guesty.client import GuestyClient
from ..guesty.reservation_financials import build_breakdown, categorize_item
from ..guesty.reservations_batch import fetch_range, summary_row
from .payment_model import applied_adjustments, compute_breakdown

# Stable column order for the folded fee-category pivot (mirrors reservations_batch.main).
CAT_ORDER = [
    "accommodation_fare", "accommodation_adjustment", "accommodation_discount",
    "cleaning_fee", "pet_fee", "extra_person_fee", "parking_fee", "resort_fee",
    "service_fee", "management_fee", "damage_protection", "additional_fees", "other_fee",
    "tax_state", "tax_county", "tax_city", "tax_local", "tax_occupancy",
    "tax_residential", "tax_destination", "tax_reservation_total", "tax_other",
    "markup", "promotion", "discount_length_of_stay", "discount_channel",
    "discount_weekly_monthly", "airbnb_resolution_center", "host_channel_fee",
]

# summary FIELDS + guestsCount (needed for the UI-shaped export's NUMBER OF GUESTS)
# + specialRequests (Expedia stamps "Payment method: EXPEDIA VIRTUAL CARD" there, which
# selects the virtual-card channel-fee formula in payment_model).
FIELDS = ("confirmationCode source status checkIn checkOut nightsCount guestsCount "
          "listing.nickname listingId guest.fullName money specialRequests")




def build_summary_frame(reservations: list[dict]) -> pd.DataFrame:
    """One row per reservation: summary_row fields + folded fee categories +
    guestsCount. Matches the `summary` sheet reservations_batch writes, which is
    exactly what both adapters read."""
    rows = []
    for x in reservations:
        b = build_breakdown(x)
        by_cat = {}
        for li in b["line_items"]:
            cat = categorize_item(li)
            by_cat[cat] = round(by_cat.get(cat, 0.0) + li["amount"], 2)
        row = {**summary_row(b),
               **{c: by_cat.get(c, 0.0) for c in CAT_ORDER if c in by_cat}}
        row["guestsCount"] = x.get("guestsCount")
        # Successful Stripe transaction count — each captured charge (deposit +
        # balance, etc.) carries its own $0.30 flat fee in the Stripe-fee formula.
        # Count only status=='SUCCEEDED' payments (CANCELLED/FAILED/PENDING don't
        # settle; no security-deposit/auth-hold rows appear among SUCCEEDED).
        pays = (x.get("money") or {}).get("payments") or []
        row["n_transactions"] = sum(1 for p in pays if str(p.get("status")).upper() == "SUCCEEDED")
        # Virtual-card flag: Expedia stamps its payment method in specialRequests
        # ("Payment method: EXPEDIA VIRTUAL CARD"). Detecting it here (rather than from the
        # PCM invoice line) is what picks the correct Expedia channel-fee formula — the PCM
        # line is present on only some virtual-card bookings, so it under-detects.
        row["is_virtual_card"] = "virtual card" in str(x.get("specialRequests") or "").lower()
        rows.append(row)
    df = pd.DataFrame(rows)
    # Dedupe (overlapping pages / mixed statuses can repeat a confirmationCode).
    if "confirmationCode" in df.columns:
        df = df.drop_duplicates("confirmationCode", keep="first").reset_index(drop=True)
    return df


# ---------------------------------------------------------------------------
# Adapter A: summary frame -> UI-export-shaped CSV (for convert_guesty_export.py)
# ---------------------------------------------------------------------------
_TAXCOLS = ["tax_occupancy", "tax_state", "tax_county", "tax_city", "tax_local",
            "tax_reservation_total", "tax_other", "tax_destination", "tax_residential"]


def _g(df, col, default=0.0):
    """Numeric column, or a 0-filled series if absent. fillna(0) matters: folded
    fee-category columns are NaN for rows lacking that category, and NaN poisons
    the tax sum (578.97 + NaN = NaN)."""
    s = df[col] if col in df.columns else pd.Series(default, index=df.index)
    return pd.to_numeric(s, errors="coerce").fillna(0.0)


def to_ui_csv(S: pd.DataFrame) -> pd.DataFrame:
    """Map the summary frame to the exact column names Guesty's UI export uses,
    so convert_guesty_export.py consumes it unchanged."""
    tot_tax = sum((_g(S, c) for c in _TAXCOLS), start=pd.Series(0.0, index=S.index))
    ui = pd.DataFrame({
        "LISTING'S NICKNAME": S["listing"],
        "CONFIRMATION CODE": S["confirmationCode"],
        "SOURCE": S["source"],
        "GUEST": S["guest"],
        "CHECK-IN": S["checkIn"].astype(str).str[:10],
        "CHECK-OUT": S["checkOut"].astype(str).str[:10],
        "NUMBER OF GUESTS": _g(S, "guestsCount").fillna(0),
        "NUMBER OF NIGHTS": S["nights"],
        "ACCOMMODATION FARE": _g(S, "accommodation"),
        "PET FEE": _g(S, "pet_fee"),
        "CLEANING FARE": _g(S, "cleaning"),
        "CHANNEL COMMISSION": _g(S, "host_channel_fee").abs(),
        "TOTAL FEES": _g(S, "total_fees"),
        "AIRBNB RESOLUTION CENTER": _g(S, "airbnb_resolution_center"),
        "TOTAL REFUNDED": _g(S, "total_refunded"),
        # Σ itemized tax lines — NOT money.totalTaxes (which undercounts).
        "TOTAL TAXES": tot_tax.round(2),
        # host_payout = Σ invoiceItems, exactly as Guesty's money object reports it
        # (ARC stays inside, host got the post-ARC cash). A handful of older bookings
        # differ from a stale UI-export snapshot due to post-snapshot modifications;
        # the fresh API value is the current truth.
        "TOTAL PAYOUT": _g(S, "host_payout"),
        "TOTAL PAID": _g(S, "total_paid"),
        "STATUS": S["status"],
    })
    return ui


def main():
    ap = argparse.ArgumentParser(description="Pull one month from Guesty and write the two pipeline inputs.")
    ap.add_argument("--period", required=True, help="YYYY-MM (e.g. 2026-07).")
    ap.add_argument("--status", default="confirmed,canceled",
                    help="Comma-separated status filter (default confirmed,canceled).")
    ap.add_argument("--deactivated-csv", default=None,
                    help="Optional Guesty UI export (filtered to deactivated listings) to fold in.")
    args = ap.parse_args()

    y, mo = map(int, args.period.split("-"))
    dfrom = f"{y:04d}-{mo:02d}-01"
    dto = f"{y:04d}-{mo:02d}-{calendar.monthrange(y, mo)[1]:02d}"
    statuses = [s.strip() for s in args.status.split(",") if s.strip()] or None

    client = GuestyClient()
    print(f"Fetching reservations with check-in {dfrom} … {dto} (status in {statuses})")
    reservations = fetch_range(client, dfrom, dto, statuses, fields=FIELDS)
    print(f"Building summary for {len(reservations)} reservation(s)…")
    S = build_summary_frame(reservations)
    S["data_source"] = "API (active listing)"

    if args.deactivated_csv:
        from .deactivated import merge_deactivated
        S = merge_deactivated(S, args.deactivated_csv)

    out_dir = paths.inputs_dir(args.period)
    out_dir.mkdir(parents=True, exist_ok=True)

    # Output A — UI-shaped booking export.
    ui = to_ui_csv(S)
    a_path = paths.guesty_booking_csv(args.period)
    ui.to_csv(a_path, index=False)
    print(f"[A] wrote {len(ui)} rows -> {a_path}")

    # Output B — per-booking payment waterfall (Section 1).
    bd, dropped = compute_breakdown(S, paths.LISTING_TAX_RATES)
    bd.insert(0, "property_id", bd["listing"].map(to_property_id))
    b_path = paths.payment_breakdown_csv(args.period)
    bd.to_csv(b_path, index=False)
    print(f"[B] wrote {len(bd)} rows (dropped {dropped} with InvoiceItem=0) -> {b_path}")

    # A source string no row group claims falls through to `unknown`, which charges NO
    # channel/Guesty/Stripe fee — so it silently OVERSTATES the owner's net rather than
    # failing. Guesty adds storefront variants without warning ("homeaway2" showed up in
    # 2026-08 and cost $314.77 over 5 bookings), so say so loudly here instead of leaving
    # it to be spotted on a statement.
    unknown = bd[bd["row_group"].eq("unknown")]
    if len(unknown):
        print(f"\n  !! {len(unknown)} booking(s) on {unknown['channel'].nunique()} UNMAPPED "
              f"source(s) — charged NO fees (net overstated). Map them in "
              f"payment_model._assign_row_group:")
        for ch, g in unknown.groupby("channel"):
            print(f"       {str(ch):22s} {len(g):3d} booking(s)  "
                  f"InvoiceItem ${g['InvoiceItem'].sum():,.2f}")

    # Every per-reservation manual adjustment this pull re-applied. This file is the ONLY
    # writer of payment_breakdown_<p>.csv and it overwrites, so a correction that lives
    # only in the CSV is destroyed here; printing the list is how you confirm each one is
    # in `payment_model` and actually fired, rather than discovering months later that a
    # number quietly reverted.
    hits = applied_adjustments(bd["confirmationCode"].astype(str))
    if hits:
        print(f"\n  manual adjustments re-applied for {args.period} ({len(hits)}):")
        for name, code, value in hits:
            print(f"    {code:24s} {name}" + (f" = {value}" if value is not None else ""))
    else:
        print(f"\n  no manual adjustments apply to {args.period}.")

    print("\nNext: python -m src.breakdown.convert_export "
          f"--input {a_path} --output {paths.guesty_converted_csv(args.period)}")


if __name__ == "__main__":
    main()
