import argparse
import pandas as pd
import re
from pathlib import Path


# Guesty listing nickname → canonical property_id used in QBO/DB
_NICKNAME_ALIASES = {
    "cottage 1":       "osbr_1",
    "cottage 2":       "osbr_2",
    "cottage 3":       "osbr_3",
    "cottage 4":       "osbr_4",
    "cottage 5":       "osbr_5",
    "cottage 6":       "osbr_6",
    "cottage 7":       "osbr_7",
    "cottage 9":       "osbr_9",
    "cottage 10":      "osbr_10",
    "cottage 11 (tiny)": "osbr_11",
    "cottage 12":      "osbr_12",
    # Bare building nicknames from Guesty → default unit
    "seattle 906":     "seattle_906_lower",
    "seattle 7434":    "seattle_7434_whole",
}

def to_property_id(name: str) -> str:
    cleaned = str(name).strip()
    alias = _NICKNAME_ALIASES.get(cleaned.lower())
    if alias:
        return alias
    cleaned = cleaned.lower()
    cleaned = re.sub(r"[^a-z0-9]+", "_", cleaned)
    cleaned = re.sub(r"_+", "_", cleaned).strip("_")
    return cleaned


def to_float(v):
    if pd.isna(v):
        return 0.0
    s = str(v).replace("$", "").replace(",", "").strip()
    if s == "":
        return 0.0
    return float(s)


def convert(input_file, output_file):

    df = pd.read_csv(input_file, encoding="utf-8-sig")

    # Filter: confirmed bookings OR canceled with actual revenue (TOTAL PAYOUT > 0)
    if "STATUS" in df.columns:
        confirmed = df["STATUS"].str.lower() == "confirmed"
        canceled_paid = (df["STATUS"].str.lower() == "canceled") & (pd.to_numeric(df["TOTAL PAYOUT"], errors="coerce") > 0) & (pd.to_numeric(df["TOTAL PAID"], errors="coerce") > 0)
        df = df[confirmed | canceled_paid]
        print(f"Filtered to {len(df)} rows (confirmed + canceled with gross revenue > 0)")

    out = pd.DataFrame()

    out["property_id"] = df["LISTING'S NICKNAME"].apply(to_property_id)
    out["booking_id"] = df["CONFIRMATION CODE"]
    out["channel"] = df["SOURCE"]
    out["guest_name"] = df["GUEST"]
    out["checkin"] = pd.to_datetime(df["CHECK-IN"]).dt.strftime("%Y-%m-%d")
    out["checkout"] = pd.to_datetime(df["CHECK-OUT"]).dt.strftime("%Y-%m-%d")

    out["accommodation_fee"] = df["ACCOMMODATION FARE"].apply(to_float)
    out["pet_fee"] = df["PET FEE"].apply(to_float)
    out["cleaning_fee"] = df["CLEANING FARE"].apply(to_float)
    out["channel_commission"] = df["CHANNEL COMMISSION"].apply(to_float)
    out["total_fee"] = df["TOTAL FEES"].apply(to_float)
    out["airbnb_resolution"] = df["AIRBNB RESOLUTION CENTER"].apply(to_float)
    out["refund"] = df["TOTAL REFUNDED"].apply(to_float)
    out["taxes"] = df["TOTAL TAXES"].apply(to_float)
    out["total_payout"] = df["TOTAL PAYOUT"].apply(to_float)
    out["status"] = df["STATUS"].str.lower()

    # Calculate cleaning_fee and service_fee based on channel
    def _get_fees(row):
        channel = str(row.get("SOURCE", "")).lower()
        total_fee = to_float(row["TOTAL FEES"])
        cleaning_fare = to_float(row["CLEANING FARE"])
        pet_fee = to_float(row["PET FEE"])

        if channel in ["expedia", "hotel.com"]:
            # For Expedia, Hotel.com: cleaning_fee = total_fee - pet_fee
            cleaning_fee = max(0.0, total_fee - pet_fee)
            service_fee = 0.0
        elif channel in ["be-api", "manual", "website"]:
            # For BE-API, Manual, Website: service_fee = total_fee - cleaning_fare - pet_fee
            cleaning_fee = cleaning_fare
            service_fee = max(0.0, total_fee - cleaning_fare - pet_fee)
        else:
            # For all other channels: service_fee = 0, cleaning_fee = cleaning_fare
            cleaning_fee = cleaning_fare
            service_fee = 0.0

        return pd.Series({'cleaning_fee': cleaning_fee, 'service_fee': service_fee})

    fee_cols = df.apply(_get_fees, axis=1)
    out["cleaning_fee"] = fee_cols['cleaning_fee']
    out["service_fee"] = fee_cols['service_fee']

    # Net revenue calculation based on booking status and channel
    def _net_revenue(idx, row):
        total_payout = to_float(row["TOTAL PAYOUT"])
        total_paid = to_float(row["TOTAL PAID"])
        is_cancelled = str(row.get("STATUS", "")).lower() == "canceled"

        if is_cancelled:
            # For cancelled bookings: net_revenue = amount actually paid by guest (TOTAL PAID)
            return max(0.0, total_paid)
        else:
            # For confirmed bookings: net_revenue = total_payout - cleaning - tax - refund
            # All channel and stripe fees (and their deductions) come from QBO in run_month_close
            cleaning = to_float(row["CLEANING FARE"])
            tax = to_float(row["TOTAL TAXES"])
            refund = to_float(row["TOTAL REFUNDED"])

            channel = str(row.get("SOURCE", "")).lower()

            # For Booking.com: tax comes from QBO invoice, NOT from Guesty
            # For all other channels: tax comes from Guesty
            if channel == "booking.com":
                net_rev = total_payout - cleaning - refund
            else:
                net_rev = total_payout - cleaning - refund - tax

            # Service fee deduction for BE-API, Manual, Website (from calculated service_fee)
            if channel in ["be-api", "manual", "website"]:
                service_fee = fee_cols.loc[idx, 'service_fee']
                net_rev = net_rev - service_fee

            # All other fees (channel_fee, stripe_fee, airbnb_resolution, etc.) come from QBO
            # For Booking.com: tax also comes from QBO invoice
            return max(0.0, net_rev)

    out["net_revenue"] = df.apply(lambda row: _net_revenue(row.name, row), axis=1)

    out["notes"] = (
        "Guests=" + df["NUMBER OF GUESTS"].astype(str) +
        "; Nights=" + df["NUMBER OF NIGHTS"].astype(str)
    )

    out.to_csv(output_file, index=False)

    print(f"Converted {len(out)} rows → {output_file}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert raw Guesty CSV export to canonical format")
    parser.add_argument("--input", help="Input Guesty CSV file (auto-detected from /data/ if not specified)")
    parser.add_argument("--output", help="Output converted CSV file (defaults to guesty_converted.csv in /data/)")
    parser.add_argument("--period", help="Period for filename (e.g., 2026-05)")
    args = parser.parse_args()

    base_dir = Path(__file__).parent.parent
    data_dir = base_dir / "data"

    # Auto-detect input file if not specified
    if not args.input:
        # Look for Guesty_booking_YYYY-MM.csv files
        guesty_files = sorted(data_dir.glob("Guesty_booking_*.csv"), reverse=True)
        if guesty_files:
            args.input = str(guesty_files[0])
            print(f"Auto-detected Guesty file: {args.input}")
        else:
            parser.error("No Guesty CSV files found in /data/. Please specify --input")

    # Auto-generate output filename if not specified
    if not args.output:
        if args.period:
            args.output = str(data_dir / f"guesty_converted_{args.period}.csv")
        else:
            args.output = str(data_dir / "guesty_converted.csv")

    convert(args.input, args.output)