import argparse
import pandas as pd
import re


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

    # Filter: confirmed bookings OR canceled with actual revenue (TOTAL PAID > 0)
    if "STATUS" in df.columns:
        confirmed = df["STATUS"].str.lower() == "confirmed"
        canceled_paid = (df["STATUS"].str.lower() == "canceled") & (pd.to_numeric(df["TOTAL PAID"], errors="coerce") > 0)
        df = df[confirmed | canceled_paid]
        print(f"Filtered to {len(df)} rows (confirmed + canceled with payment)")

    out = pd.DataFrame()

    out["property_id"] = df["LISTING'S NICKNAME"].apply(to_property_id)
    out["booking_id"] = df["CONFIRMATION CODE"]
    out["channel"] = df["SOURCE"]
    out["guest_name"] = df["GUEST"]
    out["checkin"] = pd.to_datetime(df["CHECK-IN"]).dt.strftime("%Y-%m-%d")
    out["checkout"] = pd.to_datetime(df["CHECK-OUT"]).dt.strftime("%Y-%m-%d")

    out["rent"] = df["ACCOMMODATION FARE"].apply(to_float) + df["EXTRA PERSON FEE"].apply(to_float) + df["PET FEE"].apply(to_float)
    out["cleaning_fee"] = df["CLEANING FARE"].apply(to_float)
    out["other_fees"] = 0.0
    out["discount"] = 0.0
    out["refund"] = df["TOTAL REFUNDED"].apply(to_float)
    out["taxes"] = 0.0

    # Net rental revenue (accommodation only, excluding cleaning):
    # - Airbnb/direct: TOTAL PAYOUT - CLEANING FARE (Airbnb nets their commission before depositing)
    # - VRBO, Booking.com, all other OTAs: ACCOMMODATION FARE + EXTRA PERSON FEE + PET FEE
    #   (gross payout includes channel/stripe fees that belong to Valta, not owner)
    _AIRBNB_LIKE = {"airbnb", "airbnb2", "manual", "owner", "website"}

    def _net_revenue(row):
        ch = str(row["SOURCE"]).strip().lower()
        if ch in _AIRBNB_LIKE:
            return max(0.0, to_float(row["TOTAL PAYOUT"]) - to_float(row["CLEANING FARE"]))
        else:
            return to_float(row["ACCOMMODATION FARE"]) + to_float(row["EXTRA PERSON FEE"]) + to_float(row["PET FEE"])

    out["net_revenue"] = df.apply(_net_revenue, axis=1)

    out["notes"] = (
        "Guests=" + df["NUMBER OF GUESTS"].astype(str) +
        "; Nights=" + df["NUMBER OF NIGHTS"].astype(str)
    )

    out.to_csv(output_file, index=False)

    print(f"Converted {len(out)} rows → {output_file}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    convert(args.input, args.output)