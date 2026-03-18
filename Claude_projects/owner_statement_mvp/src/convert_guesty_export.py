import argparse
import pandas as pd
import re


def to_property_id(name: str) -> str:
    name = str(name).strip().lower()
    name = re.sub(r"[^a-z0-9]+", "_", name)
    name = re.sub(r"_+", "_", name).strip("_")
    return f"{name}"


def to_float(v):
    if pd.isna(v):
        return 0.0
    s = str(v).replace("$", "").replace(",", "").strip()
    if s == "":
        return 0.0
    return float(s)


def convert(input_file, output_file):

    df = pd.read_csv(input_file)

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

    out["net_booking_revenue"] = df["TOTAL PAYOUT"].apply(to_float)

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