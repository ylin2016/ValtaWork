import os
import re
import glob
import numpy as np
import pandas as pd
from datetime import datetime
pd.set_option("display.max_colwidth", None)
pd.set_option("display.expand_frame_repr", False)
# ---------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------

def excel_serial_to_date(s):
    """Convert Excel serial dates (origin 1899-12-30) to datetime64[ns]."""
    return pd.to_datetime(s, errors="coerce", unit="d", origin="1899-12-30")

def safe_upper(df):
    df = df.copy()
    df.columns = (df.columns
      .str.upper()
      .str.replace("-", ".", regex=False)
      .str.replace(" ", ".", regex=False)
      .str.replace("'", ".", regex=False)
    )
    return df

def ensure_cols(df, cols):
    """Make sure df has all columns in cols; create missing as NaN."""
    for c in cols:
        if c not in df.columns:
            df[c] = np.nan
    return df

def natural_join(left, right):
    """
    Mimic plyr::join with natural keys = common column names.
    If no common columns, just attach right's columns without merging (cartesian would be wrong),
    so we return left unchanged in that case.
    """
    common = list(set(left.columns).intersection(set(right.columns)))
    if not common:
        return left.copy()
    return left.merge(right, on=common, how="left")

def build_guesty_2325_and_write(
    base_path="/Users/ylin/Google Drive/My Drive/01- Compensation Calculation/Working/Data/",
    out_confirm="/Users/ylin/ValtaWork/Valta_BookingManagement/Data/ConfirmedGuesty2325.csv",
    out_canceled="/Users/ylin/ValtaWork/Valta_BookingManagement/Data/CanceledGuesty2325.csv",
):
    file23 = [p for p in glob.glob(os.path.join(base_path, "2023", "Guesty_booking_c*")) if not p.endswith(".xlsx")]
    file24 = [p for p in glob.glob(os.path.join(base_path, "2024", "Guesty_booking_c*")) if not p.endswith(".xlsx")]
    file25 = [
        p for p in glob.glob(os.path.join(base_path, "2025", "Guesty_booking_c*"))
        if not (p.endswith(".xlsx") or re.search(r"01-09|pre", os.path.basename(p)))
    ]
    frames = []
    for p in (file23 + file24 + file25):
        tmp = pd.read_csv(p, na_values=["", " "])
        tmp = safe_upper(tmp)
        frames.append(tmp)
    if not frames:
        return None, None
    guesty = pd.concat(frames, ignore_index=True, sort=False)
    confirmed = guesty.loc[guesty.get("STATUS").eq("confirmed")]
    canceled  = guesty.loc[guesty.get("STATUS").eq("canceled")]
    #confirmed.to_csv(out_confirm, index=False, na_rep="")
    #canceled.to_csv(out_canceled, index=False, na_rep="")
    return confirmed, canceled

# ---------------------------------------------------------------------
# Property / Employee inputs
# ---------------------------------------------------------------------

def property_input():
    cohost = pd.read_excel("/Users/ylin/Google Drive/My Drive/Data and Reporting/Data/Property_Cohost.xlsx")
    cohost = cohost[~cohost["Listing"].isin(["Ashford 137", "Auburn 29123", "Hoquiam 21", "Valta Realty", "Maria"])]
    return cohost

# ---------------------------------------------------------------------
# Reservations formatting
# ---------------------------------------------------------------------

def format_reservation(df, startdate, enddate):
    df = safe_upper(df).copy()

    # Dates
    df["CHECKIN"]  = pd.to_datetime(df["CHECK.IN"].astype(str).str[:10],  errors="coerce")
    df["CHECKOUT"] = pd.to_datetime(df["CHECK.OUT"].astype(str).str[:10], errors="coerce")
    if "CONFIRMATION.DATE" in df.columns:
        df["CONFIRMED.DATE"] = pd.to_datetime(df["CONFIRMATION.DATE"].astype(str).str[:10], errors="coerce")
        df["LEAD.TIME"] = np.where(df["CONFIRMED.DATE"].isna(),np.nan,
                                    np.where(df["CHECKIN"] == df["CONFIRMED.DATE"],0,
                                        (df["CHECKIN"] - df["CONFIRMED.DATE"]).dt.days))
    else:
        df["LEAD.TIME"] = np.nan

    df["LEAD.TIME"] = np.where(df["LEAD.TIME"] < 0, np.nan, df["LEAD.TIME"])

    # Filter by check-in window
    df = df[(df["CHECKIN"] >= pd.to_datetime(startdate)) & (df["CHECKIN"] <= pd.to_datetime(enddate))].copy()

    # Earnings fallback
    if "EARNINGS" not in df.columns:
        total_payout = pd.to_numeric(df.get("TOTAL.PAYOUT"), errors="coerce")
        total_paid   = pd.to_numeric(df.get("TOTAL.PAID"),   errors="coerce")
        df["EARNINGS"] = np.where((~total_payout.isna()) & (total_payout > 0), total_payout, total_paid)

    # Cleaning fee override via CLEANING.FARE if present
    if "CLEANING.FARE" in df.columns:
        clean_fare = pd.to_numeric(df["CLEANING.FARE"], errors="coerce")
        if "CLEANING.FEE" not in df.columns:
            df["CLEANING.FEE"] = np.nan
        df["CLEANING.FEE"] = np.where(~clean_fare.isna() & (clean_fare != 0), clean_fare, df["CLEANING.FEE"])

    needed = [
        "LISTING.S.NICKNAME","CONFIRMATION.CODE","NUMBER.OF.GUESTS","NUMBER.OF.ADULTS",
        "NUMBER.OF.CHILDREN","NUMBER.OF.INFANTS","CHECKIN","CHECKOUT","NUMBER.OF.NIGHTS",
        "EARNINGS","SOURCE","CLEANING.FEE","PET.FEE","ACCOMMODATION.FARE","PLATFORM","LEAD.TIME",
        "CONFIRMED.DATE"
    ]
    df = ensure_cols(df, needed)
    df = df[needed].copy()

    df = df.rename(columns={
        "LISTING.S.NICKNAME": "Listing",
        "CONFIRMATION.CODE":  "Confirmation.Code",
        "NUMBER.OF.GUESTS":   "guests",
        "NUMBER.OF.ADULTS":   "adults",
        "NUMBER.OF.CHILDREN": "children",
        "NUMBER.OF.INFANTS":  "infants",
        "CHECKIN":            "checkin_date",
        "CHECKOUT":           "checkout_date",
        "NUMBER.OF.NIGHTS":   "nights",
        "EARNINGS":           "earnings",
        "SOURCE":             "booking_source",
        "CLEANING.FEE":       "cleaning_fee",
        "PET.FEE":            "pet_fee",
        "ACCOMMODATION.FARE": "accommodation_fare",
        "PLATFORM":           "booking_platform",
        "LEAD.TIME":          "lead_time",
        "CONFIRMED.DATE":     "confirmation_date",
    })

    # Derived fields
    df["nights"] = pd.to_numeric(df["nights"], errors="coerce")
    df["earnings"] = pd.to_numeric(df["earnings"], errors="coerce")
    df["cleaning_fee"] = pd.to_numeric(df["cleaning_fee"], errors="coerce")

    df["DailyListingPrice"] = df["earnings"] / df["nights"]  # includes cleaning
    df["AvgDailyRate"]      = df["accommodation_fare"]/ df["nights"]
    df["total_revenue"]     = df["earnings"]
    df["yearmonth"]             = pd.to_datetime(df["checkin_date"]).dt.strftime("%Y-%m")
    df["checkin_date_plot"] = pd.to_datetime(df["checkin_date"])
    return df

def import_data():
    filepath = "/Users/ylin/Google Drive/My Drive/Data and Reporting/"
    platforms = pd.read_excel(filepath+"Data/Revenue/Source_Platform.xlsx")

    # Guesty pre-2025 (already filtered & saved from your earlier step)
    guesty_bf25 = pd.read_csv(filepath+"/Data/Revenue/Guesty_bookings_bf2025.csv", na_values=["", " "])

    # 2025 Guesty (exclude specific listings)
    guesty_2025 = pd.read_csv(filepath+"Data/Revenue/Guesty_bookings_2025.csv", na_values=["", " "])
    guesty_2025 = guesty_2025[~guesty_2025["LISTING'S NICKNAME"].isin(["Ashford 137", "Auburn 29123", "Hoquiam 21"])]
    guesty_2025.columns=np.delete(guesty_bf25.columns,[-7,-1])

    # 2026 Guesty (exclude specific listings)
    guesty_2026 = pd.read_csv(filepath+"Data/Revenue/Guesty_bookings_2026-20260223.csv", na_values=["", " "])
    guesty_2026 = guesty_2026[~guesty_2026["LISTING'S NICKNAME"].isin(["Ashford 137", "Auburn 29123", "Hoquiam 21"])]
    guesty_2026.columns=np.delete(guesty_bf25.columns,[-7,-1])

    confirmed_guesty = pd.concat([guesty_2025, guesty_bf25,guesty_2026], ignore_index=True, sort=False)
    confirmed_guesty = confirmed_guesty.merge(platforms, on="SOURCE", how="left")
    
    #manual correct errors:
    confirmed_guesty.loc[confirmed_guesty["CONFIRMATION.CODE"]=="HA-jNbd0Rc","ACCOMMODATION.FARE"]=2235

    confirmed_fmt = format_reservation(confirmed_guesty, "2017-01-01", "2026-12-31")
    
    # 2023 historicals
    guesty2023 = pd.read_csv(filepath+"/Input_PowerBI/Guesty_PastBooking_airbnb_adj_12312023.csv", na_values=["", " "])
    chs        = pd.read_csv(filepath+"/Input_PowerBI/Rev_CH_2023.csv", na_values=["", " "])
    vrbo2023   = pd.read_csv(filepath+"/Input_PowerBI/VRBO_20200101-20231230.csv", na_values=["", " "])

    # Keep only 2023 rows for CH
    chs["CHECK.OUT"] = chs["CHECK.OUT"].astype(str)
    ch2023 = chs[chs["CHECK.OUT"].str[:4] == "2023"].copy()

    dat2023 = pd.concat([guesty2023, ch2023, vrbo2023], ignore_index=True, sort=False)
    dat2023 = dat2023.merge(platforms, on="SOURCE", how="left")
    dat2023["PET.FEE"] = np.nan
    dat2023["ACCOMMODATION.FARE"] = dat2023["Earnings"]-dat2023["Cleaning.fee"]
    dat2023_fmt = format_reservation(dat2023, "2017-01-01", "2023-12-31")

    # Keep rows from 2023 set that are not in confirmed_fmt by Confirmation.Code
    diff_codes = set(dat2023_fmt["Confirmation.Code"]).difference(set(confirmed_fmt["Confirmation.Code"]))
    carry = dat2023_fmt[dat2023_fmt["Confirmation.Code"].isin(diff_codes)].copy()

    data = pd.concat([confirmed_fmt, carry], ignore_index=True, sort=False)

    # Bring in adjusted values (earnings / revenue / rates) from dat2023_fmt
    adj_cols = ["Listing", "Confirmation.Code", "earnings", "total_revenue", "DailyListingPrice", "AvgDailyRate"]
    data = data.merge(dat2023_fmt[adj_cols],
                        on=["Listing", "Confirmation.Code"],
                        how="left",
                        suffixes=("", ".adj"))

    # If earnings differ, replace all value-coupled fields with *.adj
    mask = data["earnings"].ne(data["earnings.adj"]) & ~data["earnings.adj"].isna()
    for c in ["earnings", "total_revenue", "DailyListingPrice", "AvgDailyRate"]:
        data.loc[mask, c] = data.loc[mask, f"{c}.adj"]
        data.drop(columns=[f"{c}.adj"], inplace=True)
    # Drop any remaining *.adj (in case some didn’t differ)
    adj_leftovers = [c for c in data.columns if c.endswith(".adj")]
    if adj_leftovers:
        data = data.drop(columns=adj_leftovers) 

    # Cleaning fee table (pre/post 2025-03-01)
    cleaning = pd.read_excel(filepath+"Data/Property_Cohost.xlsx",
        sheet_name="Cleaning").copy()
    cleaning["newCleaning.fee"] = cleaning["Cleaning.fee"]

    data["checkout_date"] = pd.to_datetime(data["checkout_date"], errors="coerce")

        # Before 2025-03-01
    left = data[data["checkout_date"] < pd.Timestamp("2025-03-01")].merge(
        cleaning[["Listing", "Cleaning.fee.bf0325"]],
        on="Listing",
        how="left"
    )
    left["cleaning_fee"] = np.where(
        left["cleaning_fee"].isna() | (left["cleaning_fee"] == 0),
        left["Cleaning.fee.bf0325"],
        left["cleaning_fee"]
    )
    left = left.drop(columns=["Cleaning.fee.bf0325"], errors="ignore")

    # On/after 2025-03-01  
    right = data[(data["checkout_date"] >= pd.Timestamp("2025-03-01")) &(data["checkout_date"] <= pd.Timestamp("2025-12-31"))].merge(
        cleaning[["Listing", "Cleaning.fee.202512"]],
        on="Listing",
        how="left"
    )
    right["cleaning_fee"] = np.where(
        right["cleaning_fee"].isna() | (right["cleaning_fee"] == 0),
        right["Cleaning.fee.202512"],
        right["cleaning_fee"]
    )
    right = right.drop(columns=["Cleaning.fee.202512"], errors="ignore")
    
    # cleaning fee changed from 2026-0-01 to new rates
    new = data[(data["checkout_date"] >= pd.Timestamp("2026-01-01"))].merge(
        cleaning[["Listing", "Cleaning.fee"]],
        on="Listing",
        how="left"
    )
    new ["cleaning_fee"] = np.where(
        new ["cleaning_fee"].isna() | (new ["cleaning_fee"] == 0),
        new ["Cleaning.fee"],
        new ["cleaning_fee"]
    )
    new  = new .drop(columns=["Cleaning.fee"], errors="ignore")
    
    data = pd.concat([left, right,new], ignore_index=True, sort=False)

    # Final filters & recalcs
    data = data[~data["Listing"].isin(["Ashford 137", "Auburn 29123", "Hoquiam 21","Bellevue 4551","Bothell 21833","NorthBend 44406"])].copy()
    data["DailyListingPrice"] = data["earnings"] / data["nights"]
    data["total_revenue"] = data["earnings"]

    # LTR bookings
    LRT = pd.read_excel(filepath+"Data/Revenue/LRT_bookings.xlsx")
    LRT = safe_upper(LRT).copy()
    # Rows in LRT not already in data by CONFIRMATION.CODE
    lrt_diff_codes = set(LRT["CONFIRMATION.CODE"]).difference(set(data["Confirmation.Code"]))
    LRT_diff = LRT[LRT["CONFIRMATION.CODE"].isin(lrt_diff_codes)].copy()

    # Fill required columns as in R
    for c, val in {
        "CONFIRMATION.DATE": np.nan,
        "TOTAL.PAID": 0,
        "NUMBER.OF.GUESTS": np.nan,
        "NUMBER.OF.ADULTS": np.nan,
        "NUMBER.OF.CHILDREN": np.nan,
        "NUMBER.OF.INFANTS": np.nan,
        "PET.FEE": np.nan
    }.items():
        if c not in LRT_diff.columns:
            LRT_diff[c] = val
        else:
            if c == "Cleaning.Fee":
                LRT_diff[c] = LRT_diff[c].fillna(0)

    LRT_diff = LRT_diff.merge(platforms, on="SOURCE", how="left")

    LRT_diff_fmt = format_reservation(LRT_diff, "2023-01-01", "2030-12-31").copy()
    LRT_diff_fmt["Term"] = "LTR"

    # Tag STR/LTR on main data
    data["Term"] = "STR"
    if "CONFIRMATION.CODE" in LRT.columns:
        in_lrt = set(LRT["CONFIRMATION.CODE"])
        data.loc[data["Confirmation.Code"].isin(in_lrt), "Term"] = "LTR"

    # Combine
    data = pd.concat([data, LRT_diff_fmt], ignore_index=True, sort=False)
    data.loc[data["Listing"]=="Seattle 906","Listing"] = "Seattle 906 Lower" 
    data["lead_time"] = np.where(data["lead_time"]<0, np.nan,data["lead_time"])
    data = data.copy()
    return data


