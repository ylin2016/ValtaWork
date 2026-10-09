"""Weekly revenue report — script port of Data and Reporting/RevenueReport.ipynb.

Inputs: the Guesty API pull (fetch_guesty.py), Wheelhouse market data
(wheelhouse.sqlite, matched per listing on MARKET + BEDROOM bucket), plus the
same static/manual files the notebook used (pre-2026 bookings, LTR list,
owner payouts, ratings, Property_Cohost.xlsx), copied into data/inputs/.

    python build_report.py --asof 2026-09-29               # writes to output/
    python build_report.py --asof 2026-09-29 --publish     # also to Google Drive

Differences from the notebook (intentional):
  * as-of date is a parameter (no date.today() file names)
  * owner-payout month can't go <= 0 in January/February
  * market occupancy (Occ_All / Occ_HP) is matched on market AND bedrooms,
    falling back to the market's all-bedroom figure when a segment is empty
  * onboarding denominators: explicit ONBOARD dates, plus automatic handling
    for any other listing whose first stay is in the current year
  * newest "*guesty_reviews.xlsx" is picked up automatically
"""
import argparse
import re
import sqlite3
from calendar import monthrange
from datetime import date
from pathlib import Path

import numpy as np
import pandas as pd
from openpyxl.utils import get_column_letter

from paths import (DATA_DIR, DRIVE, GUESTY_CANCELED, OUTPUT_DIR, OVERALL_RATINGS, PUB_OCCUPANCY,
                   PUB_REPORT, PUB_TRACKING_DIR, RENT_ROLL, REVIEWS_DIR, SOURCE_PLATFORM,
                   WHEELHOUSE_DB)
from reporting.DataProcessing import format_reservation, import_data, property_input
from reporting.RevenueReportHelpers import (build_owner_payout_26, build_owner_payout_2425,
                                            cal_occupancy, days_in_month_from_yearmonth,
                                            safe_round, yearrecords, yoy_delta)

pd.options.mode.chained_assignment = None

YEARS = ["2024", "2025", "2026"]
CUR = YEARS[-1]
PREV = YEARS[-2]

EXCLUDED_LISTINGS = {"Bellevue 4551", "Bothell 21833", "NorthBend 44406",
                     "Ashford 137", "Auburn 29123", "Hoquiam 21"}
OUTPUT_EXCL = ["Seatac 12834", "Seatac 12834 Lower", "Seatac 12834 Upper",
               "Seattle 1512", "Mercer 3627 Main"]
TEST_CODES = ["HMBE2E392M", "HMQAWPFBSZ", "HM5ZZBP4NK"]

# Listings onboarded this year: OccRt denominator starts here instead of Jan 1.
# Dates reproduce the notebook's hard-coded day counts (275, 214, 209, 202,
# 160, 169, 132 days to Dec 31). NOTE three of the notebook comments disagree
# with their numbers: Lynnwood said Jul 6 (160 days = Jul 25), Yacinde E3/C1
# said Jul 15 (169 = Jul 16), Yacinde B1-B4 said Aug 20 (132 = Aug 22).
ONBOARD = {
    **dict.fromkeys(["Seattle 7434", "Seattle 7434 Upper", "Seattle 7434 Lower"], "2026-04-01"),
    **dict.fromkeys(["Bellevue 2323 Whole", "Bellevue 2323 Main", "Bellevue 2323 ADU"], "2026-06-01"),
    "Elektra 1413": "2026-06-06",
    "Shoreline 15510": "2026-06-13",
    "Lynnwood 17506": "2026-07-25",
    **dict.fromkeys(["Yacinde E3", "Yacinde C1", "Yacinde B6", "Yacinde E1"], "2026-07-16"),
    **dict.fromkeys(["Yacinde B1", "Yacinde B2", "Yacinde B3", "Yacinde B4",
                     "Renton 18823"], "2026-08-22"),
}

# Rent roll listing names -> Property_Cohost / Guesty names
RENT_ROLL_ALIASES = {
    "Seattle 906 upper": "Seattle 906 Upper",
    "Microsoft D303": "Microsoft 14615-D303",
    **{f"Bellevue 14507 4-Plex {i}": f"Bellevue 14507U{i}" for i in range(1, 5)},
}

# Property_Cohost "Market" -> Wheelhouse market name prefix
WH_MARKET = {"Hawaii": "Big Island", "ONP": "Olympic National Park"}

FORMAT_RULES = {
    "currency": (r"(Revenue|Payout|OwnerDist|ADR|Profit|Cost|Fee)", "$#,##0"),
    "percent": (r"(OccRt|Ratio|RevLoss|Pct|Occ_All|Occ_HP)", "0%"),
}


# ---------------------------------------------------------------------------
# Market data (Wheelhouse)
# ---------------------------------------------------------------------------
# Listing groups (a listing can be in several, "|"-joined): Elektra / Yacinde by name,
# OSBR / Beachwood / Microsoft / Remote by the first word of Property_Cohost "Set"
# (Remote also takes the Hawaii market), plus one group per city in GROUP_CITIES.
SET_GROUPS = {"osbr": "OSBR", "remote": "Remote", "beachwood": "Beachwood", "microsoft": "Microsoft"}
GROUP_CITIES = ["Seattle", "Bellevue", "Kirkland", "Redmond"]


def listing_groups(r):
    name = str(r["Listing"])
    g = [x for x in ("Elektra", "Yacinde") if name.startswith(x)]
    w = str(r["Set"]).split()[0].lower() if pd.notna(r["Set"]) and str(r["Set"]).strip() else ""
    if w in SET_GROUPS:
        g.append(SET_GROUPS[w])
    if r["Market"] == "Hawaii" and "Remote" not in g:
        g.append("Remote")
    if r["City"] in GROUP_CITIES:
        g.append(r["City"])
    return "|".join(g)


def bedroom_bucket(b):
    if pd.isna(b):
        return ""
    b = int(b)
    return "4+" if b >= 4 else str(b)


def load_market(snapshot: str | None) -> tuple[pd.DataFrame, str]:
    """Monthly occupancy_adjusted + adr_w_fees per (Market, bedrooms):
    Occ_All/Occ_HP (fractions, 0.43 = 43%) and ADR_All/ADR_HP ($, incl. fees).
    bedrooms '' = all sizes."""
    con = sqlite3.connect(WHEELHOUSE_DB)
    if snapshot is None:
        snapshot = con.execute("SELECT MAX(snapshot_date) FROM market_monthly").fetchone()[0]
    df = pd.read_sql_query("""
        SELECT m.name AS wh_market, mm.performance, mm.bedrooms, mm.month AS yearmonth,
               mm.metric, mm.value
        FROM market_monthly mm
        JOIN markets m ON m.market_id = mm.market_id AND m.snapshot_date = mm.snapshot_date
        WHERE mm.snapshot_date = ? AND mm.metric IN ('occupancy_adjusted', 'adr_w_fees')
          AND mm.performance IN ('', 'high')""", con, params=(snapshot,))
    con.close()
    if df.empty:
        raise RuntimeError(f"No Wheelhouse market data for snapshot {snapshot}")
    col = (np.where(df["metric"] == "adr_w_fees", "ADR_", "Occ_")
           + np.where(df["performance"] == "high", "HP", "All"))
    wide = (df.assign(col=col)
              .pivot_table(index=["wh_market", "bedrooms", "yearmonth"], columns="col",
                           values="value", aggfunc="first")
              .reset_index())
    wide.columns.name = None
    return wide, snapshot


def attach_market(listings: pd.DataFrame, market: pd.DataFrame) -> pd.DataFrame:
    """listings: Listing, Market, BEDROOMS -> one row per Listing x yearmonth with
    Occ_All/Occ_HP for its market+bedroom (fallback: market all-bedrooms)."""
    lst = listings[["Listing", "Market", "BEDROOMS"]].dropna(subset=["Market"]).copy()
    lst["Bedrooms"] = lst["BEDROOMS"].map(bedroom_bucket)
    names = market["wh_market"].unique()

    def resolve(mkt):
        key = WH_MARKET.get(mkt, mkt).lower()
        hits = [n for n in names if n.lower().startswith(key)]
        return hits[0] if hits else None
    lst["wh_market"] = lst["Market"].map(resolve)
    missing = sorted(lst.loc[lst["wh_market"].isna(), "Market"].unique())
    if missing:
        print(f"  WARNING: no Wheelhouse market for {missing}")

    seg = lst.merge(market, left_on=["wh_market", "Bedrooms"],
                    right_on=["wh_market", "bedrooms"], how="left")
    allb = lst.merge(market[market["bedrooms"] == ""], on="wh_market", how="left")
    seg = seg.set_index(["Listing", "yearmonth"])
    allb = allb.set_index(["Listing", "yearmonth"])
    out = allb[["Market", "Bedrooms"]].copy()
    for c in ["Occ_All", "Occ_HP", "ADR_All", "ADR_HP"]:
        out[c] = seg[c].reindex(out.index)
        out[c + "_src"] = np.where(out[c].isna(), "market", "bedroom")
        out[c] = out[c].fillna(allb[c])
    return out.reset_index()


def load_sets(snapshot: str) -> pd.DataFrame:
    """Listing -> Wheelhouse dynamic (comp) set monthly occupancy + ADR.
    Wheelhouse listing names are the Guesty nicknames. A listing can be in
    more than one set (one row per set)."""
    con = sqlite3.connect(WHEELHOUSE_DB)
    df = pd.read_sql_query("""
        SELECT l.name AS Listing, d.name AS set_name, substr(a.period, 1, 7) AS yearmonth,
               a.metric, a.value
        FROM dynamic_set_associated_listings s
        JOIN listings l ON l.listing_id = s.user_listing_id AND l.snapshot_date = s.snapshot_date
        JOIN dynamic_sets d ON d.set_id = s.set_id AND d.snapshot_date = s.snapshot_date
        JOIN dynamic_set_aggregated_metrics a ON a.set_id = s.set_id AND a.snapshot_date = s.snapshot_date
        WHERE s.snapshot_date = ? AND a.metric IN ('occupancy_adjusted', 'adr', 'lead_time')""",
        con, params=(snapshot,))
    con.close()
    df["set_name"] = df["set_name"].str.strip()
    wide = df.pivot_table(index=["Listing", "set_name", "yearmonth"], columns="metric",
                          values="value", aggfunc="first").reset_index()
    wide.columns.name = None
    if "lead_time" not in wide:
        wide["lead_time"] = np.nan
    return wide.rename(columns={"occupancy_adjusted": "Occ", "adr": "ADR", "lead_time": "Lead"})


def dashboard_tables(data, monthly, prop, market, snap, money_csv=None):
    """Tidy inputs for the dashboard artifact (build_artifact.py).
    listing_monthly: our figures per Listing x yearmonth.
    benchmarks: per Listing x yearmonth x benchmark (comp set / market high
    performers / whole market, both at the listing's bedroom size)."""
    # ADR incl. cleaning, allocated to stay nights (comparable to Wheelhouse w_fees)
    s = data[data["Term"] == "STR"][["Listing", "checkin_date", "checkout_date", "nights",
                                     "accommodation_fare", "cleaning_fee"]].copy()
    s = s[(s["nights"] > 0) & s["checkout_date"].notna()]
    s["per_night"] = (pd.to_numeric(s["accommodation_fare"], errors="coerce").fillna(0)
                      + pd.to_numeric(s["cleaning_fee"], errors="coerce").fillna(0)) / s["nights"]
    s["date"] = [pd.date_range(a, b - pd.Timedelta(days=1)) for a, b in
                 zip(pd.to_datetime(s["checkin_date"]), pd.to_datetime(s["checkout_date"]))]
    s = s.explode("date").dropna(subset=["date"])
    s["yearmonth"] = pd.to_datetime(s["date"]).dt.strftime("%Y-%m")
    adr_fees = s.groupby(["Listing", "yearmonth"], as_index=False).agg(ADR_w_fees=("per_night", "mean"))

    # revenue in the month: each booking's revenue split evenly over its nights, so a
    # stay that crosses a month end counts in both months (Revenue puts short-term
    # bookings in the check-in month). Built from the bookings themselves, not
    # Revenue_occ, which also folds unit nights into whole-house parents.
    rm = data[["Listing", "checkin_date", "checkout_date", "nights", "total_revenue"]].copy()
    rm = rm[(rm["nights"] > 0) & rm["checkout_date"].notna()]
    rm["per_night"] = pd.to_numeric(rm["total_revenue"], errors="coerce").fillna(0) / rm["nights"]
    rm["date"] = [pd.date_range(a, b - pd.Timedelta(days=1)) for a, b in
                  zip(pd.to_datetime(rm["checkin_date"]), pd.to_datetime(rm["checkout_date"]))]
    rm = rm.explode("date").dropna(subset=["date"])
    rm["yearmonth"] = pd.to_datetime(rm["date"]).dt.strftime("%Y-%m")
    rev_month = rm.groupby(["Listing", "yearmonth"], as_index=False).agg(Revenue_month=("per_night", "sum"))

    # guest-payment parts (rent / cleaning / tax / channel fee) — itemized from 2026,
    # the first year with the API's host ledger
    from revenue_mix import MIX_COLS, monthly_mix
    mix = monthly_mix(data, money_csv) if money_csv else pd.DataFrame(columns=["Listing", "yearmonth", *MIX_COLS])
    mix = mix[mix["yearmonth"] >= "2026-01"]

    # lead time (days from confirmation to check-in), per booking, by check-in month
    lt = data[(data["Term"] == "STR") & data["lead_time"].notna()][["Listing", "checkin_date", "lead_time"]].copy()
    lt["yearmonth"] = pd.to_datetime(lt["checkin_date"]).dt.strftime("%Y-%m")
    lead = lt.groupby(["Listing", "yearmonth"], as_index=False).agg(
        LeadTime=("lead_time", "mean"), Bookings=("lead_time", "size"))

    lm = (monthly[["Listing", "yearmonth", "Year", "Month", "Revenue", "Revenue_occ", "occdays",
                   "OccRt", "ADR", "Payout"]]
          .merge(adr_fees, on=["Listing", "yearmonth"], how="left")
          .merge(lead, on=["Listing", "yearmonth"], how="left")
          .merge(rev_month, on=["Listing", "yearmonth"], how="left")
          .merge(mix, on=["Listing", "yearmonth"], how="left")
          .merge(prop[["Listing", "Property", "Type", "Status", "Market", "BEDROOMS", "City", "Set"]],
                 on="Listing", how="left"))
    lm["Bedrooms"] = lm["BEDROOMS"].map(bedroom_bucket)
    lm["Group"] = lm.apply(listing_groups, axis=1)
    lm = lm.drop(columns=["Set", "City"])
    lm = safe_round(lm.drop(columns="BEDROOMS"))

    mk = attach_market(prop.loc[prop["Type"] == "STR"], market)
    # market high performers only at the listing's OWN bedroom size (they drive the
    # occupancy flag); never the all-sizes fallback, never for a listing with no count
    hp = mk[(mk["Occ_HP_src"] == "bedroom") & (mk["Bedrooms"] != "")]
    bench = [
        hp[["Listing", "yearmonth", "Occ_HP", "ADR_HP"]]
          .rename(columns={"Occ_HP": "Occ", "ADR_HP": "ADR"})
          .assign(benchmark="Market high performers", kind="market_hp"),
        mk[["Listing", "yearmonth", "Occ_All", "ADR_All"]]
          .rename(columns={"Occ_All": "Occ", "ADR_All": "ADR"})
          .assign(benchmark="Whole market", kind="market_all"),
    ]
    sets = load_sets(snap)
    bench.append(sets.assign(benchmark="Set: " + sets["set_name"], kind="set")
                     .drop(columns="set_name"))
    bench = safe_round(pd.concat(bench, ignore_index=True), ndigits=4)
    return lm, bench


def _month_starts(a, c):
    """First-of-month timestamps for every month a stay [a, c) has a night in."""
    return pd.date_range(a.to_period("M").to_timestamp(), (c - pd.Timedelta(days=1)).to_period("M").to_timestamp(),
                         freq="MS")


def split_ltr_by_month(data: pd.DataFrame, listings) -> pd.DataFrame:
    """Split LTR rows of `listings` into one row per calendar month (same daily
    rate), so a single month of a multi-month lease can be replaced."""
    is_split = (data["Term"] == "LTR") & data["Listing"].isin(listings) \
        & data["checkin_date"].notna() & data["checkout_date"].notna()
    keep, ltr = data[~is_split], data[is_split]
    pieces = []
    for _, r in ltr.iterrows():
        a, c = pd.Timestamp(r["checkin_date"]), pd.Timestamp(r["checkout_date"])
        if c <= a:
            continue
        for ms in _month_starts(a, c):
            s0, s1 = max(a, ms), min(c, ms + pd.offsets.MonthBegin(1))
            n = (s1 - s0).days
            if n <= 0:
                continue
            q = r.copy()
            q["checkin_date"], q["checkout_date"], q["nights"] = s0, s1, n
            q["earnings"] = q["total_revenue"] = r["DailyListingPrice"] * n
            q["accommodation_fare"] = r["AvgDailyRate"] * n
            q["yearmonth"] = ms.strftime("%Y-%m")
            pieces.append(q)
    return pd.concat([keep, pd.DataFrame(pieces)], ignore_index=True, sort=False)


def add_rent_roll(data: pd.DataFrame, path=RENT_ROLL):
    """Rent roll = source of truth for lease income (one row per listing x month,
    'Received rent'). For each listing-month with rent:
      * STR bookings that month  -> rent roll skipped (Guesty stay wins, no double count)
      * LTR lease rows that month -> replaced by the rent roll amount
      * nothing that month        -> rent roll added
    Months without rent in the rent roll keep the booking data unchanged.
    Each rent-roll month = one full-month LTR stay (code RR-<listing>-<yyyy-mm>)."""
    if not path.exists():
        print(f"  rent roll: {path} not found — skipped")
        return data, pd.DataFrame()
    x = pd.ExcelFile(path)
    rr = pd.concat([pd.read_excel(x, sh) for sh in x.sheet_names], ignore_index=True)
    rr = rr.dropna(subset=["Listing", "Month"])
    rr["Listing"] = rr["Listing"].astype(str).str.strip().replace(RENT_ROLL_ALIASES)
    rr["yearmonth"] = rr["Month"].map(lambda v: f"{float(v):.2f}".replace(".", "-"))
    raw = rr["Received rent"]
    is_date = raw.map(lambda v: hasattr(v, "year"))
    for _, r in rr[is_date].iterrows():
        print(f"  rent roll: WARNING {r['Listing']} {r['yearmonth']} rent is a date ({r['Received rent']}) — skipped")
    rr["rent"] = pd.to_numeric(raw.where(~is_date), errors="coerce")
    rr = rr[rr["rent"] > 0].groupby(["Listing", "yearmonth"], as_index=False)["rent"].sum()

    data = split_ltr_by_month(data, set(rr["Listing"]))
    sub = data[data["Listing"].isin(rr["Listing"])].dropna(subset=["checkin_date", "checkout_date"])
    sub = sub[pd.to_datetime(sub["checkout_date"]) > pd.to_datetime(sub["checkin_date"])]
    months = [_month_starts(pd.Timestamp(a), pd.Timestamp(c)).strftime("%Y-%m").tolist()
              for a, c in zip(sub["checkin_date"], sub["checkout_date"])]
    lm = sub.assign(ym=months).explode("ym")
    str_months = set(map(tuple, lm.loc[lm["Term"] != "LTR", ["Listing", "ym"]].values))
    ltr_amt = lm[lm["Term"] == "LTR"].groupby(["Listing", "ym"])["total_revenue"].sum()

    key = list(zip(rr["Listing"], rr["yearmonth"]))
    rr["booked_ltr"] = [ltr_amt.get(k, np.nan) for k in key]
    rr["action"] = np.where([k in str_months for k in key], "skipped (STR bookings)",
                            np.where(rr["booked_ltr"].notna(), "replaced", "added"))
    use = rr[rr["action"] != "skipped (STR bookings)"]
    drop = data["Listing"].isin(use["Listing"]) & (data["Term"] == "LTR") \
        & pd.Series(list(zip(data["Listing"], data["yearmonth"])), index=data.index).isin(
            set(zip(use["Listing"], use["yearmonth"])))
    data = data[~drop]

    start = pd.to_datetime(use["yearmonth"] + "-01")
    nights = ((start + pd.offsets.MonthBegin(1)) - start).dt.days
    rows = pd.DataFrame({
        "Listing": use["Listing"].values,
        "Confirmation.Code": ("RR-" + use["Listing"].str.replace(" ", "") + "-" + use["yearmonth"]).values,
        "checkin_date": start.values, "checkout_date": (start + pd.offsets.MonthBegin(1)).values,
        "nights": nights.values, "earnings": use["rent"].values, "total_revenue": use["rent"].values,
        "accommodation_fare": use["rent"].values, "cleaning_fee": 0.0,
        "DailyListingPrice": (use["rent"] / nights).values, "AvgDailyRate": (use["rent"] / nights).values,
        "booking_source": "rent_roll", "booking_platform": "Rent roll",
        "yearmonth": use["yearmonth"].values, "Term": "LTR",
    })
    c = rr["action"].value_counts()
    rep = rr[rr["action"] == "replaced"]
    print(f"  rent roll: {len(rr)} listing-months with rent — {c.get('replaced', 0)} replaced lease rows "
          f"(${rep['booked_ltr'].sum():,.0f} -> ${rep['rent'].sum():,.0f}), {c.get('added', 0)} added "
          f"(${rr.loc[rr['action'] == 'added', 'rent'].sum():,.0f}), "
          f"{c.get('skipped (STR bookings)', 0)} skipped (STR bookings that month)")
    return pd.concat([data, rows], ignore_index=True, sort=False), rr


# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------
def build(asof: date, guesty_csv, market_snapshot=None):
    today = asof

    # --- cell 1: bookings ---
    data = import_data(str(guesty_csv))
    data = data[~data["Listing"].isin(EXCLUDED_LISTINGS)].copy()
    data = data[~((data["booking_source"] == "owner") | (data["accommodation_fare"] == 0))].copy()
    data.loc[data["Listing"].eq("Seattle 3617 Origin"), "Listing"] = "Seattle 3617"
    data.loc[data["Listing"].eq("Seattle 906"), "Listing"] = "Seattle 906 Lower"
    prop = property_input()
    data, rent_roll = add_rent_roll(data)
    unknown = sorted(set(rent_roll.get("Listing", [])) - set(prop["Listing"]))
    if unknown:
        print(f"  rent roll: WARNING not in Property_Cohost (no Type/Market): {unknown}")
    occupancy, daily = cal_occupancy(data)

    # --- cell 4: owner payouts (paid month m covers revenue month m-1) ---
    owner_payout_last, OwnerPayout_last = build_owner_payout_2425()
    payout_month = today.month - (2 if today.day < 11 else 1)
    payout_month = max(payout_month, 0)   # notebook went negative in Jan/Feb
    owner_payout_curr, OwnerPayout_curr = build_owner_payout_26(payout_month)
    owner_payout = pd.concat([owner_payout_last, owner_payout_curr], ignore_index=True)
    owner_payout["Payout"] = pd.to_numeric(owner_payout["Payout"], errors="coerce")

    # --- cell 5: monthly ---
    monthly_STR = (data[data["Term"] == "STR"]
                   .groupby(["Listing", "yearmonth"], as_index=False)
                   .agg(Revenue=("total_revenue", "sum"))
                   .merge(occupancy[["Listing", "yearmonth", "occdays", "OccRt", "Revenue_occ",
                                     "Revenue_occ_accom", "minADR", "maxADR", "avgADR", "medADR"]],
                          on=["Listing", "yearmonth"], how="outer"))
    monthly_STR["ADR"] = monthly_STR["Revenue_occ_accom"] / monthly_STR["occdays"]
    monthly_LTR = (daily[daily["Term"] == "LTR"]
                   .groupby(["Listing", "yearmonth"], as_index=False)
                   .agg(occdays=("date", "count"), Revenue_defer=("DailyListingPrice", "sum"),
                        minADR=("AvgDailyRate", "min"), maxADR=("AvgDailyRate", "max"),
                        avgADR=("AvgDailyRate", "mean"), medADR=("AvgDailyRate", "median")))
    monthly_LTR["OccRt"] = monthly_LTR["occdays"] / monthly_LTR["yearmonth"].apply(days_in_month_from_yearmonth)
    monthly = monthly_STR.merge(monthly_LTR, on=["Listing", "yearmonth"], how="outer", suffixes=("", "_LTR"))
    monthly["Revenue"] = np.where(monthly["Revenue_defer"].isna(), monthly["Revenue"], monthly["Revenue_occ"])
    monthly["Month"] = monthly["yearmonth"].str[5:7]
    monthly["Year"] = monthly["yearmonth"].str[0:4]
    monthly = monthly.merge(owner_payout[["Property", "yearmonth", "Payout"]],
                            left_on=["Listing", "yearmonth"], right_on=["Property", "yearmonth"], how="left")
    monthly = monthly.merge(prop[["Listing", "Type", "Status"]], on="Listing", how="outer")
    monthly = monthly.loc[~monthly["yearmonth"].isna()].copy()

    # --- cell 7: monthly tab (wide by year) ---
    monthly_yrs = [yearrecords(monthly, y) for y in YEARS]
    for df in monthly_yrs:
        df["Month"] = df["Month"].astype(int)
    all_listings = pd.Index(pd.concat([df["Listing"] for df in monthly_yrs]).unique(), name="Listing")
    monthly_tab = pd.MultiIndex.from_product([all_listings, pd.Index(range(1, 13), name="Month")]).to_frame(index=False)
    for df in monthly_yrs:
        monthly_tab = monthly_tab.merge(df, on=["Listing", "Month"], how="left")
    monthly_tab = safe_round(monthly_tab.sort_values(["Listing", "Month"], kind="stable"))
    incr = []
    for a, b in zip(YEARS[:-1], YEARS[1:]):
        monthly_tab, r = yoy_delta(monthly_tab, monthly, a, b)
        incr.append(r)
    monthly_tab = safe_round(monthly_tab)
    monthly = monthly.merge(pd.concat(incr, ignore_index=True), on=["Listing", "yearmonth"], how="left")

    # --- cell 8: yearly ---
    yearly = monthly.groupby(["Listing", "Year"], as_index=False).agg(
        nights=("occdays", "sum"), Revenue=("Revenue", "sum"), ADR=("avgADR", "mean"))
    yearly_rev = monthly.copy()
    # fiscal year = Dec(prior) .. Nov
    dec = yearly_rev["yearmonth"].str.endswith("-12")
    yearly_rev.loc[dec, "Year"] = (yearly_rev.loc[dec, "Year"].astype(int) + 1).astype(str)
    yearly_rev = yearly_rev.groupby(["Listing", "Year"], as_index=False).agg(Revenue_fiscal=("Revenue", "sum"))
    yearly = yearly.merge(yearly_rev, on=["Listing", "Year"], how="left")
    firstbooking = daily.groupby(["Listing", "Year"], as_index=False).agg(firstbooking=("date", "min"))
    yearly = yearly.merge(firstbooking, on=["Listing", "Year"], how="left")
    yearly["firstbooking"] = pd.to_datetime(yearly["firstbooking"], errors="coerce")
    yearly["days_to_eoy"] = (pd.to_datetime({"year": yearly["firstbooking"].dt.year, "month": 12, "day": 31})
                             - yearly["firstbooking"]).dt.days + 1
    yearly["OccRt"] = yearly["nights"] / yearly["days_to_eoy"]
    yearly = safe_round(yearly)
    metrics = ["Revenue", "Revenue_fiscal", "ADR", "OccRt"]
    cols = [f"{m}_{y}" for m in metrics for y in YEARS]
    yearly_tab = (yearly[yearly["Year"].isin(YEARS)]
                  .pivot(index="Listing", columns="Year", values=metrics).reset_index())
    yearly_tab.columns = ["Listing", *cols]

    # --- cell 9: Valta occupancy by market (STR only) ---
    mask = ((data["total_revenue"] == 48.5) & (data["yearmonth"].isin(["2024-10", "2024-11"]))) \
        | (data["Confirmation.Code"].isin(TEST_CODES))
    data_adr = data[(~mask) & (data["checkin_date"] >= f"{YEARS[0]}-01-01")
                    & (data["checkin_date"] <= f"{CUR}-12-31")] \
        .merge(prop[["Listing", "Set", "Type", "Status"]], on="Listing", how="left")
    data_str = data_adr.loc[(data_adr["Status"] == "Active") & (data_adr["Type"] == "STR")
                            & (data_adr["nights"] <= 31)].copy()
    occupancy_str, _ = cal_occupancy(data_str)
    occ_valta = (occupancy_str.loc[occupancy_str["Year"].isin(YEARS),
                                   ["Listing", "yearmonth", "Year", "Month", "occdays", "avgADR"]]
                 .merge(prop[["Listing", "Market"]], on="Listing", how="left")
                 .groupby(["yearmonth", "Market"], as_index=False)
                 .agg(listings=("Listing", "nunique"), avgocc=("occdays", "mean"), avgADR=("avgADR", "mean")))
    occ_valta["avg_OccRt"] = occ_valta["avgocc"] / occ_valta["yearmonth"].apply(days_in_month_from_yearmonth)
    occ_valta = safe_round(occ_valta).sort_values(["Market", "yearmonth"])
    occ_valta = occ_valta[["yearmonth", "Market", "avg_OccRt", "avgADR"]]
    occ_valta["Year"] = occ_valta["yearmonth"].str[0:4]

    # --- cell 12: year-to-date (through the end of the as-of month) ---
    months_excl = range(today.month + 1, 13)
    ytd_end = pd.Timestamp(today.year, today.month, monthrange(today.year, today.month)[1])
    ytd_days = {y: (pd.Timestamp(int(y), today.month, monthrange(int(y), today.month)[1])
                    - pd.Timestamp(int(y), 1, 1)).days + 1 for y in YEARS}
    yearly_todate = (monthly.loc[~monthly["Month"].isin([f"{x:02d}" for x in months_excl])]
                     .groupby(["Listing", "Year"], as_index=False)
                     .agg(nights=("occdays", "sum"), Revenue=("Revenue", "sum"), ADR=("avgADR", "mean")))
    yearly_todate["OccRt"] = yearly_todate["nights"] / yearly_todate["Year"].map(ytd_days)

    # onboarding this year: denominator from the start date, not Jan 1
    first_ever = daily.groupby("Listing")["date"].min()
    auto = {l: d.strftime("%Y-%m-%d") for l, d in first_ever.items()
            if d.year == today.year and l not in ONBOARD}
    starts = {**auto, **ONBOARD}
    cur = yearly_todate["Year"] == CUR
    for listing, start in starts.items():
        idx = cur & (yearly_todate["Listing"] == listing)
        days = (ytd_end - pd.Timestamp(start)).days + 1
        yearly_todate.loc[idx, "OccRt"] = yearly_todate.loc[idx, "nights"] / days
    yearly_todate = safe_round(yearly_todate)
    cols_new = [f"{m}_{y}" for m in ["Revenue", "ADR", "OccRt"] for y in YEARS]
    yearly_todate_tab = (yearly_todate[yearly_todate["Year"].isin(YEARS)]
                         .pivot(index="Listing", columns="Year", values=["Revenue", "ADR", "OccRt"]).reset_index())
    yearly_todate_tab.columns = ["Listing", *cols_new]
    yearly_todate_tab = safe_round(yearly_todate_tab)
    denom = yearly_todate_tab[f"Revenue_{PREV}"].replace(0, np.nan)
    yearly_todate_tab["Ratio_today"] = yearly_todate_tab[f"Revenue_{CUR}"] / denom
    yearly_todate_tab["RevLoss_today"] = np.where(yearly_todate_tab["Ratio_today"] < 0.95, "Loss", "")

    # --- cell 13: owner payouts on yearly ---
    OwnerPayout = OwnerPayout_last.merge(OwnerPayout_curr, on="Listing", how="outer")
    OwnerPayout.columns = ["Listing", *[f"OwnerDist_{y}" for y in YEARS]]
    OwnerPayout = safe_round(OwnerPayout)
    yearly_tab = yearly_tab.merge(
        yearly_todate_tab[["Listing", f"Revenue_{CUR}", f"ADR_{CUR}", f"OccRt_{CUR}", "Ratio_today", "RevLoss_today"]],
        on="Listing", how="left", suffixes=["", "_today"])
    yearly_tab = yearly_tab.merge(OwnerPayout, on="Listing", how="left")
    yearly_tab = prop.loc[prop["Status"] == "Active", ["Listing", "Type"]].merge(yearly_tab, on="Listing")
    yearly_tab["Ratio25"] = np.where(yearly_tab["Revenue_2024"].isin([0, np.nan]), np.nan,
                                     yearly_tab["Revenue_2025"] / yearly_tab["Revenue_2024"])
    yearly_tab["RevLoss25"] = np.where(yearly_tab["Ratio25"] < 0.95, "Loss", "")

    # --- cell 14: payout tab ---
    Payout = (yearly_tab[["Listing", "Revenue_2024", "Revenue_2025", f"Revenue_{CUR}", f"Revenue_{CUR}_today"]]
              .merge(OwnerPayout, on="Listing", how="outer")
              .merge(prop[["Property", "Listing", "Type", "Status"]], on="Listing", how="left"))
    Payout = Payout[(Payout["Property"].isin(["Beachwood", "OSBR", "Mercer 3627"]))
                    | ((Payout["Type"] == "STR") & (Payout[f"Revenue_{CUR}_today"] >= 0))]
    rev_cols = ["Revenue_2024", "Revenue_2025", f"Revenue_{CUR}", f"Revenue_{CUR}_today"]
    Payout[rev_cols] = Payout[rev_cols].apply(pd.to_numeric, errors="coerce")
    num_col = Payout.select_dtypes(include=[np.number]).columns
    Payout_tab = Payout.groupby(["Property"], as_index=False).agg({c: "sum" for c in num_col})
    for c in num_col:
        Payout_tab[c] = Payout_tab[c].replace(0, np.nan)
    Payout_tab = safe_round(Payout_tab)
    Payout_tab["owner_pay_perc_last"] = Payout_tab[f"OwnerDist_{PREV}"] / Payout_tab[f"Revenue_{PREV}"]
    Payout_tab["owner_pay_perc_today"] = Payout_tab[f"OwnerDist_{CUR}"] / Payout_tab[f"Revenue_{CUR}_today"]
    Payout_tab["Delta"] = Payout_tab["owner_pay_perc_today"] - Payout_tab["owner_pay_perc_last"]
    Payout_tab["flag"] = np.where(Payout_tab["Delta"] < -0.02,
                                  np.where(Payout_tab[f"Revenue_{CUR}"] < Payout_tab[f"Revenue_{PREV}"], "High", "Med"),
                                  "Low")
    Payout_tab = safe_round(Payout_tab)
    Payout_tab = prop[["Property", "Type"]].drop_duplicates(subset="Property").merge(Payout_tab, on="Property", how="right")

    # --- cells 15-19: ratings ---
    yearly_tab = add_ratings(yearly_tab, data)

    # --- cells 20-23: market data, matched on market + bedrooms ---
    market, snap = load_market(market_snapshot)
    print(f"  market: Wheelhouse snapshot {snap}")
    lm = attach_market(prop.loc[prop["Type"] == "STR"], market)
    lm_cur = lm[lm["yearmonth"].str.startswith(CUR)].copy()
    lm_cur["Month"] = lm_cur["yearmonth"].str[5:].astype(int)

    # year-to-date: day-weighted mean over Jan .. as-of month
    t = lm_cur[lm_cur["Month"] <= today.month].copy()
    t["Days"] = t["yearmonth"].apply(days_in_month_from_yearmonth)
    for c in ["Occ_All", "Occ_HP"]:
        t[c + "_w"] = t[c] * t["Days"]
    market_today = t.groupby(["Listing", "Market", "Bedrooms"], as_index=False).agg(
        Occ_All_w=("Occ_All_w", "sum"), Occ_HP_w=("Occ_HP_w", "sum"), Days=("Days", "sum"),
        Occ_src=("Occ_HP_src", lambda s: "bedroom" if (s == "bedroom").all() else "market"))
    market_today["Occ_All_today"] = market_today["Occ_All_w"] / market_today["Days"]
    market_today["Occ_HP_today"] = market_today["Occ_HP_w"] / market_today["Days"]
    market_today = safe_round(market_today[["Listing", "Market", "Bedrooms", "Occ_src",
                                            "Occ_All_today", "Occ_HP_today"]])

    yearly_tab = yearly_tab.merge(market_today, on="Listing", how="left")
    occ = yearly_tab[f"OccRt_{CUR}_today"]
    yearly_tab["PerformanceFlag"] = np.where(
        occ.isna(), None,
        np.where(occ < yearly_tab["Occ_All_today"], "< Market Occ",
                 np.where(occ > yearly_tab["Occ_HP_today"], "> HP Occ", "Middle")))

    monthly_tab = monthly_tab.merge(
        lm_cur[["Listing", "Month", "Market", "Bedrooms", "Occ_All", "Occ_HP", "Occ_HP_src"]],
        on=["Listing", "Month"], how="left")
    occm = monthly_tab[f"OccRt_{CUR}"]
    monthly_tab["PerformanceFlag"] = np.where(
        occm.isna(), None,
        np.where(occm < (monthly_tab["Occ_HP"] - 0.1), "< HP-10%",
                 np.where(occm > (monthly_tab["Occ_HP"] + 0.1), "> HP+10%", "No change")))

    # --- cell 24: output frames ---
    yearly_out = yearly_tab.loc[(yearly_tab["Type"] == "STR") & (~yearly_tab["Listing"].isin(OUTPUT_EXCL))]
    out = {
        "yearly": yearly_out,
        "payout": Payout_tab.loc[Payout_tab["Type"] == "STR", PAYOUT_COLS],
        "monthly": monthly_tab,
        "monthly_long": monthly.assign(Year=lambda d: pd.to_numeric(d["Year"], errors="coerce").astype("Int64"),
                                       Month=lambda d: pd.to_numeric(d["Month"], errors="coerce").astype("Int64")),
    }
    tracking = {
        "yearly": yearly_out[["Listing", "Market", "Bedrooms", "Occ_src", "Occ_All_today",
                              "Occ_HP_today", "PerformanceFlag"]].assign(Date=today),
        "monthly": monthly_tab[["Listing", "Month", "Market", "Bedrooms", "Occ_All", "Occ_HP",
                                "PerformanceFlag"]].assign(Date=today),
    }
    info = {"auto_onboard": auto, "market_snapshot": snap, "rent_roll": rent_roll}
    money_csv = Path(guesty_csv).with_name(Path(guesty_csv).name.replace("Guesty_bookings_", "Guesty_summary_"))
    info["dashboard"] = dashboard_tables(data, monthly, prop, market, snap, money_csv)
    return out, occ_valta, tracking, info


YEARS_COLS = [
    "Listing", "Type", "Revenue_2024", "ADR_2024", "OccRt_2024",
    "Revenue_2025", "ADR_2025", "OccRt_2025", "Ratio25", "flag25", "RevLoss25",
    "Revenue_2026", "Revenue_2026_today", "ADR_2026_today", "OccRt_2026_today",
    "Ratio_today", "flag", "RevLoss_today",
    "2024.0_Airbnb", "2024.0_Booking.com", "2024.0_VRBO", "2025.0_Airbnb",
    "2025.0_Booking.com", "2025.0_VRBO", "Current_weighted_rating",
    "Current_rating_Airbnb", "Current_rating_Booking", "Current_rating_VRBO", "Nreview",
    "rating_2024", "rating_2025", "rating_2026", "reviews_2024", "reviews_2025", "reviews_2026",
    "OwnerDist_2024", "OwnerDist_2025", "OwnerDist_2026", "ADR_2026", "OccRt_2026",
    "Revenue_fiscal_2024", "Revenue_fiscal_2025", "Revenue_fiscal_2026"]
PAYOUT_COLS = ["Property", "Type", "Revenue_2024", "OwnerDist_2024", "Revenue_2025", "OwnerDist_2025",
               "Revenue_2026", "Revenue_2026_today", "OwnerDist_2026",
               "owner_pay_perc_last", "owner_pay_perc_today", "Delta", "flag"]


def _fmt_rating(val, n):
    return np.where(~np.isin(val, [0, np.nan]),
                    val.round(2).astype(str) + " (" + n.astype("Int64").astype(str) + ")", np.nan)


def add_ratings(yearly_tab, data):
    """Notebook cells 15-19: VA baseline (2025-09-28) + Guesty reviews since."""
    current = pd.read_excel(OVERALL_RATINGS)
    num_cols = [c for c in current.columns if re.search(r"(Number|Overall)", c)]
    for c in num_cols:
        current[c] = pd.to_numeric(current[c], errors="coerce")
    current[num_cols] = current[num_cols].round(2).fillna(0)
    current["Nreview"] = 0
    current["Current_weighted_rating"] = 0
    for k in ["Airbnb", "VRBO", "Booking"]:
        ncol, ocol = f"Number of reviews {k}", f"Overall {k}"
        idx = (current[ncol].isin([0, np.nan])) | (current[ocol].isin([0, np.nan]) & current[ncol].notna())
        current.loc[idx, [ocol, ncol]] = 0
        if k != "Airbnb":
            current[ocol] = current[ocol] / 2
        current["Nreview"] = current["Nreview"] + current[ncol]
        current["Current_weighted_rating"] = current["Current_weighted_rating"] + current[ncol] * current[ocol]
    current["Current_weighted_rating"] = current["Current_weighted_rating"] / current["Nreview"]

    latest = sorted(REVIEWS_DIR.glob("* guesty_reviews.xlsx"))[-1]
    print(f"  ratings: {latest.name}")
    ratings = pd.read_excel(latest)

    canceled = pd.read_csv(GUESTY_CANCELED, na_values=["", " "])
    canceled.columns = [re.sub(" |-|'", ".", c) for c in canceled.columns]
    for col in ["NUMBER.OF.ADULTS", "NUMBER.OF.CHILDREN", "NUMBER.OF.INFANTS", "PET.FEE"]:
        canceled[col] = np.nan
    platforms = pd.read_excel(SOURCE_PLATFORM)
    canceled_fmt = format_reservation(canceled.merge(platforms, on="SOURCE", how="left"),
                                      "2017-01-01", "2025-12-31")
    canceled_fmt["status"] = "canceled"
    guestydata = pd.concat([data.assign(status="confirmed"), canceled_fmt], ignore_index=True, sort=False)
    ratings = ratings.merge(
        guestydata[["Listing", "Confirmation.Code", "status", "checkin_date", "checkout_date", "booking_platform"]],
        left_on="Reservation", right_on="Confirmation.Code", how="left")
    ratings["createdAt"] = pd.to_datetime(ratings["createdAt"], errors="coerce")

    ratings_add = (ratings[ratings["createdAt"] > pd.Timestamp("2025-09-28")]
                   .groupby(["Listing", "booking_platform"], as_index=False)
                   .agg(number=("Overall", "count"), Overall=("Overall", "mean"))
                   .pivot(index="Listing", columns="booking_platform", values=["number", "Overall"])
                   .reset_index())
    ratings_add.columns = ["_".join([c for c in col if c]).strip("_") for col in ratings_add.columns.to_flat_index()]
    ratings_add["Overall_Booking"] = ratings_add["Overall_Booking.com"] / 2.0
    ratings_add["number_Booking"] = ratings_add["number_Booking.com"]
    cur_upd = current.merge(ratings_add, on="Listing", how="outer")
    cur_upd["Current_weighted_rating"] = cur_upd["Current_weighted_rating"] * cur_upd["Nreview"]
    for k in ["Airbnb", "VRBO", "Booking"]:
        ncol = cur_upd[f"Number of reviews {k}"].fillna(0)
        ocol = cur_upd[f"Overall {k}"].fillna(0)
        ncol1 = cur_upd[f"number_{k}"].fillna(0)
        ocol1 = cur_upd[f"Overall_{k}"].fillna(0)
        with np.errstate(divide="ignore", invalid="ignore"):
            cur_upd[f"Current_rating_{k}"] = _fmt_rating((ncol * ocol + ncol1 * ocol1) / (ncol + ncol1), ncol + ncol1)
        cur_upd["Nreview"] = cur_upd["Nreview"].fillna(0) + ncol1
        cur_upd["Current_weighted_rating"] = cur_upd["Current_weighted_rating"].fillna(0) + ncol1 * ocol1
    cur_upd["Current_weighted_rating"] = cur_upd["Current_weighted_rating"] / cur_upd["Nreview"]
    cur_upd = safe_round(cur_upd)

    ratings["checkin_date"] = pd.to_datetime(ratings.get("checkin_date"), errors="coerce")
    base = ratings.assign(year=lambda d: d["checkin_date"].dt.year,
                          score=np.where(ratings["booking_platform"] == "Booking.com",
                                         ratings["Overall"] / 2, ratings["Overall"])).query("year >= 2024")
    ratings_sum_yr = (base.groupby(["Listing", "year", "booking_platform"], as_index=False)
                      .agg(reviews=("Overall", "count"), overall=("Overall", "mean"))
                      .assign(Rating_reviews=lambda d: d["overall"].round(2).astype(str)
                              + " (" + d["reviews"].astype(str) + ")")
                      .pivot(index="Listing", columns=["year", "booking_platform"], values="Rating_reviews")
                      .reset_index())
    ratings_sum_yr.columns = ["_".join([str(c) for c in col if c]).strip("_")
                              for col in ratings_sum_yr.columns.to_flat_index()]
    ratings_all = (base.groupby(["Listing", "year", "booking_platform"], as_index=False)
                   .agg(reviews=("Overall", "count"), score=("score", "sum"))
                   .groupby(["Listing", "year"], as_index=False)
                   .agg(reviews=("reviews", "sum"), yearly_rating=("score", "sum")))
    ratings_all["rating"] = ratings_all["yearly_rating"] / ratings_all["reviews"]
    ratings_all = ratings_all.pivot(index="Listing", columns="year", values=["rating", "reviews"]).reset_index()
    ratings_all.columns = ["Listing"] + [f"{m}_{int(y)}" for m, y in ratings_all.columns[1:]]
    ratings_sum_yr = ratings_sum_yr.merge(ratings_all, on="Listing", how="left")

    yearly_tab = yearly_tab.merge(ratings_sum_yr, on="Listing", how="left").merge(
        cur_upd[["Listing", "Current_weighted_rating", "Nreview", "Current_rating_Airbnb",
                 "Current_rating_VRBO", "Current_rating_Booking"]], on="Listing", how="left")

    def compute_flag(row, ratio_col):
        if row.get("Type") == "LTR":
            return np.nan
        rating = row.get("Current_weighted_rating")
        low = pd.notna(rating) and rating < 4.8
        if row.get(ratio_col) == "Loss":
            return "High" if low else "Med"
        return "Low" if low else np.nan

    yearly_tab["flag"] = yearly_tab.apply(compute_flag, ratio_col="RevLoss_today", axis=1)
    yearly_tab["flag25"] = yearly_tab.apply(compute_flag, ratio_col="RevLoss25", axis=1)
    return yearly_tab[[c for c in YEARS_COLS if c in yearly_tab.columns]]


# ---------------------------------------------------------------------------
# Writers
# ---------------------------------------------------------------------------
def write_book(path, sheets: dict, formats=True):
    path.parent.mkdir(parents=True, exist_ok=True)
    with pd.ExcelWriter(path, engine="openpyxl") as ew:
        for name, df in sheets.items():
            sheet = name[:31]
            df.to_excel(ew, sheet_name=sheet, index=False, na_rep="")
            ws = ew.sheets[sheet]
            ws.freeze_panes = "A2"
            ws.auto_filter.ref = ws.dimensions
            if not formats:
                continue
            for pattern, fmt in FORMAT_RULES.values():
                rx = re.compile(pattern, flags=re.IGNORECASE)
                for j, c in enumerate(df.columns, start=1):
                    if not rx.search(str(c)):
                        continue
                    letter = get_column_letter(j)
                    for row in range(2, ws.max_row + 1):
                        cell = ws[f"{letter}{row}"]
                        if cell.value not in (None, ""):
                            cell.number_format = fmt
                    ws.column_dimensions[letter].width = 14
    print(f"  wrote {path}")


def main(argv=None):
    ap = argparse.ArgumentParser(description="Build the weekly revenue report.")
    ap.add_argument("--asof", default=date.today().isoformat())
    ap.add_argument("--guesty-csv", default=None, help="Default: data/guesty/Guesty_bookings_2026-<asof>.csv")
    ap.add_argument("--market-snapshot", default=None, help="Wheelhouse snapshot date (default: latest)")
    ap.add_argument("--publish", action="store_true", help="Also write the Google Drive copies")
    args = ap.parse_args(argv)

    asof = date.fromisoformat(args.asof)
    tag = asof.strftime("%Y%m%d")
    guesty_csv = args.guesty_csv or DATA_DIR / "guesty" / f"Guesty_bookings_2026-{tag}.csv"
    out, occ_valta, tracking, info = build(asof, guesty_csv, args.market_snapshot)
    if info["auto_onboard"]:
        print(f"  onboarding (auto, first stay this year): {info['auto_onboard']}")

    local = OUTPUT_DIR / asof.isoformat()
    local.mkdir(parents=True, exist_ok=True)
    lm, bench = info["dashboard"]
    lm.to_csv(local / "dash_listing_monthly.csv", index=False)
    bench.to_csv(local / "dash_benchmarks.csv", index=False)
    print(f"  dashboard data: {len(lm)} listing-months, {len(bench)} benchmark rows")
    if len(info["rent_roll"]):
        info["rent_roll"].to_csv(local / "rent_roll_applied.csv", index=False)
    targets = [(local / "RevenueReport_upd.xlsx", local / "Valta_OccupancyRate.xlsx",
                local / f"RevenuePerformanceTracking_{asof.isoformat()}.xlsx")]
    if args.publish:
        if not DRIVE.exists():
            raise SystemExit(f"--publish: no Google Drive folder at {DRIVE} (set VALTA_DRIVE)")
        targets.append((PUB_REPORT, PUB_OCCUPANCY,
                        PUB_TRACKING_DIR / f"RevenuePerformanceTracking_{asof.isoformat()}.xlsx"))
    for rep, occ, trk in targets:
        write_book(rep, out)
        write_book(occ, {"Occupancy": occ_valta}, formats=False)
        write_book(trk, tracking, formats=False)


if __name__ == "__main__":
    main()
