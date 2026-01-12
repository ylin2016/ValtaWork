# -------------------------------------------------------------------
# Small helpers
# -------------------------------------------------------------------

def days_in_month_from_yearmonth(ym: str) -> int:
    """ym like '2024-05' -> number of days in that month."""
    y, m = map(int, ym.split("-"))
    return monthrange(y, m)[1]

def to_yearmonth(dtcol: pd.Series) -> pd.Series:
    return pd.to_datetime(dtcol).dt.strftime("%Y-%m")

def substr_year(s: str) -> str:
    return s[:4]

def safe_round(df: pd.DataFrame, ndigits: int = 2) -> pd.DataFrame:
    num = df.select_dtypes(include=[np.number]).columns
    df[num] = df[num].round(ndigits)
    return df
# ---- normalize Property: if starts with number, swap order ----
def reorder_property(name):
    if isinstance(name, str):
        parts = name.split()
        if len(parts) == 2 and parts[0].isdigit():
            return f"{parts[1]} {parts[0]}"
    return name

# Owner payouts (2024–2025)
# -------------------------------------------------------------------
def build_owner_payout(endmonth):
    pay_path = "/Users/ylin/My Drive/Cohost/Accounting/"
    y25 = [f"2025.{m:02d}" for m in range(1, 13)]
    y24 = [s.replace("2025","2024") for s in y25]
    owner_payout = []
    # 2025 payouts: sheets from 2024.12 and 2025.11 in "01-OwnerPayout Records.xlsx"\\
    for k in [y24[11], *y25[:11]]:  
        tmp = pd.read_excel(pay_path+"/* Monthly/0-Process & Template/Old files/2025 OwnerPayout Records.xlsx", sheet_name=k)
        idx = tmp["Date"].notna() & (tmp['Payout'].notna()) & (tmp['Payout'] != 0)
        tmp["Date"]=k
        take = tmp.loc[idx, ["Date","Property","Payout"]].copy()
        owner_payout.append(take)
        
    # 2024 payouts: earlier 2024 sheets from legacy workbook
    old_path = pay_path+"/* Monthly/0-Process & Template/Old files/2024 OwnerPayout Records.xlsx"
    for k in ["2023.12", *y24[:11]]:
        tmp = pd.read_excel(old_path, sheet_name=k)
        if k =="2023.12":
            idx = (tmp['Owner payout'].notna()) & (tmp['Owner payout'] != 0) &(tmp["Property"].str.contains("Total")==False)
            take = tmp.loc[idx, ["Property","Owner payout"]].copy()
            take.insert(0, "Date", k)
            take.columns = ["Date","Property","Payout"]
            take["Property"] = take["Property"].apply(reorder_property)
            take["Property"] = take["Property"].apply(lambda x: x.capitalize() if isinstance(x, str) else x)
        else:
            if k != "2024.11":
                idx = (tmp['Payout'].notna()) & (tmp['Payout'] != 0) &(tmp["Property"].str.contains("Total")==False)
                take = tmp.loc[idx, ["Property","Payout"]].copy()
                take.insert(0, "Date", k)
            else:
                idx = tmp["Date"].notna() & (tmp['Payout'].notna()) & (tmp['Payout'] != 0) & (tmp["Property"].str.contains("Total")==False)
                take = tmp.loc[idx, ["Date","Property","Payout"]].copy()
                take["Date"] = take["Date"].astype(str)   
        owner_payout.append(take)

    owner_payout = pd.DataFrame(pd.concat(owner_payout, ignore_index=True))
    owner_payout["PayPeriod"] = owner_payout["Date"]
    owner_payout["Date"]= pd.to_datetime(owner_payout["Date"],format="%Y.%m") + pd.DateOffset(months=1)
    owner_payout["Date"]= owner_payout["Date"].astype(str).str[:7]

    # Manually add Jing's property payouts
    jing_property = pd.DataFrame({"Date":["2024.05"]*4+["2024.06"]*4+["2024.07"]*4,
                    "Property":["Elektra 1004", "Elektra 1108","Elektra 1115","Microsoft 14615-D303"]*3,
                    "Payout": [-325.99,3942.64,1627.11,3150.69, 4345.90,1249.86,3954.50,3929.28,\
                            1824.40,2392.14,3247.69,4060.26]})
    idx = owner_payout['Property'].str.contains("Jing", na=False)
    owner_payout = pd.concat([owner_payout.loc[~idx], jing_property], ignore_index=True)
    owner_payout["Year"] = owner_payout["Date"].str[:4]
    owner_payout["yearmonth"] = owner_payout["Date"]#.str.replace(".", "-")
    owner_payout["Property"] = owner_payout["Property"].str.strip()
    owner_payout["Property"] = owner_payout["Property"].str.replace(r"\bIsland\s+", "", regex=True)
    owner_payout["Property"] = owner_payout["Property"].str.replace("SeaTac", "Seatac", regex=False)

    # Recode property names
    txt = [
        "Microsoft 14645 C19","Microsoft 14620 E205","E205","D201",
        "Kirkland D201","Kirkland 11321 - corrected","OSBR - All","OSBR",
        "Burien 14407 Middle","Burien 14407 Top","Seatac 12834 - Lower",
        "Seatac 12834 - Upper","Bellevue 13020 - already paid","Seattle 906",
        "1424c seattle",'1430b seattle',"C19 bellevue",
    ]
    change = [
        "Microsoft 14645-C19","Microsoft 14620-E205","Total","Kirkland 8252-D201",
        "Kirkland 8252-D201","Kirkland 11321","Cottages All OSBR","Cottages All OSBR",
        "Burien 14407 middle","Burien 14407 top","Seatac 12834 Lower",
        "Seatac 12834 Upper","Bellevue 13020","Seattle 906 Lower",
        "Seattle 1424C","Seattle 1430B","Microsoft 14645-C19",
    ]
    mapping = dict(zip(txt, change))
    owner_payout["Property"] = owner_payout["Property"].replace(mapping)

    # Filter out specific duplicate line
    mask_dupe = (
        (owner_payout["Property"] == "Kirkland 11321") &
        (owner_payout["yearmonth"] == "2024-04") &
        (owner_payout["Payout"] == 1515.68)
    )
    owner_payout = owner_payout.loc[~mask_dupe].copy()

    # Collapse to (Property, yearmonth)
    owner_payout = (
        owner_payout.loc[~owner_payout["Property"].astype(str).str.contains("Total", na=False)]
        .dropna(subset=["Payout"])
    )
    owner_payout = owner_payout[owner_payout["Payout"] != 0]
    owner_payout = (
        owner_payout.groupby(["Property", "yearmonth"], as_index=False)
        .agg(Year=("yearmonth", lambda s: s.iloc[0][:4]), Payout=("Payout", "sum"))
        .drop_duplicates()
    )
    owner_payout.loc[owner_payout["Property"]=="Bellevue 14507","Property"] = "Bellevue 14507U3"
    owner_payout["Payout"]= pd.to_numeric(owner_payout["Payout"], errors="coerce")

    # OwnerPayout wide by Year
    owner_dist = (
        owner_payout.groupby(["Year", "Property"], as_index=False)["Payout"].sum()
        .rename(columns={"Payout": "OwnserDist"})
    )
    owner_dist = owner_dist[(owner_dist["OwnserDist"] > 0) & owner_dist["Property"].notna()]
    owner_wide = owner_dist.pivot(index="Property", columns="Year", values="OwnserDist").reset_index()
    owner_wide.columns.name = None  # remove MultiIndex name
    owner_wide = owner_wide.rename(columns={"Property": "Listing"})

    comb_data = owner_wide[owner_wide["Listing"].str.contains(r"Beachwood", case=False, na=False)]
    num_col1 = comb_data.select_dtypes(include=[np.number]).columns
    comb_sum = comb_data.assign(Listing="Beachwood").groupby("Listing", as_index=False).agg({c:"sum" for c in num_col1})

    mask = owner_wide["Listing"].str.contains(r"Beachwood", case=False, na=False)
    owner_wide = pd.concat([owner_wide[~mask],comb_sum],ignore_index=True)
    return owner_payout, owner_wide

def cal_occupancy(data):
    """Calculate occupancy rate by listing and yearmonth."""
    daily = (
        data[["checkin_date","checkout_date","Confirmation.Code","Listing",\
            "DailyListingPrice","AvgDailyRate","Term"]].copy()
    )
    daily["checkin_date"] = pd.to_datetime(daily["checkin_date"])
    daily["checkout_date"] = pd.to_datetime(daily["checkout_date"])
    # Create list of dates per row (nights only: exclude checkout)
    daily["date_list"] = daily.apply(
        lambda r: pd.date_range(r["checkin_date"], r["checkout_date"] - pd.Timedelta(days=1), freq="D")
        if pd.notna(r["checkin_date"]) and pd.notna(r["checkout_date"]) and r["checkout_date"] > r["checkin_date"]
        else pd.DatetimeIndex([], dtype="datetime64[ns]"),
        axis=1
    )
    daily = daily.explode("date_list").rename(columns={"date_list": "date"}).dropna(subset=["date"])
    daily["yearmonth"] = to_yearmonth(daily["date"])
    daily["Year"] = daily["date"].dt.strftime("%Y")
    daily["Month"] = daily["date"].dt.strftime("%m")

    # Occupancy & revenue (by stayed-nights)
    occ = (
        daily.groupby(["Listing","yearmonth","Year","Month"], as_index=False)
        .agg(
            occdays=("date", "count"),
            Revenue_occ=("DailyListingPrice", "sum"),
            Revenue_occ_accom=("AvgDailyRate","sum"),
            minADR = ("AvgDailyRate","min"),
            maxADR = ("AvgDailyRate","max"),
            avgADR = ("AvgDailyRate","mean"),
            medADR = ("AvgDailyRate","median"),
        )
    )
    occ["OccRt"] = occ["yearmonth"].map(lambda ym: occ.loc[occ["yearmonth"].eq(ym), "occdays"].iloc[0])  # placeholder
    # Compute OccRt properly per row:
    occ["OccRt"] = occ["yearmonth"].apply(lambda ym: np.nan)  # will replace below
    occ["OccRt"] = occ["occdays"] / occ["yearmonth"].apply(days_in_month_from_yearmonth)
    return occ,daily

def yoy_delta(monthly_tab, monthly, prev: str, curr: str) -> pd.DataFrame:
    """Calculate year-over-year delta and percentage change between two years.""" 
    delta_col = f"RevenueIncr{prev}{curr}"
    perc_col = f"RevenueIncr{prev}{curr}_perc"
    monthly_tab = monthly_tab.assign(
        **{
            delta_col: monthly_tab[f"Revenue_{curr}"] - monthly_tab[f"Revenue_{prev}"],
            perc_col: (monthly_tab[f"Revenue_{curr}"] - monthly_tab[f"Revenue_{prev}"]) / monthly_tab[f"Revenue_{prev}"]
        }
    )
    # Append YoY increments back to monthly (per yearmonth)
    rev_incr = monthly_tab[["Listing","Month",delta_col,perc_col]].copy()
    rev_incr["yearmonth"] = curr + "-" + rev_incr["Month"].astype(str)
    rev_incr = rev_incr.rename(columns={delta_col: "RevenueIncr", perc_col: "RevenueIncr_perc"})
    rev_incr = rev_incr[["Listing","yearmonth","RevenueIncr","RevenueIncr_perc"]]
    return monthly_tab,rev_incr

def combine_osbr_beachwood(yearly_table):
    """Combine OSBR and Beachwood listings into a single entry."""
    osbr_data = yearly_table[yearly_table["Listing"].str.contains(r"Cottage|OSBR", case=False, na=False)]
    osbr_data
    osbr_summary = (
        osbr_data.assign(Listing="Cottages All OSBR").groupby("Listing")
        .agg(
            Revenue_2024=("Revenue_2024", "sum"),
            Revenue_2025=("Revenue_2025", "sum"),
            Revenue_2026=("Revenue_2026", "sum")
        ).reset_index()
    )
    # Remove individual OSBR and Beachwood entries
    beachwood = yearly_table[yearly_table["Listing"].str.contains(r"Beachwood", case=False, na=False)]
    beachwood_summary = (beachwood.assign(Listing="Beachwood").groupby("Listing")
                        .agg(
            Revenue_2024=("Revenue_2024", "sum"),
            Revenue_2025=("Revenue_2025", "sum"),
            Revenue_2026=("Revenue_2026", "sum")).reset_index()
            )
    # Append combined entry
    mask = yearly_table["Listing"].str.contains(r"Cottage|OSBR|Beachwood", case=False, na=False)
    yearly_table = pd.concat([yearly_table[~mask], osbr_summary,beachwood_summary], 
                             ignore_index=True)
    return yearly_table
