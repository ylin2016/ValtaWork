import pandas as pd, numpy as np
from pathlib import Path
D=Path("/Users/ylin/ValtaWork/Claude_projects/GuestyFinancials/data")

files=[("2026-06","reservations_2026-06_financials.xlsx"),
       ("2026-07","reservations_2026-07_financials.xlsx")]        # FRESH pulls
parts=[]
for month,f in files:
    s=pd.ExcelFile(D/f).parse("summary"); s["month"]=month
    s["data_source"]="API (active listing)"; parts.append(s)
# carry the 14 deactivated-listing reservations (API list excludes them) from the prior snapshot, IF it still exists
comp_path=D/"reservations_2026-07_COMPLETE.xlsx"
if comp_path.exists():
    comp=pd.ExcelFile(comp_path).parse("summary")
    if "data_source" in comp.columns:
        deact=comp[comp["data_source"].astype(str).str.contains("csv",case=False,na=False)].copy()
        deact["month"]="2026-07"; parts.append(deact)
        print(f"carried {len(deact)} deactivated-listing reservations from prior snapshot")
else:
    print("NOTE: reservations_2026-07_COMPLETE.xlsx not found -> 14 deactivated-listing reservations NOT included")
S=pd.concat(parts, ignore_index=True)

# the fetcher can return the same reservation on overlapping pages, and the unfiltered pull
# includes inquiries/declined/closed (not real bookings) -> dedupe + keep only booked statuses
n0=len(S)
S=S.drop_duplicates("confirmationCode", keep="first")
n1=len(S)
S=S[S["status"].astype(str).str.lower().isin(["confirmed","canceled","cancelled"])].reset_index(drop=True)
print(f"deduped {n0-n1} duplicate rows; kept {len(S)} confirmed/canceled (dropped {n1-len(S)} inquiry/declined/closed)")

# itemized tax lines — Guesty's money.totalTaxes UNDERCOUNTS (omits e.g. tax_destination), so sum these instead
TAXCOLS=["tax_occupancy","tax_state","tax_county","tax_city","tax_local","tax_reservation_total",
         "tax_other","tax_destination","tax_residential"]
num=["host_payout","host_service_fee","host_channel_fee","guest_service_fee","cleaning",
     "total_taxes","payment_fees","total_refunded","total_paid","balance_due","pm_commission","service_fee",
     "airbnb_resolution_center"]+TAXCOLS
for c in num:
    if c not in S: S[c]=0.0
S[num]=S[num].fillna(0.0)

# ---------------- building blocks ----------------
II    = S["host_payout"].astype(float)         # InvoiceItem = Σ invoiceItems, exactly as Guesty shows it
clean = S["cleaning"].astype(float)
svcf  = S["service_fee"].astype(float)         # manual/direct add-on "service fee" (kept as revenue)
tot_tax = S[TAXCOLS].sum(axis=1).astype(float) # ledger tax = Σ itemized tax lines (money.totalTaxes undercounts)
src   = S["source"].astype(str).str.lower()
has_pcm = S["host_channel_fee"].abs()>0.01     # channel fee already a line inside InvoiceItem?

# ---------------- assign each reservation to a payment-structure ROW ----------------
is_ab   = src.str.contains("airbnb")
is_haw  = is_ab & (tot_tax>0.01)               # Airbnb-Hawaii: tax sits in the host ledger (Keaau 15-1542)
grp = pd.Series("unknown", index=S.index)
grp[is_ab & ~is_haw] = "airbnb"
grp[is_haw]          = "airbnb_hawaii"
grp[src.isin(["vrbo","homeaway ca","homeaway de","homeaway uk","vrbo canada","expediaintegrated"])] = "homeaway"
grp[src.eq("expedia") &  has_pcm] = "expedia_pcm"     # integrated Expedia (PCM line present)
grp[src.eq("expedia") & ~has_pcm] = "expedia_grp"
grp[src.isin(["expedia affiliate network","hotels.com","travelocity","orbitz","american express travel"])] = "expedia_grp"
grp[src.eq("booking.com")] = "booking"
grp[src.isin(["capital one","hopper","hopper web/app"])] = "hopper"
grp[src.isin(["be-api","booking engine","direct","direct bookings","manual","owner","owner-guest","website"])] = "manual"
grp[src.eq("homesvillasbymarriott")] = "marriott"
grp[src.isin(["tripcom","trip.com"])] = "tripcom"
grp[src.eq("whimstay")] = "whimstay"
grp[src.eq("bluegroundnestpick")] = "blueground"
grp[src.eq("bnbfinder")] = "bnbfinder"

# ---------------- tax that lives INSIDE InvoiceItem (drives the channel-fee base & net revenue) ----------------
# non-airbnb + Hawaii-airbnb -> ledger tax; mainland airbnb -> 0 (Airbnb remits tax, not in the ledger)
tax_in_II = np.where(grp.eq("airbnb"), 0.0, tot_tax)

# ---------------- Airbnb tax for DISPLAY (property rate; base excludes ARC) ----------------
tmap=pd.read_csv(D/"_listing_tax_rates.csv")
rate_by_nick=dict(zip(tmap["nickname"], tmap["tax_rate_pct"]/100.0))
acct_nicks=set(tmap.loc[tmap["useAccountTaxes"]==True,"nickname"])
arc = np.where(is_ab, S["airbnb_resolution_center"].values, 0.0)     # Airbnb resolution line (signed)
rate_frac=S["listing"].map(rate_by_nick).mask(S["listing"].isin(acct_nicks))
ab_tax_known = is_ab & rate_frac.notna()

# ---------------- Channel fee = (InvoiceItem - tax) x rate ----------------
RATE={"airbnb":.155,"airbnb_hawaii":.155,"homeaway":.05,"expedia_pcm":.18,"expedia_grp":.18,
      "booking":.15,"hopper":.14,"marriott":.185,"tripcom":.111,"whimstay":.05,"blueground":.07,
      "manual":0.0,"bnbfinder":0.0,"unknown":0.0}
rate=grp.map(RATE).astype(float).values
cf_base = II.values - tax_in_II
channel_fee = np.round(cf_base*rate, 2)

# Airbnb display tax = ((InvoiceItem - ARC) + channel fee) * property_rate
ab_tax = (((II.values-arc)+channel_fee)*rate_frac.values)
airbnb_tax = np.where(ab_tax_known.values, np.round(ab_tax,2), 0.0)
tax_disp = np.where(is_ab.values, airbnb_tax, np.round(tot_tax.values,2))

# ---------------- Guesty fee (1%) + Stripe fee (tiered) -> host/Guesty-collected channels only ----------------
# Stripe = first $200 x 3.05% + remainder x 3%   (== 200*3.05%+(II-200)*3% for II>=200; clamps sensibly below 200)
has_processing = grp.isin(["homeaway","expedia_pcm","expedia_grp","manual","bnbfinder"]).values
tiered_stripe  = np.minimum(II.values,200.0)*0.0305 + np.maximum(II.values-200.0,0.0)*0.03
guesty_fee = np.where(has_processing, np.round(II.values*0.01, 2), 0.0)
stripe_fee = np.where(has_processing, np.round(tiered_stripe, 2), 0.0)

# ---------------- Guest pay (per-row formula) ----------------
gsf10 = np.round((II.values-tax_in_II)*0.10, 2)         # HomeAway guest service fee = (InvoiceItem-tax) x 10%
guest_pay = np.select(
    [grp.isin(["airbnb","airbnb_hawaii"]).values,
     grp.eq("homeaway").values,
     grp.isin(["expedia_pcm","hopper","marriott","whimstay","blueground"]).values],
    [II.values + channel_fee + tax_disp,
     II.values + gsf10,
     II.values + channel_fee],
    default=II.values)                                   # expedia_grp, booking, manual, tripcom, unknown -> InvoiceItem

# ---------------- Host payout (per-row formula) ----------------
payout_minus_proc = grp.isin(["homeaway","expedia_pcm","expedia_grp","manual"]).values  # NOT bnbfinder
host_payout = np.select(
    [grp.eq("booking").values,
     payout_minus_proc],
    [II.values - channel_fee,                            # Booking: InvoiceItem - channel fee
     II.values - stripe_fee - guesty_fee],               # HomeAway/Expedia/Manual: - stripe - Guesty
    default=II.values)                                   # channel-collected & bnbFinder: Host payout = InvoiceItem

# ---------------- Net revenue (per-row formula) ----------------
tax_sub = np.where(is_ab.values, tax_in_II, tot_tax.values)   # tax actually inside InvoiceItem
cf_in_net = np.isin(grp.values, ["homeaway","expedia_grp","booking","tripcom"])  # rows that subtract channel fee in net rev
net_revenue = (II.values - clean.values - tax_sub
               - np.where(cf_in_net, channel_fee, 0.0)
               - np.where(grp.eq("manual").values, svcf.values, 0.0)
               - stripe_fee - guesty_fee)

refund = np.round(-arc, 2)                               # display only (Airbnb resolution); never re-subtracted

out=pd.DataFrame({
    "month":S["month"], "confirmationCode":S["confirmationCode"], "channel":S["source"],
    "row_group":grp, "listing":S["listing"], "guest":S["guest"],
    "checkIn":S["checkIn"].astype(str).str[:10], "checkOut":S["checkOut"].astype(str).str[:10],
    "status":S["status"],
    "InvoiceItem":II.round(2),
    "guest_pay":np.round(guest_pay,2),
    "cleaning_fee":clean.round(2),
    "tax":np.round(tax_disp,2),
    "channel_fee":np.round(channel_fee,2),
    "guesty_fee":np.round(guesty_fee,2),
    "stripe_fee":np.round(stripe_fee,2),
    "host_payout":np.round(host_payout,2),
    "net_revenue":np.round(net_revenue,2),
    "refund":refund,
})

out["note"]=""
def add_note(mask, text):
    m=np.asarray(mask)
    out.loc[m,"note"]=(out["note"]+np.where(out["note"]=="","","; ")+text)[m]
add_note(arc!=0, "Airbnb resolution $"+pd.Series(arc,index=out.index).map(lambda v:f"{v:,.2f}")+" -> refund")
add_note(S["total_refunded"].values>0, "REFUND $"+S["total_refunded"].map(lambda v:f"{v:,.2f}"))
add_note(S["status"].astype(str).str.lower().isin(["canceled","cancelled"]).values, "CANCELED")
add_note(grp.eq("airbnb_hawaii").values, "Airbnb-Hawaii (tax in ledger)")
add_note((is_ab & ~ab_tax_known & (II>0)).values, "AIRBNB TAX: property rate not configured")
add_note(grp.eq("unknown").values, "UNMAPPED source -> passthrough")
if "data_source" in S.columns:
    add_note(S["data_source"].astype(str).str.contains("csv",case=False,na=False).values, "from CSV (deactivated listing)")

n_before=len(out)
out=out[out["InvoiceItem"].abs()>0.005].copy()          # drop $0 InvoiceItem rows (canceled/comp/owner)
print(f"dropped {n_before-len(out)} reservations with InvoiceItem = 0")
out=out.sort_values(["listing","checkIn"]).reset_index(drop=True)

OUT=D.parent/"output"; OUT.mkdir(exist_ok=True)
xlsx=OUT/"payment_breakdown_2026-06_07.xlsx"
with pd.ExcelWriter(xlsx, engine="openpyxl") as xw:
    out.to_excel(xw, sheet_name="reservations", index=False)
    (out.groupby(["month","channel"])[["guest_pay","channel_fee","guesty_fee","stripe_fee","host_payout","net_revenue","refund"]]
        .sum().round(2)).to_excel(xw, sheet_name="by_channel")
    # freeze the header row + turn on autofilter across the header
    ws=xw.sheets["reservations"]
    ws.freeze_panes="A2"
    ws.auto_filter.ref=ws.dimensions
print(f"WROTE {xlsx}  ({len(out)} reservations)")

pd.set_option("display.width",300); pd.set_option("display.max_columns",30)
print("\nSAMPLE (one per row_group):")
samp=out.groupby("row_group",group_keys=False).head(1)
cols=["channel","row_group","InvoiceItem","guest_pay","cleaning_fee","tax","channel_fee","guesty_fee","stripe_fee","host_payout","net_revenue","refund"]
print(samp[cols].to_string(index=False))

print("\nTOTALS by month:")
print(out.groupby("month")[["guest_pay","channel_fee","guesty_fee","stripe_fee","host_payout","net_revenue","refund"]].sum().round(2).to_string())

print("\nrow_group counts:"); print(grp.value_counts().to_string())
