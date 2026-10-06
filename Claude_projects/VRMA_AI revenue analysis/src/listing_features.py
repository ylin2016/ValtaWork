"""One row per active listing with every signal the action report uses ->
data/listing_features.pkl"""
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
RAW = ROOT / "data" / "raw"

gl = pd.read_csv(RAW / "guesty_listings.csv")
cal = pd.read_csv(RAW / "guesty_calendar.csv")
res = pd.read_csv(RAW / "guesty_reservations.csv")
wl = pd.read_csv(RAW / "wh_listings.csv")
st = pd.read_csv(RAW / "wh_listing_settings.csv")
nb = pd.read_csv(RAW / "wh_neighborhood_pricing.csv")
scan = pd.read_pickle(ROOT / "data" / "scan_all.pkl")
mp = pd.read_pickle(ROOT / "data" / "min_price_check.pkl")
mins = pd.read_excel(ROOT / "output" / "min_price_check_2026-10-06.xlsx", sheet_name="by_listing")

act = gl[gl.active == True].copy()
paid = res[(res.source != "owner") & (res.nights < 28) & (res.fare_accommodation_adjusted > 0)].copy()
paid["adr"] = paid.fare_accommodation_adjusted / paid.nights
ly = paid[(paid.check_in >= "2025-10-06") & (paid.check_in < "2026-04-04")]
fwd = paid[(paid.check_in >= "2026-10-06")]
first_res = paid.groupby("listing_id").check_in.min()

rows = []
for l in act.itertuples():
    lid = l.listing_id
    c = cal[cal.listing_id == lid]
    av = c[c.status == "available"]
    s = scan[scan.listing_id == lid]
    f = s[s.severity.notna()]
    n = nb[nb.listing_id == lid]
    stt = st[st.listing_id == lid]
    w = wl[wl.listing_id == lid]
    mr = mins[mins.listing == l.nickname]
    strong = f[f.step <= 2]
    down12 = strong[strong.side == "DOWN"]
    up12 = strong[strong.side == "UP"]
    rows.append({
        "listing_id": lid, "listing": l.nickname, "city": l.city, "bedrooms": l.bedrooms,
        "in_wheelhouse": not w.empty, "set_name": w.set_name.iloc[0] if not w.empty else None,
        "first_booking": first_res.get(lid), "ly_stays": int((ly.listing_id == lid).sum()),
        "ly_adr": ly[ly.listing_id == lid].adr.median(), "fwd_booked_adr": fwd[fwd.listing_id == lid].adr.median(),
        "fwd_booked_stays": int((fwd.listing_id == lid).sum()),
        "nights_total": len(c), "nights_avail": len(av), "nights_booked": int((c.status == "booked").sum()),
        "nights_blocked": int((c.status == "unavailable").sum()),
        "ask_median": av.price.median(), "comp_median": n.median_price.median() if len(n) else np.nan,
        "min_price": stt.min_price.iloc[0] if len(stt) else np.nan,
        "adjustment": stt.base_price_adjustment.iloc[0] if len(stt) else np.nan,
        "min_pattern": mr.pattern.iloc[0] if len(mr) else None,
        "nights_below_min": int(mr.nights_below_min.iloc[0]) if len(mr) else 0,
        "median_price_below_min": mr.median_price_below.iloc[0] if len(mr) else np.nan,
        "flags": len(f),
        "down_B": int(((f.side == "DOWN") & (f.severity == "BLUNDER")).sum()),
        "down_M": int(((f.side == "DOWN") & (f.severity == "MISTAKE")).sum()),
        "down_I": int(((f.side == "DOWN") & (f.severity == "INACCURACY")).sum()),
        "up_B": int(((f.side == "UP") & (f.severity == "BLUNDER")).sum()),
        "up_M": int(((f.side == "UP") & (f.severity == "MISTAKE")).sum()),
        "up_I": int(((f.side == "UP") & (f.severity == "INACCURACY")).sum()),
        "down_strong": len(down12), "up_strong": len(up12),
        "down_gap_median": (down12.floor / down12.price - 1).median() if len(down12) else np.nan,
        "up_gap_median": (1 - up12.ceiling / up12.price).median() if len(up12) else np.nan,
        "steps": s.step.value_counts().to_dict(),
    })
F = pd.DataFrame(rows)
F["down_share"] = (F.down_B + F.down_M + F.down_I) / F.nights_avail.replace(0, np.nan)
F["up_share"] = (F.up_B + F.up_M + F.up_I) / F.nights_avail.replace(0, np.nan)
F["ask_vs_ly"] = F.ask_median / F.ly_adr
F["ask_vs_comp"] = F.ask_median / F.comp_median
F.to_pickle(ROOT / "data" / "listing_features.pkl")
pd.set_option("display.width", 300)
print(F[["listing", "in_wheelhouse", "ly_stays", "nights_avail", "ask_median", "ly_adr", "fwd_booked_adr",
         "comp_median", "min_price", "adjustment", "min_pattern", "down_B", "down_M", "down_I", "up_B", "up_M",
         "up_I", "down_strong", "up_strong", "down_gap_median", "ask_vs_ly"]].round(2).to_string(index=False))
