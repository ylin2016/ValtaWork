"""Revenue dashboard — reads the weekly report outputs (output/<date>/dash_*.csv).

    /Users/ylin/ValtaWork/.venv/bin/streamlit run dashboard.py

Views (sidebar):
  Company     revenue by month, 2024 vs 2025 vs 2026 (+ by market)
  Listing     one listing: revenue by month by year; occupancy and ADR vs its
              Wheelhouse comp set(s), market high performers and whole market
              (both at the listing's bedroom size)
  Portfolio   every listing vs its benchmarks, year-to-date or a single month
"""
from calendar import monthrange
from datetime import date

import altair as alt
import numpy as np
import pandas as pd
import streamlit as st

from paths import OUTPUT_DIR

st.set_page_config(page_title="Revenue dashboard", layout="wide")

MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
# Light blue / purple / pink (validated: CVD + normal-vision separation pass;
# below 3:1 contrast, so every chart keeps tooltips + a table/legend).
YEAR_COLORS = ["#ec6fa9", "#8660dc", "#4fb8e0"]           # oldest (pink) -> current (blue)
THIS_YEAR, LAST_YEAR = "#4fb8e0", "#8660dc"               # metric cards


# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------
def report_dates():
    return sorted([p.name for p in OUTPUT_DIR.iterdir()
                   if (p / "dash_listing_monthly.csv").exists()], reverse=True)


@st.cache_data
def load(run: str):
    d = OUTPUT_DIR / run
    lm = pd.read_csv(d / "dash_listing_monthly.csv")
    bench = pd.read_csv(d / "dash_benchmarks.csv")
    for df in (lm, bench):
        df["Year"] = df["yearmonth"].str[:4].astype(int)
        df["Month"] = df["yearmonth"].str[5:7].astype(int)
    lm["Bedrooms"] = lm["Bedrooms"].astype("string").fillna("")
    # LTR "ADR" = rent received / nights leased (lease rows can carry a 0 fare)
    ltr = lm["Type"] == "LTR"
    lm.loc[ltr, "ADR"] = lm.loc[ltr, "Revenue_occ"] / lm.loc[ltr, "occdays"].replace(0, np.nan)
    lm["days"] = [monthrange(y, m)[1] for y, m in zip(lm["Year"], lm["Month"])]
    bench["days"] = [monthrange(y, m)[1] for y, m in zip(bench["Year"], bench["Month"])]
    # report's year-to-date occupancy (onboarding-date aware) for the current year
    yearly = pd.read_excel(d / "RevenueReport_upd.xlsx", sheet_name="yearly")
    occ_col = [c for c in yearly.columns if c.startswith("OccRt_") and c.endswith("_today")]
    occ_ytd = yearly.set_index("Listing")[occ_col[0]] if occ_col else pd.Series(dtype=float)
    return lm, bench, occ_ytd


runs = report_dates()
if not runs:
    st.error(f"No dashboard data in {OUTPUT_DIR}. Run build_report.py first.")
    st.stop()

# ---- sidebar: view switch + all filters ------------------------------------
sb = st.sidebar
sb.title("Revenue dashboard")
page = sb.radio("View", ["Company", "Listing", "Portfolio"], key="page")
sb.divider()
run = sb.selectbox("Report date", runs)
lm_all, bench, occ_ytd = load(run)
asof = date.fromisoformat(run)
markets = sorted(lm_all["Market"].dropna().unique())
market = sb.selectbox("Market", ["All markets", *markets], key="market")
sel_markets = markets if market == "All markets" else [market]
term = sb.selectbox("Type", ["STR", "LTR", "All"], key="term")
TYPES = ["STR", "LTR"] if term == "All" else [term]
basis = sb.radio("Revenue basis", ["Check-in month (report)", "Stay nights"],
                 help="Check-in month = the report's Revenue column (whole booking in its "
                      "check-in month). Stay nights = revenue spread over the nights stayed.")
REV = "Revenue" if basis.startswith("Check-in") else "Revenue_occ"

lm = lm_all if market == "All markets" else lm_all[lm_all["Market"] == market]
if term != "All":
    lm = lm[lm["Type"] == term]
years = sorted(lm["Year"].unique())
years = [y for y in years if y >= asof.year - 2 and y <= asof.year]
ycolor = alt.Scale(domain=[str(y) for y in years], range=YEAR_COLORS[-len(years):])

st.title({"Company": "Company revenue", "Listing": "Listing performance",
          "Portfolio": "Portfolio vs benchmarks"}[page])


def money(v):
    return "—" if pd.isna(v) else f"${v:,.0f}"


def pct(v):
    return "—" if pd.isna(v) or np.isinf(v) else f"{v:+.1%}"


def short_money(v):
    """$548k / $1.2M / $950 — bar labels."""
    if pd.isna(v):
        return ""
    a = abs(v)
    if a >= 1e6:
        return f"${v / 1e6:.2f}M"
    if a >= 1e3:
        return f"${v / 1e3:.3g}k"
    return f"${v:,.0f}"


def grouped_month_bars(df, value, title, fmt="$,.0f"):
    """Month on x, one bar per year side by side, value label on top, hover tooltip."""
    df = df.assign(YearS=df["Year"].astype(str), MonthName=df["Month"].map(lambda m: MONTHS[m - 1]),
                   label=df[value].map(short_money))
    base = alt.Chart(df, title=title).encode(
        x=alt.X("MonthName:N", sort=MONTHS, title=None),
        xOffset=alt.XOffset("YearS:N"),
        y=alt.Y(f"{value}:Q", title=None, scale=alt.Scale(nice=True, domainMin=0),
                axis=alt.Axis(format=fmt.replace(".0f", "~s") if fmt.startswith("$") else fmt)))
    bars = base.mark_bar(cornerRadiusTopLeft=4, cornerRadiusTopRight=4, stroke="white", strokeWidth=2).encode(
        color=alt.Color("YearS:N", scale=ycolor, title="Year", legend=alt.Legend(orient="top")),
        tooltip=[alt.Tooltip("YearS:N", title="Year"), alt.Tooltip("MonthName:N", title="Month"),
                 alt.Tooltip(f"{value}:Q", title=title, format=fmt)])
    # vertical labels: 36 bars are too narrow for horizontal text
    labels = base.mark_text(angle=270, align="left", baseline="middle", dx=4, fontSize=10,
                            color="#52514e").encode(text="label:N")
    return (bars + labels).properties(height=380)


# ---------------------------------------------------------------------------
# Company
# ---------------------------------------------------------------------------
if page == "Company":
    co = (lm[lm["Year"].isin(years)].groupby(["Year", "Month"], as_index=False)[REV].sum()
          .rename(columns={REV: "Revenue"}))
    ytd = co[co["Month"] <= asof.month].groupby("Year")["Revenue"].sum()
    full = co.groupby("Year")["Revenue"].sum()

    st.caption(f"Year-to-date = January through {MONTHS[asof.month - 1]} "
               f"(the as-of month includes stays already booked for the rest of it).")
    k = st.columns(len(years))
    for i, y in enumerate(years):
        prev = ytd.get(y - 1)
        k[i].metric(f"{y} YTD revenue", money(ytd.get(y)),
                    None if prev in (None, 0) else pct(ytd.get(y) / prev - 1))

    st.altair_chart(grouped_month_bars(co, "Revenue", "Revenue by month"), width="stretch")

    # active listings = listings with booked nights or revenue in the month
    act = lm[lm["Year"].isin(years) & ((lm["occdays"].fillna(0) > 0) | (lm[REV].fillna(0) != 0))]
    n_m = act.groupby(["Month", "Year"])["Listing"].nunique().unstack()
    n_ytd = act[act["Month"] <= asof.month].groupby("Year")["Listing"].nunique()
    n_full = act.groupby("Year")["Listing"].nunique()

    rev = co.pivot(index="Month", columns="Year", values="Revenue")
    tbl = pd.DataFrame(index=range(1, 13))
    for y in years:
        tbl[f"{y} revenue"] = rev.get(y)
        tbl[f"{y} listings"] = n_m.get(y)
    for a, b in zip(years[:-1], years[1:]):
        tbl[f"{b} vs {a}"] = tbl[f"{b} revenue"] / tbl[f"{a} revenue"] - 1
    tbl.index = [MONTHS[m - 1] for m in tbl.index]
    extra = {}
    for label, r, n in [(f"YTD (Jan–{MONTHS[asof.month - 1]})", ytd, n_ytd), ("Full year", full, n_full)]:
        row = {}
        for y in years:
            row[f"{y} revenue"], row[f"{y} listings"] = r.get(y), n.get(y)
        for a, b in zip(years[:-1], years[1:]):
            row[f"{b} vs {a}"] = r.get(b) / r.get(a) - 1 if r.get(a) else np.nan
        extra[label] = row
    tbl = pd.concat([tbl, pd.DataFrame(extra).T])
    cfg = {}
    for c in tbl.columns:
        if c.endswith("revenue"):
            cfg[c] = st.column_config.NumberColumn(c, format="$%,.0f")
        elif c.endswith("listings"):
            cfg[c] = st.column_config.NumberColumn(c, format="%d",
                                                   help="Listings with booked nights or revenue in the period")
        else:
            cfg[c] = st.column_config.NumberColumn(c, format="percent")
    with st.expander("Table", expanded=True):
        st.dataframe(tbl, column_config=cfg, width="stretch")
        st.caption("Listings = listings with booked nights or revenue in that month "
                   "(YTD / full-year rows: in any month of the period).")

    bym = (lm[lm["Year"].isin(years) & (lm["Month"] <= asof.month)]
           .groupby(["Market", "Year"], as_index=False)[REV].sum().rename(columns={REV: "Revenue"}))
    bym["YearS"] = bym["Year"].astype(str)
    st.altair_chart(
        (lambda base: (
            base.mark_bar(cornerRadiusTopRight=4, cornerRadiusBottomRight=4, stroke="white", strokeWidth=2)
                .encode(color=alt.Color("YearS:N", scale=ycolor, title="Year", legend=alt.Legend(orient="top")),
                        tooltip=["Market", alt.Tooltip("YearS:N", title="Year"),
                                 alt.Tooltip("Revenue:Q", format="$,.0f")])
            + base.mark_text(align="left", baseline="middle", dx=4, fontSize=11, color="#52514e")
                  .encode(text="label:N")
        ).properties(height=max(260, 70 * bym["Market"].nunique())))(
            alt.Chart(bym.assign(label=bym["Revenue"].map(short_money)),
                      title=f"Year-to-date revenue by market (Jan–{MONTHS[asof.month - 1]})")
            .encode(y=alt.Y("Market:N", title=None,   # largest current-year market first
                            sort=bym[bym["Year"] == max(years)].sort_values("Revenue", ascending=False)["Market"].tolist()),
                    yOffset="YearS:N",
                    x=alt.X("Revenue:Q", title=None, scale=alt.Scale(nice=True, padding=30),
                            axis=alt.Axis(format="$~s", tickCount=6)))),
        width="stretch")


# ---------------------------------------------------------------------------
# Listing
# ---------------------------------------------------------------------------
SERIES_COLORS = ["#4fb8e0", "#8660dc", "#ec6fa9"]   # listing (blue) / comp set / market
LY_TICK = "#2b2a27"

STAT_CSS = """
<style>
.ss{display:flex;gap:10px;flex-wrap:wrap;margin:2px 0 4px}
.ss-i{flex:1;min-width:150px;border:1px solid #e9e4f7;border-radius:10px;padding:10px 14px;background:#fff}
.ss-top{display:flex;align-items:center;gap:6px;font-size:12px;color:#6b6a64}
.ss-dot{width:9px;height:9px;border-radius:2px;display:inline-block}
.ss-v{font-size:24px;font-weight:700;color:#1f1f1e;display:flex;align-items:center;gap:8px;margin-top:2px}
.ss-ly{font-size:12px;color:#8a8984}
.mc-badge{font-size:12px;font-weight:600;padding:2px 7px;border-radius:5px;white-space:nowrap}
.mc-up{background:#e3f4e8;color:#17663a}.mc-down{background:#fbe6e6;color:#a4262c}
.mc-flat{background:#efefec;color:#52514e}
</style>
"""


def _badge(cur, ly):
    if pd.isna(cur) or pd.isna(ly) or ly == 0:
        return ""
    d = cur / ly - 1
    cls, arrow = ("mc-up", "↗") if d > 0.005 else ("mc-down", "↘") if d < -0.005 else ("mc-flat", "")
    return f'<span class="mc-badge {cls}">{arrow} {d:+.0%}</span>'


CARD_CSS = """
<style>
.mc-wrap{display:grid;grid-template-columns:1fr;border:1px solid #e9e4f7;border-radius:10px;background:#fff;color:#1f1f1e}
.mc{display:flex;gap:28px;padding:18px 22px;align-items:flex-start;min-width:0}
.mc + .mc{border-top:1px solid #e9e4f7}
.mc-head{flex:0 0 230px;width:230px}
.mc-big{font-size:28px;font-weight:700;line-height:1.1;display:flex;align-items:center;gap:8px;flex-wrap:wrap}
.mc-label{font-size:13px;color:#6b6a64;margin-top:6px}
.mc-cmp{margin-top:12px;display:flex;flex-direction:column;gap:6px;font-size:12px;color:#52514e}
.mc-cmp span{display:flex;align-items:center;gap:6px;flex-wrap:wrap}
.mc-cmp b{font-weight:600;color:#1f1f1e}
.mc-dot{width:9px;height:9px;border-radius:2px;display:inline-block}
.mc-rows{flex:1 1 auto;display:flex;flex-direction:column;gap:9px;min-width:0}
.mc-row{display:grid;grid-template-columns:32px minmax(0,1fr) 160px;align-items:center;gap:12px;font-size:13px}
.mc-m{color:#52514e}
.mc-trks{display:flex;flex-direction:column;gap:2px}
.mc-track{position:relative;height:6px;width:100%}
.mc-track:before{content:"";position:absolute;left:0;right:0;top:50%;border-top:1px dotted #d6d0ea}
.mc-bar{position:absolute;left:0;top:0;height:6px;border-radius:0 3px 3px 0}
.mc-ly{position:absolute;top:-2px;width:2px;height:10px;background:#2b2a27;margin-left:-1px}
.mc-v{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap}
.mc-v small{color:#8a8984;font-size:11px}
.mc-legend{display:flex;gap:18px;flex-wrap:wrap;font-size:12px;color:#6b6a64;margin:8px 2px 0}
.mc-legend span{display:inline-flex;align-items:center;gap:6px}
.lg-ly{width:2px;height:12px;background:#2b2a27;display:inline-block}
</style>
"""


def metric_card(label, series, yr, head_m, fmt, full=None):
    """Wheelhouse-style card. series: [(name, color, {month: (this_year, last_year)})].
    One row per month; inside it one thin bar per series (this year) with a dark
    tick at the same month last year. Headline = first series (this listing)."""
    f = (lambda v: "—" if pd.isna(v) else fmt(v))
    vals = [v for _, _, d in series for pair in d.values() for v in pair if pd.notna(v)]
    scale = full if full else (max(vals) * 1.08 if vals else 1)
    w = (lambda v: f"{min(v / scale, 1):.1%}")
    (n0, c0, d0) = series[0]
    hc, hl = d0.get(head_m, (np.nan, np.nan))
    cmp = "".join(f'<span><i class="mc-dot" style="background:{c}"></i>{n}: <b>{f(d.get(head_m, (np.nan,))[0])}</b>'
                  f'{_badge(*d.get(head_m, (np.nan, np.nan)))}</span>' for n, c, d in series[1:])
    html = [f'<div class="mc"><div class="mc-head"><div class="mc-big">{f(hc)}{_badge(hc, hl)}</div>'
            f'<div class="mc-label">{label} · {MONTHS[head_m - 1]} {yr}</div>'
            f'<div class="mc-cmp">{cmp}</div></div><div class="mc-rows">']
    for m in range(1, 13):
        trks, tip = [], [MONTHS[m - 1]]
        for n, c, d in series:
            cur, ly = d.get(m, (np.nan, np.nan))
            bar = "" if pd.isna(cur) else f'<div class="mc-bar" style="width:{w(cur)};background:{c}"></div>'
            tick = "" if pd.isna(ly) else f'<div class="mc-ly" style="left:{w(ly)}"></div>'
            trks.append(f'<div class="mc-track">{bar}{tick}</div>')
            tip.append(f"{n}: {f(cur)} (last year {f(ly)})")
        others = " · ".join(f(d.get(m, (np.nan,))[0]) for _, _, d in series[1:])
        html.append(f'<div class="mc-row" title="{chr(10).join(tip)}"><span class="mc-m">{MONTHS[m - 1]}</span>'
                    f'<div class="mc-trks">{"".join(trks)}</div>'
                    f'<span class="mc-v">{f(d0.get(m, (np.nan,))[0])}'
                    f'{f" <small>{others}</small>" if others else ""}</span></div>')
    html.append("</div></div>")
    return "".join(html)


def range_chart(df, value, title, fmt, occ=False):
    """One row per year: min-max range bar, median tick, min/max labels."""
    df = df.assign(YearS=df["Year"].astype(str))
    x_scale = alt.Scale(domain=[0, 1], padding=36) if occ else alt.Scale(zero=False, nice=True, padding=48)
    base = alt.Chart(df, title=title).encode(
        y=alt.Y("YearS:N", title=None, sort="descending"),
        tooltip=[alt.Tooltip("YearS:N", title="Year"),
                 alt.Tooltip("lo:Q", title="Lowest month", format=fmt),
                 alt.Tooltip("med:Q", title="Median month", format=fmt),
                 alt.Tooltip("hi:Q", title="Highest month", format=fmt),
                 alt.Tooltip("n:Q", title="Months")])
    color = alt.Color("YearS:N", scale=ycolor, legend=None)
    bar = base.mark_bar(height=14, cornerRadius=7, opacity=0.85).encode(
        x=alt.X("lo:Q", title=None, scale=x_scale, axis=alt.Axis(
            format=fmt, values=[0, 0.25, 0.5, 0.75, 1] if occ else alt.Undefined)), x2="hi:Q", color=color)
    med = base.mark_tick(color="#2b2a27", thickness=2.5, size=22).encode(x="med:Q")
    lo = base.transform_filter("datum.lo != datum.hi").mark_text(
        align="right", dx=-6, fontSize=12, color="#52514e").encode(x="lo:Q", text=alt.Text("lo:Q", format=fmt))
    hi = base.mark_text(align="left", dx=6, fontSize=12, color="#52514e").encode(
        x="hi:Q", text=alt.Text("hi:Q", format=fmt))
    return (bar + med + lo + hi).properties(height=60 * len(df) + 30)


def ltr_view(one):
    """Long-term lease: revenue to date per year, ADR and occupancy ranges per year."""
    upto = asof.month
    d = one[one["Year"].isin(years)].copy()
    # revenue to date (Jan..as-of month) per year, plus full year for past years
    ytd = d[d["Month"] <= upto].groupby("Year")[REV].sum()
    full = d.groupby("Year")[REV].sum()
    tiles = ['<div class="ss">']
    for i, y in enumerate(years):
        prev = ytd.get(y - 1)
        badge = _badge(ytd.get(y, np.nan), prev if prev else np.nan)
        sub = (f"full year ${full.get(y, 0):,.0f}" if y < asof.year else
               f"booked through Dec ${full.get(y, 0):,.0f}")
        tiles.append(f'<div class="ss-i"><div class="ss-top"><i class="ss-dot" style="background:'
                     f'{YEAR_COLORS[-len(years) + i]}"></i>{y} · revenue Jan–{MONTHS[upto - 1]}</div>'
                     f'<div class="ss-v">${ytd.get(y, 0):,.0f}{badge}</div><div class="ss-ly">{sub}</div></div>')
    st.html(STAT_CSS + "".join(tiles) + "</div>")
    st.caption(f"Badge = vs the same months (Jan–{MONTHS[upto - 1]}) of the prior year.")

    # ranges over the months between the first and last lease month of each year (to date)
    d = d[d["Month"] <= upto]
    act = d[d["occdays"].fillna(0) > 0].groupby("Year")["Month"].agg(["min", "max"])
    d = d.merge(act, left_on="Year", right_index=True)
    d = d[(d["Month"] >= d["min"]) & (d["Month"] <= d["max"])]
    d["Occ"] = d["OccRt"].fillna(0).clip(upper=1)
    adr = d[d["occdays"] > 0].groupby("Year")["ADR"].agg(lo="min", med="median", hi="max", n="count").reset_index()
    occ = d.groupby("Year")["Occ"].agg(lo="min", med="median", hi="max", n="count").reset_index()
    if adr.empty:
        st.info("No lease months in this period.")
        return
    g1, g2 = st.columns(2)
    g1.altair_chart(range_chart(adr, "ADR", f"ADR range by year (Jan–{MONTHS[upto - 1]})", "$,.0f"),
                    width="stretch")
    g2.altair_chart(range_chart(occ, "Occ", f"Occupancy range by year (Jan–{MONTHS[upto - 1]})", ".0%", occ=True),
                    width="stretch")
    st.caption("Each bar runs from the lowest to the highest month; the dark tick is the median month. "
               "ADR = rent received ÷ nights leased. Occupancy counts the months between the year's first "
               "and last lease month, so a vacancy between tenants shows as a low minimum.")
    with st.expander("Monthly detail"):
        t = one[one["Year"].isin(years)][["yearmonth", REV, "occdays", "OccRt", "ADR"]] \
            .rename(columns={REV: "Revenue", "occdays": "Nights", "OccRt": "Occupancy"})
        st.dataframe(t.sort_values("yearmonth", ascending=False), hide_index=True, width="stretch",
                     column_config={"Revenue": st.column_config.NumberColumn(format="$%,.0f"),
                                    "ADR": st.column_config.NumberColumn(format="$%,.0f"),
                                    "Occupancy": st.column_config.NumberColumn(format="percent")})


if page == "Listing":
    sb.divider()
    sb.subheader("Listing")
    show_inactive = sb.toggle("Include inactive listings", value=False, key="inactive")
    pool = lm_all[lm_all["Type"].isin(TYPES) & (show_inactive | (lm_all["Status"] == "Active"))]
    pool = pool[pool["Market"].isin(sel_markets)]
    listings = sorted(pool["Listing"].dropna().unique())
    if not listings:
        st.info("No listings for these filters.")
    else:
        listing = sb.selectbox("Listing", listings, key="listing")
        one = lm_all[lm_all["Listing"] == listing]
        b1 = bench[(bench["Listing"] == listing)]
        info = one.dropna(subset=["Market"]).iloc[0] if one["Market"].notna().any() else None
        sets = sorted(b1.loc[b1["kind"] == "set", "benchmark"].str.removeprefix("Set: ").unique())
        is_ltr = info is not None and info["Type"] == "LTR"
        if info is not None:
            bench_txt = ("long-term lease — no Wheelhouse benchmarks" if is_ltr else
                         f"comp set: {', '.join(sets) if sets else '— none in Wheelhouse'}")
            st.markdown(f"**{listing}** · {info['Type']} · {info['Market']} · "
                        f"{info['Bedrooms'] or '?'} BR · {bench_txt}")

        if is_ltr:
            ltr_view(one)
        else:
            rv = one[one["Year"].isin(years)].groupby(["Year", "Month"], as_index=False)[REV].sum() \
                .rename(columns={REV: "Revenue"})
            st.altair_chart(grouped_month_bars(rv, "Revenue", "Revenue by month"), width="stretch")

            # each month of the chosen year: bar = that year, tick = same month a year earlier
            yr = sb.selectbox("Year", [y for y in sorted(years, reverse=True) if y > min(years)], key="li_year")
            head_m = sb.selectbox("Headline month", range(1, 13), index=asof.month - 1,
                                  format_func=lambda m: MONTHS[m - 1], key="head_m")
            if is_ltr:
                mkt_kind, adr_basis = "High performers", "Nightly rate"
            else:
                mkt_kind = sb.radio("Benchmark market", ["High performers", "Whole market"], key="mkt_kind")
                adr_basis = sb.radio("Our ADR", ["Nightly rate", "Nightly rate + cleaning"],
                                     key="adr_basis",
                                     help="Wheelhouse market ADR includes fees (adr_w_fees); comp-set ADR is "
                                          "nightly rate. Nightly rate = accommodation fare incl. markup ÷ nights.")
            ADR = "ADR" if adr_basis == "Nightly rate" else "ADR_w_fees"
            kind = "market_hp" if mkt_kind == "High performers" else "market_all"
            mkt_name = ("Market high performers" if kind == "market_hp" else "Whole market") \
                + (f", {info['Bedrooms']} BR" if info is not None and info["Bedrooms"] else "")
            set_name = ("Comp set" + (" (avg of 2)" if len(sets) > 1 else "")) if sets else "Comp set — none"

            series = {   # who -> DataFrame indexed by yearmonth with Occ / ADR
                "This listing": one.set_index("yearmonth").rename(columns={"OccRt": "Occ"})
                                   .assign(ADR=lambda d: d[ADR].fillna(d["ADR"]))[["Occ", "ADR"]],
            }
            if not is_ltr:   # leases have no comp set / market benchmark
                series[set_name] = b1[b1["kind"] == "set"].groupby("yearmonth")[["Occ", "ADR"]].mean()
                series[mkt_name] = b1[b1["kind"] == kind].set_index("yearmonth")[["Occ", "ADR"]]

            def val(df, y, m, col):
                return df[col].get(f"{y}-{m:02d}", np.nan)

            colors = dict(zip(series, SERIES_COLORS))
            cards = []
            for col, label, fmt, full in [("ADR", "ADR", lambda v: f"${v:,.0f}", None),
                                          ("Occ", "Occupancy", lambda v: f"{v:.0%}", 1.0)]:
                ser = [(who, colors[who], {m: (val(df, yr, m, col), val(df, yr - 1, m, col)) for m in range(1, 13)})
                       for who, df in series.items()]
                cards.append(metric_card(label, ser, yr, head_m, fmt, full))
            legend = "".join(f'<span><i class="mc-dot" style="background:{c}"></i>{n}</span>'
                             for n, c in colors.items())
            st.html(CARD_CSS + STAT_CSS + '<div class="mc-wrap">' + "".join(cards) + "</div>"
                    f'<div class="mc-legend">{legend}<span><i class="lg-ly"></i>same month {yr - 1}</span>'
                    f"<span>Right column: this listing, then {' · '.join(list(series)[1:])}</span></div>")
            st.caption(f"Bars = {yr}; dark tick = same month {yr - 1}. Hover a month for every value. "
                       "Badges = vs the same month last year."
                       + (" LTR ADR = rent received ÷ nights leased." if is_ltr else ""))
            st.caption("Wheelhouse occupancy is *adjusted* (booked ÷ available nights); ours is booked "
                       "nights ÷ days in month, so owner blocks count against us. Months from the report "
                       "date on are on-the-books; last year's are final.")


# ---------------------------------------------------------------------------
# Portfolio
# ---------------------------------------------------------------------------
if page == "Portfolio":
    sb.divider()
    sb.subheader("Portfolio")
    year = sb.selectbox("Year", sorted(years, reverse=True), key="pf_year")
    period = sb.radio("Period", ["Year to date", "Single month"], key="pf_period")
    last_m = asof.month if year == asof.year else 12
    if period == "Single month":
        pm = sb.selectbox("Month", range(1, 13), index=last_m - 1,
                          format_func=lambda m: MONTHS[m - 1], key="pf_month")
        months_sel = [pm]
        st.caption(f"{MONTHS[pm - 1]} {year} — every active listing, including vacant ones. "
                   f"Revenue prior year = {MONTHS[pm - 1]} {year - 1}. "
                   "Occupancy flag: above high performers / between / below whole market.")
    else:
        months_sel = list(range(1, last_m + 1))
        st.caption(f"Jan–{MONTHS[last_m - 1]} {year}, starting from each listing's first stay that year "
                   "(current year: the report's OccRt, which starts at the onboarding date). "
                   "Benchmarks are day-weighted over the same months. "
                   "Occupancy flag: above high performers / between / below whole market.")
    ytd_mode = period == "Year to date"

    cur = lm[lm["Type"].isin(TYPES) & (lm["Status"] == "Active") & (lm["Year"] == year)
             & (lm["Month"].isin(months_sel))]
    if ytd_mode:   # start at each listing's first stay that year
        first = cur[cur["occdays"] > 0].groupby("Listing")["Month"].min()
        cur = cur[cur["Month"] >= cur["Listing"].map(first)]
    else:
        first = pd.Series(months_sel[0], index=cur["Listing"].unique())
    ours = cur.groupby(["Listing", "Type", "Market", "Bedrooms"], as_index=False).agg(
        Revenue=(REV, "sum"), occdays=("occdays", "sum"), days=("days", "sum"),
        adr_num=("ADR", lambda s: (s * cur.loc[s.index, "occdays"]).sum()))
    if not ytd_mode:   # vacant listings have no row that month: add them at 0
        base = (lm[lm["Type"].isin(TYPES) & (lm["Status"] == "Active")]
                [["Listing", "Type", "Market", "Bedrooms"]].drop_duplicates("Listing"))
        ours = base.merge(ours.drop(columns=["Type", "Market", "Bedrooms"]), on="Listing", how="left")
        ours[["Revenue", "occdays", "adr_num"]] = ours[["Revenue", "occdays", "adr_num"]].fillna(0)
        ours["days"] = monthrange(year, months_sel[0])[1]
    ours["Occ"] = ours["occdays"].fillna(0) / ours["days"]
    if ytd_mode and year == asof.year:   # same figure as the report's OccRt_<year>_today
        ours["Occ"] = ours["Listing"].map(occ_ytd).fillna(ours["Occ"])
    ours["ADR"] = ours["adr_num"] / ours["occdays"].replace(0, np.nan)

    prev = lm[(lm["Year"] == year - 1) & (lm["Month"].isin(months_sel))].groupby("Listing")[REV].sum()
    ours["Revenue prior year"] = ours["Listing"].map(prev)
    ours["vs prior year"] = ours["Revenue"] / ours["Revenue prior year"].replace(0, np.nan) - 1

    bb = bench[(bench["Year"] == year) & (bench["Month"].isin(months_sel))]
    if ytd_mode:
        bb = bb[bb["Month"] >= bb["Listing"].map(first)]
    def wavg(df, col):
        df = df.dropna(subset=[col])
        return (df[col] * df["days"]).groupby([df["Listing"], df["kind"]]).sum() / \
            df["days"].groupby([df["Listing"], df["kind"]]).sum()
    # a listing in two sets: average its sets
    occ_b = wavg(bb, "Occ").unstack()
    adr_b = wavg(bb, "ADR").unstack()
    for k, lab in [("set", "Set"), ("market_hp", "HP"), ("market_all", "Market")]:
        ours[f"Occ {lab}"] = ours["Listing"].map(occ_b.get(k, pd.Series(dtype=float)))
        ours[f"ADR {lab}"] = ours["Listing"].map(adr_b.get(k, pd.Series(dtype=float)))
    ours["Occ vs HP (pts)"] = (ours["Occ"] - ours["Occ HP"]) * 100
    ours["Flag"] = np.select(
        [ours["Occ"] > ours["Occ HP"], ours["Occ"] < ours["Occ Market"]],
        ["▲ above HP", "▼ below market"], "● between")
    ours.loc[ours["Occ HP"].isna() & ours["Occ Market"].isna(), "Flag"] = "no benchmark"
    ours.loc[ours["Type"] == "LTR", "Flag"] = "lease"      # no STR benchmark for leases

    show = ours[["Listing", "Type", "Market", "Bedrooms", "Revenue", "Revenue prior year", "vs prior year",
                 "Occ", "Occ Set", "Occ HP", "Occ Market", "Occ vs HP (pts)", "Flag",
                 "ADR", "ADR Set", "ADR HP"]].sort_values("Occ vs HP (pts)")
    counts = show["Flag"].value_counts()
    flags_all = ["▲ above HP", "● between", "▼ below market", "no benchmark", "lease"]
    flags_sel = sb.multiselect("Flag", [f for f in flags_all if f in counts.index],
                               default=[f for f in flags_all if f in counts.index],
                               key=f"pf_flags_{term}")   # resets when the type filter changes
    show = show[show["Flag"].isin(flags_sel)]
    m1, m2, m3 = st.columns(3)
    for col, (key, lab, style) in zip((m1, m2, m3), [
            ("▲ above HP", "▲ above high performers", "#dff3e6;color:#17663a"),
            ("● between", "● between", "#fdf1d3;color:#8a5a00"),
            ("▼ below market", "▼ below whole market", "#fbe3e3;color:#a4262c")]):
        col.html(f'<div style="background:{style};border-radius:10px;padding:10px 16px">'
                 f'<div style="font-size:13px;font-weight:600">{lab}</div>'
                 f'<div style="font-size:30px;font-weight:700">{int(counts.get(key, 0))}</div></div>')
    flag_style = {"▲ above HP": "background-color:#dff3e6;color:#17663a;font-weight:600",
                  "● between": "background-color:#fdf1d3;color:#8a5a00;font-weight:600",
                  "▼ below market": "background-color:#fbe3e3;color:#a4262c;font-weight:600",
                  "lease": "background-color:#efefec;color:#52514e",
                  "no benchmark": "background-color:#efefec;color:#52514e"}
    pct_cols = ["vs prior year", "Occ", "Occ Set", "Occ HP", "Occ Market"]
    usd_cols = ["Revenue", "Revenue prior year", "ADR", "ADR Set", "ADR HP"]
    num_cols = pct_cols + usd_cols + ["Occ vs HP (pts)"]
    show[num_cols] = show[num_cols].apply(pd.to_numeric, errors="coerce")
    styled = (show.style
              .map(lambda v: flag_style.get(v, ""), subset=["Flag"])
              .format("{:.0%}", subset=pct_cols, na_rep="—")
              .format("${:,.0f}", subset=usd_cols, na_rep="—")
              .format("{:+.1f}", subset=["Occ vs HP (pts)"], na_rep="—"))
    st.dataframe(styled, hide_index=True, width="stretch", height=620,
                 column_order=[c for c in show.columns if not (c == "Type" and term != "All")])
    st.caption(f"{len(show)} listing(s) shown.")
    st.caption("ADR = nightly rate (accommodation fare incl. markup ÷ nights). ADR HP is Wheelhouse "
               "adr_w_fees (includes fees), so it reads high against ours; ADR Set is nightly rate.")
