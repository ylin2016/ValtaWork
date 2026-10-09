"""Guest-review dashboard.

Run:  streamlit run src/dashboard.py

Sidebar filters drive two modes:
  * a single listing selected -> per-listing detail view
  * "All listings (Company)"  -> portfolio view (trends, pivot, summary)

All scores are on the 0..5 scale. The cumulative Airbnb rating is a *lifetime*
metric and is intentionally computed on the full history (it ignores the date /
channel filters, which only affect the monthly/average views).
"""
from __future__ import annotations

import os

import altair as alt
import pandas as pd
import streamlit as st

from cumulative import cumulative_by_listing, cumulative_company, coverage_report
from data_source import ensure_data
from insights import (CATEGORIES, LOW, category_averages, low_score_reviews,
                      summarize, theme_counts)
from load_reviews import build, load_reviews
from metrics import (company_monthly, listing_overview, monthly_scores, pivot)
from normalize import load_config, project_path

st.set_page_config(page_title="Guest Reviews", layout="wide")
ALL = "All listings (Company)"
SCORE_COLS = ["overall_5"] + [f"{c}_5" for c in CATEGORIES]


@st.cache_resource
def _hydrate() -> bool:
    """Once per container: pull data inputs from Google Drive if configured
    (Streamlit Cloud), then build the SQLite if it isn't on disk yet. Local
    runs with data/ already populated and a prebuilt DB skip both steps."""
    ensure_data()
    if not os.path.exists(project_path(load_config()["paths"]["db"])):
        build(verbose=False)
    return True


@st.cache_data
def get_data() -> pd.DataFrame:
    return load_reviews()


@st.cache_data
def get_cumulative() -> pd.DataFrame:
    return cumulative_by_listing(load_reviews())


@st.cache_data
def get_cumulative_company() -> pd.DataFrame:
    return cumulative_company(load_reviews())


def score_chart(ms: pd.DataFrame, cats: list[str]):
    """Monthly average lines for the chosen score columns + a volume bar."""
    long = ms.melt(id_vars=["month", "reviews"], value_vars=cats,
                   var_name="metric", value_name="score").dropna(subset=["score"])
    long["metric"] = long["metric"].str.replace("_5", "", regex=False)
    line = (alt.Chart(long).mark_line(point=True)
            .encode(x=alt.X("month:O", title="Month"),
                    y=alt.Y("score:Q", scale=alt.Scale(domain=[0, 5]), title="Avg score (0-5)"),
                    color=alt.Color("metric:N", title="Metric"),
                    tooltip=["month", "metric", "score"])
            .properties(height=320))
    return line


def cumulative_chart(plot: pd.DataFrame):
    """Per-channel cumulative-rating lines (channel_family as color); the
    combined 'all' line is drawn thicker."""
    base = alt.Chart(plot).encode(
        x=alt.X("month:O", title="Month"),
        y=alt.Y("cum_avg:Q", scale=alt.Scale(zero=False), title="Cumulative rating"),
        color=alt.Color("channel_family:N", title="Channel"),
        tooltip=["month", "channel_family", "cum_avg", "cum_count"])
    others = base.transform_filter(alt.datum.channel_family != "all").mark_line(point=True)
    combined = (base.transform_filter(alt.datum.channel_family == "all")
                    .mark_line(point=True, strokeWidth=4))
    return (others + combined).properties(height=320)


def volume_chart(ms: pd.DataFrame):
    return (alt.Chart(ms).mark_bar(opacity=0.5)
            .encode(x=alt.X("month:O", title="Month"),
                    y=alt.Y("reviews:Q", title="# reviews"),
                    tooltip=["month", "reviews"])
            .properties(height=140))


# ---------------------------------------------------------------- sidebar
_hydrate()          # fetch data from Drive + build DB on first run (cloud)
df = get_data()
st.sidebar.title("Guest Reviews")
listings = [ALL] + sorted(df["listing"].dropna().unique().tolist())
selected = st.sidebar.selectbox("Listing", listings)

min_d, max_d = df["created_dt"].min(), df["created_dt"].max()
default_start = max(min_d, pd.Timestamp("2025-01-01"))  # default window: 2025 -> now
date_range = st.sidebar.date_input("Review date range", (default_start, max_d),
                                   min_value=min_d, max_value=max_d)
fams = sorted(df["channel_family"].unique())
chans = st.sidebar.multiselect("Channels", fams, default=fams)

# apply filters
mask = df["channel_family"].isin(chans)
if isinstance(date_range, (list, tuple)) and len(date_range) == 2:
    lo, hi = pd.Timestamp(date_range[0]), pd.Timestamp(date_range[1])
    mask &= df["created_dt"].between(lo, hi)
fdf = df[mask]

st.sidebar.caption(f"{len(fdf):,} reviews after filters")

# ================================================================ LISTING VIEW
if selected != ALL:
    ldf = fdf[fdf["listing"] == selected]
    alias = df.loc[df["listing"] == selected, "property_alias"].dropna()
    title = selected + (f"  ·  {alias.iloc[0]}" if not alias.empty else "")
    st.header(title)

    if ldf.empty:
        st.warning("No reviews for this listing in the selected filters.")
        st.stop()

    cum = get_cumulative()
    lcum = cum[cum["listing"] == selected]
    combined = lcum[lcum["channel_family"] == "all"]["cum_avg"].dropna()
    cur_cum = combined.iloc[-1] if not combined.empty else None

    c1, c2, c3, c4 = st.columns(4)
    c1.metric("Reviews", f"{len(ldf):,}")
    c2.metric("Avg overall (filtered)", f"{ldf['overall_5'].mean():.2f}")
    c3.metric("Cumulative (latest, all ch.)", "n/a" if cur_cum is None else f"{cur_cum:.2f}")
    c4.metric("Channels", ldf["channel_family"].nunique())

    ms = monthly_scores(ldf).sort_values("month")

    st.subheader("Average scores by month")
    overlay = st.multiselect(
        "Metrics to plot", SCORE_COLS,
        default=["overall_5"],
        format_func=lambda c: c.replace("_5", ""))
    if overlay:
        st.altair_chart(score_chart(ms, overlay), width='stretch')
    st.altair_chart(volume_chart(ms), width='stretch')

    st.subheader("Cumulative rating by month, per channel (Oct 2025 →)")
    plot = lcum.dropna(subset=["cum_avg"])
    if plot.empty:
        st.info("No cumulative data for this listing.")
    else:
        st.altair_chart(cumulative_chart(plot), width='stretch')
        st.caption("Airbnb is anchored to the 9/28/2025 lifetime snapshot; VRBO / "
                   "Booking / Expedia accumulate the review file from the beginning. "
                   "**all** = count-weighted across channels.")

    st.subheader("Monthly score table")
    show = ms.rename(columns=lambda c: c.replace("_5", "")).set_index("month")
    st.dataframe(show.style.format("{:.2f}", subset=[c.replace("_5", "") for c in SCORE_COLS]),
                 width='stretch')

    st.subheader("Insights")
    s = summarize(ldf, selected)
    bw = s["category_best_worst"]
    i1, i2, i3 = st.columns(3)
    if bw["best"]:
        i1.metric("Best category", bw["best"][0], f"{bw['best'][1]:.2f}")
        i2.metric("Worst category", bw["worst"][0], f"{bw['worst'][1]:.2f}")
    tr = s["trend"]
    i3.metric("Recent trend", tr["direction"],
              f"{tr['delta']:+.2f}" if tr["delta"] else None)

    tc = theme_counts(ldf)
    if not tc.empty:
        st.markdown("**Negative themes in low-score reviews**")
        st.bar_chart(tc)

    st.markdown(f"**Low-score reviews** (overall < {LOW:g}) — {s['low_count']} found")
    low = low_score_reviews(ldf)
    if low.empty:
        st.success("No low-score reviews in this selection.")
    else:
        disp = low.copy()
        disp["themes"] = disp["themes"].map(lambda t: ", ".join(t))
        disp["date"] = disp["created_dt"].dt.date
        st.dataframe(disp[["date", "channel_family", "overall_5", "themes", "review_text"]],
                     width='stretch', hide_index=True)

# ================================================================ COMPANY VIEW
else:
    st.header("Company — all listings")
    comp_cum = get_cumulative_company()
    comb = comp_cum[comp_cum["channel_family"] == "all"]["cum_avg"].dropna()
    cur = comb.iloc[-1] if not comb.empty else None
    k1, k2, k3, k4 = st.columns(4)
    k1.metric("Reviews (filtered)", f"{len(fdf):,}")
    k2.metric("Avg overall", f"{fdf['overall_5'].mean():.2f}")
    k3.metric("Listings", fdf["listing"].nunique())
    k4.metric("Cumulative (latest, all ch.)", "n/a" if cur is None else f"{cur:.2f}")

    t_trend, t_pivot, t_cum, t_insight, t_cover = st.tabs(
        ["Trends", "Pivot", "Cumulative", "Insights", "Data coverage"])

    with t_trend:
        cm = company_monthly(fdf)
        st.altair_chart(
            alt.Chart(cm).mark_line(point=True).encode(
                x=alt.X("month:O", title="Month"),
                y=alt.Y("overall_5:Q", scale=alt.Scale(domain=[0, 5]), title="Avg overall"),
                tooltip=["month", "overall_5", "reviews"]).properties(height=300),
            width='stretch')
        st.altair_chart(volume_chart(cm.rename(columns={"reviews": "reviews"})),
                        width='stretch')

    with t_pivot:
        metric = st.radio("Cell value", ["Average score", "Review count"], horizontal=True)
        pv = pivot(fdf, value="count" if metric == "Review count" else "score")
        st.caption("Listings (rows) × months (columns). Scroll horizontally for history.")
        fmt = "{:.0f}" if metric == "Review count" else "{:.2f}"
        styler = pv.style.format(fmt, na_rep="·")
        if metric == "Average score":
            styler = styler.background_gradient(cmap="RdYlGn", vmin=3, vmax=5, axis=None)
        st.dataframe(styler, width='stretch', height=600)

    with t_cum:
        st.subheader("Company cumulative rating, per channel (Oct 2025 →)")
        st.altair_chart(cumulative_chart(comp_cum.dropna(subset=["cum_avg"])),
                        width='stretch')
        st.caption("Count-weighted across all listings. Airbnb anchored to the "
                   "9/28/2025 snapshot; VRBO / Booking / Expedia accumulate the "
                   "review file from the beginning. **all** = weighted across channels.")

    with t_insight:
        s = summarize(fdf, "Company")
        bw = s["category_best_worst"]
        a, b, c = st.columns(3)
        if bw["best"]:
            a.metric("Best category", bw["best"][0], f"{bw['best'][1]:.2f}")
            b.metric("Worst category", bw["worst"][0], f"{bw['worst'][1]:.2f}")
        c.metric("Low-score reviews", s["low_count"])

        st.markdown("**Average category scores**")
        st.bar_chart(category_averages(fdf))

        ov = listing_overview(fdf)
        ov = ov[ov["reviews"] >= 5]  # ignore tiny samples in rankings
        lo, hi = st.columns(2)
        lo.markdown("**Top listings** (≥5 reviews)")
        lo.dataframe(ov.head(10)[["listing", "avg_overall", "reviews"]],
                     hide_index=True, width='stretch')
        hi.markdown("**Lowest listings** (≥5 reviews)")
        hi.dataframe(ov.tail(10).iloc[::-1][["listing", "avg_overall", "reviews"]],
                     hide_index=True, width='stretch')

        tc = theme_counts(fdf)
        if not tc.empty:
            st.markdown("**Negative themes across low-score reviews**")
            st.bar_chart(tc)

    with t_cover:
        st.caption("Join coverage between the reviews file and the 9/28 Airbnb baseline. "
                   "Listed items are surfaced, not silently dropped.")
        st.json(coverage_report(df))
