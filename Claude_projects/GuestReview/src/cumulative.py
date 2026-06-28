"""Running *cumulative* (lifetime) rating per listing per channel, month by month.

Two reconstruction methods, by channel:

* Airbnb  — we can't recompute Airbnb's lifetime average from the review file
  alone (it only goes back to 2022 and is the Guesty export, not Airbnb's own
  count), so we ANCHOR on a snapshot:
      Property_OverallRatings.xlsx (Extract Date 2025-09-28) gives, per Airbnb
      listing, the lifetime `Overall Airbnb` average and review count.
        baseline_sum = avg*count        baseline_count = count
  Then add only reviews with createdAt strictly AFTER the snapshot (so the
  baseline's pre-snapshot reviews aren't double-counted).

* VRBO / Booking.com / Expedia — no snapshot. Accumulate the review file FROM
  THE BEGINNING (earliest review), so the cumulative at month m is the average
  of every such review with createdAt <= end of m.

In both cases we store cum_sum AND cum_count so channels roll up to an
"All channels" combined rating as a count-weighted mean. The display window
starts at `summary_start` (Oct 2025); for the non-Airbnb channels the totals
still include all earlier history, we just don't plot the pre-window months.

Join detail: the snapshot's `Listing` matches the review `nickname`
(canonicalized via listing_key), NOT `Property`.
"""
from __future__ import annotations

import pandas as pd

from normalize import (allowed_listing_keys, canon_listing, load_config,
                       project_path)

CFG = load_config()
SNAPSHOT = pd.Timestamp(CFG["cumulative"]["snapshot_date"])
SUMMARY_START = CFG["cumulative"]["summary_start"]
BASELINE_FAMILY = CFG["cumulative"]["channel_family"]   # the channel that uses the snapshot
COMBINED = "all"


def load_baseline() -> pd.DataFrame:
    """Per-Airbnb-listing baseline: listing_key, baseline_count, baseline_sum.

    'No Rating' / 'Not Listed' / blank => no baseline contribution (0 / 0)."""
    raw = pd.read_excel(project_path(CFG["paths"]["baseline_xlsx"]))
    out = pd.DataFrame()
    out["listing_key"] = raw["Listing"].map(canon_listing)
    cnt = pd.to_numeric(raw["Number of reviews Airbnb"], errors="coerce")
    avg = pd.to_numeric(raw["Overall Airbnb"], errors="coerce")
    valid = cnt.notna() & avg.notna()
    out["baseline_count"] = cnt.where(valid, 0).fillna(0).astype(int)
    out["baseline_sum"] = (avg * cnt).where(valid, 0.0).fillna(0.0)
    out = out.dropna(subset=["listing_key"])
    allow = allowed_listing_keys()           # same managed-listing restriction
    if allow is not None:
        out = out[out["listing_key"].isin(allow)]
    return out.groupby("listing_key", as_index=False).agg(
        baseline_count=("baseline_count", "sum"),
        baseline_sum=("baseline_sum", "sum"))


def _labels(reviews: pd.DataFrame) -> pd.Series:
    """listing_key -> display name (falls back to the key for baseline-only keys)."""
    return (reviews.dropna(subset=["listing"]).drop_duplicates("listing_key")
                   .set_index("listing_key")["listing"])


def _family_cumulative(reviews: pd.DataFrame, family: str, last_month: str) -> pd.DataFrame:
    """Per (listing_key, month) cumulative for one channel family."""
    sub = reviews[(reviews["channel_family"] == family)
                  & reviews["overall_5"].notna()].copy()

    if family == BASELINE_FAMILY:
        base = load_baseline().set_index("listing_key")
        sub = sub[sub["created_dt"] > SNAPSHOT]
        start_month = SUMMARY_START            # baseline already covers pre-snapshot
        keys = sorted(set(base.index) | set(sub["listing_key"].dropna()))
    else:
        base = None
        if sub.empty:
            return pd.DataFrame()
        start_month = min(sub["month"].min(), SUMMARY_START)  # accumulate from the start
        keys = sorted(set(sub["listing_key"].dropna()))

    if not keys:
        return pd.DataFrame()

    nm = (sub.groupby(["listing_key", "month"])
             .agg(new_count=("overall_5", "size"), new_sum=("overall_5", "sum"))
             .reset_index())
    months = pd.period_range(start_month, last_month, freq="M").strftime("%Y-%m")
    grid = pd.MultiIndex.from_product([keys, months],
                                      names=["listing_key", "month"]).to_frame(index=False)
    df = grid.merge(nm, on=["listing_key", "month"], how="left").fillna(
        {"new_count": 0, "new_sum": 0.0})

    if base is not None:
        df["seed_count"] = df["listing_key"].map(base["baseline_count"]).fillna(0)
        df["seed_sum"] = df["listing_key"].map(base["baseline_sum"]).fillna(0.0)
        df["has_baseline"] = df["seed_count"] > 0
    else:
        df["seed_count"], df["seed_sum"], df["has_baseline"] = 0.0, 0.0, False

    df = df.sort_values(["listing_key", "month"])
    df["cum_count"] = df["seed_count"] + df.groupby("listing_key")["new_count"].cumsum()
    df["cum_sum"] = df["seed_sum"] + df.groupby("listing_key")["new_sum"].cumsum()
    df["channel_family"] = family
    return df[["listing_key", "channel_family", "month", "new_count",
               "cum_count", "cum_sum", "has_baseline"]]


def cumulative_by_listing(reviews: pd.DataFrame) -> pd.DataFrame:
    """Long frame: one row per (listing, channel_family, month) within the
    display window, plus a combined channel_family='all' (count-weighted)."""
    last_month = reviews["month"].dropna().max()
    families = sorted(reviews["channel_family"].dropna().unique())
    parts = [d for d in (_family_cumulative(reviews, f, last_month) for f in families)
             if not d.empty]
    long = pd.concat(parts, ignore_index=True)

    # combined "all channels" = sum sums/counts across families per listing-month
    combined = (long.groupby(["listing_key", "month"])
                    .agg(new_count=("new_count", "sum"),
                         cum_count=("cum_count", "sum"),
                         cum_sum=("cum_sum", "sum"),
                         has_baseline=("has_baseline", "any"))
                    .reset_index())
    combined["channel_family"] = COMBINED
    long = pd.concat([long, combined], ignore_index=True)

    long = long[long["month"] >= SUMMARY_START].copy()
    long["cum_avg"] = (long["cum_sum"] / long["cum_count"]).where(
        long["cum_count"] > 0).round(3)
    label = _labels(reviews)
    long["listing"] = long["listing_key"].map(label).fillna(long["listing_key"])
    return long.sort_values(["listing", "channel_family", "month"])


def cumulative_company(reviews: pd.DataFrame) -> pd.DataFrame:
    """Company-wide running rating per channel_family (+ combined 'all'),
    count-weighted across all listings."""
    df = cumulative_by_listing(reviews)
    g = (df.groupby(["channel_family", "month"])
           .agg(cum_sum=("cum_sum", "sum"), cum_count=("cum_count", "sum"))
           .reset_index())
    g["cum_avg"] = (g["cum_sum"] / g["cum_count"]).where(g["cum_count"] > 0).round(3)
    return g.sort_values(["channel_family", "month"])


def coverage_report(reviews: pd.DataFrame) -> dict:
    """Surface Airbnb baseline join gaps (the other channels need no baseline)."""
    base = load_baseline()
    base_keys = set(base["listing_key"])
    air = reviews[(reviews["channel_family"] == BASELINE_FAMILY)
                  & (reviews["created_dt"] > SNAPSHOT)]
    post_keys = set(air["listing_key"].dropna())
    review_keys = set(reviews["listing_key"].dropna())
    label = _labels(reviews)

    return {
        "baseline_listings": len(base_keys),
        "baseline_with_rating": int((base["baseline_count"] > 0).sum()),
        "post_snapshot_airbnb_listings": len(post_keys),
        "new_airbnb_listings_no_baseline": sorted(label.get(k, k) for k in (post_keys - base_keys)),
        "baseline_listings_never_in_reviews": sorted(base_keys - review_keys),
    }


if __name__ == "__main__":
    from load_reviews import load_reviews
    d = load_reviews()
    print("COVERAGE REPORT")
    for k, v in coverage_report(d).items():
        print(f"  {k}: {v if not isinstance(v, list) else f'{len(v)} -> {v[:8]}'}")
    print("\nCompany cumulative by channel (latest month):")
    cc = cumulative_company(d)
    print(cc[cc.month == cc.month.max()].to_string(index=False))
    print("\nOne listing, all channels (tail):")
    cl = cumulative_by_listing(d)
    first = cl["listing"].iloc[0]
    print(cl[cl.listing == first].tail(10)[
        ["listing", "channel_family", "month", "cum_count", "cum_avg"]].to_string(index=False))
