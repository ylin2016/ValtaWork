"""Aggregations off the tidy `reviews` table.

All scores are already on the 0..5 scale (see normalize.py). Grouping key is
`property` unless otherwise noted.
"""
from __future__ import annotations

import pandas as pd

from normalize import load_config

CFG = load_config()
CATEGORIES = CFG["categories"]


def monthly_counts(df: pd.DataFrame, by: str = "listing") -> pd.DataFrame:
    """Number of reviews per listing per month (requirement 1)."""
    g = (df.dropna(subset=["month"])
           .groupby([by, "month"]).size().reset_index(name="reviews"))
    return g.sort_values([by, "month"])


def monthly_scores(df: pd.DataFrame, by: str = "listing") -> pd.DataFrame:
    """Average overall_5 + each category average + review count, per listing
    per month (requirement 2)."""
    score_cols = ["overall_5"] + [f"{c}_5" for c in CATEGORIES]
    agg = {c: "mean" for c in score_cols}
    g = (df.dropna(subset=["month"])
           .groupby([by, "month"]).agg(agg))
    g["reviews"] = df.dropna(subset=["month"]).groupby([by, "month"]).size()
    g = g.reset_index().sort_values([by, "month"])
    for c in score_cols:
        g[c] = g[c].round(3)
    return g


def company_monthly(df: pd.DataFrame) -> pd.DataFrame:
    """Company-wide monthly average overall_5 and review volume."""
    g = (df.dropna(subset=["month"])
           .groupby("month")
           .agg(overall_5=("overall_5", "mean"), reviews=("overall_5", "size"))
           .reset_index())
    g["overall_5"] = g["overall_5"].round(3)
    return g.sort_values("month")


def pivot(df: pd.DataFrame, value: str = "score", by: str = "listing") -> pd.DataFrame:
    """Listings (rows) x months (cols). value='score' -> mean overall_5;
    value='count' -> review counts."""
    sub = df.dropna(subset=["month"])
    if value == "count":
        return (sub.pivot_table(index=by, columns="month", values="overall_5",
                                aggfunc="size", fill_value=0))
    return (sub.pivot_table(index=by, columns="month", values="overall_5",
                            aggfunc="mean").round(2))


def listing_overview(df: pd.DataFrame, by: str = "listing") -> pd.DataFrame:
    """One row per listing: lifetime review count, avg overall_5, #channels,
    last review month. Used to rank best/worst listings company-wide."""
    g = (df.groupby(by)
           .agg(reviews=("overall_5", "size"),
                avg_overall=("overall_5", "mean"),
                channels=("channel_family", "nunique"),
                last_month=("month", "max"))
           .reset_index())
    g["avg_overall"] = g["avg_overall"].round(3)
    return g.sort_values("avg_overall", ascending=False)


if __name__ == "__main__":
    from load_reviews import load_reviews
    d = load_reviews()
    print("monthly_counts sample:\n", monthly_counts(d).head())
    print("\nmonthly_scores sample:\n", monthly_scores(d).head())
    print("\ncompany_monthly tail:\n", company_monthly(d).tail())
    print("\nlisting_overview head:\n", listing_overview(d).head())
