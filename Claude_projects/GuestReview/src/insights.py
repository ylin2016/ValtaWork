"""Insight generation from scores and public-review text.

Two scopes: a single listing, or the whole company (pass the already-filtered
frame). Everything works off the 0..5 scale.
"""
from __future__ import annotations

import pandas as pd

from normalize import load_config

CFG = load_config()
CATEGORIES = CFG["categories"]
LOW = float(CFG["low_score_threshold"])
THEMES = CFG["negative_themes"]


def category_averages(df: pd.DataFrame) -> pd.Series:
    """Mean of each category (0..5), highest first. NaN categories dropped."""
    cols = [f"{c}_5" for c in CATEGORIES]
    means = df[cols].mean().round(3)
    means.index = [c.replace("_5", "") for c in means.index]
    return means.dropna().sort_values(ascending=False)


def best_worst_category(df: pd.DataFrame) -> dict:
    ca = category_averages(df)
    if ca.empty:
        return {"best": None, "worst": None}
    return {"best": (ca.index[0], float(ca.iloc[0])),
            "worst": (ca.index[-1], float(ca.iloc[-1]))}


def trend(df: pd.DataFrame, window: int = 3) -> dict:
    """Compare the mean overall_5 of the last `window` months vs the prior
    `window` months. Returns direction + delta."""
    m = (df.dropna(subset=["month", "overall_5"])
           .groupby("month")["overall_5"].mean().sort_index())
    if len(m) < 2:
        return {"direction": "n/a", "delta": 0.0, "recent": None, "prior": None}
    recent = m.tail(window).mean()
    prior = m.iloc[:-window].tail(window).mean() if len(m) > window else m.iloc[:-1].mean()
    delta = round(recent - prior, 3)
    direction = "improving" if delta > 0.05 else "declining" if delta < -0.05 else "flat"
    return {"direction": direction, "delta": delta,
            "recent": round(recent, 3), "prior": round(prior, 3)}


def tag_themes(text: str) -> list[str]:
    """Keyword-tag a review's text with negative themes (see config)."""
    if not isinstance(text, str) or not text.strip():
        return []
    low = text.lower()
    return [theme for theme, kws in THEMES.items() if any(k in low for k in kws)]


def low_score_reviews(df: pd.DataFrame, threshold: float = LOW) -> pd.DataFrame:
    """Reviews below the threshold, with their text and tagged themes,
    most recent first."""
    low = df[df["overall_5"] < threshold].copy()
    text = low["public_review"].fillna("")
    # fall back to Booking.com negative content when no public review
    text = text.where(text.str.strip().ne(""), low["booking_negative"].fillna(""))
    low["review_text"] = text
    low["themes"] = low["review_text"].map(tag_themes)
    cols = ["created_dt", "month", "listing", "channel_family",
            "overall_5", "review_text", "themes"]
    return low.sort_values("created_dt", ascending=False)[cols]


def theme_counts(df: pd.DataFrame, threshold: float = LOW) -> pd.Series:
    """Frequency of negative themes across low-score reviews."""
    low = low_score_reviews(df, threshold)
    counts: dict[str, int] = {}
    for themes in low["themes"]:
        for t in themes:
            counts[t] = counts.get(t, 0) + 1
    return pd.Series(counts).sort_values(ascending=False)


def summarize(df: pd.DataFrame, scope_label: str = "Selection") -> dict:
    """One bundle of insights for a scope (a listing or the whole company)."""
    overall = df["overall_5"].mean()
    return {
        "scope": scope_label,
        "reviews": len(df),
        "avg_overall": None if pd.isna(overall) else round(overall, 3),
        "category_best_worst": best_worst_category(df),
        "trend": trend(df),
        "low_count": int((df["overall_5"] < LOW).sum()),
        "theme_counts": theme_counts(df).to_dict(),
    }


if __name__ == "__main__":
    from load_reviews import load_reviews
    d = load_reviews()
    print("=== COMPANY ===")
    for k, v in summarize(d, "Company").items():
        print(f"  {k}: {v}")
    top = d["listing"].value_counts().index[0]
    print(f"\n=== LISTING: {top} ===")
    for k, v in summarize(d[d.listing == top], top).items():
        print(f"  {k}: {v}")
    print("\nLow-score sample:")
    print(low_score_reviews(d[d.listing == top]).head(3).to_string(index=False))
