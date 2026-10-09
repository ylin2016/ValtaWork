"""Ingest the Guesty reviews xlsx into a tidy SQLite `reviews` table.

Run:  python src/load_reviews.py
Rebuilds data/reviews.sqlite from scratch each time (idempotent).
"""
from __future__ import annotations

import sqlite3

import pandas as pd

from normalize import (allowed_listing_keys, load_config, normalize_reviews,
                       project_path)


def _dedupe(df: pd.DataFrame) -> tuple[pd.DataFrame, int]:
    """Drop duplicate reservationId rows, preferring the row with an Overall
    score. Rows with no reservationId are kept (valid reviews without an id)."""
    has_id = df["reservationId"].notna()
    keyed, unkeyed = df[has_id].copy(), df[~has_id].copy()
    # Stable sort so rows with a non-null Overall win when we keep="first".
    keyed["_has_overall"] = pd.to_numeric(keyed["Overall"], errors="coerce").notna()
    keyed = keyed.sort_values("_has_overall", ascending=False, kind="stable")
    before = len(keyed)
    keyed = keyed.drop_duplicates(subset="reservationId", keep="first")
    dropped = before - len(keyed)
    keyed = keyed.drop(columns="_has_overall")
    return pd.concat([keyed, unkeyed], ignore_index=True), dropped


def build(verbose: bool = True) -> pd.DataFrame:
    cfg = load_config()
    raw = pd.read_excel(project_path(cfg["paths"]["reviews_xlsx"]))
    raw, dropped = _dedupe(raw)
    tidy = normalize_reviews(raw, cfg)

    # Restrict to the managed-listing allow-list (Listing_contacts.csv).
    allow = allowed_listing_keys()
    excluded = 0
    if allow is not None:
        keep = tidy["listing_key"].isin(allow)
        excluded = int((~keep).sum())
        tidy = tidy[keep].reset_index(drop=True)

    db_path = project_path(cfg["paths"]["db"])
    with sqlite3.connect(db_path) as conn:
        # store datetimes as ISO strings for portability
        out = tidy.copy()
        for col in ("created_dt", "check_in", "check_out"):
            out[col] = out[col].dt.strftime("%Y-%m-%d")
        out.to_sql("reviews", conn, if_exists="replace", index=False)

    if verbose:
        print(f"Loaded {len(tidy)} reviews ({dropped} duplicate reservationIds dropped"
              + (f", {excluded} not in Listing_contacts excluded)" if allow is not None
                 else ")"))
        print(f"  channels : {tidy['channel_family'].value_counts().to_dict()}")
        print(f"  months   : {tidy['month'].min()} -> {tidy['month'].max()}")
        print(f"  listings : {tidy['property'].nunique()} properties, "
              f"{tidy['nickname'].nunique()} nicknames")
        print(f"  wrote    : {db_path}")
    return tidy


def load_reviews() -> pd.DataFrame:
    """Read the tidy reviews table back from SQLite (used by dashboard/metrics)."""
    cfg = load_config()
    with sqlite3.connect(project_path(cfg["paths"]["db"])) as conn:
        df = pd.read_sql("SELECT * FROM reviews", conn)
    for col in ("created_dt", "check_in", "check_out"):
        df[col] = pd.to_datetime(df[col], errors="coerce")
    return df


if __name__ == "__main__":
    build()
