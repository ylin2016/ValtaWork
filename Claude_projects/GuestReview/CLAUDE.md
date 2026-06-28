# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A small analytics MVP over short-term-rental guest reviews exported from Guesty.
It answers: how many reviews per listing per month, average scores per
listing/month (normalized across booking channels), a running cumulative Airbnb
rating, and per-listing / company insights — surfaced in a Streamlit dashboard.

## Commands

Use the shared workspace virtualenv (it has pandas/streamlit/altair already):

```bash
source /Users/ylin/ValtaWork/.venv/bin/activate

python src/load_reviews.py        # (re)build data/reviews.sqlite from the xlsx
streamlit run src/dashboard.py    # launch the dashboard (reads the sqlite)

# each pipeline module has a __main__ smoke test:
cd src && python metrics.py       # aggregations
cd src && python cumulative.py    # cumulative + join coverage report
cd src && python insights.py      # category/trend/theme insights
```

Headless dashboard test (no browser), used to verify the app boots end-to-end:

```bash
cd src && python -c "from streamlit.testing.v1 import AppTest; \
at=AppTest.from_file('dashboard.py',default_timeout=60).run(); \
assert not at.exception, at.exception; print('ok')"
```

`src/` modules import each other by bare name (`from metrics import ...`), so run
them with `src/` as the working directory (the `streamlit run src/...` and
`cd src` forms above both satisfy this).

## Data inputs (`data/`, git-ignored, not committed)

- `*guesty_reviews.xlsx` — one row per review. ~4.8k rows, 6 channels, 2022→now.
- `Property_OverallRatings.xlsx` — **snapshot dated 2025-09-28** of each Airbnb
  listing's lifetime `Overall Airbnb` average + review count. The anchor for the
  cumulative metric. Also has VRBO/Booking columns (unused; Airbnb-only for now).

Paths, the channel scale map, snapshot date, and insight thresholds all live in
`config.yml` — change data there, not in code.

## Architecture & data flow

`config.yml` → `normalize.py` (shared helpers) → `load_reviews.py` builds
`data/reviews.sqlite` (`reviews` table, one tidy row per review). Everything
downstream reads that table:

- `metrics.py` — counts, monthly averages, pivots, listing rankings.
- `cumulative.py` — reconstructs the running Airbnb lifetime rating.
- `insights.py` — best/worst category, MoM trend, low-score reviews + theme tags.
- `dashboard.py` — Streamlit UI: per-listing detail vs. company portfolio view.

## Three non-obvious things that drove the design

1. **Scores are on different scales per channel.** Airbnb / Vrbo / Expedia report
   1–5; Booking.com reports 1–10 (sub-scores 2.5/5/7.5/10). `normalize.py` divides
   by the per-channel `divisor` in `config.yml` so every `*_5` column is a true
   0–5 value. Never compare raw score columns across channels — use the `_5` ones.

2. **`Property` is mostly empty; `nickname` is the listing key.** In the reviews
   file `Property` is filled on only ~589 Airbnb rows (a partial hand alias) and
   is null for ALL Booking/Vrbo/Expedia rows. So listings are keyed on
   `listing_key` = canonicalized `nickname` (trim/collapse-ws/casefold, see
   `canon_listing`). `listing` is a readable display label; `property_alias` is
   the curated `Property` name shown when present. Grouping by `Property` would
   silently drop ~88% of reviews — don't.

3. **Cumulative (lifetime) rating uses two methods, by channel.**
   - **Airbnb** can't be recomputed from the reviews alone (it's a lifetime
     average over a count we don't fully have), so we anchor on the 2025-09-28
     snapshot (`baseline_sum = avg×count`, `baseline_count = count`) and add only
     reviews with `createdAt` strictly AFTER the snapshot. The snapshot's
     `Listing` joins to `nickname` (canonicalized), NOT `Property`.
   - **VRBO / Booking.com / Expedia** have no snapshot — they accumulate the
     review file FROM THE BEGINNING (earliest review forward).
   Both store sum+count (not just the average) so channels roll up to an
   "all" combined line and to company level as count-weighted means. The display
   window starts at `summary_start` (Oct 2025); non-Airbnb totals still include
   earlier history, those months just aren't plotted. The cumulative metric is
   computed on the FULL history and ignores the dashboard's date/channel filters.
   See `cumulative.cumulative_by_listing` (long frame, one row per
   listing×channel×month, plus `channel_family='all'`).

Join gaps (listings in the snapshot with no reviews, or new Airbnb listings with
no baseline) are reported via `cumulative.coverage_report()` and shown in the
dashboard's "Data coverage" tab — surfaced, never silently dropped.

## Conventions

- Rebuilding is idempotent: `load_reviews.py` does `to_sql(..., if_exists="replace")`.
  Re-run it whenever the xlsx or `config.yml` changes.
- Score column naming: `<category>_raw` (as reported) and `<category>_5` (0–5).
  Categories are defined once in `config.yml` (`categories`).
