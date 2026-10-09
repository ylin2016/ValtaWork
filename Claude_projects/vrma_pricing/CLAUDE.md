# CLAUDE.md — vrma_pricing (VRMA AI revenue analysis; folder renamed 2026-10-09)

Pricing-error scan for the Valta short-term-rental portfolio. For every unbooked
night in the next 180 days it builds a **band** (floor and ceiling), flags nights
priced outside it with chess severities, and turns the result into per-listing
action items (PDF, an xlsx tracker, and a shared web page with done-checkboxes).

The method came from a revenue-analyst brief written for PriceLabs exports. We
have none, so it runs on **Guesty** (bookings, live calendar) and **Wheelhouse**
(comp sets, neighborhood, market, listing settings) instead.

## Run order

Everything runs on the workspace venv.

```bash
cd /Users/ylin/ValtaWork/Claude_projects/vrma_pricing
PY=/Users/ylin/ValtaWork/.venv/bin/python

$PY   src/pull_guesty.py          # listings, confirmed reservations, 180-day calendar -> data/raw/
$PY   src/pull_wheelhouse.py      # neighborhood pricing/occupancy, price recs, market + comp-set daily (~10 min)
$PY   src/pull_wh_settings.py     # min price, base adjustment per listing
$PY   src/scan.py                 # band every unbooked night, whole portfolio -> data/scan_all.pkl (~1 min)
$PY   src/report.py config/table_notes.json   # 4-listing pricing-flags PDF + xlsx
$PY   src/min_price_report.py     # needs data/min_price_check.pkl (built inline in the session; see below)
$PY   src/listing_features.py     # one row per active listing
$PY   src/actions.py              # rule engine -> data/actions.pkl (prints every listing's actions)
$PY   src/report_actions.py       # output/listing_actions_<date>.pdf + .xlsx tracker
$PY   src/report_html.py          # output/listing_actions.html -> publish as the shared Artifact
```

`TODAY`/`HORIZON` are hard-coded (2026-10-06) in `pull_*.py`, `scan.py`, `report*.py`.
Bump them for a new run. `config/table_notes.json` holds the hand-written
"shared cause" lines under the 4-listing tables. **Rewrite it after every
re-run**, because it describes specific rows.

`data/min_price_check.pkl` was built by an inline script (calendar ⨝ WH recs ⨝
settings: `below = price < min_price`, `gap`, `days_out`, `match = |price-rec|<=3`).
Fold it into a script before the next run.

## Credentials and reuse

- Everything shared comes from `Claude_projects/shared/` (`valta_common`, installed in the
  workspace venv — see `../shared/README.md`):
  - Guesty: `valta_common.guesty.client` with the ONE cached token in `shared/secrets/`
    (5 tokens/24h shared across projects). Never force-refresh.
  - Wheelhouse: `valta_common.wheelhouse` client; key = `WHEELHOUSE_API_KEY` in
    `shared/secrets/.env`. The listing ↔ comp-set roster is read (read-only) from the
    shared `shared/data/wheelhouse.sqlite`, which weekly_revenue refreshes weekly.
- The Wheelhouse web app needs the user's login. The API exposes only each
  listing's **default** `min_price`, not date-range minimums.

## Method (scan.py)

- **Floor** = the matching night last year (same weekday −364d; calendar date for
  fixed holidays; Easter-aligned). A stay's `fareAccommodationAdjusted` is split
  across its nights by the market's daily ADR. **Ceiling** = floor × 1.30.
- **Ladder** when step 1 is missing: 2 same-weekday nights ±4 wks (no
  holidays/events) → 3 portfolio siblings (same comp set, else same market ±1BR),
  scaled by LY-rate ratio or WH-rec ratio → **3b** (`step == 3.5`) the comp set's
  median ADR around the LY date × the listing's position against its set →
  4 neighborhood low/high band → 5 skip. Steps 3, 3b and 4 are capped at INACCURACY.
  A step-3 flag ≥30% is lifted to BLUNDER when ≥2 siblings have step-1 BLUNDERs that
  date on the same side.
- **Market check (v2):** the comp set's booked ADR ±7d vs LY when it has ≥15%
  on the books, else its trailing 60-day YoY (clip 0.75–1.30). <0.90 lowers the
  floor; >1.10 lifts the ceiling. **Pacing:** neighborhood observed/expected
  bookings ±3d (≥20 expected). ≤0.75 lowers the floor 10%; ≥1.25 lifts the
  ceiling 10%. Total lift is capped at 1.35.
- Caps: the LY stay spanned a holiday but the target isn't one → max MISTAKE.
  Distressed = booked ≤3d out at <60% of siblings → not a floor.
- Severity: BLUNDER ≥30% outside, MISTAKE 20–30%, INACCURACY 10–20%.

## Gotchas learned the hard way

- Exclude **28+ night stays** (an 81-night $110/night stay set fake floors).
- **Guesty `base_price` is useless** as a size scaler (Elektra 1115 $85 vs 1004 $199,
  same building and size).
- **Small comp sets are lumpy daily** (one-off $514 nights, zeros on occupied
  nights). Never use a single day: take the median of ≥8 priced nights, widening
  to ±21d.
- Neighborhood `median/low/high_price` are comps' **asking** prices and run above
  what units book. Use them as the last rung only.
- Comp-set future ADR is mostly empty after ~2 months, so the 60-day trend covers
  far dates. It reflects late summer/fall, so re-run weekly.
- Guesty calendar price ≈ Wheelhouse recommendation on ~98% of nights. 39
  listings post below their own default minimum, on round numbers held for weeks,
  which points to hidden date-range minimums. That's unconfirmed until someone
  checks in the Wheelhouse UI.
- No price history anywhere, so the "was" column needs two snapshots to diff.
- Guesty's reservations API omits deactivated listings (e.g. Longbranch
  Upper/Lower).

## Outputs and the shared page

- Shared page: https://claude.ai/artifact/XK5aLiSvHBR7tw8kUpUbft (shared "anyone with
  the link"). Republish by publishing `output/listing_actions.html` from this
  session, or by passing the URL from another one. Capabilities: `db` + `user`
  (profile).
- Shared status lives in db collection **`status`**, one doc per action, keyed
  `sha1(listing|action text)[:16]`, with `{done, by (user id), at, listing, priority,
  action}`. A re-run that rewords an action gives it a new key, so it shows as open
  again. Read progress with `ArtifactData list status`.
- `data/v1/` keeps the first-run pickles for before/after comparisons.
- `data/` and `output/` are git-ignored (business data, generated).
- Never change prices or settings in Wheelhouse or Guesty from here. Everything is
  advice for the user to apply.
