# CLAUDE.md — Weekly Revenue Updates

Replaces the manual weekly run of `Data and Reporting/RevenueReport.ipynb`
(Guesty UI export + 16 hand-downloaded Wheelhouse CSVs).

## Run

```bash
cd "/Users/ylin/ValtaWork/Claude_projects/Weekly Revenue Updates"
/Users/ylin/ValtaWork/.venv/bin/python run_weekly.py            # local output/<date>/
/Users/ylin/ValtaWork/.venv/bin/python run_weekly.py --publish  # + overwrite Drive copies
```

Steps: `fetch_guesty.py` → Wheelhouse `src.run_weekly --only market` (its own
venv, subprocess) → `build_report.py`. `--skip-guesty` / `--skip-market` rebuild
from existing pulls. Paths are all in `paths.py`.

## Dashboard

```bash
/Users/ylin/ValtaWork/.venv/bin/streamlit run dashboard.py   # http://localhost:8501
```

Reads `output/<date>/dash_listing_monthly.csv`, `dash_benchmarks.csv` (written
by `build_report.py`) and the `yearly` sheet of that run's report. Tabs:
Company (revenue by month 2024–26, YTD by market), Listing (revenue by month; Wheelhouse-style ADR + Occupancy cards for
this listing / comp set / market at its bedroom size — per month, bar = selected
year, tick = same month last year, badge = headline month YoY; occupancy track
fixed 0–100%), Portfolio (YTD per listing vs benchmarks).
Comp sets come from Wheelhouse dynamic sets (Wheelhouse listing name = Guesty
nickname; a listing can be in 2 sets). ADR caveat: market ADR is
`adr_w_fees` (incl. fees), set ADR is nightly — hence the "nightly rate +
cleaning" toggle. Neutral gray theme in `.streamlit/config.toml`. After every
rebuild, restart Streamlit.

## Inputs

- **Guesty** (`fetch_guesty.py`): confirmed reservations, check-in >= 2026-01-01,
  written in the Guesty UI export layout to `data/guesty/` so
  `DataProcessing.format_reservation` is unchanged. Reuses GuestyFinancials'
  client + cached token (5 tokens/24h — never force-refresh).
  - Deactivated listings are excluded by the Open API. They are **filled from a
    Guesty UI export** dropped in `data/guesty/Guesty_UI_bookings_*.csv` (newest
    used): only listings the API returned NOTHING for are added (so a stale export
    can't resurrect a cancellation); added rows logged to
    `data/guesty/ui_supplement_<date>.csv`. Re-apply without an API call:
    `fetch_guesty.py --asof <date> --supplement-only` (idempotent). 2026-09-29: +222
    rows on 15 listings (Seattle 4201, Bellevue 1420/2243/242/701, Seattle 1424C, …).
    Refresh the UI export when listings are deactivated.
  - Dates use `checkInDateLocalized` (UTC `checkIn` shifts 4pm PT to next day).
  - `ACCOMMODATION FARE` = `fareAccommodation` + MAR + EPF + AFWD items
    (excludes LOSD/GCD/AFD discounts and AFA). Pet fee = invoice items titled
    "Pet". Verified vs the 2026-09-22 UI export: only altered bookings differ.
- **Wheelhouse**: `occupancy_adjusted` from `wheelhouse.sqlite` `market_monthly`.
  Each listing is matched on **Market + bedroom bucket** (0/1/2/3/4+, from
  Property_Cohost `BEDROOMS`): `Occ_All` = whole market at that size, `Occ_HP` =
  high performers at that size; falls back to the market's all-bedroom figure
  (`*_src` column says which). Market map: Hawaii→Big Island, ONP→Olympic
  National Park.
- **Rent roll** (`data/LTR/Rent roll.xlsx`, one row per listing × month, "Received
  rent"; sheets 2025 + 2026) is the **source of truth for lease income**
  (`build_report.add_rent_roll`, user decision 2026-09-29). Lease (Term LTR) rows
  are first split into calendar-month pieces; then for each listing-month with rent:
  STR bookings that month → rent roll skipped (the Guesty stay wins); LTR rows →
  replaced by the rent-roll amount; nothing → added. Months without rent keep the
  booking data. Each rent-roll month = one full-month LTR stay (code
  `RR-<listing>-<yyyy-mm>`). Per-month actions are written to
  `output/<date>/rent_roll_applied.csv`. Names aliased in `RENT_ROLL_ALIASES`
  (4-Plex N → Bellevue 14507UN, "Microsoft D303", "Seattle 906 upper"); a rent typed
  as a date is skipped with a warning. 2026-09-29: 222 replaced ($769,345 → $729,679),
  90 added ($335,446), 14 skipped. Fixed Beachwood 3/7 double leases and Mercer 2449
  half rent. Still >100% occ (outside the rent roll): Beachwood 3 Oct-26,
  Bellevue 16237 Jan-26 (1 night), Elektra 809 Dec-25.
- Still manual/static: pre-2026 bookings, `LRT_bookings.xlsx`, owner payout
  workbooks, `Property_Cohost.xlsx`, `Property_OverallRatings.xlsx`,
  `GuestyCanceled.csv`, reviews (newest `* guesty_reviews.xlsx` auto-picked).

## Differences from the notebook (deliberate)

- `Revenue_fiscal_2026` = Dec 2025–Nov 2026. The notebook also added Dec 2026
  (13 months) because it only shifted 2023–2025 Decembers.
- Onboarding denominators: `ONBOARD` dict reproduces the notebook's hard-coded
  day counts; any other listing whose first stay is this year is detected
  automatically (printed as "onboarding (auto …)").
- Owner-payout month clamps at 0 (notebook went negative in Jan/Feb).

Validated 2026-09-29: on the 09-22 manual inputs every sheet matches the
notebook's 09-22 report except the two items above.

## Not yet year-proof

`YEARS`, `build_owner_payout_26`, the 2025/2026 Guesty files in `import_data`
and `YEARS_COLS` are 2024–2026. Rolling to 2027 needs a pass in January.
