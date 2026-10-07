# CLAUDE.md — Weekly Revenue Updates

Replaces the manual weekly run of `Data and Reporting/RevenueReport.ipynb`
(Guesty UI export + 16 hand-downloaded Wheelhouse CSVs).

## Run

```bash
cd "/Users/ylin/ValtaWork/Claude_projects/Weekly Revenue Updates"
/Users/ylin/ValtaWork/.venv/bin/python run_weekly.py            # local output/<date>/
/Users/ylin/ValtaWork/.venv/bin/python run_weekly.py --publish  # + overwrite Drive copies
```

Steps: `fetch_guesty.py` → Wheelhouse `src.run_weekly --only report` (its own
venv, subprocess: set listings + set monthly metrics + market data — `--only
market` alone leaves the comp sets empty) → `build_report.py` → `build_artifact.py`.
The Wheelhouse pull takes ~3.5 min: calls run 6 in parallel at the ~55/min rate
limit (~180 calls), and only the current year of market data is fetched — earlier
months are copied from the previous snapshot (`market_incremental` in its config). `--skip-guesty` / `--skip-market` rebuild
from existing pulls. Paths are all in `paths.py`.

## Dashboard (Claude Artifact — the only dashboard)

The user shares the dashboard as a Claude Artifact; there is no local/Streamlit
dashboard (removed 2026-10-07).

Team settings (2026-10-07): the artifact declares `capabilities: {db: {}, user:
{scopes: ["profile"]}}` (a redeploy that omits `capabilities` keeps them). Shared db:
`settings/team` = {flags: {hpRed, hpAmber (pts below market HP), revRed, revAmber, rpRed,
rpAmber} in percent, hidden: [listing names], by: user id, at} and `notes/<listing key>` =
{listing, text, by, at} (key = name with unsafe chars as `~hex`). Default rules:
Contributor (`interact`) and up write, Viewers read only. Hidden listings drop out of
the Valta and Trends views (not Owner). Read them with ArtifactData if needed.

`build_artifact.py` (run by `run_weekly.py`) writes one self-contained page —
`artifact_template.html` + the run's data embedded as JSON — to
`output/<date>/revenue_dashboard.html` and the stable copy
`output/revenue_dashboard.html`. Layout is the user's design (str-dashboard-v2.jsx,
2026-10-07): burgundy header with Year / Quarter / Month / Market / Type filters and
Valta | Trends | Owner views. Valta and Trends also filter by Bedrooms (0/1/2/3/4+ buckets)
and Listing group (`build_report.listing_groups`; a listing can be in several): Elektra /
Yacinde by listing name; OSBR / Remote / Beachwood / Microsoft by the first word of
Property_Cohost `Set` (Remote also takes the Hawaii market); Seattle / Bellevue / Kirkland /
Redmond = every listing whose Property_Cohost `City` is that city. (No separate location
filter — removed 2026-10-07 at the user's request.) Valta = KPI tiles (vs last year, vs comp), flag legend
+ flagged list (rules in `FLAG_DEFAULT` / team settings: occupancy 10/5+ pts below
market high performers at the SAME market + bedroom bucket, compared month by month
over finished months (gap weighted by available nights; build_report drops the
all-sizes HP fallback); revenue -15/-10% vs last; RevPAR -10/-5% vs comp), property rows (This/Last/Comp bars);
Trends = monthly revenue 3 years, pacing for the next 3 months, markets table, top
movers; Owner = per owner statement (`Property`) with combined revenue + payout
(monthly `Payout`, else the yearly `payout` sheet). Clicking a listing opens a drawer
with monthly revenue and Wheelhouse-style ADR/occupancy rows. Comp = comp set, else
whole market at the bedroom size; ADR/RevPAR on the incl.-cleaning basis throughout.
Comp sets come from Wheelhouse dynamic sets (Wheelhouse listing name = Guesty
nickname; a listing can be in 2 sets). ADR basis: Wheelhouse market ADR is
`adr_w_fees` and comp-set `adr` also includes fees (checked 2026-10-07: set `adr`
tracks the set's daily `adr_w_fees`, no cleaning-sized gap), so the listing side uses
`ADR_w_fees` ((accommodation fare + cleaning) / nights), not `ADR` (nightly) or
`Revenue_occ` (Guesty EARNINGS, incl. cleaning, net of host fees).
Revenue on the page is `Revenue_month` (each booking split over its nights, so a stay
crossing a month end counts in both months); the Excel report's `Revenue` still puts
short-term bookings in the check-in month. Lead time = confirmation → check-in, by
check-in month (`LeadTime`, `Bookings`); comp lead time = Wheelhouse set `lead_time`.
Trends also stacks each 2026 month's guest payments into rent / lease rent /
cleaning / tax / channel fee / not itemized, with the host payout as a line
(`revenue_mix.py`, `Mix_*` columns). ALL fee rules come from Owner_statement_whole:
`fetch_guesty.py` saves each API booking's itemization with
its `build_summary_frame` to `data/guesty/Guesty_summary_2026-<date>.csv`, and
`revenue_mix.py` runs its `payment_model.compute_breakdown` (= its user-owned
`config/payment_structure.xlsx`) with its `config/_listing_tax_rates.csv`. Bookings on
deactivated listings (UI supplement) and pre-2026 check-ins are "not itemized". The previous
design is kept in `artifact_template_v1.html`. Published 2026-10-06 as https://claude.ai/artifact/Pcwts2dWwSjRyzhnDgbcqy
(private). Each week: run, then ask Claude to republish
`output/revenue_dashboard.html` (same path / `url`) so the link stays the same.

Comp-set maps (Owner tab, 2026-10-07): one street map per Wheelhouse comp set for each
of the owner's short-term listings — pink pins = active comps (click opens Airbnb, hover =
last-365-day ADR/occupancy/rating), grey dots = comps in review, burgundy house = this
listing, dark houses = other Valta listings. `comp_map.py` (called by build_artifact) frames
each set (5-95th percentile of comps + our listings), stitches OpenStreetMap tiles (cached in
`data/tiles/`; no contact info in the User-Agent) into `output/maps/s<set index>.jpg`. Comps =
wheelhouse `dynamic_set_members` (now pulled by `--only report`); our coordinates = Guesty
listing addresses, saved by `fetch_guesty.py` to `data/guesty/listing_locations.csv`.
The artifact can't load tiles itself (CSP), so **republish with the images**: `root` =
`output/`, `files` = every `maps/s*.jpg` mapped to itself.

## Inputs

- **Guesty** (`fetch_guesty.py`): confirmed reservations, check-in >= 2026-01-01,
  written in the Guesty UI export layout to `data/guesty/` so
  `DataProcessing.format_reservation` is unchanged. Uses Owner_statement_whole's
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
