# Wheelhouse Market Data & Dynamic Set Downloader

Pulls listing-performance data from the [Wheelhouse Revenue Management API](https://api.usewheelhouse.com/wheelhouse_rm_api)
into a snapshot-tagged SQLite database and exports each pull to CSV + Excel.

Two datasets:

1. **Market data** (refresh weekly) — market reports (daily time series + monthly
   distributions). By default it pulls the markets **where your listings are**
   (`params.markets_from_listings: true`), auto-updating as your portfolio changes.
   Pin a fixed list instead with `params.market_names` / `market_ids`.
2. **Dynamic sets** ("dynamite sets") — Wheelhouse's comparable-property groupings:
   which of our listings map to each set, the comp members, and each set's
   aggregated metrics, time series, distributions, and membership changelog.

## Setup

```bash
pip install -r requirements.txt
cp .env.example .env
# paste your Wheelhouse RM API key into .env  (WHEELHOUSE_API_KEY=...)
```

Get an RM key in your Wheelhouse profile → **Api Key → Revenue Management API Keys →
Create RM Key** (shown once — copy it immediately). The RM API is in free beta and
requires request-access approval first.

## Usage

```bash
# Full weekly pull, tagged with today's date, then export CSV + xlsx
python -m src.run_weekly

# Run one part only
python -m src.run_weekly --only market          # market reports for configured cities
python -m src.run_weekly --only dynamic_sets     # portfolio comp sets
python -m src.run_weekly --only listings --limit 3   # /listings demo (auth smoke test)

# Other flags
python -m src.run_weekly --snapshot-date 2026-07-20   # backfill/label a snapshot
python -m src.run_weekly --no-export                   # DB only, skip files
```

Outputs:
- SQLite DB: `data/wheelhouse.sqlite`
- Exports: `data/exports/<snapshot_date>/*.csv` and `wheelhouse_<snapshot_date>.xlsx`

Re-running the same `--snapshot-date` is **idempotent** (every row's primary key
includes the snapshot date, so it overwrites rather than duplicates).

## How it works

- `src/wheelhouse_client.py` — HTTP client: `X-Integration-Api-Key` auth, throttling
  under the ~60 req/min limit, 429/5xx retry with backoff, pagination. Logs every raw
  response into the `raw_responses` table.
- `src/download_*.py` — one module per dataset; tolerant normalizers (`src/transforms.py`)
  flatten metrics into tidy `(date, metric, value)` / `(month, metric, bucket, value)`
  rows and always keep the full payload in a `raw_json` column.
- `src/run_weekly.py` — orchestrates market → dynamic sets → export. Market reports
  cover the cities in `params.market_names`; dynamic sets cover the whole portfolio.
- `config.yml` — base URL, rate limit, request params, and **all endpoint path
  templates**. Paths are aligned to the Wheelhouse RM API help docs (Jul 2026); if a
  live call 404s, fix the path here only.

## Endpoints & required params (from the RM API help docs)

| Data | Endpoint | Required query params |
|------|----------|-----------------------|
| Markets | `GET /market_report` | `country_code` |
| Market time series | `GET /market_report/{id}/time_series` | `start_date`, `end_date` |
| Market distribution | `GET /market_report/{id}/distribution` | `month` (first of month) |
| Dynamic sets | `GET /sets`, `GET /sets/{id}/...` | (dates/month optional) |

Metric keys: `asking_rate_w_fees, occupancy, occupancy_adjusted, adr_w_fees, lead_time,
revpar_adjusted_w_fees, revpar_w_fees, revenue_w_fees, nights_bookable`. The `params:`
block in `config.yml` controls the country codes, time-series window (`history_days` /
`forward_days`), how many months of distributions to pull, and an optional explicit
`market_ids` override.

Field names still aren't 100% pinned in the docs, so the first authenticated run also
serves as discovery: every table keeps a `raw_json` column and every call is logged to
`raw_responses`, so nothing is lost even before a field is normalized into its own column.

## Related: Wheelhouse MCP (interactive, not for this job)

Wheelhouse also offers an [MCP server](https://help.usewheelhouse.com/en/articles/15809193-how-do-i-connect-an-ai-assistant-to-wheelhouse-via-mcp)
(58 tools, enable via Connections → API Key → "Enable MCP Access") to connect Claude/Cursor
for ad-hoc questions like "how am I pacing vs. neighborhood comps?". That's for interactive
use; this project is the automated, scheduled bulk-download path.

## Scheduling (later)

Once the pull is verified against the live API, wire `python -m src.run_weekly` into a
weekly schedule (e.g. every Monday morning).
