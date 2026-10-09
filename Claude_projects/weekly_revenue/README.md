# weekly_revenue — Weekly Revenue Updates

Weekly revenue report + the shared dashboard (a Claude Artifact). Pulls Guesty
reservations and Wheelhouse market/comp-set data, builds the Excel report in
`output/<date>/` and the dashboard page `output/revenue_dashboard.html`.

You need this folder **and `../shared/`** (code, secrets and databases several
projects share — see `../shared/README.md`). Details for Claude (and humans) are in
`CLAUDE.md`.

## Setup

1. Python 3.11+ and the packages (this also installs `../shared` as `valta_common`):
   ```bash
   python3 -m venv .venv
   .venv/bin/pip install -r requirements.txt
   ```
2. **Secrets** — copy `../shared/secrets/.env.example` to `../shared/secrets/.env` and
   fill in the Guesty Open API client id/secret and the Wheelhouse RM API key (ask the
   project owner; never commit `secrets/`). Guesty issues at most **5 tokens per 24h**
   for the whole account and every project shares the cached
   `../shared/secrets/guesty_token.json` — don't delete it or force a refresh.
3. **Data** — not in git (guest bookings, owner payouts, Wheelhouse history). Get copies
   from the project owner:
   - `data/inputs/` — pre-2026 bookings, LTR list, owner payout workbooks, ratings,
     reviews, `Property_Cohost.xlsx` (refreshed from Google Drive by `sync_inputs.py`
     when Drive is mounted; otherwise the copies are used as-is)
   - `data/LTR/Rent roll.xlsx` — lease income
   - `data/guesty/` — Guesty pulls + the UI export for deactivated listings
   - `data/tiles/` — map tile cache (optional; rebuilt on demand)
   - `../shared/data/wheelhouse.sqlite` — Wheelhouse history (the weekly pull only
     fetches the current year and copies earlier months from it). `guesty.sqlite` is
     rebuilt by the first run. `VALTA_SHARED=/path` points at a shared folder elsewhere.

## Run

```bash
.venv/bin/python run_weekly.py                        # full weekly run, as of today
.venv/bin/python run_weekly.py --skip-guesty --skip-market   # rebuild from existing pulls
.venv/bin/python run_weekly.py --publish              # also write the Drive copies
```

If Google Drive for desktop is mounted somewhere other than
`~/Google Drive/My Drive`, set `VALTA_DRIVE=/path/to/My Drive`.

Then republish `output/revenue_dashboard.html` (with the `output/maps/*.jpg` images)
to the dashboard artifact — see "Dashboard" in `CLAUDE.md`. You need edit access to
the artifact to update the shared link.
