# shared — what several Claude_projects use

One place for the code, reference tables, secrets and API data that more than one
project needs. **Every item has exactly one writer**; everyone else only reads it.
Nothing here imports from a project, and no project reaches into another project for
these things any more (2026-10-09).

```
shared/
  valta_common/   shared Python code (pip install -e shared)
  reference/      hand-maintained tables
  secrets/        ONE copy of every credential and token cache (git-ignored)
  data/           the API databases (git-ignored)
```

## Registry

| Item | Writer (owner) | Readers |
|---|---|---|
| `valta_common.guesty` — client, `reservation_financials`, `reservations_batch`, `summary.build_summary_frame` | edited here | Owner_statement_whole, weekly_revenue, qbo_reservation_bookkeeping, yacinde_expense, vrma_pricing |
| `valta_common.fees.payment_model` — the fee rules (= the user-owned `Owner_statement_whole/config/payment_structure.xlsx`) | edited here; Owner_statement_whole's rules | Owner_statement_whole, weekly_revenue, qbo_reservation_bookkeeping |
| `valta_common.wheelhouse` — Wheelhouse API client (+ `config.yml` API settings) | edited here | weekly_revenue (downloader), vrma_pricing |
| `valta_common.paths` — every location below | edited here | all Python projects above |
| `reference/Listing_contacts.csv` — listing → statement, term, supplies, PM %, owner | Owner_statement_whole (hand-edited) | Owner_statement_whole, qbo_reservation_bookkeeping |
| `reference/mapping_classes.yml` — property_id → QBO class → owner | Owner_statement_whole (`sync-mappings`, hand edits) | Owner_statement_whole, QBO_operations, qbo_reservation_bookkeeping, expense_form (`sync_properties`) |
| `reference/listing_tax_rates.csv` — per-listing tax rates for the fee model | Owner_statement_whole | Owner_statement_whole, weekly_revenue, qbo_reservation_bookkeeping |
| `secrets/.env` — QBO, Guesty and Wheelhouse credentials | you | every project that calls an API |
| `secrets/qbo_tokens.json` — the ONE QuickBooks OAuth token | QBO_operations runs the OAuth flow; any client refreshes in place | QBO_operations, Owner_statement_whole, qbo_reservation_bookkeeping |
| `secrets/guesty_token.json` — the ONE cached Guesty token | whichever client needs a new one (≈1/day) | every Guesty caller incl. GuestyAccess |
| `data/guesty.sqlite` | weekly_revenue (`fetch_guesty.py` → `guesty_store.py`), weekly | anyone (read-only) |
| `data/wheelhouse.sqlite` | weekly_revenue (`wheelhouse/` downloader), weekly | weekly_revenue, vrma_pricing (roster) |

**Shared, but stays with its owning project** (read in place through that project's
bridge; they change at month close and belong with that pipeline):
Owner_statement_whole's `db/owner_statement.sqlite` (read-only by QBO_operations and
qbo_reservation_bookkeeping), its `inputs/<period>/` Guesty exports (QBO_operations),
and its `scope/` and `ltr/labels` code (bookkeeping, QBO_operations). QBO_operations'
`accounts.yml` / Resolver are read by qbo_reservation_bookkeeping. The two QuickBooks
*clients* also stay in their projects on purpose: Owner_statement_whole's is
read-only, QBO_operations' writes — only their token and credentials are shared.

Not shared: QBO_subsidiary_reader keeps its own token (a different QuickBooks
company), and anything archived in `_archive/` is never read.

## Setup (new machine or colleague)

```bash
cd Claude_projects
/Users/ylin/ValtaWork/.venv/bin/pip install -e shared --no-deps   # and any project venv:
Owner_statement_whole/venv/bin/pip install -e shared --no-deps
cp shared/secrets/.env.example shared/secrets/.env                  # then fill it in
```

`--no-deps` leaves the venv's own pandas/requests alone. Installed so far: the
workspace `.venv` (weekly_revenue, vrma_pricing, yacinde_expense) and
`Owner_statement_whole/venv` (which also runs qbo_reservation_bookkeeping).
QBO_operations, expense_form and GuestyAccess (its own Guesty client) don't import
`valta_common`: they point at `shared/` directly (they only need a token, the `.env` or
the class map), and Owner_statement_whole's
`paths.py` does the same so its hosted `deploy/` copy keeps working without `shared/`.

Guesty issues at most **5 tokens per 24h** for the whole app. With one cache the
workspace uses ≈1 a day — never delete `guesty_token.json` or force a refresh.

## Reading the databases

Open them read-only so a reader can never change or lock a file during the weekly write:

```python
import sqlite3
from valta_common.paths import GUESTY_DB, read_only
con = sqlite3.connect(read_only(GUESTY_DB), uri=True)
```

```r
con <- DBI::dbConnect(RSQLite::SQLite(), ".../Claude_projects/shared/data/guesty.sqlite",
                      flags = RSQLite::SQLITE_RO)
```

Check freshness before relying on it: `SELECT * FROM pulls ORDER BY pulled_at DESC`
(Guesty) or `SELECT max(snapshot_date) FROM market_monthly` (Wheelhouse).

### guesty.sqlite

- `listings` — every Guesty listing, active or not: `listing_id`, `nickname`, `title`,
  `active`, `listed`, `bedrooms`, `bathrooms`, `accommodates`, `property_type`,
  `room_type`, address (`address_full`, `city`, `state`, `zipcode`, `lat`, `lng`),
  `tags` (JSON), `raw_json`. Replaced on every pull.
- `reservations` — **confirmed** reservations with local check-in on/after 2026-01-01:
  `confirmation_code` (key), `reservation_id`, `listing_id`, `listing_nickname`,
  `status`, `source`, `platform`, `check_in`/`check_out` (local dates), `nights`,
  `guests`, `confirmed_at`, `created_at`, `currency`, `host_payout`, `total_paid`,
  `total_refunded`, `fare_accommodation`, `fare_cleaning`, `guest_name`, and the full
  API record in `raw_json` (money, invoice items, payments). Each pull replaces this
  whole scope, so a booking cancelled since the last pull is gone.
- `reservation_fees` — one row per reservation in the fee-category layout
  `fees.payment_model` reads (`build_summary_frame`).
- `pulls` — when each table was refreshed, its scope and row count.

Not in here: canceled/inquiry reservations, check-ins before 2026, bookings on
**deactivated** listings (the Open API hides them; weekly_revenue adds those from a
Guesty UI export for its own report only), calendars/prices. Projects that need those,
or need data fresher than weekly (e.g. the owner-statement month close), still pull the
API themselves — through `valta_common.guesty`, on the shared token.

### wheelhouse.sqlite

Same schema as the Wheelhouse downloader (`weekly_revenue/wheelhouse/schema.sql`),
keyed by `snapshot_date` (the weekly run date). The weekly run fills:
`markets`, `market_monthly` (`metric` = adr_w_fees / occupancy_adjusted / revpar_w_fees,
by `performance` '' (all) or 'high' × `bedrooms` '' (all) / 0-3 / 4+), `dynamic_sets`,
`dynamic_set_associated_listings` (our listing ↔ comp set), `listings` (our
Wheelhouse listings), `dynamic_set_aggregated_metrics` (monthly comp-set adr /
occupancy_adjusted / revpar / lead_time), `dynamic_set_members` (the comp listings,
with `raw_json`). Weekly snapshots accumulate (the report-mode pull doesn't prune), so
always filter on the snapshot you want, usually `max(snapshot_date)`.

## Changing something here

- **Fee rules** (`payment_model.py`): the statements, the dashboard and the QBO
  bookkeeping all change at once. Follow Owner_statement_whole's CLAUDE.md (audit the
  stored `payment_breakdown_<p>.csv` files after any change).
- **A table's columns or scope** in `guesty.sqlite`: change `weekly_revenue/guesty_store.py`
  and update this README in the same edit — every reader sees it.
- **Moving the folder**: set `VALTA_SHARED=/new/path` for `valta_common`; the three
  dependency-free `paths.py` files (Owner_statement_whole, QBO_operations, expense_form)
  assume `Claude_projects/shared/`.
