# CLAUDE.md — Owner_statement_whole

Guidance for Claude Code working in this project.

## What this is

The **unified owner-statement pipeline** for Valta Realty vacation rentals. It is the
successor to `../owner_statement_mvp` (the SQLite + QBO statement engine) with its Guesty
data-ingestion switched from a **manual UI CSV export** to an **automated Guesty Open API
pull** (the fetch/breakdown code comes from `../GuestyFinancials`).

Output: per-owner Excel + PDF statements and a Streamlit dashboard.

Most business logic is inherited wholesale from `owner_statement_mvp` — read
`../owner_statement_mvp/CLAUDE.md` for the deep rules (net-revenue channel math, rollups,
PM-rate single-source, `owner_pays_cleaning/taxes`, central supplies, OSBR-RV, refund
offsets, penny reconciliation, LTR/deferred import). This file documents what is **new or
different here**.

## Folder layout (config vs data separated — owner requirement)

```
config/                       # human-maintained, rarely changes
  mapping_classes.yml, mapping_accounts.yml, Listing_contacts.csv
  payment_structure.xlsx      # SOURCE OF TRUTH for the Section-1 fee model (see below)
  config.yml, _listing_tax_rates.csv
  secrets/                    # git-ignored: .env (QBO+Guesty creds), qbo_tokens.json, guesty_token.json
inputs/<period>/              # per-month inputs (YYYY-MM)
  Guesty_booking_<period>.csv       # Task 1 output A (UI-export shaped)
  payment_breakdown_<period>.csv    # Task 1 output B (Section-1 fee waterfall)
  guesty_converted.csv              # convert_guesty_export.py output
  LTR_<period>.csv                  # LTR/deferred input
output/<period>/statements/   # generated Excel + PDF
db/owner_statement.sqlite     # SINGLE shared ledger — NOT per-month (balances roll forward)
src/                          # code, organized BY FUNCTION (see below)
```

### src/ layout (organized by function)

```
src/
  paths.py            # ANCHOR — layout single-source; stays at src root (parent.parent = project root)
  run_month_close.py  # WORKFLOW CONDUCTOR — the pipeline CLI (python -m src.run_month_close <cmd>)
  dashboard.py        # Streamlit app (run as a flat script; stays at src root — see note)
  deploy_pack.py      # PACKAGE FOR DEPLOY — re-packs deploy/ from the latest build
  common/     # shared infra:       db, config, utils, mappings
  scope/      # STATEMENT SCOPE:    listing_filter (which listings get statements), pm_rate (commission rate)
  guesty/     # ACCESS GUESTY API:  client, config, reservation_financials, reservations_batch
  breakdown/  # BOOKINGS BREAKDOWN: payment_model, fetch_month, deactivated, convert_export, adapter
  ltr/        # LTR BOOKING SOURCE: import_ltr (pull LTR/deferred rent → ledger), records (display)
  netrevenue/ # BUILD NET REVENUE:  engine (was statement_engine)
  expense/    # EXPORT EXPENSE:     qbo_client, qbo_sync, booking_fees
  reporting/  # BUILD DASHBOARD:    excel_writer, export_end_balances
  onetime/    # one-time & maintenance scripts (quarantined; see src/onetime/README.md)
```

**Booking sources** are parallel: `guesty/` (Open-API pull, → `breakdown/`) and `ltr/` (LTR CSV
→ ledger via `import_ltr`; `ltr/records.py` reads them back for display). **`scope/` sets up the
statement scope** — `listing_filter` decides which listings get a statement, `pm_rate` the
commission rate — resolved up front by `build`, dashboard, and PDF alike.

**All paths are centralized in `src/paths.py`** — the one place that knows the layout. Never
hardcode a path; add/resolve it through `paths`. `paths.py` **must stay at `src/` root**: its
`parent.parent` resolves to the project root (and to `deploy/` in the packaged copy). The
dashboard runs as a **flat Streamlit script** (`src/` on `sys.path`), so it and the deploy bundle
import the display code by absolute name — `import paths`, `from scope.listing_filter import …`,
`from ltr.records import …`. `scope/listing_filter` and `ltr/records` read `paths` through a
`try: from ..paths … except ImportError: from paths …` shim so they work both as package modules
(`python -m src.…`) and as flat imports under the running script. The deploy bundle therefore
ships `paths.py` (flat) + the `scope/` and `ltr/`-display packages (`ltr/import_ltr.py`, the
importer, is workflow-only and excluded).

## Three tasks / data flow

```
Guesty Open API ─▶ src/breakdown/fetch_month.py ─▶ inputs/<p>/Guesty_booking_<p>.csv (A)
                                                 └▶ inputs/<p>/payment_breakdown_<p>.csv (B)
(A) ─▶ breakdown/convert_export.py ─▶ guesty_converted.csv ─▶ guesty-import ─▶ ledger (source=guesty)
QuickBooks Online ─▶ qbo-sync ─▶ ledger (source=qbo)
LTR CSV ─▶ src/ltr/import_ltr.py ─▶ ledger (source=qbo, source_object=LTR/DEFERRED)
ledger + (B) ─▶ build ─▶ statement_property_totals ─▶ dashboard / Excel / PDF
```

### Task 1 — Guesty API ingestion (`src/guesty/` + `src/breakdown/`)

**`src/guesty/`** (access the API) — copied from GuestyFinancials + rewired to `paths`:
`client.py` (was `guesty_client.py`), `config.py` (endpoints are constants; token cached to
`config/secrets/guesty_token.json`), `reservation_financials.py`, `reservations_batch.py`.
**`src/breakdown/`** (bookings breakdown) holds the fee-waterfall + ingestion adapters:

- **`payment_model.py`** — `compute_breakdown(summary_df, tax_rates_csv)`; a faithful,
  importable transcription of `payment_structure.xlsx` (was GuestyFinancials'
  `payment_breakdown.py`). Produces the per-booking fee waterfall.
- **`fetch_month.py`** — orchestrator. ONE API pull → two CSVs:
  - **Output A** `to_ui_csv()`: UI-export-shaped so `convert_guesty_export.py` consumes it
    unchanged. `TOTAL TAXES` = Σ itemized `tax_*` (NOT `money.totalTaxes`, which undercounts);
    `TOTAL PAYOUT` = `host_payout` as-is. Tax columns are `fillna(0)` before summing (NaN
    poisons the sum).
  - **Output B** `payment_breakdown_<p>.csv`: Section-1 data, keyed by `property_id`.
- **`deactivated.py`** — the Open API silently drops **deactivated** (`active=false`) listings.
  `--deactivated-csv <ui_export>` folds them in (add-only-missing by confirmation code).

### Task 2 — QBO expenses → ledger (`src/expense/`)

`qbo_sync.py` / `booking_fees.py` / `qbo_client.py`, ~unchanged from mvp.

### Task 3 — dashboard / Excel / PDF (`src/reporting/` + root `dashboard.py`)

`dashboard.py` (+ `generate_pdf` inside it) stays at `src/` root (flat Streamlit script);
`reporting/excel_writer.py` and `reporting/export_end_balances.py` are the other outputs.
See dashboard notes below. Net-revenue math lives in `netrevenue/engine.py` (was
`statement_engine.py`); property scope + commission rate come from `scope/`
(`listing_filter`, `pm_rate`); `run_month_close.py` (root) conducts all tasks.

## STR commission basis = payment-breakdown net (owner decision, whole-pipeline)

STR (guesty) bookings are commissioned on the **payment_breakdown net** (the sheet-faithful
Guesty-basis net), not the convert+QBO-fee net. `run_month_close._apply_payment_breakdown_net`
runs in `build` (AFTER `_apply_guesty_fees`, BEFORE `build_statements`) and **overrides each
guesty INCOME ledger `amount`** with `payment_breakdown_<p>.csv` net_revenue, matched by
confirmation code (`source_txn_id`). Because `statement_engine`, dashboard, Excel, and PDF all
read the ledger `amount`, this one override propagates to every product — stored `amount_due`,
Summary Net Income, and the Net Revenue Section's Net Rental Revenue + commission all use it and
foot. Bookings absent from the breakdown (deactivated / $0 / dropped) **keep the DB net as a
fallback**. Idempotent (overwrites `amount` only; `base_amount` stays immutable; re-derived from
the CSV each build — `build` twice gives identical totals).

## payment_structure.xlsx is the SOURCE OF TRUTH for the fee model

`config/payment_structure.xlsx` (one row per channel, formula cells) drives every number in
`payment_model.py`. **When the owner says they edited the sheet, re-read it and update
`payment_model.py` to match** — never invent or leave a stale rate. Current transcription:

- **Channel fee** = `(InvoiceItem − tax) × rate`: Airbnb 15.5, HomeAway 5, Expedia 18,
  Booking 15, Hopper 14, Marriott 18.5, Trip.com 11.1, Whimstay 5, Blueground 7%. Manual/bnbFinder none.
- **Guesty fee** = `InvoiceItem × 1%` (HomeAway/Expedia/Manual/bnbFinder only).
- **Stripe fee** = `InvoiceItem × 2.9% + (successful transactions) × $0.30` (host/Guesty-collected
  only). `transactions` = count of `money.payments` with `status=='SUCCEEDED'` (deposit + balance
  each incur their own $0.30). Threaded from the API as `n_transactions` in the summary frame
  (`fetch_month.build_summary_frame`); rows lacking payment data (deactivated UI merge) fall back
  to 1. (Owner updated 2026-08 from the old tiered `200×3.05% + (II−200)×2.9% + $0.30` — the
  first-$200 3.05% premium and the single flat $0.30 both went away.)
- **Net revenue** subtracts the channel fee for **HomeAway, Booking, Expedia-group, Trip.com**
  (`cf_in_net`) — their Invoice Amount *includes* the channel fee and the host pays it back.
  Owner confirmed the sheet's Expedia-group "hostServiceFee" term MEANS this 18% channel fee
  (do NOT swap in the API's `money.hostServiceFee`).

## Booking Breakdown (Section 1) is shared across dashboard, Excel, and PDF

The per-unit Booking Breakdown table is built ONCE in `reporting/booking_breakdown.py`
(`build_by_unit`, dependency-free/data-only) and rendered by all three products, so they
never drift: the dashboard (`st.table`), the Excel writer (`excel_writer._write_breakdown`),
and the PDF (`dashboard.generate_pdf._bd_table`). `run_month_close` passes the tables into
`write_statement`; the dashboard passes them into `generate_pdf`. It slots directly above
each unit's Net Revenue table (interleaved for rollups), with a "Booking Breakdown — Total
(All Units)" grand row for multi-unit statements. The builder takes the deduped Guesty
payment-breakdown + LTR/deferred records; `ltr_row` accepts BOTH the raw `ltr.records` shape
(Excel build) and the dashboard's display shape (`_pick`). The deploy bundle ships the module
(display-only; `excel_writer` is pipeline-only and excluded).

## Dashboard layout

- Section-1 "Payment Breakdown" is **not** a standalone top section. It renders **per unit
  inside the Net Revenue Section loop**: for each listing, its breakdown table sits directly
  above its statement table (so rollups like OSBR interleave by unit: OSBR 1 breakdown → OSBR 1
  net revenue → OSBR 2 …). Excel and PDF mirror this (see above).
- Payment breakdown columns (owner-set order): Guest, Conf Code, Channel, Guest Pay, Channel
  Fee, **Invoice Amount** (=InvoiceItem), Guesty Fee, Stripe Fee, Cleaning Fee, Tax, **Net Rental
  Revenue**. Rendered via `st.table` (static, full width, headers wrap to 2 lines so all columns
  fit, no scroll) with a highlighted TOTAL row.
- Net Revenue Section: **no Gross Revenue column** (removed; Excel/PDF never had it). Net Rental
  Revenue = payment-breakdown net; commission on it. `st.dataframe` with explicit
  `height=(rows+1)*35+3` so nothing scrolls internally (pages may be long — owner preference).
- Section-1 "Net Rental Revenue" equals Section-2 "Net Rental Revenue" (same figure per booking).

## Booking breakdown covers BOTH sources (Guesty + LTR)

The Section-1 booking breakdown is built from BOTH booking sources (owner's pull-steps model:
pull Guesty → pull LTR → dedup → make booking breakdown → create revenue sections). Guesty rows
come from `payment_breakdown_<p>.csv` (the fee waterfall); **LTR rows are fee-free**: channel label
`LTR`, **Total Payout (Guest Pay) = accommodation fare + cleaning fee**, no channel/Guesty/Stripe
fee, no tax. Cleaning comes from the LTR CSV's `Cleaning.Fee` column (0 if blank) and is
**non-commissioned**, so **Net Rental Revenue = accommodation fare (rent)** only — the exact same
convention as STR (`net_revenue` excludes cleaning; cleaning is a separate owner credit). Wiring
(all shared-layer, so dashboard/Excel/PDF/end_balances stay in sync):

- `ltr/records.py` exposes `cleaning_fee` + `gross_revenue = rent + cleaning`.
- Dashboard folds LTR cleaning into Net Owner Proceeds and merges LTR breakdown rows into the
  per-unit `_pb_by_unit` tables (`_ltr_pb_row`).
- `import_ltr.py` posts a non-commissioned `OWNER_ADJ` line `source_object='LtrCleaningCredit'`
  per LTR row with cleaning > 0, so the build's `amount_due` / Net Income include it. The distinct
  source_object keeps it clear of the STR `_apply_owner_cleaning_credit` (which DELETEs only
  `OwnerCleaningCredit` and sums **guesty** rows — no overlap with LTR).

Current LTR CSVs have `Cleaning.Fee` blank everywhere, so this is numerically inert until the owner
populates it (a populated cleaning fee then flows to the owner's payout non-commissioned).

## Commands

Activate the venv first: `source venv/bin/activate` (or call `venv/bin/python …`).

Full month close (period = `YYYY-MM`):

```bash
# 1. Pull Guesty (writes Guesty_booking_<p>.csv + payment_breakdown_<p>.csv)
python -m src.breakdown.fetch_month --period 2026-07 [--deactivated-csv <ui_export.csv>]
# 2. Convert the UI-shaped export to canonical form
python -m src.breakdown.convert_export --input inputs/2026-07/Guesty_booking_2026-07.csv \
       --output inputs/2026-07/guesty_converted.csv
# 3. Import guesty income (clear first to refresh base_amount)
sqlite3 db/owner_statement.sqlite "DELETE FROM ledger_lines WHERE source='guesty' AND category='INCOME';"
python -m src.run_month_close guesty-import --csv inputs/2026-07/guesty_converted.csv
# 4. QBO expenses/fees/JEs (re-sync: delete the period's qbo rows first — see mvp CLAUDE.md)
python -m src.run_month_close qbo-sync --start 2026-07-01 --end 2026-07-31
# 5. LTR/deferred rent (if inputs/<p>/LTR_<p>.csv exists) — run AFTER guesty-import, BEFORE build
python -m src.ltr.import_ltr --period 2026-07
# 6. Build: applies QBO fees, overrides STR net with payment_breakdown net, calcs PM fees,
#    writes totals + Excel/PDF statements
python -m src.run_month_close build --period 2026-07
# 7. Dashboard
venv/bin/streamlit run src/dashboard.py   # http://localhost:8501
# 8. Re-pack the deploy/ snapshot (DB + inputs + config + display code, gate re-injected)
python -m src.deploy_pack --period 2026-07
```

First-time setup: `python -m src.run_month_close init-db` then `sync-mappings`; QBO OAuth via
`qbo-auth` / `qbo-exchange --code <CODE>` (tokens → `config/secrets/qbo_tokens.json`).

## Deploy package (`deploy/` — dashboard-only, display-only)

A self-contained, **read-only** copy of the dashboard for hosting (Streamlit Cloud / Render),
mirroring `../owner_statement_mvp/deploy/`. The heavy pipeline runs locally; its result is
**baked in** and the deploy only reads + renders.

- **Layout mirrors the main project ROOTED AT `deploy/`** — `src/paths.py` is copied **verbatim**
  (its `parent.parent` resolves to `deploy/`), so the packaged `config/`, `inputs/<period>/`, and
  `db/` are found with **zero code change**. Contents: `streamlit_app.py` (host entry → runs
  `src/dashboard.py`), `src/{dashboard.py, paths.py, scope/{listing_filter,pm_rate}, ltr/records}`
  (all display-only — no pipeline / QBO / Guesty code, no secrets), `config/{mapping_classes.yml,
  config.yml, Listing_contacts.csv}`, `db/owner_statement.sqlite` (via `VACUUM INTO`),
  `inputs/<period>/{payment_breakdown, guesty_converted, LTR}`.
- **The only edit vs. the main dashboard** is a password gate right after `set_page_config`,
  enforced **only** when a `st.secrets["app_password"]` is set (off for local dev). `deploy_pack.py`
  re-injects this gate automatically after each pack — don't hand-edit `deploy/src/dashboard.py`.
- Available periods come from the DB (`statement_runs`). The DB may carry more periods than have
  input CSVs (e.g. 2026-05/06 have no `inputs/`); those degrade gracefully (Section 1 skipped) —
  same as the main dashboard.
- **Re-pack after each build** with `python -m src.deploy_pack --period <p>` — it VACUUMs the DB,
  copies the period's `inputs/<p>/*` + `config/*`, syncs the 4 display modules verbatim, and rewrites
  `deploy/src/dashboard.py` with the gate re-injected. The deploy is a snapshot until re-packed.
- Preview via the `deploy` entry in `.claude/launch.json` (port 8503, runs `deploy/streamlit_app.py`
  from the project root).

## Guesty token budget

5 tokens / 24h / client, shared with `GuestyFinancials` and `GuestyAccess`. This project caches
its own token at `config/secrets/guesty_token.json` and reuses it (≈1 mint/day). **Do not
force-refresh in a loop.**

## Working principles

Think before coding; state assumptions. Only implement what was asked. Surgical changes; match
existing style. Fix at the shared source layer so dashboard/Excel/PDF/end_balances update
together — never patch one product and leave the others stale.
