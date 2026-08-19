# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Python pipeline that generates monthly owner statements for Valta Realty vacation rental properties. It syncs data from QuickBooks Online (QBO), imports guest bookings from Guesty, and generates Excel statements and a Streamlit dashboard showing revenue, expenses, and owner payouts.

## Critical Business Logic: Net Revenue Calculation

**This is the most important thing to understand.** The net revenue formula is **channel-specific** and critical to statement accuracy.

### Cancelled Bookings
- Use `TOTAL PAID` (what the guest actually paid), NOT `TOTAL PAYOUT` (the full booking amount)
- This prevents showing credits for payments that were never received

### Confirmed Bookings (Guesty base in `convert_guesty_export.py`)

**Booking.com**:
- Guesty: `total_payout - cleaning - refund` (NO tax deducted; tax comes from QBO)
- QBO adjustment in run_month_close.py: `guesty_net - abs(channel_fee) - abs(tax_fee) - abs(stripe_fee)`

**Other channels (VRBO, Airbnb)**:
- Guesty: `total_payout - cleaning - refund - tax` (tax already in Guesty)
- QBO adjustment: `guesty_net - abs(channel_fee) - abs(stripe_fee)`

**Critical detail**: QBO fees from `get_qbo_fees()` are already negative (costs). Use `abs()` when subtracting. Do NOT apply fee deductions when fees exist.

**Example (BC-vx72pZyW5 Booking.com)**:
- Guesty: $3,747.59 (total_payout $3,872.59 - cleaning $125.00)
- Channel Fee (QBO): -$509.39
- Tax (QBO): $476.64 (cost, subtract)
- Stripe Fee: $0.00
- **Net = $3,747.59 - $509.39 - $476.64 = $2,761.56** ✓

### When Modifying Net Revenue Formula
1. **NEVER** restore database files with `git checkout` — loses all previous work
2. Delete ONLY guesty INCOME: `sqlite3 data/owner_statement.sqlite "DELETE FROM ledger_lines WHERE source='guesty' AND category='INCOME';"`
3. Re-run: convert → import → build in sequence
4. Pipeline recalculates dependent values (PM fees, totals, statements)

### `build` is idempotent — net revenue can no longer double-subtract fees
Each guesty INCOME row stores its immutable Guesty-base net (pre-QBO-fee) in
`ledger_lines.base_amount`, set at import time. `build` recomputes
`amount = base_amount − QBO fees` from that base every run (`_apply_guesty_fees()`
in `run_month_close.py`, which runs **before** `build_statements` so the stored
totals reflect the same fee-adjusted net the Excel shows). Consequences:
- Running `build` N times in a row yields identical net revenue and totals.
- The historical hazard — `build` subtracting fees from an already-adjusted
  `amount` (turning e.g. $642 → $559.29 → $476.58) — is gone. A single `build`
  now produces correct totals; no "build twice" lag.
- Clearing + re-importing guesty INCOME before `build` is still good hygiene
  (refreshes `base_amount` from the latest CSV) but is no longer required to
  avoid double-subtraction.

## LTR Rents & Deferred Revenue Import

Long-term lease (LTR) rents and deferred-revenue bookings are added to statements from a CSV (e.g. `data/LTR_YYYY-MM.csv`) via `src/import_ltr.py` (standalone, not a subcommand). The CSV's `Source` column splits rows: `Source=='LTR'` → tagged `source_object='LTR'`; everything else (BE-API/airbnb2/manual/VRBO monthly stays) → `source_object='DEFERRED'`.

**Storage trick**: `ledger_lines.source` has a CHECK constraint allowing only `qbo`/`guesty`/`manual`, so both groups insert as `source='qbo', category='INCOME'`, distinguished by `source_object` ('LTR'/'DEFERRED' — never collide with real QBO objects Bill/Deposit/Invoice/JournalEntry/Purchase). This routes them through the existing `other_income` path, so they render on statements and receive PM commission at the property's contract rate with **zero pipeline-code changes**. The importer is idempotent (deletes its own prior LTR/DEFERRED rows for the period first).

**Add-only-missing dedup**:
- DEFERRED: **LTR wins on confirmation-code collision.** If the code already exists in the ledger as a non-LTR/DEFERRED row (typically a guesty booking), that row is **deleted** and the DEFERRED row inserted in its place (`_replace_colliding()` in `import_ltr.py`). Rationale: guesty net for these channel bookings is sometimes wildly inflated (e.g. a single booking showing $16K); the LTR CSV carries the correct recognized amount. Since guesty rows aren't restored on re-run, keep the pipeline order guesty-import → import_ltr → build; a later `guesty-import` re-adds the deleted booking (dedup only skips if present), re-introducing the collision until `import_ltr` runs again.
- LTR: recognize each property's full monthly rent **once**. Add only the SHORTFALL = `LTR_amount − sum(existing period INCOME whose description LIKE '%rent%')`. Shortfall ≤ 0 → skip (rent already fully in QBO). Non-rent income (utilities, credits) is excluded from the base so it stays additive.

**Why the shortfall matters — Valta trust accounting**: QBO recognizes each month's rent via a JE into `Trust Liabilities:Owner Payables:1A - Net Earnings:Rents` (dated the 1st), with a matching `Management Commissions` line at the contract PM rate. Cash arrives as Deposits in `1B - Long term rent - owner`, often **split across multiple partial payments and months** (descriptions encode progress, e.g. "May rent 400/2100", "May rent $1700/$2100"). The `qbo-sync` ledger is frequently **stale/incomplete** — it can miss some deposits and JE Rents lines. The LTR CSV (whose amounts match the `Net Earnings:Rents` recognition) fills those gaps; the shortfall math avoids double-counting whatever cash *was* synced. **Caveat**: if `qbo-sync` is later re-run and pulls the previously-missing deposits, they double up with the LTR fill — prefer re-syncing QBO as the long-term fix, or re-run `import_ltr` afterward.

## Architecture

### Data Flow
```
Guesty CSV → convert_guesty_export.py → guesty_converted.csv
guesty_converted.csv + QBO data → run_month_close.py → SQLite
LTR/deferred CSV → import_ltr.py → SQLite (source=qbo, source_object=LTR/DEFERRED)
SQLite → dashboard.py (Streamlit) / excel_writer.py (statements)
```

### Key Tables (SQLite)
- **ledger_lines**: All transactions (sources: guesty, qbo; categories: INCOME, EXPENSE, FEE, TAX, RESERVE, OWNER_ADJ)
- **statement_runs**: Period metadata
- **statement_property_totals**: Per-property calculations (gross revenue, PM fees, amount due to owner)
- **properties, owners, owner_contracts**: Master data; owner_contracts stores PM fee rates

### Core Modules

**convert_guesty_export.py**: Guesty CSV → canonical format
- Filters: Confirmed + Cancelled with TOTAL PAID > 0
- Channel-specific net revenue calculation
- Output: guesty_converted.csv

**booking_fees.py**: QBO fee lookup
- `get_qbo_fees()`: Returns channel_fee, stripe_fee, tax, deductions (all negative)
- Deduplicates by description + amount
- Handles multiple Booking.com tax line items

**run_month_close.py**: Orchestrator
- `cmd_guesty_import()`: Imports CSV to ledger_lines (sets `base_amount = net_revenue`)
- `_apply_guesty_fees()`: Idempotently recomputes each guesty INCOME `amount = base_amount − QBO fees` from the immutable base; runs **before** `build_statements`
- `cmd_build()`: Calls `_apply_guesty_fees()`, then `build_statements`, then writes Excel. Uses `abs()` on QBO fees, calculates PM fees, generates statements
- Idempotent: re-running `build` cannot double-subtract fees (see "build is idempotent" above)

**dashboard.py**: Streamlit UI
- Property/period selector
- Net Revenue table (columns: Guest Name, Confirmation Code, Reservation Dates, Gross Revenue, Net Rental Revenue, Commission, Net Owner Revenue)
- Expenses by category (Cleaning, Supplies, Repairs, Utilities, Other) with column widths: Date 80px, Description large, Type medium, Amount 80px
- Summary: Starting Balance, Net Income, Current Balance, Owner Payout

**statement_engine.py**: Calculates property totals and PM fees; aggregates `statement_rollups` members into their parent
**excel_writer.py**: Generates Excel statements (font size 14)

**import_ltr.py**: LTR + deferred-revenue CSV → ledger_lines (standalone script, see "LTR Rents & Deferred Revenue Import")

## CLI Command Reference

All commands run through `src/run_month_close.py` (entry point: `python -m src.run_month_close <cmd>`). Activate the venv first: `source venv/bin/activate`.

| Command | Purpose |
|---------|---------|
| `init-db` | Create SQLite DB from `schema.sql` |
| `sync-mappings` | Load `mapping_classes.yml` / `mapping_accounts.yml` into DB master tables |
| `qbo-auth` | Print QBO OAuth URL (one-time) |
| `qbo-exchange --code <CODE>` | Exchange auth code for tokens → `data/qbo_tokens.json` |
| `qbo-sync --start YYYY-MM-DD --end YYYY-MM-DD` | Pull QBO expenses/bills/JEs into `ledger_lines` (source=qbo) |
| `guesty-import --csv <path>` | Import converted Guesty CSV into `ledger_lines` (source=guesty) |
| `build --period YYYY-MM` | Apply QBO fees, calc PM fees, write totals + Excel statements |

Helper scripts (not subcommands): `python -m src.list_classes` (pull QBO classes), `python -m src.export_class_stubs` (write `mapping_classes_stub.yml`), `python -m src.check_counts` (sanity-check ledger row counts).

Standalone scripts (not subcommands):
- `python src/convert_guesty_export.py` — with no `--input`, auto-detects the latest Guesty CSV in `data/`.
- `python -m src.import_ltr --csv data/LTR_YYYY-MM.csv --period YYYY-MM` — import LTR rents + deferred revenue (see "LTR Rents & Deferred Revenue Import").

## Common Tasks

### Re-syncing QBO for a period (pulls newly-added QBO transactions)
`qbo-sync` **appends** (plain INSERT with random UUIDs, no period-clear), so re-running it
without clearing first **duplicates every QBO row**. Procedure (owner preference: no DB
backups — overwrite in place, do NOT create `*.sqlite.bak-*` copies):
```bash
sqlite3 data/owner_statement.sqlite "DELETE FROM ledger_lines WHERE source='qbo' AND posting_date>='2026-05-01' AND posting_date<='2026-05-31';"
python -m src.run_month_close qbo-sync --start 2026-05-01 --end 2026-05-31   # refreshes ALL May QBO data
python -m src.import_ltr --csv data/LTR_2026-05.csv --period 2026-05         # the DELETE also removed LTR/DEFERRED rows
python -m src.run_month_close build --period 2026-05
```
The DELETE removes LTR/DEFERRED rows too (they're `source='qbo'`), so re-run `import_ltr`.
Token note: `data/qbo_tokens.json` access token auto-refreshes; the refresh token lasts ~100 days
(re-auth via `qbo-auth`/`qbo-exchange` if it has expired). "Owner other activities" (e.g. an
owner who collected an Airbnb payout directly) post in QBO as a JournalEntry with an offsetting
`1C - Owner Expenses:Other Owner Expenses` line — these reach statements only via this sync.

### Full Pipeline Rebuild
```bash
python src/convert_guesty_export.py
# clear + re-import guesty INCOME to refresh base_amount from the latest CSV
# (build is idempotent and won't double-subtract, but this keeps the base current):
sqlite3 data/owner_statement.sqlite "DELETE FROM ledger_lines WHERE source='guesty' AND category='INCOME';"
python -m src.run_month_close guesty-import --csv data/guesty_converted.csv
python -m src.import_ltr --csv data/LTR_2026-05.csv --period 2026-05   # if LTR/deferred CSV exists
python -m src.run_month_close build --period 2026-05
```
Run `import_ltr` AFTER `guesty-import` (its DEFERRED dedup matches existing guesty bookings by code) and BEFORE `build`.

### Yacinde bookings — RETIRED `import_yacinde_old.py` (now in the Guesty CSV)
**As of 2026-07-30, Yacinde reservations arrive in the normal Guesty export
(`Guesty_booking_YYYY-MM.csv`) and flow through the standard pipeline — do NOT run
`src/import_yacinde_old.py` or read `data/Yacinde old bookings.csv` any more.** The
Guesty export now carries both the owner-direct reservations (SOURCE `owner`/
`owner-guest`/`manual`, mostly `TOTAL PAYOUT=0`) and the real channel bookings
(airbnb2/VRBO/BE-API, real payout). The standard converter + `guesty-import` handle
both: $0 owner bookings → $0 net revenue (no commission); channel bookings → real
net revenue, commissioned at the property's MCR (18%). All `Yacinde B1…F5` listings
are `Supplies='central'` in `Listing_contacts.csv`, so `_apply_central_supplies()`
(in `build`) charges the same **`0.9 × guests × min(nights, 60)`** per confirmed
booking (a `CentralSupply` line) — the supply fee is now data-driven off the real
Guesty bookings instead of the hand-maintained old CSV. Channel bookings that also
appear as QBO deposits are de-duplicated by `_exclude_duplicate_guesty_deposits`
(Guesty kept), so no double-count.

**Migration when rebuilding a period the old way was used for**: `import_yacinde_old`
left `source_object='YacindeOld'` ($0 `source='guesty'` INCOME) and
`source_object='YacindeSupply'` (`source='manual'` EXPENSE) rows. The guesty-income
clear (`DELETE … source='guesty' AND category='INCOME'`) removes the `YacindeOld`
rows, but `YacindeSupply` is `source='manual'` and survives BOTH a `qbo-sync` clear
and a guesty-income clear — so it must be deleted explicitly or it double-charges
against the new `CentralSupply` lines:
```bash
sqlite3 data/owner_statement.sqlite "DELETE FROM ledger_lines WHERE source_object IN ('YacindeOld','YacindeSupply') AND posting_date>='YYYY-MM-01' AND posting_date<='YYYY-MM-31';"
```
July 2026 has been migrated. Other months (Aug–Dec 2026) still hold old
`YacindeOld`/`YacindeSupply` rows from the retired importer — clear them (above) when
rebuilding those periods with their own Guesty exports. `src/import_yacinde_old.py`
and `data/Yacinde old bookings.csv` are kept only for historical reference.

### Central supplies — formula supply fee for `Supplies='central'` properties
The `Supplies` column in `Listing_contacts.csv` is the indicator: `central` (Valta
provides supplies and charges a per-booking formula fee), `owner` (owner pays; actual
QBO supplies), or `LTR`. `listing_filter.central_supply_property_ids()` maps the
`central` rows to property_ids (same label→pid aliasing as `allowed_property_ids`).
`run_month_close._apply_central_supplies()` (in `build`, before `build_statements`)
for those properties:
1. **Suppresses** the QBO per-booking supply lines — description `LIKE 'Supplies Charge
   | …'` — by setting `include_in_statement=0` (re-includes first each build, so a
   property leaving `central` gets them back). These come as a double-entry pair (a
   `Billable Expense Income` side that never counted + an `Owner Expenses:Supplies`
   side that did); suppressing both removes only the counted cost. **General /
   non-booking** supply lines (Costco/Amazon/reimbursements — anything NOT matching
   `Supplies Charge | …`) are left untouched, so they still come from QBO.
2. **Inserts** one `source='manual'`, `source_object='CentralSupply'` Supplies expense
   per confirmed central-property Guesty booking = **`0.9 × guests × min(nights, 60)`**
   (skips cancelled). Guests come from the `guests` column the converter writes to
   `guesty_converted.csv`; nights = checkout − checkin. Yacinde is now a normal
   `central` property here — its bookings arrive in the Guesty CSV (the retired
   `import_yacinde_old`/`YacindeOld` special-case no longer applies; see the Yacinde
   section above).
Idempotent. The shared formula is `run_month_close.supply_charge(guests, nights)`.
Because the manual supplies match the `%Owner Expenses%`
amount_due filter and the display queries accept `source IN ('qbo','manual')`,
dashboard/Excel/amount_due agree. QBO's own `Supplies Charge` amounts were already
~this formula, so owner payouts barely move (change is mainly de-duplication + cancelled
exclusion + the 60-night cap).

### OSBR-RV — Hipcamp.com credits as commissioned Net Revenue
OSBR's RV site is booked through Hipcamp, whose payouts land in QBO as plain INCOME
Deposits (`property_id='osbr'`, `vendor_customer LIKE '%Hipcamp%'`). Left alone they'd
sit **uncommissioned** in "Other Credits". `run_month_close._apply_osbr_rv()` (in `build`,
before `_apply_guesty_fees`) moves each into the Net Revenue section as an **`osbr_rv`**
sub-listing so it earns PM commission at OSBR's rate:
1. **Ensures** the display property `osbr_rv` (name **"OSBR-RV"**) exists — `INSERT OR
   IGNORE`, reusing OSBR's `owner_id`/`qbo_class_id`, `is_active=0` so it stays out of the
   dashboard property dropdown (it's a sub-listing, not a standalone statement).
2. **Suppresses** each OSBR Hipcamp deposit (`include_in_statement=0`, out of Other
   Credits) and **inserts** a `source='guesty' INCOME`, `source_object='OsbrRV'` "booking":
   `property_id='osbr_rv'`, `amount=base_amount=`credit, **gross = net = credit**,
   `checkin=checkout=`the credit's `posting_date` (**Reservation Date = credit date**;
   renders as a 0-night same-day entry), `guest_name='Hipcamp'`, code `HIPCAMP-<date>`.
Because `osbr_rv` is a member of the **`osbr` rollup** (`config.yml`), the row aggregates
into OSBR's statement, renders as its own Net Revenue sub-section, and commissions at
OSBR's parent rate (16%) via the standard guesty path — no display-code changes. Excluded
from `_apply_central_supplies` (like `YacindeOld`). Idempotent: deletes its own prior
`OsbrRV` rows and re-derives from the Hipcamp deposits each build, so a `qbo-sync` that
re-includes the deposits is re-corrected next build. Net effect on OSBR: the RV revenue
stays in gross but now carries MCR (e.g. July $564.40 → −$90.30 fee; amount_due
$31,581.75 → $31,491.45), matching QBO's own "Hipcamp MCR" bill.

### View Dashboard
```bash
streamlit run src/dashboard.py  # http://localhost:8501 (Streamlit default)
```

### Debug Booking Calculation
```python
import pandas as pd
df = pd.read_csv('data/guesty_converted.csv')
booking = df[df['booking_id'] == 'BC-vx72pZyW5'].iloc[0]
print(booking[['total_payout', 'cleaning_fee', 'net_revenue']])
```

## Configuration

- **mapping_classes.yml**: Property metadata (property_id, owner name/email, address, pm_fee_rate, term, owner_pays_* flags)
- **mapping_accounts.yml**: QBO account → expense category rules
- **config.yml**: Database path, QBO realm ID, PM fee defaults, `statement_rollups`
- **Listing_contacts.csv**: Property addresses and owner contact info (source for mapping_classes.yml)

**Statement rollups** (`config.yml` → `statement_rollups`): map a parent `property_id` to a list of member listings. The parent gets ONE statement aggregating its own + members' ledger lines (`property_id IN (...)` in both `statement_engine.build_statements` and `run_month_close.cmd_build`); members are skipped (no separate statement) and PM uses the parent's rate. Current use: `bellevue_14507` consolidates its 4 units; `beachwood` consolidates `beachwood_1..10`; `osbr` consolidates `osbr_1..12`. Each parent statement's Net Revenue section renders **one sub-table per member listing** (labelled by listing name, each with its own nights + $ subtotal) followed by a **Total Net Revenue** grand-total row; the Expenses section's **Type column shows the listing name** each cost belongs to. This per-listing layout is implemented in all three products — dashboard, `excel_writer.write_statement` (takes a `property_names` map; bookings/expenses carry `property_id` from `cmd_build`), and `dashboard.generate_pdf` (takes the pre-built `booking_records`, so LTR rent is included in the PDF). Single-listing statements render one section with no grand-total row. Note: `build` does not delete stale per-member Excel files from earlier runs — remove them manually (e.g. `rm output/<period>/statements/beachwood_[0-9]*_owner_statement_*.xlsx`).

**PM fee rate — single source of truth (`src/pm_rate.py`), NO silent default**: The authoritative rate is per-property in `owner_contracts` (loaded from `mapping_classes.yml` `pm_fee_rate`), selected by the row **effective for the period**. `resolve_pm_fee_rate(conn, property_id, period_start)` in `pm_rate.py` is the ONE implementation of that lookup; `statement_engine.build_statements` (stored `amount_due`), `dashboard.load_statement_data` (per-booking table + LTR rows), and `dashboard.generate_pdf` all call it, so every product commissions at the same rate and the per-booking commission column foots to the stored Net Income. (It lives in its own dependency-free module because the dashboard runs as a script and can't bare-import `statement_engine`, which uses relative imports.) **There is deliberately no default fallback**: `resolve_pm_fee_rate` returns `None` when a property has no configured rate. If such a property has commissionable revenue, `build_statements` **raises `ValueError`** (naming the property and the revenue) and the dashboard **`st.stop()`s** with an instruction to set the rate — rather than guessing. So a missing rate surfaces loudly instead of quietly commissioning at a wrong number. `config.yml` `default_pm_fee_rate` is no longer consulted by `build`. When onboarding a property or changing fee rates, set `pm_fee_rate` in `mapping_classes.yml` and run `sync-mappings`. Prior bugs: dashboard/PDF used a hardcoded `0.18` fallback with no effective-date filter (drifted from stored `amount_due`), and `build`/dashboard/Excel silently substituted the `0.16` config default for no-contract properties.

**PM commission base (what gets commissioned)**: PM fee is charged on STR booking revenue (`source='guesty'`) **plus ALL rent** — whether the rent arrived as an `import_ltr` `source_object IN ('LTR','DEFERRED')` row OR as a **plain QBO rent Deposit** ("July Rent" etc.). This is the single `statement_engine._RENT_PRED` definition of commissionable rent: `source='qbo' INCOME` where `source_object IN ('LTR','DEFERRED')` OR `description LIKE '%rent%'` (excluding `garage`/`parking` rent). It is **NOT** charged on non-rent QBO "Other Credits" — utilities, garage/parking rent, and misc/insurance/payroll deposits (these lack `rent` in the description, so they're excluded automatically). See `statement_engine.build_statements` `commission_base = guesty_rev + commissionable_other`. Non-rent credits still appear in gross revenue and on the statement's "Other Credits" section at full value; they just aren't commissioned.

- **Owner decision (2026-07): commission ALL rent, LTR or STR.** This reversed the prior rule (commission only Guesty + LTR/DEFERRED). It also **fixed a real under-commission bug**: when a property's full rent arrived as a QBO deposit, `import_ltr`'s shortfall was $0 → no LTR/DEFERRED row → the rent escaped commission in `amount_due`, even though the dashboard/Excel (which build rent Net-Revenue rows from `data/LTR_<period>.csv`) already showed commission on it. E.g. Beachwood July: only $2,000 of $19,250 rent was commissioned; now all $19,250 is (amount_due $20,433 → **$18,707.76**, ~$1,725 more PM fee).
- **Penny reconciliation**: STR bookings commission **per booking**; rent commissions **per listing** (`GROUP BY property_id` in the PM-fee sum), because each product shows **one full-rent row per listing** (from the CSV). By `import_ltr` construction, DB commissionable rent per listing (deposits + LTR/DEFERRED) **equals** the CSV `Net Revenue` per listing, so `Σ round(rate·rent_listing)` in `statement_engine` matches the dashboard/Excel per-listing commission exactly. Verified all 19 July rent listings match DB↔CSV.
- **Demo note**: the demo dashboard needs `data/LTR_<period>.csv` shipped in `deploy/data/` to render rent under Net Revenue (commissioned); without it, rent falls back to "Other Credits" and won't foot to the (now rent-commissioned) `amount_due`. July's CSV is shipped; May/June are not (July-only scope).

**Expense refunds (QBO `Credit` purchases)**: A QBO Purchase with `Credit: true` is a vendor refund/return (e.g. a returned supply). `qbo_sync.sync_qbo_expenses` stores it **positive** (`abs(amount)`) so it REDUCES the expense subtotal; normal purchases stay negative. Don't revert to a blanket `-abs(amount)`.

**Refunded non-Guesty channel bookings (`…1A - Net Earnings:Resolutions`)**: A Booking.com/channel booking that came in as a QBO **Deposit** (shown in "Other Credits", counted as owner income) and was later **refunded** posts the refund to `Trust Liabilities:Owner Payables:1A - Net Earnings:Resolutions`. No statement section pulls that account — the expense section (display AND `amount_due`) filters `%Owner Expenses%` (i.e. `1C - Owner Expenses`) — so the refund is invisible and the unoffset deposit **overstates the payout**. `run_month_close._offset_refunded_deposits()` (in `build`, after `_exclude_duplicate_guesty_deposits`) pairs each such refund with its Other-Credits deposit (**same property, equal magnitude, shared confirmation-code token**) and **suppresses BOTH** (`include_in_statement=0`) so the pair nets to $0. **Deliberately narrow (owner decision):** ONLY refunds matching a non-Guesty Other-Credits deposit are offset; the ~55 Resolutions lines/month tied to **Guesty bookings are left untouched** (Guesty net is the STR revenue basis — deducting them could double-count). Both rows must fall in the period (each build self-contained). Idempotent; re-applied each build so a later `qbo-sync` re-inclusion is re-corrected. E.g. OSBR July: the Jessica Smith deposit+refund ($1,083.26) is removed, dropping amount_due $31,491.45 → $30,408.19.

**Tax paid to owner (transient occupancy tax pass-through)**: Lodging tax collected from guests and passed to the owner posts to QBO account `…Owner Income:Taxes Paid to Owners` (stored positive). It is owner income, shown as the per-booking **"Tax Paid to Owner"** column (after Management Commission), added to Net Owner Revenue, and **NOT** commissioned. `statement_engine` folds it into `taxes`/`amount_due` (so totals reconcile) **only for `owner_pays_taxes` properties** — `cmd_build` passes `tax_props` (the set of flagged property_ids) to `build_statements`, which gates the fold `if tax_props is None or pid in tax_props`. This must match the display, which also only shows the per-booking "Tax Paid to Owner" column when the flag is set; without the gate a non-flagged property that ever accrues a "Taxes Paid to Owners" line would inflate `amount_due` with no offsetting row and break reconciliation. Per-booking display is matched by the stay's date range (`get_owner_costs(..., checkin, checkout)`, dashboard `owner_tax`). These lines are NOT in the `'%Owner Expenses%'` expense section, so there's no double-count. **Known limitation**: the per-booking date-range match (`description LIKE '%<checkin> to <checkout>%'`) can attribute one tax line to multiple bookings that share identical checkin/checkout (e.g. rollup members), double-counting in the displayed Tax total while `amount_due` counts each line once.

**Owner cleaning fee credit (`owner_pays_cleaning` properties)**: The Guesty net-revenue formula subtracts the guest-paid cleaning fee out of booking revenue. For `owner_pays_cleaning` properties the owner is responsible for cleaning, so that fee belongs to the owner. `run_month_close._apply_owner_cleaning_credit` (runs in `build`, before `build_statements`) sums each property's Guesty `cleaning_fee` (from the converted CSV, keyed by `booking_id`, **0 for canceled** stays) and posts it as a single **non-commissioned `OWNER_ADJ` line** (`source_object='OwnerCleaningCredit'`) so it lands in `amount_due` via `statement_engine`'s `owner_adjustments`. Idempotent (clears its own prior lines each build). `OWNER_ADJ` has no display section, so it does **not** double-print the per-booking "Owner Cleaning Fee" column. The dashboard shows that column and reads Net Income straight from `amount_due` (it must **NOT** add the cleaning fee again — that was a bug); the Excel "Owner Cleaning Fee" column also shows this guest-paid fee (via `booking_dict['owner_cleaning_cost']` overridden in `cmd_build` from `guesty_clean_by_code`), **not** the QBO cleaning cost. Net effect: `amount_due`, dashboard, Excel, and end_balances all agree, and match VRP (e.g. bellevue_242 June $5,789.10 vs VRP $5,789.11).

**Penny reconciliation (stored, not just display)**: All money is rounded to cents **at storage**, so derived totals reconcile without display hacks. Key sources of sub-cent drift and their fixes: guesty net + `base_amount` rounded in `_apply_guesty_fees`; guesty/LTR import amounts rounded; and — critically — the **PM fee equals the SUM of per-line rounded commissions** (`Σ round(-amount_i·rate)`), NOT `round(rate·Σamount)`. The two differ by a penny and the latter makes stored Net Income disagree with (Net Owner Revenue − Expenses) by $0.01. `statement_engine` also rounds every stored total in `statement_property_totals`. Dashboard/Excel additionally round per-cell so each column TOTAL equals the sum of the rounded rows shown.

---

## Working Principles

**Think before coding.** State assumptions. If multiple interpretations exist, present them. If something is unclear, ask.

**Simplicity first.** Only implement what was asked. No speculative abstractions, error handling for impossible scenarios, or "flexibility" that wasn't requested.

**Surgical changes.** Only touch what you must. Match existing style. Don't refactor adjacent code or delete pre-existing dead code unless asked.

**Goal-driven execution.** Define success criteria upfront. Loop independently by making explicit what needs verification.
