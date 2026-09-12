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
  common/     # shared infra:       db, config, utils, mappings, income_rules (QBO INCOME classification)
  scope/      # STATEMENT SCOPE:    listing_filter (which statements exist + rollups), pm_rate (commission rate)
  guesty/     # ACCESS GUESTY API:  client, config, reservation_financials, reservations_batch
  breakdown/  # BOOKINGS BREAKDOWN: payment_model, fetch_month, deactivated, convert_export, adapter
  ltr/        # LTR BOOKING SOURCE: import_ltr (pull LTR/deferred rent → ledger), records (display)
  netrevenue/ # BUILD NET REVENUE:  engine (was statement_engine)
  expense/    # EXPORT EXPENSE:     qbo_client, qbo_sync, owner_costs
  reporting/  # BUILD DASHBOARD:    excel_writer, export_end_balances, booking_breakdown,
              #                     other_income (Section-1 source #3),
              #                     period_sources (THE loader for all three sources),
              #                     monthly_summary (+ its osbr_summary wrapper)
  onetime/    # one-time & maintenance scripts (quarantined; see src/onetime/README.md)
```

**Booking sources** are parallel: `guesty/` (Open-API pull, → `breakdown/`) and `ltr/` (LTR CSV
→ ledger via `import_ltr`; `ltr/records.py` reads them back for display). **`scope/` sets up the
statement scope** — `listing_filter` decides which statements exist AND which listings roll
into each (see "Statement scope" below), `pm_rate` the commission rate — resolved up front by
`build`, dashboard, and PDF alike.

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

### `inputs/**/*.csv` is DERIVED, gitignored, and only `fetch_month` writes it

`payment_breakdown_<p>.csv` has exactly ONE writer — `breakdown/fetch_month.py:191`, which
runs `payment_model.compute_breakdown` on a fresh Guesty pull and **overwrites the file**.
Six modules read it (`run_month_close`, `dashboard`, `monthly_summary`, `exception_bookings`,
the deploy dashboard) and none write it. Two consequences that have both bitten:

- **A `payment_model.py` change does not reach any statement** until that period's CSV is
  regenerated. With 5 Guesty tokens/24h a 20-month re-pull is impossible, so a fee-rule fix
  has to be BACKFILLED into every stored CSV — see the VRBO refund and Stripe rules above.
  After any `payment_model` change, audit the stored CSVs; do not assume the code landed.
- **`.gitignore:8` ignores `inputs/**/*.csv`**, so those backfills have no version history and
  no `git diff` to review or revert. A hand-correction that `compute_breakdown` cannot
  reproduce is therefore invisible AND silently lost on the next pull of that period.

#### EVERY manual adjustment lives in `payment_model.py`, never only in a CSV

Owner requirement 2026-09-02: a per-reservation correction must be **remembered and
re-applied on every `fetch_month`**. Since `fetch_month` is the CSV's only writer and it
overwrites, the only way to survive a re-pull is to be in the code that computes it. There
are now no CSV-only corrections left: `STRIPE_FEE_OVERRIDES` absorbed the last three
(`HA-PLHAdi3`, `HA-sebniGp`, `GY-MuLckM3e` — QBO's own charge where the
`base x rate + n x $0.30` formula cannot reproduce it).

- **The tables** are grouped in `payment_model.py` and registered in
  `MANUAL_ADJUSTMENT_TABLES`: `NO_PROCESSING_FEE_CODES`, `PET_FEE_NOT_IN_CHANNEL_FEE`,
  `STRIPE_INTERNATIONAL_CARD`, `STRIPE_DOMESTIC_CARD`, `MANUAL_SERVICE_FEES`,
  `STRIPE_FEE_OVERRIDES`. **Add a new correction to one of these — never to a CSV.**
- **`fetch_month` prints what it re-applied** for the period after writing Output B, so a
  correction that stopped matching (booking cancelled, code re-keyed) shows up as a missing
  line in the run's own output instead of as a number quietly reverting.
- **`python -m src.breakdown.audit_adjustments`** checks the other direction — that every
  stored CSV on disk matches the tables. No API token needed, exits 1 on a mismatch, and
  reports `dangling` codes whose booking is in no stored CSV. Run it after any
  `payment_model` change and after any `fetch_month`.
- An override applies through the **cancel branch** too (`canc_stripe` re-applies
  `STRIPE_FEE_OVERRIDES`): QBO's actual charge outranks the recomputation. `has_processing`
  still outranks the override, so a code in `NO_PROCESSING_FEE_CODES` stays at $0.

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

`qbo_sync.py` / `owner_costs.py` / `qbo_client.py`, ~unchanged from mvp.

## This project READS QuickBooks. Writes live in `../QBO_operations`

Owner decision 2026-09-08. QuickBooks **writes** — channel payout journal entries,
reservation payments, account recategorisations — are a separate concern from
producing owner statements, and they now live in the sibling `QBO_operations`
project. Read its CLAUDE.md before touching anything QBO-write-shaped.

It is enforced in code, not by convention: `expense/qbo_client.py` raises
`ReadOnlyError` on any method but GET, so a write introduced here fails at the
client instead of silently posting to the live company file. `auth_url` and
`exchange_code_for_tokens` are gone from it, and `run_month_close qbo-auth` is now a
stub that points at `QBO_operations`. Everything the monthly close needs is a read:
`qbo_sync`, `reporting/qbo_adjustments`, `reporting/exception_bookings`,
`onetime/list_classes`, `onetime/export_class_stubs`.

**The OAuth token lives over there** — `paths.QBO_TOKENS` points into
`../QBO_operations/config/secrets/`. Intuit rotates the refresh token on every
refresh, so two copies invalidate each other and there is exactly ONE on disk;
this project still refreshes it (that mutates the token, not the books) and writes
back to the same file. Never add a local `qbo_tokens.json`. `.env` is duplicated in
both projects, which is safe because the client id/secret are static.

`QBO_operations` reads back from here — `mapping_classes.yml`, the stored Guesty
exports, `ltr.labels.to_property_id`, the ledger DB — all through its single
`src/bridge.py`. That is why **`to_property_id` now lives in `ltr/labels.py`**
(dependency-free, only `re`) and `ltr/records.py` re-exports it: the sibling and the
deploy bundle can import the one map without pulling in pandas. Both files ship in
the deploy bundle; adding one without the other breaks the deploy dashboard.

### Task 3 — dashboard / Excel / PDF (`src/reporting/` + root `dashboard.py`)

`dashboard.py` (+ `generate_pdf` inside it) stays at `src/` root (flat Streamlit script);
`reporting/excel_writer.py` and `reporting/export_end_balances.py` are the other outputs.
See dashboard notes below. Net-revenue math lives in `netrevenue/engine.py` (was
`statement_engine.py`); property scope + commission rate come from `scope/`
(`listing_filter`, `pm_rate`); `run_month_close.py` (root) conducts all tasks.

## Statement scope: `config/Listing_contacts.csv` is the standard (Listing / Property)

The contacts file is the SINGLE source for **which statements exist** and **which listings
roll into each one**. Two identity columns:

- **`Listing`** — the individual unit; matches a QBO class / Guesty listing (`property_id`).
- **`Property`** — the statement it reports under. Every Listing sharing a Property is
  consolidated into ONE statement.

`src/scope/listing_filter.py` turns it into both artefacts (`resolve_scope`,
`statement_rollups`, `allowed_property_ids`), and `run_month_close` + `dashboard` BOTH read
it, so Excel/PDF and the web app can never drift. Current groupings: `Seattle 10057` ←
Lower/Upper/Whole; `Seattle 906` ← Lower/Upper; `Seattle 710` ← 710 + ADU;
`Yacinde NuGrowth` ← B1 B2 B6 C1 E1 E3 F1 F5; `Yacinde Holdings` ← B3 B4; plus the older
Beachwood / OSBR / Bellevue 2323 / Burien 14407 / Seattle 7434 rollups.

- **Listing** labels resolve through `_norm`/`_alias` (`Cottage 4` → `osbr_4`), and the
  lookup includes INACTIVE properties, because a deactivated listing (`osbr_rv`) is still a
  valid rollup member. **Property** labels resolve by EXACT normalized match, else a slug
  (`Seattle 10057` → `seattle_10057`) — deliberately no alias, or `Seattle 906` would fold
  into the Lower unit instead of getting its own shared statement.
- A Property that is NOT itself a QBO class needs a **statement-only parent** row in
  `config/mapping_classes.yml` with a `synthetic:<pid>` qbo_class_id + `pm_fee_rate`
  (`seattle_10057`, `seattle_906`). Without it the Property resolves to nothing and its
  units drop out of scope entirely. Nothing in `src/` reads the `synthetic:` prefix — it is
  simply an id no QBO ClassRef can match.
- **But check QBO first: a `synthetic:` id on a Property that DOES have a real class silently
  eats every expense posted to the parent.** `yacinde_nugrowth` and `yacinde_holdings` carried
  synthetic ids while QBO had real classes for them (`1000000034` / `1000000035`), so
  `qbo_sync` logged those lines as `CLASS_NOT_MAPPED` and dropped them — $3,218.18 of 2026-08
  staging/supply cost and a $10,000.00 2026-07 line, invisible on the statement. Fixed
  2026-09-02 by pointing both rows at the real ids. `renton_18823` was the same failure from
  the other side: an Active listing in `Listing_contacts.csv` with QBO class `1000000036` and
  no `mapping_classes.yml` row at all, so **guesty-import skipped its bookings too**
  ("unknown properties skipped") and it had no statement.
  **After any scope change, read both signals:** guesty-import's "N unknown properties
  skipped" line, and `SELECT code, message, COUNT(*) FROM exceptions GROUP BY 1,2` after a
  qbo-sync (the table is cleared per sync, so it only shows the period just pulled).
- `config.yml: statement_rollups` is now only a MERGED-IN fallback for units the contacts
  file omits (`beachwood_6`, `bellevue_14507_unit_*`) — they carry real ledger lines, and
  dropping them would delete their money from the parent's statement.
- `central_supply_property_ids` resolves per **Listing** (the supply charge is posted
  against the unit that took the booking).

### Retiring a listing: `Status = Inactive`, NEVER delete the row

The file has no time dimension, so **deleting a row erases that property's statement from
every historical month** — removing `Kirkland 10219` silently wiped $32,202.67 of statements
across 17 past periods. Set `Status = Inactive` instead:
`allowed_property_ids(conn, base_dir, period_start, period_end)` keeps a retired Property in
scope only for periods with **at least one nonzero ledger line**, so its history stays intact
and it stops the month it winds down (a wound-down listing keeps trailing $0 bookkeeping rows
for months — Kirkland carries 17 zero-amount expense lines in 2026-07). Testing for INCOME
alone is too strict: a renovation month with costs and no bookings is still a statement the
owner needs. Callers with no period argument get the unfiltered set.

## STR net comes from the payment breakdown ONLY — QBO is never asked for a fee

Owner decision 2026-08-29: **the payment breakdown IS the fee model.** It already carries the
channel / Guesty / Stripe fee for every booking (`breakdown/payment_model`, a transcription of
`payment_structure.xlsx`), so there is nothing to look up in QuickBooks.

`build` used to run `_apply_guesty_fees` first (net = `base_amount` − fees matched out of QBO
Bills) and then overwrite it with the breakdown net — so that step was dead work for every
booking that HAS a breakdown row, and survived only on the bookings it was least able to price.
Worse, its fallback matched QBO fees by **date range + channel name**, which attached booking
`6417694642`'s $132.20 Booking.com channel fee to the unrelated $0 owner stay `GY-9n5Dk4ZL`
and drove its ledger net to **−$132.20**. **`_apply_guesty_fees` is deleted.**

`_apply_payment_breakdown_net` is now self-contained and sets EVERY guesty INCOME row, two
cases and only two: **in the breakdown → its `net_revenue`; not in it → `base_amount`**, the
immutable Guesty total payout. (A booking is absent because the API dropped its deactivated
listing, or because it is a $0/comp row the breakdown drops.) `base_amount` is never written
over, so the step stays idempotent. If the period's breakdown CSV is missing it prints a loud
warning and every booking falls back to the Guesty base — gross of channel fees, so pull the
period rather than shipping that.

The QBO fee lookup is gone from `build` **entirely**, not just from net revenue. The Excel
loop also called `get_qbo_fees` (plus `_calculate_implied_channel_fee`) once per booking to fill
`booking_dict['total_channel_and_card_fees']` — and nothing read that key: it appeared exactly
once more in the whole repo, in an `excel_writer.write_statement` docstring. The Excel Net
Revenue section's columns are Reservation Dates / Confirmation Code / Guest Name / Net Rental
Revenue / Management Commission / Net Owner Proceeds / Commission % (+ optional Owner Cleaning
Fee / Tax Paid to Owner) — no fee column at all. The Channel/Guesty/Stripe **Fee columns that do
exist in Excel belong to the Booking Breakdown table**, which `reporting.booking_breakdown`
builds from `payment_breakdown_<p>.csv`. So `_calculate_implied_channel_fee` and the
`get_qbo_fees` import are deleted; `get_owner_costs` stays (`owner_cleaning_cost` /
`owner_tax_cost` ARE read, at `excel_writer.py:269-270`). Deleting it moved $0.00.
**`get_qbo_fees`' 79-line body has since been deleted too** — it outlived its last caller by
one cleanup and was still there to be re-imported by mistake. Its module, having no fee logic
left in it, is now `expense/owner_costs.py` rather than `expense/booking_fees.py`.

**Never reintroduce a QBO fee lookup here.** If a fee is needed for display, take it from the
payment breakdown, which is the one place fees are defined.

It is the single STR commission basis: `statement_engine`, dashboard, Excel and PDF all read
the ledger `amount`, so this one write propagates to every product at once — stored
`amount_due`, Summary Net Income, and the Net Revenue Section's Net Rental Revenue + commission
all read it and foot. `build` twice gives identical totals.

## The commission base is net revenue AFTER the Guesty and Stripe fees

Owner decision 2026-08-30, settling the one structural difference left against QuickBooks:
**owner income is struck after the Guesty 1% and the Stripe fee, and commission is charged on
that.** This is already what the pipeline does and needs no code change —
`payment_model.compute_breakdown` subtracts `- stripe_fee - guesty_fee` unconditionally in the
net-revenue formula (they are $0 on the channel-collected groups), `_apply_payment_breakdown_net`
writes that net onto the guesty ledger `amount`, and `netrevenue.engine` commissions the `amount`.
So this file records the decision, not a change.

**QuickBooks agrees — but only once its fee-Deduction credits are netted in.** QBO bills
commission on Σ `Guest Charges:Owner Income:*` (before any fee) and then posts SEPARATE credit
lines to the same account — `Stripe fee Deduction | <code>`, `VRBO channel fee Deduction |
<code>` — worth `rate × fee`. Net of those, QBO's basis is the after-fees net, exactly ours.
**Any comparison that matches only `description LIKE 'Management Commission | %'` silently
omits them** and manufactures a gap the size of the fees: 393 such credits in 2026-07 alone,
−$4,830.43. Netting them moved the 2026-06..08 tie rate from 579/1,032 to 771/1,032 and halved
the apparent gap. Per period, against QBO's NET commission:

| period | ours | QBO net | gap | tie |
|---|---|---|---|---|
| 2026-06 | $55,066.57 | $55,072.78 | **+$6.21 (0.01%)** | 172/260 |
| 2026-07 | $71,385.74 | $71,523.99 | **+$138.25 (0.19%)** | **388/401 (97%)** |
| 2026-08 | $55,192.77 | $59,478.20 | +$4,285.43 (7.21%) | 211/371 |

2026-08 is an OPEN month whose fee bills are only ~95% posted, so its gap is incomplete
billing, not a modelling difference — do not chase it until the month closes.

**The one real remaining difference is REFUNDS, and OURS wins** (owner, 2026-08-30):
**a guest refund reduces the commission base — we do not charge on money the owner gave back.**
QuickBooks does not make that reduction, so a refunded booking will always show us billing less
than QBO. That is intended; do not reconcile it away.

`GY-hkpagAqS` (longbranch_6821 2026-07) is the clean case — a $2,811.40 booking with $600
refunded. QBO charges $230.40 (= $2,304.00 Owner Income × 10%) less a $10.46 Stripe-fee
deduction = **$219.94**; we charge $159.95 on a net of $1,599.46. The whole $59.99 difference is
the $600 × 10%; nothing else about the booking differs.

The mechanism is already in `payment_model`: `net_base = total_paid` for a refunded booking
(NEVER `InvoiceItem − total_refunded` — `total_paid` is already net of refunds, and for a
booking QBO re-cut the InvoiceItem is too, so subtracting again double-counts). Portfolio cost
of the policy over 20 months: **29 confirmed bookings, $5,844.85 out of the commission base,
$878.84 of commission (~$527/yr)**. Movers: shelton_250 `HA-Na54Efv` 2025-04 −$132.48,
bellevue_2243 `HA-fT3tQ4a` 2026-07 −$108.00, hoodsport_26060 `HA-dsmlBrB` 2025-10 −$95.14.

Checked for double-counting: the matching `1A - Net Earnings:Resolutions` −$600.00 line is
`category='EXPENSE'` but does NOT match the `%Owner Expenses%` filter, so it never reaches the
statement. The refund is deducted exactly once, in the Guesty net.

## payment_structure.xlsx is the SOURCE OF TRUTH for the fee model

`config/payment_structure.xlsx` (one row per channel, formula cells) drives every number in
`payment_model.py`. **When the owner says they edited the sheet, re-read it and update
`payment_model.py` to match** — never invent or leave a stale rate. Current transcription:

- **Channel fee rates**: Airbnb 15.5, Airbnb-Hawaii 15.5, HomeAway 5, Expedia 18, Booking 15,
  Hopper 14, Marriott 18.5, **Trip.com 15** (was 11.1 — owner updated the sheet 2026-08),
  Whimstay 5, Blueground 7%. Manual/bnbFinder none.
- **Channel fee base — two forms.** For HomeAway / Expedia-group / Booking / Trip.com the
  InvoiceItem INCLUDES the fee, so it is the plain `(InvoiceItem − tax) × rate` (they are also
  the ones that subtract it again in net revenue). For **Airbnb, Airbnb-Hawaii, Hopper,
  Marriott, Whimstay, blueground** (`CF_OUTSIDE_II`) the sheet's column C ends in
  "− channel fee": the InvoiceItem is already NET of the fee, so it has to be grossed up —
  `cf = (InvoiceItem − tax) × r/(1−r)`, which is literally how sheet rows 10/12/13 write it.
  Verified against Guesty's own reported CHANNEL COMMISSION: the ratio is exactly `r/(1−r)`
  for each (Airbnb .18343, Hopper 1/.86, Whimstay 1/.95, Marriott 1/.815). `expedia_pcm` is
  excluded — it carries its own equivalent gross-up, `(accommodation + markup)/82% × 18%`.
- **`Airbnb_Hawaii` (sheet row 3) is its own row group**, split from mainland Airbnb by
  `tot_tax > 0.01` (Keaau 15-1542): the Hawaii tax IS in the host ledger and INSIDE
  InvoiceItem. Two consequences — **Guest pay = `InvoiceItem + channel fee` with NO `+ tax`**
  (mainland row 2 is the only "+ tax" row; adding it for Hawaii double-counted the tax, e.g.
  HMJ9P9WMKS showed $1,744.64 instead of $1,503.50), and the **Tax column shows the real
  ledger tax**, not the property-rate re-derivation mainland Airbnb needs. Re-deriving it
  both overstated the tax and disagreed with the tax net revenue actually subtracts.
- **Mainland Airbnb display tax** = `((InvoiceItem − ARC) + channel fee) × property_rate` —
  the base INCLUDES the channel fee, so a wrong channel fee silently skews the tax too.
- **Guesty fee** = `InvoiceItem × 1%` (HomeAway/Expedia/Manual/bnbFinder only).
- **Stripe fee** = `InvoiceItem × rate + (successful transactions) × $0.30` (host/Guesty-collected
  only). `transactions` = count of `money.payments` with `status=='SUCCEEDED'` (deposit + balance
  each incur their own $0.30). Threaded from the API as `n_transactions` in the summary frame
  (`fetch_month.build_summary_frame`); rows lacking payment data (deactivated UI merge) fall back
  to 1. (Owner updated 2026-08 from the old tiered `200×3.05% + (II−200)×2.9% + $0.30` — the
  first-$200 3.05% premium and the single flat $0.30 both went away.)
- **The Stripe RATE depends on the card's issuing country** (owner, 2026-08-30): **2.9% US,
  **4.4%** non-US** (2.9% + Stripe's 1.5% cross-border surcharge). The per-transaction $0.30 and
  the Guesty 1% are unchanged. Proven against QuickBooks' own `Credit Card Fees` bills across all
  20 months — QBO folds the Guesty 1% into that line, so the true Stripe charge is
  `bill − guesty_fee`; fitting `base × rate + n × $0.30` to 1,526 billed bookings gives **1,438
  exact at 2.9%, 24 exact at 4.4%, and nothing in between**. Worth $482.23 over 20 months
  (~$289/yr); all 24 now tie QBO to the cent.
  **Guesty does not report the card country**, so `payment_model` detects it two ways:
  `STRIPE_INTERNATIONAL_SOURCES` — the non-US HomeAway storefronts (`HomeAway CA/DE/UK`), where
  15 of 16 bookings are international — plus `STRIPE_INTERNATIONAL_CARD`, an owner-verified set of
  9 confirmation codes for foreign guests who booked through a **US** channel (VRBO, BE-API),
  which nothing in our data distinguishes. `STRIPE_DOMESTIC_CARD` is the reverse override (one US
  card on the Canadian site). **Add new codes to the set** as they surface; find them by comparing
  `(QBO Credit Card Fees bill − guesty_fee)` against `InvoiceItem × 2.9% + n × $0.30`.
  Recorded in `config/payment_structure.xlsx` (column H on every Stripe row + the legend at B17/B18).
- **Stripe does NOT return its fee on a refund** (owner, 2026-08-30) — the same principle as
  VRBO's channel fee. The card was charged the full amount, so the base is the **gross charged**
  (`total_paid + total_refunded`), not an InvoiceItem QuickBooks later re-cut down. It only bites
  when QBO DID re-cut: for a refund it left alone, InvoiceItem already IS the gross (`HA-G5Zh9AS`:
  II 680.63 = paid 611.63 + refunded 69.00), so the rule is a no-op there. `HA-ADSOY1B` 2026-07 is
  the shape: gross $1,349.44 × 2.9% + 2×$0.30 = **$39.73 = QBO's bill**, against an InvoiceItem of
  only $945.47.
  **Scope it to REFUNDED rows.** A handful of clean bookings carry `total_paid` slightly above
  InvoiceItem (an overpayment, or an invoice that shrank) and QBO bills those on InvoiceItem —
  `HA-dCtCqhs` is $126 apart. Unscoped, the rule contradicts 8 QBO bills; scoped, it contradicts
  **zero** and fixes 28 ($253.25).
- **A booking paid outside the card rails carries NEITHER processing fee** —
  `payment_model.NO_PROCESSING_FEE_CODES`. Guesty reports a direct booking as `manual`, which is a
  `has_processing` group, so the model bills it the Stripe fee AND the Guesty 1% by default; when
  the guest actually paid by bank transfer / direct collection, neither is charged. **One set gates
  both fees** (`has_processing` feeds `guesty_fee` at line 310 and `stripe_fee` at 311, and the
  cancel branch at 368/369), so a single code entry removes the pair. Owner-verified per
  reservation — Guesty does not report the payment method, so there is nothing to detect on.
  Today: `GY-CQiSs4Nu` (2026-07 hoodsport_26060, $12.64) and `GY-c2Q3YukP` (2026-07 yacinde_b6,
  Stripe $16.28 + Guesty $5.62 = $21.90). Both leave net revenue exactly `InvoiceItem − tax`.
  8 more candidates sit in the OPEN 2026-08 month; wait for its fee billing to finish before
  judging them. The durable fix is threading Guesty's payment method into the summary frame,
  which needs an API pull.
- **Both Stripe rules together tie 1,460 of 1,499 QBO-billed bookings to the cent (97.4%),
  residual +$194.08 on $49,507.55 (0.39%).** Three bookings the formula cannot reproduce are
  pinned to QBO's own charge in **`STRIPE_FEE_OVERRIDES`** (code, not CSV — see "EVERY manual
  adjustment lives in payment_model.py"), and still want investigating: `HA-PLHAdi3` (2025-04,
  $45.99), `HA-sebniGp` (2025-06, $176.75 — QBO $5.00 above 2.9% × gross), `GY-MuLckM3e`
  (2025-06, $190.14 — QBO's base ≈ $6,556, neither II $5,515 nor gross $9,041).
  Two more remain unfitted: `HA-5stSknh` (2026-06,
  −$56.79) and `HA-dsmlBrB` (2025-10, −$34.75). The other ~34 are bookings whose QBO card-fee
  bill nets to ≈$0 against the Guesty 1% (split or cross-month billing), not formula errors.
- **A refund does NOT return VRBO's channel fee** (owner, 2026-08-29). The host still pays 5%
  of the FULL booking, so `homeaway` keeps the whole `(InvoiceItem − tax)` channel-fee base even
  when refunded; only the base of net revenue itself (and the Guesty 1%) drops to `total_paid`.
  Verified against VRT: `HA-fT3tQ4a` (bellevue_2243 2026-07, $600 refunded) nets **$2,972.53**
  with the full base against VRT's $2,972.56 — we showed $3,002.53 before. The 3¢ is VRT using
  a $599.40 refund where Guesty's `TOTAL REFUNDED` says $600.00.
  Scope: **48 bookings / $804.03 of channel fee** across 20 months, and **every refunded booking
  carrying a channel fee is homeaway** — no other channel has one, so the "the platform returns
  its commission" branch that survives for the other channels is UNTESTED. Confirm per channel
  before trusting it.
  **QuickBooks confirms the rule independently**: its own `Channel Fees` bills match our
  `channel_fee` to $0.00 on all 6 refunded VRBO bookings it billed in 2026-07, including the two
  whose invoice it re-cut (`HA-ADSOY1B`, `HA-4OBbJHE`) — e.g. `HA-G5Zh9AS` (burien_14407_top,
  $69.00 refunded) is $30.25 = 5% x $605.00 on both sides. **Never read a QBO month-end
  commission credit as the platform returning its fee** — `HA-G5Zh9AS` carries a −$11.83 credit
  implying a $65.72 basis reduction against the $69.00 refunded, but the channel-fee bill on the
  same booking is the undiminished $30.25, so the credit is a manual bookkeeper adjustment whose
  basis is not reconstructible from the ledger. $0.59 of commission; ask, don't infer.
  A **CANCELED** VRBO booking is the opposite case and carries NO channel fee: QBO posts a $0.00
  `VRBO channel fee | Cancelled | <code>` bill on every one (6/6 checked), which is what
  `payment_model`'s cancel branch already does. Cancel ≠ refund — the reservation is voided and
  VRBO's commission with it, whereas a partial refund is a stay that happened.

  **A code fix here does NOT reach the statements — `payment_breakdown_<p>.csv` is the stored fee
  model and only a `fetch_month` API pull rewrites it.** With 5 Guesty tokens/24h, re-pulling 20
  months is impossible, so this rule shipped in code but stayed wrong in 21 bookings across 13
  periods until the CSVs themselves were corrected in place (`channel_fee = 0.05 x (II − tax)`,
  `net_revenue` down by the delta — deterministic, byte-identical to what a re-pull writes).
  Channel fee +$167.96 / owner payout −$143.04 across 22 statements. **After ANY payment_model
  change, audit every period's stored CSV — do not assume the code change landed.**
- **Net revenue** subtracts the channel fee for **HomeAway, Booking, Expedia-group, Trip.com**
  (`cf_in_net`) — their Invoice Amount *includes* the channel fee and the host pays it back.
  Owner confirmed the sheet's Expedia-group "hostServiceFee" term MEANS this 18% channel fee
  (do NOT swap in the API's `money.hostServiceFee`).

## The Net Revenue section (Section 2) IS the Booking Breakdown + commission

Owner decision 2026-08-29. Section 2 used to be re-assembled from the guesty INCOME ledger
rows plus the LTR CSV — which silently omitted **booking source #3** (QBO-recorded rental
income and Hipcamp/OsbrRV) even though the engine commissions it. `kirkland_8017` 2026-06
listed 4 bookings totalling $8,834.75 under an $11,910.35 payout, with the $6,115.00
`GY-cN8BWajQ` booking visible only in Section 1. Affected 5 statements in 2026-06 and 2 in
2026-07.

**`booking_breakdown.net_revenue_rows(by_unit, pm_fee_rate, cleaning_by_code, tax_by_code)`**
is now the ONE builder: the last column of a Section-1 row (`Net Rental Revenue`) is the first
column of its Section-2 row, and commission is applied to it. `run_month_close` (Excel) and
`dashboard` (web + PDF) both call it, so the two sections cannot drift.

Three things it has to keep doing:

- **Round commission PER LEDGER LINE, not per displayed row.** A merged add-on (a pet fee, the
  `elektra_1413` parking fee) is its own ledger line, and `netrevenue.engine` rounds per line.
  Rows carry the hidden `_parts` list for exactly this; `_merge_addon` appends to it. Rounding
  once on the merged net drifts a penny from the stored PM fee. `excel_writer` therefore uses
  the supplied `commission` when present instead of recomputing it.
- **`engine` rounds rent per line too**, not grouped per listing — the statement now shows one
  row per BOOKING, and every rent line (an LTR CSV row, a QBO rent deposit, an add-on) is
  exactly one row.
- **`cleaning_by_code` / `tax_by_code` stay caller-supplied**, NOT read off the row's own
  Cleaning Fee / Tax cells: those show what the GUEST paid, while the owner-facing columns are
  what the ledger credits the owner (`_apply_owner_cleaning_credit`, `%Taxes Paid to Owners%`),
  which is what `amount_due` is built from.

Rows also carry hidden `_guest` / `_in` / `_out` so Section 2 can render Guest Name and
Reservation Dates. A source-#3 row has no stay dates — its posting date fills both, and the
renderers show a single date rather than a range.

**Every record handed to `build_by_unit` must carry the dates**, including the dashboard's own
LTR display shape. `dashboard.build_ltr_records` formats a `"Reservation Dates"` STRING and used
to discard the raw `checkin`/`checkout`, so `ltr_row`'s `_pick("checkin", "_in")` fell through to
`""` and Section 2 died on `strptime('')` — **21 statements across 2026**, every one holding an
LTR or DEFERRED row (Beachwood, Mercer 2449, Seattle 11331, Microsoft 14645-C19 …), while pure
Guesty statements rendered fine and hid it. The display record now sets `_in`/`_out`/`_guest`
next to `_net`/`_cleaning`/`_code`, and `dashboard.py`'s Net-Revenue loop parses dates through a
local `_d()` that returns None instead of raising — a display-only field must never be able to
take a whole statement down.

`build` now also SWEEPS statement files it did not write this run (it only ever added them
before), so a scope change stops leaving a stale statement behind: `mercer_3627_adu` 2026-07 sat
at $574.63 against stored totals of $0.00.

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

## Booking breakdown covers ALL THREE sources (Guesty + LTR + other rental income)

The Section-1 booking breakdown is built from every booking source (owner's pull-steps model:
pull Guesty → pull LTR → dedup → make booking breakdown → create revenue sections), which is
what makes Section-1 "BK Net Rental Revenue" equal Section-2 "Net Rental Revenue" in every
period. Guesty rows come from `payment_breakdown_<p>.csv` (the fee waterfall); **LTR rows are
fee-free**: channel label
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

`Cleaning.Fee` is populated from 2026-05 on (that CSV carries $1,757 across four rows). The credit
is posted for `Source=='LTR'` rows only — a **DEFERRED** row's cleaning fee gets no OWNER_ADJ,
which is correct while those listings are `owner_pays_cleaning=False` (the fee is Valta's income,
not the owner's). If a DEFERRED row ever lands on an `owner_pays_cleaning` listing, that credit
has to be added: `import_ltr` DELETES the guesty row, so the STR `_apply_owner_cleaning_credit`
(which sums **guesty** rows) cannot see it either.

### A unit that goes long-term needs an LTR CSV row — QBO alone will NOT bring it in

QBO books a long-term lease as a **Purchase** to
`Trust Liabilities:Owner Payables:1B - Long term rent - owner` (the cash payout) plus a monthly
commission **Bill**. Both are `category='EXPENSE'`, and neither account matches the statement's
`qbo_account LIKE '%Owner Expenses%'` filter — so a unit that switches from STR to LTR simply
**disappears from the statement** (no Guesty booking either) while QBO keeps commissioning it.
`osbr_8` (Cottage 8) went long-term Dec 2025 – Mar 2026 and lost 4 months of rent that way
($6,041.50 revenue / $966.64 commission). The fix is an LTR CSV row per month, then
`import_ltr` + `build`.

Two things make it reliable going forward:

- **Recognize the rent QBO commissioned, not the cash it paid out.** `1B` payouts are weekly and
  slip across month ends (Cottage 8: $1,776.90 paid in Dec for $1,421.50 of Dec rent), so the
  basis is the commission Bill ÷ the PM rate — which makes our commission tie to QBO to the cent.
- **Find them with this sweep** — any property/period with `1B - Long term rent - owner` activity
  but no `category='INCOME'` line is a hole:

```sql
SELECT property_id, substr(posting_date,1,7) p, ROUND(-SUM(amount),2) rent_paid
FROM ledger_lines WHERE qbo_account LIKE '%1B - Long term rent - owner%'
GROUP BY property_id, p
-- ...LEFT JOIN the same table grouped on category='INCOME' AND include_in_statement=1
-- and keep the rows where income is 0.
```

**`ltr/records.to_property_id` is the ONE Listing-label -> property_id map**; `ltr/import_ltr`
imports it rather than keeping its own copy. It used to be two copies and only one knew
`"Cottage N" -> osbr_N`, so Cottage 8's rent was commissioned in Section 2 but had no Section-1
row and `osbr_summary` failed its BK == Net Rental Revenue assertion. Add a new label in
`records.py` only.

**Source #3 — `reporting/other_income.py`**: rental income with NO payment-breakdown row, which
used to be in gross revenue but invisible in Section 1 (the "BK Net Rental Revenue" vs "Net
Rental Revenue" gap). Two kinds: synthetic Guesty income whose code never came back from the API
pull (the Hipcamp `source_object='OsbrRV'` bookings on `osbr_rv`), and QBO-recorded rent (Lease
Rent deposits, tenant Zelle, pet fees, extra nights). Rendered fee-free like LTR. Which QBO rows
qualify is **`common/income_rules.py`** — the SAME `RENT_PRED` the engine commissions on, so
"earns commission" and "gets a breakdown row" are one rule; `NON_RENTAL_PRED` (refundable
deposits, utility reimbursements) wins over it and lands in owner_adjustments instead, leaving
the payout unchanged. `income_rules.py` is import-free so it ships in the display-only deploy
bundle. A record whose confirmation code matches an existing row is MERGED into it
(`_merge_addon`) — a pet fee belongs in that booking's Net Rental Revenue, not on its own line.

### `(legal)` QBO income is not the owner's money — it leaves the statement entirely

A fourth bucket sits ABOVE the other three: `income_rules.NOT_OWNER_PRED` — QBO INCOME whose
description starts with `(legal)`, the marker the bookkeeper puts on Valta's own legal /
insurance matters. Unlike `NON_RENTAL_PRED` (which only reclassifies owner money as an
adjustment and leaves the payout untouched), these are not the owner's money in any form:
left alone they fall through to the uncommissioned "Other Credits" bucket, which still adds
them to gross revenue AND to the payout. `run_month_close._drop_non_owner_income` marks them
`include_in_statement=0` each build, which drops them from gross revenue, the Booking
Breakdown, `amount_due`, dashboard, Excel, PDF and end_balances at once (commission is
unaffected — they were never in the base). One line matches today: `seattle_1117` 2026-07,
$99.36, which the owner was being paid.

It does NOT un-suppress first, unlike `_drop_tiny_cancellations`: that function owns an
identifiable set (the period's guesty INCOME) it can reset, whereas the only handle here is
the predicate itself, and resetting on the shared `source_txn_id` would release rows
`_exclude_duplicate_guesty_deposits` suppressed on the same QBO transaction. **Narrowing the
predicate needs a one-off UPDATE to bring the rows back.** It is self-healing after a re-sync
(qbo-sync re-inserts with `include_in_statement=1`; the next `build` takes it out again).

Keep it narrow. Insurance claims that ARE the owner's — `kirkland_8017` "Damage protection
sofa replacement" $989.60, `elektra_809` "washing machine insurance claim" $353.05 — pay for
the owner's own property and stay in. `(legal)` is the only signal separating them; ask the
owner before widening it.

### Net Rental Revenue IS the commission base — non-rent QBO income leaves it

Owner principle 2026-08-28: **Net Rental Revenue is the sum of the per-booking net revenues
(STR + LTR + other rental income), and that sum IS what commission is charged on.** It used to
be `guesty + ALL qbo INCOME - non_rental_credit`, so an insurance payout, a garage rental, a
tenant reimbursement or a cleaning re-bill sat inside "Net Rental Revenue" earning no
commission and carrying no Section-1 row — which is what every "BK != Net Rental Revenue" gap
actually was. `netrevenue.engine` now computes

    non_rental_credit = other_income - commissionable_other
    gross_rev         = guesty_rev + commissionable_other

so the residual lands in `owner_adjustments` instead. **The payout is unchanged to the penny**
(both terms feed `net_before_reserve`), and Section 1 can no longer disagree with Section 2 —
all 20 periods reconcile at max gap 0.00. This SUBSUMES `NON_RENTAL_PRED`; the constant stays
only because `RENT_PRED` inlines it as a guard.

The cost of the inversion is that a genuine rent line `RENT_PRED` fails to match now silently
loses its commission instead of surfacing as a Section-1 gap. So `build_statements` PRINTS the
residual per statement — read that list every close; a name that does not look like a credit is
rent the predicate is missing. 2026-07 moved $7,217.95 across 4 statements (beachwood $4,512.41
reimbursement, edmonds_7819 $1,150.00 demolition + cleaning re-bill, bellevue_2243 $977.71
garage rental, seattle_906 $577.83), 2026-06 $3,205.19 across 3.

**Long-term rent is NOT at risk here: the owner supplies it as an `LTR_<p>.csv` row**, which
`import_ltr` posts as `source_object IN ('LTR','DEFERRED')` — the first `RENT_PRED` clause.
Never try to infer a lease from a recurring QBO deposit; `DEPOSIT ID NUMBER XX8484` carries
nothing to match on, and guessing would double-count the CSV row.

`'%extended stay%'` joined `'%extra night%'` / `'%additional night%'` in `RENT_PRED` at the same
time (redmond_15357, $266.67/month) — a stay extension is rent and earns commission.

### `BOOKING_CHANNELS` is prefix-anchored, so a channel alias silently falls to bucket 3

`_CHANNEL_CLAUSE` matches `lower(vendor_customer) LIKE '<channel> - %'`. QBO writes Marriott
add-ons as `homesvillasbymarriott - <guest> - <code>`, which `'marriott - %'` does NOT match —
so 13 lines / $5,189.22 land in "Other Credits": in gross revenue, uncommissioned, and with no
Section-1 row (they cannot reach `_merge_addon` either). `elektra_1413` 2026-07's $105.00
parking fee is one of them. Adding the alias makes that money commissionable, so it is an
owner decision, not a bug fix.

## Why "BK Net Rental Revenue" != "Net Rental Revenue" (Section 1 vs Section 2)

They are the same money counted two ways and MUST match — the summary sheets assert it. When
a row fails, it is one of these, in descending order of how common it is. Portfolio-wide the
gap is **3.1% of revenue over 20 months, in 69 of 1,438 property-months**:

1. **Non-rent QBO credits — BY DESIGN, ~84% of the gap (62 rows).** Section 2's
   "Net Rental Revenue" is the engine's `gross_booking_revenue` = `guesty + ALL qbo INCOME −
   non_rental_credit`. Section 1 lists **bookings**. So an insurance-claim deposit, a sales-tax
   reversal, an inter-company transfer or garage/parking rent lands in gross revenue with no
   breakdown row (`income_rules.py` bucket 3 — uncommissioned, shown under "Other Credits").
   Not a bug; the label just reads wider than the section. NOTE when writing diagnostics:
   `vendor_customer IS NULL` makes `RENT_PRED` evaluate to **NULL, not 0** (the channel clause
   is `lower(vendor_customer) LIKE 'airbnb - %'`), so bucket a row with
   `COALESCE((RENT_PRED),0)=0` or SQL three-valued logic will silently mis-classify it.
2. **Orphan DEFERRED rows — a missing input file (2026-05, $16,149.78).** Section 1 renders
   LTR/deferred from `inputs/<p>/LTR_<p>.csv`; Section 2 reads the LEDGER. The ledger keeps
   `source_object='DEFERRED'` rows forever, so deleting the period's CSV strands them: the money
   stays in the statement, the breakdown row disappears. **Never delete an `LTR_<p>.csv` whose
   rows were imported.**
3. **An LTR CSV that was never imported** (2026-05). Section 1 renders LTR rows straight from
   `inputs/<p>/LTR_<p>.csv`, while Section 2 reads the LEDGER — so a CSV sitting on disk that
   `import_ltr` never ran on shows its rent in Section 1 *and* the raw QBO rent deposits it was
   meant to replace, i.e. a NEGATIVE gap (Section 1 > Section 2). Check every period with
   `SELECT source_object, COUNT(*) ... WHERE source_object IN ('LTR','DEFERRED')` against the
   CSV's row count; `import_ltr --period <p>` then `build` fixes it.

**Owner rule (2026-08-28): if the LTR CSV carries the booking, the LTR record wins.** Two fixes
implement it, both at the shared layer:

- **`ltr.records.guesty_code_checker(conn, period)` is the ONE `is_guesty_code` predicate**, used
  by `run_month_close` (Excel), `dashboard` and `reporting.monthly_summary`. It asks the LEDGER
  (not the payment-breakdown CSV — `import_ltr._replace_colliding` DELETES the guesty row a
  DEFERRED row supersedes, so the code outlives it in the CSV) and it is **period-scoped**,
  mirroring the filter `ltr_claimed_codes` already applies on the other side of the same dedup.
  Unscoped, a stay recognized month by month (`GY-LsV362As`: guesty row in 2026-04, DEFERRED rows
  in 05/06/07) matched the April row and was skipped in EVERY later month.
- **`import_ltr._replace_colliding` matches the code in `vendor_customer` too.** QBO Deposits key
  on the QBO transaction id and carry the confirmation code in Guesty's
  `"<channel> - <guest> - <code>"` customer string, so matching `source_txn_id` alone missed them
  and one booking was counted twice (bellevue_1326 2026-07: a $9,112.25 DEFERRED row AND a
  $6,519.38 QBO deposit for `HMPA8BRAYA`). Guesty rows are DELETED; QBO rows are **suppressed**
  (`include_in_statement=0`) because qbo-sync would re-add a deleted one — and the importer
  un-suppresses first, so it stays idempotent. It now runs for LTR rows as well as DEFERRED ones.

## A cancelled/refunded booking QBO ADJUSTED is recognized on the QBO record

Owner decision 2026-08-28: **when QuickBooks has adjusted a cancelled or refunded booking, the
QBO record wins** over the Guesty payment-breakdown net. A cancellation after check-in is not the
channel's flat fee — the owner keeps part of the stay, the rest is refunded, and Valta re-cuts the
numbers in QBO at month end. `HA-xdDv3ud` (Bellevue 2243, 2026-07): guest paid $1,053.17 of a
$5,419.74 booking after a $4,366.57 refund; Guesty nets $1,011.50, QBO recognized **$772.68** and
billed $139.08 commission.

- **The commission Bill ÷ the PM rate IS the adjusted net** — `reporting/qbo_adjustments.py`
  (`adjusted_nets`). It is the only signal present on every adjusted booking: a few carry a full
  re-cut invoice set, most carry nothing but the `Management Commission | Cancelled | <code>`
  Bill. Dividing it back out recovers QBO's exact basis and makes our commission tie to QBO to
  the cent — the same technique the LTR importer uses for long-term rent.
- A **$0.00** commission Bill is NOT an adjustment: that is how QBO records a cancellation it
  charged nothing for, the $6.10 residues included.
- `run_month_close._apply_qbo_cancellation_adjustments` applies the map to the **guesty INCOME
  `amount`** (so stored `amount_due`, Net Revenue, commission, dashboard, Excel and PDF all
  follow); the SAME map goes into `build_by_unit(..., net_overrides=…)` so Section 1 keeps
  footing. Runs AFTER `_apply_payment_breakdown_net` — it overrides that net.
- **The adjusted Section-1 row shows Guest Pay as the QBO-recognized gross** (`net + fees +
  cleaning + tax`), not the raw guest payment, and leaves the fee components alone. Excel and the
  dashboard break Fees out into Channel/Guesty/Stripe columns, so absorbing the difference into
  the folded `Fees` value alone leaves those tables not adding up; and inventing a split across
  the fee columns would assert a breakdown QBO never recorded.
- Impact across 20 months: 49 property-months, **+$2,222.45 revenue / +$451.47 commission /
  +$1,770.98 owner payout**. Most are pennies; the movers are poulsbo_563 2025-08 (+$2,061.71),
  keaau 2025-02 (+$676.23), burien_14407 2025-07 (+$461.47), shelton_310 2026-02 (−$245.02),
  bellevue_2243 2026-07 (−$238.83), seattle_1117 2026-06 (−$226.52).
- The build prints two warnings you should act on rather than ignore: adjusted bookings with **no
  Guesty income row** (7 across the history — the booking never came through the API pull, so
  there is nothing to adjust and the revenue is simply absent), and adjustments **skipped for a
  missing PM rate** (5 properties: bothell_11131, mercer_3627_main, redmond_16012, seatac_12834,
  seattle_1512).

### `import_ltr._replace_colliding` must scope its DELETE to the period

It deletes the guesty row a DEFERRED row supersedes. Unscoped, importing month N wiped the
ORIGINAL Guesty booking wherever it lived: importing 2026-05 deleted `GY-LsV362As` from 2026-04
and took **$14,772.94** of Seattle 9021's April revenue with it, plus `HA-CmP0Mb2`,
`HMPWR32NZB` and `GY-4Ra24Nat` from 2026-03/04 — silently, because the loss only surfaces on the
NEXT build of the robbed period. Recovery is a `guesty-import` of that period (delete its guesty
INCOME rows first). Verify with: every `booking_id` in `inputs/<p>/guesty_converted.csv` should
have a ledger row in `<p>`, except codes a DEFERRED row legitimately replaced **in that same
period**.

## `qbo_sync` keeps QuickBooks' line sign — never `abs()`

QBO Bill/Purchase/Deposit lines carry a meaningful sign: a POSITIVE line debits the account
(a charge), a NEGATIVE line credits it. `qbo_sync` stored `-abs(amount)` for Bills (and
`abs(amount)` for Deposits), collapsing both into a cost. Fixed 2026-08-28 — all three
handlers now store `-amount` / `amount` and keep the sign. (The JournalEntry handler already
did the right thing via `posting_type == "Debit"`.) Two things this was breaking:

- **Owners were charged for credits Valta had absorbed.** The `garbage collection` bills post
  `1C - Owner Expenses:Other Owner Expenses -100.00` against `0_Employees 1099:Wages -
  Cohost/PM +100.00` — a credit TO the owner. `-abs()` billed it as a $100 cost instead, i.e.
  a $200 swing per property per month (mercer_3925, seattle_1117, seattle_9021 in both
  2026-06 and 2026-07: +$1,200 owner payout once fixed). Normal owner expenses are POSITIVE
  in QBO, so they are unaffected — only 8 lines in two months flipped.
- **Nothing downstream could reconcile commission against QuickBooks**, because a commission
  credit and a charge looked identical.

**Two traps when re-syncing a period after this change:**

- **`reporting/qbo_adjustments.py` had to flip with it.** On `Management Commissions Revenue`
  QBO writes a CHARGE negative, so with the sign preserved the charge lands POSITIVE in the
  ledger and the basis is `+commission / rate` (it was `-commission / rate`). Missing this
  turned every QBO-adjusted booking's revenue NEGATIVE — a $11,597 gross swing over two
  months before it was caught. A net-negative group is a pure credit, not a re-cut basis, and
  is skipped.
- **`qbo-sync` DELETEs the period's `source='qbo'` rows, which includes the LTR/DEFERRED rows
  `import_ltr` posted.** Always re-run `import_ltr --period <p>` after a re-sync, then `build`.

`booking_fees.get_booking_fees` is unaffected: `_apply_guesty_fees` takes `abs()` of whatever
it returns, and `channel_fee_deduction`/`stripe_fee_deduction` have no live consumers. So are
`%Taxes Paid to Owners%` (Invoice-sourced), `%Resolutions%` (JournalEntry/Purchase) and
`%Owner Expenses%Cleaning%` (all-negative). **Prove it the same way if you touch this again:
re-sync with the OLD code first to snapshot the totals, then with the new one — otherwise
QuickBooks' own edits since the last sync get blamed on the code change.** That drift was
real here: an unrelated re-sync of 2026-06/07 moved gross by -$4,412.58 on its own.

## Exception bookings: `reporting/exception_bookings.py` (refunds / adjustments / cancels)

    python -m src.reporting.exception_bookings --period 2026-06 2026-07   [--no-qbo]

One row per booking that is NOT a clean, fully-paid stay -> `output/exception_bookings.{csv,xlsx}`.
Flags: FULL/PARTIAL REFUND, RESOLUTION, QBO CANCELLED, QBO REFUND, CANCELED W/ INCOME, DROPPED,
DEFERRED, NO LEDGER ROW. It carries our commission next to QuickBooks' own, per booking.

Three things it exists to encode, all of which cost real time to work out:

- **`payment_breakdown`'s `refund` column is NOT the refund.** It is `-airbnb_resolution_center`
  (ARC), display-only, and `payment_model` never subtracts it from net. The actual refund is
  `TOTAL REFUNDED`, which survives only in `inputs/<p>/guesty_converted.csv` (`refund`). A
  report that reads the breakdown's column alone will call HA-fT3tQ4a un-refunded when the
  guest got $600 back, and call HMNDEMPHQ8 refunded when it only had a $478.35 resolution.
- **QBO writes commission in THREE description forms, and only one is an adjustment.**
  `Management Commission | Cancelled | <code>` is the re-cut charge (the check-in-dated bill
  is $0.00) -> `qbo_adjustments.adjusted_nets` uses it. `Management Commission | Refund |
  <code>` and `… Deduction | <code>` are CREDITS on a bill already charged; our net already
  nets the refund and the fees, so adding them as adjustments would double-count. A plain
  `Management Commission | <code>` re-billed at month end is also a credit — verified against
  QBO: bellevue_2243 July ties to $1,871.99 vs our $1,877.40, the $5.41 being an 18% slice of
  a $30 channel-fee difference between Guesty and QBO.
- **`qbo_sync` used to store every Bill line as `-abs(amount)`, which DESTROYED the
  charge/credit sign — fixed 2026-08-28 (see the next section).** Until a period is
  re-synced its ledger still carries the collapsed sign, so this module reads commissions
  through a live read-only Bill query, which is correct for every period; `--no-qbo` blanks
  those columns. Inferring the sign from bill order instead FAILS (623 of 1,595 bookings,
  some yielding negative commission) — don't try.

**A DEFERRED booking cannot tie to QBO in a single month.** QuickBooks commissions the whole
stay in the check-in month; we recognize it month by month, and `import_ltr._replace_colliding`
deletes the Guesty row, so the booking looks absent from the period. The report matches those
via the confirmation code in the QBO customer string and flags them DEFERRED rather than
NO LEDGER ROW (bellevue_1326 `HMPA8BRAYA`, microsoft_14645_c19 `HM5HY3DSXK`). Only a genuine
NO LEDGER ROW is missing revenue.

Verified over 2026-06 + 2026-07: 669 bookings compared against QBO's true-signed commission,
85% tie to the cent, portfolio gap +$36.58 on $123,914 (0.03%).

## Cancellation residues leave the statement (canceled + InvoiceItem <= $6.10)

VRBO/HomeAway keeps a flat **$6.10** out of a refunded $200 deposit when a guest cancels,
so the booking survives the Guesty pull as a ~$5.56 "stay" the owner never earned. Owner
decision 2026-08-28: those rows leave the statement **entirely**, not just Section 1.

- The rule is ONE definition — `reporting/booking_breakdown.DROPPED_CANCELLATION_MAX_INVOICE`
  + `is_dropped_cancellation` / `dropped_cancellation_codes`. `build_by_unit` filters Section 1
  on it, and `run_month_close._drop_tiny_cancellations` marks the matching **guesty INCOME**
  ledger rows `include_in_statement=0` — which is what drops them from gross revenue,
  commission, `amount_due`, the dashboard, Excel, PDF and end_balances at once. Change the
  threshold in `booking_breakdown.py` only; the module ships in the deploy bundle.
- `_drop_tiny_cancellations` **un-suppresses first**, so it is idempotent and a lowered
  threshold (or a booking that leaves the CSV) restores the row on the next build.
- **`NON_COMMISSIONED_BOOKING_CODES`** (same module) is the OTHER owner override, and the two
  are not interchangeable: a DROPPED booking leaves the statement entirely, a
  non-commissioned one **keeps its revenue and is simply charged nothing**. `netrevenue.engine`
  drops those lines from the PM-fee sum AND from `commission_base` (so a statement whose whole
  revenue is non-commissioned no longer trips the "no PM fee rate configured" guard over a fee
  that would be $0), while `gross_rev` is untouched; `net_revenue_rows` renders the row's
  commission as $0.00. Both read the ONE set, so the section commissions still sum to the
  stored PM fee exactly. `HMRN4KYMAB` (kirkland_13070 2026-07, $76.05) is the first —
  QuickBooks bills no commission on it either (no `Management Commission | HMRN4KYMAB` line
  exists), and removing ours took Kirkland 13070 from a $13.69 VRT difference to an exact tie.
  NOTE `engine` imports the constant from `reporting.booking_breakdown` — a mild layering
  inversion, accepted because that module is deliberately dependency-free and is already the
  single source of the drop rule `run_month_close` reads.
- **`DROPPED_BOOKING_CODES`** is the owner's escape hatch for a residue ABOVE the threshold —
  a per-reservation set, NOT a raised threshold. `HA-LILLEKS` (elektra_1212 2026-07) is the
  first: an $1,848.27 booking cancelled with $1,794.07 refunded, leaving VRBO's retained
  $54.20. QBO bills $0.00 of commission, channel fee and Stripe fee on it, so QuickBooks
  agrees the owner earned nothing; VRT agrees too — removing it took Elektra 1212 from a
  $43.77 difference to an exact tie. Raising the threshold to $54.20 instead would have swept
  in the $50.00 booking sitting just under it.
- **The threshold is a value, not a shape.** 107 bookings across 20 months qualify
  (−$588.72 revenue / −$96.86 commission / −$491.86 owner payout); 106 of them are exactly
  $6.10. The next canceled booking up is **$50.00** and Airbnb cancellations run into the
  thousands, so there is a wide gap under it — raising it starts deleting real money. Check
  that gap (`status=='canceled'` in `payment_breakdown_<p>.csv`, sorted by `InvoiceItem`)
  before touching it.
- QBO agrees: every `Cancelled` Bill for these bookings is $0.00, and the $200/−$193.90
  Stripe clearing pair sits in `Trust Assets`, which the `%Owner Expenses%` filter already
  excludes — so nothing else had to change.

## Central supplies is a SUBSTITUTION, not two blanket operations

`_apply_central_supplies` swaps a central property's QBO per-booking supply charges for the
formula fee (`0.9 × guests × min(nights, 60)`). Two rules keep the swap honest:

- **Suppress ONLY where a formula charge was actually generated.** A central property with QBO
  supply lines but no Guesty booking that month (an owner/timeshare stay, or a listing whose
  reservations never reached the API) would otherwise have its real cost suppressed with nothing
  put back — that silently deleted $1,665 of supply cost across 18 periods (`yacinde_f5` $43.20
  in 2026-07, `seattle_1424c` $83.70/$62.10). The build prints which properties kept their QBO
  cost; a name showing up there usually means a stay never reached Guesty.
- **Match the description whitespace-tolerantly** (`'Supplies Charge%|%'`). QBO writes both
  `Supplies Charge | …` and `Supplies Charge  | …` (two spaces); matching only the one-space form
  let the two-space rows escape suppression, so `yacinde_b1` carried BOTH its QBO supply cost and
  the formula charge for the same stay.

## Repairs and Maintenance are SEPARATE expense subcategories

QBO books them to two accounts (`…Owner Expenses:Repairs - Owner` /
`…:Maintenance - Owner`) and the owner reads them as different things, so
`config/mapping_accounts.yml` gives each its own subcategory (owner decision 2026-08;
they used to share one `Repairs & Maintenance` rule, which hid $94k of maintenance
inside $250k of repairs). Rule order matters — `(?i)repair` comes FIRST, so an account
naming both lands in Repairs. Every product that lists expense sections carries the
pair: `dashboard.all_categories`, `excel_writer.ORDER`, `reporting/osbr_summary.EXP_COLS`.
The PDF renders one flat expense table, so it needed no change.

The subcategory is stamped onto `ledger_lines` at qbo-sync time, so a mapping edit only
reaches NEW rows. `src/onetime/split_repairs_maintenance.py` relabelled the 1,387
historical rows in place through the SAME `apply_account_rules` the sync uses — far
cheaper than re-syncing 20 months from QuickBooks for a pure display change. It only
touches `subcategory`, so payouts are untouched: rebuilding all 20 periods afterwards
moved $0.00 across all 72 properties. **If the owner splits another account, do the
same: edit the rules, write a one-time relabel, then `build` each period** — a mapping
change alone leaves history mislabelled.

## Commands

Activate the venv first: `source venv/bin/activate` (or call `venv/bin/python …`).

Full month close (period = `YYYY-MM`):

```bash
# 1. Pull Guesty (writes Guesty_booking_<p>.csv + payment_breakdown_<p>.csv)
python -m src.breakdown.fetch_month --period 2026-07 [--deactivated-csv <ui_export.csv>]
#    It PRINTS the manual adjustments it re-applied; confirm every expected one is listed.
# 1b. Confirm the stored CSVs still carry every manual adjustment (no API token needed).
python -m src.breakdown.audit_adjustments        # exits 1 if any is missing on disk
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
python -m src.deploy_pack --period 2026-06 2026-07
# 9. Refresh the month-by-month summaries. BOTH read the STORED totals, so they go
#    stale silently — re-run after ANY rebuild. Each asserts BK Net Rental Revenue ==
#    Net Rental Revenue and prints the rows that fail.
python -m src.reporting.monthly_summary     # every statement -> output/all_properties_monthly_summary.{csv,xlsx}
python -m src.reporting.osbr_summary        # OSBR only       -> output/osbr_monthly_summary.{csv,xlsx}
```

### `reporting/period_sources.py` is THE loader for a period's booking sources

`run_month_close` (Excel), `dashboard` (web + PDF) and `monthly_summary` all need the same
six things for a month, and each used to assemble them itself — three hand-maintained copies
of "which bookings exist this month", which is precisely the drift the shared
`booking_breakdown` layer exists to prevent. `PeriodSources(conn, period, pids)` is now the
one definition:

| attr | what |
|---|---|
| `pb` | the `payment_breakdown_<p>.csv` frame (the stored fee model), or `None` |
| `codes` | every confirmation code that CSV covers |
| `ltr` / `ltr_covered` | booking source #2 — LTR/deferred records + the pids they cover |
| `other` | booking source #3 — fee-free rental income with no `pb` row |
| `claimed` | codes an LTR/DEFERRED row superseded (dedup; LTR wins) |
| `net_overrides` | QBO-adjusted nets for cancelled/refunded bookings |

**Load once for the whole portfolio, not per statement.** `build_by_unit` filters by `members`
itself, and every other field is keyed by confirmation code or property_id, so a portfolio-wide
load answers a per-statement question identically — verified over 502 statement-periods. The
Excel build used to re-read the same CSV once per statement (72x a month).

It ships in the deploy bundle (`deploy_pack.DISPLAY_PACKAGES`) and carries the
`try: from ..x import … except ImportError: from x import …` shim, so it imports the same way
as a package module and as a flat import under the Streamlit script. **Adding an import to it
that is not display-only would break the deploy dashboard** — keep it to pandas, `paths`, and
the display-only readers.

`ltr.records.build_records(period, members, is_guesty_code)` no longer takes the `base_dir`
first argument: `csv_path` had ignored it since the layout moved into `paths.py`, and the three
callers were passing three different values to a parameter that did nothing.

### The two summary sheets share ONE engine (`reporting/monthly_summary.py`)

`osbr_summary` is a thin wrapper that calls `monthly_summary.build(conn, periods, only=['osbr'])`
and renames the sheet — so the OSBR numbers and the all-properties numbers can never disagree.
Put new math in `monthly_summary`. It emits three sheets: **By property** (one row per statement
per month — the long table to pivot), **All properties** (those rows summed, one row per month),
and **Due by month** (`Amount Due to Owner` as a property x month matrix).

Two things it must keep doing, both learned the hard way:

- **Resolve scope PER PERIOD** via `allowed_property_ids(conn, root, period_start, period_end)`,
  the same call `build` makes — otherwise rollup members get their own rows and a retired
  listing keeps minting $0 rows forever (the sheet correctly goes 72 -> 71 statements in 2026-07
  when Kirkland 10219 winds down).
- **Pass `config.yml: statement_rollups` into `statement_rollups()`**, exactly like
  `run_month_close` and the dashboard. Calling it as `statement_rollups(conn, {})` drops the
  legacy members that have no contacts row (`beachwood_6`, `bellevue_14507_unit_*`), which left
  their rent out of the Booking Breakdown while the stored totals still counted it — 15 phantom
  Section-1 gaps.

Load the period's booking sources ONCE per period (`reporting.period_sources.PeriodSources`,
shared with `run_month_close` and the dashboard) and let `build_by_unit` do the member
filtering; per-statement re-reads would mean 1,400+ reads of the same CSV.

`build` writes new statement files but never deletes old ones, so after a scope change the
`output/<period>/statements/` folders keep stale per-unit files for listings that are now rollup
members. Sweep them (a file whose `property_id` is not in that period's `allowed_property_ids`)
before handing the folder to anyone.

First-time setup: `python -m src.run_month_close init-db` then `sync-mappings`. QBO OAuth runs
in the sibling project, which owns the shared token store:
`cd ../QBO_operations && python -m src.auth url`, then `python -m src.auth exchange --code <CODE>`
(tokens → `QBO_operations/config/secrets/qbo_tokens.json`, which this project reads).

## Deploy package (`deploy/` — dashboard-only, display-only)

A self-contained, **read-only** copy of the dashboard for hosting (Streamlit Cloud / Render),
mirroring `../owner_statement_mvp/deploy/`. The heavy pipeline runs locally; its result is
**baked in** and the deploy only reads + renders.

- **Layout mirrors the main project ROOTED AT `deploy/`** — `src/paths.py` is copied **verbatim**
  (its `parent.parent` resolves to `deploy/`), so the packaged `config/`, `inputs/<period>/`, and
  `db/` are found with **zero code change**. Contents: `streamlit_app.py` (host entry → runs
  `src/dashboard.py`), `src/{dashboard.py, paths.py, scope/{listing_filter,pm_rate}, ltr/records}`
  plus `src/{common/income_rules, reporting/{booking_breakdown, other_income}}`
  (all display-only — no pipeline / QBO / Guesty code, no secrets), `config/{mapping_classes.yml,
  config.yml, Listing_contacts.csv}`, `db/owner_statement.sqlite` (via `VACUUM INTO`),
  `inputs/<period>/{payment_breakdown, guesty_converted, LTR}`. `mapping_classes.yml` ships
  because the statement-only parents live there; `Listing_contacts.csv` because it IS the scope.
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
