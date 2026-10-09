# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A monorepo of analytics, accounting, and operations tooling for **Valta Realty**, a
vacation-rental property-management ("cohost") business. It is not one application —
it's a collection of largely independent scripts, reports, dashboards, and small
pipelines that turn data from **Guesty** (the property-management system / booking
source) and **QuickBooks Online (QBO)** into owner statements, payroll, revenue
reports, cleaning costs, and review analytics.

The code is majority **R** (~120 files: plain `.R`, R Markdown `.Rmd`, R Shiny) with a
growing **Python** side (~35 files: scripts, Jupyter notebooks, Streamlit apps). There
is no shared build system, test suite, or package manifest tying the tree together —
each subdirectory is run on its own.

## Data lives outside the repo

**Almost no data is committed.** `.gitignore` excludes all `*.xlsx/*.csv/*.pdf/*.png`
and Office files, so scripts read their inputs from absolute paths in the user's
Google Drive, e.g. `/Users/ylin/My Drive/...` and `/Users/ylin/Google Drive/My Drive/Accounting/...`.
When a script fails on a missing file, the fix is usually a Drive path, not repo data.
Model artifacts (`*.pkl`) and local databases (`*.sqlite`) are the rare committed
binaries. Don't assume you can reproduce a script's output from repo contents alone.

## Running things

Language dictates how to run — there is no top-level entry point.

- **R scripts / reports:** `Rscript "<dir>/Script.R"`; `.Rmd` reports render with
  `Rscript -e 'rmarkdown::render("<file>.Rmd")'`. Scripts assume the working directory
  and Drive paths above; expect to `setwd()` or run from the script's folder.
- **Python:** a shared workspace virtualenv exists at `/Users/ylin/ValtaWork/.venv`
  — `source /Users/ylin/ValtaWork/.venv/bin/activate` (has pandas/numpy/streamlit/
  scikit-learn per the root `requirements.txt`). Some sub-projects
  (`Claude_projects/*`) keep their own `requirements.txt`/venv.
- **Jupyter notebooks** (`.ipynb`) are run interactively / in VS Code.
- **Streamlit apps:** `streamlit run <path>/app.py`. The `.devcontainer` auto-launches
  `Cleaning/CleaningFeeApp/app/app_cleaning_fee.py` on port 8501.
- **R Shiny apps:** `GuestReviews/app.R` and `Valta_BookingManagement/` are Shiny;
  the latter deploys to shinyapps.io (account `alin-2025`, see `rsconnect/`).

## Sub-project CLAUDE.md files — defer to these

Active Python projects under `Claude_projects/` carry
their **own** CLAUDE.md with commands, architecture, and hard-won business rules.
**Read the local file before touching either — the business logic there is subtle
(channel-specific net-revenue math, PM-fee sourcing, idempotent rebuilds) and easy to
break:**

- `Claude_projects/Owner_statement_whole/CLAUDE.md` — QBO + Guesty API → monthly owner
  statements (SQLite pipeline, Excel/PDF output, Streamlit dashboard); the rules it
  inherited from the archived owner_statement_mvp are in its
  `docs/inherited_rules_owner_statement_mvp.md`.

**Shared things live in `Claude_projects/shared/`** (registry: `shared/README.md`):
`valta_common` (the Guesty client, fee rules `payment_model`, Wheelhouse client, shared
paths — `pip install -e shared`), `reference/` (Listing_contacts.csv, mapping_classes.yml,
listing_tax_rates.csv, maintained by Owner_statement_whole), `secrets/` (ONE `.env` and
ONE Guesty + QBO token cache for every project — this project's client uses them too),
and `data/` (`guesty.sqlite`, `wheelhouse.sqlite`, refreshed weekly by weekly_revenue).
Read the databases read-only instead of re-pulling when weekly freshness is enough.
Never copy a shared file into a project; import or read it from `shared/`.

Projects in `Claude_projects/_archive/` (owner_statement_mvp, GuestReview,
GuestyFinancials, wheelhouse_marketdata_download, …) are retired: never read, run or
import from them for active work — anything still needed has been copied into an
active project.

## Directory map

| Directory | Purpose | Stack |
|-----------|---------|-------|
| `Accounting/` | Financial statements, invoices, owner payouts (ACH), deferred revenue, monthly invoice migration | R |
| `ChildCompanyAccounting/` | Venmo transaction reformatting | R |
| `Claude_projects/` | Self-contained Python sub-projects, each with its own CLAUDE.md/venv | Python |
| `Cleaning/` | Cleaning-fee ML model + Streamlit app, dispatch cards, laundry cost, cleaner payouts | R + Python (scikit-learn) |
| `Data and Reporting/` | Revenue / supply / occupancy reporting, formatting records back into Guesty | Python + R + notebooks |
| `EmployeeCohostCompensation/` | Monthly reservation sheets & payroll per employee/cohost (year-partitioned folders `2023/2024/2025`) | R |
| `GuestReviews/` | R Shiny review dashboard, review formatting, issue detection | R |
| `OnboardingTemplate/` | Multi-step property-onboarding info templates built from PPT decks | R + notebook |
| `Valta_BookingManagement/` | Booking-management Shiny app (deployed to shinyapps.io) | R |

## Recurring domain vocabulary

- **Guesty** — the PMS; source of bookings, reservations, and reviews. Channels:
  Airbnb, VRBO, Booking.com, Expedia. Score scales and net-revenue formulas differ
  **per channel** — never treat channels uniformly.
- **QBO** — QuickBooks Online; source of expenses, fees, and taxes. QBO fee amounts
  are stored **negative** (costs).
- **Listing / nickname / Property** — a listing is keyed on its canonicalized
  `nickname` (trim, collapse whitespace, casefold), not the frequently-empty `Property`
  field — in the Guesty reviews file `Property` is filled on only ~589 Airbnb rows and
  null for all Booking/Vrbo/Expedia rows, so grouping by it drops ~88% of reviews.
  Parent/child listing relationships exist for occupancy/revenue rollups.
- **PM fee** — property-management commission; the authoritative rate is per-property
  in contract/mapping data, and hardcoded fallbacks elsewhere can drift.
- **OSBR** — a recurring reservation/booking report name across scripts.

## Conventions

- Scripts are written to be re-run (rebuild-from-source), not incrementally mutated;
  when a pipeline has a database, clear the relevant rows and rebuild rather than
  editing in place (this is called out explicitly in the sub-project CLAUDE.md files).
- Many R files share a sibling `Functions.R` / `*_functions.R` — check for one before
  reimplementing a helper.
- Files are often period-stamped (`...202605.R`, `-20260117.Rmd`); the newest date is
  usually the live version, older ones are kept for history.
