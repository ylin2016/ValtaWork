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

## Architecture

### Data Flow
```
Guesty CSV → convert_guesty_export.py → guesty_converted.csv
guesty_converted.csv + QBO data → run_month_close.py → SQLite
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
- `cmd_guesty_import()`: Imports CSV to ledger_lines
- `cmd_build()` (lines 231–252): Applies QBO fees using `abs()`, calculates PM fees, generates statements
- Must clear old guesty INCOME before each run

**dashboard.py**: Streamlit UI
- Property/period selector
- Net Revenue table (columns: Guest Name, Confirmation Code, Reservation Dates, Gross Revenue, Net Rental Revenue, Commission, Net Owner Revenue)
- Expenses by category (Cleaning, Supplies, Repairs, Utilities, Other) with column widths: Date 80px, Description large, Type medium, Amount 80px
- Summary: Starting Balance, Net Income, Current Balance, Owner Payout

**statement_engine.py**: Calculates property totals and PM fees
**excel_writer.py**: Generates Excel statements (font size 14)

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

Note: `convert_guesty_export.py` runs standalone (not a subcommand); with no `--input` it auto-detects the latest Guesty CSV in `data/`.

## Common Tasks

### Full Pipeline Rebuild
```bash
python src/convert_guesty_export.py
python -m src.run_month_close guesty-import --csv data/guesty_converted.csv
python -m src.run_month_close build --period 2026-05
```

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
- **config.yml**: Database path, QBO realm ID, PM fee defaults
- **Listing_contacts.csv**: Property addresses and owner contact info (source for mapping_classes.yml)

**PM fee rate sources (watch for drift)**: The authoritative rate is per-property in `owner_contracts` (loaded from `mapping_classes.yml` `pm_fee_rate`). `config.yml` `default_pm_fee_rate` (0.16) is the fallback used by `build`, while `dashboard.py` hardcodes `0.18` for display — these three can disagree. When changing fee rates, update the contract/mapping, not just config or the dashboard constant.

---

## Working Principles

**Think before coding.** State assumptions. If multiple interpretations exist, present them. If something is unclear, ask.

**Simplicity first.** Only implement what was asked. No speculative abstractions, error handling for impossible scenarios, or "flexibility" that wasn't requested.

**Surgical changes.** Only touch what you must. Match existing style. Don't refactor adjacent code or delete pre-existing dead code unless asked.

**Goal-driven execution.** Define success criteria upfront. Loop independently by making explicit what needs verification.
