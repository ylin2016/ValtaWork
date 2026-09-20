# CLAUDE.md

Guidance for Claude Code when working in this project.

## What this is

Allocates Yacinde **cleaning** (AA Professional Cleaners invoices) and **supplies**
costs to the party that bears them. The owner comes from one of two files:

- **Fractional units** (B1 B2 B3 B4 F1): the fractional-master week owner. Individual → Fractional share, Developer → NuGrowth, LLC → Yacinde Holdings.
- **Whole-owner units** (B6 C1 E1 E3 F5): `YCA_Owners` gives the owner for the whole year (all NuGrowth Chelan LLC) → NuGrowth.

**Who pays each clean** (`cleaning_paid_by`, `hoa_paid`, `owner_paid`):

- Whole-owner units: HOA pays the first 4 cleans per unit per calendar month.
- Fractional units (B1 B2 B3 B4 F1): **every fraction week gets 1 HOA clean**, whatever the owner type (Individual, Developer, LLC), so about 4 per unit per month. A clean belongs to the week with `week_start < clean date <= week_end`, so a Friday checkout clean counts for the week that just ended. The first clean that week is HOA's; later cleans are paid by that week's owner. A week with no invoiced clean gets a **$0 HOA row** (`Invoice # = not invoiced`), dated week end, starting with the week ending 07/03.
- Everything else is paid by the party. Invoice 1173 (B6 intensive smoke clean, $250) was
  originally excluded from HOA coverage; the owner confirmed on 2026-09-17 that **the HOA
  pays it**, and both output tabs now show it that way.

Mapping and rules: `config/allocation_rules.yml`.

## Commands

```bash
source /Users/ylin/ValtaWork/.venv/bin/activate
cd /Users/ylin/ValtaWork/Claude_projects/yacinde_expense
python -m src.fetch_guesty [--checkin-from 2026-06-01 --checkin-to 2026-12-31]   # API pull
python -m src.build [--start 2026-07-01 --end 2026-09-15]                        # -> output/*.xlsx
python -m src.resummarize [--file output/yacinde_expense_allocation_20260701_20260915.xlsx]
python -m src.resummarize --file <workbook> --split-by-month      # -> one workbook per month
```

**Hand corrections:** the user edits the **Bookings + Cleaning** tab directly: moving cleans between rows, changing `clean_cleaning_paid_by`, and adding $0 HOA placeholders on stays. After that, run `src.resummarize`, **not** `src.build`, because build overwrites the workbook and loses the edits. Resummarize treats the combined tab as the source of truth and never writes to it. It rebuilds Summary, By Owner, both by-month tabs, **Invoice - HOA / NuGrowth / Yacinde Holdings** (one line per booking: `cleaning_amount` and `supplies_cost` side by side with the guest count, totalled; a line appears when that party owes either), Cleaning Allocation and Exceptions from that tab. `--split-by-month` instead writes one workbook per month (`..._2026jul.xlsx`, `..._2026aug.xlsx`), each holding that month's source rows and its own summaries and invoices; a row belongs to the month of its clean date, or its checkout when it has no clean. The original file is left alone. It also reads a trimmed single-tab extract (e.g. `yacinde_expense_allocation_2026jul_aug.xlsx`): missing ownership_type / party come from the fractional master and `YCA_Owners`, supplies columns are optional, and a `paid_by` written as a correction ("NuGrowth->HOA") counts as the party after the arrow. A clean is any row with `clean_cleaning_paid_by`. `hoa_paid` and `owner_paid` are re-derived from paid_by × `clean_Amount`, since the stored columns can go stale. Exceptions flag stays with no clean, HOA over its limits (4/month on whole-owner units, 1 per fraction week counted by the row's week), and fraction weeks with no HOA clean.

`fetch_guesty` reuses **GuestyFinancials'** client via importlib (both projects
have a `src` package) along with its cached token. Guesty allows only 5 tokens per
24h, so never force a refresh. It pulls status **confirmed + canceled** only. That
already includes `owner` and `owner-guest` stays. The only status left out is
`inquiry` (unbooked quotes).

## Inputs (`data/`)

- `previous_company/…Arrivals….xlsx`: rows with `Res. Status == Hold` are dropped. These are 395-night Twyford blocks. The file has **no guest count**.
- `owner_timeshare/Yacinde_Fractional_Owner_Master….xlsx`, sheet `Bookings`: owner weeks, Fri→Fri, for **B1 B2 B3 B4 F1 only**. B1 segment K is listed twice (Nugrowth + Lynn); the Individual row wins.
- `owner_timeshare/YCA_Owners_Aug 13 2026.xlsx`: owner per unit. Rows with a unit title like `B6` (no segment letter) are whole-owned.
- `guesty/guesty_reservations.json`: written by `fetch_guesty`.
- `cleaning/Yacinde_Cleaning_Records_Jul-Sep_2026.xlsx`: one row per clean, keyed from the PDF invoices. Invoice 1173 (B6 intensive clean, $250, 07/24) was added on 2026-09-16, and the formula ranges were extended to row 110.

## Rules in `src/build.py`

1. **Dedupe** on exact unit + check-in + check-out.
   - Guesty's own duplicates are resolved first, keeping a confirmed row over a canceled one.
   - Before the `migration_cutoff` (2026-07-30), the previous-company status wins. On or after the cutoff, Guesty's status wins.
   - A previous-company row dated on or after the cutoff that is missing from Guesty is dropped, except when both of these hold: the same guest surname appears in Guesty for that unit on non-overlapping dates, **and** a clean follows the original checkout. Then the row is kept and flagged in the Dedup Log (example: F1 Emma Trude).
2. **Owner weeks:** Individual owner weeks in the fractional master, starting with the week of `timeshare_weeks_from` (06/26–07/03), are added as stays with `source=fractional_master` when no booking (even a canceled one) overlaps them. This covers early July, before the previous-company file (arrivals from 07/09) and Guesty start.
3. **Ownership** comes from the master week that contains the check-in date. Stays that cross into another owner's week are flagged. Whole-owner units take the owner from `YCA_Owners`. Units in neither file get `UNASSIGNED`. A clean with no matching stay on a whole-owner unit still gets that unit's owner.
4. **Supplies** = guests × nights × 0.9, for confirmed stays only. **HOA bears the supplies of every booking checking in on/after `supplies.hoa_pays_from` (07/12), whatever the unit's owner or party**; stays checking in earlier are marked `not charged` and appear on no invoice. An extract without the supplies columns takes each stay's guest count and cost from `supplies.source_workbook` by unit + check-in + check-out; `--split-by-month` writes both into each month's source tab.
   - Timeshare stays (ownership type Individual) use max sleeps (Guesty `listing.accommodates`).
   - These also fall back to max sleeps: previous-company-only rows, owner weeks added from the master, and `(NWV)` owner reservations migrated into Guesty, whose guestsCount is only a placeholder.
5. **Cleans** merge to stays **by checkout date**, same unit. Exact date matches come first. Otherwise a clean goes to the most recent uncleaned checkout 1–`max_days_after_checkout` (3) days earlier, but not while another guest is mid-stay. Each stay gets one clean. Unmatched cleans are listed in Exceptions and still allocated through their fraction week or whole owner.
6. The period filters stays by **checkout date**. Every clean in the file is allocated.

## Output sheets

Summary (by party only: stays, nights, cleans, cleaning and supplies cost), the three Invoice tabs, then the combined tab, **Bookings + Cleaning** (built on the owner fraction weeks. Green week columns (unit, week_start, week_end, segment, owner, ownership_type, party) come first, then the blue `stay_*` columns for each in-period confirmed stay that **checks in** during the week, then the orange `clean_*` columns for that stay's clean, merged on checkout. A week with no stay still gets a row. A clean with no stay sits in its allocated week, or shares the row of a same-day checkout that has no clean of its own (e.g. the week-end $0 HOA clean). `clean_week_start` shows the week that clean counts toward for HOA, which can be the next week. Whole-owner units have no weeks and are listed by date. Supplies are shown once per stay so the column sums), By Owner (hoa_paid / owner_paid / owner_total_cost), Whole Owner Cleans by Month, Fraction Cleans by Month, Bookings, Cleaning Allocation, Exceptions, Dedup Log, Prev Holds Excluded.

## Confirmed by user (2026-09-16)

- HOA's 4 cleans/month are counted **per unit** (`hoa_cleaning.scope: unit`).
- The period starts **2026-07-01** (checkout basis), even though the first AA clean is 07/13.
