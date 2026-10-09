# CLAUDE.md

Guidance for Claude Code when working in this project.

## What this is

Allocates Yacinde **cleaning** (AA Professional Cleaners invoices) and **supplies**
costs to the party that bears them. The owner comes from one of two files:

- **Fractional units** (B1 B2 B3 B4 F1): the fractional-master week owner. Individual → Fractional share, Developer → NuGrowth, LLC → Yacinde Holdings.
- **Whole-ownership units** (B6 C1 E1 E3 F5): `YCA_Owners` gives the owner for the whole year (all NuGrowth Chelan LLC) → NuGrowth.

**Who pays each clean** (`cleaning_paid_by`, `hoa_paid`, `owner_paid`):

- Whole-ownership units: the HOA covers nothing. NuGrowth bears every clean in B6 C1 E1 E3 F5
  (user, 2026-09-25; `hoa_cleaning.free_cleans_per_month: 0`).
- Fractional units (B1 B2 B3 B4 F1): **every fraction week gets 1 HOA clean**, whatever the owner type (Individual, Developer, LLC), so about 4 per unit per month. A clean belongs to the week with `week_start < clean date <= week_end`, so a Friday checkout clean counts for the week that just ended. The first clean that week is HOA's; later cleans are paid by that week's owner. A week with no invoiced clean gets a **$0 HOA row** (`Invoice # = not invoiced`), dated week end, starting with the week ending 07/03.
- Everything else is paid by the party. Invoice 1173 (B6 intensive smoke clean, $250) is **charged to
  NuGrowth at $180**, the normal rate, and the remaining **$70 stays with the company** (user,
  2026-10-02). That split is `cleaning_caps` in the config: resummarize charges the party
  `min(amount, cap)`, puts the rest in `company_absorbed`, shows both in Cleaning Allocation
  (`billed_by_cleaner` / `charged_to_party` / `company_absorbed`), notes it on the invoice line, and
  adds a `Company (not billed out)` row to Summary so the Summary total still equals what the cleaner
  was paid.

Mapping and rules: `config/allocation_rules.yml`.

## Commands

```bash
source /Users/ylin/ValtaWork/.venv/bin/activate
cd /Users/ylin/ValtaWork/Claude_projects/yacinde_expense
python -m src.fetch_guesty [--checkin-from 2026-06-01 --checkin-to 2026-12-31]   # API pull
python -m src.build [--start 2026-07-01 --end 2026-09-30]                        # -> output/*.xlsx
python -m src.resummarize [--file output/yacinde_expense_allocation_20260701_20260915.xlsx]
python -m src.resummarize --file <workbook> --split-by-month      # -> one workbook per month
```

**Hand corrections:** the user edits the **Bookings + Cleaning** tab directly: moving cleans between rows, changing `clean_cleaning_paid_by`, and adding $0 HOA placeholders on stays. After that, run `src.resummarize`, **not** `src.build`, because build overwrites the workbook and loses the edits. Resummarize treats the combined tab as the source of truth and never writes to it. It rebuilds Summary, By Owner, both by-month tabs, **Invoice - HOA / NuGrowth / Yacinde Holdings** (one line per booking: `cleaning_amount` and `supplies_cost` side by side with the guest count, totalled; a line appears when that party owes either), Cleaning Allocation and Exceptions from that tab. `--split-by-month` instead writes one workbook per month (`..._2026jul.xlsx`, `..._2026aug.xlsx`), each holding that month's source rows and its own summaries and invoices; a row belongs to the month of its clean date, or its checkout when it has no clean. The original file is left alone. It also reads a trimmed single-tab extract (e.g. `yacinde_expense_allocation_2026jul_aug.xlsx`): missing ownership_type / party come from the fractional master and `YCA_Owners`, supplies columns are optional, and a `paid_by` written as a correction ("NuGrowth->HOA") counts as the party after the arrow. A clean is any row with `clean_cleaning_paid_by`. `hoa_paid` and `owner_paid` are re-derived from paid_by × `clean_Amount`, since the stored columns can go stale. Exceptions flag stays with no clean, HOA over its limits (4/month on whole-ownership units, 1 per fraction week counted by the row's week), and fraction weeks with no HOA clean.

`fetch_guesty` uses the shared Guesty client (`valta_common.guesty.client`, from
`Claude_projects/shared/`) with the one cached token every project shares. Guesty allows only 5 tokens per
24h, so never force a refresh. It pulls status **confirmed + canceled** only. That
already includes `owner` and `owner-guest` stays. The only status left out is
`inquiry` (unbooked quotes).

## Inputs (`data/`)

- `previous_company/…Arrivals….xlsx`: rows with `Res. Status == Hold` are dropped. These are 395-night Twyford blocks. The file has **no guest count**.
- `owner_timeshare/Yacinde_Fractional_Owner_Master….xlsx`, sheet `Bookings`: owner weeks, Fri→Fri, for **B1 B2 B3 B4 F1 only**. B1 segment K is listed twice (Nugrowth + Lynn); the Individual row wins.
- `owner_timeshare/YCA_Owners_Aug 13 2026.xlsx`: owner per unit. Rows with a unit title like `B6` (no segment letter) are whole-owned.
- `guesty/guesty_reservations.json`: written by `fetch_guesty`. Re-pulled 2026-10-01 for check-ins
  06/01-12/31: 209 Yacinde reservations, 30 more than the 2026-09 pull (8 with a September checkout:
  E3 09/23, F5 09/27, B1 09/27, B2 09/27, B6 09/27, F1 09/27, C1 09/28, and a canceled B4 09/27), and
  3 that had since been canceled (B1 + B2 Snell 09/18-09/25, B2 Tift 09/25-09/28). Unmatched cleans
  fell from 12 to 6. All ten listings are nicknamed `Yacinde <unit>`, so the nickname filter in
  `fetch_guesty` drops none of them. Always re-pull before reporting a month that is still open.
- `cleaning/Yacinde_Cleaning_Records_Jul-Sep_2026.xlsx`: one row per clean, keyed from the PDF invoices. Invoice 1173 (B6 intensive clean, $250, 07/24) was added on 2026-09-16. Invoice 1204 (Sep 16-30, $3,860, 22 cleans) was added on 2026-10-01; the data block now ends at row 132 and the By Unit / By Invoice formula ranges follow it. 131 cleans, $23,300.00 in total.

## Rules in `src/build.py`

1. **Dedupe** on exact unit + check-in + check-out.
   - Guesty's own duplicates are resolved first, keeping a confirmed row over a canceled one.
   - Before the `migration_cutoff` (2026-07-30), the previous-company status wins. On or after the cutoff, Guesty's status wins.
   - A previous-company row dated on or after the cutoff that is missing from Guesty is dropped, except when both of these hold: the same guest surname appears in Guesty for that unit on non-overlapping dates, **and** a clean follows the original checkout. Then the row is kept and flagged in the Dedup Log (example: F1 Emma Trude).
2. **Owner weeks:** Individual owner weeks in the fractional master, starting with the week of `timeshare_weeks_from` (06/26–07/03), are added as stays with `source=fractional_master` when no booking (even a canceled one) overlaps them. This covers early July, before the previous-company file (arrivals from 07/09) and Guesty start.
3. **Ownership** comes from the master week that contains the check-in date. Stays that cross into another owner's week are flagged. Whole-ownership units take the owner from `YCA_Owners`. Units in neither file get `UNASSIGNED`. A clean with no matching stay on a whole-owner unit still gets that unit's owner.
4. **Supplies** = guests × nights × 0.9, for confirmed stays only. From `supplies.hoa_pays_from` (07/12) supplies **follow the unit**: the whole-ownership units (B6 C1 E1 E3 F5) go to their owner, NuGrowth, and the fractional units go to the HOA. Stays checking in earlier are marked `not charged` and appear on no invoice. Each bearing party gets its own `Invoice_<party>_supply` tab. An extract without the supplies columns takes each stay's guest count and cost from `supplies.source_workbook` by unit + check-in + check-out; `--split-by-month` writes both into each month's source tab.
   - Timeshare stays (ownership type Individual) use max sleeps (Guesty `listing.accommodates`).
   - These also fall back to max sleeps: previous-company-only rows, owner weeks added from the master, and `(NWV)` owner reservations migrated into Guesty, whose guestsCount is only a placeholder.
5. **Cleans** merge to stays **by checkout date**, same unit. Exact date matches come first. Otherwise a clean goes to the most recent uncleaned checkout 1–`max_days_after_checkout` (3) days earlier, but not while another guest is mid-stay. Each stay gets one clean. Unmatched cleans are listed in Exceptions and still allocated through their fraction week or whole owner.
6. The period filters stays by **checkout date**. Every clean in the file is allocated.

## Output sheets

Summary (by party only: stays, nights, cleans, cleaning and supplies cost), the three Invoice tabs, then the combined tab, **Bookings + Cleaning** (built on the owner fraction weeks. Green week columns (unit, week_start, week_end, segment, owner, ownership_type, party) come first, then the blue `stay_*` columns for each in-period confirmed stay that **checks in** during the week, then the orange `clean_*` columns for that stay's clean, merged on checkout. A week with no stay still gets a row. A clean with no stay sits in its allocated week, or shares the row of a same-day checkout that has no clean of its own (e.g. the week-end $0 HOA clean). `clean_week_start` shows the week that clean counts toward for HOA, which can be the next week. Whole-ownership units have no weeks and are listed by date. Supplies are shown once per stay so the column sums), By Owner (hoa_paid / owner_paid / owner_total_cost), Whole Owner Cleans by Month, Fraction Cleans by Month, Bookings, Cleaning Allocation, Exceptions, Dedup Log, Prev Holds Excluded.

## September 2026

Two AA invoices cover the month: 1200 (Sep 1-15, $4,680) and 1204 (Sep 16-30, $3,860). The September
report is `output/yacinde_expense_allocation_202609.xlsx`, built from
`..._20260701_20260930.xlsx`: a full `src.build` run, then the payer column of the combined tab was
overwritten with the user's own values for every clean dated on or before 09/15 (they are the
source of truth), leaving only 09/16-30 computed. Then `src.resummarize --split-by-month --months 2026-09`.
September allocates to HOA $3,140 (18 cleans), NuGrowth $4,500 (25), Yacinde Holdings $720 (4) and an
individual owner $180 (1). Supplies, still on hold: HOA $425.70, NuGrowth $167.40.

**Each month has its own source workbook, by the user's instruction (2026-10-02): July and August
stay exactly as issued, from `..._20260701_20260915.xlsx`; September comes from column W of
`..._20260701_20260930.xlsx`.** Never regenerate July or August from the 0930 file or from a fresh
build.

```bash
python -m src.resummarize --file output/yacinde_expense_allocation_20260701_20260915.xlsx --split-by-month --months 2026-07,2026-08
python -m src.resummarize --file output/yacinde_expense_allocation_20260701_20260930.xlsx --split-by-month --months 2026-09
```

Issued totals: HOA $8,260 (47 cleans), NuGrowth $12,700 (71), Yacinde Holdings $2,160 (12) = $23,120
over 130 cleans - July $4,910, August $9,850, September $8,360. No clean is billed to an individual
fractional owner. Two September decisions of the user's to preserve:

- The 09/08 F1 line on invoice 1200 ($180) was **not paid and is not allocated** - F1 had no checkout
  that day and the week's clean is 09/11. Invoice 1200 was paid $4,500 of the $4,680 billed, so
  September is $8,360 and 130 of the 131 billed cleans are passed on. The row is still in the cleaning
  records, flagged NOT PAID in its Notes.
- The 09/25 clean moved from B1 to B3 (B1's Snell stay was canceled after the pull; B3 had the
  09/21-09/25 checkout).

The 08/20 clean stays on F1 and paid by the HOA, as the July/August workbooks were issued, even
though invoice 1186 puts that line on C1 and the 0930 file's column W allocates it to NuGrowth. That
is the whole $180 difference between the two files for August.

Column W puts 2 HOA cleans in some fraction weeks (B2 wk 08/28, B4 wk 09/18) and none in others; the
Exceptions tab lists them. These are the user's calls - do not "fix" them.

## Paid vs invoiced

`output/yacinde_cleaning_paid_vs_invoiced_jul_sep_2026.xlsx` reconciles the cleaner's invoices to the
party invoices: billed by AA $23,300 / paid $23,120 / invoiced to the parties $23,050 (HOA $8,260,
NuGrowth $12,630, Yacinde Holdings $2,160) + $70 on the company. Paid = invoiced out + company share
in every month; the only unpaid line is the 09/08 F1 one above. Each AA invoice period falls inside
one month, so invoice totals and cleaning-date months agree: July $4,910 paid ($4,840 invoiced out),
August $9,850, September $8,360 paid of $8,540 billed.
Outside it: invoice 1186 was paid $6,543.37 ($63.37 of that a pack-and-play, not cleaning), the HOA
invoice also carries maintenance and maintenance labor, and supplies stay on hold.

## Confirmed by user (2026-09-16)

- HOA's 4 cleans/month are counted **per unit** (`hoa_cleaning.scope: unit`).
- The period starts **2026-07-01** (checkout basis), even though the first AA clean is 07/13.
