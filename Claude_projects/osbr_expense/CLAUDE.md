# osbr_expense — OSBR receipt itemization & categorization

Ocean Spray Beach Resort (OSBR, Grayland WA) receipts → itemized lines → categories
(Supply, Repair, Maintenance, Landscaping, Staging, Decor, …) → spend report.

## Why a script (not Claude reading files directly)
Receipts live on the billing@ Google Drive mount (default `--src` in src/paths.py is
currently `~/Google Drive/My Drive/Company Transactions/2026/2026-08/`, a symlink into):
`~/Library/CloudStorage/GoogleDrive-billing@valtarealty.com/.shortcut-targets-by-id/0B8udUTg0byTRZ1hOQllLRjRIckk/Bookkeeping/Company Transactions/2026/`.
macOS blocks Claude's shell *and* the Claude app terminal from that path; the user
will not copy files or grant Full Disk Access. The Drive connector is vacation@ and
can't see it. So the work happens in a Claude Code session launched from **VS Code**
(which has Drive access). A session in the Claude desktop app can only work on the
cached JSON / report in `output/`.

## Default workflow: Claude Code inside VS Code does the itemizing (no API key)
The user chose this over the API (no extra cost). A Claude Code session started from
VS Code inherits VS Code's Google Drive access, so it can read the receipts. When the
user says "process receipts" / "do the next batch", run this loop:

```bash
source /Users/ylin/ValtaWork/.venv/bin/activate
cd /Users/ylin/ValtaWork/Claude_projects/osbr_expense
python -m src.manual stage --limit 10   # next 10 pending → output/staging/ (symlinks, not copies)
```
1. Read `output/staging/INSTRUCTIONS.md` once (rules, categories, exact JSON shape).
2. For each entry in `output/staging/manifest.json`: Read the `staged` file, then Write
   `output/extracted/<hash>.json`, `"model": "claude-code"`. Every key required, no extras;
   never invent a total — set `total_legible: false` instead.
3. `python -m src.manual check` → fix every error it prints, rerun until clean.
4. Repeat stage → itemize → check until nothing is pending (batches keep context small).
5. `python -m src.build` → report the summary lines and the Flags sheet to the user.

If `stage` says "Operation not permitted", this session isn't running under VS Code — tell
the user to start `claude` from VS Code's integrated terminal (or the VS Code extension).
Staged links are named `<hash>.pdf/.jpg` because receipts have no file extension and the
Read tool picks the reader from the extension.

## Alternative: API mode (needs ANTHROPIC_API_KEY, paid per call, ~$0.03–0.10/receipt)
```bash
pip install -r requirements.txt          # once: anthropic
python -m src.extract --list             # free: which files match "OSBR"
python -m src.extract --limit 3          # costs API tokens
python -m src.extract                    # all new files; cached ones are skipped
python -m src.build
```
Options: `--src <folder>`, `--match TEXT` (repeatable), `--force`, `--workers N`.
Key: `ANTHROPIC_API_KEY` env var or `osbr_expense/.env` (gitignored).
Both modes write the same `output/extracted/<hash>.json`, so they can be mixed.

## Pipeline
- `src/extract.py`: finds files whose relative path contains "OSBR" or "Cottage" (case-insensitive) and whose
  **content** (magic bytes, not extension) is PDF/JPEG/PNG/GIF/WebP/HEIC — receipts are
  often saved with no extension, e.g. `9967/20260804_OSBR_Andrea_7.28_Homedepot_Housekeeping supplies_304.78`; HEIC and >3.5 MB photos are converted/shrunk with macOS `sips`. One
  claude-opus-5 call per file with a strict JSON schema (receipts[] → items[] with
  category enum from `config/categories.yml`, confidence, notes). Server-side refusal
  fallback on. Result cached to `output/extracted/<sha256[:16]>.json`; errors are not
  cached, so a re-run retries them.
- `src/build.py`: no API. Allocates tax, shipping+fees, order discount pro rata per item
  (Decimal, penny remainder → largest item). Flags: does not reconcile (>$0.02),
  total not legible, no date, no items, possible duplicate (vendor+date+total),
  low confidence, unknown category, not a receipt.
- File-name convention `<upload date>_OSBR_<person>_<M.D>_<vendor>_<desc>_<amount>`, top
  folder = card last 4 (9967, 3038). build shows `card_folder` + `name_amount` and flags
  "file name amount differs" when Σ receipt totals ≠ name amount (>$0.02).
- Sheets: Summary (by category + statement month×category + cottage×category), Items, Receipts, Flags, Files.

## Statement month (which owner statement a cost is charged on)
Report scope: `REPORT_MONTHS` in src/paths.py (now 2026-07, 2026-08); `python -m src.build
--months 2026-07,2026-08,2026-09` overrides. Every sheet is filtered to those statement months.
Default = the YYYYMM that starts the file name (`20260804_OSBR_…` → 2026-08). Exceptions in
`config/statement_months.csv` (source_file,line_no,statement_month,note; blank line_no = whole
file, a line_no moves one line of a receipt split across statements). Summary has a Statement
month × Category table; compare it with `input/<YYYY-MM> - Grayland Ocean Spray Resort.pdf`.
Reconciled 2026-09-23: August $22,192.02 vs statement $22,192.24 (Diet Coke tax −0.32,
Walmart towels +0.08, Dollar General +0.02). July $18,380.81 vs statement $8,583.08: +$104.23 7/15
laundry supplies and +$9,693.50 July cleaner labor, both included per user and on no statement.

## Manual entries (no receipt file)
`config/manual_expenses.csv` — date,vendor,description,amount,category,cottage,statement_month,source,note.
Each row becomes a one-item receipt (`source_file` = `manual/<source>/<n>`). Holds the
August 2026 cleaner payouts from the owner statement ($11,266.75, category Cleaning), July cleaner
labor ($2,887.50 + $6,806.00), Ed's labor/mileage, landscaping, Jing housekeeping, SilverStar telecom.

## Excluded files
`config/excluded_files.csv` — source_file,reason. Dropped from the report entirely (not in
Items/Receipts/Flags/Files). Holds guest pass-throughs (pet fee, USPS shipping), Valta Homes backup receipts, the June Sirens
payout, and the July cleaner payout files (their amounts are manual_expenses rows).

## Corrections
`config/overrides.csv` — `receipt_file,line_no,category,cottage,note`; category `Exclude` drops a personal line with its tax share (e.g. crossed-out Diet Cokes) (values from the Items
sheet) → rerun `src.build`. Never hand-edit the xlsx; it's regenerated.

## Category rules confirmed by user
- Vendor receipts re-billed on a Valta Homes invoice are BACKUP, not separate spend: keep the
  amounts on the invoice lines and add the backup files to excluded_files.csv (e.g. the 5/1 and
  6/1 dumpster receipts behind `Valta Homes_06.30_Ling_Repair_1379.67`; True Value 6/17 + LeMay 7/9
  behind `Valta Homes_07.23_Jing_Repair_670.2`). Never zero the invoice line instead.
- Guest pass-throughs are NOT OSBR spend → add to excluded_files.csv: pet fees paid out,
  guest lost-item shipping ("<guest> shippinghandling fee", USPS), anything billed back to a guest.
- July cleaner labor belongs in Cleaning (user 2026-09-23, reversing 09-22): Maria/Olga $2,887.50 is a
  manual_expenses row, statement month 2026-07. Sirens $8,916.24 is "OSBR and Others" — OSBR share
  $6,806.00 per user, also a July manual_expenses row. Neither is on the July/Aug statements.
- Staging and Decor both mean items that **stay**. Staging = functional furnishings
  (lamp, rug, curtain, mirror, bath mat); Decor = purely ornamental (art, vases, pillows).
- OSBR = Ocean Spray Beach Resort, cottages 1–12 ("Cottage 5" = "OSBR 5"). Cottage comes
  from the receipt, else the Drive path, else "Common" (resort-wide) / "Unassigned".
