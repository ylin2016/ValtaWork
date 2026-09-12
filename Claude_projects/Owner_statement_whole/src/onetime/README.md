# src/onetime — one-time & maintenance scripts

**Not part of the monthly close.** These ran once (migrations, backfills) or are
occasional diagnostics. They are quarantined here so the functional workflow
packages (`guesty/`, `breakdown/`, `netrevenue/`, `expense/`, `reporting/`) stay
clean. Nothing in the workflow imports anything here.

| Script | What it does | Status |
|---|---|---|
| `check_counts.py` | Print ledger row counts by source/category for a sanity check | diagnostic — run ad hoc |
| `check_exceptions_detail.py` | Dump rows from the `exceptions` table with detail | diagnostic — run ad hoc |
| `list_classes.py` | List QBO classes (id + name) from the API | setup / occasional |
| `export_class_stubs.py` | Emit `mapping_classes.yml` stub entries from QBO classes | setup / occasional |
| `import_yacinde_old.py` | One-time backfill of old Yacinde timeshare bookings into the ledger | one-time (done) |
| `update_mapping_from_contacts.py` | First-gen `mapping_classes.yml` sync from `Listing_contacts.csv` | superseded |
| `update_mapping_from_contacts_v2.py` | Second-gen version of the above | superseded |
| `build_channel_calculator.py` | Pull the Guesty account channel **markups** and build `output/channel_pricing_calculator.xlsx` — a live-formula what-if: one accommodation fare -> markup / fees / tax / net revenue on every channel | tool — run ad hoc |
| `update_mapping_term.py` | Backfill the `term` (STR/LTR) field on mapping entries | one-time (done) |

## Moved out — QuickBooks WRITES live in `../QBO_operations`

The channel-payout journal entries, the reservation-payment repointing and the
account recategorisations used to sit here. They write to QuickBooks, and this
project only reads it, so they moved to the sibling `QBO_operations` project:

| was | now |
|---|---|
| `build_airbnb_je.py` | `QBO_operations/src/je/build_airbnb.py` |
| `build_bookingcom_je.py` | `QBO_operations/src/je/build_bookingcom.py` |
| `post_airbnb_je.py` | `QBO_operations/src/je/post.py` |
| `repoint_bookingcom_payments.py` | `QBO_operations/src/payments/repoint.py` |
| `fix_passthrough_tot.py` | `QBO_operations/src/fixes/passthrough_tot.py` |
| `move_refunds_to_resolutions.py` | `QBO_operations/src/fixes/refunds_to_resolutions.py` |

`inputs/JE/` moved to `QBO_operations/inputs/JE/` and the review CSVs to
`QBO_operations/review/`. `list_classes.py` and `export_class_stubs.py` stayed:
they only read, and they feed this project's `mapping_classes.yml`.

Run them (from the project root) as modules, e.g.:

```bash
python -m src.onetime.check_counts
python -m src.onetime.list_classes
```

To retire one for good, delete it — no workflow code depends on it.
