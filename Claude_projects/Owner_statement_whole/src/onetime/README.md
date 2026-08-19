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
| `update_mapping_term.py` | Backfill the `term` (STR/LTR) field on mapping entries | one-time (done) |

Run them (from the project root) as modules, e.g.:

```bash
python -m src.onetime.check_counts
python -m src.onetime.list_classes
```

To retire one for good, delete it — no workflow code depends on it.
