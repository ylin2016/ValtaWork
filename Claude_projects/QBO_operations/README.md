# QBO_operations

Everything that **writes** to the Valta Realty QuickBooks company file
(realm `9130356236278636`): channel payout journal entries, reservation payments,
and account recategorisations.

Its counterpart, `../Owner_statement_whole`, **only reads** QuickBooks — that is
enforced in code, not by convention (its client raises `ReadOnlyError` on any
method but GET).

## Setup

```bash
python3 -m venv venv && venv/bin/pip install -r requirements.txt
```

Secrets live in `config/secrets/` (git-ignored): `.env` with `QBO_CLIENT_ID` /
`QBO_CLIENT_SECRET` / `QBO_REDIRECT_URI`, and `qbo_tokens.json`.

```bash
python -m src.auth url                          # open it, approve
python -m src.auth exchange --code <CODE>       # tokens land in config/secrets/
python -m src.verify_accounts                   # every Id in accounts.yml still resolves
```

## The workflow: build → review → post

Nothing writes to QuickBooks without a review CSV a human has looked at first, and
no script writes anything without `--confirm`. Bare invocations are dry runs.

```bash
# 1. BUILD a review CSV from the channel payout export (read-only)
python -m src.je.build_airbnb      --indir inputs/JE --out review/airbnb_je.csv
python -m src.je.build_bookingcom  --src inputs/JE/bookings_JE_input_jul_aug.csv \
                                   --out review/bookingcom_je.csv
#    --skip-existing drops payouts already on the books

# 2. REVIEW review/*.csv.  Check the WARN lines, and that debits == credits.

# 3. POST
python -m src.je.post --csv review/airbnb_je.csv --all             # dry run
python -m src.je.post --csv review/airbnb_je.csv --all --confirm   # writes
```

Zelle lump-sum invoice payments come from the tracking workbook, same shape:

```bash
python -m src.invoices.build_zelle --skip-existing     # -> review/zelle_expenses.csv
python -m src.invoices.build_zelle --src inputs/Invoice_payment/20260911_Expense2qbo.xlsx \
       --out review/expense2qbo_20260911.csv --skip-existing   # newer layout
python -m src.invoices.post --all                      # dry run
python -m src.invoices.post --all --confirm            # writes
```

Payments and corrections follow the same dry-run-then-`--confirm` shape:

```bash
python -m src.payments.repoint --csv review/bookingcom_payment_rebuild.csv [--confirm]
python -m src.fixes.passthrough_tot [--confirm]
python -m src.fixes.refunds_to_resolutions [--cancelled-back] [--confirm]
python -m src.fixes.fee_rebill_accounts [--limit N] [--confirm]   # resumable, 17k bills
```

## Layout

```
config/
  config.yml       realm / base_url / minorversion
  accounts.yml     named account Ids + FullyQualifiedNames (no bare literals in code)
  payees.yml       Zelle-sheet payee spelling -> QBO Vendor DisplayName
  Account Mapping.csv   expense-sheet Category -> QBO account (owner-maintained)
  property_classes.yml  sheet property names that are not a listing -> Class
  line_splits.yml       lines carved in two (hot tub: $30 -> Maintenance - Owner)
  secrets/         .env, qbo_tokens.json — git-ignored
inputs/JE/         channel payout exports (Airbnb, Booking.com)
inputs/Invoice_payment/  Zelle_payment_tracking.xlsx (Records + account_map sheets)
review/            generated review CSVs — the human checkpoint
src/
  paths.py         layout single-source
  config.py        config/accounts loaders + the ONE QBOClient factory
  qbo_client.py    read/write client + OAuth (the only writing client in the estate)
  resolver.py      name -> QBO Id lookup, cached
  bridge.py        the ONE place this project reads Owner_statement_whole
  auth.py          OAuth flow
  verify_accounts.py
  je/              build_airbnb, build_bookingcom, post, payload
  invoices/        build_zelle, post -- Zelle lump-sum Expenses
  payments/        repoint
  fixes/           passthrough_tot, refunds_to_resolutions, fee_rebill_accounts
```

## Relationship to Owner_statement_whole

One direction only, and every crossing goes through `src/bridge.py`:

| What | Why not copied |
|---|---|
| `config/mapping_classes.yml` | maintained there from `Listing_contacts.csv`; a copy goes stale |
| `inputs/<period>/` Guesty exports | check-in/out dates for JE descriptions; Guesty allows 5 tokens/24h |
| `ltr.labels.to_property_id` | that project's CLAUDE.md makes it the ONE Listing-label map |
| `db/owner_statement.sqlite` | read-only, conf code -> listing |

Every consumer takes `--statements-root` to override the default sibling path.

**The OAuth token goes the other way**: this project owns
`config/secrets/qbo_tokens.json`, and Owner_statement_whole reads that same file.
Intuit rotates the refresh token on every refresh, so there is exactly **one** copy
on disk — never duplicate it.
