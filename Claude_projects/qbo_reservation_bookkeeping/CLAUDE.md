# CLAUDE.md — qbo_reservation_bookkeeping

Guidance for Claude Code working in this project.

## What this is

The **replacement for VRP** (VRPlatform). VRP synced every Guesty reservation into the Valta
Realty QuickBooks company file (realm `9130356236278636`) nightly, and **stopped after
2026-09-28 ~22:14 PT** (its last Payments 2026-09-29). This project writes the same objects
in the **same format** so the books, the bank feed and Owner_statement_whole keep working:

| Object | Source | One per |
|---|---|---|
| Invoice | Guesty reservation | reservation |
| $0 fee Bills (commission, supplies, channel fee, Stripe fee) | Guesty + rates below | applicable fee per reservation |
| Payment (Invoice → channel clearing) | owner-supplied payout files | reservation per payout |
| Payout JE (clearing → Chase 9967) | owner-supplied payout files | payout batch |

**Read `docs/VRP_QBO_FORMAT.md` before writing any builder.** It is the reverse-engineered
spec — DocNumbers, customers, items, accounts, classes, descriptions, signs — taken from
the objects VRP actually posted, with worked samples that tie to the cent. Match it exactly
unless a decision below says otherwise.

Status (2026-10-06): **backfill POSTED.** 154 reservations written 2026-10-06 (76 new invoices + customers, 26 invoice updates, 10 voids, 177 Bill creates, 59 updates, 16 zeroed); re-snapshot + rebuild = 249 SAME. Held: HA-CUd9gPx ($200 kept) and HA-31ztBpZ ($200 refunded) -- voids blocked by a linked payment; month-end step. Next: payments + payout JEs. Parity vs
VRP on 470 reservations it already posted: 383/392 invoices, 362/375 commission, 213/214 supplies,
62/64 Booking + 62/63 VRBO fee Bills identical; every residual explained (VRP stale, Stripe
policy). Next: owner review of the open questions in the build output, the poster (`--confirm`),
then payments + payout JEs (Airbnb + itemized Stripe payout files are in `inputs/`).

## Commands (use `../Owner_statement_whole/venv/bin/python` -- it has every dependency)

    python -m src.guesty_pull --since 2026-09-14T00:00:00Z   # 1 Guesty token; stores inputs/guesty/
    python -m src.qbo_snapshot          # read-only: VRP's Invoices/Bills for those codes
    python -m src.build_invoices        # changed since VRP's cutoff -> review/plan|reservations|lines_*
    python -m src.build_invoices --parity   # rebuild what VRP posted and diff -> review/parity_*
    python -m src.post_invoices [--code X ...]          # DRY RUN of the latest plan (live checks)
    python -m src.post_invoices [--code X ...] --confirm   # write; appends review/CHANGE_LOG.csv

`post_invoices` posts the PLAN FILE the owner reviewed, never a rebuild. It refuses a CREATE
whose DocNumber now exists, an UPDATE/ZERO/VOID whose SyncToken moved since the snapshot
(rebuild the plan), and a VOID with a payment linked (month-end cancellation step). First
live post 2026-10-05: HMHTND2XXN (Invoice 120128, Bills 120129/120130) -- format verified.
    python -m src.payout_inbox [--dry-run]  # inputs/inbox/ Airbnb + Booking.com exports -> new,
                                            # verified payouts in inputs/airbnb|bookingcom + payout_ledger.csv

Both siblings call their package `src`; `src/bridge.py` loads them as `qbo_ops` / `osw`.

## What parity taught (2026-10-04) -- all encoded, keep it that way

- **Booking.com DocNumber = Booking's 10-digit `integration.bookingCom.reservationId`**, not
  Guesty's `BC-…`. The list endpoint returns it EMPTY, so `guesty_pull` fetches it per booking.
- DocNumbers longer than 21 chars are truncated (`HVBM-M1789050058259-F`, `RZ3O_EXP-2540847848-U`);
  memos and customer names keep the full code. Owner stays (`owner`) use customer `Direct - …`.
- Commission base = Owner Income fare lines (fare, markup, extra person, adjustment, discounts)
  + channel-commission line **+ Pet Fee (Owner)**. Rounding is **half-even** (`pct`).
- Cleaning item (Owner vs PM) follows VRP's own latest invoice for the class -- VRP books every
  OSBR and Yacinde unit as Owner, which `owner_pays_cleaning` does not say. Trip.com "Cleaning"
  and Expedia "Service" are always `Cleaning Fee (PM)`. VRBO taxes → `Misc Guest Fee (PM)`.
- Airbnb Resolution Center lines are NOT invoiced (paid separately as resolution payouts).
- `owner` / `owner-guest` stays: no invoice (VRP's amounts on those were hand-entered).
- VRP was often STALE: it missed Guesty changes (nights, added fees) and cancellations made
  before its cutoff -- those surface as UPDATE / VOID even in parity.
- VRP's Stripe fee = fees on the payments collected so far (2.9% + $0.30 + Guesty 1% per
  charge), updated as each payment came in. It was right up to its cutoff; what it misses is
  any payment charged after 2026-09-28 (e.g. HA-6S32KPN: VRP's $8.10 is the 5/30 $200
  deposit; the $3,185.26 balance charged 9/28 PT adds $124.52 -> Bill $132.62).

## Owner decisions 2026-10-04

- **Stripe fee Bill (RZ3O_) = ACTUAL fees** (`src/stripe_fees.py`): each Guesty SUCCEEDED payment
  is matched by Stripe charge id (`charges.data[].id`, or the PaymentIntent's `latest_charge`)
  to the itemized `inputs/stripe/<amount> Payout.csv` (`Fees` = Stripe + Guesty 1%, less any
  "Application fee refund" adjustment). Charges not yet in a payout file, and any unpaid
  balance, are ESTIMATED (2.9%, 4.4% non-US card, + $0.30 + Guesty 1%) and replaced on re-run;
  the review CSV splits it: `stripe_pre_cutoff` (payments charged before VRP stopped, VRP's
  per-payment formula), `stripe_actual` (payout files), `stripe_estimate` (charged since but not
  yet in a payout file, plus unpaid balance), and `stripe_vrp_posted` (VRP's Bill) -- a WARN fires
  when VRP's Bill != pre-cutoff fees (HA-YmrTS7p: VRP missed a 9/24 payment).
  payment_model only decides WHETHER a booking carries card fees.
- **Expedia bookings get the Stripe fee Bill** (VRP never billed it; Expedia charges do appear
  in the Stripe payout files).
- The main build also re-plans any reservation charged in a Stripe payout file, even if Guesty
  did not change, so its fee Bill moves to actual.
- Bellevue 15746 (20%, central supplies) and Renton 18823 (24%, central supplies) added to
  Owner_statement_whole `Listing_contacts.csv` + `mapping_classes.yml` (real QBO classes
  1000000039 / 1000000036; owner name/email still placeholders) and synced to owner_contracts.

## Owner decisions (2026-10-02 / 03) — these override VRP's behaviour

1. **Lives here, writes through QBO_operations' connection.** Do NOT copy
   `../shared/secrets/qbo_tokens.json` or `.env` (the one shared copy, since 2026-10-09): Intuit rotates the refresh
   token on every refresh, so two copies invalidate each other. Import/borrow
   `QBO_operations/src/qbo_client.py` + `config.client()` (or point a client at that token
   path). QBO_operations' CLAUDE.md still says it is the only writer — update it to name this
   project as the second permitted writer when the first post happens.
2. **Only applicable Bills.** VRP created every fee-type Bill on every reservation, $0 where it
   did not apply. We create commission, supplies (central listings only) and the ONE channel /
   Stripe fee Bill that applies. Same DocNumber prefix, vendor, accounts, memo format.
3. **Cancellations: VOID the invoice** (not VRP's $0 "Cancelled reservation" rewrite). At month
   end, if any money was kept from a cancelled booking (seen in the payout files), bring the
   invoice back to that amount with the `Cancellation Fee` item and let its fee Bills follow.
   **Unverified:** whether the API allows editing a voided invoice back to non-zero — test on
   one first; fallback is a separate invoice `<code>-CXL`.
4. **Booking.com is two-step like Airbnb and Stripe** — Payment into `Payments Clearing -
   Booking.com` (1676), payout JE clearing → Chase 9967. Owner supplies the payout file.
5. **Stripe fee in both places**, as VRP did: COGS `Fee - Stripe Processing` lines in the Stripe
   payout JE AND the `RZ3O_` rebill Bill charging the owner.
6. **Fix Pass Through Tot**: Airbnb payout JEs credit clearing for the FULL payout (the invoice
   already books the tax under `Guest Charges:Tax`); never route it to Tax Liability.
7. **Backfill** everything created or changed in Guesty since 2026-09-28 22:00 PT.

## Rates — never invent, always resolve from Owner_statement_whole

- **PM commission rate**: `../Owner_statement_whole/src/scope/pm_rate.py`
  (`pm_rate_resolver`, `owner_contracts` table in `db/owner_statement.sqlite`), effective-dated
  **at check-in**. Returns None when missing → **block that reservation and ask**; never default.
  Commission base = Accommodation fare + Markup − any channel-commission line on the invoice;
  each fee Bill gives back `rate × fee` as a "Deduction" pair.
- **Channel + Stripe fees**: `config/payment_structure.xlsx`, as transcribed in
  `../shared/valta_common/fees/payment_model.py` (`compute_breakdown`, `RATE`,
  Stripe 2.9%/4.4% + $0.30 per charge + Guesty 1%, and the per-reservation override tables
  `STRIPE_FEE_OVERRIDES`, `NO_PROCESSING_FEE_CODES`, … — reuse them, never duplicate).
- **Supplies**: `0.9 × guests × min(nights, 60)` (`run_month_close.supply_charge`) for listings
  with `Supplies='central'` in `../shared/reference/Listing_contacts.csv`.
- **Class / listing mapping**: `../shared/reference/mapping_classes.yml` — but the map
  is **not proof a class exists** (some entries are one level too shallow). Resolve every class
  against the live company file, as QBO_operations' `Resolver.klass_fqn` does.

Read across to the sibling projects; never copy their maps or tables into this one — they
drift the moment a listing is renamed.

## Safety rules inherited from QBO_operations (read its CLAUDE.md — the traps are real)

- **Build → review CSV → `--confirm`.** Every script is a dry run by default; nothing posts
  until the owner has read the review CSV. Standing instruction.
- **Duplicate checks are content-based**, not just DocNumber: before creating, look up the
  DocNumber AND the content (customer/code + amount, or `(TxnDate, debit to Chase 9967)` for
  payout JEs). VRP objects already exist for most reservations up to 2026-09-28 — never
  create a second Invoice/Bill for a code VRP already posted; update it if it changed.
- **Payments never deposit straight to the bank** when a payout JE also debits the bank
  (that double-counted $55,972.41 once).
- **QBO full-updates blank omitted fields** — update by reading the object back, changing the
  fields, echoing it whole with the current `SyncToken`.
- JE line `AccountRef.name` is the fully qualified name — compare with `.endswith()`.
- `qbo.post()` already unwraps the entity; after any crash mid-post, check QBO for what was
  written before re-running.
- A refund on a stay that happened → `1A - Net Earnings:Resolutions`; a cancelled booking's
  refund washes in clearing.
- Account Ids live in `../QBO_operations/config/accounts.yml`; run
  `python -m src.verify_accounts` there before a posting session.
- `PMS Clearing – A/P` (Id 1600) has an EN DASH.

## Guesty API budget

Guesty allows **5 tokens / 24h shared across all projects**. Reuse the cached token in
`../shared/secrets/guesty_token.json` through the shared `valta_common.guesty.client`
(`bridge.guesty_client()`); do not request new tokens in a loop. The fee model and summary
frame are `valta_common.fees.payment_model` / `valta_common.guesty.summary` (see
`../shared/README.md`). One pull should cover all
reservations created or updated in the window (future check-ins included — VRP invoiced a
booking as soon as it appeared).

## Google Drive access (payout files etc.)

Drive is mounted at `~/Library/CloudStorage/GoogleDrive-billing@valtarealty.com/` (and
`~/Google Drive/My Drive/`). macOS privacy (TCC) decides per **app** whether it can be read:
the Claude desktop app gets `Operation not permitted`. In VS Code:

1. System Settings → Privacy & Security → **Full Disk Access** → add **Visual Studio Code**
   (or Files and Folders → allow Google Drive), then restart VS Code.
2. Probe first: `ls "/Users/ylin/Google Drive/My Drive/"`. `Operation not permitted` is TCC,
   not a missing file.
3. Copy what you need into `inputs/` rather than reading Drive paths from code.

Fallback: download the file manually into `inputs/<channel>/`.

**Payout files live at** `~/Google Drive/My Drive/* Monthly/Quickbook_integration/payment_records/`
(note the folder is `* Monthly` — asterisk, SPACE, Monthly):

| Drive file | What it is | Copy to |
|---|---|---|
| `airbnb/<account>_<MMDD-MMDD>.csv` | Airbnb transaction-history export per host account (`vacation`, `vacation2`, `Lucia`); Payout rows followed by their Reservation / Adjustment / Resolution rows | `inputs/airbnb/` |
| `stripe/Stripe platform payment <MMDD-MMDD>.csv` | Stripe payout list (po_ id, amount, arrival) | `inputs/stripe/` |
| `stripe/<amount> Payout.csv` | ONE payout itemized: Charge / Refund / Adjustment (application-fee refund) rows with Amount, Fees (Stripe + Guesty 1%), Net; code in `Description` (metadata often blank). Net sums to the payout exactly | `inputs/stripe/` |
| `bookingcom/` | Booking.com payout files (none yet) | `inputs/bookingcom/` |

Ignore the `QBO transactions*.csv` files in that folder — owner said not to check them.
An Airbnb payout's lines sum to its `Paid out` exactly.

## Layout

```
config/            # project config (no secrets — the token lives in QBO_operations)
docs/              # VRP_QBO_FORMAT.md — the spec
inputs/airbnb/ inputs/bookingcom/ inputs/stripe/   # owner-supplied payout files
inputs/guesty/     # stored Guesty pulls (budget!)
review/            # review CSVs + CHANGE_LOG.csv — read before --confirm
src/               # builders (dry run) and posters (--confirm)
```

Python: use `../QBO_operations/venv/bin/python` until this project has its own venv
(needs requests, PyYAML, python-dotenv, openpyxl, pandas).
