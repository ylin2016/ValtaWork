# CLAUDE.md — QBO_operations

Guidance for Claude Code working in this project.

## What this is

The **only** project permitted to write to the Valta Realty QuickBooks company file
(realm `9130356236278636`). It posts channel payout journal entries, creates and
repoints reservation payments, and recategorises account lines.

`../Owner_statement_whole` is the owner-statement pipeline; it **reads** the same
company file and never writes to it. That is enforced, not merely documented: its
`expense/qbo_client.py` raises `ReadOnlyError` on any method but GET. If a change to
QuickBooks is needed, it belongs here.

## Nothing posts without a review CSV and `--confirm`

Every script is a dry run by default and prints what it WOULD do. The pipeline is
always **build → review → post**, and the owner reads the review CSV in between.
This is a standing instruction, not a nicety: "don't create Journal on quickbook
before I review them."

Keep it that way when adding a script — dry run is the default, `--confirm` writes.

## The duplicate check is CONTENT-based, never DocNumber

A second bookkeeping system posts the same Airbnb and Booking.com payouts under
`YYMMDD-XXXX` DocNumbers. Matching on the channel reference code therefore misses
them and **double-posts the cash**. Every payout check keys on
`(TxnDate, debit to Chase Trust Checking 9967 - STR)` — see `je/post.existing_payouts`,
which both builders' `--skip-existing` also reuses rather than re-implementing.

There are three layers, and all three earn their keep: DocNumber already in QBO;
the content key against what is already on the books; and an in-run `posted` dict,
because two batches in one CSV can share a date and amount.

## Two-step clearing: a payment and a payout JE, never both to the bank

The model for channel money is:

    Payment      Dr Payments Clearing - <channel>   Cr A/R      (per reservation)
    Payout JE    Dr Chase Trust Checking 9967       Cr Clearing (per payout batch)

The bank deposit in the feed matches the **JE**. If a reservation Payment deposits
straight to Chase 9967 *and* a payout JE debits Chase 9967, the cash is counted
twice — this happened, for **$55,972.41**, and a duplicate check that looked only at
JournalEntry and Deposit objects did not catch it because the deviation was in
`Payment` objects.

**When checking for double-counted cash, query Payment, Deposit, JournalEntry and
Purchase.** A clean JE ledger proves nothing on its own.

Misrouted payments come from **manual Receive Payment entries** inheriting a bank
account default — 29 of 30 lacked `PaymentRefNum` where 551 of 579 synced ones had
it. The sync is correct; the hand-entered ones are the deviation.

## A booking's payment must equal its net exposure across ALL JEs, not one payout

A refund often lands in a *different* payout batch from the reservation. Reconciling
one JE against one invoice manufactures gaps that are not there (this produced a
false $96.07 discrepancy; the guest simply had two JEs for one payout, $654.62 +
$96.07 = the $750.69 invoice exactly).

Payments **debit** clearing. So fixing an understated payment makes the clearing
balance go **up**, not down — a sign error here sent a whole reconciliation the
wrong way once.

## Refund routing: cancelled goes to clearing, partial goes to Resolutions

- **Cancelled booking** — the reservation credit and the refund debit both sit in
  clearing and wash. The stay never happened, so no owner payable arises.
- **Partial refund on a stay that happened** — the refund reduces what the owner is
  owed, so it belongs on
  `Trust Liabilities:Owner Payables:1A - Net Earnings:Resolutions`.

`fixes/refunds_to_resolutions.py` implements both directions (`--cancelled-back`
reverses).

## JE line reads carry the FULLY QUALIFIED account name

A JE line's `AccountRef.name` is the full path
(`Trust Assets other than Cash & A/R:Payments in transit to Trust:Payments Clearing - Airbnb`),
so `== "Payments Clearing - Airbnb"` silently matches **nothing**. Compare with
`.endswith()`. `config/accounts.yml` records the Id and the full name side by side
for exactly this reason.

## Account Ids live in config/accounts.yml, never as literals

A wrong Id posts real money to the wrong account and QuickBooks will not complain.
`python -m src.verify_accounts` checks every Id and name against the live company
file; it is a pure read. **Run it after editing `accounts.yml` and before a posting
session.** It has already caught one wrong name.

## Writing: always build the path through `qbo.post(entity, body)`

Passing a bare `"journalentry"` to `request` makes `requests` treat it as a
hostname and fail with a DNS error that looks like a network problem. `post()`
builds `/v3/company/<realm>/<entity>`.

QBO **full-updates blank any field the payload omits**, so an update must carry
`DocNumber` explicitly and the `SyncToken` just read back — `je/payload.je_update`
is that shape, in one place.

## Hawaii pass-through tax credits clearing for the FULL payout

`Pass Through Tot` (Keaau transient-accommodation tax) is part of the Airbnb payout
and the reservation invoice already recognises it under `Guest Charges:Tax`.
Routing it to Tax Liability in the payout JE strands it in clearing, which the
payment then cannot clear. Owner decision: credit clearing for the full payout.

## Reading Owner_statement_whole — only through `src/bridge.py`

The dependency runs one way. Keep every crossing in `bridge.py` and give each
consumer a `--statements-root` override. Do not copy the class map, the Guesty
exports, or `to_property_id` into this project — they drift the moment a listing is
renamed. `to_property_id` lives in `ltr/labels.py` over there, kept dependency-free
(only `re`) so importing it does not drag pandas across the boundary.

Guesty has a budget of **5 API tokens / 24h** shared across projects, so the stored
exports under `inputs/<period>/` are the only affordable source of stay dates.
**Do not add a Guesty API pull here.**

## The OAuth token: one copy, owned here

Intuit rotates the refresh token on every refresh. Two copies on disk invalidate
each other, so `config/secrets/qbo_tokens.json` is the single store and
Owner_statement_whole's `paths.QBO_TOKENS` points into this project. It may refresh
(that mutates the token, not the books) — it writes back to the same file.

**Never duplicate that file**, and never add a token path to the statement project.

## Bank-feed undos are the owner's job

Undoing a categorised bank-feed transaction is not available through the QBO API.
When a fix needs deposits undone, say which ones and wait — the owner does them in
the UI, then the scripts run.

## Zelle lump-sum invoice payments are an Expense, not a JE

One Zelle transfer pays several invoices. `inputs/Invoice_payment/Zelle_payment_tracking.xlsx`
records them one row per invoice; the `Zelle content` column is what groups the rows
of a single transfer and `JE Amount` is that transfer's total. Owner decision: book
it the way the hand-entered ones already are — a **Purchase** paid from Chase 9967
with one line per invoice (see Purchase 116690, Andrea Brannon 2026-08-29, $1,487.69
over 13 lines) — so the bank feed matches it as it matches those. `src/invoices/`.

The lines must add up to `JE Amount` or nothing posts. That column is the owner's
own record of what left the bank; deriving the total from the lines instead would
make a missing invoice row invisible.

**Category -> account comes from the workbook's `account_map` sheet, not from
`accounts.yml`.** The owner maintains it beside the data. Every name is still
resolved against the live company file, so a typo fails loudly rather than posting
to the wrong account. `Landscaping` and `Maintainence` both map to Maintenance -
Owner on purpose.

The duplicate key is `(TxnDate, amount paid from Chase 9967)` over **Purchase**
objects, and it matches within **±3 days**: the bank feed books the same transfer
under its own `Zelle payment to <name> JPM...` memo carrying no reference of ours,
and a transfer can post a day or two after the date on the sheet.

Payee names on the sheet are abbreviated ("Andea"). `config/payees.yml` maps them to
Vendor DisplayNames; an unmapped name is a WARN that blocks the post, never a guess.

### The `<date>_Expense2qbo.xlsx` layout

The newer sheets have no `Amount` or `Property` column; both live in the `filename`,
`<date>_<Property>_<payee>_…_<line amount>_<transfer total>`. Only the two ends of that
name are fixed, so the builder reads the property from field 2 and the amount from the
second-to-last field, and WARNs if the last field disagrees with `JE Amount`. The map is
`config/Account Mapping.csv` when the workbook has no `account_map` sheet; categories
match case-insensitively (the sheet writes both `Cleaning payout` and `Cleaning Payout`).

- **`Cleaning payout` means `Cleaning Fee Revenue:Cleaning Payout` (Id 1534)**, not the
  liability that is literally named `Cleaning payout` (Id 1697). 157 of Camila's 159
  cleaning lines used 1534; the mapping row said 1697 until 2026-09-12.
- **Hot tub**: a line mentioning a hot tub splits into $30 → Maintenance - Owner and the
  remainder → its own category (`config/line_splits.yml`, owner decision 2026-09-12).
- Property names that are not a listing — `All Units` → Valta Realty, a bare
  `Seattle 10057` → Seattle 10057 Whole — are in `config/property_classes.yml`.

**The DocNumber is `<date>_<payee>_<total>`, not the `Zelle content`.** The long
reference truncated to 21 chars made `20260911_Camila_Cleaning Inv349-351_605` and
`20260911_Camila_Cleaning Payout_…_580.00` both `20260911_Camila_Clean`, and the
DocNumber check would have refused the second transfer as already posted. The full
reference goes in the memo. The same scheme reproduces `20260908_Andea_455.93` exactly.

The duplicate key includes the paid-from account — `(bank, TxnDate, amount)` — because
the review CSV's `PaidFrom` is now what the poster uses.

**All supplies are paid from the trust account** — including company-wide
`All units supplies` (class Valta Realty). That is owner policy (2026-09-12), so the
default `PaidFrom` of Chase Trust Checking 9967 - STR is correct for them; do not flag
it as company money spent from trust or suggest repaying it from 7197.

## Fee rebills credit their OWN account, not the Supplies one

`Valta Realty - Channel Fee` bills credit
`Billable Expense Income:Billable Expense Income - Channel Fee` and `- Stripe Fee` bills
credit `- Stripe Fee`. Only genuine supplies rebills (`Valta Realty - Supplies`) may
credit `- Supplies Owner`.

This is not cosmetic. Owner_statement_whole categorises a ledger line by matching a
regex against the **account name**, and `(?i)suppl` matched
`Billable Expense Income - Supplies Owner` — so the offsetting half of every fee bill was
filed under the owner's *Supplies* line while its matching charge went to Other Expense.
718 lines, $22,828.07, 73 properties. Both fee accounts already existed and were unused.

`fixes/fee_rebill_accounts` repointed 17,428 bills (2024-2027) on 2026-09-10. All three
accounts sit under the same `Billable Expense Income` parent, so no P&L subtotal moved
and no bill total changed — the script aborts on the first `TotalAmt` that does.

A Bill is FULL-updated by reading the object back and echoing it with one `AccountRef`
changed. Never hand-build a Bill payload: QBO blanks every field the payload omits, and a
Bill carries `APAccountRef`, `DocNumber`, `DueDate`, `LinkedTxn`, per-line `CustomerRef`
and `ClassRef` that are invisible until they are gone.

**`qbo.post()` already unwraps the entity** — it returns the object, not `{"Bill": {...}}`.
Indexing `["Bill"]` on the result turns a SUCCESSFUL write into an exception, which once
logged thousands of good writes as failures.

### The fee Bills are generated upstream, and the generator is still wrong

This project has never created a Bill. The `Valta Realty - *` fee Bills come from an
external integration (DocNumber prefixes such as `YKY6_`, `RZ3O_`, `9Z5X_`) and it is
**still running**: fee Bills created after the 2026-09-10 repoint credit plain
`Billable Expense Income` (Id 1493), not the named accounts. So the repoint is a
point-in-time fix — re-run it until the integration's account mapping is changed:

    python -m src.fixes.fee_rebill_accounts --from billable_expense_income --confirm   # both vendors

Plain `Billable Expense Income` does not match `(?i)suppl`, so these stragglers do not
re-create the Supplies misfiling on owner statements; they are only off-convention.

Two more things that generator does, both left alone because neither moves money:
- Every fee Bill's A/P account (`APAccountRef`) is **Insurance Liability** (1600), not
  Accounts Payable. Harmless while the Bills total $0.00 — which all 38,291 do.
- 198 cancelled-reservation Bills created 2026-09-03 are labelled `Tripcom Channel fee`
  although only ONE is a Trip.com booking: 93 are Airbnb (`HM…`), 54 Booking.com
  (10-digit), 31 VRBO (`HA-`), 14 direct (`GY-`). All $0.00. The reservation code in the
  memo is the truth; the label is not.
