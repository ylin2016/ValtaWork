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

### Which model applies depends on channel AND year

The two-step model is not universal back in time. Before 2026, **Booking.com had no payout
JEs**: in 2025, 457 of 458 Booking.com reservation payments were deposited straight to
Chase 9967 and reconciled there, and not one JE touched Booking.com clearing. Airbnb was
two-step throughout (2025: 1,807 payments to Airbnb clearing, 870 payout JEs).

So a catch-up payment for an old unpaid invoice goes where its channel and year put it:

| Channel / period          | Reservation payment deposits to   | Bank line matches    |
|---------------------------|-----------------------------------|----------------------|
| Airbnb, any year          | Payments Clearing - Airbnb        | the payout JE        |
| Booking.com, 2024 – 2025  | **Chase Trust Checking 9967**     | the payment itself   |
| Booking.com, 2026 on      | Payments Clearing - Booking.com   | the payout JE        |

A 2025 Booking.com payment moved into Booking.com clearing strands its amount there —
there is no payout JE to clear it against (Payment 118025, 2026-09-14).

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

`build_airbnb` does this correctly. But **an external integration also posts Airbnb
payout JEs** — same `M-…` Airbnb reference as DocNumber, memo
`External Payment ID: M-…`, created late at night (22:16 and 22:22 on 9/06 and 9/12) —
and it **still credits Pass Through Tot to Tax Liability**. It is not a scheduled task
here and nothing in `review/CHANGE_LOG.csv` created them. So every Keaau payout it books
needs `fixes/passthrough_tot` afterwards; re-run the dry run to find new ones.

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
logged thousands of good writes as failures. It matches the response key
case-insensitively: it used to capitalise only the first letter, so `journalentry`
looked for `Journalentry`, missed QuickBooks' `JournalEntry`, and `je/post` crashed
reading the Id of a JE it had just written (JE 117999, 2026-09-14). **After any crash in
a posting run, check QuickBooks for what was written before re-running.**

## The class map is not proof a class exists

Owner_statement_whole's `mapping_classes.yml` records some listings one level too
shallow — `Listings:Yacinde E1` for `Listings:Yacinde NuGrowth:Yacinde E1`, and
`Listings:Yacinde B3` for `Listings:Yacinde Holdings:Yacinde B3`. `build_bookingcom`
now checks every class against the company file (`Resolver.klass_fqn`) and writes the
real full name into the review CSV, WARNing when it corrected one. `build_airbnb` gets
classes a different way and does not do this check yet.

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

## The HOA's share of a cleaning bill is a RECEIVABLE, not an owner expense

One AA Professional Cleaners bill (`YacindeCL_<inv#>`, vendor Id 200000021) covers cleans
that two different parties bear.  The HOA is a third party that REIMBURSES -- not an owner
whose payable can be charged -- so its share is an asset until the HOA is invoiced and pays:

    owner's clean   1C - Owner Expenses:Cleaning Expense - Owner (1678)   class = the listing
    HOA's clean     HOA Receivable - Yacinde (1150040013)                 class = the listing

**As of 2026-09-17 these lines carry their LISTING class, not the flat one** (owner decision,
for per-unit reporting; `fixes/yacinde_hoa_classes --to listing`).  That leaves a known
exposure: until `qbo_sync` gates on the ACCOUNT, the next statement build charges the ten
Yacinde owners $11,160.00 for the HOA's cleans.  `--to hoa` reverses it in one run.  The
paragraph below is why the flat class existed and why it is still the safe state.

**The class matters as much as the account.** `Owner_statement_whole`'s `qbo_sync` ingests
EVERY Bill line carrying a mapped class and filters on nothing else -- there is no account
filter -- so a listing class on an HOA line charges that owner for a clean the HOA is
paying for, whatever account it points at.  `Yacinde HOA` (1000000041) is deliberately not
under `Listings:` and not in the statement project's `mapping_classes.yml`, so those lines
land in its exceptions table as CLASS_NOT_MAPPED and reach no statement.  The unit then has
nowhere to live but the Description: `07/13/2026 C1 Cleaning Fee`.

Which cleans are the HOA's is never INFERRED here, but there are now two sources that
STATE it, and which one wins is a per-tool decision:

| source                                                   | used by                              |
|----------------------------------------------------------|--------------------------------------|
| allocation workbook, `Invoice - HOA` / `Cleaning Allocation` | `fixes/yacinde_hoa_split`            |
| the expense sheet's own `Category` column                | `invoices/build_aa_cleaning` (default) |

The allocation workbook is `yacinde_expense/output/yacinde_expense_allocation_<period>.xlsx`
after the owner's hand corrections.  Read `Cleaning Allocation`, not `Invoice - HOA`, when
you need BOTH halves of the split: only it carries `hoa_paid` and `owner_paid` side by side,
and its `$0` rows are "fraction week, no clean" markers, not cleans.

`fixes/yacinde_hoa_split` repointed all four bills on 2026-09-17: 63 lines,
$11,160.00 of $14,760.00, leaving $3,600.00 on owners.  It is idempotent (a line already on
the receivable is skipped), so re-run it as each month's bill arrives.

Two gotchas it exists to survive:

- **A line description can contain a newline** (`"07/24/2026 Cleaning_Intensive cleaning do
  to marijuana\nsmoked"`).  A `.*$` parse stops at it and the whole match fails, so that
  line's date came back None and the $250 B6 clean matched nothing.  Use `re.S`.
- **The workbook and the bill can name different units for the same clean** -- 08/20 F1 vs
  C1, 08/22 F5 vs B3.  The two-way reconciliation was otherwise exact (83 lines, $14,760.00
  on both sides), so it is one clean under two labels and the HOA pays it either way; only
  the description text is in doubt.  The script relaxes the UNIT and flags it.
- **They can also disagree on the DATE, because a clean gets RESCHEDULED.**  On INV1200 the
  allocation carries F1 at 09/08 -- its own note reads `checkout at 9/8, rescheduled
  cleaning` -- where AA invoiced it on 09/11.  Refusing to pair them dropped the line and
  left the expense $180.00 short of the money that actually left the bank, so the bank feed
  would not have matched.  Owner decision 2026-09-19: **pair them**, under AA's invoice date.
  Unit and amount must still agree exactly, and the relaxation is the LAST pass, after both
  the exact match and the unit relaxation, so it can never steal a clean from a better pair.

Relax one field at a time and flag every relaxation; two at once is not a reconciliation.

### Valta pays the cleaner first, so the cash out is an EXPENSE, not a bill

The four bills above arrived as `Bill` + `BillPayment` (Check).  From INV1200 the shape is
the owner's stated model -- **expense first, then recover**:

    Expense    Dr HOA Receivable / Cleaning Expense - Owner   Cr Chase Trust 9967
    Invoice    Dr A/R (Yacinde HOA)                           Cr HOA Receivable

`invoices/build_aa_cleaning` builds the expense from the same `<date>_Expense2qbo.xlsx`
sheet the Zelle transfers come from, reconciles it two-way against the allocation workbook,
and writes the ordinary review CSV -- so `invoices/post` posts it with no special case, and
the duplicate check, the DocNumber rule and the change log all apply unchanged.  Pay it from
**Chase Trust 9967**: that is where the past AA BillPayments came from (117673, 117675) and
where the HOA's reimbursement deposits back to.

**The owner's share needs no separate bill.**  `Cleaning Expense - Owner` is an owner
payable, so the statement pipeline charges it on its own; "bill the owner for their portion"
means put the line on that account, not raise a second document.

Two things this changed, both owner decisions of 2026-09-19 that OVERRIDE rules stated
absolutely above -- read them together, not separately:

- **The expense sheet's `Category` column outranks the allocation workbook** for who bears a
  clean (`--split sheet`, the default; `--split allocation` is the reverse).  The owner fills
  that column in by hand per invoice, knowing what they intend to recover.  On INV1200 it put
  all 25 cleans on the receivable where the allocation gave 2 ($360.00, B3 on 09/03 and
  09/07) to Yacinde Holdings.  This does NOT retire the 2026-09-17 "the allocation workbook
  is right" call -- that still governs `fixes/yacinde_hoa_split`, which repoints BILL lines
  and has no hand-marked column to read.  The disagreement is always printed, never silent.
- **Know what `--split sheet` costs.**  The HOA is then invoiced for the full amount, and
  anything it declines to reimburse strands on the receivable instead of reaching the owner
  who bore it.  The receivable being the control account is what catches this: a non-zero
  balance after the HOA pays is the signal, so do not write it off, find the line.

Posted 2026-09-19: Purchase 118594, INV1200, $4,500.00 over 25 lines, all on the receivable.
The Zelle reference reads `..._4680_4500` while both the sheet and the allocation say
$4,500.00 -- **open**: if AA's paper invoice is $4,680.00 a 26th clean is missing from both.

`invoices/hoa_cleaning` is the other half: one Invoice per month to Customer `Yacinde HOA`
(100000031), one line per clean, through the Service item `HOA Reimbursement - Cleaning`
(182) whose income account IS the receivable.  Recovering a cost is not revenue, so nothing
reaches the P&L -- and an item in this file may point at a balance-sheet account, which is
the house pattern rather than a workaround (`Guest Charges:Tax` credits Rental Taxes
Payable).  A margin, if one is ever charged, is a separate line against a real income
account.  An invoice LINE has no account of its own: the credit account comes from the item,
which is why an item has to exist at all.  Class tracking here is per transaction LINE
(`ClassTrackingPerTxnLine`), so the class goes on the line, not the item -- not one of the
50 items carries a ClassRef.

Posted 2026-09-17: HOA-2026-07 (118386, $4,550.00) and HOA-2026-08 (118387, $6,610.00),
which took the receivable to $0.00.  **The receivable is the control account** -- $0.00
means everything laid out has been invoiced; a balance means cleans paid for and not yet
billed on.  It stands at **$4,731.06** as of 2026-09-19 and that is expected, not a fault:
$4,500.00 of INV1200 plus $231.06 of pool items, all awaiting HOA-2026-09.  The HOA's payment
deposits to **Chase Trust 9967**, the account that paid the cleaner: reimbursement landing
in 7197 leaves the trust permanently short.

### Cleaning is not the only thing the HOA bears

Pool chemicals, a pool hook, common-area maintenance -- these have no allocation workbook
behind them.  They arrive one Amazon purchase at a time and get categorised to an owner
expense account by whoever enters them, which charges the Yacinde owners for a cost the
HOA reimburses.  Two scripts, and the order is not optional:

    fixes/hoa_recategorize --ids 117982,117983     1C - Owner Expenses:Maintenance - Owner
                                                   -> HOA Receivable - Yacinde, class Yacinde HOA
    invoices/hoa_maintenance --into HOA-2026-08     bills whatever sits on the receivable

**The receivable IS the source list.**  There is nothing to maintain: a line categorised to
`HOA Receivable - Yacinde` is by definition money laid out and not yet billed on, so
`hoa_maintenance` reads the account's open debits rather than a workbook tab or a hardcoded
set of Ids.  It skips AA Professional Cleaners and `YacindeCL_` bills, because
`invoices/hoa_cleaning` owns those and double-billing them is the obvious failure mode.

Recategorising FIRST is what makes a cost billable at all.  Invoicing a line still sitting
on `Maintenance - Owner` credits an asset that was never debited: the receivable goes
negative and the owners keep carrying the money.

**Which purchases the HOA bears is never inferred, and `pool` in a description is not a
rule** -- `hoa_recategorize` takes explicit Ids from the owner, exactly as the workbook
decides which cleans are the HOA's.  Both scripts are reversible and abort on the first
TotalAmt that moves; only AccountRef and ClassRef change.

These lines carry class **`Yacinde HOA`, not a listing**: the pool is common area and
belongs to no single unit, so there is no listing class to give them.  That also keeps the
purchase and the invoice line on the same class, which is what makes the two sides
reconcile.  The item is `Owner Charges:HOA Reimbursement - Maintenance` (184), income
account 1150040013 -- the receivable, so nothing reaches the P&L, same as the cleaning item.

`hoa_maintenance --into <DocNumber>` APPENDS to an existing invoice rather than raising a
new one, reading it back and echoing it whole: an Invoice carries `BillAddr`,
`TxnTaxDetail`, `CustomField` and per-line `ItemAccountRef` that QBO blanks if the payload
omits them, and the new lines go in BEFORE the `SubTotalLineDetail` line.  It aborts unless
the new total is the old total plus exactly what was added.

2026-09-19: $352.78 of pool chemicals appended to HOA-2026-08 ($6,610.00 -> $6,962.78, 40
lines), and the two September pool purchases (117982, 117983, $231.06) recategorised --
leaving the receivable open at exactly $231.06, awaiting HOA-2026-09.

**These were paid by Business Credit Card (4783), not the trust.**  The cleaning on the same
invoice came out of Chase Trust 9967, so one HOA payment against a mixed invoice puts money
into the trust that the company actually spent.  Open question, not yet decided: whether
that share moves back to the company afterwards.

## Cleared / reconciled status: use the General Ledger report, never TransactionList

`reports/TransactionList` has **no `account` parameter** — it silently ignores one and
returns the whole company, and its `Clr` column then misreports bank-line status. It
once showed a week of Chase 9967 payout JEs as "uncleared, never matched" when every one
was reconciled. `reports/GeneralLedger` with `account=<Id>` does filter, and its
`is_cleared` column (`Clr`: blank / C / R) is the truth for that account.

A bank-feed line in *For Review* whose transaction is already **R** will never appear in
Find match — QuickBooks does not offer reconciled transactions. Such a line is a leftover
of a period reconciled in the register without matching the feed: **Exclude** it. Adding it
would put a second deposit into a period that already balances.
