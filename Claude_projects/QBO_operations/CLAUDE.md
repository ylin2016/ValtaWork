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

**A row's invoice can be months older than the transfer, and the Expense is dated by the
TRANSFER.** The sheet records what was reimbursed, not when the work was done: the
2026-09-26 batch paid a 06/19 Bellevue 321 repair ($71.51) and an 08/18 Redmond 11641 clean
($40.00) alongside eleven September lines. That is correct — the cash left on the 26th, and
the bank feed matches on the date and amount — but those costs then reach the SEPTEMBER
owner statement, not June's or August's. So a rebuilt June or August statement will not
contain them, and a per-month expense reconciliation against service dates will never tie.
Read the date out of each `filename` field before assuming a batch is all one month.

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

### The fee Bills are generated upstream, and the generator has been FIXED (2026-09-26)

This project has never created a Bill. The `Valta Realty - *` fee Bills come from an
external integration (DocNumber prefixes such as `YKY6_`, `RZ3O_`, `9Z5X_`). It used to
credit plain `Billable Expense Income` (Id 1493) on everything it wrote after the
2026-09-10 repoint, which made that repoint a point-in-time fix needing re-running.

**Checked 2026-09-26: it no longer does.** All 2,588 fee Bills dated on or after
2026-09-01 credit their own named account — 750 Channel Fee (1150040011), 365 Stripe Fee
(1150040012), 574 Supplies Owner (1695) — and **zero** sit on 1493. So the integration's
mapping was corrected upstream and `fixes/fee_rebill_accounts` is no longer a standing
chore. Keep the script: it is the fix if the mapping ever regresses, and re-checking costs
one query.

    python -m src.fixes.fee_rebill_accounts --from billable_expense_income            # dry run: expect 0

Verify before assuming, in either direction — a count of zero stragglers means nothing
unless the generator is also still producing Bills, which is why the check above counts
both. `review/fee_rebill_done.csv` is the original run's resume ledger and
`fixes/fee_rebill_accounts` READS it to skip Bills it has already repointed, so that file
is a dependency, not an artefact: do not tidy it away.

Plain `Billable Expense Income` does not match `(?i)suppl`, so any future straggler does
not re-create the Supplies misfiling on owner statements; it would only be off-convention.

Two more things that generator does, both left alone because neither moves money:
- Every fee Bill's A/P account (`APAccountRef`) is 1600, not the default Accounts
  Payable. Harmless while the Bills total $0.00 — which all 38,291 do. (1600 was named
  *Insurance Liability*; as of 2026-09-20 it is **`PMS Clearing – A/P`**, type Accounts
  Payable, and it is what every owner-payout Bill uses too. The name carries an EN DASH.)
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
Bill lines by their mapped class, so a listing class on an HOA line puts that clean in front
of an owner's statement whatever account it points at.  (It DOES also gate on the account --
`expense/qbo_sync.py` calls `account_is_owner_side` in the Bill branch and files a rejected
line as `ACCOUNT_NOT_OWNER_SIDE`; an earlier note here saying there is no account filter was
wrong.  Do not lean on it as the only defence: rows ingested before the gate existed are
still flagged `include_in_statement=1`, so the class remains the safety net that has actually
held.)  `Yacinde HOA` (1000000041) is deliberately not
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
(100000031), one line per clean, through the Service item `Owner Charges:HOA - Cleaning Fee`
(182, renamed from `HOA Reimbursement - Cleaning`) whose income account IS the receivable.  Recovering a cost is not revenue, so nothing
reaches the P&L -- and an item in this file may point at a balance-sheet account, which is
the house pattern rather than a workaround (`Guest Charges:Tax` credits Rental Taxes
Payable).  A margin, if one is ever charged, is a separate line against a real income
account.  An invoice LINE has no account of its own: the credit account comes from the item,
which is why an item has to exist at all.  Class tracking here is per transaction LINE
(`ClassTrackingPerTxnLine`), so the class goes on the line, not the item -- not one of the
50 items carries a ClassRef.

Posted 2026-09-17: HOA-2026-07 (118386) and HOA-2026-08 (118387).  Both were **re-split on
2026-09-25** against corrected workbooks -- $4,550.00 -> $1,930.00 and $6,610.00 -> $3,190.00
of cleaning; see "The allocation gets corrected" below.  **The receivable is the control
account** -- $0.00 means everything laid out has been invoiced; a balance means cleans paid
for and not yet billed on.  The HOA's payment
deposits to **Chase Trust 9967**, the account that paid the cleaner: reimbursement landing
in 7197 leaves the trust permanently short.

### The allocation gets corrected, and BOTH halves have to move (2026-09-25)

The workbook is generated and then hand-corrected, so the split that was invoiced is not the
final one.  A clean moves in EITHER direction, which is why `fixes/yacinde_hoa_split` does not
fit a correction: it only moves lines ONTO the receivable, reads `Invoice - HOA` (the HOA's
half alone) and queries **Bill** objects, and cleaning now arrives as an Expense.

Two scripts, and **neither is correct on its own**:

    fixes/yacinde_hoa_resplit      the EXPENSE lines -- repoints Purchase lines both ways
    invoices/hoa_cleaning_update   the INVOICE lines -- rebuilds cleaning, KEEPS everything else

Run the expense one FIRST.  In between, the receivable is temporarily negative rather than
overstated, which is the safer half-way state: an overstated receivable reads as money owed.

Lowering the invoice alone leaves the cost on the receivable, where it reads as "laid out and
not yet billed" when it is really "not the HOA's at all".  Repointing the expense alone leaves
the HOA billed for a clean it no longer bears.  Together they leave the receivable unchanged,
and that is the check: **if the receivable moves, one half is missing.**

- **`hoa_resplit` reads `Cleaning Allocation`, never `Invoice - HOA`** -- only it carries
  `hoa_paid` and `owner_paid` side by side, and a re-split needs to know where a clean goes,
  not merely that the HOA stopped paying for it.  `$0` rows are fraction-week markers and
  rows with no `invoice` were never billed by AA; neither reaches an expense line.
- **Only `AccountRef` moves.**  Both halves already carry the LISTING class (2026-09-17), so
  the class is not what distinguishes them and rewriting it would be a second, silent change.
- **`EntityRef` is not a queryable property on Purchase**, so the vendor cannot be the filter.
  The lookup is the DocNumber scheme `<yyyymmdd>_INV<aa invoice>_<total>` against the
  workbook's own `invoice` column, then the vendor is checked on the object.
- **`hoa_cleaning_update` keeps every non-cleaning line verbatim.**  Maintenance reaches an
  invoice from `hoa_maintenance`, which reads the RECEIVABLE and not a workbook, so the
  workbook cannot reproduce those lines -- rebuilding the whole invoice from it drops them.
  Both scripts full-update: `sparse` false, the object echoed whole.
- **The items were RENAMED** after the first invoices posted -- `HOA Reimbursement - Cleaning`
  is now `Owner Charges:HOA - Cleaning Fee` (still Id 182, still crediting the receivable).
  `hoa_cleaning`'s old constant resolved to nothing, and `--create-missing` would then have
  raised a SECOND item against the same account.

2026-09-25, July + August: 34 lines over Expenses 118602/118603/118604/118605 moved
**$6,040.00** off the receivable onto owners -- all NuGrowth whole-owner units (E3 $1,390,
F5 $1,260, C1 $1,210, B6 $1,150, E1 $1,030) -- and invoices 118386/118387 came down by the
same $6,040.00 ($4,550.00 -> $1,930.00 and $8,898.78 -> $5,478.78, the latter keeping its
$2,288.78 of maintenance).  Every expense total unchanged, all four tie to their workbook
line-for-line with no relaxation, and the receivable ended where it started at **$2,795.06**.

**INV1200 was deliberately not touched**: it is September cleaning and there is no `202609`
workbook yet.  Its full $4,500.00 still sits on the receivable awaiting HOA-2026-09.

**Open: $1,936.00 of the receivable is a credit with no debit.**  HOA-2026-08 carries a
`Derrick Holm 8.15-8.31 wage` line that was invoiced but never posted TO the receivable, so
invoicing it credited an asset that was never debited -- the failure mode named above.  The
$2,795.06 balance is therefore understated: genuinely unbilled is **$4,731.06** ($4,500.00
INV1200 + $231.06 pool).  Book the wage to the receivable and the control account means what
it says again.

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
reconcile.  The item is `Owner Charges:HOA - Maintenance` (184, likewise renamed), income
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

### AA cleaning is an Expense, not a Bill (from 2026-09)

Owner decision 2026-09-19: AA Professional Cleaners cleaning is entered as an **Expense**
paid from Chase Trust 9967, because that is what happens -- the invoice is paid on arrival,
so parking it in A/P and paying it back out the same week adds a step and nothing else.
The first one entered this way was `20260917_INV1200_4500` (Purchase 118594).  DocNumber
scheme: `<yyyymmdd>_INV<aa invoice>_<total>`.

The four historical Bills were converted by `fixes/bill_to_expense`, which CREATES the
replacement and leaves the delete to the owner -- QBO has no "convert", deleting is not
something this project does, and QBO refuses to delete a paid Bill until its payment goes
first.  **Create before deleting.**  In between the amount is on the books twice and
plainly visible; the other order leaves the HOA receivable short while the HOA invoices
still clear debits that no longer exist, which is a far worse place to stop half way.

| Bill | AA inv | was | Expense | now |
|------|--------|-----|---------|-----|
| 116622 | 1167 | 2026-07-31 $4,660.00 | 118595 | 2026-08-03 |
| 116623 | 1173 | 2026-07-31 $250.00   | 118596 | 2026-08-03 |
| 116789 | 1180 | 2026-08-31 $3,370.00 | 118597 | 2026-08-18 |
| 116696 | 1186 | 2026-08-31 $6,480.00 | 118598 | 2026-09-01 |

**An Expense has ONE date where a Bill had two, and the choice moves money between owner
statements.**  These Bills were all dated month-end; the cash left days later.  `--date
payment` (chosen) re-matches the bank-feed line released by deleting the Bill Payment, but
moved $360.00 of owner-borne cleaning from July to August and $2,880.00 from August to
September -- so July and August owner statements no longer reproduce and need rebuilding.
`--date bill` keeps every line in its month and makes the bank line match a few days off.
The dry run prints the movement under each; decide from that, never from the flag name.

**The subtotal guard compares Ids, never names.** QuickBooks renders the same class
differently depending on which object is asked: Bill 116623 reports
`Listings:Yacinde NuGrowth:Yacinde B6`, the Purchase built from it reports `Yacinde B6`,
both Id 1000000030.  Comparing names rejected a Purchase that was correct in every respect
and aborted the run after it had already been written.  Same family as the JE
`AccountRef.name` trap above.

`fixes/yacinde_hoa_split` still queries **Bill** objects only.  Now that cleaning arrives as
an Expense it must read Purchase too, or each month's HOA split has to be done by hand --
which is how INV1200 was entered, and it put all 25 lines on the receivable when the
allocation workbook says the HOA's share is 23 cleans / $4,140.00.

#### A Purchase's PaymentType is what it IS, and it is final

`Cash` renders as an **Expense**, `Check` renders as a **Check**, `CreditCard` as an Expense
on a card.  The posting is identical -- same accounts, classes, amounts, bank -- but the
label, the register icon and the internal TxnType (`PurchaseEx`: 54 vs 3) are not.

**It cannot be changed afterwards, and QuickBooks does not say so honestly.** A full update
answers `610 Object Not Found: Something you're trying to use has been made inactive`, which
names no field and sends you hunting for a deactivated account, class or vendor -- every one
of which was active.  A SPARSE update is worse: it returns 200 with the old value intact, so
it looks like it worked.  Forcing `PurchaseEx.TxnType` fails the same way.  The only fix is
to create a new object and delete the old one.

So `--paytype` is decided in the dry run, not afterwards.  `fixes/bill_to_expense --clone
<purchase ids> --paytype Cash` exists because that lesson cost four objects: the first
conversion mirrored each Bill's `BillPaymentCheck` and produced four Checks.  It carries
everything over except the server-owned fields (`Id`, `SyncToken`, `MetaData`, `PurchaseEx`,
`PrintStatus`, and the per-line `Id`s) and verifies the new object reproduces the old
totals and per-account/per-class subtotals before printing what to delete.

Final chain, 2026-09-19: Bill 116622/116623/116789/116696 -> Check 118595/118596/118597/118598
-> Expense **118602/118603/118604/118605**.  The Bills and their Bill Payments were deleted by
the owner; the Checks are deleted last, after the Expenses exist.

## Booking.com commission: billed per reservation, invoiced per PROPERTY

Commission touches the books three times and all three should agree:

    billed to the owner   9Z5X_<reservation> Bill, DEBIT 1A - Net Earnings:Channel Fees
                          (a $0.00 rebill -- the owner is charged, Valta books the matching
                          Billable Expense Income - Channel Fee, so Valta nets to zero)
    invoiced by Booking   the monthly commission invoice, ONE ROW PER PROPERTY
    paid in cash          Purchase line, DEBIT Fee - Processing & Commission:
                          Fee - Booking.com Commission (1602), out of Chase Trust 9967

**A bank charge from Booking.com is COGS (1602), never a `1C - Owner Expenses` account.**
The owner was already charged at reservation time; booking it as an owner expense charges
them twice.  This is also why the statement account gate correctly drops 1602.

`reconcile/bookingcom_commission` is the three-way report (read-only).  Four things it
exists to get right:

- **The invoice is per PROPERTY**, so a per-reservation match is impossible from the invoice
  side however much one wants it.  The reservation detail is only on OUR side, in the
  `9Z5X_<reservation>` DocNumbers, so the report compares at property level and lists the
  reservations behind each row.
- **Join in two hops**: invoice `ID` -> NICKNAME from the payout CSVs under `payment/`,
  NICKNAME -> property_id via `ltr.labels.to_property_id` through `bridge.py`.  That is what
  makes `Cottage 3` and `OSBR 3` one unit.  Never hardcode it; the map is the statement
  project's and a copy goes stale on the first rename.
- **Keep every `Invoice Type`, not just `Commission`.**  A file also carries
  `Customer complaint costs`, booked to the same 1602 (Purchase 114697 has one at $201.84
  beside a commission line).  Filtering them out made the invoice look $296.70 short of the
  cash that paid it.
- **Take the period from the invoice DATE, not the filename.**  Booking.com bills in
  arrears -- an invoice dated 2026-09-06 is AUGUST's commission, paid later still.  The
  same file has arrived named `..._2026-08.xlsx`, `..._20260919.xlsx` and
  `20260815_Booking.xlsx`: the stay month, the due date and the issue date.  Comparing cash
  from the bills' own month lines August's payment of July's commission up against August's
  bills and is pure noise.

#### The commission invoice is RE-EXPORTED IN PLACE, and it shrinks as rows get paid

`20260919_Booking commission invoice.xlsx` held 45 rows / **$10,221.81** on 2026-09-20 (27
`Paid` $5,967.16 + 18 `Overdue` $4,254.65).  The same filename now holds **17 rows /
$4,046.48** -- Booking.com re-issues the invoice showing only what is still outstanding, and
the owner saves it over the old one.  So the file is a snapshot of a MOVING document, the
same trap as Maria's workbook below, and two things follow:

- **Never reconcile "invoice total vs cash booked" from the current file.**  It reads
  $6,175.33 short of the $10,013.64 actually on 1602 for August, because the rows that paid
  are gone from it.  The books are the record of what was booked; the file only says what is
  left.
- **A row can vanish without ever being `Paid`.**  $208.17 of the original 18 Overdue rows is
  in neither export nor the books -- Booking.com dropped or credited it between issues.  That
  is the one difference worth chasing, and it is invisible unless the old total is written
  down.  Hence the figures above.

Statuses in the file are not a payment record either: all 17 remaining rows still read
`Overdue (due by 2026-09-19)` although the batch that paid them posted on 2026-09-24.  The
status is as of the export, so `--status all` is what posts a batch the bank has since
debited -- and the check that it really debited is the FEED, not the spreadsheet.

**`--group batch` DocNumbers as `<yyyymmdd>_BCOM_<total>`**, one Expense over every property
in the batch (`20260919_BCOM_4046.48`, Purchase 119389, 17 lines, memo
`Booking.com commission 2026-08, 17 properties`).  That is what makes the DocNumber check
able to refuse a second run, which is how a re-post of those 17 was caught on 2026-09-26 --
`--group property` would have produced 17 new DocNumbers and no collision.  Prefer `batch`
whenever the bank debited once.

**Reconcile at least two months together.** A 9Z5X_ bill is dated by the stay and the
invoice by Booking.com's cut-off, so a reservation on the boundary lands in different months
on the two sides and shows as equal and opposite gaps.  Jul+Aug 2026: $392.95 of apparent
difference cancelled exactly that way (elektra_1212 $200.14, osbr_4 $107.63, osbr_7 $37.72,
bellevue_1420 $237.87), leaving **$1,133.28 apparently real**.

**Shelton 310's $890.13 of that was the SAME artefact, and it is now closed (2026-09-26).**
The claim recorded here -- "invoiced $997.68 in July with no owner charged at all" -- was
wrong.  Bill **100893**, DocNumber `9Z5X_5813844607`, dated **2026-06-30**, charges the owner
exactly $997.68 to `1A - Net Earnings:Channel Fees` class `Listings:Shelton 310`, with the
matching $997.68 credit to `Billable Expense Income - Channel Fee`: a correct $0.00 rebill.
Its memo says why the two sides disagree -- **`2026-06-30 to 2026-07-07 | 7 nights`**.  The
stay straddles the month end, so the bill is dated by CHECK-IN in June while Booking.com
invoiced it in July.  Nothing was lost and no owner was under-charged.

The lesson is not new, it is that the warning above was not applied hard enough: a
two-month window is not sufficient when it is the WRONG two months.  July+August was
reconciled; this reservation needed June+July.  Before calling any residual real, find the
`9Z5X_<reservation>` bill by AMOUNT across all months and read its memo -- the stay dates
are in there, and one exact-cent match settles it faster than any period arithmetic.  The
rest of the $1,133.28 has not been re-examined this way and should not be quoted as real
until it has been.

Cash ties exactly where it can be checked: July's invoice $8,225.26 = August's cash
$8,225.26, property by property.  August's $10,221.81 invoice (27 rows Paid $5,967.16,
18 Overdue $4,254.65) was still unbooked as of 2026-09-20.

### The bank feed cannot be categorised through the API -- create the match instead

QuickBooks has no public endpoint for the *For Review* list, so no script here can touch a
downloaded bank line: not to categorise it, not to add it, not to write a bank rule.  The
only automation available is to put the right transaction on the books FIRST.  The feed
then offers it under **Find match** and the owner accepts it with one click instead of
choosing an account and a class 27 times.  A match keeps the transaction's OWN date, so an
Expense dated a few days off the real debit still matches cleanly -- the amount is what
identifies it.

**It cannot be READ either, so never report feed state as current.**  Nothing here can tell
whether a line is still For Review, already matched, or excluded -- only the owner can see
that.  A `review/CHANGE_LOG.csv` note saying "the 27 bank-feed lines are still For Review and
must be MATCHED" is a record of what was true the day it was WRITTEN, and restating it in the
present tense invents a status.  That happened on 2026-09-26: the six-day-old note was
repeated as live, read as Booking.com still being outstanding, and prompted a post of a
commission batch and four payout JEs that were all already on the books.  Say "as of
<date>", or say nothing about the feed.

What CAN be checked is the books, and that is what a question about a batch should be
answered from -- Purchase lines on the account, the payout JEs, and the poster's own
duplicate check, which reports `DocNumber already in QBO as Id <n>` in a dry run.  Check
before characterising, not after.

`invoices/build_bcom_commission` does this for Booking.com commission:

    python -m src.invoices.build_bcom_commission                     # newest invoice, Paid rows
    python -m src.invoices.post --all --csv review/bcom_commission_expenses_<period>.csv --confirm

Four things it has to get right, none of them guessable from the invoice alone:

- **Booking.com is a CUSTOMER here (Id 471), not a Vendor**, and these Expenses carry
  Location **`Trust`**.  A report that groups by payee splits in two if a new one disagrees.

  **This entry used to say `Valta Realty`, and saying so made it true 32 times.**  The claim
  "every commission Expense already on the books does" was read off the most RECENT ones --
  which were themselves the anomaly.  Counted over the whole file on 2026-09-26: **490
  commission purchases on Trust against 186 on Valta Realty**, and unbroken Trust from
  2025-04 through 2026-07.  The bad note set `LOCATION` in `build_bcom_commission`, which put
  the 27 September Expenses, the 119389 batch and four catch-ups on the wrong side;
  `fixes/purchase_location` moved 56 purchases / $19,747.25 back on 2026-09-26, after which
  all 191 of 2026's commission purchases are Trust.  The 2024 and early-2025 ones are
  genuinely mixed and were left alone.

  The lesson generalises past this account: **"what the books already do" is a COUNT, not a
  glance at the last few rows.**  A convention read off recent transactions reproduces
  whatever the last mistake was, and a default taken from a doc note nobody re-counted is how
  one wrong Location becomes thirty-two.
- **Only `Paid` rows have left the bank.**  August's invoice was 27 Paid / 18 Overdue;
  posting an Overdue row invents cash that has not moved.  `--status all` overrides.
- **Booking.com debits per PROPERTY** -- 2026-08-15 came through as 27 separate bank lines,
  one per invoice row -- so the default is one Expense per property.  It is not invariable:
  Purchase 114699 is a single $2,629.86 debit covering 11.  `--group batch` builds that
  shape, and the dry run prints the count and total so the feed can be compared against it.
- **The class takes two hops**, Property ID -> NICKNAME (`config/booking_Id.csv`) ->
  property_id -> QBO class, each verified against the live company file.  A direct leaf
  lookup is tried first; the statement project's map is only consulted when that misses,
  which is where `Cottage 3` -> `OSBR 3` lives.  A nickname reaching no class is written
  `*** UNMAPPED ***` and blocks the post.

`invoices/post` is shared, not forked: `DocNumber`, `EntityType`, `LineCustomer` and `Memo`
are optional columns that default to the Zelle behaviour.  That is what keeps the
content-based duplicate check -- `(bank, TxnDate, amount)` within +/-3 days over Purchase --
covering this builder too, which matters here precisely because the owner may *Add* a feed
line instead of matching it.

**`invoices/post` does NOT write `review/CHANGE_LOG.csv`** -- it has no code that touches the
file, so every Zelle, commission and form-expense entry in there was written by hand after
the run.  (`deposits/post_guest_fees` does append; the shared expense poster does not.  An
earlier note here saying "the change log applies unchanged" to this path was wrong about
that one thing.)  Log the batch yourself: the posted Ids are only in the run's stdout, and
once that scrolls away the review CSV cannot tell you which Purchase a line became.

## The monthly owner payout: a Bill AND its payment, one pair per bank line

The bank pays one ACH per property, so the books carry one pair per property:

    Bill          Dr 2 - Owner Distributions (Payouts) (1633)   Cr PMS Clearing - A/P (1600)
    BillPayment   Dr PMS Clearing - A/P                         Cr Chase Trust 9967   (Check)

**The BillPayment is the object the feed matches**, not the Bill. A Bill on its own leaves
the bank line uncategorised and an open A/P balance, so the pair is posted together and
`invoices/post_owner_payout` stops the whole run if a payment fails after its Bill posted
— naming the Bill Id, because half a pair is the one state worth interrupting for.

    python -m src.invoices.build_owner_payout --expect-total <the bank batch>   # review CSV
    python -m src.invoices.post_owner_payout --all --csv review/owner_payout_<period>.csv
    #   ... --confirm  to WRITE

### The bill is dated by the STATEMENT month, the payment by the bank (2026-09-26)

The payout settles a payable that belongs to the owner-statement month, but the ACH leaves
the bank days later.  Owner decision: **the Bill carries the statement month-end, the
BillPayment carries the bank's own date.**  September's 62 bills went out dated 2026-09-15/16
and were re-dated to **2026-08-31** on 2026-09-26 ($334,625.91) with
`fixes/bill_txndate`; their payments stayed put.

    python -m src.fixes.bill_txndate --from 2026-09-01 --to 2026-09-30 \
        --date 2026-08-31 --created-after 2026-09-20 [--confirm]

**Never re-date the BillPayment to match.**  It is the object the bank feed matches, and
moving it breaks a match already made.  A Bill dated before its payment is ordinary; the
reverse is not, so the script refuses a target date later than any linked payment instead of
creating one.  `DueDate` moves only with `--due` -- "raised 08-31, due 09-15" reads correctly
and nothing here groups by it.

Re-dating moves the distribution between months, so **any owner statement already built for
either month no longer reproduces** and has to be rebuilt.

The guard is a FINGERPRINT, not a total: totals, `APAccountRef`, `DocNumber`, vendor,
`LinkedTxn` and every line's account / class / customer / amount are captured before the
write and compared after it, because a full Bill update blanks whatever the payload omits and
those fields are invisible until they are gone.  Read the object back and echo it; never
hand-build a Bill payload.

Selecting by date alone is not enough -- other payout Bills already sat on the target date
(10 of them, $34,715.47).  `--created-after` is what keeps a re-date to the batch you meant,
and the verification afterwards reads back the exact Ids from the review CSV rather than
re-querying by date, which would count those strangers as successes.

### The sheet's `Paid From` column says which bank, and it is not optional

**The bank is not derivable from anything else on the sheet, and one month used two
accounts at once.** 2026-09: $270,868.91 and the individual lines debited Chase Trust
9967 - STR, while a $23,191.62 batch of six debited **Chase Trust 3038 - monthly** — and
Beachwood, which had been on 3038 since June, came back to 9967. Nothing in the register
predicts that, so the owner records it per row and `build_owner_payout` reads it.

A payment on the wrong bank is the hardest error in this project to see. The Bill is
right, the vendor is right, the amount and the date are right, the owner statement is
right, and `Balance = 0` says it is paid. Only the feed disagrees — by silently never
offering the payment under *Find match*, which reads as a QuickBooks fault rather than a
data one. It cost a full round of "where are the bills?" on 2026-09-20.
`fixes/repoint_payment_bank --ids <...> --to "<bank>"` moves just
`CheckPayment.BankAccountRef` and re-reads every object to prove the write landed.

**Column roles come from the HEADER, never from position.** The sheet was one column,
then grew a batch column, and then grew `Paid From` IN THAT SAME POSITION — so a builder
counting columns read every bank name as a batch label and carried on, because a batch
nobody named is not an error. `read_sheet` matches the header text (`Paid From` / `Bank` /
`Account`, or `Batch`) and **refuses a header it does not recognise**: a column the owner
took the trouble to fill in is not something to ignore.

The spelling is the bank statement's, not the company file's — `Chase Trust 9967 - STR`
against `Chase Trust Checking 9967 - STR`, or just `3038`. `resolve_bank` tries the exact
name, then containment, then every word appearing somewhere in the account name, and each
pass demands a UNIQUE hit. `Chase Trust` matches both trust accounts and is therefore
**refused, never picked**. An unresolved bank is written `*** NEEDS BANK: ... ***` into
`PaidFrom`, which blocks the post exactly as an unresolved vendor or class does.

`--bank <name>` supplies it for a sheet that has no such column, and `--batch
NAME=AMOUNT@BANK` for one carved out by `--only`/`--exclude`. Precedence: the row's own
column, then its batch's `@BANK`, then `--bank`, then the built-in default.

The build now prints what the feed will be asked to match, per account, and **each group
must be a real debit in THAT account's feed**:

      paid from                              rows            total
      Chase Trust Checking 3038 - monthly       6        23,191.62
      Chase Trust Checking 9967 - STR          57       319,145.81

Cross-check it against the books with `reports/GeneralLedger`, `account=<Id>`, the `Clr`
column: uncleared bill payments per bank must equal that bank's outstanding line.

### Beachwood is USUALLY paid by direct ACH — check before you exclude it

`Valta Beachwood LLC` has been paid by an ACH that the bank feed books as an **Expense
straight to 1633**, every month for two years: 2026-06-16 $15,455.53, 2026-07-16
$19,522.74, 2026-08-20 $27,492.92 (Purchases 107241 / 110850 / 114906). When one of those
exists, a `Beachwood` row on the payout sheet is the same cash a second time.

**September 2026 was not one of those months, and assuming it was cost an afternoon.**
"Exclude Beachwood, it's already posted" (owner decision, 2026-09-20) was taken on a
reported ACH Expense of $20,952.13 dated 2026-09-15 that **did not exist** — no Purchase
of that amount anywhere in 2026, no Purchase line on 1633 in September at all. The payout
was unbooked, and excluding it left it that way. Posted the same day as an ordinary pair,
Bill 118842 + BillPayment 118843.

So: Beachwood is excluded when the ACH is **on the books**, not because it is Beachwood.
`post_owner_payout` prints the count it found — `N direct-ACH payout Expense(s) already
on the books` — and `0` there means nothing is being double-paid and the row should post.

**The ACH does not always leave the same account.** It came out of Chase Trust 9967 - STR
through 2026-05, moved to Chase Trust 3038 - monthly for June–August, and the owner
confirmed September is back on 9967. The register is therefore not evidence for the
current month: ask, because only the BillPayment carries the account and it is what the
feed matches. The Bill half is bank-independent — Dr 1633 / Cr PMS Clearing – A/P — so
this is the only place the answer shows up.

This is still why the duplicate check reads **Purchase**, not just Bill and BillPayment:
an ACH Expense is invisible to a query over the other two, exactly as the $55,972.41 of
double-counted channel cash was. The check is keyed `(Vendor, amount)` across the window
rather than on an exact date, because the ACH carries the bank's date and the sheet
carries another.

### The sheet is the bank, and the statement is only a check against it

`inputs/owner_payout/Payout_<period>.xlsx` is one column of the same filename-style
reference the expense sheets use, `<yyyymmdd>_<Property>_<payee>_Owner Payout_<amount>`.
Two things about it are not negotiable:

- **The amount posted is the BANK's, never the statement's.** The builder prints every
  difference against `amount_due_to_owner` for the period and writes `StatementAmount` and
  `Diff` into the review CSV, so a payout that went out at the wrong figure is visible
  before it is booked. 2026-08: 34 of 52 tied to the cent; 18 did not, +$16,223.22 net,
  the large ones being Yacinde NuGrowth +$16,804.22 (a timeshare-pool distribution, not a
  per-listing statement) and Seattle 1502 +$4,679.93. It also lists properties with a
  balance and NO payout row — 18 of them for 2026-08, incl. OSBR $28,951.04 and Beachwood
  $23,832.13. Beachwood was paid $20,952.13 on 09-16, $2,880.00 under its statement.
- **The payee is the bank's ACH label, not a QuickBooks DisplayName** — `VALTA COJI LLC`,
  `Jing Zhou Chase`, `AFN SheltonAdvisorLLC`. `config/payees.yml` (shared with the Zelle
  builder) carries the spelling-only differences. A row naming TWO owners
  (`Huijing Tao Jing Zhou`, `Zhongyan Qiu Yuhui Mao`, `JiachenHuang ChenyuDai`) is never
  resolved to one of them by the script; it stays `*** NEEDS VENDOR ***` and blocks the
  post until the owner says which.

**A property paid as one ACH is not always a class.** Bellevue 2323, Burien 14407, Seattle
10057, Seattle 7434 and Seattle 906 have only per-unit classes, and the statement project's
`mapping_classes.yml` claims `Listings:Bellevue 2323` and `Listings:Burien 14407` exist —
they do not, the same "class map is not proof a class exists" trap. Owner decision
2026-09-20: **follow the register, do not create property-level classes.**
`config/payout_classes.yml` records which unit class each already sits on, and it is
deliberately SEPARATE from `property_classes.yml`: that file says a bare `Bellevue 2323`
*expense* is the Whole class, while the *payout* is on ADU. Merging them moves one.

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

## Expense form reimbursements feed build_zelle (2026-09-24)

`../expense_form` (the no-login receipt form) writes `Expense_processing/exports/zelle_expense2qbo.csv`
on billing@'s Drive after each weekly reimbursement run. `build_zelle --src <that .csv>` reads it:
the row's `qbo_account` is used as-is (the form's category map), `Paid from` is the bank, and a row
with `typed_in` set (a name/property/category typed under "Other", not yet mapped) is DROPPED with a
warning, so its transfer fails the JE Amount check rather than posting short. Same build -> review ->
`post --confirm` path as the hand workbook. The form never writes to QBO.

## Guest pet/parking fees from the expense form (2026-09-24)

`deposits/build_guest_fees` reads the form's exports (`submissions.csv`, `bank_deposits.csv`,
`config/deposit_types.csv`, `config/properties.csv` under billing@'s Drive
`Expense_processing/exports/`) and writes `review/guest_fees.csv`; `deposits/post_guest_fees`
posts it (dry run by default, `--confirm` writes). Per fee: customer by confirmation code
(`Resolver.customer("x - <code>")`); an invoice carrying the fee item with enough Balance ->
PAY_EXISTING, no invoice carries it -> NEW_INVOICE (`<code>-PET` / `<code>-PARK`, item
`Pet Fee (Owner)` / `Parking Fee`, class from the form's `qbo_class_name`), carried but paid ->
HELD (nothing drafted, owner decides). Payments -> **Undeposited Funds** (`names.undeposited_funds`),
then ONE Deposit per bank_deposits row into 9967 for exactly the bank line; a group with a HELD /
ERROR fee or a total mismatch is not deposited. Before a Deposit, the same amount already in 9967
within 3 days as a Deposit, a direct-to-bank Payment or a JE debit stops it. Re-runs find the
invoice (DocNumber), payment (customer+amount+linked invoice) and deposit (same linked payments)
instead of duplicating. Writes append to review/CHANGE_LOG.csv; Ids go to review/guest_fees_posted.csv.
Offline tests: `venv/bin/python -m unittest tests.test_guest_fees`. **Not yet run against live QBO**
-- first run `python -m src.verify_accounts` (checks the Undeposited Funds name).

## Card / bank purchases from the expense form (2026-09-24)

`invoices/build_form_expenses` reads the form's exports (`submissions.csv`, `attachments.csv`,
`config/money_accounts.csv`, `config/expense_categories.csv`, `config/properties.csv`) and writes
`review/form_expenses.csv` in the shared poster's shape; post with
`python -m src.invoices.post --all --csv review/form_expenses.csv [--confirm]`. One approved expense
= one Purchase, one line, matching the card feed's own Expenses (checked live 2026-09-24):
paid from `money_accounts.qb_account`, `PaymentType` **CreditCard** for kind `card` (post.py's new
optional column; Cash otherwise -- final once posted), vendor = the store through `payees.yml`
(case-insensitive; Costco -> Costco Wholesale, Amazon -> Amazon.com, Home Depot -> The Home Depot)
or an exact Vendor name, else `*** NO VENDOR ***` which blocks; line account = the category's
qb_account; class = `qbo_class_name`, else property_classes.yml / `Listings:<name>` / `<name>`;
Location **Trust** for a `Trust ...` account, else **Valta Realty** (what the books do); memo and
description = the receipt's Drive name; DocNumber `EF<submission id>`. Skipped: `personal` (that is
build_zelle), `in_qbo` FALSE (owner-paid, Valta Homes, Baselane), `typed_in`, already-posted rows.
The poster's content check (same account, amount, ±3 days) skips a purchase the card feed already
booked. Offline tests: `venv/bin/python -m unittest tests.test_form_expenses`. Live name resolution
checked with sample rows (read-only); **no live post yet**.

## Maria's cleaning: one JE per service month, a pure CLASS move (2026-09-26)

Maria Rangel's lump Zelle payments land on `Cleaning Fee Revenue:Cleaning Payout:Destiny's
cleaning` (1588) under class Valta Realty, so no listing carries its own cleaning cost.
`je/build_maria_cleaning` reads the per-month sheets of
`inputs/JE/Cleaning payouts/Maria cleaning payment process_copied20260117.xlsx` and builds one
JE per service month, dated month-end, DocNumber `MariaCL_<YYYY-MM>`:

    Dr ...:Destiny's cleaning (1588)   class = the listing    `Total for cleaner` per listing
    Dr Accounts Receivable (807)       class = Valta Realty   `Residential cleaning`,
                                                              Customer = **Valta Home**
    Cr ...:Destiny's cleaning (1588)   class = Valta Realty   the sheet's `Total payment`

**Both sides sit on ONE account, so the JE moves no money between accounts -- only between
classes** (owner rule, 2026-09-26).  That account is whichever holds the month's cash:
Destiny's cleaning from 2026-01, `Cleaning Payout` (1534) itself before that, because through
2025 Maria's payments were booked to the parent and touching 1588 for them would strand a
balance there.  So a 2025 JE is a pure class move within 1534 and a 2026 one within 1588 --
same rule, different account.  `credit_account` and `debit_account` are both aliases of
`posting_account` for exactly this reason: **a Maria JE that names two accounts is wrong.**

**`Residential cleaning` is not a listing and is not Valta's cost** -- it is rebilled to Valta
Home, so it leaves the JE as an A/R debit carrying that customer ON THE LINE, and the credit
is then the sheet's own `Total payment`, not the listings subtotal.  The figure is read from
the sheet every month (it moves, and 2026-03 once had none at all); a month with no
residential gets no line rather than a $0.00 one.

### The workbook is a snapshot of a live Google Sheet, and a stale one lies quietly

The xlsx under `inputs/` is exported by hand from a sheet the owner edits continuously
(`doc_id 1DYndAEzV1V4vLXVcGs8IVoZY-jrIYGuPBpZpH68DDKQ`).  A stale export does not fail --
**it silently reads a blank `Residential cleaning` cell as "none this month" and writes no
line**, and it reproduces listing totals that have since been corrected.  A 13-day-old copy
did exactly that on 2026-09-26: 2026-03 looked like it had no residential when it had
$7,831.84, and three months' listing subtotals were wrong.  `fixes/maria_cleaning_je_shape`
prints the file's modification time for this reason.  **Re-export before every run.**

### A corrected sheet means REBUILD, not patch

A correction can move a listing's amount, drop a listing or add one -- 2026-03 went from 40
listings / $22,237.00 to 38 / $22,620.00.  No line-by-line patch expresses that, so:

    python -m src.je.build_maria_cleaning --rebuild          # emit rows for months already in QBO
    python -m src.fixes.maria_cleaning_je_rebuild            # dry run, per-JE diff
    python -m src.fixes.maria_cleaning_je_rebuild --confirm  # full-update each JE in place

`--rebuild` is what makes the builder emit a month it would otherwise SKIP as already booked.
The fix script constructs no lines of its own: it reuses `je/post.build_payload`, so the
builder stays the single source of truth for what a Maria JE looks like.  It full-updates,
keeping the Id, DocNumber and date -- never delete-and-recreate.

**`build_payload` resolves every name to an Id and drops the name**, so a listing line and the
A/R line are indistinguishable in its output.  Anything reporting on what changed must read
the review CSV's `Class`/`Name` columns instead; measuring the new shape off the payload put
every listing into the A/R bucket and made a correct rebuild look catastrophic.

`fixes/maria_cleaning_je_shape` is the narrower tool -- it re-points accounts and adds a
missing A/R line without touching amounts.  It is superseded by the rebuild whenever the sheet
itself has changed.  Its one lasting lesson: **a line's role cannot be inferred from its
class.**  Deriving "listing means debit, flat means credit" broke `MariaCL_2026-06`, because
the A/R line is neither a listing nor the credit.

2026-01..08 rebuilt 2026-09-26 from the corrected sheet (JE 119390-119397, $254,365.75 of
sheet totals; listing debits moved off 1534 onto 1588 and all eight A/R lines added or
corrected).  2025-07..12 (JE 119406-119411, $124,977.00) are untouched and already correct
under the rule -- Dr/Cr both within 1534.
