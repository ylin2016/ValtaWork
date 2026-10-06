# How VRP wrote reservations to QuickBooks (reverse-engineered 2026-10-02)

Source: read-only survey of realm `9130356236278636`, all objects created since 2026-08-01
(1,152 Invoices, 3,413 Bills, 1,484 Payments, 486 JEs), plus prefix check back to 2025-03.
VRP writes as QBO user `9130356236278686` (`MetaData.LastModifiedByRef`).

**VRP's last writes: Bills/Invoices/JEs 2026-09-28 ~22:14 PT, Payments 2026-09-29 22:09.**
It ran as a nightly batch (~22:00–22:20 PT). Everything after that is missing.

## 1. Invoice — one per reservation (A/R side)

| field | value |
|---|---|
| DocNumber | Guesty confirmation code (`HM…` Airbnb, 10-digit Booking.com, `HA-…` VRBO, `GY-…` direct, `EXP-…`, `HVBM-…`) |
| Customer | `<source> - <guest name> - <code>` — one customer per reservation; source = Guesty `source` lower-case (`airbnb`, `bookingcom`, `homeaway`, `manual`, `expedia`, `hopper`, `homesvillasbymarriott`, `tripcom`, `whimstay`) |
| TxnDate = DueDate | check-in date |
| Location (DepartmentRef) | `Trust` |
| Class | on each LINE (`Listings:<listing>`), none on header |
| Tax code | `NON` on every line, TotalTax 0 |
| Custom fields | Check-In / Check-Out / No of Guests present but **empty** |
| Line description | `<Guesty label> \| <check-in> to <check-out> \| <N> nights` |
| Created | when the reservation appears (often months ahead); updated in place when it changes |

Line label → Item (observed, consistent across channels unless noted):

| Guesty label | Item |
|---|---|
| Accommodation fare, Markup, Extra person fee | `Guest Charges:Owner Income:Accommodation Fare` |
| Accommodation fare adjustment | `…:Accommodation Fare Adjustment` |
| Discounts (Length of stay, New listing promotion, Last minute, Weekly, Return Guest, Discount…) | `…:Accommodation Fare Discount` (negative) |
| Host channel fee / Host Service Fee (Airbnb, Expedia, Hopper, Whimstay) | `…:Owner Income:Channel Commission` (**negative line on the invoice**) |
| Cleaning fee | `PM Income:Cleaning Fee (PM)` — or `Owner Income:Cleaning Fee (Owner)` for owner-pays-cleaning listings |
| Pet fee | `Owner Income:Pet Fee (Owner)` |
| Service fee (direct), Extended Travel Coverage | `PM Income:Misc Guest Fee (PM)` |
| Damage Protection | `PM Income:Damage Waiver` |
| Taxes (CITY/STATE/COUNTY/LOCAL, Destination, Tax…) | `Guest Charges:Tax` (→ Rental Taxes Payable) |
| **VRBO (homeaway) taxes** | `PM Income:Misc Guest Fee (PM)` (channel remits) |
| Transient occupancy tax (Keaau) | `Owner Income:Taxes Paid to Owners` |
| Cancelled reservation | invoice rewritten to ONE $0 line, item `Services`, "Cancelled reservation" (unless a cancellation fee is kept) |

## 2. Bills — one $0.00 Bill per fee type per reservation (owner charge ↔ Valta revenue)

All share: Vendor `Valta Realty - <type>`, A/P = `PMS Clearing – A/P` (Id 1600, EN DASH),
Location `Trust`, TxnDate = DueDate = check-in, every line carries the listing Class and the
reservation Customer, `NotBillable`, memo + line description
`<label> | <code> | <in> to <out> | <N> nights` (`| Cancelled |` inserted when cancelled → all lines $0).

| DocNumber | Vendor | Lines (sign as posted) |
|---|---|---|
| `5W4Y_<code>` | Valta Realty - commission | −C `Management Commissions Revenue` / +C `1A - Net Earnings:Management Commissions` |
| `RR26_<code>` | Valta Realty - Supplies | −S `Billable Expense Income - Supplies Owner` / +S `1C - Owner Expenses:Supplies - Reservations - Owner` (older: `Supplies - Owner`) |
| `9Z5X_<code>` | Valta Realty - Channel Fee | Booking.com channel fee: −F `Billable Expense Income - Channel Fee` / +F `1A:Channel Fees`; **plus "Deduction" pair** +r·F `Management Commissions Revenue` / −r·F `1A:Management Commissions` |
| `8M10_<code>` | Valta Realty - Channel Fee | VRBO channel fee, same 4-line shape |
| `YKY6_<code>` | Valta Realty - Channel Fee | Tripcom channel fee, same shape |
| `RZ3O_<code>` | Valta Realty - Stripe Fee | Stripe fee: −F `Billable Expense Income - Stripe Fee` / +F `1A:Credit Card Fees` + Deduction pair |
| `X9M5_<code>` | Valta Realty | Marriott cleaning fee (`Cleaning Fee Revenue` / `1A:Misc Guest Fees`) |

VRP created **every** fee-type Bill for every reservation, $0 where it didn't apply (an Airbnb
booking got $0 `9Z5X_`, `8M10_`, `RZ3O_` Bills). Prefixes are fixed codes, unchanged since 2025.

**Commission math (verified on samples):** C = rate × (Accommodation fare + Markup − invoice
channel commission line). Each fee Bill then gives back r × fee ("Deduction"), so the owner's
net commission = rate × (fare − all channel/Stripe fees).
- Booking.com 5584577557: 20% × (1049 + 188.82) = 247.56; fee 212.67 → deduction 42.53.
- Airbnb HMKJPNMP2D: 16% × (857.60 + 214.40 − 204.14) = 138.86.
- Supplies = **0.9 × guests × min(nights, 60)** for `Supplies='central'` listings in
  Owner_statement_whole `config/Listing_contacts.csv` (= `run_month_close.supply_charge`); ties the
  samples exactly: Seattle 8415 0.9×4×8 = 28.80, Bellevue 1326 0.9×2×7 = 12.60, Yacinde C1 0.9×1×3 = 2.70.
  Owner-supplied listings got a $0 `Supplies - Owner` Bill → we skip it.

**Rate sources (decided 2026-10-03):** PM rate from Owner_statement_whole `scope/pm_rate`
(`owner_contracts`, effective-dated at check-in, no default — a missing rate blocks); channel and
Stripe fees from `payment_structure.xlsx` as transcribed in `breakdown/payment_model.py`
(incl. its per-reservation override tables).

## 3. Payments — one per reservation per payout

| field | value |
|---|---|
| Customer / applied to | the reservation customer, linked to its Invoice |
| TxnDate | payout date |
| PaymentRefNum | confirmation code |
| PrivateNote | `External Payment ID: <payout id>` (`M-…` Airbnb, `po_…` Stripe) |
| DepositToAccount | channel clearing: Airbnb → `Payments Clearing - Airbnb` (1626); Stripe-processed (VRBO, direct/manual, Expedia) → `Payments Clearing - Stripe` (1627) |

VRP did **not** create Booking.com payments (those in Sept were hand-entered, mostly straight to
bank 800 — conflicts with QBO_operations' 2026 rule that Booking.com goes to clearing 1676).

## 4. Payout JEs — one per payout (clearing → bank)

**Airbnb** `DocNumber = M-<ref>`, memo `External Payment ID: M-…`, date = payout date, no Location:
Cr `Payments Clearing - Airbnb` per reservation (class + customer entity, desc
`Reservation | airbnb | <in> to <out> | N nights`) / Dr `Chase Trust Checking 9967 - STR` total
(desc `Payout | Transfer to … 9967 (USD) | M-…`). Pass Through Tot → Tax Liability (known bug,
see QBO_operations `fixes/passthrough_tot`). QBO_operations `je/build_airbnb` already builds these.

**Stripe** `DocNumber = po_<id>` (truncated to 21 chars), memo full `po_…`:
- Cr `Payments Clearing - Stripe` per charge (gross)
- Dr `Fee - Processing & Commission:Fee - Stripe Processing` per charge — "Stripe processing fees"
  and "Guesty application fee" lines (Cr for "Application fee refund")
- Dr `1A - Net Earnings:Resolutions` for refunds ("REFUND FOR CHARGE (<code>) - ch_…")
- Dr `Chase Trust Checking 9967 - STR` net payout

**Booking.com** — not VRP; QBO_operations `je/build_bookingcom` builds them from the payout CSV.

## 5. Open questions for the owner

1. ~~Where does this code live?~~ **Decided 2026-10-02:** own folder; borrows QBO_operations'
   client + `config/secrets/qbo_tokens.json` (never copied). QBO_operations' CLAUDE.md to name this
   project as the second permitted writer.
2. ~~$0 Bills?~~ **Decided 2026-10-02:** create only the Bills that apply (commission, supplies, and
   the one channel/Stripe fee Bill), same DocNumber/vendor/account format.
3. **Cancellations — decided 2026-10-02:** VOID the invoice (not VRP's $0 rewrite). At month end,
   check the payout records: if we kept any money from a cancelled booking, bring the invoice back
   to that amount (Cancellation Fee item) so the payment has something to apply to. Its fee bills
   follow the kept amount. *To verify before building:* whether QBO's API lets a voided invoice be
   edited back to a non-zero amount; if not, a separate invoice `<code>-CXL` is the fallback.
4. **Booking.com — decided 2026-10-02:** owner supplies the Booking.com payout file; same two steps
   as Airbnb and Stripe — per-reservation Payment into `Payments Clearing - Booking.com` (1676),
   then the payout JE clearing -> Chase 9967 (reuse QBO_operations `je/build_bookingcom` logic).
5. **Stripe fee — decided 2026-10-02:** keep both, as VRP did: COGS `Fee - Stripe Processing` in the
   payout JE AND the `RZ3O_` rebill Bill to the owner.
6. **Backfill — decided 2026-10-02:** everything created/changed in Guesty since 2026-09-28 22:00 PT.
7. **Pass Through Tot — decided 2026-10-02:** fix it. Airbnb payout JEs credit clearing for the FULL
   payout (the invoice already books it under `Guest Charges:Tax`), never Tax Liability.
   Fee-bill credit accounts use the named accounts (`- Channel Fee`, `- Stripe Fee`,
   `- Supplies Owner`), as the samples above show.
