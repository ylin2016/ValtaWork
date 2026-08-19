# CLAUDE.md — GuestyFinancials

Guidance for Claude Code when working in this project.

## What this is

A small, self-contained tool that pulls **reservation financials** from the
**Guesty Open API** — the guest-pay vs host-payout breakdown for one reservation,
or for every reservation in a check-in date range. Output is Excel.

It is **independent of** the sibling `GuestyAccess` project (which pulls/creates
*listing* records). This project only reads reservations; it never writes to
Guesty. The two share the same Guesty Open API credentials and the same client
code (`src/config.py`, `src/guesty_client.py`), copied into each so each runs on
its own.

## Running

Use the shared workspace venv (has requests/pandas/openpyxl/PyYAML/python-dotenv):

```bash
source /Users/ylin/ValtaWork/.venv/bin/activate
cd /Users/ylin/ValtaWork/Claude_projects/GuestyFinancials
```

Run modules from the project root as a package:

- **One reservation** (by confirmation code or 24-hex `_id`):
  ```bash
  python -m src.reservation_financials HMTSFNDRDY            # print breakdown
  python -m src.reservation_financials HMTSFNDRDY --excel    # + line-item .xlsx
  python -m src.reservation_financials HMTSFNDRDY --json     # + raw money object
  ```
- **A whole month / range** (all listings):
  ```bash
  python -m src.reservations_batch --month 2026-07
  python -m src.reservations_batch --month 2026-07 --status confirmed,canceled
  python -m src.reservations_batch --checkin-from 2026-07-01 --checkin-to 2026-07-31
  ```
  Writes `data/reservations_<tag>_financials.xlsx` with three sheets:
  `summary` (one row/reservation), `items_by_category` (fees split into named
  columns), `line_items` (long format).

Smoke-test the API connection: `python -m src.guesty_client`.

**Always pass `--status confirmed,canceled` for the batch.** A bare `--month`
also returns `inquiry`/`declined`/`closed` — unbooked quotes that still carry a
phantom `hostPayout`, and whose `confirmationCode`s overlap real bookings so the
combined pull *looks like* it has duplicate rows. It is not a fetcher/pagination
bug: within a single status the codes are unique (verified 2026-08-11: 327/327
Jun, 495/495 Jul). If you ever mix statuses, dedupe by `confirmationCode`.

## Credentials & token (shared limit — read this)

- Credentials live in git-ignored `.env` (`GUESTY_CLIENT_ID` /
  `GUESTY_CLIENT_SECRET`). Copy `.env.example` to start.
- Auth is OAuth2 **client credentials**. Guesty allows only **5 tokens / 24h /
  client**, each valid 24h, so the token is cached to `data/guesty_token.json`
  and reused. **Do not delete it and do not force-refresh in a loop.** This
  project and `GuestyAccess` use the *same* client, so between them keep token
  minting rare (each project mints at most ~1/day from its own cache).

## The money model (verified — do not invent channel math)

- Financials live in `reservation.money`.
- `money.invoiceItems` is the **host ledger**; item amounts **sum exactly to
  `money.hostPayout`**. `build_breakdown()` relies on this.
- `ownerRevenue = hostPayout − commission`; `commission` = PM commission;
  `hostServiceFee` = channel host fee; guest service fee =
  `guestFeeBase + guestFeeVat`.
- **Who collects matters:** channel-collected (Airbnb/Booking/Expedia) show
  `totalPaid=0`, `balanceDue=hostPayout`; Guesty/host-collected (VRBO/direct)
  show the real `totalPaid`.
- Guesty lumps pet/parking/extra-person/service/damage fees under
  `normalType=AFE`. `categorize_item()` splits them into named categories **by
  item title**, which is why the batch report can show a `pet_fee`,
  `extra_person_fee`, etc. column. If a channel introduces a new fee title,
  extend `categorize_item()` in `reservation_financials.py`.
- **`money.totalTaxes` UNDERCOUNTS the real tax** (found 2026-08-11): it omits
  some tax lines (seen: `tax_destination`). Example `BC-nVlG573MY` reported
  `26.62` but the true tax is `26.62 + 43.26 = 69.88`. For a correct total, SUM
  the itemized `tax_*` summary columns
  (`tax_state/county/city/local/occupancy/residential/destination/reservation_total/other`),
  **not** `total_taxes`. `tax_reservation_total` is a standalone type that never
  coexists with the others, so summing does not double-count. Across Jun+Jul 2026
  the undercount was ~$2,766 over 40 reservations.

## Deactivated-listing gotcha (important for month-end completeness)

The Open API `/reservations` endpoint **silently excludes** reservations on
**deactivated** listings (`active=false`), but the Guesty UI and its CSV export
include them. For July 2026 this produced a 656 (API) vs 670 (UI) gap — the 14
missing reservations were on 4 deactivated listings. It is NOT
timezone/status/pagination.

**For a complete month, cross-check the API count against the Guesty UI**, and
supplement deactivated listings from a Guesty CSV export. The delivered
`data/reservations_2026-07_COMPLETE.xlsx` was the API pull merged with 14
CSV rows (flagged via a `data_source` column).

## Per-channel payment breakdown (`payment_breakdown.py`)

A downstream analysis on top of the batch exports: models, per reservation and
per channel, how guest-pay splits into channel fee / Guesty fee / Stripe fee /
host payout / net revenue.

**`data/payment structure.xlsx` is THE spec that drives every fee calculation.**
It has one row per channel with columns `Platform · Source · InvoiceItem ·
Channel Fee · Guest pay · Guesty Fee · Stripe fee · Host payout · Net Revenue ·
Note`, each cell a formula. `payment_breakdown.py` is a **faithful transcription
of that sheet** — the InvoiceItem definition, the channel-fee rate, the Guesty
fee, the Stripe fee, and the host-payout / net-revenue formulas below all come
from it, nowhere else. When the user says they edited the sheet, **re-read it and
update the code to match**; never invent a rate or leave a stale one. The rates
and formulas listed under "Decisions baked in" are the current transcription — if
they disagree with the sheet, the sheet wins.

Run from the project root:

```bash
python payment_breakdown.py       # reads data/, writes output/payment_breakdown_2026-06_07.xlsx
```

**Inputs** (all in `data/`): `reservations_2026-06_financials.xlsx` +
`reservations_2026-07_financials.xlsx` (batch exports; months are currently
hard-coded), `payment structure.xlsx` (**user-owned** formula sheet — the source
of truth for every channel's math; re-read it whenever the user says they edited
it), and `_listing_tax_rates.csv` (per-listing property tax rate, for Airbnb).
**Output:** `output/payment_breakdown_2026-06_07.xlsx` (not `data/`).

Decisions baked in (agreed with the user — do not silently change):

- **InvoiceItem = Guesty `hostPayout` exactly as shown** (Σ invoiceItems). Never
  re-derive it. For Airbnb, the Resolution Center (ARC) line **stays inside**
  InvoiceItem, so `host_payout` and `guest_pay` are already net of the refund.
  The `refund` column is **display-only** and is never subtracted again. (This
  was settled after much back-and-forth — the host got the post-ARC cash.)
- **Row groups:** every `source` maps to one row of `payment structure.xlsx` —
  `airbnb`, `airbnb_hawaii`, `homeaway` (Vrbo family), `expedia_pcm` (integrated,
  has a PCM host-fee line), `expedia_grp` (Expedia/Hotels.com/Orbitz/… no PCM),
  `booking`, `hopper` (Capital One/Hopper), `manual` (BE-API/direct/website/
  owner), `marriott`, `tripcom`, `whimstay`, `blueground`, `bnbfinder`. The
  Expedia split is driven by `has_pcm` (presence of a `host_channel_fee` line),
  not the source string.
- **Channel fee** = `(InvoiceItem − tax) × rate`, rate per row group: Airbnb
  15.5%, HomeAway/Whimstay 5%, Blueground 7%, Trip.com 11.1%, Hopper 14%,
  Booking 15%, Expedia 18%, Marriott 18.5%. Manual/bnbFinder have no channel fee.
  (These are the sheet's model rates, not Guesty's actual host fee.)
- **Guesty fee** = `InvoiceItem × 1%`; **Stripe fee** = tiered
  `min(II,200) × 3.05% + max(II−200,0) × 3%` (the sheet writes it as
  `200×3.05% + (II−200)×3%`; clamped so sub-$200 bookings don't go negative).
  Both apply **only** to host/Guesty-collected channels: `homeaway`,
  `expedia_pcm`, `expedia_grp`, `manual`, `bnbfinder`. Zero elsewhere.
- **Host payout:** channel-collected groups (Airbnb, Hopper, Marriott, Whimstay,
  Blueground, Trip.com, bnbFinder) = InvoiceItem as-is; Booking = InvoiceItem −
  channel fee; HomeAway/Expedia/Manual = InvoiceItem − Stripe − Guesty.
  **Net revenue** = InvoiceItem − cleaning − tax − (channel fee where the group
  subtracts it) − (service fee for Manual) − Stripe − Guesty.
- **Tax:** sum the itemized `tax_*` columns (see money-model note above).
  Mainland **Airbnb** tax isn't in the ledger → compute it as
  `((InvoiceItem − ARC) + channel fee) × property_rate` (rate from
  `_listing_tax_rates.csv`; base **excludes** ARC) and it is *not* subtracted in
  net revenue. **Airbnb-Hawaii** = listing **"Keaau 15-1542"** (detected by tax
  sitting in the host ledger) → tax *is* in InvoiceItem and *is* subtracted.
- **Filtering & sheet polish:** dedupe by `confirmationCode`; keep only
  `confirmed`/`canceled`; **drop rows where InvoiceItem = 0** (canceled/comp/
  owner); sort by `listing`, then `checkIn`; freeze the header row (`A2`) and add
  an autofilter across the header. Two sheets: `reservations` + `by_channel`.
- The 14 deactivated-listing reservations are included **only** if
  `data/reservations_2026-07_COMPLETE.xlsx` is present (see gotcha above); the
  script notes when it's missing.

## Data lives outside git

`.gitignore` excludes `.env`, the token cache, and all `data/*.{json,csv,xlsx}`.
Generated reports and any Guesty CSV exports you merge in are not committed.
