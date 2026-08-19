# GuestyFinancials

Pull **reservation financials** from the Guesty Open API — the guest-pay vs
host-payout breakdown for a single reservation, or for every reservation in a
check-in date range — and write them to Excel.

This is a standalone sibling of **GuestyAccess** (which handles *listings*). It
only reads reservations; it never writes to Guesty.

## Setup

```bash
source /Users/ylin/ValtaWork/.venv/bin/activate     # shared workspace venv
cd /Users/ylin/ValtaWork/Claude_projects/GuestyFinancials
cp .env.example .env                                 # then paste your credentials
python -m src.guesty_client                          # smoke test the connection
```

### Getting credentials

In Guesty: **Integrations → API / OAuth Applications → create an application**.
Copy the **Client ID** and the **Client Secret** (the secret is shown only once)
into `.env` as `GUESTY_CLIENT_ID` / `GUESTY_CLIENT_SECRET`. Auth is OAuth2
client-credentials; the access token is cached to `data/guesty_token.json` and
reused (Guesty allows only 5 tokens / 24h / client).

## Usage

Single reservation (confirmation code or 24-hex `_id`):

```bash
python -m src.reservation_financials HMTSFNDRDY            # print breakdown
python -m src.reservation_financials HMTSFNDRDY --excel    # + line-item .xlsx
```

Whole month or custom range, all listings:

```bash
python -m src.reservations_batch --month 2026-07 --status confirmed,canceled
python -m src.reservations_batch --checkin-from 2026-07-01 --checkin-to 2026-07-31
```

Batch output `data/reservations_<tag>_financials.xlsx` has three sheets:

| Sheet | What |
|-------|------|
| `summary` | one row per reservation — accommodation, cleaning, taxes, fees, host payout, PM commission, owner revenue |
| `items_by_category` | one row per reservation, fees split into named columns (pet, extra-person, parking, service, damage, taxes by type…) |
| `line_items` | long format, one row per invoice line item |

## Per-channel payment breakdown

`python payment_breakdown.py` turns the batch exports plus the user-owned
`data/payment structure.xlsx` into `output/payment_breakdown_<range>.xlsx` —
guest pay split into channel fee / Guesty fee / Stripe fee / host payout / net
revenue, per reservation and per channel. The channel formulas and all modeling
decisions live in `CLAUDE.md`.

## Two things to remember

- **The invoice items sum to host payout.** All figures shown are Guesty's own —
  no derived per-channel arithmetic (net-revenue math is channel-specific).
- **Deactivated listings are missing from the API.** The `/reservations`
  endpoint silently drops reservations on deactivated listings; the Guesty UI/CSV
  include them. For a complete month, cross-check the API count against the UI
  and supplement from a CSV export. See `CLAUDE.md` for details.
