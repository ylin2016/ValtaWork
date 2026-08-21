# guesty_api

Pull property/listing data from the **Guesty Open API** and create new listings
from a prepared Excel sheet. Built to feed the other Valta downstream projects.

## What's here

| File | Purpose |
|------|---------|
| `src/guesty_client.py` | OAuth2 client-credentials auth + disk-cached token + rate-limit handling. Run directly as a connection smoke test. |
| `src/pull_listings.py` | `GET /listings` (all pages) → `data/listings.json` + `data/listings.xlsx`. |
| `src/make_template.py` | Writes `templates/new_listings.xlsx` — the blank sheet you fill in. |
| `src/listing_mapper.py` | Maps one Excel row → a Guesty `POST /listings` payload. |
| `src/create_listings.py` | Reads the sheet → creates listings (with a `--dry-run` mode). |
| `config.yml` | Base URLs, token cache path, per-field defaults, create delay. |
| `.env` | Your credentials (git-ignored; copy from `.env.example`). |

## 1. Get Guesty Open API credentials

> Requires **Open API access on your Guesty plan**. If you don't see the option
> below, contact Guesty support / your account manager to enable it. Note:
> OTA/channel *partners* are told to use the partner program, not the Open API.

1. Log in to Guesty → open the **Integrations** / **Marketplace** area and find
   **Open API** (a.k.a. "API keys" / "Developer"). Guesty's own walkthrough:
   <https://help.guesty.com/hc/en-gb/articles/9370472424605-Using-Guesty-s-Open-API>
2. Create a new API key / application. Guesty generates a **Client ID** and a
   **Client Secret**.
3. **Copy the Client Secret immediately — it is shown only once.** After that
   Guesty redacts it and you'd have to roll a new one.

## 2. Configure & install

```bash
cd Claude_projects/guesty_api
cp .env.example .env
# edit .env and paste GUESTY_CLIENT_ID and GUESTY_CLIENT_SECRET

# use the shared workspace venv (has requests/pandas/openpyxl/…)
source /Users/ylin/ValtaWork/.venv/bin/activate
pip install -r requirements.txt   # installs python-dotenv/PyYAML if missing
```

## 3. Verify the connection

```bash
python -m src.guesty_client
# → "Got access token (…abc123)."  and a listings count
```

This mints **one** token and caches it to `data/guesty_token.json`. Guesty
allows only **5 tokens per 24h per client**, and the token lasts 24h — the
client reuses the cached token automatically, so don't delete it casually.

## 4. Pull property data

```bash
python -m src.pull_listings                 # all listings → data/listings.{json,xlsx}
python -m src.pull_listings --limit 5        # quick peek
python -m src.pull_listings --fields _id,nickname,title,address.full,prices.basePrice
```

`.json` keeps the full nested objects (use this downstream); `.xlsx` is a
flattened one-row-per-listing view for eyeballing.

## 5. Create new listings from Excel

```bash
python -m src.make_template          # → templates/new_listings.xlsx
# Fill in one row per listing. See the "column_guide" sheet for every column.
# Required: nickname, address_full, base_price.

# ALWAYS dry-run first — prints the exact payloads, calls nothing:
python -m src.create_listings --input templates/new_listings.xlsx --dry-run

# Create for real (results written to data/create_results.xlsx):
python -m src.create_listings --input templates/new_listings.xlsx
python -m src.create_listings --input templates/new_listings.xlsx --limit 1   # just the first row
```

### Notes on creating listings

- **Listings are created UNLISTED** (`isListed=false`) by default so nothing
  goes public by accident — set the `is_listed` column to `true`, or publish
  later in the Guesty UI. Change the default in `config.yml`.
- A single `POST /listings` is enough for a standard single-unit property. Only
  multi-units/complexes need the extra `spaces` / `house-rules` / `complex`
  calls (not implemented here — ask if you need them).
- `address_full` is parsed by Guesty into street/city/zip/etc., so a complete
  address string is usually all you need; the discrete columns are optional
  overrides.
- Creating a listing is **not easily reversible via this tool** — dry-run and
  `--limit 1` before a bulk run.

## How auth works (why the token is cached)

Guesty Open API uses OAuth2 **client credentials**: we POST `client_id` +
`client_secret` to `https://open-api.guesty.com/oauth2/token` with
`grant_type=client_credentials&scope=open-api` and get a bearer token valid for
24h. Because only 5 tokens can be minted per 24h, `guesty_client.py` caches the
token to disk and refreshes only within `token_refresh_buffer_seconds` of
expiry. All data calls hit `https://open-api.guesty.com/v1`.

Docs: <https://open-api-docs.guesty.com/docs/authentication> ·
<https://open-api-docs.guesty.com/docs/create-a-property>
