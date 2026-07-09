# Guesty API tool

Pull property/listing data from the **Guesty Open API** and create new listings
from a prepared Excel sheet.

Run everything from this folder (the repo root) — commands are `python -m src.…`.

## Files

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

> Requires **Open API access on your Guesty plan**. If you don't see the option,
> contact Guesty support to enable it (the Open API is separate from the
> OTA/channel partner program).

1. Log in to Guesty → **Integrations** / **Marketplace** → **Open API** (a.k.a.
   API keys / Developer). Guesty's walkthrough:
   <https://help.guesty.com/hc/en-gb/articles/9370472424605-Using-Guesty-s-Open-API>
2. Create a new API key. Guesty gives you a **Client ID** and **Client Secret**.
3. **Copy the Client Secret immediately — it is shown only once.**

## 2. Configure & install

```bash
# from this folder (repo root)
cp .env.example .env          # then edit .env, paste your Client ID + Secret
source /Users/ylin/ValtaWork/.venv/bin/activate
pip install -r requirements.txt
```

## 3. Verify the connection

```bash
python -m src.guesty_client    # "Got access token…" + your listing count
```

Mints **one** token, cached to `data/guesty_token.json` (git-ignored). Guesty
allows only **5 tokens / 24h / client** and each lasts 24h — the client reuses
the cache automatically, so don't delete it casually.

## 4. Pull property data

```bash
python -m src.pull_listings                 # all → data/listings.{json,xlsx}
python -m src.pull_listings --limit 5        # quick peek
python -m src.pull_listings --fields _id,nickname,title,address.full,prices.basePrice
```

## 5. Create new listings from Excel

```bash
python -m src.make_template          # → templates/new_listings.xlsx
# Fill one row per listing (see the "column_guide" sheet).
# Required: nickname, address_full, base_price.

python -m src.create_listings --input templates/new_listings.xlsx --dry-run   # always first
python -m src.create_listings --input templates/new_listings.xlsx             # create for real
python -m src.create_listings --input templates/new_listings.xlsx --limit 1   # just first row
```

Listings are created **unlisted** (`isListed=false`) by default so nothing goes
public by accident — set the `is_listed` column to `true`, or publish later in
the Guesty UI. A single `POST /listings` is enough for a standard single-unit
property. Creating a listing isn't easily undone here — dry-run + `--limit 1`
before any bulk run.

Docs: <https://open-api-docs.guesty.com/docs/authentication> ·
<https://open-api-docs.guesty.com/docs/create-a-property>
