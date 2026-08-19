# Owner Statements — Deploy (display-only)

A self-contained, **read-only** copy of the owner-statement dashboard for hosting.

## What's in here (and what's NOT)

The heavy work — Guesty API pull, `convert`, `guesty-import`, QBO `qbo-sync`, LTR
import, and `build` (fee/commission math, the payment-breakdown net override) —
runs **locally** in the main project. Its result is baked into
`db/owner_statement.sqlite` plus the per-period display CSVs under `inputs/`.
This folder is the "packed content": the dashboard only **reads** them and renders.

- **No credentials.** No QBO tokens, no Guesty token, no `.env`. The dashboard
  never calls QuickBooks or Guesty — it needs no secrets.
- **No pipeline code.** Only the display app + 4 read-only helper modules
  (`paths.py`, `ltr_records.py`, `listing_filter.py`, `pm_rate.py`).
- Contains **real owner data**, so the app is protected by a password (see below).

### Layout (mirrors the main project, rooted here)

```
streamlit_app.py            # host entry point → runs src/dashboard.py
src/                        # dashboard.py (+ password gate) + 4 display-only modules
config/                     # mapping_classes.yml, config.yml, Listing_contacts.csv
db/owner_statement.sqlite   # VACUUM'd read-only snapshot of the ledger
inputs/<period>/            # payment_breakdown_<p>.csv, guesty_converted.csv, LTR_<p>.csv
```

`src/paths.py` is copied verbatim from the main project — its `parent.parent`
resolves to THIS folder, so the packaged `config/`, `inputs/`, and `db/` are found
with zero code changes. The only edit vs. the main dashboard is the password gate.
Available periods come from the DB (`statement_runs`); the selector lists whatever
`build` has written.

## Refreshing the data

Re-run the pipeline in the main project for the period, then re-pack with one command:

```bash
# from the main project root (Owner_statement_whole/)
python -m src.deploy_pack --period 2026-07
```

It VACUUMs the ledger DB into `deploy/db/`, copies the period's display CSVs
(`payment_breakdown`, `guesty_converted`, `LTR`) and the config files, syncs the
4 display-only modules verbatim, and rewrites `deploy/src/dashboard.py` with the
password gate re-injected. (Manual equivalent, if ever needed:)

```bash
sqlite3 db/owner_statement.sqlite "VACUUM INTO 'deploy/db/owner_statement.sqlite';"
P=2026-07
mkdir -p deploy/inputs/$P
cp inputs/$P/payment_breakdown_$P.csv inputs/$P/guesty_converted.csv inputs/$P/LTR_$P.csv deploy/inputs/$P/
cp config/mapping_classes.yml config/config.yml config/Listing_contacts.csv deploy/config/
# then re-apply the password gate to deploy/src/dashboard.py by hand
```

Commit and redeploy. The deploy is a snapshot — it won't change until you re-pack.

## Run locally

```bash
pip install -r requirements.txt
streamlit run streamlit_app.py     # or: streamlit run src/dashboard.py
```

## Set the password

Real data on a public URL must be gated. Set an `app_password`:

- **Local:** copy `.streamlit/secrets.toml.example` → `.streamlit/secrets.toml` and edit.
- **Streamlit Cloud:** App → Settings → **Secrets**, paste `app_password = "..."`.

With no `app_password` set, the gate is off (convenient for local dev, unsafe for public).

## Deploy to Streamlit Community Cloud

1. Push **this `deploy/` folder** to its own **private** GitHub repo.
2. streamlit.io/cloud → New app → pick the repo, main file `streamlit_app.py`.
3. Add the `app_password` secret (step above).
4. Deploy.

## Deploy to Render (alternative)

- New Web Service → this repo.
- Build: `pip install -r requirements.txt`
- Start: `streamlit run streamlit_app.py --server.port $PORT --server.address 0.0.0.0`
- Add `app_password` as a secret file at `.streamlit/secrets.toml`.
