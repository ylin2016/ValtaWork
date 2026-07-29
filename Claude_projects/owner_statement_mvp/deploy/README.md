# Owner Statements — Demo (display-only)

A self-contained, **read-only** copy of the owner-statement dashboard for demos.

## What's in here (and what's NOT)
The heavy work — QBO sync, Guesty import, fee/commission math, `build` — runs
**locally** in the main project. The result is baked into `data/owner_statement.sqlite`.
This folder is the "packed content": the dashboard only **reads** that DB and renders.

- **No QBO tokens.** The dashboard never calls QuickBooks; it needs no credentials.
- **No pipeline code.** Only the display app + 3 read-only helper modules.
- Contains **real owner data**, so the app is protected by a password (see below).

## Refreshing the data
Re-run the pipeline in the main project, then re-pack the DB:
```bash
sqlite3 ../data/owner_statement.sqlite "VACUUM INTO 'data/owner_statement.sqlite';"
cp ../data/guesty_converted.csv ../config.yml ../mapping_classes.yml .   # if they changed
```
Commit and redeploy. The demo is a snapshot — it won't change until you re-pack.

## Run locally
```bash
pip install -r requirements.txt
streamlit run src/dashboard.py     # or: streamlit run streamlit_app.py
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
4. Deploy. (The 28 MB DB is under Cloud's limits.)

## Deploy to Render (alternative)
- New Web Service → this repo.
- Build: `pip install -r requirements.txt`
- Start: `streamlit run streamlit_app.py --server.port $PORT --server.address 0.0.0.0`
- Add `app_password` as an environment secret file at `.streamlit/secrets.toml`.
