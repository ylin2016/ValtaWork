# Deploying to Streamlit Community Cloud (data on Google Drive)

The data inputs are **never committed** (they carry owner PII). On Streamlit
Cloud the app downloads them from Google Drive at startup using a **service
account**, then builds the SQLite in the container. Locally nothing changes —
the files already live in `data/`.

## 1. Put the three data files on Google Drive

Upload to a private Drive folder:

- the Guesty reviews `*.xlsx`
- `Property_OverallRatings.xlsx`
- `Listing_contacts.csv`

For each file, **Share → copy link**. The link looks like
`https://drive.google.com/file/d/`**`<FILE_ID>`**`/view` — keep the `<FILE_ID>`.

## 2. Create a Google service account (one-time)

1. <https://console.cloud.google.com/> → create/select a project.
2. **APIs & Services → Library →** enable **Google Drive API**.
3. **APIs & Services → Credentials → Create credentials → Service account.**
   Name it e.g. `guest-reviews`, create, no roles needed.
4. Open the service account → **Keys → Add key → Create new key → JSON.** A
   JSON file downloads — this is your secret.
5. Copy the service account's **email** (`...@...iam.gserviceaccount.com`).
6. Back in Drive, **share each of the three files with that email** (Viewer).
   This is what makes the private files reachable without making them public.

## 3. Put the secrets into Streamlit

Use [.streamlit/secrets.toml.example](.streamlit/secrets.toml.example) as the
template. Fill `[gdrive.service_account]` from the JSON key (paste `private_key`
as one line with literal `\n`), and map each `[gdrive.file_ids]` entry to the
`<FILE_ID>` from step 1.

- **On Cloud:** App → **Settings → Secrets** → paste the whole thing.
- **Locally (optional):** save it as `.streamlit/secrets.toml` (git-ignored).

## 4. Deploy

1. <https://share.streamlit.io/> → sign in with GitHub → **Create app**.
2. Repository `ylin2016/ValtaWork`, branch `main`, **Main file path:**
   `Claude_projects/GuestReview/src/dashboard.py`.
3. Add the secrets (step 3), **Deploy**.
4. App → Settings → **Sharing** → set to private and invite viewers by email
   (the dashboard shows owner PII — don't leave it public).

## Notes

- Dependencies install from `requirements.txt` (includes the Google libraries).
- The container fetches data + builds the DB **once**. If you replace a Drive
  file with newer data, **Reboot app** (Manage app → ⋮ → Reboot) to refetch.
- To run entirely without Drive (e.g. a private repo with committed data), just
  don't set the `[gdrive]` secrets — `ensure_data()` becomes a no-op and the app
  reads whatever is in `data/`.
