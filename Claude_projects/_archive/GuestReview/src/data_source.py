"""Optionally hydrate `data/` from Google Drive (for Streamlit Cloud deploys).

The data inputs (reviews xlsx, the Airbnb-rating snapshot, the contacts CSV with
owner PII) are git-ignored and never committed. Locally they already sit in
`data/`, so this module is a no-op. On Streamlit Community Cloud there is no
`data/`, so we download each file from Google Drive using a *service account*
whose JSON key + the Drive file IDs live in `st.secrets["gdrive"]`.

Secrets shape (see .streamlit/secrets.toml.example):

    [gdrive.service_account]      # the whole service-account JSON key
    type = "service_account"
    client_email = "...@...iam.gserviceaccount.com"
    private_key = "-----BEGIN PRIVATE KEY-----\\n...\\n-----END PRIVATE KEY-----\\n"
    ...

    [gdrive.file_ids]             # config path-key -> Drive file ID
    reviews_xlsx     = "1AbC..."
    baseline_xlsx    = "1DeF..."
    listing_contacts = "1GhI..."

Share each Drive file (read-only) with the service account's client_email.
"""
from __future__ import annotations

import io
import os

from normalize import load_config, project_path

# config.yml path-keys we know how to fetch
_FETCHABLE = ("reviews_xlsx", "baseline_xlsx", "listing_contacts")
_SCOPES = ["https://www.googleapis.com/auth/drive.readonly"]


def _gdrive_secrets():
    """Return the [gdrive] secrets mapping, or None when secrets aren't set.

    Accessing st.secrets raises (not returns None) when no secrets.toml exists,
    so every access is guarded — local runs simply get None and skip Drive."""
    try:
        import streamlit as st
        return dict(st.secrets["gdrive"])
    except Exception:
        return None


def _download(drive, file_id: str, dest: str) -> None:
    from googleapiclient.http import MediaIoBaseDownload
    os.makedirs(os.path.dirname(dest), exist_ok=True)
    buf = io.BytesIO()
    dl = MediaIoBaseDownload(buf, drive.files().get_media(fileId=file_id))
    done = False
    while not done:
        _, done = dl.next_chunk()
    with open(dest, "wb") as fh:
        fh.write(buf.getvalue())


def ensure_data() -> list[str]:
    """Make sure the data inputs exist on disk; fetch missing ones from Drive.

    No-op (returns []) when there's no `[gdrive]` secrets section — i.e. local
    runs, where the files are already in `data/`. Returns the list of relative
    paths that were downloaded."""
    gd = _gdrive_secrets()
    if not gd:
        return []
    file_ids = dict(gd.get("file_ids", {}))
    if not file_ids:
        return []

    cfg = load_config()
    # resolve which files are missing before spinning up a Drive client
    wanted = {k: project_path(cfg["paths"][k])
              for k in file_ids if k in _FETCHABLE and k in cfg["paths"]}
    missing = {k: dest for k, dest in wanted.items() if not os.path.exists(dest)}
    if not missing:
        return []

    from google.oauth2.service_account import Credentials
    from googleapiclient.discovery import build as gbuild

    creds = Credentials.from_service_account_info(
        dict(gd["service_account"]), scopes=_SCOPES)
    drive = gbuild("drive", "v3", credentials=creds, cache_discovery=False)

    fetched = []
    for key, dest in missing.items():
        _download(drive, file_ids[key], dest)
        fetched.append(os.path.relpath(dest, project_path(".")))
    return fetched
