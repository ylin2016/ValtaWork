"""QuickBooks Online REST client -- the READ/WRITE one.

This is the only QuickBooks client in the estate permitted to mutate data.  Its
opposite number, ``Owner_statement_whole/src/expense/qbo_client.py``, refuses any
method but GET; if you find yourself wanting to POST from the statement pipeline,
the operation belongs in this project instead.

Self-contained on purpose (no imports from the statement project): this project
owns the OAuth flow and the token store, so it must not depend on the sibling it
lends tokens to.
"""
from __future__ import annotations

import base64
import json
import os
import time
from pathlib import Path
from urllib.parse import urlencode

import requests

from .paths import QBO_TOKENS

TOKEN_PATH_DEFAULT = str(QBO_TOKENS)


def _read_json(path: str, default=None):
    try:
        return json.loads(Path(path).read_text())
    except (OSError, ValueError):
        return default


def _write_json(path: str, obj) -> None:
    p = Path(path)
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(json.dumps(obj, indent=2))
    # The refresh token lives here; keep it off other users' eyes.
    p.chmod(0o600)


class QBOClient:
    def __init__(self, realm_id: str, base_url: str, minorversion: int = 75,
                 token_path: str = TOKEN_PATH_DEFAULT):
        self.realm_id = realm_id
        self.base_url = base_url.rstrip("/")
        self.minorversion = minorversion
        self.token_path = token_path

        self.client_id = os.environ.get("QBO_CLIENT_ID")
        self.client_secret = os.environ.get("QBO_CLIENT_SECRET")
        self.redirect_uri = os.environ.get("QBO_REDIRECT_URI", "http://localhost:8000/callback")

        if not self.client_id or not self.client_secret:
            raise RuntimeError("Missing QBO_CLIENT_ID or QBO_CLIENT_SECRET in environment (.env).")

    # --- OAuth ------------------------------------------------------------------
    def _basic_auth_header(self) -> str:
        tok = f"{self.client_id}:{self.client_secret}".encode("utf-8")
        return "Basic " + base64.b64encode(tok).decode("utf-8")

    def auth_url(self, state: str) -> str:
        params = {
            "client_id": self.client_id,
            "scope": "com.intuit.quickbooks.accounting",
            "redirect_uri": self.redirect_uri,
            "response_type": "code",
            "state": state,
        }
        return "https://appcenter.intuit.com/connect/oauth2?" + urlencode(params)

    def exchange_code_for_tokens(self, code: str) -> dict:
        url = "https://oauth.platform.intuit.com/oauth2/v1/tokens/bearer"
        headers = {
            "Authorization": self._basic_auth_header(),
            "Accept": "application/json",
            "Content-Type": "application/x-www-form-urlencoded",
        }
        data = {"grant_type": "authorization_code", "code": code, "redirect_uri": self.redirect_uri}
        r = requests.post(url, headers=headers, data=data, timeout=60)
        # NB: never print r.text here -- the body carries the access and refresh tokens.
        r.raise_for_status()
        tokens = r.json()
        tokens["_obtained_at"] = int(time.time())
        _write_json(self.token_path, tokens)
        return tokens

    def refresh_tokens(self) -> dict:
        tokens = _read_json(self.token_path, default=None)
        if not tokens or "refresh_token" not in tokens:
            raise RuntimeError("No refresh_token found. Run `python -m src.auth url` first.")
        url = "https://oauth.platform.intuit.com/oauth2/v1/tokens/bearer"
        headers = {
            "Authorization": self._basic_auth_header(),
            "Accept": "application/json",
            "Content-Type": "application/x-www-form-urlencoded",
        }
        data = {"grant_type": "refresh_token", "refresh_token": tokens["refresh_token"]}
        r = requests.post(url, headers=headers, data=data, timeout=60)
        # NB: never print r.text here -- the body carries the access and refresh tokens.
        r.raise_for_status()
        newt = r.json()
        newt["_obtained_at"] = int(time.time())
        if "refresh_token" not in newt:
            newt["refresh_token"] = tokens["refresh_token"]
        _write_json(self.token_path, newt)
        return newt

    def _get_access_token(self) -> str:
        tokens = _read_json(self.token_path, default=None)
        if not tokens:
            raise RuntimeError("No tokens found. Run `python -m src.auth url` first.")
        obtained = int(tokens.get("_obtained_at", 0))
        expires_in = int(tokens.get("expires_in", 3600))
        if int(time.time()) > obtained + expires_in - 120:
            tokens = self.refresh_tokens()
        return tokens["access_token"]

    # --- transport ---------------------------------------------------------------
    def request(self, method: str, path: str, params: dict | None = None,
                json_body: dict | None = None) -> dict:
        token = self._get_access_token()
        headers = {"Authorization": f"Bearer {token}", "Accept": "application/json",
                   "Content-Type": "application/json"}
        params = params or {}
        params["minorversion"] = self.minorversion
        url = f"{self.base_url}{path}"
        r = requests.request(method, url, headers=headers, params=params, json=json_body, timeout=90)
        if r.status_code >= 400:
            # QBO puts the actual reason (a stale SyncToken, an unbalanced JE) in the body;
            # raise_for_status alone reports only "400 Client Error" and hides it.
            raise requests.HTTPError(f"{r.status_code} {method} {path}: {r.text[:2000]}",
                                     response=r)
        return r.json()

    def query(self, q: str, start_position: int = 1, max_results: int = 1000) -> dict:
        # QBO expects STARTPOSITION / MAXRESULTS inside the query string.
        q_clean = q.strip().rstrip(";")
        q_paged = f"{q_clean} STARTPOSITION {int(start_position)} MAXRESULTS {int(max_results)}"
        return self.request("GET", f"/v3/company/{self.realm_id}/query",
                            params={"query": q_paged})

    # --- writes ------------------------------------------------------------------
    def post(self, entity: str, body: dict) -> dict:
        """POST to /v3/company/<realm>/<entity>, returning the entity object.

        Always build the path here.  Passing a bare ``"journalentry"`` to
        ``request`` makes requests treat it as a hostname and fail with a DNS
        error -- which has happened, and only looked like a network problem.
        """
        key = entity[:1].upper() + entity[1:]
        res = self.post_raw(entity, body)
        return res.get(key, res)

    def post_raw(self, entity: str, body: dict) -> dict:
        return self.request("POST", f"/v3/company/{self.realm_id}/{entity.lower()}",
                            json_body=body)

    def query_all(self, select: str, entity: str, page_size: int = 1000) -> list[dict]:
        """Every page of a query, e.g. query_all("SELECT * FROM JournalEntry ...",
        "JournalEntry"). The paging loop was copy-pasted into five scripts."""
        out, pos = [], 1
        while True:
            batch = (self.query(select, start_position=pos, max_results=page_size)
                     .get("QueryResponse", {}).get(entity, []) or [])
            out.extend(batch)
            if len(batch) < page_size:
                return out
            pos += page_size
