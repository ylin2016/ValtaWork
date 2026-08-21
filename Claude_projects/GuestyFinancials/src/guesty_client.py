"""Guesty Open API client.

Auth is OAuth2 *client credentials* (not authorization-code): we POST the
client_id/secret to the token endpoint and get back a bearer token good for
24h. Guesty allows only **5 tokens per 24h per client**, so the token is
cached to disk (`config.token_path`) and reused until it is near expiry — do
NOT mint a fresh token per run.

Docs: https://open-api-docs.guesty.com/docs/authentication
"""
import json
import time
from pathlib import Path

import requests

from .config import env, load_config, resolve


class GuestyClient:
    def __init__(self, config: dict | None = None):
        self.cfg = config or load_config()
        self.base_url = self.cfg["base_url"].rstrip("/")
        self.token_url = self.cfg["token_url"]
        self.scope = self.cfg.get("scope", "open-api")
        self.token_path = resolve(self.cfg["token_path"])
        self.refresh_buffer = int(self.cfg.get("token_refresh_buffer_seconds", 3600))

        self.client_id = env("GUESTY_CLIENT_ID")
        self.client_secret = env("GUESTY_CLIENT_SECRET")
        if not self.client_id or not self.client_secret:
            raise RuntimeError(
                "Missing GUESTY_CLIENT_ID / GUESTY_CLIENT_SECRET. "
                "Copy .env.example to .env and fill in your credentials."
            )

    # ----- token handling -------------------------------------------------
    def _read_cached_token(self) -> dict | None:
        if not self.token_path.exists():
            return None
        try:
            return json.loads(self.token_path.read_text())
        except (json.JSONDecodeError, OSError):
            return None

    def _write_cached_token(self, tok: dict) -> None:
        self.token_path.parent.mkdir(parents=True, exist_ok=True)
        self.token_path.write_text(json.dumps(tok, indent=2))

    def _token_is_fresh(self, tok: dict) -> bool:
        obtained = int(tok.get("_obtained_at", 0))
        expires_in = int(tok.get("expires_in", 0))
        # Treat as stale once we are within the refresh buffer of expiry.
        return time.time() < (obtained + expires_in - self.refresh_buffer)

    def _fetch_new_token(self) -> dict:
        resp = requests.post(
            self.token_url,
            headers={
                "Accept": "application/json",
                "Content-Type": "application/x-www-form-urlencoded",
            },
            data={
                "grant_type": "client_credentials",
                "scope": self.scope,
                "client_id": self.client_id,
                "client_secret": self.client_secret,
            },
            timeout=60,
        )
        if resp.status_code != 200:
            raise RuntimeError(
                f"Token request failed ({resp.status_code}): {resp.text}\n"
                "If this is 429, you have hit the 5-tokens/24h limit — wait and "
                "reuse the cached token."
            )
        tok = resp.json()
        tok["_obtained_at"] = int(time.time())
        self._write_cached_token(tok)
        return tok

    def get_access_token(self, force_refresh: bool = False) -> str:
        tok = None if force_refresh else self._read_cached_token()
        if tok and self._token_is_fresh(tok):
            return tok["access_token"]
        return self._fetch_new_token()["access_token"]

    # ----- requests -------------------------------------------------------
    def request(
        self,
        method: str,
        path: str,
        params: dict | None = None,
        json_body: dict | None = None,
        _retried_auth: bool = False,
    ) -> dict:
        token = self.get_access_token()
        url = f"{self.base_url}{path}" if path.startswith("/") else f"{self.base_url}/{path}"
        resp = requests.request(
            method,
            url,
            headers={
                "Authorization": f"Bearer {token}",
                "Accept": "application/json",
                "Content-Type": "application/json",
            },
            params=params,
            json=json_body,
            timeout=90,
        )

        # Token expired mid-life -> refresh once and retry.
        if resp.status_code == 403 and not _retried_auth:
            self.get_access_token(force_refresh=True)
            return self.request(method, path, params, json_body, _retried_auth=True)

        # Rate limited -> honor Retry-After and retry once.
        if resp.status_code == 429:
            wait = int(resp.headers.get("Retry-After", "5"))
            print(f"Rate limited (429). Waiting {wait}s then retrying {method} {path}…")
            time.sleep(wait)
            return self.request(method, path, params, json_body, _retried_auth=_retried_auth)

        if not resp.ok:
            raise RuntimeError(f"{method} {path} failed ({resp.status_code}): {resp.text}")

        return resp.json() if resp.text else {}

    def get(self, path: str, params: dict | None = None) -> dict:
        return self.request("GET", path, params=params)

    def post(self, path: str, json_body: dict) -> dict:
        return self.request("POST", path, json_body=json_body)


if __name__ == "__main__":
    # Smoke test: mint/reuse a token and confirm it works, without changing data.
    client = GuestyClient()
    token = client.get_access_token()
    print(f"Got access token (…{token[-6:]}). Cached at {client.token_path}")
    data = client.get("/listings", params={"limit": 1})
    total = data.get("count", data.get("total", "?"))
    print(f"GET /listings OK — account reports {total} listing(s).")
