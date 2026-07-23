import json
import time
from datetime import datetime, timezone

import requests

from .config import env


class WheelhouseError(RuntimeError):
    pass


class WheelhouseClient:
    """Thin client for the Wheelhouse Revenue Management API.

    - Auth via the X-Integration-Api-Key header.
    - Throttles to stay under the ~60 req/min rate limit.
    - Retries 429 and 5xx with exponential backoff.
    - Optionally logs every raw response into the raw_responses table.
    """

    def __init__(self, cfg: dict, conn=None, snapshot_date: str | None = None):
        api = cfg["api"]
        self.base_url = api["base_url"].rstrip("/")
        self.timeout = api.get("timeout_seconds", 60)
        self.max_retries = api.get("max_retries", 5)
        self.min_interval = 60.0 / max(1, api.get("requests_per_minute", 55))
        self._last_call = 0.0

        self.conn = conn
        self.snapshot_date = snapshot_date

        self.api_key = env("WHEELHOUSE_API_KEY")
        if not self.api_key:
            raise WheelhouseError(
                "Missing WHEELHOUSE_API_KEY in environment (.env). "
                "Create an RM API key in your Wheelhouse profile and add it to .env."
            )

        self.session = requests.Session()
        self.session.headers.update(
            {
                "X-Integration-Api-Key": self.api_key,
                "Accept": "application/json",
            }
        )

    # -- internal ----------------------------------------------------------

    def _throttle(self) -> None:
        elapsed = time.monotonic() - self._last_call
        if elapsed < self.min_interval:
            time.sleep(self.min_interval - elapsed)
        self._last_call = time.monotonic()

    def _log_raw(self, endpoint: str, ref_id: str, status: int, body) -> None:
        if self.conn is None or self.snapshot_date is None:
            return
        self.conn.execute(
            "INSERT OR REPLACE INTO raw_responses "
            "(snapshot_date, endpoint, ref_id, status, json, pulled_at) "
            "VALUES (?, ?, ?, ?, ?, ?)",
            (
                self.snapshot_date,
                endpoint,
                str(ref_id or ""),
                status,
                json.dumps(body) if not isinstance(body, str) else body,
                datetime.now(timezone.utc).isoformat(),
            ),
        )
        self.conn.commit()

    # -- public ------------------------------------------------------------

    def get_json(self, path: str, params: dict | None = None, ref_id: str = ""):
        """GET path (relative to base_url), return parsed JSON. Retries transient errors."""
        url = self.base_url + path
        backoff = 2.0
        last_exc = None
        for attempt in range(1, self.max_retries + 1):
            self._throttle()
            try:
                resp = self.session.get(url, params=params, timeout=self.timeout)
            except requests.RequestException as exc:  # network hiccup -> retry
                last_exc = exc
                time.sleep(backoff)
                backoff *= 2
                continue

            if resp.status_code == 204:
                # No Content — a valid "nothing for this request" answer, not an
                # error. Wheelhouse returns it when a date range predates the
                # available data.
                self._log_raw(path, ref_id, 204, None)
                return None

            if resp.status_code == 200:
                try:
                    body = resp.json()
                except ValueError:
                    body = resp.text
                self._log_raw(path, ref_id, 200, body)
                return body

            if resp.status_code == 429 or resp.status_code >= 500:
                # rate-limited or server error -> honor Retry-After, else backoff
                wait = float(resp.headers.get("Retry-After", backoff))
                time.sleep(wait)
                backoff *= 2
                last_exc = WheelhouseError(
                    f"{resp.status_code} on {path}: {resp.text[:300]}"
                )
                continue

            # non-retryable client error
            self._log_raw(path, ref_id, resp.status_code, resp.text)
            raise WheelhouseError(
                f"{resp.status_code} on {path} (params={params}): {resp.text[:500]}"
            )

        raise WheelhouseError(
            f"Gave up on {path} after {self.max_retries} retries: {last_exc}"
        )

    def get_paginated(
        self, path: str, params: dict | None = None, ref_id: str = "", per_page: int = 100
    ):
        """Yield items across all pages. Handles both list and {data|results|items:[...]} shapes."""
        params = dict(params or {})
        params["per_page"] = per_page
        page = 1
        while True:
            params["page"] = page
            body = self.get_json(path, params=params, ref_id=f"{ref_id}:p{page}")
            items = _extract_items(body)
            if not items:
                break
            for item in items:
                yield item
            if len(items) < per_page:
                break
            page += 1


def _extract_items(body):
    """Normalize a paginated payload into a list of records."""
    if body is None:
        return []
    if isinstance(body, list):
        return body
    if isinstance(body, dict):
        for key in ("data", "results", "items", "listings", "dynamic_sets"):
            if isinstance(body.get(key), list):
                return body[key]
        # single-object dict that isn't a wrapper -> treat as one record
        return [body]
    return []
