"""Config + environment for the Guesty Open API client.

Credentials come from shared/secrets/.env; the 24h access token is cached in
shared/secrets/guesty_token.json and reused by EVERY project. Guesty issues at most
5 tokens per 24h for the app — never force a refresh. (Moved from
Owner_statement_whole/src/guesty/config.py, 2026-10-09.)
"""
import os
from pathlib import Path

from dotenv import load_dotenv

from ..paths import ENV_FILE, GUESTY_TOKEN

load_dotenv(ENV_FILE)

# Fixed Guesty Open API endpoints.
_GUESTY_CONFIG = {
    "base_url": "https://open-api.guesty.com/v1",
    "token_url": "https://open-api.guesty.com/oauth2/token",
    "scope": "open-api",
    "token_path": str(GUESTY_TOKEN),
    "token_refresh_buffer_seconds": 3600,
}


def env(name: str, default=None):
    """Return an environment variable, or `default` if unset/empty."""
    val = os.environ.get(name)
    return val if val not in (None, "") else default


def load_config(path: str | None = None) -> dict:
    """Return the Guesty API client config (endpoints + token cache path)."""
    return dict(_GUESTY_CONFIG)


def resolve(path_str: str) -> Path:
    """Resolve a possibly-relative path against the current directory (the CLI tools
    write their ./data/... files into the project they are run from)."""
    p = Path(path_str)
    return p if p.is_absolute() else (Path.cwd() / p)
