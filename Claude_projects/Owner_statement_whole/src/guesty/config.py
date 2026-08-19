"""Config + environment for the Guesty Open API client (Task 1).

The Guesty REST/OAuth endpoints are fixed constants (they never change);
credentials come from config/secrets/.env; the 24h access token is cached to
config/secrets/guesty_token.json. All locations resolve through src/paths.py so
this module never hardcodes a project path.
"""
import os
from pathlib import Path

from dotenv import load_dotenv

from ..paths import ENV_FILE, GUESTY_TOKEN, PROJECT_ROOT

# Load Guesty (+ QBO) credentials from the shared secrets .env.
load_dotenv(ENV_FILE)

# Fixed Guesty Open API endpoints (formerly GuestyFinancials/config.yml).
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
    """Resolve a possibly-relative path against the project root."""
    p = Path(path_str)
    return p if p.is_absolute() else (PROJECT_ROOT / p)
