"""Config + environment loading for the Guesty API client.

Reads `config.yml` (non-secret settings) and `.env` (credentials). Both are
resolved relative to the project root so scripts work regardless of CWD.
"""
import os
from pathlib import Path

import yaml
from dotenv import load_dotenv

PROJECT_ROOT = Path(__file__).resolve().parent.parent

# Load .env from the project root (no-op if the file is absent).
load_dotenv(PROJECT_ROOT / ".env")


def env(name: str, default=None):
    """Return an environment variable, or `default` if unset/empty."""
    val = os.environ.get(name)
    return val if val not in (None, "") else default


def load_config(path: str | None = None) -> dict:
    """Load config.yml as a dict."""
    cfg_path = Path(path) if path else PROJECT_ROOT / "config.yml"
    with open(cfg_path, "r") as f:
        return yaml.safe_load(f)


def resolve(path_str: str) -> Path:
    """Resolve a possibly-relative path from config against the project root."""
    p = Path(path_str)
    return p if p.is_absolute() else (PROJECT_ROOT / p)
