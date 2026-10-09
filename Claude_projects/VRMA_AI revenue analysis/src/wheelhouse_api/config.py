"""Config for the Wheelhouse client: config.yml next to this file + the API key in
secrets/.env (copied from wheelhouse_marketdata_download/src/config.py, 2026-10-09)."""
import os
from pathlib import Path

import yaml
from dotenv import load_dotenv

HERE = Path(__file__).resolve().parent
ENV_FILE = HERE.parent.parent / "secrets" / ".env"


def load_config(config_path: Path = HERE / "config.yml") -> dict:
    load_dotenv(ENV_FILE)
    return yaml.safe_load(Path(config_path).read_text(encoding="utf-8"))


def env(name: str, default=None):
    return os.environ.get(name, default)
