"""Wheelhouse client settings: config.yml next to this file + WHEELHOUSE_API_KEY from
shared/secrets/.env. A project with its own config (weekly_revenue's downloader) passes
a dict with the same `api` section to WheelhouseClient instead."""
import os
from pathlib import Path

import yaml
from dotenv import load_dotenv

from ..paths import ENV_FILE

load_dotenv(ENV_FILE)


def load_config(config_path: Path = Path(__file__).resolve().parent / "config.yml") -> dict:
    return yaml.safe_load(Path(config_path).read_text(encoding="utf-8"))


def env(name: str, default=None):
    return os.environ.get(name, default)
