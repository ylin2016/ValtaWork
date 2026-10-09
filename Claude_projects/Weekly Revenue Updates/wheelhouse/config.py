"""Config for the Wheelhouse pull: wheelhouse/config.yml + the API key in secrets/.env.

Copied from wheelhouse_marketdata_download (src/config.py, 2026-10-09) so this
project runs on its own. The DB is the shared Claude_projects/shared_data/wheelhouse.sqlite
(paths.WHEELHOUSE_DB); export_dir resolves against the project folder.
"""
import os
from pathlib import Path

import yaml
from dotenv import load_dotenv

from paths import ENV_FILE, PROJECT, WHEELHOUSE_DB

DEFAULT_CONFIG_PATH = str(Path(__file__).resolve().parent / "config.yml")


def load_config(config_path: str = DEFAULT_CONFIG_PATH) -> dict:
    load_dotenv(ENV_FILE)
    cfg = yaml.safe_load(Path(config_path).read_text(encoding="utf-8"))
    cfg["app"]["db_path"] = str(WHEELHOUSE_DB)     # the shared DB (paths.py)
    p = Path(cfg["app"]["export_dir"])
    cfg["app"]["export_dir"] = str(p if p.is_absolute() else PROJECT / p)
    return cfg


def env(name: str, default=None):
    return os.environ.get(name, default)
