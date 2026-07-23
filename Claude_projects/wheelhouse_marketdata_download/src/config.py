from pathlib import Path
import os
import yaml
from dotenv import load_dotenv

# Same lightweight pattern as the owner_statement_mvp project: config lives in a
# YAML file, secrets live in a .env loaded here.

DEFAULT_CONFIG_PATH = str(Path(__file__).resolve().parent.parent / "config.yml")


def load_config(config_path: str = DEFAULT_CONFIG_PATH) -> dict:
    load_dotenv()
    return yaml.safe_load(Path(config_path).read_text(encoding="utf-8"))


def env(name: str, default=None):
    return os.environ.get(name, default)
