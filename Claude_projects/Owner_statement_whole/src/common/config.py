from pathlib import Path
import os
import yaml
from dotenv import load_dotenv
from ..paths import ENV_FILE

def load_config(config_path: str) -> dict:
    load_dotenv(ENV_FILE)
    return yaml.safe_load(Path(config_path).read_text(encoding="utf-8"))

def env(name: str, default=None):
    return os.environ.get(name, default)
