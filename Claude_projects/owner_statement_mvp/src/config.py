from pathlib import Path
import os
import yaml
from dotenv import load_dotenv

def load_config(config_path: str) -> dict:
    load_dotenv()
    return yaml.safe_load(Path(config_path).read_text(encoding="utf-8"))

def env(name: str, default=None):
    return os.environ.get(name, default)
