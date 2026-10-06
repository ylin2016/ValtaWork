"""Read the stored Guesty pulls (never the API -- see guesty_pull)."""
from __future__ import annotations

import json
from pathlib import Path

from .paths import GUESTY_INPUTS


def latest_pull(path: str | None = None) -> tuple[Path, dict]:
    p = Path(path) if path else max(GUESTY_INPUTS.glob("reservations_*.json"))
    return p, json.loads(p.read_text())
