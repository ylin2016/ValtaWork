"""
Shared preprocessing utilities for the CleaningFeeApp.

You can move common data-loading / feature-engineering functions here
if you want to reuse them across training scripts, notebooks, etc.
"""

from __future__ import annotations

from pathlib import Path
from typing import Tuple

import numpy as np
import pandas as pd


def project_root() -> Path:
    """Return the root directory of the project (CleaningFeeApp)."""
    return Path(__file__).resolve().parents[1]


def data_dir() -> Path:
    """Return the data directory path."""
    return project_root() / "data"
