"""Entry point for hosting (Streamlit Community Cloud / Render).

The real dashboard lives in src/dashboard.py, which resolves its data paths
relative to its own location (../data, ../config.yml, ../mapping_classes.yml).
This shim just runs it so the platform's "main file" can sit at the repo root.
"""
import runpy
from pathlib import Path

runpy.run_path(str(Path(__file__).parent / "src" / "dashboard.py"), run_name="__main__")
