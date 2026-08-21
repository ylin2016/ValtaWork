"""Entry point for hosting (Streamlit Community Cloud / Render).

The real dashboard lives in src/dashboard.py, which resolves every data path
through src/paths.py — and paths.py is rooted at THIS folder (its parent.parent
is the deploy/ root), so the packaged config/, inputs/, and db/ are found without
any code change. This shim just runs it so the platform's "main file" can sit at
the repo root.
"""
import runpy
from pathlib import Path

runpy.run_path(str(Path(__file__).parent / "src" / "dashboard.py"), run_name="__main__")
