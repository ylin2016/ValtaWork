"""Central paths for qbo_reservation_bookkeeping. Dependency-free (only pathlib)."""
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
CLAUDE_PROJECTS = PROJECT_ROOT.parent

CONFIG_DIR = PROJECT_ROOT / "config"
INPUTS_DIR = PROJECT_ROOT / "inputs"
GUESTY_INPUTS = INPUTS_DIR / "guesty"     # stored Guesty pulls (5 tokens / 24h -- reuse these)
QBO_INPUTS = INPUTS_DIR / "qbo"           # read-only snapshots of what VRP / we already posted
REVIEW_DIR = PROJECT_ROOT / "review"

# Siblings -- read through src/bridge.py only, never copied. Shared code, reference
# tables and secrets come from Claude_projects/shared/ (valta_common).
QBO_OPS_ROOT = CLAUDE_PROJECTS / "QBO_operations"
STATEMENTS_ROOT = CLAUDE_PROJECTS / "Owner_statement_whole"


def review_csv(name: str) -> Path:
    return REVIEW_DIR / (name if name.endswith(".csv") else f"{name}.csv")
