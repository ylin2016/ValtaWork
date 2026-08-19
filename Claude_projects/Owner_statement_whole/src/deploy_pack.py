"""Package the display-only ``deploy/`` bundle from the freshly-built project.

    python -m src.deploy_pack --period 2026-07

Automates ``deploy/README.md``'s re-pack recipe so the hosted dashboard reflects
the latest local build. One command does everything that used to be manual:

  1. VACUUM the ledger DB into ``deploy/db/`` (compact read-only snapshot).
  2. Copy the period's display CSVs (payment_breakdown, guesty_converted, LTR).
  3. Copy the human-maintained config (mapping_classes, config.yml, Listing_contacts).
  4. Sync the display-only code bundle: ``paths``/``listing_filter``/``ltr_records``/
     ``pm_rate`` verbatim, and ``dashboard.py`` with the deploy-only PASSWORD GATE
     re-injected after ``set_page_config`` (the step that was easy to forget by hand).

The deploy bundle is display-only: no secrets, no pipeline/QBO/Guesty code. Run this
AFTER ``run_month_close build`` for the period; the deploy stays a snapshot until re-packed.
"""
import argparse
import shutil
import sqlite3
from pathlib import Path

from . import paths

DEPLOY = paths.PROJECT_ROOT / "deploy"

# Display-only code copied VERBATIM (no gate, no pipeline code). `paths.py` is flat;
# `scope/` (property scope + PM rate) and `ltr/` (LTR display records) ship as packages.
# NOTE: ltr/import_ltr.py is the LTR *importer* (needs common.db) — a workflow module,
# NOT display-only — so it is deliberately excluded from the deploy bundle.
FLAT_MODULES = ["paths.py"]
DISPLAY_PACKAGES = {
    "scope": ["__init__.py", "listing_filter.py", "pm_rate.py"],
    "ltr": ["__init__.py", "records.py"],
    # booking_breakdown is the SHARED Section-1 builder the dashboard imports (flat);
    # it is dependency-free (data-only). excel_writer etc. are NOT shipped (pipeline-only).
    "reporting": ["__init__.py", "booking_breakdown.py"],
}
# Stale flat modules from the pre-scope/ltr layout — removed if a prior pack left them.
_STALE_FLAT = ["listing_filter.py", "ltr_records.py", "pm_rate.py"]

# Human-maintained config the dashboard / listing_filter read.
CONFIG_FILES = ["mapping_classes.yml", "config.yml", "Listing_contacts.csv"]

# Tables no deploy/src code reads — stripped from the snapshot to keep the
# committed DB small. `exceptions` is raw pipeline diagnostics (~68% of the file);
# `statement_outputs` is a build manifest (output_path/sha256) the dashboard never
# queries (grep deploy/src: no FROM statement_outputs).
DEPLOY_DROP_TABLES = ["exceptions", "statement_outputs"]

# The only `ledger_lines` rows the dashboard/ltr code ever SELECTs (see
# deploy/src/dashboard.py load_statement_data + ltr/records.py). Everything else —
# ~86% of the table: central-supply formula lines, QBO expense detail on non-owner
# accounts, transfers, other months — is dead weight in a display-only snapshot.
# A SUPERSET of what the queries filter (we don't replicate include_in_statement
# here, to never drop a row a query might read). Rows are additionally scoped to the
# shipped periods by posting_date. Keep this in sync with the dashboard's queries.
LEDGER_KEEP_PREDICATE = (
    "("
    "   (source='guesty' AND category='INCOME')"          # bookings + STR/LTR dedup + existence check
    " OR (source='qbo' AND category='INCOME')"            # other income (rent/deferred)
    " OR (category='EXPENSE' AND qbo_account LIKE '%Owner Expenses%')"  # itemized owner expenses
    " OR (qbo_account LIKE '%Taxes Paid to Owners%')"     # per-booking owner tax pass-through
    " OR (source_object IN ('LTR','DEFERRED'))"           # LTR/deferred codes
    ")"
)

# Per-period display CSVs the dashboard reads (Section 1 + LTR). Missing ones are skipped.
def _period_inputs(period: str) -> list[str]:
    return [f"payment_breakdown_{period}.csv", "guesty_converted.csv", f"LTR_{period}.csv"]

# The one deploy-specific addition on top of an otherwise verbatim dashboard.py.
GATE = '''# ── Password gate (deploy only) ───────────────────────────────────────────────
# This packaged copy carries real owner data, so a public URL must be gated. The
# gate is enforced ONLY when an `app_password` secret is configured (Streamlit
# Cloud → Secrets, or .streamlit/secrets.toml). With no secret set (local dev),
# the gate is off. The main project's dashboard has no gate; this block is the
# one deploy-specific addition on top of an otherwise verbatim copy.
try:
    _APP_PW = st.secrets.get("app_password")
except Exception:
    _APP_PW = None
if _APP_PW:
    if not st.session_state.get("_authed"):
        _gate = st.empty()
        with _gate.container():
            _pw = st.text_input("Password", type="password")
            if not _pw:
                st.stop()
            if _pw != _APP_PW:
                st.error("Incorrect password")
                st.stop()
        # Correct password: clear the prompt, mark authed, rerun clean.
        st.session_state["_authed"] = True
        _gate.empty()
        st.rerun()

'''


def _inject_gate(src_text: str) -> str:
    """Return dashboard.py source with the password gate inserted right after the
    ``st.set_page_config(...)`` call (mirrors the hand-maintained deploy copy)."""
    lines = src_text.splitlines(keepends=True)
    try:
        start = next(i for i, l in enumerate(lines) if l.lstrip().startswith("st.set_page_config("))
    except StopIteration:
        raise SystemExit("deploy_pack: could not find st.set_page_config( in dashboard.py")
    j = start
    while ")" not in lines[j]:
        j += 1
    insert_at = j + 1
    # Keep exactly one blank line between the call and the gate.
    if insert_at < len(lines) and lines[insert_at].strip() == "":
        insert_at += 1
        sep = ""
    else:
        sep = "\n"
    return "".join(lines[:insert_at]) + sep + GATE + "".join(lines[insert_at:])


def _copy(src: Path, dst: Path, label: str) -> None:
    if not src.exists():
        print(f"  skip {label}: {src} not found")
        return
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(src, dst)
    print(f"  {label}: {src.name}")


def pack(periods) -> None:
    if isinstance(periods, str):
        periods = [periods]
    src_dir = paths.PROJECT_ROOT / "src"
    print(f"Packing deploy/ for {', '.join(periods)}\n")

    # 1. DB — VACUUM INTO a compact read-only snapshot, then drop tables the
    #    display never reads. `exceptions` is pure pipeline diagnostics (~68% of
    #    the file) and is queried by nothing under deploy/src, so shipping it just
    #    bloats the repo with raw data. Drop it and re-VACUUM to reclaim the pages.
    (DEPLOY / "db").mkdir(parents=True, exist_ok=True)
    dst_db = DEPLOY / "db" / "owner_statement.sqlite"
    dst_db.unlink(missing_ok=True)
    with sqlite3.connect(str(paths.DB_PATH)) as conn:
        conn.execute("VACUUM INTO ?", (str(dst_db),))
    with sqlite3.connect(str(dst_db)) as snap:
        for table in DEPLOY_DROP_TABLES:
            snap.execute(f"DROP TABLE IF EXISTS {table}")
        # Keep only the shipped periods' statement rows so the dashboard's period
        # selector (which reads statement_runs) can't surface a stale month whose
        # display CSVs aren't shipped. No FK cascade here, so prune all three tables
        # by run_id. Main DB is untouched — this only trims the read-only snapshot.
        ph = ",".join("?" * len(periods))
        keep = snap.execute(
            f"SELECT run_id FROM statement_runs "
            f"WHERE strftime('%Y-%m', period_start) IN ({ph})", periods
        ).fetchall()
        keep_ids = [r[0] for r in keep]
        kph = ",".join("?" * len(keep_ids))
        for table in ("statement_property_totals", "statement_runs"):
            snap.execute(f"DELETE FROM {table} WHERE run_id NOT IN ({kph})", keep_ids)
        # Prune ledger_lines to the shipped periods AND the rows the display reads.
        snap.execute(
            f"DELETE FROM ledger_lines WHERE NOT ("
            f"strftime('%Y-%m', posting_date) IN ({ph}) AND {LEDGER_KEEP_PREDICATE})",
            periods,
        )
        ledger_kept = snap.execute("SELECT count(*) FROM ledger_lines").fetchone()[0]
        snap.commit()  # VACUUM cannot run inside the DELETEs' implicit transaction
        snap.execute("VACUUM")
    print(f"  db: VACUUM'd (dropped {', '.join(DEPLOY_DROP_TABLES)}; "
          f"kept periods {', '.join(periods)}; {ledger_kept} ledger rows) -> "
          f"{dst_db.relative_to(paths.PROJECT_ROOT)} "
          f"({dst_db.stat().st_size // (1024*1024)} MB)")

    # 2. Period display CSVs — one inputs/<period>/ subdir per shipped month.
    for period in periods:
        for name in _period_inputs(period):
            _copy(paths.inputs_dir(period) / name, DEPLOY / "inputs" / period / name, "input")

    # 3. Config.
    for name in CONFIG_FILES:
        _copy(paths.CONFIG_DIR / name, DEPLOY / "config" / name, "config")

    # 4. Display-only code: verbatim helpers + dashboard.py with the gate re-injected.
    for name in _STALE_FLAT:  # drop leftovers from the old flat layout
        (DEPLOY / "src" / name).unlink(missing_ok=True)
    for name in FLAT_MODULES:
        _copy(src_dir / name, DEPLOY / "src" / name, "code")
    for pkg, mods in DISPLAY_PACKAGES.items():
        for name in mods:
            _copy(src_dir / pkg / name, DEPLOY / "src" / pkg / name, f"code {pkg}/")
    dash = (src_dir / "dashboard.py").read_text()
    (DEPLOY / "src" / "dashboard.py").write_text(_inject_gate(dash))
    print("  code: dashboard.py (+ password gate re-injected)")

    print("\nDone. Commit & redeploy the deploy/ folder to publish the snapshot.")


def main() -> None:
    ap = argparse.ArgumentParser(description="Re-pack the display-only deploy/ bundle.")
    ap.add_argument("--period", required=True, nargs="+",
                    help="One or more YYYY-MM (e.g. --period 2026-06 2026-07).")
    pack(ap.parse_args().period)


if __name__ == "__main__":
    main()
