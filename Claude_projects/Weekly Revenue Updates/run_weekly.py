"""Weekly revenue update — one command:

  1. Guesty: confirmed reservations (check-in >= 2026-01-01) via the Open API
  2. Wheelhouse: market data (overall + high performers, by bedroom)
  3. Build the revenue report (output/<date>/, and Google Drive with --publish)

    python run_weekly.py                    # as of today, local output only
    python run_weekly.py --publish          # also overwrite the Drive copies
    python run_weekly.py --skip-guesty --skip-market   # rebuild from existing pulls
"""
import argparse
import subprocess
import sys
from datetime import date

import build_artifact
import build_report
import fetch_guesty
from paths import WHEELHOUSE_PROJECT


def main():
    ap = argparse.ArgumentParser(description="Weekly revenue update")
    ap.add_argument("--asof", default=date.today().isoformat())
    ap.add_argument("--publish", action="store_true")
    ap.add_argument("--skip-guesty", action="store_true")
    ap.add_argument("--skip-market", action="store_true")
    args = ap.parse_args()

    if not args.skip_guesty:
        print("[1/3] Guesty reservations")
        fetch_guesty.main(["--asof", args.asof])
    if not args.skip_market:
        print("[2/3] Wheelhouse market data")
        # its own venv + package; keep it a separate process ('src' name clash)
        subprocess.run([str(WHEELHOUSE_PROJECT / ".venv" / "bin" / "python"), "-m", "src.run_weekly",
                        "--only", "market", "--snapshot-date", args.asof],
                       cwd=WHEELHOUSE_PROJECT, check=True)
    print("[3/3] Report")
    build_report.main(["--asof", args.asof, "--market-snapshot", args.asof]
                      + (["--publish"] if args.publish else []))
    build_artifact.build(args.asof)   # output/revenue_dashboard.html -> republish the artifact


if __name__ == "__main__":
    sys.exit(main())
