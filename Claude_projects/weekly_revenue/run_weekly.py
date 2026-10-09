"""Weekly revenue update — one command:

  0. Refresh the live inputs in data/inputs/ from Google Drive (skipped without Drive)
  1. Guesty: confirmed reservations (check-in >= 2026-01-01) via the Open API
  2. Wheelhouse: comp sets + market data (overall + high performers, by bedroom)
  3. Build the revenue report (output/<date>/, and Google Drive with --publish)
     and the dashboard page (output/revenue_dashboard.html)

    python run_weekly.py                    # as of today, local output only
    python run_weekly.py --publish          # also overwrite the Drive copies
    python run_weekly.py --skip-guesty --skip-market   # rebuild from existing pulls
"""
import argparse
import sys
from datetime import date

import build_artifact
import build_report
import fetch_guesty
import sync_inputs
from wheelhouse import run_weekly as wheelhouse_pull


def main():
    ap = argparse.ArgumentParser(description="Weekly revenue update")
    ap.add_argument("--asof", default=date.today().isoformat())
    ap.add_argument("--publish", action="store_true")
    ap.add_argument("--skip-guesty", action="store_true")
    ap.add_argument("--skip-market", action="store_true")
    ap.add_argument("--no-sync", action="store_true", help="Don't refresh data/inputs from Drive")
    args = ap.parse_args()

    if not args.no_sync:
        sync_inputs.main([])
    if not args.skip_guesty:
        print("[1/3] Guesty reservations")
        fetch_guesty.main(["--asof", args.asof])
    if not args.skip_market:
        print("[2/3] Wheelhouse market data")
        wheelhouse_pull.main(["--only", "report", "--snapshot-date", args.asof, "--no-export"])
    print("[3/3] Report")
    build_report.main(["--asof", args.asof, "--market-snapshot", args.asof]
                      + (["--publish"] if args.publish else []))
    build_artifact.build(args.asof)   # output/revenue_dashboard.html -> republish the artifact


if __name__ == "__main__":
    sys.exit(main())
