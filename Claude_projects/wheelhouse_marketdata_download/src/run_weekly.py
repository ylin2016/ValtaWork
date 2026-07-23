"""Orchestrator: pull Wheelhouse market data + dynamic sets for one weekly snapshot.

Market reports are pulled for the cities configured in config.yml
(params.market_names). Dynamic-set data is pulled for every set (your portfolio).

Usage:
  python -m src.run_weekly                      # full pull, snapshot = today
  python -m src.run_weekly --only market        # just market reports
  python -m src.run_weekly --only dynamic_sets  # just dynamic sets
  python -m src.run_weekly --only listings      # /listings demo (auth smoke test)
  python -m src.run_weekly --snapshot-date 2026-07-20
  python -m src.run_weekly --limit 3            # cap items per group (dev)
  python -m src.run_weekly --no-export
"""
import argparse
from datetime import date

from .config import load_config
from . import db
from .wheelhouse_client import WheelhouseClient
from .download_listings import download_listings
from .download_market_data import download_markets
from .download_dynamic_sets import download_dynamic_sets, collect_associated_listings
from .export_csv import export_snapshot


def parse_args(argv=None):
    p = argparse.ArgumentParser(description="Download Wheelhouse market data & dynamic sets.")
    p.add_argument("--snapshot-date", default=date.today().isoformat(),
                   help="Snapshot date tag (YYYY-MM-DD). Default: today.")
    p.add_argument("--only", choices=["listings", "market", "dynamic_sets"],
                   help="Run only one part of the pull.")
    p.add_argument("--limit", type=int, default=None,
                   help="Cap items per group (markets/sets) — for dev/testing.")
    p.add_argument("--no-export", action="store_true", help="Skip CSV/xlsx export.")
    p.add_argument("--config", default=None, help="Path to config.yml.")
    return p.parse_args(argv)


def main(argv=None) -> int:
    args = parse_args(argv)
    cfg = load_config(args.config) if args.config else load_config()
    snapshot_date = args.snapshot_date

    conn = db.connect(cfg["app"]["db_path"])
    client = WheelhouseClient(cfg, conn=conn, snapshot_date=snapshot_date)

    print(f"Wheelhouse pull — snapshot {snapshot_date}"
          + (f" (limit {args.limit})" if args.limit else ""))

    do_all = args.only is None

    # --only listings is a standalone auth smoke test (GET /listings returns just a
    # demo listing). The real portfolio is stored by the dynamic-sets pull.
    if args.only == "listings":
        print("Listings...")
        download_listings(client, conn, cfg, snapshot_date, limit=args.limit)

    # Dynamic sets run first: they harvest the real listings (and their markets),
    # which market selection uses when markets_from_listings is on.
    if do_all or args.only == "dynamic_sets":
        print("Dynamic sets...")
        download_dynamic_sets(client, conn, cfg, snapshot_date, limit=args.limit)

    if do_all or args.only == "market":
        # A market-only run still needs to know which markets our listings are in,
        # so harvest them (lists sets + associated listings; no heavy detail).
        if args.only == "market":
            collect_associated_listings(client, conn, cfg, snapshot_date, limit=args.limit)
        print("Market data...")
        download_markets(client, conn, cfg, snapshot_date, limit=args.limit)

    if not args.no_export:
        print("Export...")
        export_snapshot(conn, cfg["app"]["export_dir"], snapshot_date)

    conn.close()
    print("Done.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
