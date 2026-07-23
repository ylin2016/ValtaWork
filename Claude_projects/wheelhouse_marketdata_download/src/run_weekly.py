"""Orchestrator: pull Wheelhouse market data + dynamic sets for one weekly snapshot.

Usage:
  python -m src.run_weekly                      # full pull, snapshot = today
  python -m src.run_weekly --only listings      # just listings (auth smoke test)
  python -m src.run_weekly --only market
  python -m src.run_weekly --only dynamic_sets
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
from .download_market_data import download_markets, download_neighborhood
from .download_dynamic_sets import download_dynamic_sets
from .export_csv import export_snapshot


def parse_args(argv=None):
    p = argparse.ArgumentParser(description="Download Wheelhouse market data & dynamic sets.")
    p.add_argument("--snapshot-date", default=date.today().isoformat(),
                   help="Snapshot date tag (YYYY-MM-DD). Default: today.")
    p.add_argument("--only", choices=["listings", "market", "dynamic_sets"],
                   help="Run only one part of the pull.")
    p.add_argument("--limit", type=int, default=None,
                   help="Cap items per group (listings/markets/sets) — for dev/testing.")
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

    # Listings are needed by market (neighborhood) pulls, so fetch them whenever
    # we're doing market data or listings.
    listings = []
    if do_all or args.only in ("listings", "market"):
        print("Listings...")
        listings = download_listings(client, conn, cfg, snapshot_date, limit=args.limit)

    if do_all or args.only == "market":
        print("Market data...")
        market_ids = sorted({l["market_id"] for l in listings if l.get("market_id")})
        download_markets(client, conn, cfg, snapshot_date, market_ids, limit=args.limit)
        download_neighborhood(client, conn, cfg, snapshot_date, listings, limit=args.limit)

    if do_all or args.only == "dynamic_sets":
        print("Dynamic sets...")
        download_dynamic_sets(client, conn, cfg, snapshot_date, limit=args.limit)

    if not args.no_export:
        print("Export...")
        export_snapshot(conn, cfg["app"]["export_dir"], snapshot_date)

    conn.close()
    print("Done.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
