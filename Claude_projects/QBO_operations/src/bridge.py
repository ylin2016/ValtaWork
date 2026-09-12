"""The ONE place QBO_operations reads Owner_statement_whole.

The dependency runs in exactly one direction -- this project reads the statement
project's reference data, and the statement project never reads this one (it takes
only the OAuth token file, which this project owns).  Keeping every crossing here
means the coupling is visible in one file and each consumer can override it with a
CLI flag.

What crosses, and why it is not copied:

  * ``mapping_classes.yml``   -- property_id -> QBO class.  Maintained by the
    statement pipeline from ``Listing_contacts.csv``; a copy would go stale the
    first time a listing is added.
  * the Guesty exports under ``inputs/<period>/`` -- check-in/check-out dates for
    JE descriptions.  Guesty allows 5 API tokens/24h, so the stored exports are the
    only affordable source.
  * ``ltr.labels.to_property_id`` -- the statement project's CLAUDE.md makes this
    the ONE Listing-label -> property_id map.  Imported, never duplicated.  It was
    split out of ``ltr.records`` (which needs pandas) into a dependency-free module
    precisely so this import stays cheap.
  * ``db/owner_statement.sqlite`` -- read-only, to resolve a confirmation code to
    the listing that carried it.
"""
from __future__ import annotations

import sys
from pathlib import Path

from .paths import STATEMENTS_ROOT


def statements_root(override: str | None = None) -> Path:
    root = Path(override) if override else STATEMENTS_ROOT
    if not root.is_dir():
        raise SystemExit(
            f"Owner_statement_whole not found at {root}.\n"
            f"Pass --statements-root if it lives somewhere else.")
    return root


def mapping_classes(override: str | None = None) -> Path:
    return statements_root(override) / "config" / "mapping_classes.yml"


def ledger_db(override: str | None = None) -> Path:
    return statements_root(override) / "db" / "owner_statement.sqlite"


def guesty_inputs(override: str | None = None) -> Path:
    """inputs/ -- holds guesty_converted.csv and Guesty_booking_<p>.csv per period."""
    return statements_root(override) / "inputs"


def to_property_id(override: str | None = None):
    """Import the statement project's Listing-label -> property_id map.

    Imported rather than copied: its CLAUDE.md names it the single source, and a
    second copy here would silently disagree the next time a listing is renamed.
    """
    src = statements_root(override) / "src"
    if str(src) not in sys.path:
        sys.path.insert(0, str(src))
    from ltr.labels import to_property_id as fn  # noqa: PLC0415 - deliberate late import
    return fn
