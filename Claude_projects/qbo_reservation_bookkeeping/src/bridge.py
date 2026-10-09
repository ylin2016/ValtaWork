"""The ONE place this project reaches into its two siblings.

Both siblings call their package ``src``, as this project does, so they cannot simply be
put on sys.path. Each is loaded once under an alias -- ``qbo_ops`` (QBO_operations) and
``osw`` (Owner_statement_whole) -- which keeps their relative imports working.

What crosses, and why it is borrowed rather than copied:
  * QBO_operations' client (on the ONE shared token, Claude_projects/shared/secrets/) and
    its Resolver / accounts.yml.
  * Owner_statement_whole's PM-rate resolver over owner_contracts, its listing_filter
    label normalisation and to_property_id, and its ledger DB (read-only).

The rest is shared code / data in Claude_projects/shared/ (valta_common), not a sibling:
the Guesty client + its one cached token (5 tokens / 24h for every project), the payment
model (channel / Stripe fees and their per-reservation override tables), the summary
frame it reads, and the reference tables (class map, Listing_contacts.csv, tax rates).
"""
from __future__ import annotations

import csv
import importlib.util
import sqlite3
import sys
from functools import lru_cache

import yaml
from valta_common import paths as shared

from .paths import QBO_OPS_ROOT, STATEMENTS_ROOT


def _load_pkg(alias: str, root):
    if alias in sys.modules:
        return sys.modules[alias]
    init = root / "src" / "__init__.py"
    spec = importlib.util.spec_from_file_location(
        alias, init, submodule_search_locations=[str(root / "src")])
    mod = importlib.util.module_from_spec(spec)
    sys.modules[alias] = mod
    spec.loader.exec_module(mod)
    return mod


def _sub(alias: str, root, dotted: str):
    _load_pkg(alias, root)
    return importlib.import_module(f"{alias}.{dotted}")


# --- QBO_operations ---------------------------------------------------------------
def qbo_config():
    return _sub("qbo_ops", QBO_OPS_ROOT, "config")


def qbo_client():
    """The read/write client on QBO_operations' single token store."""
    return qbo_config().client()


def resolver(qbo):
    return _sub("qbo_ops", QBO_OPS_ROOT, "resolver").Resolver(qbo)


# --- shared (valta_common) -------------------------------------------------------
def guesty_client():
    from valta_common.guesty.client import GuestyClient
    return GuestyClient()


def guesty_financials():
    return importlib.import_module("valta_common.guesty.reservation_financials")


def guesty_summary():
    """build_summary_frame: the per-reservation fee-category frame payment_model reads."""
    return importlib.import_module("valta_common.guesty.summary")


def payment_model():
    return importlib.import_module("valta_common.fees.payment_model")


def tax_rates_csv():
    return shared.LISTING_TAX_RATES


# --- Owner_statement_whole --------------------------------------------------------
def pm_rate():
    return _sub("osw", STATEMENTS_ROOT, "scope.pm_rate")


def to_property_id(nickname: str) -> str:
    """Guesty listing nickname -> property_id: the statement project's Guesty mapper,
    whose _NICKNAME_ALIASES folds bare building nicknames ("Seattle 7434") onto a unit."""
    return _sub("osw", STATEMENTS_ROOT, "breakdown.convert_export").to_property_id(nickname)


def statements_db():
    """Read-only connection to the statement DB (owner_contracts)."""
    p = STATEMENTS_ROOT / "db" / "owner_statement.sqlite"
    return sqlite3.connect(f"file:{p}?mode=ro", uri=True)


@lru_cache(maxsize=1)
def class_map() -> dict[str, dict]:
    """property_id -> mapping_classes.yml entry."""
    items = yaml.safe_load(shared.MAPPING_CLASSES.read_text())
    return {it["property_id"]: it for it in items if it.get("property_id")}


@lru_cache(maxsize=1)
def listing_contacts() -> dict[str, dict]:
    """Unit property_id -> Listing_contacts.csv row (Supplies, Owner Clean, ...).

    Keyed by the `Listing` column (the unit that takes the booking); `Property` is the
    statement it rolls into ("Cottage 3" -> "OSBR"). Falls back to Property for the old
    single-column file. The unit label resolves the way the statement project's scope
    does (listing_filter._norm/_alias), so "Cottage 11 (tiny)" -> osbr_11."""
    lf = _sub("osw", STATEMENTS_ROOT, "scope.listing_filter")
    out = {}
    with shared.LISTING_CONTACTS.open(encoding="utf-8-sig") as fh:
        for r in csv.DictReader(fh):
            label = (r.get("Listing") or r.get("Property") or "").strip()
            if label:
                n = lf._alias(lf._norm(label))
                out[n] = r
    return out


def contacts_row(property_id: str) -> dict | None:
    """The Listing_contacts row for a unit property_id, matched on the normalized form."""
    lf = _sub("osw", STATEMENTS_ROOT, "scope.listing_filter")
    rows = listing_contacts()
    n = lf._norm(property_id)
    return rows.get(n) or rows.get(lf._alias(n))
