"""The ONE place this project reaches into its two siblings.

Both siblings call their package ``src``, as this project does, so they cannot simply be
put on sys.path. Each is loaded once under an alias -- ``qbo_ops`` (QBO_operations) and
``osw`` (Owner_statement_whole) -- which keeps their relative imports working.

What crosses, and why it is borrowed rather than copied:
  * QBO_operations' client + token (Intuit rotates the refresh token: two copies on disk
    invalidate each other) and its Resolver / accounts.yml.
  * Owner_statement_whole's Guesty client + cached token (5 tokens / 24h, shared), the
    PM-rate resolver over owner_contracts, the payment model (channel / Stripe fees and
    their per-reservation override tables), the class map and Listing_contacts.csv.
"""
from __future__ import annotations

import csv
import importlib.util
import sqlite3
import sys
from functools import lru_cache

import yaml

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


# --- Owner_statement_whole --------------------------------------------------------
def guesty_client():
    return _sub("osw", STATEMENTS_ROOT, "guesty.client").GuestyClient()


def guesty_financials():
    return _sub("osw", STATEMENTS_ROOT, "guesty.reservation_financials")


def fetch_month():
    return _sub("osw", STATEMENTS_ROOT, "breakdown.fetch_month")


def payment_model():
    return _sub("osw", STATEMENTS_ROOT, "breakdown.payment_model")


def pm_rate():
    return _sub("osw", STATEMENTS_ROOT, "scope.pm_rate")


def to_property_id(nickname: str) -> str:
    """Guesty listing nickname -> property_id: the statement project's Guesty mapper,
    whose _NICKNAME_ALIASES folds bare building nicknames ("Seattle 7434") onto a unit."""
    return _sub("osw", STATEMENTS_ROOT, "breakdown.convert_export").to_property_id(nickname)


def tax_rates_csv():
    return STATEMENTS_ROOT / "config" / "_listing_tax_rates.csv"


def statements_db():
    """Read-only connection to the statement DB (owner_contracts)."""
    p = STATEMENTS_ROOT / "db" / "owner_statement.sqlite"
    return sqlite3.connect(f"file:{p}?mode=ro", uri=True)


@lru_cache(maxsize=1)
def class_map() -> dict[str, dict]:
    """property_id -> mapping_classes.yml entry."""
    items = yaml.safe_load((STATEMENTS_ROOT / "config" / "mapping_classes.yml").read_text())
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
    with (STATEMENTS_ROOT / "config" / "Listing_contacts.csv").open(encoding="utf-8-sig") as fh:
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
