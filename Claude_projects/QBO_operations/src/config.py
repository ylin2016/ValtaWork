"""Config loading and the one QBOClient factory.

Six scripts used to repeat the same eight lines -- load_dotenv, read config.yml,
pull the ``qbo`` block, construct QBOClient.  ``client()`` is that, once.
"""
from __future__ import annotations

from functools import lru_cache

import yaml
from dotenv import load_dotenv

from .paths import ACCOUNTS_YML, BOOKING_IDS, CONFIG_YML, ENV_FILE, PAYEES_YML
from .qbo_client import QBOClient


@lru_cache(maxsize=1)
def config() -> dict:
    return yaml.safe_load(CONFIG_YML.read_text())


@lru_cache(maxsize=1)
def accounts() -> dict:
    return yaml.safe_load(ACCOUNTS_YML.read_text())


@lru_cache(maxsize=1)
def payees() -> dict:
    """Zelle-sheet payee spelling -> QuickBooks Vendor DisplayName."""
    if not PAYEES_YML.exists():
        return {}
    return (yaml.safe_load(PAYEES_YML.read_text()) or {}).get("payees", {})


@lru_cache(maxsize=1)
def booking_ids() -> dict[str, list[str]]:
    """Booking.com Property ID -> listing nickname(s), from `config/booking_Id.csv`.

    Owner-maintained and the single source.  A nickname legitimately has MORE THAN ONE
    id -- `Microsoft 14615-D303` has two, one per Booking.com listing of the same unit --
    so this is many ids to one listing, never the reverse.

    Read here rather than in each consumer: the payout builder needs it because the raw
    Booking.com export carries no nickname at all, and the commission reconciliation needs
    it to join the invoice to a class.  Two readers would disagree the first time a
    listing is renamed.
    """
    import csv  # noqa: PLC0415 - only this one function needs it
    from collections import defaultdict  # noqa: PLC0415
    m: dict[str, set] = defaultdict(set)
    if BOOKING_IDS.exists():
        with BOOKING_IDS.open(encoding="utf-8-sig") as fh:
            for r in csv.DictReader(fh):
                pid, nk = (r.get("ID") or "").strip(), (r.get("NICKNAME") or "").strip()
                if pid and nk:
                    m[pid].add(nk)
    return {k: sorted(v) for k, v in m.items()}


def acct_id(key: str) -> str:
    """The QBO Id for a named account, e.g. acct_id('clearing_airbnb') -> '1626'."""
    return accounts()["accounts"][key]["id"]


def acct_name(key: str) -> str:
    """The FullyQualifiedName for a named account."""
    return accounts()["accounts"][key]["name"]


def name(key: str) -> str:
    """An account referenced by name only, e.g. name('bank_str')."""
    return accounts()["names"][key]


def location() -> str:
    """The QBO Location every JE this project writes is stamped with."""
    return accounts()["location"]


def client() -> QBOClient:
    load_dotenv(ENV_FILE)
    q = config()["qbo"]
    return QBOClient(realm_id=q["realm_id"], base_url=q["base_url"],
                     minorversion=int(q.get("minorversion", 75)))
