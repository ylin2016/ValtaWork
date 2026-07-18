"""Guesty listing custom-field definitions (fieldId <-> human name).

A listing stores custom fields as bare `{fieldId, value}` — the readable name
lives in the account definitions (`GET /accounts/{id}/custom-fields`, where each
def has `fieldId`, `key` (label), `displayName` (snake_case), `object`, `type`).
We cache those to `data/custom_fields.json` so the template tools can label
custom fields offline. Template column names are `cf_` + displayName.
"""
import json

from .config import resolve

CACHE_PATH = "./data/custom_fields.json"
LISTINGS_JSON = "./data/listings.json"
CF_PREFIX = "cf_"


def get_account_id() -> str | None:
    """Derive the account id from the pulled listings (they all carry accountId)."""
    p = resolve(LISTINGS_JSON)
    if not p.exists():
        return None
    for x in json.loads(p.read_text()):
        if x.get("accountId"):
            return x["accountId"]
    return None


def fetch_and_cache(client, account_id: str | None = None) -> list[dict]:
    """Fetch listing custom-field definitions from the API and cache them."""
    account_id = account_id or get_account_id()
    if not account_id:
        raise RuntimeError("No accountId found — run `python -m src.pull_listings` first.")
    r = client.get(f"/accounts/{account_id}/custom-fields")
    items = r.get("results", r.get("data", r)) if isinstance(r, dict) else r
    if not isinstance(items, list):
        items = [items]
    defs = [it for it in items if it.get("object") in (None, "listing")]
    cache = resolve(CACHE_PATH)
    cache.parent.mkdir(parents=True, exist_ok=True)
    cache.write_text(json.dumps(defs, indent=2, default=str))
    return defs


def load_definitions() -> list[dict]:
    """Read cached definitions (empty list if never fetched)."""
    p = resolve(CACHE_PATH)
    return json.loads(p.read_text()) if p.exists() else []


def _colname(d: dict) -> str:
    name = d.get("displayName") or d.get("key") or d.get("fieldId")
    return CF_PREFIX + str(name)


def cf_columns(defs: list[dict]) -> list[str]:
    return [_colname(d) for d in defs if d.get("fieldId")]


def id_to_column(defs: list[dict]) -> dict:
    return {d["fieldId"]: _colname(d) for d in defs if d.get("fieldId")}


def column_to_id(defs: list[dict]) -> dict:
    return {_colname(d): d["fieldId"] for d in defs if d.get("fieldId")}
