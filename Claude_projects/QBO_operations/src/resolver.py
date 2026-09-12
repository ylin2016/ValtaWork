"""Name -> QBO Id lookup, cached for the life of a run.

A write payload references accounts, classes, locations and customers by Id, while
every review CSV a human reads carries names.  This is the translation layer.
"""
from __future__ import annotations

from .qbo_client import QBOClient


def esc(s: str) -> str:
    """Escape a value for a QBO query string literal."""
    return s.replace("'", "\\'")


class Resolver:
    def __init__(self, qbo: QBOClient) -> None:
        self.qbo = qbo
        self._cache: dict[tuple[str, str], str | None] = {}

    def _one(self, entity: str, field: str, value: str) -> str | None:
        key = (entity, value)
        if key not in self._cache:
            r = self.qbo.query(
                f"SELECT Id, {field} FROM {entity} WHERE {field} = '{esc(value)}'"
            )
            got = r.get("QueryResponse", {}).get(entity, [])
            self._cache[key] = got[0]["Id"] if got else None
        return self._cache[key]

    def account(self, name: str) -> str | None:
        return self._one("Account", "FullyQualifiedName", name)

    def klass(self, name: str) -> str | None:
        return self._one("Class", "FullyQualifiedName", name)

    def department(self, name: str) -> str | None:
        return self._one("Department", "FullyQualifiedName", name)

    def vendor(self, name: str) -> str | None:
        return self._one("Vendor", "DisplayName", name)

    def customer(self, name: str) -> str | None:
        """Exact DisplayName, else fall back to the confirmation code.

        Airbnb's CSV abbreviates guest names ("Heidrun" for "Heidrun Dorre"), so an
        exact match misses customers that plainly exist.  The confirmation code is
        unique, so a single LIKE hit on it is the same guest -- more than one is
        ambiguous and must not be guessed at.
        """
        hit = self._one("Customer", "DisplayName", name)
        if hit is not None:
            return hit
        code = name.rsplit(" - ", 1)[-1].strip()
        if not code:
            return None
        key = ("Customer", f"~{code}")
        if key not in self._cache:
            r = self.qbo.query(
                f"SELECT Id, DisplayName FROM Customer WHERE DisplayName LIKE '%{esc(code)}%'"
            )
            got = {c["Id"]: c["DisplayName"] for c in r.get("QueryResponse", {}).get("Customer", [])}
            if len(got) == 1:
                cid, disp = next(iter(got.items()))
                print(f"    matched by conf code: {name!r} -> {disp!r} (Id {cid})")
                self._cache[key] = cid
            else:
                self._cache[key] = None
        return self._cache[key]
