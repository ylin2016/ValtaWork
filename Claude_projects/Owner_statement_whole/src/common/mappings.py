from pathlib import Path
import yaml
import re

def load_class_mapping(path: str) -> list[dict]:
    return yaml.safe_load(Path(path).read_text(encoding="utf-8"))

def load_account_rules(path: str) -> list[dict]:
    y = yaml.safe_load(Path(path).read_text(encoding="utf-8"))
    return y.get("rules", [])

def apply_account_rules(account_name: str, vendor: str, rules: list[dict]) -> tuple[str, str]:
    acct = account_name or ""
    vend = vendor or ""
    for r in rules:
        ok = True
        if "account_exact" in r:
            ok = ok and (acct.strip() == r["account_exact"].strip())
        if "account_regex" in r:
            ok = ok and re.search(r["account_regex"], acct) is not None
        if "vendor_regex" in r:
            ok = ok and re.search(r["vendor_regex"], vend) is not None
        if ok:
            return r["category"], r["subcategory"]
    return "EXPENSE", "Other Expense"

def load_account_gate(path: str) -> dict:
    """The account gate from mapping_accounts.yml (see the `gate:` block there).

    A missing block disables the gate, so an older config keeps the pre-2026-09-17
    behaviour rather than silently dropping every line.
    """
    y = yaml.safe_load(Path(path).read_text(encoding="utf-8")) or {}
    g = y.get("gate") or {}
    return {
        "enabled": bool(g.get("enabled", False)),
        "include_prefixes": tuple(g.get("include_prefixes") or ()),
        "include_exact": frozenset(g.get("include_exact") or ()),
    }


def account_is_owner_side(account_name: str, gate: dict) -> bool:
    """Does this account put the line on an OWNER's statement?

    The class says which property a line belongs to; this says whether the owner bears
    it. Both must agree before a line is charged to anyone. Compare on the FULLY
    QUALIFIED name -- QuickBooks returns the whole path on a line's AccountRef, and the
    tree position is the whole point.
    """
    if not gate.get("enabled"):
        return True
    acct = (account_name or "").strip()
    if not acct:
        return False
    return acct in gate["include_exact"] or acct.startswith(gate["include_prefixes"])
