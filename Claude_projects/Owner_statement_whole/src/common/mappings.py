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
