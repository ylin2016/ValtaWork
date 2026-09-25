"""Regenerate config/properties.csv from Owner_statement_whole's mapping_classes.yml.

mapping_classes.yml is the source of truth for property names and QBO class IDs (the owner
statement pipeline and QBO_operations use the same names), so this file is generated, not edited.

    venv/bin/python -m src.sync_properties            # writes config/properties.csv
    venv/bin/python -m src.sync_properties --check    # exit 1 if the CSV is out of date

Skipped: the parent class `Listings` (not a property), and every property_id listed in
config/properties_exclude.txt (the user's choice of what the form must not offer — edit that file,
not the CSV). Kept with a blank qbo_class_id: entries whose id is `synthetic:…`, which have no QBO
class of their own.
"""

import argparse
import csv
import io
import sys
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT.parent / "Owner_statement_whole" / "config" / "mapping_classes.yml"
TARGET = ROOT / "config" / "properties.csv"
EXCLUDE = ROOT / "config" / "properties_exclude.txt"

FIELDS = ["code", "name", "qbo_class_id", "qbo_class_name", "term", "active"]
SKIP_NAMES = {"Listings"}


def excluded(path: Path = EXCLUDE) -> set[str]:
    if not path.exists():
        return set()
    lines = (ln.split("#", 1)[0].strip() for ln in path.read_text(encoding="utf-8").splitlines())
    return {ln for ln in lines if ln}


def build_rows(source: Path = SOURCE) -> list[dict]:
    skip = excluded()
    out = []
    for r in yaml.safe_load(source.read_text(encoding="utf-8")) or []:
        name = str(r.get("property_name") or "").strip()
        if not name or name in SKIP_NAMES or r["property_id"] in skip:
            continue
        class_id = str(r.get("qbo_class_id") or "").strip()
        if class_id.startswith("synthetic:"):
            class_id = ""
        out.append({
            "code": r["property_id"],
            "name": name,
            "qbo_class_id": class_id,
            "qbo_class_name": r.get("qbo_class_name") or "",
            "term": r.get("term") or "",
            "active": "TRUE",
        })
    unknown = skip - {str(r.get("property_id")) for r in yaml.safe_load(source.read_text(encoding="utf-8")) or []}
    if unknown:
        print(f"WARNING: {EXCLUDE.name} lists ids not in {source.name}: {sorted(unknown)}", file=sys.stderr)
    out.sort(key=lambda row: row["name"].lower())
    for key in ("code", "name"):
        seen = [row[key] for row in out]
        dups = {v for v in seen if seen.count(v) > 1}
        if dups:
            raise SystemExit(f"duplicate {key} in {source.name}: {sorted(dups)}")
    return out


def render(rows: list[dict]) -> str:
    buf = io.StringIO()
    w = csv.DictWriter(buf, fieldnames=FIELDS, lineterminator="\n")
    w.writeheader()
    w.writerows(rows)
    return buf.getvalue()


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--check", action="store_true", help="only report whether the CSV is current")
    args = ap.parse_args()

    text = render(build_rows())
    current = TARGET.read_text(encoding="utf-8") if TARGET.exists() else ""
    if args.check:
        print("up to date" if text == current else f"{TARGET.name} is OUT OF DATE — run without --check")
        sys.exit(0 if text == current else 1)
    TARGET.write_text(text, encoding="utf-8")
    print(f"wrote {TARGET.relative_to(ROOT)}: {text.count(chr(10)) - 1} properties from {SOURCE.name}")


if __name__ == "__main__":
    main()
