"""Manual mode: a Claude Code session (run inside VS Code, which can read Google Drive)
does the itemizing instead of the API. No API key, no per-call cost.

    python -m src.manual stage [--limit 10]   # pick pending receipts, make readable links
    (Claude Code reads each link, writes output/extracted/<hash>.json)
    python -m src.manual check                # validate JSONs, clear finished links
    python -m src.build                       # report

`stage` never copies receipts: it makes symlinks named <hash>.pdf / .jpg / .png in
output/staging/ pointing at the Drive files, because Claude Code picks the reader from
the file extension and most receipts are saved without one. HEIC photos are the exception:
they are converted to a small JPEG there, since Claude Code can't read HEIC.
"""
from __future__ import annotations

import argparse
import json
import shutil
import subprocess
import sys
from pathlib import Path

from .extract import (build_schema, build_system, file_hash, find_files,
                      load_categories, sniff)
from .paths import DEFAULT_MATCH, DEFAULT_SRC, EXTRACTED, OUTPUT

STAGING = OUTPUT / "staging"
MANIFEST = STAGING / "manifest.json"
INSTRUCTIONS = STAGING / "INSTRUCTIONS.md"
EXT = {"application/pdf": ".pdf", "image/jpeg": ".jpg", "image/png": ".png",
       "image/gif": ".gif", "image/webp": ".webp"}


def validate(value, schema: dict, path: str = "$") -> list[str]:
    """Just enough JSON Schema for build_schema(): object/array/string/number/boolean + enum."""
    t = schema.get("type")
    if t == "object":
        if not isinstance(value, dict):
            return [f"{path}: expected object"]
        errs = [f"{path}: missing '{k}'" for k in schema["required"] if k not in value]
        errs += [f"{path}: unexpected '{k}'" for k in value if k not in schema["properties"]]
        for k, sub in schema["properties"].items():
            if k in value:
                errs += validate(value[k], sub, f"{path}.{k}")
        return errs
    if t == "array":
        if not isinstance(value, list):
            return [f"{path}: expected array"]
        return [e for i, v in enumerate(value) for e in validate(v, schema["items"], f"{path}[{i}]")]
    ok = {"string": lambda v: isinstance(v, str),
          "number": lambda v: isinstance(v, (int, float)) and not isinstance(v, bool),
          "boolean": lambda v: isinstance(v, bool)}[t](value)
    if not ok:
        return [f"{path}: expected {t}, got {value!r}"]
    if "enum" in schema and value not in schema["enum"]:
        return [f"{path}: {value!r} not in {schema['enum']}"]
    return []


def write_instructions(categories: dict) -> None:
    example = {
        "source_file": "<from manifest>", "hash": "<from manifest>", "model": "claude-code",
        "extracted_at": "<YYYY-MM-DDTHH:MM:SS>",
        "result": {"receipts": [{
            "vendor": "Home Depot", "date": "2026-07-28", "document_type": "receipt",
            "receipt_number": "", "payment_method": "Visa 9967", "cottage": "5",
            "subtotal": 280.0, "tax": 24.78, "shipping": 0, "other_fees": 0,
            "order_discount": 0, "total": 304.78, "total_legible": True,
            "items": [{"description": "HDX 33-gal trash bags, 40 ct", "qty": 2,
                       "unit_price": 19.97, "amount": 39.94, "category": "Supplies",
                       "cottage": "", "confidence": "high", "notes": ""}],
            "notes": ""}], "not_a_receipt_reason": ""},
    }
    INSTRUCTIONS.write_text(
        "# How to itemize the staged receipts\n\n"
        "For each entry in manifest.json: Read its `staged` file (a PDF or image), then Write\n"
        "`output/extracted/<hash>.json` in exactly the shape below (every key required, no extras).\n"
        "Use the `source_file` path as the file path mentioned in the rules. When done with the batch,\n"
        "run `python -m src.manual check` and fix anything it reports.\n\n"
        "## Rules\n\n" + build_system(categories) + "\n\n## JSON shape\n\n```json\n"
        + json.dumps(example, indent=2) + "\n```\n\n"
        "Allowed `category` values: " + ", ".join(categories) + ".\n"
        "`document_type`: receipt | invoice | order_confirmation | refund | other. "
        "`confidence`: high | medium | low.\n")


def stage(args) -> None:
    try:
        matched, unsupported = find_files(args.src, args.match or DEFAULT_MATCH)
    except PermissionError:
        sys.exit(f"Can't read {args.src}. Run from Claude Code inside VS Code (it has Drive access).")
    EXTRACTED.mkdir(parents=True, exist_ok=True)
    if STAGING.exists():
        shutil.rmtree(STAGING)
    STAGING.mkdir(parents=True)

    # The same receipt is often filed under more than one card folder; it hashes
    # identically, so keep the first path and remember the rest as duplicates.
    pending, dupes, seen = [], {}, set()
    for p in matched:
        h = file_hash(p)
        if (EXTRACTED / f"{h}.json").exists():
            continue
        if h in seen:
            dupes.setdefault(h, []).append(p)
            continue
        seen.add(h)
        pending.append((p, h))
    batch = pending[:args.limit] if args.limit else pending
    manifest = []
    for p, h in batch:
        media = sniff(p)
        if media == "image/heic":
            staged = STAGING / f"{h}.jpg"
            subprocess.run(["sips", "-s", "format", "jpeg", "-s", "formatOptions", "80",
                            "-Z", "2000", str(p), "--out", str(staged)],
                           check=True, capture_output=True)
        else:
            staged = STAGING / f"{h}{EXT[media]}"
            staged.symlink_to(p)
        manifest.append({"hash": h, "source_file": str(p.relative_to(args.src)),
                         "staged": str(staged),
                         "duplicate_of": [str(d.relative_to(args.src)) for d in dupes.get(h, [])]})
    MANIFEST.write_text(json.dumps(manifest, indent=2))
    write_instructions(load_categories())

    n_dupes = sum(len(v) for v in dupes.values())
    print(f"{len(matched)} receipt files, {len(matched) - len(pending) - n_dupes} done, "
          f"{n_dupes} duplicate, {len(pending)} pending; staged {len(batch)} in {STAGING}")
    for p in unsupported:
        print(f"  skip (not PDF/image): {p.relative_to(args.src)}")
    for m in manifest:
        print(f"  {m['hash']}  {m['source_file']}")
        for d in m["duplicate_of"]:
            print(f"      duplicate: {d}")
    if batch:
        print(f"\nNext: itemize per {INSTRUCTIONS}, then: python -m src.manual check")


def check(_args) -> None:
    schema = build_schema(list(load_categories()))
    bad = 0
    for f in sorted(EXTRACTED.glob("*.json")):
        try:
            rec = json.loads(f.read_text())
        except json.JSONDecodeError as e:
            print(f"{f.name}: invalid JSON: {e}")
            bad += 1
            continue
        errs = [f"missing '{k}'" for k in ("source_file", "hash", "result") if k not in rec]
        if "result" in rec:
            errs += validate(rec["result"], schema)
        if rec.get("hash") and f.stem != rec["hash"]:
            errs.append(f"file name {f.stem} != hash {rec['hash']}")
        if errs:
            bad += 1
            print(f"{f.name} ({rec.get('source_file', '?')}):")
            for e in errs[:20]:
                print(f"   {e}")

    left = []
    if MANIFEST.exists():
        for m in json.loads(MANIFEST.read_text()):
            if (EXTRACTED / f"{m['hash']}.json").exists():
                Path(m["staged"]).unlink(missing_ok=True)
            else:
                left.append(m)
        MANIFEST.write_text(json.dumps(left, indent=2))
    n = len(list(EXTRACTED.glob("*.json")))
    print(f"{n} extractions, {bad} with errors; {len(left)} staged receipts still to do")
    for m in left:
        print(f"  todo: {m['hash']}  {m['source_file']}")
    if not bad:
        print("Next: python -m src.build")


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    s = sub.add_parser("stage", help="link the next batch of pending receipts")
    s.add_argument("--src", type=Path, default=DEFAULT_SRC)
    s.add_argument("--match", action="append")
    s.add_argument("--limit", type=int, default=10, help="batch size (0 = all pending)")
    s.set_defaults(func=stage)
    c = sub.add_parser("check", help="validate extracted JSON, clear finished links")
    c.set_defaults(func=check)
    args = ap.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
