"""Find OSBR receipts on the Drive mount and itemize each one with Claude.

Run from a terminal that can read Google Drive (VS Code), not from Claude's shell:

    python -m src.extract --list          # free: show which files match
    python -m src.extract --limit 3       # try a few first
    python -m src.extract                 # everything not yet extracted

Each source file's result is cached in output/extracted/<hash>.json, so re-runs
only send new or changed files. Use --force to re-extract.
"""
from __future__ import annotations

import argparse
import base64
import hashlib
import json
import os
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime
from pathlib import Path

import yaml

from .paths import CATEGORIES_YML, DEFAULT_MATCH, DEFAULT_SRC, EXTRACTED, ROOT

MODEL = "claude-opus-5"
MAX_IMAGE_BYTES = 3_500_000  # API cap is 5 MB after base64; stay well under
API_MEDIA = {"application/pdf", "image/jpeg", "image/png", "image/gif", "image/webp"}


# ---------- file discovery ----------

def sniff(p: Path) -> str | None:
    """Media type from the file's first bytes. Names can't be trusted: many receipts are
    saved without an extension (e.g. '..._Laundry supplies_32.10' -> suffix '.10')."""
    try:
        with open(p, "rb") as f:
            head = f.read(32)
    except OSError:
        return None
    if head.startswith(b"%PDF") or b"%PDF" in head:
        return "application/pdf"
    if head.startswith(b"\xff\xd8\xff"):
        return "image/jpeg"
    if head.startswith(b"\x89PNG"):
        return "image/png"
    if head.startswith(b"GIF8"):
        return "image/gif"
    if head.startswith(b"RIFF") and head[8:12] == b"WEBP":
        return "image/webp"
    if head[4:8] == b"ftyp" and head[8:12] in {b"heic", b"heix", b"mif1", b"msf1", b"heis", b"hevc"}:
        return "image/heic"
    return None


def find_files(src: Path, patterns: list[str]) -> tuple[list[Path], list[Path]]:
    """Files whose path (relative to src — folder or file name) contains any pattern."""
    pats = [p.lower() for p in patterns]
    matched, unsupported = [], []
    for p in sorted(src.rglob("*")):
        if not p.is_file() or p.name.startswith("."):
            continue
        rel = str(p.relative_to(src)).lower()
        if not any(x in rel for x in pats):
            continue
        (matched if sniff(p) else unsupported).append(p)
    return matched, unsupported


def file_hash(p: Path) -> str:
    h = hashlib.sha256()
    with open(p, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()[:16]


# ---------- request building ----------

def load_categories() -> dict[str, str]:
    return yaml.safe_load(CATEGORIES_YML.read_text())["categories"]


def build_schema(categories: list[str]) -> dict:
    money = {"type": "number"}
    item = {
        "type": "object",
        "properties": {
            "description": {"type": "string"},
            "qty": {"type": "number"},
            "unit_price": money,
            "amount": money,
            "category": {"type": "string", "enum": categories},
            "cottage": {"type": "string"},
            "confidence": {"type": "string", "enum": ["high", "medium", "low"]},
            "notes": {"type": "string"},
        },
        "required": ["description", "qty", "unit_price", "amount", "category",
                     "cottage", "confidence", "notes"],
        "additionalProperties": False,
    }
    receipt = {
        "type": "object",
        "properties": {
            "vendor": {"type": "string"},
            "date": {"type": "string"},
            "document_type": {"type": "string", "enum": [
                "receipt", "invoice", "order_confirmation", "refund", "other"]},
            "receipt_number": {"type": "string"},
            "payment_method": {"type": "string"},
            "cottage": {"type": "string"},
            "subtotal": money,
            "tax": money,
            "shipping": money,
            "other_fees": money,
            "order_discount": money,
            "total": money,
            "total_legible": {"type": "boolean"},
            "items": {"type": "array", "items": item},
            "notes": {"type": "string"},
        },
        "required": ["vendor", "date", "document_type", "receipt_number",
                     "payment_method", "cottage", "subtotal", "tax", "shipping", "other_fees",
                     "order_discount", "total", "total_legible", "items", "notes"],
        "additionalProperties": False,
    }
    return {
        "type": "object",
        "properties": {
            "receipts": {"type": "array", "items": receipt},
            "not_a_receipt_reason": {"type": "string"},
        },
        "required": ["receipts", "not_a_receipt_reason"],
        "additionalProperties": False,
    }


def build_system(categories: dict[str, str]) -> str:
    cat_lines = "\n".join(f"- {k}: {v}" for k, v in categories.items())
    return f"""You itemize purchase receipts for Ocean Spray Beach Resort (OSBR), a vacation \
resort in Grayland, WA managed by Valta Realty, with 12 cottages (Cottage 1-12, also written OSBR 1-12). The goal is to see where the money went, by category and by cottage.

For each receipt in the document, list every purchased line item and put it in exactly one category:
{cat_lines}

Rules:
- One file can hold several receipts (e.g. a multi-page PDF); return each separately. If the file \
is not a receipt or invoice at all, return no receipts and explain in not_a_receipt_reason.
- amount = the line's extended price after any discount printed on that line (qty x unit price \
minus item discount). Fold item-level discounts, coupons, and deposit/return lines into the item \
they belong to; order-wide discounts go in order_discount as a positive number.
- Gift cards, store credit, rewards points and split payments are HOW it was paid, not \
discounts: total = the full order amount (before gift card), and mention the split in notes.
- tax, shipping, other_fees (delivery, bag fees, service/environmental fees, tips) are receipt-level, \
never items. Use 0 when absent.
- total = the grand total actually charged. total_legible = false if you cannot read it with \
certainty; never invent a number.
- Refunds/returns: use negative amounts and the original item's category; document_type = refund.
- date in YYYY-MM-DD ("" if unreadable). payment_method like "Visa 1234" or "Cash" ("" if absent).
- Expand cryptic POS abbreviations into a readable description when you can (e.g. "HDX 33G \
TRSH BG 40CT" -> "HDX 33-gal trash bags, 40 ct"). Keep the SKU in notes if it helps.
- confidence reflects the category choice and legibility together: low when the item text is \
cryptic or partly unreadable, or the category is a real toss-up. Explain every low or medium in notes.
- Category Other always needs a note saying what it is.
- cottage (receipt level): the cottage number "1".."12" the purchase is for, taken from the document (handwritten notes, job/PO/memo fields, ship-to) or else from the folder/file path you are given (e.g. "OSBR 5", "Cottage 5", "OSBR5", "C5"). "Common" if clearly for shared areas or the whole resort (grounds, office, laundry, pool). "" if nothing indicates it.
- A path naming several cottages ("Cottage 1_5" = cottages 1 and 5) is a shared purchase: set \
the receipt cottage to "" and give each item its cottage when the document says which is which.
- cottage (item level): only when a line is for a different cottage than the receipt (e.g. a note splitting items across cottages); otherwise "".
- Staging vs Decor (both stay in the cottage): Staging if the item does a job (you'd miss it \
functionally — a lamp, rug, curtain, mirror, bath mat); Decor if it is only there to look good."""


def content_block(p: Path, tmpdir: Path) -> dict:
    media = sniff(p)
    if media == "application/pdf":
        data = base64.standard_b64encode(p.read_bytes()).decode()
        return {"type": "document",
                "source": {"type": "base64", "media_type": "application/pdf", "data": data}}
    path = p
    if media not in API_MEDIA or p.stat().st_size > MAX_IMAGE_BYTES:
        # macOS sips: convert HEIC and shrink large phone photos to JPEG
        path = tmpdir / (p.stem + ".jpg")
        subprocess.run(["sips", "-s", "format", "jpeg", "-s", "formatOptions", "85",
                        "-Z", "2400", str(p), "--out", str(path)],
                       check=True, capture_output=True)
        media = "image/jpeg"
    data = base64.standard_b64encode(path.read_bytes()).decode()
    return {"type": "image", "source": {"type": "base64", "media_type": media, "data": data}}


# ---------- API ----------

def load_api_key() -> None:
    """Fall back to a project .env (gitignored) when ANTHROPIC_API_KEY isn't exported."""
    if os.environ.get("ANTHROPIC_API_KEY"):
        return
    env = ROOT / ".env"
    if env.exists():
        for line in env.read_text().splitlines():
            k, _, v = line.partition("=")
            if k.strip() == "ANTHROPIC_API_KEY" and v.strip():
                os.environ["ANTHROPIC_API_KEY"] = v.strip().strip('"').strip("'")


def extract_one(client, p: Path, src: Path, h: str, system: str, schema: dict) -> dict:
    import anthropic

    rec = {"source_file": str(p.relative_to(src)), "source_abs": str(p), "hash": h,
           "model": MODEL, "extracted_at": datetime.now().isoformat(timespec="seconds")}
    with tempfile.TemporaryDirectory() as td:
        block = content_block(p, Path(td))
        try:
            resp = client.beta.messages.create(
                model=MODEL,
                max_tokens=16000,
                betas=["server-side-fallback-2026-07-01"],
                thinking={"type": "adaptive"},
                system=system,
                output_config={"format": {"type": "json_schema", "schema": schema}},
                fallbacks="default",
                messages=[{"role": "user", "content": [
                    block,
                    {"type": "text", "text": f"File path: {rec['source_file']}\nItemize this document."},
                ]}],
            )
        except (anthropic.BadRequestError, anthropic.AuthenticationError,
                anthropic.PermissionDeniedError, anthropic.NotFoundError) as e:
            rec["error"] = f"{type(e).__name__}: {e.message}"
            return rec
        except anthropic.APIStatusError as e:  # 429/5xx already retried by the SDK
            rec["error"] = f"API {e.status_code}: {e.message}"
            return rec
        except anthropic.APIConnectionError as e:
            rec["error"] = f"connection: {e}"
            return rec

    rec["usage"] = {"input_tokens": resp.usage.input_tokens,
                    "output_tokens": resp.usage.output_tokens}
    if resp.stop_reason == "refusal":
        rec["error"] = f"refusal: {getattr(resp.stop_details, 'category', None)}"
        return rec
    if resp.stop_reason == "max_tokens":
        rec["error"] = "output truncated (max_tokens) — receipt too long, split the file"
        return rec
    text = "".join(b.text for b in resp.content if b.type == "text")
    try:
        rec["result"] = json.loads(text)
    except json.JSONDecodeError as e:
        rec["error"] = f"bad JSON: {e}"
    return rec


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--src", type=Path, default=DEFAULT_SRC, help="folder to search (recursive)")
    ap.add_argument("--match", action="append", help=f"path substring, repeatable (default {DEFAULT_MATCH})")
    ap.add_argument("--list", action="store_true", help="only list matching files; no API calls")
    ap.add_argument("--limit", type=int, help="extract at most N new files")
    ap.add_argument("--force", action="store_true", help="re-extract files already cached")
    ap.add_argument("--workers", type=int, default=4)
    args = ap.parse_args()
    patterns = args.match or DEFAULT_MATCH

    try:
        matched, unsupported = find_files(args.src, patterns)
    except PermissionError:
        sys.exit(f"Can't read {args.src}\nRun this from VS Code's terminal (it has Google Drive access).")

    EXTRACTED.mkdir(parents=True, exist_ok=True)
    todo, cached = [], 0
    for p in matched:
        h = file_hash(p)
        if not args.force and (EXTRACTED / f"{h}.json").exists():
            cached += 1
            continue
        todo.append((p, h))

    print(f"Search: {args.src}\nMatch:  {patterns}")
    print(f"{len(matched)} receipt files ({cached} already extracted, {len(todo)} new); "
          f"{len(unsupported)} skipped (not a PDF or image by content)")
    for p in unsupported:
        with open(p, "rb") as f:
            head = f.read(8)
        print(f"  skip: {p.relative_to(args.src)}  [starts {head!r}]")
    if args.list:
        for p in matched:
            print(f"  {p.relative_to(args.src)}")
        return
    if args.limit is not None:
        todo = todo[:args.limit]
    if not todo:
        print("Nothing to extract. Next: python -m src.build")
        return

    load_api_key()
    if not os.environ.get("ANTHROPIC_API_KEY"):
        sys.exit("Set ANTHROPIC_API_KEY (export it, or put ANTHROPIC_API_KEY=... in osbr_expense/.env)")
    import anthropic
    client = anthropic.Anthropic(max_retries=5)

    categories = load_categories()
    system, schema = build_system(categories), build_schema(list(categories))

    tin = tout = errors = 0
    with ThreadPoolExecutor(max_workers=args.workers) as pool:
        futs = {pool.submit(extract_one, client, p, args.src, h, system, schema): p for p, h in todo}
        for i, fut in enumerate(as_completed(futs), 1):
            p = futs[fut]
            try:
                rec = fut.result()
            except Exception as e:  # e.g. sips failure on a corrupt image
                print(f"[{i}/{len(todo)}] ERROR {p.name}: {e}")
                errors += 1
                continue
            u = rec.get("usage", {})
            tin += u.get("input_tokens", 0)
            tout += u.get("output_tokens", 0)
            if "error" in rec:
                errors += 1
                print(f"[{i}/{len(todo)}] ERROR {p.name}: {rec['error']}")
                continue  # not cached, so the next run retries it
            (EXTRACTED / f"{rec['hash']}.json").write_text(json.dumps(rec, indent=2))
            rs = rec["result"]["receipts"]
            desc = ", ".join(f"{r['vendor']} {r['date']} ${r['total']:.2f} ({len(r['items'])} items)" for r in rs) \
                or f"not a receipt: {rec['result']['not_a_receipt_reason']}"
            print(f"[{i}/{len(todo)}] {p.name}: {desc}")

    cost = tin / 1e6 * 5 + tout / 1e6 * 25  # claude-opus-5 list price
    print(f"\nDone. {len(todo) - errors} extracted, {errors} errors. "
          f"Tokens in/out: {tin:,}/{tout:,} (~${cost:.2f}).\nNext: python -m src.build")


if __name__ == "__main__":
    main()
