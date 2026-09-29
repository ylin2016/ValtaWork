"""Compare Guesty listing descriptions with the local Word copies in each property folder.

    python -m src.compare_descriptions                  # pull Guesty fresh, active listings
    python -m src.compare_descriptions --cached         # reuse data/desc_listings.json
    python -m src.compare_descriptions --all            # include inactive Guesty listings

Local copies are the "* Listing Description*.docx" files anywhere under
PROPERTIES_ROOT (closed properties excluded). Each docx is split on the
Guesty-level headings (Summary, The Space, ...) into the same fields as
Guesty's `publicDescription`; sub-headings such as "Outdoor Space" or
"Bedrooms and Bathrooms" stay inside their parent section, exactly as Guesty
stores them.

A listing is matched to docs by its nickname (number + unit qualifier, e.g.
"Bellevue 2323 ADU", "Cottage 4" -> OSBR4, "Beachwood 7" -> Unit 7). When
several docs qualify (an STR and a "CH LTR" copy, say) the one closest to the
Guesty text is compared and the rest are listed as alternates.

Writes Output/description_diff_<date>.xlsx and .html. Read-only on Guesty.
"""
from __future__ import annotations

import argparse
import datetime as dt
import difflib
import html
import json
import re
import unicodedata
from pathlib import Path

import docx
from docx.oxml.ns import qn
import pandas as pd

from .config import load_config, resolve

DRIVE = Path("/Users/ylin/Google Drive/My Drive")
PROPERTIES_ROOT = DRIVE / "** Properties ** -- Valta"
EXCLUDE_DIRS = re.compile(r"^([0-4]_|Y_Closed)")
DESC_FILE = re.compile(r"descri", re.IGNORECASE)

FIELDS = ["title", "summary", "space", "neighborhood", "access",
          "transit", "interactionWithGuests", "notes", "houseRules"]
FIELD_LABELS = {
    "title": "Title", "summary": "Summary", "space": "The Space",
    "neighborhood": "The Neighborhood", "access": "Guest Access",
    "transit": "Getting Around", "interactionWithGuests": "Interaction with Guests",
    "notes": "Other Things to Note", "houseRules": "House Rules",
}

# Only these headings start a new Guesty field; any other heading is kept as
# text inside the current section (Guesty stores sub-headings that way too).
HEADINGS = {
    "summary": "summary", "the summary": "summary", "summary description": "summary",
    "the space": "space",
    "the neighborhood": "neighborhood", "neighborhood": "neighborhood",
    "the neighbordhood": "neighborhood",
    "guest access": "access",
    "getting around": "transit",
    "interaction with guests": "interactionWithGuests",
    "other things to note": "notes", "other things to notes": "notes",
    "house rules": "houseRules",
}
# Older docs name a section differently; these count only while the doc has no
# section of that field yet (otherwise they are a sub-heading, e.g. "Things to
# do" under "The Neighborhood").
SOFT_HEADINGS = {"things to do": "neighborhood", "location": "neighborhood"}
TITLE_RE = re.compile(r"^\s*(listing name|listing title|title)\s*[:：]?\s*(.*)$", re.IGNORECASE)
LTR_RE = re.compile(r"\b(ch|chr)\s*l[rt]{2}\b|long[\s-]*term|pricing description", re.IGNORECASE)
QUALIFIERS = {"upper", "lower", "main", "adu", "whole", "middle", "top"}


# ------------------------------------------------------------------ guesty


def fetch_guesty(cached: bool) -> list[dict]:
    path = resolve("./data/desc_listings.json")
    if cached and path.exists():
        return json.loads(path.read_text())
    from .guesty_client import GuestyClient
    from .pull_listings import fetch_all_listings

    load_config()
    listings = fetch_all_listings(
        GuestyClient(), fields="_id,nickname,title,publicDescription,active,isListed")
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(listings, indent=2, default=str))
    return listings


def guesty_fields(listing: dict) -> dict[str, str]:
    pd_ = listing.get("publicDescription") or {}
    out = {f: (pd_.get(f) or "") for f in FIELDS if f != "title"}
    out["title"] = listing.get("title") or ""
    return out


# ------------------------------------------------------------------- local


def find_docs() -> list[Path]:
    docs = []
    for top in sorted(PROPERTIES_ROOT.iterdir()):
        if not top.is_dir() or EXCLUDE_DIRS.match(top.name):
            continue
        for p in top.rglob("*.docx"):
            if DESC_FILE.search(p.name) and not p.name.startswith("~$"):
                docs.append(p)
    return docs


def _heading_key(text: str) -> str:
    return re.sub(r"\s+", " ", re.sub(r"[^a-z ]", " ", text.lower())).strip()


def _paragraph_texts(body):
    """Every paragraph's text in document order. Walks the XML directly because
    python-docx's .paragraphs skips content controls (w:sdt), which is where
    Google-Docs-exported checklists — the ✔ lines — live."""
    for p in body.iter(qn("w:p")):
        parts = []
        for el in p.iter(qn("w:t"), qn("w:tab"), qn("w:br")):
            if el.tag == qn("w:t"):
                parts.append(el.text or "")
            else:
                parts.append(" " if el.tag == qn("w:tab") else "\n")
        text = "".join(parts).strip()
        if text:
            yield text


def parse_doc(path: Path) -> dict[str, str]:
    """docx -> {field: text}. Text before the first known heading is dropped
    (LTR pricing / application terms live there)."""
    body = docx.Document(str(path)).element.body
    sections: dict[str, list[str]] = {}
    pre: list[str] = []  # text after the title, before any heading
    current = None
    want_title = False
    for text in _paragraph_texts(body):
        if not text:
            continue
        m = TITLE_RE.match(text)
        if m and "title" not in sections:
            if m.group(2).strip():
                sections["title"] = [m.group(2).strip()]
            else:
                want_title = True
            continue
        hk = _heading_key(text)
        key = HEADINGS.get(hk)
        if not key and len(text) < 45:
            soft = next((v for k, v in SOFT_HEADINGS.items() if hk.startswith(k)), None)
            if soft and soft not in sections:
                key = soft
        if want_title:
            want_title = False
            if not key:
                sections["title"] = [text]
                continue
        if key and len(text) < 40:
            current = key
            sections.setdefault(current, [])
            continue
        if current:
            sections[current].append(text)
        elif "title" in sections:
            pre.append(text)
    # some docs put the summary straight under the title, with no heading
    if "summary" not in sections and pre and set(sections) - {"title"}:
        sections["summary"] = pre
    return {f: "\n".join(v) for f, v in sections.items()}


# ---------------------------------------------------------------- matching


def _norm_path(s: str) -> str:
    return re.sub(r"\s+", " ", re.sub(r"[^a-z0-9]+", " ", s.lower())).strip()


def _nick_patterns(nick: str) -> tuple[list[re.Pattern], set[str]]:
    """Regexes the doc path must ALL match, plus the nickname's unit qualifiers."""
    n = _norm_path(nick)
    words = n.split()
    quals = {w for w in words if w in QUALIFIERS}
    m = re.match(r"cottage (\d+)", n)
    if m:
        k = m.group(1)
        return [re.compile(rf"\b(osbr ?{k}|cottage {k})\b")], quals
    m = re.match(r"beachwood (\d+)$", n)
    if m:
        k = m.group(1)
        return [re.compile(r"beachwood"), re.compile(rf"\b(beachwood {k}|unit {k})\b")], quals
    m = re.match(r"yacinde (\w+)$", n)
    if m:
        return [re.compile(rf"\byacinde {m.group(1)}\b")], quals
    m = re.match(r"sammamish 5124 (\d)$", n)
    if m:
        return [re.compile(rf"\b5124 {m.group(1)}\b")], quals
    nums = [w for w in words if w.isdigit() and len(w) >= 3]
    if not nums:
        return [], quals
    # "Keaau 15-1542" -> 1542; "Microsoft 14615-D303" -> 14615
    num = max(nums, key=len)
    return [re.compile(rf"\b{num}\b")], quals


def candidates(nick: str, docs: list[Path], all_quals_for: dict[str, set[str]]) -> list[Path]:
    pats, quals = _nick_patterns(nick)
    if not pats:
        return []
    out = []
    for d in docs:
        rel = _norm_path(str(d.relative_to(PROPERTIES_ROOT)))
        # the unit lives below the property folder (the folder name holds the address)
        tail = _norm_path(" ".join(d.relative_to(PROPERTIES_ROOT).parts[1:]))
        if not all(p.search(rel) for p in pats):
            continue
        doc_quals = {w for w in tail.split() if w in QUALIFIERS}
        if quals and not (quals <= doc_quals or ("whole" in quals and "shared" in tail.split())):
            continue
        # an unqualified listing must not take a sibling unit's doc
        siblings = all_quals_for.get(pats[0].pattern, set()) - quals
        if not quals and doc_quals & siblings:
            continue
        out.append(d)
    return out


# ---------------------------------------------------------------- compare


_TRANS = str.maketrans({"‘": "'", "’": "'", "“": '"', "”": '"', "–": "-", "—": "-",
                        " ": " ", "​": ""})


def norm(s: str) -> str:
    return re.sub(r"\s+", " ", unicodedata.normalize("NFKC", s or "").translate(_TRANS)).strip()


def loose(s: str) -> str:
    """Letters and digits only — differences beyond this are cosmetic."""
    return re.sub(r"[^a-z0-9]+", "", norm(s).lower())


def similarity(a: str, b: str) -> float:
    a, b = norm(a).split(), norm(b).split()
    if not a and not b:
        return 1.0
    return difflib.SequenceMatcher(None, a, b, autojunk=False).ratio()


def compare_field(local: str, guesty: str) -> tuple[str, float]:
    if not norm(local) and not norm(guesty):
        return "both empty", 1.0
    if not norm(local):
        return "missing locally", 0.0
    if not norm(guesty):
        return "missing on Guesty", 0.0
    if norm(local) == norm(guesty):
        return "identical", 1.0
    if loose(local) == loose(guesty):
        return "cosmetic only", 1.0
    return "different", similarity(local, guesty)


def change_kind(local: str, guesty: str) -> str:
    """What a 'different' field's edits are: pure additions on one side, or rewording."""
    a, b = norm(local).split(), norm(guesty).split()
    ops = {op for op, *_ in difflib.SequenceMatcher(None, a, b, autojunk=False).get_opcodes()} - {"equal"}
    if ops == {"insert"}:
        return "Guesty adds text"
    if ops == {"delete"}:
        return "local has extra text"
    return "edited"


def word_diff(local: str, guesty: str, fmt: str) -> str:
    a, b = norm(local).split(), norm(guesty).split()
    out = []
    for op, i1, i2, j1, j2 in difflib.SequenceMatcher(None, a, b, autojunk=False).get_opcodes():
        la, gb = " ".join(a[i1:i2]), " ".join(b[j1:j2])
        if fmt == "html":
            la, gb = html.escape(la), html.escape(gb)
        if op == "equal":
            out.append(la if fmt == "html" else _ellipsize(la))
            continue
        if la:
            out.append(f"<del>{la}</del>" if fmt == "html" else f"[LOCAL: {la}]")
        if gb:
            out.append(f"<ins>{gb}</ins>" if fmt == "html" else f"{{GUESTY: {gb}}}")
    return " ".join(out)


def _ellipsize(s: str, keep: int = 8) -> str:
    w = s.split()
    return s if len(w) <= 2 * keep else " ".join(w[:keep] + ["…"] + w[-keep:])


# ------------------------------------------------------------------ report


def build(listings: list[dict], include_inactive: bool):
    docs = find_docs()
    parsed = {}
    for d in docs:
        try:
            parsed[d] = parse_doc(d)
        except Exception as e:  # corrupt / not really docx
            print(f"  ! cannot read {d.name}: {e}")
    docs = list(parsed)

    listings = [l for l in listings if include_inactive or l.get("active")]
    # qualifiers used by sibling listings of the same number, for unqualified matching
    quals_by_pat: dict[str, set[str]] = {}
    for l in listings:
        pats, q = _nick_patterns(l.get("nickname", ""))
        if pats:
            quals_by_pat.setdefault(pats[0].pattern, set()).update(q)

    summary, fields_rows, used = [], [], set()
    for l in sorted(listings, key=lambda x: x.get("nickname", "")):
        nick = l.get("nickname", "")
        g = guesty_fields(l)
        cands = candidates(nick, docs, quals_by_pat)
        g_all = "\n".join(g[f] for f in FIELDS)
        best = max(cands, key=lambda d: similarity("\n".join(parsed[d].get(f, "") for f in FIELDS), g_all),
                   default=None)
        row = {"nickname": nick, "active": l.get("active"), "listed": l.get("isListed"),
               "local_file": "", "local_type": "", "alternates": "", "status": "",
               "fields_different": 0, "fields_missing": 0, "overall_similarity": None,
               "guesty_id": l.get("_id")}
        if best is None:
            row["status"] = "NO LOCAL DOC"
            summary.append(row)
            continue
        used.update(cands)
        loc = parsed[best]
        row["local_file"] = str(best.relative_to(PROPERTIES_ROOT))
        row["local_type"] = "LTR" if LTR_RE.search(best.name) else "STR"
        row["alternates"] = "; ".join(d.name for d in cands if d != best)
        l_all = "\n".join(loc.get(f, "") for f in FIELDS)
        row["overall_similarity"] = round(similarity(l_all, g_all), 3)
        if len(norm("\n".join(v for k, v in loc.items() if k != "title")).split()) < 30:
            row["status"] = "LOCAL DOC EMPTY / NO SECTIONS"
            summary.append(row)
            continue
        for f in FIELDS:
            st, sim = compare_field(loc.get(f, ""), g[f])
            if st == "both empty":
                continue
            # the Word docs never carry house rules; report it, don't count it
            if f == "houseRules" and "houseRules" not in loc:
                st = "not kept locally"
            if st == "different":
                row["fields_different"] += 1
            elif st.startswith("missing"):
                row["fields_missing"] += 1
            fields_rows.append({
                "nickname": nick, "field": FIELD_LABELS[f], "status": st,
                "change": change_kind(loc.get(f, ""), g[f]) if st == "different" else "",
                "similarity": round(sim, 3),
                "diff": word_diff(loc.get(f, ""), g[f], "text") if st == "different" else "",
                "diff_html": word_diff(loc.get(f, ""), g[f], "html") if st == "different" else "",
                "local_text": loc.get(f, ""), "guesty_text": g[f],
            })
        n_bad = row["fields_different"] + row["fields_missing"]
        row["status"] = "MATCH" if n_bad == 0 else f"{n_bad} field(s) differ"
        summary.append(row)

    orphans = [{"local_file": str(d.relative_to(PROPERTIES_ROOT)),
                "type": "LTR" if LTR_RE.search(d.name) else "STR"}
               for d in docs if d not in used]
    return pd.DataFrame(summary), pd.DataFrame(fields_rows), pd.DataFrame(orphans)


def write_xlsx(path: Path, summary, fields, orphans):
    with pd.ExcelWriter(path, engine="openpyxl") as xw:
        summary.to_excel(xw, sheet_name="Summary", index=False)
        diffs = fields[~fields.status.isin(["identical", "not kept locally"])].drop(columns=["diff_html"])
        diffs.to_excel(xw, sheet_name="Differences", index=False)
        fields.drop(columns=["diff_html"]).to_excel(xw, sheet_name="All fields", index=False)
        orphans.to_excel(xw, sheet_name="Unmatched local docs", index=False)
        for ws in xw.book.worksheets:
            ws.freeze_panes = "B2"
            for col in ws.columns:
                width = max(len(str(c.value or "")) for c in col[:50])
                ws.column_dimensions[col[0].column_letter].width = min(max(10, width + 2), 60)


CSS = """
:root{--bg:#fff;--fg:#1d1d1f;--mut:#6e6e73;--line:#e3e3e8;--del:#fde2e1;--delfg:#a1150d;--ins:#dcf5e3;--insfg:#0b6b2b;--card:#f7f7f9}
@media (prefers-color-scheme:dark){:root{--bg:#161618;--fg:#ececf0;--mut:#9a9aa3;--line:#2e2e33;--del:#4a1c1a;--delfg:#ffb4ae;--ins:#163a22;--insfg:#9be3b0;--card:#1f1f23}}
body{background:var(--bg);color:var(--fg);font:14px/1.5 -apple-system,system-ui,sans-serif;margin:0;padding:24px 16px;max-width:1100px;margin:auto}
h1{font-size:22px;margin:0 0 4px}h2{font-size:17px;margin:32px 0 6px;border-top:1px solid var(--line);padding-top:18px}
.mut{color:var(--mut);font-size:12.5px}table{border-collapse:collapse;width:100%;font-size:13px}
td,th{border-bottom:1px solid var(--line);padding:5px 8px;text-align:left;vertical-align:top}
th{font-weight:600;color:var(--mut)}a{color:inherit}
del{background:var(--del);color:var(--delfg);text-decoration:line-through}ins{background:var(--ins);color:var(--insfg);text-decoration:none}
.f{background:var(--card);border-radius:8px;padding:10px 12px;margin:8px 0}.f b{display:block;margin-bottom:4px}
.ok{color:var(--insfg)}.bad{color:var(--delfg)}.wrap{overflow-x:auto}
"""


def write_html(path: Path, summary, fields, orphans, stamp: str):
    esc = html.escape
    rows = []
    for _, r in summary.iterrows():
        cls = "ok" if r.status == "MATCH" else "bad"
        link = f'<a href="#{esc(r.nickname)}">{esc(r.nickname)}</a>' if r.local_file and r.status != "MATCH" else esc(r.nickname)
        sim = "" if pd.isna(r.overall_similarity) else f"{r.overall_similarity:.0%}"
        rows.append(f"<tr><td>{link}</td><td class={cls}>{esc(r.status)}</td><td>{sim}</td>"
                    f"<td>{esc(r.local_type)}</td><td class=mut>{esc(r.local_file)}</td></tr>")
    parts = [f"<!doctype html><meta charset=utf-8><meta name=viewport content='width=device-width,initial-scale=1'>"
             f"<title>Guesty Description Diff</title><style>{CSS}</style>",
             f"<h1>Guesty vs local listing descriptions</h1>"
             f"<p class=mut>Generated {stamp}. <del>struck red</del> = only in the local doc; "
             f"<ins>green</ins> = only on Guesty. Whitespace, curly quotes and dashes are ignored.</p>",
             "<div class=wrap><table><tr><th>Listing</th><th>Status</th><th>Similarity</th><th>Doc</th><th>Local file</th></tr>",
             *rows, "</table></div>"]
    shown = fields[~fields.status.isin(["identical", "not kept locally", "cosmetic only"])]
    for nick, grp in shown.groupby("nickname", sort=True):
        r = summary[summary.nickname == nick].iloc[0]
        parts.append(f"<h2 id='{esc(nick)}'>{esc(nick)}</h2><div class=mut>{esc(r.local_file)}"
                     + (f"<br>alternates: {esc(r.alternates)}" if r.alternates else "") + "</div>")
        for _, f in grp.iterrows():
            if f.status == "different":
                body = f.diff_html
            elif f.status == "cosmetic only":
                body = "<span class=mut>Same words; only punctuation, symbols or case differ.</span>"
            elif f.status == "missing on Guesty":
                body = f"<del>{esc(norm(f.local_text))}</del>"
            else:
                body = f"<ins>{esc(norm(f.guesty_text))}</ins>"
            parts.append(f"<div class=f><b>{esc(f.field)} — {esc(f.change or f.status)}"
                         + (f" ({f.similarity:.0%} similar)" if f.status == "different" else "")
                         + f"</b>{body}</div>")
    if len(orphans):
        parts.append("<h2>Local docs not matched to an active Guesty listing</h2><ul>"
                     + "".join(f"<li class=mut>{esc(o.local_file)}</li>" for _, o in orphans.iterrows())
                     + "</ul>")
    path.write_text("\n".join(parts))


def main():
    ap = argparse.ArgumentParser(description="Diff Guesty listing descriptions against local docx copies.")
    ap.add_argument("--cached", action="store_true", help="Reuse data/desc_listings.json instead of pulling.")
    ap.add_argument("--all", action="store_true", help="Include inactive Guesty listings.")
    ap.add_argument("--out", default="./Output/description_diff", help="Output path stem.")
    args = ap.parse_args()

    listings = fetch_guesty(args.cached)
    summary, fields, orphans = build(listings, args.all)

    stamp = dt.date.today().isoformat()
    stem = resolve(args.out)
    stem = stem.with_name(f"{stem.name}_{stamp.replace('-', '')}")
    stem.parent.mkdir(parents=True, exist_ok=True)
    write_xlsx(stem.with_suffix(".xlsx"), summary, fields, orphans)
    write_html(stem.with_suffix(".html"), summary, fields, orphans, stamp)

    print(summary.status.map(lambda s: "DIFFERS" if "differ" in s else s).value_counts().to_string())
    print(f"Wrote {stem.with_suffix('.xlsx')}\nWrote {stem.with_suffix('.html')}")


if __name__ == "__main__":
    main()
