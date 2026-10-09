"""Shareable HTML version of the per-listing action report -> output/listing_actions.html
(same data as report_actions.py; published as an Artifact)."""
import hashlib
import json
from pathlib import Path

import sys

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent))
from report_actions import CHANGES  # noqa: E402  (same "what changed" text as the PDF)

ROOT = Path(__file__).resolve().parent.parent
A = pd.read_pickle(ROOT / "data" / "actions.pkl")


def num(x, nd=0):
    if x is None or (isinstance(x, float) and np.isnan(x)):
        return None
    return round(float(x), nd)


rows = []
for r in A.itertuples():
    rows.append({
        "p": r.priority, "name": r.listing, "city": r.city or "", "br": None if pd.isna(r.bedrooms) else int(r.bedrooms),
        "theme": r.theme, "head": r.headline, "acts": list(r.actions),
        # stable per exact action text: a re-run that rewords an action reopens it
        "keys": [hashlib.sha1(f"{r.listing}|{a}".encode()).hexdigest()[:16] for a in r.actions],
        "ly": num(r.ly_adr), "fwd": num(r.fwd_booked_adr), "ask": num(r.ask_median), "comp": num(r.comp_median),
        "min": num(r.min_price), "adj": num(r.adjustment, 2), "open": int(r.nights_avail),
    })
n = A.theme.value_counts()
summary = {
    "counts": A.priority.value_counts().to_dict(),
    "level": int(n.get("Raise the price level", 0)), "min": int(n.get("Raise the minimum", 0)),
    "yac": int(n.get("Yacinde minimum decision", 0)),
    "below": int(A.min_pattern.isin(["Systemic", "Partial"]).sum()),
    "neg": int((A.adjustment == 0.9).sum()), "total": len(A),
    "p1lvl": A[(A.priority == "P1") & (A.theme == "Raise the price level")]
             .sort_values("severe_down", ascending=False).listing.tolist(),
    "changes": CHANGES,
}
themes = ["Raise the price level", "Raise the minimum", "Yacinde minimum decision", "Minimum not enforced",
          "Fix specific nights", "Not in Wheelhouse / closed", "Confirm a setting", "No change"]
theme_why = {
    "Raise the price level": "Under the floor on many nights compared with what it booked last year and what's already booked this season. One Wheelhouse base-adjustment change fixes all of them.",
    "Raise the minimum": "Wheelhouse wants to go lower and the price sits on the minimum most nights. Raise the minimum to the cheapest quarter of what it booked last Oct–Mar.",
    "Yacinde minimum decision": "Minimums of $300–400, but nights post at $180–250. Decide whether the minimum is an owner/HOA requirement (enforce it) or a placeholder (lower it).",
    "Minimum not enforced": "Wheelhouse posts below the listing's own minimum, most likely because of a lower date-range minimum. Either delete that setting or update the default to match.",
    "Fix specific nights": "The level is fine; a handful of nights are clearly off. Fix them by hand.",
    "Not in Wheelhouse / closed": "Outside Wheelhouse or no nights for sale, so nobody is checking these prices.",
    "Confirm a setting": "Usually a leftover −10% adjustment or a new listing priced well below comps.",
    "No change": "Inside the band.",
}
theme_rows = [{"t": t, "n": int(n.get(t, 0)), "why": theme_why[t],
               "names": A[A.theme == t].listing.tolist()} for t in themes if n.get(t, 0)]
data = json.dumps({"rows": rows, "s": summary, "themes": theme_rows}, ensure_ascii=False).replace("</", "<\\/")

HTML = r"""<title>Valta Pricing Actions</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Instrument+Sans:wght@400;500;600;700&family=JetBrains+Mono:wght@400;600&display=swap">
<style>
/* Layout: report column (summary → sequence → root causes) then a filterable listing ledger. */
:root{
  --bg:#f6f7f8; --surface:#ffffff; --ink:#17212b; --muted:#5b6977; --line:#dfe4e9; --accent:#1f5f8b;
  --p1:#c62828; --p1-bg:#fdecea; --p2:#c76a00; --p2-bg:#fff3e0; --p3:#8a6d00; --p3-bg:#fff8d6; --ok:#2e7d32; --ok-bg:#e8f5e9;
  --warn-bg:#fff7e6; --warn-line:#e0a030;
  --f-body:"Instrument Sans",system-ui,-apple-system,"Segoe UI",sans-serif;
  --f-num:"JetBrains Mono",ui-monospace,"SF Mono",Menlo,monospace;
}
@media (prefers-color-scheme: dark){:root:not([data-theme="light"]){
  --bg:#0f151b; --surface:#16202a; --ink:#e4eaf0; --muted:#93a3b3; --line:#283542; --accent:#6db3e3;
  --p1:#ff7b72; --p1-bg:#3a1a1a; --p2:#ffb057; --p2-bg:#3a2810; --p3:#e8cf5a; --p3-bg:#353010; --ok:#6fcf74; --ok-bg:#16301a;
  --warn-bg:#2d2412; --warn-line:#b9852a; color-scheme:dark}}
:root[data-theme="dark"]{
  --bg:#0f151b; --surface:#16202a; --ink:#e4eaf0; --muted:#93a3b3; --line:#283542; --accent:#6db3e3;
  --p1:#ff7b72; --p1-bg:#3a1a1a; --p2:#ffb057; --p2-bg:#3a2810; --p3:#e8cf5a; --p3-bg:#353010; --ok:#6fcf74; --ok-bg:#16301a;
  --warn-bg:#2d2412; --warn-line:#b9852a; color-scheme:dark}
*{box-sizing:border-box}
body{background:var(--bg);color:var(--ink);font:15px/1.55 var(--f-body);margin:0}
.wrap{max-width:980px;margin:0 auto;padding-inline:20px;padding-block:28px 64px;display:grid;gap:28px}
h1,h2,h3{text-wrap:balance;margin:0}
h1{font-size:30px;font-weight:700;letter-spacing:-.01em}
h2{font-size:19px;font-weight:600}
.eyebrow{font-size:12px;letter-spacing:.08em;text-transform:uppercase;color:var(--muted);font-weight:600}
.sub{color:var(--muted);font-size:13.5px;max-width:70ch}
header{display:grid;gap:8px}
.num{font-family:var(--f-num);font-variant-numeric:tabular-nums}
.tiles{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:10px}
.tile{all:unset;cursor:pointer;display:grid;gap:2px;padding:12px 14px;border-radius:8px;border:1px solid var(--line);background:var(--surface)}
.tile:focus-visible{outline:2px solid var(--accent);outline-offset:2px}
.tile b{font:600 26px/1.1 var(--f-num)}
.tile span{font-size:13px;color:var(--muted)}
.tile[data-p="P1"] b{color:var(--p1)} .tile[data-p="P2"] b{color:var(--p2)} .tile[data-p="P3"] b{color:var(--p3)} .tile[data-p="OK"] b{color:var(--ok)}
.tile[aria-pressed="true"]{border-color:currentColor;box-shadow:inset 0 0 0 1px var(--accent)}
section{display:grid;gap:12px}
.drivers{margin:0;padding-left:20px;display:grid;gap:8px;max-width:76ch}
.drivers li::marker{color:var(--muted)}
.callout{background:var(--warn-bg);border:1px solid var(--warn-line);border-radius:8px;padding:12px 16px;max-width:80ch}
.callout strong{display:block;margin-bottom:2px}
ol.steps{margin:0;padding-left:22px;display:grid;gap:8px;max-width:76ch}
ol.steps li::marker{font-family:var(--f-num);color:var(--accent);font-weight:600}
.tablewrap{overflow-x:auto;border:1px solid var(--line);border-radius:8px;background:var(--surface)}
table{border-collapse:collapse;width:100%;min-width:620px;font-size:14px}
th,td{text-align:left;padding:10px 12px;border-bottom:1px solid var(--line);vertical-align:top}
th{font-size:12px;letter-spacing:.06em;text-transform:uppercase;color:var(--muted);font-weight:600}
tr:last-child td{border-bottom:0}
td .names{color:var(--muted);font-size:12.5px;margin-top:2px}
.tbtn{all:unset;cursor:pointer;font-weight:600;color:var(--accent)}
.tbtn:hover{text-decoration:underline}
.tbtn:focus-visible{outline:2px solid var(--accent);outline-offset:2px}
.controls{display:flex;flex-wrap:wrap;gap:8px;align-items:center;position:sticky;top:env(safe-area-inset-top,0px);background:var(--bg);padding-block:10px;z-index:2;border-bottom:1px solid var(--line)}
.controls input,.controls select{font:inherit;font-size:14px;color:var(--ink);background:var(--surface);border:1px solid var(--line);border-radius:6px;padding:7px 10px;min-width:0}
.controls input{flex:1 1 200px}
.controls select{flex:0 1 240px}
.controls button{font:inherit;font-size:13px;color:var(--accent);background:none;border:0;cursor:pointer;padding:6px}
.count{font-size:13px;color:var(--muted);margin-left:auto}
.list{display:grid;gap:10px}
.card{background:var(--surface);border:1px solid var(--line);border-left:4px solid var(--c);border-radius:8px;padding:14px 16px;display:grid;gap:6px;min-width:0}
.card[data-p="P1"]{--c:var(--p1);--cb:var(--p1-bg)} .card[data-p="P2"]{--c:var(--p2);--cb:var(--p2-bg)}
.card[data-p="P3"]{--c:var(--p3);--cb:var(--p3-bg)} .card[data-p="OK"]{--c:var(--ok);--cb:var(--ok-bg)}
.top{display:flex;flex-wrap:wrap;gap:6px 10px;align-items:baseline}
.pill{font:600 11.5px/1 var(--f-num);padding:4px 7px;border-radius:4px;color:var(--c);background:var(--cb)}
.card h3{font-size:17px}
.meta{font-size:12.5px;color:var(--muted)}
.headline{font-weight:600}
.stats{display:flex;flex-wrap:wrap;gap:4px 16px;font-size:12.5px;color:var(--muted)}
.stats span b{font:500 12.5px var(--f-num);color:var(--ink)}
.card ul{margin:2px 0 0;padding-left:18px;display:grid;gap:4px;font-size:14px}
.progress{display:flex;flex-wrap:wrap;gap:6px 14px;align-items:center;font-size:13.5px;color:var(--muted)}
.bar{flex:1 1 220px;height:8px;border-radius:4px;background:var(--line);overflow:hidden;max-width:420px}
.bar i{display:block;height:100%;width:0;background:var(--ok);transition:width .3s}
.card .done-count{margin-left:auto;font:500 12px var(--f-num);color:var(--muted)}
.card.alldone{opacity:.62}
.card ul.acts{list-style:none;padding-left:0}
.act{display:grid;grid-template-columns:auto 1fr;gap:4px 10px;align-items:start}
.act input{margin-top:3px;width:17px;height:17px;accent-color:var(--ok);cursor:pointer}
.act input:disabled{cursor:not-allowed}
.act.is-done .txt{text-decoration:line-through;text-decoration-color:var(--muted);color:var(--muted)}
.act .who{grid-column:2;font-size:12px;color:var(--ok)}
.note{font-size:12.5px;color:var(--muted)}
.empty{color:var(--muted);padding:20px;text-align:center;border:1px dashed var(--line);border-radius:8px}
footer{font-size:12.5px;color:var(--muted);max-width:80ch;display:grid;gap:6px}
@media (max-width:640px){.tiles{grid-template-columns:repeat(2,minmax(0,1fr))} h1{font-size:25px} .count{margin-left:0;flex-basis:100%}}
@media (prefers-reduced-motion:no-preference){.card{transition:border-color .15s}}
</style>

<div class="wrap">
  <header>
    <div class="eyebrow">Revenue · next 180 nights · run Oct 6, 2026 · v2 (comp sets + pacing)</div>
    <h1>Pricing actions by listing</h1>
    <p class="sub">All <span class="num" id="total"></span> active Guesty listings, checked against what each booked last year, what's already booked this season, Wheelhouse comps and Wheelhouse settings. Click a priority to filter the listings below.</p>
  </header>

  <div class="tiles" id="tiles"></div>
  <div class="progress" id="progress" hidden><span><b class="num" id="pdone">0</b> of <span class="num" id="ptotal"></span> actions done</span><span class="bar"><i id="pbar"></i></span><span class="note" id="pnote"></span></div>

  <section aria-labelledby="h-driving">
    <h2 id="h-driving">What's driving it</h2>
    <ul class="drivers" id="drivers"></ul>
    <div class="callout"><strong>Before you act</strong>The "lower price setting" behind below-minimum prices is inferred. The Wheelhouse API only shows each listing's default minimum. Check one listing in Wheelhouse first (Mercer 3925 sits at $100 against a $129 minimum) before deleting anything across the portfolio. Prices may also have moved since the Oct 6 morning data pull. Nothing has been changed in Wheelhouse or Guesty.</div>
  </section>

  <section aria-labelledby="h-changed">
    <h2 id="h-changed">What changed in this version</h2>
    <ul class="drivers" id="changes"></ul>
  </section>

  <section aria-labelledby="h-first">
    <h2 id="h-first">Do these first</h2>
    <ol class="steps" id="steps"></ol>
  </section>

  <section aria-labelledby="h-causes">
    <h2 id="h-causes">By root cause</h2>
    <div class="tablewrap"><table>
      <thead><tr><th style="width:24%">Root cause</th><th style="width:9%">Listings</th><th>Why / fix</th></tr></thead>
      <tbody id="causes"></tbody>
    </table></div>
  </section>

  <section aria-labelledby="h-list" id="listings">
    <h2 id="h-list">Every listing</h2>
    <div class="controls">
      <input id="q" aria-label="Search listings" type="search" placeholder="Search a listing, e.g. Elektra" autocomplete="off">
      <select id="fp" aria-label="Priority"><option value="">All priorities</option><option>P1</option><option>P2</option><option>P3</option><option>OK</option></select>
      <select id="ft" aria-label="Root cause"><option value="">All root causes</option></select>
      <select id="fs" aria-label="Status"><option value="">Any status</option><option value="open">Has open actions</option><option value="done">All done</option></select>
      <button id="clear" type="button">Clear</button>
      <span class="count" id="count"></span>
    </div>
    <div class="list" id="list"></div>
  </section>

  <footer>
    <div><b>How to read a card:</b> LY booked = median nightly rate of last Oct–Mar stays · Booked this season = median of stays already on the books for the next 180 days · Asking = median Guesty price on open nights · Comps = Wheelhouse neighborhood median of comps' asking prices (runs above what units book) · Min / Adj = Wheelhouse minimum price and base adjustment.</div>
    <div>Date fixes show the current price, the band edge to move to, and the severity: blunder ≥30% outside the band, mistake 20–30%, inaccuracy 10–20%. Sources: Guesty confirmed reservations and calendar; Wheelhouse comps, market data and listing settings.</div>
  </footer>
</div>

<script>
const D = __DATA__;
const $ = s => document.querySelector(s);
const esc = s => String(s).replace(/[&<>"]/g, c => ({"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;"}[c]));
const money = v => v == null ? "—" : "$" + Math.round(v).toLocaleString("en-US");
const S = D.s;
$("#total").textContent = S.total;

const PL = {P1:"this week", P2:"this month", P3:"confirm", OK:"no change"};
$("#tiles").innerHTML = ["P1","P2","P3","OK"].map(p =>
  `<button class="tile" data-p="${p}" aria-pressed="false" type="button"><b>${S.counts[p]||0}</b><span>${p} · ${PL[p]}</span></button>`).join("");

$("#drivers").innerHTML = [
  `<b>${S.level} listings are priced 20–40% below what they booked last year</b>, and in most cases stays already booked this season came in even higher. Each gets a specific new Wheelhouse base adjustment (e.g. Redmond 14707 1.00 → 1.35).`,
  `<b>${S.min} listings sit on their minimum price most nights</b>, including the OSBR cottages. Each gets a new minimum set at the cheapest quarter of its stays last Oct–Mar (e.g. Cottage 5 $99 → $125).`,
  `<b>${S.yac} Yacinde units need one decision:</b> is the $300–400 minimum an owner/HOA requirement, or an onboarding placeholder? The answer settles all of them.`,
  `<b>${S.below} listings are being priced below their own minimum.</b> Each card says whether to remove the lower price setting or lower the minimum, based on what it booked last year and what's already booked this season.`,
  `<b>3 listings aren't in Wheelhouse</b> (Beachwood 1, Sammamish 5124-1, the all-cottages bundle), so nobody is checking their prices.`,
  `<b>${S.neg} listings still carry a −10% adjustment.</b> Confirm each was deliberate.`,
  `Each card also lists specific nights to fix by hand. Where a nightly flag contradicts the listing-wide fix, the card says to ignore it.`
].map(x => `<li>${x}</li>`).join("");

$("#changes").innerHTML = S.changes.map(x => `<li>${x}</li>`).join("");
$("#steps").innerHTML = [
  `<b>Audit date-range minimums in Wheelhouse.</b> ${S.below} listings post below their own minimum, most sitting on one round number for weeks. Open Mercer 3925 to find where the lower setting lives, then fix each listing per its card.`,
  `<b>Settle the Yacinde minimum question</b> (10 units). The answer decides all 10 at once.`,
  `<b>Raise the level on the ${S.p1lvl.length} P1 "raise the price level" listings.</b> The biggest gaps: ${S.p1lvl.slice(0, 8).map(esc).join(", ")}.`,
  `<b>Raise the minimums on the "raise the minimum" listings</b>, mostly OSBR cottages. They sit on the minimum most nights.`,
  `<b>Clear the ${S.neg} leftover −10% adjustments</b> unless each was deliberate.`,
  `<b>Re-run this report after the changes</b> to see what moved.`
].map(x => `<li>${x}</li>`).join("");

$("#causes").innerHTML = D.themes.map(t =>
  `<tr><td><button class="tbtn" type="button" data-theme="${esc(t.t)}">${esc(t.t)}</button><div class="names">${t.names.map(esc).join(", ")}</div></td><td class="num">${t.n}</td><td>${esc(t.why)}</td></tr>`).join("");
$("#ft").innerHTML += D.themes.map(t => `<option>${esc(t.t)}</option>`).join("");

function card(r){
  const meta = [r.city, r.br != null ? r.br + "BR" : "", r.theme].filter(Boolean).map(esc).join(" · ");
  return `<article class="card" data-p="${r.p}" data-name="${esc(r.name)}">
    <div class="top"><span class="pill">${r.p}</span><h3>${esc(r.name)}</h3><span class="meta">${meta}</span><span class="done-count" data-count="${r.name}"></span></div>
    <div class="headline">${esc(r.head)}</div>
    <div class="stats">
      <span>LY booked <b>${money(r.ly)}</b></span><span>Booked this season <b>${money(r.fwd)}</b></span>
      <span>Asking <b>${money(r.ask)}</b></span><span>Comps <b>${money(r.comp)}</b></span>
      <span>Min <b>${money(r.min)}</b></span><span>Adj <b>${r.adj == null ? "—" : r.adj.toFixed(2)}</b></span>
      <span>Open nights <b>${r.open}</b></span>
    </div>
    <ul class="acts">${r.acts.map((a, i) => `<li class="act" data-key="${r.keys[i]}">
      <input type="checkbox" id="c-${r.keys[i]}" aria-label="Mark done" ${STATUS.ready ? "" : "disabled"}>
      <label class="txt" for="c-${r.keys[i]}">${esc(a)}</label><span class="who"></span></li>`).join("")}</ul>
  </article>`;
}

// ---- shared status (db capability) -------------------------------------
const STATUS = {ready: false, map: {}, db: null, user: null, writable: true};
const doneOf = k => !!(STATUS.map[k] && STATUS.map[k].done);
const allDone = r => r.keys.every(doneOf);
const fmtDate = iso => { try { return new Date(iso).toLocaleDateString("en-US", {month: "short", day: "numeric"}); } catch (e) { return ""; } };

async function applyStatus(){
  const ids = [...new Set(Object.values(STATUS.map).filter(v => v.done && v.by).map(v => v.by))];
  const ps = STATUS.user && ids.length ? await STATUS.user.profiles(ids) : {};
  document.querySelectorAll("li.act").forEach(li => {
    const k = li.dataset.key, v = STATUS.map[k], cb = li.querySelector("input"), who = li.querySelector(".who");
    const d = !!(v && v.done);
    if (cb !== document.activeElement || cb.checked !== d) cb.checked = d;
    cb.disabled = !STATUS.ready || !STATUS.writable;
    li.classList.toggle("is-done", d);
    who.textContent = d ? `Done${v.by ? " by " + ((ps[v.by] && ps[v.by].name) || "someone") : ""}${v.at ? " · " + fmtDate(v.at) : ""}` : "";
  });
  for (const r of D.rows) {
    const n = r.keys.filter(doneOf).length, el = document.querySelector(`[data-count="${CSS.escape(r.name)}"]`);
    if (el) { el.textContent = STATUS.ready ? `${n}/${r.keys.length} done` : ""; el.closest(".card").classList.toggle("alldone", n === r.keys.length); }
  }
  const total = D.rows.reduce((a, r) => a + r.keys.length, 0), done = D.rows.reduce((a, r) => a + r.keys.filter(doneOf).length, 0);
  $("#ptotal").textContent = total; $("#pdone").textContent = done; $("#pbar").style.width = (100 * done / total) + "%";
}

$("#list").addEventListener("change", async e => {
  const cb = e.target.closest("li.act input"); if (!cb || !STATUS.db) return;
  const li = cb.closest("li.act"), k = li.dataset.key, card = li.closest(".card");
  const r = D.rows.find(x => x.name === card.dataset.name), i = r.keys.indexOf(k);
  const body = {done: cb.checked, by: (await STATUS.user?.id()) || null, at: new Date().toISOString(),
                listing: r.name, priority: r.p, action: r.acts[i].slice(0, 300)};
  cb.disabled = true;
  try { await STATUS.db.collection("status").doc(k).set(body); }
  catch (err) {
    cb.checked = !cb.checked;
    if (err && err.code === "invalid_argument") { STATUS.writable = false; $("#pnote").textContent = "You have view-only access, so you can see progress but not tick items."; }
    else if (err && err.code === "quota_exceeded") { $("#pnote").textContent = "The tracker is full. Ask the owner to clear old items."; }
    else { $("#pnote").textContent = "Couldn't save that change. Try again in a moment."; }
  }
  finally { applyStatus(); }
});

(async () => {
  const [db, user] = await Promise.all([claude.use("db"), claude.use("user")]);
  if (!db) { $("#progress").hidden = false; $("#pnote").textContent = "Sign in to claude.ai to see and update shared progress."; return; }
  STATUS.db = db; STATUS.user = user;
  const w = user ? await user.can("data.write") : null;
  if (w === false) { STATUS.writable = false; }
  $("#progress").hidden = false;
  $("#pnote").textContent = STATUS.writable ? "Tick an action when it's done. Everyone with access sees it live." : "You have view-only access, so you can see progress but not tick items.";
  db.collection("status").onSnapshot(snap => {
    const m = {}; snap.docs.forEach(d => { m[d.id] = d.data(); });
    STATUS.map = m; STATUS.ready = true; applyStatus();
    if ($("#fs").value) render();
  }, () => { $("#pnote").textContent = "Live progress stopped updating. Reload the page to reconnect."; });
})();

function render(){
  const q = $("#q").value.trim().toLowerCase(), fp = $("#fp").value, ft = $("#ft").value, fs = $("#fs").value;
  const rows = D.rows.filter(r => (!fp || r.p === fp) && (!ft || r.theme === ft) &&
    (!q || (r.name + " " + r.city).toLowerCase().includes(q)) &&
    (!fs || (fs === "done") === allDone(r)));
  $("#list").innerHTML = rows.length ? rows.map(card).join("") :
    `<div class="empty">No listings match. Clear the filters to see all ${D.rows.length}.</div>`;
  $("#count").textContent = `${rows.length} of ${D.rows.length} listings`;
  applyStatus();
  document.querySelectorAll(".tile").forEach(t => t.setAttribute("aria-pressed", String(t.dataset.p === fp)));
  try { localStorage.setItem("pa-filters", JSON.stringify({q, fp, ft, fs})); } catch (e) {}
}
function jump(){ $("#listings").scrollIntoView({behavior: matchMedia("(prefers-reduced-motion: reduce)").matches ? "auto" : "smooth"}); }
$("#tiles").addEventListener("click", e => { const t = e.target.closest(".tile"); if (!t) return;
  $("#fp").value = $("#fp").value === t.dataset.p ? "" : t.dataset.p; render(); jump(); });
$("#causes").addEventListener("click", e => { const b = e.target.closest(".tbtn"); if (!b) return;
  $("#ft").value = b.dataset.theme; $("#fp").value = ""; render(); jump(); });
["q","fp","ft","fs"].forEach(id => $("#" + id).addEventListener("input", render));
$("#clear").addEventListener("click", () => { $("#q").value = ""; $("#fp").value = ""; $("#ft").value = ""; $("#fs").value = ""; render(); });
try { const f = JSON.parse(localStorage.getItem("pa-filters") || "{}"); $("#q").value = f.q || ""; $("#fp").value = f.fp || ""; $("#ft").value = f.ft || ""; $("#fs").value = f.fs || ""; } catch (e) {}
render();
</script>
"""

out = ROOT / "output" / "listing_actions.html"
out.write_text(HTML.replace("__DATA__", data), encoding="utf-8")
print(out, out.stat().st_size)
