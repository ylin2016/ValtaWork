"""Comp-set maps for the dashboard's Owner tab.

For each Wheelhouse comp set: one street-map image (OpenStreetMap tiles, cached
under data/tiles/) framing the set's comp listings and our listings in it, plus
the projection the page needs to place pins on it. The artifact can't load map
tiles itself (its CSP blocks images from other hosts), so the image is published
next to the page as maps/s<set index>.jpg.

Comp listings come from the shared wheelhouse.sqlite dynamic_set_members (newest
snapshot on or before the run); our listings' coordinates from the shared
guesty.sqlite `listings` table (refreshed by fetch_guesty.py).
"""
import io
import json
import math
import sqlite3
import time

import pandas as pd
import requests
from PIL import Image

from guesty_store import listing_locations
from paths import DATA_DIR, OUTPUT_DIR, WHEELHOUSE_DB

TILE_DIR = DATA_DIR / "tiles"
MAP_DIR = OUTPUT_DIR / "maps"          # published next to output/revenue_dashboard.html
TILE_URL = "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
USER_AGENT = "ValtaRevenueDashboard/1.0 (weekly comp-set maps; tiles cached)"
W, H, PAD, TILE, MAX_Z = 760, 480, 40, 256, 15


def _px(lat, lng, z):
    """Web Mercator world pixel at zoom z."""
    n = TILE * 2 ** z
    s = math.sin(math.radians(lat))
    return (lng + 180) / 360 * n, (0.5 - math.log((1 + s) / (1 - s)) / (4 * math.pi)) * n


_session = None


def _tile(z, x, y):
    f = TILE_DIR / str(z) / str(x) / f"{y}.png"
    if not f.exists():
        global _session
        if _session is None:
            _session = requests.Session()
            _session.headers["User-Agent"] = USER_AGENT
        r = _session.get(TILE_URL.format(z=z, x=x, y=y), timeout=30)
        r.raise_for_status()
        f.parent.mkdir(parents=True, exist_ok=True)
        f.write_bytes(r.content)
        time.sleep(0.1)               # be gentle with the OSM tile servers
    return Image.open(f).convert("RGB")


def _render(lats, lngs):
    """Pick the largest zoom where the points fit, stitch tiles, return (image, z, x0, y0)."""
    for z in range(MAX_Z, 2, -1):
        xs, ys = zip(*(_px(a, b, z) for a, b in zip(lats, lngs)))
        if max(xs) - min(xs) <= W - 2 * PAD and max(ys) - min(ys) <= H - 2 * PAD:
            break
    x0 = round((max(xs) + min(xs)) / 2 - W / 2)
    y0 = round((max(ys) + min(ys)) / 2 - H / 2)
    img = Image.new("RGB", (W, H), (242, 239, 233))
    for tx in range(x0 // TILE, (x0 + W) // TILE + 1):
        for ty in range(max(0, y0 // TILE), min(2 ** z - 1, (y0 + H) // TILE) + 1):
            img.paste(_tile(z, tx % 2 ** z, ty), (tx * TILE - x0, ty * TILE - y0))
    return img, z, x0, y0


def _members(run):
    con = sqlite3.connect(WHEELHOUSE_DB)
    snap = con.execute("SELECT max(snapshot_date) FROM dynamic_set_members WHERE snapshot_date <= ?",
                       (run,)).fetchone()[0]
    rows = con.execute("""
        SELECT trim(d.name), m.status, m.raw_json FROM dynamic_set_members m
        JOIN dynamic_sets d ON d.set_id = m.set_id AND d.snapshot_date = m.snapshot_date
        WHERE m.snapshot_date = ? AND m.status IN ('active', 'review')""", (snap,)).fetchall()
    if not rows:   # sets listed on a different snapshot than the members
        rows = con.execute("""
            SELECT trim(d.name), m.status, m.raw_json FROM dynamic_set_members m
            JOIN dynamic_sets d ON d.set_id = m.set_id
            WHERE m.snapshot_date = ? AND m.status IN ('active', 'review')
            GROUP BY m.set_id, m.member_listing_id, m.status""", (snap,)).fetchall()
    con.close()
    return snap, rows


def build_maps(run, set_names, listing_sets):
    """set_names: dashboard set list (index = set id on the page).
    listing_sets: {listing name: [set index, ...]}.
    Returns ({set index: map spec}, {listing name: [lat, lng]}, members snapshot)."""
    loc = listing_locations()
    snap, rows = _members(run)
    sidx = {n: i for i, n in enumerate(set_names)}
    comps = {}
    for name, status, raw in rows:
        if name not in sidx or not raw:
            continue
        m = json.loads(raw)
        if m.get("lat") is None or m.get("long") is None:
            continue
        r = lambda k, nd: None if m.get(k) is None else round(float(m[k]), nd)
        comps.setdefault(sidx[name], []).append([
            round(m["lat"], 5), round(m["long"], 5), 0 if status == "active" else 1,
            m.get("title") or "", m.get("url") or "", m.get("bedrooms"),
            r("adr_365_0", 0), r("occupancy_365_0", 3), r("occupancy_adjusted_365_0", 3),
            r("star_rating", 2), m.get("review_count")])
    ours = {}
    for n, sl in listing_sets.items():
        for i in sl:
            if n in loc:
                ours.setdefault(i, []).append(loc[n])
    MAP_DIR.mkdir(parents=True, exist_ok=True)
    maps = {}
    for i, cs in sorted(comps.items()):
        lats = pd.Series([c[0] for c in cs])
        lngs = pd.Series([c[1] for c in cs])
        # frame the bulk of the set (5-95th percentile) plus our own listings, so
        # one far-off comp doesn't zoom the map out to the whole state
        keep = (lats.between(*lats.quantile([.05, .95])) & lngs.between(*lngs.quantile([.05, .95]))) \
            if len(cs) >= 12 else pd.Series(True, index=lats.index)
        fl = list(lats[keep]) + [p[0] for p in ours.get(i, [])]
        fg = list(lngs[keep]) + [p[1] for p in ours.get(i, [])]
        img, z, x0, y0 = _render(fl, fg)
        buf = io.BytesIO()
        img.save(buf, "JPEG", quality=80, optimize=True, progressive=True)
        (MAP_DIR / f"s{i}.jpg").write_bytes(buf.getvalue())
        maps[i] = {"img": f"maps/s{i}.jpg", "z": z, "x0": x0, "y0": y0, "w": W, "h": H, "m": cs}
    print(f"  comp maps: {len(maps)} sets, {sum(len(c) for c in comps.values())} comp listings "
          f"(members snapshot {snap}), {len(loc)} Guesty locations -> {MAP_DIR}")
    return maps, loc, snap
