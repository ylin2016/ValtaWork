"""Build the training table from the historical dispatch cards.

One row per stop: day, car, crew, order, property, kind (turnover / residential),
bedrooms, bathrooms, residential time estimate, back-to-back flag, coordinates.

Sources (all local, read-only):
  data/Maria Cleaning Dispatch Card.xlsx  Sheet1   — the real cars (May 2025 →)
  data/Listings.xlsx                      STR      — turnover listings (beds/baths)
                                          Residential — residential clients
  Claude_projects/shared/data/guesty.sqlite        — listing coordinates + check-ins

No value is guessed. Coordinates come only from Guesty (or the shared building for
Elektra / Beachwood units) or from Lat / Lng columns you fill in Listings.xlsx; a stop
without them is reported as missing. A dispatch name must match a listing exactly
(after the spelling fixes in ALIASES) or be listed on the Aliases tab of Listings.xlsx;
anything else is reported as unmatched.
"""
import re
import sqlite3
from pathlib import Path

import numpy as np
import pandas as pd

PROJ = Path(__file__).resolve().parent.parent
DATA = PROJ / 'data'
GUESTY_DB = PROJ.parent / 'shared' / 'data' / 'guesty.sqlite'
BUILDINGS = ('elektra', 'beachwood')            # unit names that share one building address


# Spelling fixes for names typed on the dispatch card. Only add an entry the user has confirmed.
ALIASES = {
    'mercer island': 'mercer',     # "Mercer Island 3627" = "Mercer 3627"
    'kirland': 'kirkland',
    'issquah': 'issaquah',
    'millcreek': 'mill creek',
}


def key(s):
    """Normalized property name: case, spaces and '#' only ('Beachwood #2' → 'beachwood 2'),
    plus the confirmed spelling fixes in ALIASES."""
    s = re.sub(r'\s+', ' ', str(s).replace('#', ' ')).strip().lower().rstrip(',').strip()
    for wrong, right in ALIASES.items():
        s = re.sub(rf'^{wrong}\b', right, s)
    return s


# ---------------------------------------------------------------- dispatch cards
def _parse_additional(text):
    """'3. Bellevue 1108; 5. Seattle 109' / '2, Elektra 1314' / 'A, B' → [(order|None, name)]."""
    toks = [t.strip() for t in re.split(r'[;,\n]', str(text)) if t.strip()]
    out, pend = [], None
    for t in toks:
        if re.fullmatch(r'\d+\.?', t):
            pend = int(t.rstrip('.'))
            continue
        m = re.match(r'(\d+)\s*[.)]\s*(.+)', t)
        out.append((int(m[1]), m[2]) if m else (pend, t))
        pend = None
    return out


def dispatch_stops():
    raw = pd.read_excel(DATA / 'Maria Cleaning Dispatch Card.xlsx', 'Sheet1')
    rows = []
    for _, r in raw.dropna(subset=['Cleaning Date']).iterrows():
        day = pd.to_datetime(r['Cleaning Date']).normalize()
        for car in range(1, 5):
            cc = [c for c in raw.columns if re.match(rf'Car ?{car}\b', c)]
            if not cc:
                continue
            crew_col = [c for c in cc if 'Cleaners' in c][0]
            crew = [p.strip() for p in str(r[crew_col]).split(',') if p.strip() and p.strip() != 'nan']
            stops = [(i, r[c]) for i, c in enumerate([c for c in cc if 'Property' in c], 1) if pd.notna(r[c])]
            add = [c for c in cc if 'Additional' in c]
            if add and pd.notna(r[add[0]]):
                stops += _parse_additional(r[add[0]])
            for order, name in stops:
                name = str(name).strip()
                if name.lower() in ('specify other:', 'other specify:') or not name:
                    continue
                rows.append(dict(day=day, car=car, crew=crew, crew_n=len(crew), order=order, name=name, k=key(name)))
    df = pd.DataFrame(rows)
    df['order'] = df.order.fillna(99)
    return df


# ---------------------------------------------------------------- properties
def properties():
    """key → dict(kind, beds, baths, est_min, address, lat, lng, coord_src)."""
    con = sqlite3.connect(f'file:{GUESTY_DB}?mode=ro', uri=True)
    g = pd.read_sql('select nickname, address_full, city, zipcode, lat, lng from listings', con)
    con.close()
    g['k'] = g.nickname.map(key)
    gco = {r.k: (r.lat, r.lng) for r in g.itertuples() if pd.notna(r.lat)}
    elektra = gco[key('Elektra 1004')]
    beach = gco[key('Beachwood 1')]

    def coord(k, row):
        """Lat/Lng typed in Listings.xlsx win; else Guesty by exact name; else the building."""
        la, ln = getattr(row, 'Lat', None), getattr(row, 'Lng', None)
        if pd.notna(la) and pd.notna(ln): return (float(la), float(ln)), 'sheet'
        if k in gco: return gco[k], 'guesty'
        if k.startswith('elektra'): return elektra, 'building'
        if k.startswith('beachwood'): return beach, 'building'
        return (None, None), None

    props = {}
    s = pd.read_excel(DATA / 'Listings.xlsx', 'STR')
    for r in s.itertuples():
        k = key(r.Listing)
        (la, ln), src = coord(k, r)
        props[k] = dict(kind='turnover', beds=float(r.BEDROOMS or 0), baths=float(r.BATHROOMS or 0), est_min=np.nan,
                        address=r.Address, lat=la, lng=ln, coord_src=src)
    res = pd.read_excel(DATA / 'Listings.xlsx', 'Residential')
    for r in res.itertuples():
        k = key(r.Listing)
        if k in props:
            continue
        (la, ln), src = coord(k, r)
        moveout = str(getattr(r, 'Service', '')).lower().startswith(('move', 'pre-listing'))
        props[k] = dict(kind='moveinout' if moveout else 'residential',
                        beds=float(r.Bedrooms) if pd.notna(r.Bedrooms) else np.nan,
                        baths=float(r.Bathrooms) if pd.notna(r.Bathrooms) else np.nan,
                        est_min=float(r.EstMin_2ppl) if pd.notna(r.EstMin_2ppl) else np.nan,
                        address=r.Address, lat=la, lng=ln, coord_src=src)
    return props


def checkins():
    """(listing, day) check-ins, the listings Guesty has reservations for, and the first day covered."""
    con = sqlite3.connect(f'file:{GUESTY_DB}?mode=ro', uri=True)
    r = pd.read_sql("select listing_nickname n, substr(check_in,1,10) d from reservations where status='confirmed'", con)
    con.close()
    r['k'] = r.n.map(key)
    return set(zip(r.k, r.d)), set(r.k), pd.Timestamp(r.d.min())


def name_aliases():
    """Dispatch name → listing, from the Aliases tab of Listings.xlsx (filled in by hand)."""
    try:
        a = pd.read_excel(DATA / 'Listings.xlsx', 'Aliases')
    except ValueError:
        return {}
    return {key(r[0]): key(r[1]) for r in a.itertuples(index=False) if pd.notna(r[0]) and pd.notna(r[1])}


def resolve(k, props, aliases=None):
    """Exact match, or the listing given for this name on the Aliases tab. Nothing else —
    'Seattle 906' is only 'Seattle 906 Lower' because the Aliases tab says so."""
    if k in props: return k
    a = (aliases or {}).get(k)
    return a if a in props else None


def build():
    """The stop table plus a coverage report."""
    st = dispatch_stops()
    props = properties()
    ci, covered, first = checkins()
    aliases = name_aliases()
    st['pk'] = st.k.map(lambda k: resolve(k, props, aliases))
    info = st.pk.map(lambda p: props.get(p) if p else None)
    for col in ('kind', 'beds', 'baths', 'est_min', 'lat', 'lng', 'coord_src', 'address'):
        st[col] = info.map(lambda d: d[col] if d else None)
    # Back-to-back = a Guesty check-in at that exact listing that day. Unknown (None) when the
    # day is before the Guesty data starts or the listing isn't in it (delisted, or not in Guesty).
    st['b2b'] = pd.array([(k, d.strftime('%Y-%m-%d')) in ci if (d >= first and p in covered) else None
                          for k, p, d in zip(st.k, st.pk, st.day)], dtype='boolean')
    st['building'] = st.k.map(lambda k: next((b for b in BUILDINGS if k.startswith(b)), None))
    st['loc_key'] = [b or (str(a).split(',')[0].strip().lower() if a else k)
                     for b, a, k in zip(st.building, st.address, st.k)]
    return st, props


if __name__ == '__main__':
    st, props = build()
    print(f'{len(st)} stops, {st.day.nunique()} days, {st.day.min().date()} → {st.day.max().date()}')
    print('resolved to a property:', f'{st.pk.notna().mean():.1%}', '| with coordinates:', f'{st.lat.notna().mean():.1%}')
    print('kind:', st.kind.value_counts(dropna=False).to_dict())
    print('coord source:', st.coord_src.value_counts(dropna=False).to_dict())
    t = st[st.kind == 'turnover']
    print('back-to-back known for', f'{t.b2b.notna().mean():.0%}', 'of turnovers; share where known:', f'{t.b2b.mean():.1%}')
    print('unresolved:', st[st.pk.isna()].name.value_counts().head(20).to_dict())
