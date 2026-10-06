"""Pricing-error scan: band every unbooked night in the next 180 days, flag the
nights priced outside it, and rank them by severity.

Inputs: data/raw/*.csv from pull_guesty.py + pull_wheelhouse.py.
Band for listing L on date d:
  FLOOR   = what L booked at on the matching LY night (fare split across the
            stay's nights by the market's daily-ADR profile), lowered only when
            the market has genuinely dropped (forward market ADR < 90% of LY).
  CEILING = FLOOR x 1.30, lifted only when the market is clearly stronger
            (forward market ADR > 110% of LY).
Evidence ladder when there's no step-1 match: 2 comparable dates -> 3 portfolio
siblings same date -> 3b the listing's Wheelhouse comp set, same date LY (scaled
by how the listing books relative to its set) -> 4 Wheelhouse comp band -> 5 skip.
Market direction (v2): the listing's comp-set ADR vs LY -- forward booked ADR
where the set has >=15% on the books, else the set's trailing 60-day YoY -- plus
neighborhood pacing (booked vs typical at this lead time).
Every portfolio listing is banded (needed for sibling evidence and the
corroboration rule); output is limited to TARGETS.
"""
from datetime import date, timedelta
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
RAW = ROOT / "data" / "raw"
TODAY = date(2026, 10, 6)
HORIZON = TODAY + timedelta(days=179)

TARGETS = {
    "63fcea0b6f86080038100d1c": "Elektra 1115",
    "6a373ba1dcac1a00138bf739": "Yacinde E3",
    "6a280b10529d4b001113fc3e": "Shoreline 15510",
    "63fcec6ae600670037727ffe": "Longbranch 6821",
}
CEIL_MULT = 1.30          # top of the user's 10-30% headroom
MKT_DROP, MKT_STRONG = 0.90, 1.10
DISTRESS_LEAD, DISTRESS_SHARE = 3, 0.60
MIDTERM_NIGHTS = 28
PACE_STRONG, PACE_WEAK = 1.25, 0.75   # neighborhood booked / typical-at-this-lead-time
MAX_LIFT = 1.35
STEP_3B = 3.5
SEV = [(0.30, "BLUNDER"), (0.20, "MISTAKE"), (0.10, "INACCURACY")]
RANK = {"BLUNDER": 3, "MISTAKE": 2, "INACCURACY": 1, None: 0}


# ---------------------------------------------------------------- calendars
def D(s):
    return date.fromisoformat(s)


def holiday_nights() -> set[date]:
    h = set()
    for y in (2025, 2026, 2027):
        h |= {date(y, 10, 31), date(y, 12, 23), date(y, 12, 24), date(y, 12, 25), date(y, 12, 26),
              date(y, 12, 30), date(y, 12, 31), date(y, 1, 1), date(y, 2, 13), date(y, 2, 14)}
    h |= {D("2025-11-26"), D("2025-11-27"), D("2025-11-28"), D("2025-11-29"),   # Thanksgiving Wed-Sat
          D("2026-11-25"), D("2026-11-26"), D("2026-11-27"), D("2026-11-28"),
          D("2026-01-17"), D("2026-01-18"), D("2027-01-16"), D("2027-01-17"),   # MLK Sat-Sun
          D("2026-02-14"), D("2026-02-15"), D("2027-02-13"), D("2027-02-14"),   # Presidents Sat-Sun
          D("2026-04-03"), D("2026-04-04"), D("2027-03-26"), D("2027-03-27")}   # Easter Fri-Sat
    return h


HOL = holiday_nights()
FIXED_MD = {(10, 31), (12, 23), (12, 24), (12, 25), (12, 26), (12, 30), (12, 31), (1, 1), (2, 13), (2, 14)}
EASTER_MAP = {D("2027-03-26"): D("2026-04-03"), D("2027-03-27"): D("2026-04-04")}


def ly_date(d: date) -> tuple[date, str]:
    if d in EASTER_MAP:
        return EASTER_MAP[d], "Easter-aligned"
    if (d.month, d.day) in FIXED_MD:
        return d.replace(year=d.year - 1), "calendar date (fixed holiday)"
    return d - timedelta(days=364), "same weekday"


# ---------------------------------------------------------------- load
def load():
    gl = pd.read_csv(RAW / "guesty_listings.csv")
    res = pd.read_csv(RAW / "guesty_reservations.csv")
    cal = pd.read_csv(RAW / "guesty_calendar.csv")
    wl = pd.read_csv(RAW / "wh_listings.csv")
    mk = pd.read_csv(RAW / "wh_market_daily.csv", dtype={"bedrooms": str})
    nb = pd.read_csv(RAW / "wh_neighborhood_pricing.csv")
    return gl, res, cal, wl, mk, nb


def bed_bucket(b):
    if pd.isna(b):
        return "all"
    b = int(b)
    return "4+" if b >= 4 else str(b)


class Market:
    """Daily market ADR/occupancy per (market, bedroom bucket), 'all' fallback."""

    def __init__(self, mk):
        mk = mk.copy()
        mk["d"] = pd.to_datetime(mk.stay_date).dt.date
        self.adr, self.occ = {}, {}
        for (m, b), g in mk.groupby(["market_id", "bedrooms"]):
            s = g.set_index("d")
            self.adr[(str(m), b)] = s.adr_w_fees.replace(0, np.nan)
            self.occ[(str(m), b)] = s.occupancy_adjusted

    def series(self, m, b, kind="adr"):
        tbl = self.adr if kind == "adr" else self.occ
        s = tbl.get((str(m), b))
        if s is None or s.notna().sum() < 100:
            s = tbl.get((str(m), "all"))
        return s

    def adr_on(self, m, b, d):
        s = self.series(m, b)
        return None if s is None else s.get(d)

    def window_mean(self, s, d, k=7):
        idx = [d + timedelta(days=i) for i in range(-k, k + 1)]
        v = s.reindex(idx).dropna()
        return v.mean() if len(v) else np.nan

    def ratio(self, m, b, d, lyd):
        """Forward/LY market ADR around the date; None when forward data is too thin."""
        a, o = self.series(m, b), self.series(m, b, "occ")
        if a is None:
            return None
        if self.window_mean(o, d) < 0.15:          # too few forward bookings to read ADR
            return None
        fw, ly = self.window_mean(a, d), self.window_mean(a, lyd)
        return None if np.isnan(fw) or np.isnan(ly) or ly == 0 else fw / ly

    def events(self, m):
        """LY nights whose market ADR spikes >=25% over the same-weekday local median."""
        s = self.adr.get((str(m), "all"))
        out = set()
        if s is None:
            return out
        for d, v in s.dropna().items():
            if not (D("2025-09-01") <= d <= D("2026-04-30")) or d in HOL:
                continue
            nbr = s.reindex([d + timedelta(days=7 * k) for k in (-4, -3, -2, -1, 1, 2, 3, 4)]).dropna()
            if len(nbr) >= 4 and v > 1.25 * nbr.median():
                out.add(d)
        return out


class CompSet:
    """Daily ADR/occupancy per Wheelhouse dynamic (comp) set."""

    def __init__(self, sd):
        sd = sd.copy()
        sd["d"] = pd.to_datetime(sd.stay_date).dt.date
        self.adr, self.occ, self.trend = {}, {}, {}
        for sid, g in sd.groupby("set_id"):
            g = g.set_index("d")
            a = g.adr_w_fees.replace(0, np.nan)
            self.adr[int(sid)] = a
            self.occ[int(sid)] = g.occupancy_adjusted
            now = a.reindex([TODAY - timedelta(days=i) for i in range(1, 61)]).dropna()
            then = a.reindex([TODAY - timedelta(days=i + 364) for i in range(1, 61)]).dropna()
            if len(now) >= 30 and len(then) >= 30:
                self.trend[int(sid)] = now.mean() / then.mean()

    def adr_on(self, sid, d):
        """Median non-zero set ADR over d+/-7 (needs >=8 priced nights; widens to +/-21). Small sets are lumpy
        day to day (one booking can post $514 on an off-season night), so never a single day."""
        a = self.adr.get(sid)
        if a is None:
            return None
        for k in (7, 21):       # widen to +/-3 weeks when the set is thin (small winter sets)
            w = a.reindex([d + timedelta(days=i) for i in range(-k, k + 1)]).dropna()
            if len(w) >= 8:
                return float(w.median())
        return None

    def ratio(self, sid, d, lyd):
        """(ratio, source): forward booked ADR vs LY when the set has enough on the books,
        else the set's trailing 60-day YoY. Clipped to 0.75-1.30."""
        a, o = self.adr.get(sid), self.occ.get(sid)
        if a is None:
            return None, None
        win = lambda s, c: s.reindex([c + timedelta(days=i) for i in range(-7, 8)]).dropna()
        fo = win(o, d)
        if len(fo) and fo.mean() >= 0.15:
            fw, ly = win(a, d), win(a, lyd)
            if len(fw) >= 5 and len(ly) >= 5:
                return float(np.clip(fw.median() / ly.median(), 0.75, 1.30)), "comp set booked ADR vs LY"
        if sid in self.trend:
            return float(np.clip(self.trend[sid], 0.75, 1.30)), "comp set 60-day trend vs LY"
        return None, None


# ---------------------------------------------------------------- LY nights
def ly_nights(res, lmeta, mkt):
    """Explode paid LY stays into nights; split fare by the market's daily ADR profile."""
    res = res[(res.source != "owner") & (res.fare_accommodation_adjusted > 0)].copy()
    res = res[(res.check_out > "2025-01-01")]       # LY lookups + listing-vs-comp-set position
    res = res[res.nights < MIDTERM_NIGHTS]          # monthly/mid-term rates aren't nightly evidence
    rows = []
    for r in res.itertuples():
        ci, co = D(r.check_in), D(r.check_out)
        nights = [ci + timedelta(days=i) for i in range((co - ci).days)]
        if not nights:
            continue
        meta = lmeta.get(r.listing_id, {})
        w = [mkt.adr_on(meta.get("market_id"), meta.get("bucket", "all"), n) if meta else None for n in nights]
        w = [x if x and not pd.isna(x) else np.nan for x in w]
        w = np.array(w, dtype=float)
        if np.isnan(w).all():
            w = np.ones(len(nights))
        w = np.where(np.isnan(w), np.nanmean(w), w)
        share = w / w.sum()
        created = pd.Timestamp(r.created_at).tz_localize(None).date() if isinstance(r.created_at, str) else None
        lead = (ci - created).days if created else None
        has_hol = any(n in HOL for n in nights)
        for n, s in zip(nights, share):
            rows.append({"listing_id": r.listing_id, "night": n, "value": r.fare_accommodation_adjusted * s,
                         "stay_adr": r.fare_accommodation_adjusted / len(nights), "nights": len(nights),
                         "lead": lead, "stay_has_holiday": has_hol, "code": r.confirmation_code,
                         "source": r.source, "check_in": ci, "check_out": co})
    return pd.DataFrame(rows)


# ---------------------------------------------------------------- engine
def severity(pct):
    for t, name in SEV:
        if pct >= t:
            return name
    return None


def cap(sev, maxsev):
    return sev if RANK[sev] <= RANK[maxsev] else maxsev


def main():
    gl, res, cal, wl, mk, nb = load()
    mkt = Market(mk)
    wl["bucket"] = wl.bedrooms.map(bed_bucket)
    gl["bucket"] = gl.bedrooms.map(bed_bucket)
    lmeta = {r.listing_id: {"market_id": str(r.market_id), "bucket": r.bucket, "set_id": r.set_id,
                            "bedrooms": r.bedrooms, "name": r.name} for r in wl.itertuples()}
    names = dict(zip(gl.listing_id, gl.nickname))

    for v in lmeta.values():
        v["set_id"] = None if pd.isna(v["set_id"]) else int(v["set_id"])
    cs = CompSet(pd.read_csv(RAW / "wh_set_daily.csv"))
    occ = pd.read_csv(RAW / "wh_neighborhood_occupancy.csv")
    occ_i = {lid: g.set_index(pd.to_datetime(g.stay_date).dt.date)[["observed_bookings", "expected_bookings"]]
             for lid, g in occ.groupby("listing_id")}

    def pace(lid, d):
        g = occ_i.get(lid)
        if g is None:
            return None
        w = g.reindex([d + timedelta(days=i) for i in range(-3, 4)]).dropna()
        if w.expected_bookings.sum() < 20:
            return None
        return float(w.observed_bookings.sum() / w.expected_bookings.sum())

    nights = ly_nights(res, lmeta, mkt)
    nv = {(r.listing_id, r.night): r for r in nights.itertuples()}
    ly_nights_only = nights[(nights.night >= D("2025-09-01")) & (nights.night < D("2026-05-01"))]
    ly_have = set(ly_nights_only.listing_id)

    # Where each listing books relative to its comp set: median(night value / set ADR that night)
    pos = {}
    for lid, g in nights.groupby("listing_id"):
        sid = lmeta.get(lid, {}).get("set_id")
        if sid is None:
            continue
        r_ = [v / a for v, n in zip(g.value, g.night) if (a := cs.adr_on(sid, n))]
        if len(r_) >= 10:
            pos[lid] = (float(np.median(r_)), len(r_))
    events = {m: mkt.events(m) for m in wl.market_id.astype(str).unique()}

    # Size normaliser for sibling scaling. Guesty base_price is unusable (Elektra
    # 1115 = $85 vs 1004 = $199, same building/size), so: ratio of the pair's
    # median LY night values when both have >=20 LY nights, else ratio of
    # Wheelhouse's median recommended price (independent of our current prices).
    ly_med = ly_nights_only.groupby("listing_id").value.agg(["median", "count"])
    ly_med = ly_med[ly_med["count"] >= 20]["median"].to_dict()
    recs = pd.read_csv(RAW / "wh_price_recs.csv")
    rec_med = recs[recs.price > 0].groupby("listing_id").price.median().to_dict()

    def scale(t, s):
        if t in ly_med and s in ly_med:
            return min(2.0, max(0.5, ly_med[t] / ly_med[s])), "LY-rate ratio"
        if t in rec_med and s in rec_med:
            return min(2.0, max(0.5, rec_med[t] / rec_med[s])), "WH-rec ratio"
        return 1.0, "unscaled"

    def sib_pool(lid):
        """Same Wheelhouse set first; if none of those has LY history, same market +/-1 bedroom."""
        m = lmeta.get(lid)
        if not m:
            return [], ""
        pool = [k for k, v in lmeta.items() if k != lid and v["set_id"] == m["set_id"] and k in ly_have]
        if pool:
            return pool, "same comp set"
        pool = [k for k, v in lmeta.items() if k != lid and v["market_id"] == m["market_id"]
                and abs((v["bedrooms"] or 0) - (m["bedrooms"] or 0)) <= 1 and k in ly_have]
        return pool, "same market, +/-1 bedroom"

    def distressed(lid, lyd, rec):
        if rec.lead is None or pd.isna(rec.lead) or rec.lead > DISTRESS_LEAD:
            return False
        pool, _ = sib_pool(lid)
        sv = [nv[(s, lyd)].value for s in pool if (s, lyd) in nv]
        return bool(sv) and rec.value < DISTRESS_SHARE * np.median(sv)

    def band_from(v, ratio, pc):
        floor = v * (ratio if ratio is not None and ratio < MKT_DROP else 1.0)
        if pc is not None and pc <= PACE_WEAK:
            floor *= 0.90
        lift = ratio if ratio is not None and ratio > MKT_STRONG else 1.0
        if pc is not None and pc >= PACE_STRONG:
            lift *= 1.10
        return floor, v * CEIL_MULT * min(lift, MAX_LIFT)

    avail = cal[(cal.status == "available")].copy()
    avail["d"] = pd.to_datetime(avail.date).dt.date
    nbi = nb.set_index(["listing_id", "stay_date"])

    out = []
    for r in avail.itertuples():
        lid, d, price = r.listing_id, r.d, r.price
        m = lmeta.get(lid, {})
        lyd, lymode = ly_date(d)
        is_hol = d in HOL
        mid = m.get("market_id")
        ev = events.get(mid, set())
        is_event = lyd in ev
        near_event = (not is_event) and any((lyd + timedelta(days=k)) in ev for k in (-2, -1, 1, 2))
        ratio, rsrc = cs.ratio(m.get("set_id"), d, lyd) if m else (None, None)
        if ratio is None and m:
            ratio = mkt.ratio(mid, m.get("bucket", "all"), d, lyd)
            rsrc = "market booked ADR vs LY" if ratio is not None else None
        pc = pace(lid, d)
        row = dict(listing_id=lid, listing=names.get(lid, lid), date=d, dow=d.strftime("%a"),
                   days_out=(d - TODAY).days, price=price, was=None, ly_date=lyd, ly_match=lymode,
                   holiday=is_hol, event_ly=is_event, market_ratio=ratio, ratio_source=rsrc, pace=pc)
        step = floor = ceil = None
        maxsev, why, ly_stay, notes = "BLUNDER", "", None, []

        # step 1 -------------------------------------------------------
        rec = nv.get((lid, lyd))
        if rec is not None:
            if distressed(lid, lyd, rec):
                notes.append(f"LY {lyd} booking {rec.code} was made {int(rec.lead)}d out at "
                             f"${rec.value:,.0f}, under 60% of siblings -> treated as distressed")
            else:
                step = 1
                floor, ceil = band_from(rec.value, ratio, pc)
                ly_stay = f"{rec.code} {rec.check_in}->{rec.check_out} ({rec.nights}n, {rec.source})"
                if rec.stay_has_holiday and not is_hol:
                    maxsev = "MISTAKE"
                    notes.append("LY stay spanned a holiday; weekday share uncertain -> capped at MISTAKE")
                why = f"LY {lyd:%a %b %-d} booked ~${rec.value:,.0f}/night (split of {rec.nights}-night stay)"

        # step 2 / 3 order --------------------------------------------
        def try_step2():
            if is_hol or is_event:
                return None
            pool = nights[(nights.listing_id == lid)]
            cand = []
            for k in range(-4, 5):
                if k == 0:
                    continue
                cd = lyd + timedelta(days=7 * k)
                if cd in HOL or cd in ev:
                    continue
                x = nv.get((lid, cd))
                if x is not None and not distressed(lid, cd, x):
                    cand.append(x.value)
            if len(cand) < 2:
                return None
            v = float(np.median(cand))
            return v, f"{len(cand)} comparable LY {lyd:%a}s (+/-4 wks, no holidays/events) booked median ${v:,.0f}"

        def try_step3():
            pool, how = sib_pool(lid)
            vals, used = [], []
            for s in pool:
                x = nv.get((s, lyd))
                if x is None or distressed(s, lyd, x):
                    continue
                k, how_k = scale(lid, s)
                vals.append(x.value * k)
                used.append(names.get(s, s))
            if not vals:
                return None
            v = float(np.median(vals))
            return v, (f"{len(vals)} portfolio sibling(s) ({how}: {', '.join(used[:3])}"
                       f"{'…' if len(used) > 3 else ''}) booked LY {lyd:%b %-d}, scaled by {how_k} -> median ${v:,.0f}")

        if step is None:
            order = [3, 2] if near_event else [2, 3]
            for s in order:
                got = try_step2() if s == 2 else try_step3()
                if got:
                    step = s
                    floor, ceil = band_from(got[0], ratio, pc)
                    why = got[1]
                    if s == 3:
                        maxsev = "INACCURACY"
                    if near_event:
                        notes.append("date sits next to an LY event window -> siblings tried before comparable dates")
                    break

        # step 3b: the listing's comp set, same date LY ----------------
        if step is None and lid in pos:
            a = cs.adr_on(m.get("set_id"), lyd)
            if a:
                k, nk = pos[lid]
                step, maxsev = STEP_3B, "INACCURACY"
                floor, ceil = band_from(a * k, ratio, pc)
                why = (f"comp set booked a median ${a:,.0f} (ADR w/ fees) around LY {lyd:%b %-d} (+/-1-3 wks); this listing books at "
                       f"{k:.2f}x its set ({nk} nights) -> ${a * k:,.0f}")

        # step 4 --------------------------------------------------------
        if step is None and (lid, d.isoformat()) in nbi.index:
            n = nbi.loc[(lid, d.isoformat())]
            step, floor, ceil, maxsev = 4, float(n.low_price), float(n.high_price), "INACCURACY"
            why = (f"{'no LY history' if lid not in ly_have else 'no LY/comparable/sibling match'}; Wheelhouse comp band ${n.low_price:,.0f}-${n.high_price:,.0f} "
                   f"(median ${n.median_price:,.0f}, {int(n.listings_count)} comps)")
        if step is None:
            step = 5
            why = "no LY booking, no comparable dates, no siblings, no market band -> skipped"

        side = pct = sev = None
        if step < 5:
            if price < floor:
                side, pct = "DOWN", (floor - price) / floor
            elif price > ceil:
                side, pct = "UP", (price - ceil) / ceil
            if pct is not None:
                raw = severity(pct)
                sev = cap(raw, maxsev)
                if raw and sev != raw:
                    notes.append(f"raw {raw} capped at {sev} by evidence")
        if step in (1, 2, 3, STEP_3B):
            if ratio is not None and ratio < MKT_DROP:
                notes.append(f"{rsrc} {ratio:.0%} -> floor lowered")
            if ratio is not None and ratio > MKT_STRONG:
                notes.append(f"{rsrc} {ratio:.0%} -> ceiling lifted")
            if pc is not None and pc <= PACE_WEAK:
                notes.append(f"neighborhood pacing {pc:.0%} of typical -> floor lowered 10%")
            if pc is not None and pc >= PACE_STRONG:
                notes.append(f"neighborhood pacing {pc:.0%} of typical -> ceiling lifted 10%")
        row.update(step=step, floor=floor, ceiling=ceil, side=side, pct_outside=pct, severity=sev,
                   max_severity=maxsev, raw_severity=severity(pct) if pct else None,
                   evidence=why, ly_stay=ly_stay, notes="; ".join(notes), near_event=near_event)
        out.append(row)

    df = pd.DataFrame(out)

    # sibling corroboration: step-3 flags >=30% lift to BLUNDER when >=2 of the
    # listing's siblings (same pool as step 3) have step-1 BLUNDERs that date on
    # the same side.
    s1b = df[(df.step == 1) & (df.severity == "BLUNDER")]
    corro = s1b.groupby(["date", "side"]).listing_id.apply(list).to_dict()
    for i, r in df[(df.step == 3) & (df.pct_outside >= 0.30)].iterrows():
        pool = set(sib_pool(r.listing_id)[0])
        others = [x for x in corro.get((r.date, r.side), []) if x in pool]
        if len(others) >= 2:
            df.at[i, "severity"] = "BLUNDER"
            df.at[i, "notes"] = (r.notes + "; " if r.notes else "") + (
                f"lifted to BLUNDER: {len(others)} siblings ({', '.join(names.get(x, x) for x in others[:3])}) "
                f"have step-1 BLUNDERs on the {r.side.lower()}side this date")

    df.to_pickle(ROOT / "data" / "scan_all.pkl")
    tgt = df[df.listing_id.isin(TARGETS)].copy()
    tgt.to_pickle(ROOT / "data" / "scan_targets.pkl")
    print(f"portfolio: {len(df)} unbooked nights banded over {df.listing_id.nunique()} listings")
    print(tgt.groupby("listing").step.value_counts().unstack(fill_value=0))
    print(tgt.groupby(["listing", "side"]).severity.value_counts().unstack(fill_value=0))


if __name__ == "__main__":
    main()
