"""Per-listing action items from listing_features.pkl + scan_all.pkl.

Each listing gets a priority (P1 this week / P2 this month / P3 check / OK),
a one-line headline and 1-4 concrete actions. Rules, in order:
  pinned    price sits at the Wheelhouse minimum most nights -> raise the minimum
  level     many strong (step 1-2) downside flags -> raise the price level
  minimum   Wheelhouse posting below its own minimum -> fix/realign the minimum
  adjust    -10% base adjustment -> remove or justify
  dates     worst individual mispriced nights (both sides) -> fix by hand
  data      no LY history / not in Wheelhouse / calendar closed
-> data/actions.pkl
"""
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
RAW = ROOT / "data" / "raw"
F = pd.read_pickle(ROOT / "data" / "listing_features.pkl")
scan = pd.read_pickle(ROOT / "data" / "scan_all.pkl")
cal = pd.read_csv(RAW / "guesty_calendar.csv")
mins = pd.read_excel(ROOT / "output" / "min_price_check_2026-10-06.xlsx", sheet_name="by_listing")
res = pd.read_csv(RAW / "guesty_reservations.csv")
_ly = res[(res.source != "owner") & (res.nights < 28) & (res.fare_accommodation_adjusted > 0)
          & (res.check_in >= "2025-10-06") & (res.check_in < "2026-04-04")].copy()
_ly["adr"] = _ly.fare_accommodation_adjusted / _ly.nights
LY_P25 = _ly.groupby("listing_id").adr.quantile(0.25).to_dict()   # its weakest-quarter LY stays


def r5(x):
    return int(5 * round(x / 5))


def money(x):
    return f"${x:,.0f}"


def pct(x):
    return f"{x:.0%}"


def months_below(name):
    row = mins[mins.listing == name]
    if row.empty:
        return []
    cols = [c for c in mins.columns if c.startswith("% below ")]
    out = [c.replace("% below ", "") for c in cols if row[c].iloc[0] >= 50]
    return [pd.Timestamp(m + "-01").strftime("%b") for m in out]


def worst_dates(lid, side, k=3):
    s = scan[(scan.listing_id == lid) & (scan.side == side) & scan.severity.isin(["BLUNDER", "MISTAKE"])]
    s = s.sort_values("pct_outside", ascending=False).head(k)
    return list(s.itertuples())


def fmt_dates(rows, side):
    parts = []
    for r in rows:
        tgt = f"≥{money(r.floor)}" if side == "DOWN" else f"≤{money(r.ceiling)}"
        parts.append(f"{r.date:%b %-d} ({money(r.price)} → {tgt}, {r.severity.lower()})")
    return "; ".join(parts)


rows = []
for f in F.itertuples():
    lid, name = f.listing_id, f.listing
    c = cal[(cal.listing_id == lid) & (cal.status == "available")]
    acts, prio, head = [], "OK", "Priced inside the band — no change."
    sev_down = f.down_B + f.down_M
    has_ly = f.ly_stays >= 5
    yac = name.startswith("Yacinde")

    # ---- data / structural cases first
    if not f.in_wheelhouse:
        if name == "Cottages All OSBR":
            prio, head = "P3", "Bundle listing outside Wheelhouse — confirm it's intentional."
            acts.append(f"Priced {money(f.ask_median)}/night for the whole cottage set and not in Wheelhouse, so "
                        "nothing here is checked. Confirm the bundle price still beats the sum of the 12 cottages "
                        "(now ~$80–100 each) and that it blocks correctly when any cottage books.")
        else:
            prio = "P2"
            head = "Not in Wheelhouse — prices are manual and nobody is checking them."
            acts.append("Add it to Wheelhouse (or a comp set) so it gets dynamic pricing and shows up in these checks.")
            if has_ly and f.ask_vs_ly < 0.85:
                acts.append(f"Until then, raise the manual rate: asking {money(f.ask_median)} median vs "
                            f"{money(f.ly_adr)} it booked LY ({pct(f.ask_vs_ly)} of LY).")
            up = worst_dates(lid, "UP")
            dn = worst_dates(lid, "DOWN")
            if dn:
                acts.append("Raise by hand: " + fmt_dates(dn, "DOWN") + ".")
            if up:
                acts.append("Lower by hand: " + fmt_dates(up, "UP") + ".")
        rows.append(dict(listing_id=lid, listing=name, priority=prio, headline=head, actions=acts))
        continue
    if f.nights_avail == 0:
        rows.append(dict(listing_id=lid, listing=name, priority="P3",
                         headline="Nothing for sale in the next 180 days.",
                         actions=[f"Calendar is fully closed ({f.nights_booked} booked, {f.nights_blocked} blocked). "
                                  "Confirm the blocks are intended (long-term tenant / owner use / renovation); "
                                  "if not, reopen it."]))
        continue

    lowest = c.price.min() if len(c) else np.nan
    floor_share = (c.price <= lowest + 2).mean() if len(c) else 0
    level_low = has_ly and (f.down_strong >= 25 or f.down_strong / max(f.nights_avail, 1) >= 0.2)
    gap = f.down_gap_median if not np.isnan(f.down_gap_median) else 0.2
    evid = (f"booked {money(f.ly_adr)} LY (median stay)"
            + (f", {money(f.fwd_booked_adr)} on stays already booked this season" if not np.isnan(f.fwd_booked_adr) else "")
            + f"; asking {money(f.ask_median)} median now")
    p25 = LY_P25.get(lid)
    target_min = r5(max(p25, lowest)) if p25 else None
    pinned = level_low and floor_share >= 0.4 and target_min and target_min >= lowest + 5
    handled_min = False
    adj_line = None
    if level_low:
        cur = f.adjustment if not np.isnan(f.adjustment) else 1.0
        new_adj = min(1.4, round(cur * (1 + min(gap, 0.35)), 2))
        adj_line = (f"raise the Wheelhouse base adjustment {cur:.2f} → {new_adj:.2f} (about +{pct(min(gap, .35))})")

    # ---- Yacinde: owner/HOA minimum question dominates
    if yac:
        prio = "P1"
        head = f"Minimum says {money(f.min_price)}, Wheelhouse posts ~{money(f.median_price_below_min)} — decide which is real."
        acts.append(f"{f.nights_below_min} of {f.nights_avail} open nights are below the {money(f.min_price)} "
                    "minimum. If that minimum is an owner/HOA requirement, enforce it now (delete any lower "
                    "date-range minimum in Wheelhouse). If it was an onboarding placeholder, lower the default "
                    f"to ~{money(r5(f.median_price_below_min))} so the setting matches what's posting.")
        n_dn = int(f.down_B + f.down_M + f.down_I)
        if n_dn >= 10:
            acts.append(f"No LY history (first stay {f.first_booking}). Its Wheelhouse comp set's LY rates, scaled to how "
                        f"this unit books against the set, put {n_dn} open nights under the floor (weak evidence, "
                        f"capped at inaccuracy). The {money(f.ask_median)} off-season price is on the low side, which "
                        "argues for keeping a minimum above what's posting now.")
        else:
            acts.append(f"No LY history (first stay {f.first_booking}). Its comp set's LY rates and the neighborhood band "
                        f"(median {money(f.comp_median)}) both put the current {money(f.ask_median)} inside the band, so the "
                        "off-season level itself is not the problem.")
        if name == "Yacinde E3":
            acts.append("Lower Dec 25–27 ($586–$693) to ≤$525 — above the top of the comp band.")
        if f.adjustment and f.adjustment > 1:
            acts.append(f"Keeps a +{f.adjustment - 1:.0%} adjustment — fine once the minimum question above is settled.")
        rows.append(dict(listing_id=lid, listing=name, priority=prio, headline=head, actions=acts))
        continue

    # ---- stuck on a floor
    if pinned:
        prio = "P1" if sev_down >= 15 else "P2"
        hidden = lowest < f.min_price - 2
        if hidden:
            head = (f"Stuck at {money(lowest)} on {pct(floor_share)} of open nights — a hidden floor under its "
                    f"{money(f.min_price)} minimum.")
            acts.append(f"Delete the lower date-range minimum that lets it post {money(lowest)}, and set one minimum of "
                        f"{money(target_min)} (its weakest quarter of LY Oct–Mar stays booked at ≥{money(p25)}); {evid}.")
        else:
            head = f"Stuck at the {money(f.min_price)} minimum on {pct(floor_share)} of open nights — raise the minimum."
            acts.append(f"Raise the Wheelhouse minimum {money(f.min_price)} → {money(target_min)} (its weakest quarter "
                        f"of LY Oct–Mar stays booked at ≥{money(p25)}); {evid}.")
        acts.append(f"Then re-run the scan; if better nights still sit under the floor, {adj_line}.")
        handled_min = True
    elif level_low:
        prio = "P1" if sev_down >= 20 else "P2"
        head = f"Priced ~{pct(gap)} under what it books — raise the level."
        acts.append(adj_line[0].upper() + adj_line[1:] + f"; {evid}. Clears {f.down_strong} strong under-floor nights together. Re-run the scan after; "
                    "if nights still sit under the floor, go further.")

    # ---- minimum not enforced (non-Yacinde)
    if f.min_pattern in ("Systemic", "Partial") and not handled_min:
        mb = months_below(name)
        when = f" (mostly {', '.join(mb)})" if mb else ""
        bookings_back_min = ((not np.isnan(f.ly_adr) and f.ly_adr >= f.min_price)
                             or (not np.isnan(f.fwd_booked_adr) and f.fwd_booked_adr >= 1.15 * f.min_price))
        if level_low or bookings_back_min or (not has_ly and f.ask_vs_comp < 0.7):
            acts.append(f"Wheelhouse is posting below its own {money(f.min_price)} minimum on {f.nights_below_min} "
                        f"nights{when}, median {money(f.median_price_below_min)}. The evidence supports the "
                        f"{money(f.min_price)} — find and delete the lower date-range minimum.")
        else:
            acts.append(f"Wheelhouse posts below its {money(f.min_price)} minimum on {f.nights_below_min} nights{when} "
                        f"(median {money(f.median_price_below_min)}). It booked {money(f.ly_adr)} LY and "
                        f"{money(f.fwd_booked_adr)} on stays already booked, so the lower price is realistic: update the "
                        f"default minimum to ~{money(r5(f.median_price_below_min))} to match what's really applied.")
        if prio == "OK":
            prio, head = "P2", f"Posting below its {money(f.min_price)} minimum on {f.nights_below_min} nights."
    elif f.min_pattern == "Seasonal (Jan–Mar)":
        acts.append(f"Jan–Mar posts {money(f.median_price_below_min)} vs the {money(f.min_price)} minimum — looks like "
                    "a deliberate winter minimum; no change if you set it.")

    # ---- -10% adjustment
    if f.adjustment == 0.9 and not level_low:
        acts.append("Has a −10% base adjustment. Confirm it's deliberate; if it was a launch discount, reset to 1.00.")
        if prio == "OK":
            prio, head = "P3", "−10% base adjustment still on — confirm it's intended."
    elif f.adjustment == 0.9 and level_low and not pinned:
        acts[0] += " Start by removing the −10% adjustment (it's part of the gap)."

    # ---- specific dates
    up = worst_dates(lid, "UP")
    if level_low:
        floor_now = target_min if pinned else lowest
        dropped = [u for u in up if u.ceiling < floor_now]
        up = [u for u in up if u.ceiling >= floor_now]
        if dropped:
            acts.append(f"Ignore the over-ceiling flag(s) on {', '.join(f'{u.date:%b %-d}' for u in dropped)}: they come "
                        "from unusually cheap LY nights and conflict with the level fix above.")
    if up:
        acts.append("Lower: " + fmt_dates(up, "UP") + ".")
        if prio == "OK":
            prio, head = "P2", "A few nights priced over the ceiling."
    if not level_low:
        dn = worst_dates(lid, "DOWN")
        if dn:
            acts.append("Raise: " + fmt_dates(dn, "DOWN") + ".")
            if prio in ("OK", "P3"):
                prio, head = "P2", "A few nights priced under the floor."

    # ---- thin evidence
    if not has_ly:
        steps = f.steps
        acts.append(f"Thin history ({f.ly_stays} LY stays; first stay {f.first_booking}) — flags rest on siblings, its comp set or the neighborhood band "
                    f"comps (median {money(f.comp_median)} vs asking {money(f.ask_median)}). Re-check after this season.")
        if prio == "OK" and f.ask_vs_comp < 0.65:
            prio, head = "P3", f"New listing priced at {pct(f.ask_vs_comp)} of comps — watch conversion."
    if f.nights_avail < 60 and f.nights_blocked > 60:
        acts.append(f"Only {f.nights_avail} nights open ({f.nights_blocked} blocked) — likely sold as part of a "
                    "whole-house listing; confirm the split/whole blocking is intended.")
    if not acts:
        acts.append("No flags worth acting on. Leave it.")
    rows.append(dict(listing_id=lid, listing=name, priority=prio, headline=head, actions=acts))

A = pd.DataFrame(rows).merge(F, on=["listing_id", "listing"])
A["prio_rank"] = A.priority.map({"P1": 0, "P2": 1, "P3": 2, "OK": 3})
A["severe_down"] = A.down_B + A.down_M
A["theme"] = [
    "Yacinde minimum decision" if r.listing.startswith("Yacinde") else
    "Not in Wheelhouse / closed" if (not r.in_wheelhouse or r.nights_avail == 0) else
    "Raise the minimum" if r.headline.startswith("Stuck") else
    "Raise the price level" if "raise the level" in r.headline else
    "Minimum not enforced" if r.headline.startswith("Posting below") else
    "Fix specific nights" if r.headline.startswith("A few") else
    "Confirm a setting" if r.priority == "P3" else "No change"
    for r in A.itertuples()]
A = A.sort_values(["prio_rank", "severe_down"], ascending=[True, False])
A.to_pickle(ROOT / "data" / "actions.pkl")
print(A.priority.value_counts())
for r in A.itertuples():
    print(f"\n[{r.priority}] {r.listing} — {r.headline}")
    for a in r.actions:
        print("   •", a)
