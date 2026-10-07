"""Build the shareable revenue dashboard page (a single HTML file with the data
embedded) from one report run's outputs.

    python build_artifact.py                 # newest output/<date>/
    python build_artifact.py --asof 2026-09-29

Writes output/<date>/revenue_dashboard.html and the stable copy
output/revenue_dashboard.html from artifact_template.html. The page renders
the Valta / Trends / Owner views in the browser. Publish the STABLE
path as the Artifact and republish it each week so the link never changes.
"""
import argparse
import json

import numpy as np
import pandas as pd

from paths import OUTPUT_DIR, PROJECT

TEMPLATE = PROJECT / "artifact_template.html"


def _r(x, nd):
    return None if pd.isna(x) else round(float(x), nd)


def build(run: str):
    d = OUTPUT_DIR / run
    lm = pd.read_csv(d / "dash_listing_monthly.csv")
    bench = pd.read_csv(d / "dash_benchmarks.csv")
    yearly = pd.read_excel(d / "RevenueReport_upd.xlsx", sheet_name="yearly")

    # same derivations as dashboard.load()
    ltr = lm["Type"] == "LTR"
    lm.loc[ltr, "ADR"] = lm.loc[ltr, "Revenue_occ"] / lm.loc[ltr, "occdays"].replace(0, np.nan)
    lm["Bedrooms"] = lm["Bedrooms"].astype("string").fillna("")
    occ_col = [c for c in yearly.columns if c.startswith("OccRt_") and c.endswith("_today")]
    occ_ytd = ({k: _r(v, 4) for k, v in yearly.set_index("Listing")[occ_col[0]].items() if pd.notna(v)}
               if occ_col else {})

    names = sorted(set(lm["Listing"].dropna()) | set(bench["Listing"].dropna()))
    idx = {n: i for i, n in enumerate(names)}
    meta = lm.drop_duplicates("Listing").set_index("Listing")
    set_names = sorted(bench.loc[bench["kind"] == "set", "benchmark"].str.removeprefix("Set: ").unique())
    sidx = {n: i for i, n in enumerate(set_names)}
    listings = []
    for n in names:
        m = meta.loc[n] if n in meta.index else None
        sets = sorted({sidx[s.removeprefix("Set: ")] for s in
                       bench.loc[(bench["Listing"] == n) & (bench["kind"] == "set"), "benchmark"]})
        listings.append({
            "n": n,
            "t": None if m is None or pd.isna(m["Type"]) else m["Type"],
            "s": None if m is None or pd.isna(m["Status"]) else m["Status"],
            "mk": None if m is None or pd.isna(m["Market"]) else m["Market"],
            "b": "" if m is None else str(m["Bedrooms"]),
            "p": None if m is None or pd.isna(m["Property"]) else m["Property"],
            "sets": sets,
        })

    ym = lambda s: s.str[:4].astype(int) * 100 + s.str[5:7].astype(int)
    lm = lm[lm["Listing"].notna()]
    data_lm = {
        "li": lm["Listing"].map(idx).tolist(), "ym": ym(lm["yearmonth"]).tolist(),
        # revenue in the month (bookings split by nights); pre-2023 rows lack
        # check-out dates and keep the report's Revenue
        "rev": [_r(v, 2) for v in (lm["Revenue_month"].fillna(lm["Revenue"])
                                   if "Revenue_month" in lm else lm["Revenue"])], "rocc": [_r(v, 2) for v in lm["Revenue_occ"]],
        "nd": [_r(v, 0) for v in lm["occdays"]], "occ": [_r(v, 4) for v in lm["OccRt"]],
        "adr": [_r(v, 2) for v in lm["ADR"]], "adrf": [_r(v, 2) for v in lm["ADR_w_fees"]],
        "lt": [_r(v, 1) for v in lm["LeadTime"]], "nb": [_r(v, 0) for v in lm["Bookings"]],
    }
    # guest-payment parts (2026+; see revenue_mix.py), sparse: listing-month index -> parts
    mix_cols = ["Mix_rent", "Mix_clean", "Mix_tax", "Mix_channel", "Mix_lease", "Mix_none",
                "Mix_acc", "Mix_pet", "Mix_disc", "Mix_other", "Mix_comm"]
    if "Mix_rent" in lm:
        has = lm[mix_cols].notna().any(axis=1).values
        data_lm["mix"] = {"i": [int(i) for i in np.flatnonzero(has)],
                          **{c[4:].lower(): [_r(v, 0) for v in lm.loc[has, c].fillna(0)] for c in mix_cols}}
    # owner payout is per Property (owner statement) x month, repeated on each of
    # its listings -> keep one value per Property-month
    pay = (lm.dropna(subset=["Payout", "Property"]).drop_duplicates(["Property", "yearmonth"]))
    data_pay = {}
    for prop, g in pay.groupby("Property"):
        data_pay[prop] = {int(k[:4]) * 100 + int(k[5:7]): _r(v, 2) for k, v in zip(g["yearmonth"], g["Payout"])}
    # yearly owner distributions per Property (some statements, e.g. OSBR, have no
    # monthly payout rows); used for whole-year / YTD periods when monthly is missing
    po = pd.read_excel(d / "RevenueReport_upd.xlsx", sheet_name="payout")
    data_pay_y = {}
    for _, r in po.iterrows():
        ys = {int(c[-4:]): _r(r[c], 2) for c in po.columns if c.startswith("OwnerDist_") and pd.notna(r[c])}
        if ys and pd.notna(r["Property"]):
            data_pay_y[r["Property"]] = ys
    kind_code = {"set": 0, "market_hp": 1, "market_all": 2}
    bench = bench[bench["Listing"].isin(idx)]
    data_b = {
        "li": bench["Listing"].map(idx).tolist(), "ym": ym(bench["yearmonth"]).tolist(),
        "k": bench["kind"].map(kind_code).tolist(),
        "si": [sidx.get(str(b).removeprefix("Set: "), -1) if k == "set" else -1
               for b, k in zip(bench["benchmark"], bench["kind"])],
        "occ": [_r(v, 4) for v in bench["Occ"]], "adr": [_r(v, 2) for v in bench["ADR"]],
        "lt": [_r(v, 1) for v in (bench["Lead"] if "Lead" in bench else [None] * len(bench))],
    }
    payload = {"asof": run, "listings": listings, "setNames": set_names,
               "lm": data_lm, "bench": data_b, "occYtd": occ_ytd, "pay": data_pay, "payY": data_pay_y}
    blob = json.dumps(payload, separators=(",", ":")).replace("</", "<\\/")
    html = TEMPLATE.read_text().replace("/*__DATA__*/", blob).replace("__ASOF__", run)
    out = d / "revenue_dashboard.html"
    out.write_text(html)
    stable = OUTPUT_DIR / "revenue_dashboard.html"   # publish THIS path so the link stays the same
    stable.write_text(html)
    print(f"  artifact: wrote {out} and {stable} ({out.stat().st_size / 1e6:.2f} MB, {len(names)} listings)")
    return stable


def main():
    ap = argparse.ArgumentParser(description="Build the shareable revenue dashboard page.")
    ap.add_argument("--asof", default=None, help="Report date (default: newest output/<date>)")
    a = ap.parse_args()
    runs = sorted(p.name for p in OUTPUT_DIR.iterdir() if (p / "dash_listing_monthly.csv").exists())
    build(a.asof or runs[-1])


if __name__ == "__main__":
    main()
