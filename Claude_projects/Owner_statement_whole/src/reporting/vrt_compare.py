"""Reconcile our owner statements against the VRT export for one period.

    python -m src.reporting.vrt_compare --period 2026-07

Reads ``inputs/<period>/VRT_owner-statements-<period>.csv`` and compares VRT's
**Net revenue** against our **Net Rental Revenue less the PM fee** — i.e. what the owner
earned on the bookings after commission, before owner expenses and adjustments. Writes
``output/<period>/vrt_comparison_<period>.xlsx`` with four sheets:

* **Comparison**   one row per statement: VRT vs ours, with the difference.
* **Unmatched**    statements present on only one side, so nothing is silently dropped.
* **Summary**      the period's full all-properties summary (``reporting.monthly_summary``).
* **Notes**        what each column means and how the two sides are keyed.

`PM Fee` is stored NEGATIVE (a cost), so "net less the PM fee" is an ADDITION here.
`Amount Due to Owner` is carried alongside for context — it is a different figure
(expenses, taxes and owner adjustments are in it) and is NOT what VRT's Net revenue means.

**Keying**: VRT names statements slightly differently ("Cottages All OSBR", "Seattle 710
combined", "Beachwood 1"), and it reports one row where we produce two (Mercer 3627 ADU +
Lower). Both sides are therefore folded onto a shared key via `_VRT_ALIASES` /
`_OURS_REGROUP` and summed before joining — never matched on the raw label.
"""
import argparse
import re
import sqlite3

import pandas as pd

from .. import paths
from ..common.config import load_config
from ..scope.listing_filter import _alias, _label_to_pid, _norm, statement_rollups
from .monthly_summary import METRICS, build

# VRT exports in TWO shapes, and they do NOT report the same measure — mapping one onto the
# other silently compares a pre-expense figure against a post-expense one.
#
#   "net revenue" layout  (Listing/Owner/.../Net revenue/Expenses/Balance end)
#       -> VRT's `Net revenue` is bookings NET OF COMMISSION but BEFORE owner expenses,
#          so it lines up with our `Net Rental Revenue + PM Fee`.
#   "net income"  layout  (Listing/Owners/Month/.../Net Income/Owner Payout/Ending Balance)
#       -> VRT's `Net Income` is AFTER expenses: it equals its own `Owner Payout`, and it
#          lines up with our `Amount Due to Owner`. Checked on 2026-06: against Amount Due
#          the 55 matched listings sum |diff| $35,932 with 9 exact ties, against
#          net-after-PM-fee $57,877 with 4 — and the per-row residues are single dollars
#          vs hundreds (Elektra 1108: $0.00 vs $3,358.63).
#
# `LAYOUTS` maps each to (VRT amount column, our comparable column, human label).
LAYOUTS = {
    "net revenue": ("Net revenue", "Net after PM Fee",
                    "bookings net of commission, before owner expenses"),
    "net income": ("Net Income", "Amount Due to Owner",
                   "after owner expenses, taxes and adjustments — VRT's own Owner Payout"),
}

# VRT label -> shared key. Anything not listed falls through to the normalized label.
_VRT_ALIASES = {
    "cottages all osbr": "osbr",
    "seattle 710 combined": "seattle 710",
    "seattle 7434 combined": "seattle 7434",
    "yacinde nugrowth chelan": "yacinde nugrowth",
    "beachwood 1": "beachwood",
}
# Our statement id -> shared key, where VRT reports several of ours as one row.
_OURS_REGROUP = {
    "mercer_3627_adu": "mercer 3627",
    "mercer_3627_lower": "mercer 3627",
}


# Keys where the two sides label the same statement differently enough that a reviewer
# should see it spelled out before trusting the difference.
_KEY_NOTES = {
    "beachwood": "VRT labels this 'Beachwood 1'; our statement rolls up all Beachwood units.",
    "osbr": "VRT 'Cottages All OSBR' = our OSBR rollup.",
    "seattle 710": "VRT 'Seattle 710 combined' = 710 + ADU.",
    "seattle 7434": "VRT 'Seattle 7434 combined' = lower + upper + whole.",
    "mercer 3627": "VRT reports one row; we produce ADU + Lower, summed here.",
    "yacinde nugrowth": "VRT 'Yacinde - NuGrowth Chelan'.",
}


def _key(label):
    """Normalized comparison key: lowercase, punctuation collapsed to single spaces."""
    s = re.sub(r"[^a-z0-9]+", " ", str(label or "").lower()).strip()
    return _VRT_ALIASES.get(s, s)


def _money(v):
    """'$6,089.76' / '-$6,089.76' / '($12.00)' -> float."""
    s = str(v or "").strip()
    if not s:
        return 0.0
    neg = s.startswith("-") or (s.startswith("(") and s.endswith(")"))
    s = re.sub(r"[^0-9.]", "", s)
    if not s:
        return 0.0
    return -float(s) if neg else float(s)


def load_vrt(period):
    """(path, frame, layout). Detects which of the two export shapes the file is."""
    path = paths.inputs_dir(period) / f"VRT_owner-statements-{period}.csv"
    df = pd.read_csv(path)
    df.columns = [str(c).strip().strip('"').lstrip("\ufeff") for c in df.columns]
    layout = next((k for k, (col, _, _) in LAYOUTS.items() if col in df.columns), None)
    if layout is None:
        raise SystemExit(f"{path.name}: unrecognized VRT export — columns {list(df.columns)}; "
                         f"expected one of {[c for c, _, _ in LAYOUTS.values()]}")
    amount_col = LAYOUTS[layout][0]
    df["key"] = df["Listing"].map(_key)
    df["VRT Amount"] = df[amount_col].map(_money)
    df["VRT Owner"] = df["Owner"] if "Owner" in df.columns else df.get("Owners", "")
    # Only the "net revenue" layout itemizes expenses / ending balance.
    df["VRT Expenses"] = df["Expenses"].map(_money) if "Expenses" in df.columns else 0.0
    df["VRT Balance End"] = (df["Balance end"].map(_money) if "Balance end" in df.columns
                             else df["Ending Balance"].map(_money)
                             if "Ending Balance" in df.columns else 0.0)
    return path, df, layout


def _vrt_unit_folder(conn, rollups, our_names):
    """VRT per-unit label -> the key of OUR statement that unit rolls into.

    The 2026-01..06 export lists every LISTING ('Cottage 4', 'Seattle 10057 Lower',
    'Bellevue 2323 ADU'), while the 2026-07 export uses combined labels ('Cottages All
    OSBR', 'Seattle 710 combined'). Rather than hand-maintaining an alias per unit, fold
    VRT's label through the SAME scope layer the statements are built from: resolve the
    label to a property_id via listing_filter's `_label_to_pid`/`_alias` (so 'Cottage 4'
    -> osbr_4), then map that member to its statement parent via `statement_rollups`. A
    label that is already a statement parent maps to itself.
    """
    by_norm = _label_to_pid(conn)
    member_to_parent = {m: parent for parent, members in rollups.items() for m in members}
    def resolve(label):
        n = _norm(label)
        pid = by_norm.get(n) or by_norm.get(_alias(n))
        if pid is None:
            return None
        parent = member_to_parent.get(pid, pid)
        return our_names.get(parent)
    return resolve


def compare(conn, period):
    path, vrt, layout = load_vrt(period)
    vrt_col, our_col, meaning = LAYOUTS[layout]
    ours = build(conn, [period])
    ours["key"] = [_OURS_REGROUP.get(p, _key(n))
                   for n, p in zip(ours["Property"], ours["Property ID"])]
    # What VRT's "Net revenue" is: bookings net of commission. PM Fee is stored negative.
    ours["Net after PM Fee"] = (ours["Net Rental Revenue"] + ours["PM Fee"]).round(2)
    # How many listings each of our statements rolls up — a one-label-vs-many mismatch is
    # the first thing to check when a difference looks structural rather than monetary.
    rollups = statement_rollups(conn, load_config(str(paths.CONFIG_YML)).get("statement_rollups") or {})
    ours["Our Units"] = [1 + len(rollups.get(p, [])) for p in ours["Property ID"]]

    # Fold VRT's per-unit rows onto our statement keys (the 2026-01..06 export shape).
    # Applied AFTER the hand-written _VRT_ALIASES, which handle the combined labels the
    # 2026-07 shape uses; a label the scope layer cannot resolve keeps its own key and
    # surfaces on the Unmatched sheet rather than being silently dropped.
    our_key_by_pid = dict(zip(ours["Property ID"], ours["key"]))
    resolve = _vrt_unit_folder(conn, rollups, our_key_by_pid)
    vrt["key"] = [resolve(lbl) or k for lbl, k in zip(vrt["Listing"], vrt["key"])]

    o = (ours.groupby("key")
              .agg(**{"Our Statement(s)": ("Property", lambda s: " + ".join(sorted(s))),
                      "Net Rental Revenue": ("Net Rental Revenue", "sum"),
                      "PM Fee": ("PM Fee", "sum"),
                      "Net after PM Fee": ("Net after PM Fee", "sum"),
                      "Total Expenses": ("Total Expenses", "sum"),
                      "Owner Adjustments": ("Owner Adjustments", "sum"),
                      "Amount Due to Owner": ("Amount Due to Owner", "sum"),
                      "Our Units": ("Our Units", "sum")})
              .reset_index())
    v = (vrt.groupby("key")
            .agg(**{"VRT Listing(s)": ("Listing", lambda s: " + ".join(sorted(s))),
                    "VRT Owner": ("VRT Owner", "first"),
                    "VRT Amount": ("VRT Amount", "sum"),
                    "VRT Expenses": ("VRT Expenses", "sum"),
                    "VRT Balance End": ("VRT Balance End", "sum")})
            .reset_index())

    m = v.merge(o, on="key", how="outer", indicator=True)
    both = m[m["_merge"] == "both"].copy()
    both["Difference"] = (both[our_col] - both["VRT Amount"]).round(2)
    both["Match"] = both["Difference"].abs().le(0.02).map({True: "OK", False: ""})
    both["Note"] = both["key"].map(_KEY_NOTES).fillna("")
    both = both.rename(columns={"VRT Amount": f"VRT {vrt_col}"})
    # our_col is one of the context columns, so de-duplicate while preserving order —
    # a repeated label makes `both[our_col]` a DataFrame and breaks every downstream format.
    cols, seen = [], set()
    for c in ["VRT Listing(s)", "Our Statement(s)", "Our Units", "VRT Owner",
              f"VRT {vrt_col}", our_col, "Difference", "Match", "Note",
              "Net Rental Revenue", "PM Fee", "Net after PM Fee",
              "VRT Expenses", "Total Expenses", "Owner Adjustments",
              "Amount Due to Owner", "VRT Balance End"]:
        if c not in seen:
            cols.append(c); seen.add(c)
    both = both[cols]
    both = both.sort_values("Difference", key=lambda s: s.abs(), ascending=False)

    only = m[m["_merge"] != "both"].copy()
    only["Side"] = only["_merge"].map({"left_only": "VRT only (no statement from us)",
                                       "right_only": "Ours only (not in VRT)"})
    only = only.rename(columns={"VRT Amount": f"VRT {vrt_col}"})
    ocols, oseen = [], set()
    for c in ["Side", "key", "VRT Listing(s)", "Our Statement(s)", f"VRT {vrt_col}",
              "Net after PM Fee", "Amount Due to Owner"]:
        if c not in oseen:
            ocols.append(c); oseen.add(c)
    only = only[ocols].sort_values(["Side", "key"])
    return path, ours, both, only, layout


def _notes(layout):
    vrt_col, our_col, meaning = LAYOUTS[layout]
    return [(f"VRT {vrt_col}", f"VRT export column '{vrt_col}' — {meaning}."),
            (our_col, "Our comparable figure for this export shape."),
            ("Export shape", f"This file is the '{layout}' layout. VRT ships two: 'Net revenue'"
                             " (pre-expense) and 'Net Income' (post-expense, = its own Owner"
                             " Payout). They are compared against DIFFERENT columns of ours.")] + _NOTES


_NOTES = [
    ("Net Rental Revenue", "Our Section-2 net rental revenue (= gross booking revenue)."),
    ("PM Fee", "Our management commission. Stored NEGATIVE (a cost to the owner)."),
    ("Net after PM Fee", "Net Rental Revenue + PM Fee — the figure compared against VRT."),
    ("Difference", "Net after PM Fee - VRT Net Revenue. Positive = we report more."),
    ("Amount Due to Owner", "Context only. Also nets owner expenses, taxes and adjustments,"
                            " so it is NOT comparable to VRT Net revenue."),
    ("Our Units", "How many listings our statement rolls up. VRT showing one label against"
                  " a multi-unit rollup is a scope difference, not a money difference."),
    ("Keying", "VRT labels are folded onto ours: 'Cottages All OSBR'->OSBR, 'Seattle 710/7434"
               " combined', 'Beachwood 1'->Beachwood; our Mercer 3627 ADU + Lower are summed"
               " into VRT's single 'Mercer 3627' row."),
]


def main():
    ap = argparse.ArgumentParser(description="Compare our statements against the VRT export.")
    ap.add_argument("--period", required=True, help="YYYY-MM")
    args = ap.parse_args()

    conn = sqlite3.connect(str(paths.DB_PATH))
    conn.row_factory = sqlite3.Row      # other_income.build_records reads rows by name
    src, ours, both, only, layout = compare(conn, args.period)
    vrt_col, our_col, meaning = LAYOUTS[layout]

    out_dir = paths.OUTPUT_DIR / args.period
    out_dir.mkdir(parents=True, exist_ok=True)
    out = out_dir / f"vrt_comparison_{args.period}.xlsx"
    summary = ours[["Property", "Property ID", "Month"] + METRICS + ["Net after PM Fee"]]
    with pd.ExcelWriter(out, engine="openpyxl") as xl:
        both.round(2).to_excel(xl, sheet_name="Comparison", index=False)
        only.round(2).to_excel(xl, sheet_name="Unmatched", index=False)
        summary.round(2).to_excel(xl, sheet_name=f"Summary {args.period}", index=False)
        pd.DataFrame(_notes(layout), columns=["Column", "Meaning"]).to_excel(
            xl, sheet_name="Notes", index=False)

    ok = int((both["Match"] == "OK").sum())
    print(f"VRT source : {src}")
    print(f"Layout     : '{layout}' -> VRT '{vrt_col}' vs our '{our_col}' ({meaning})")
    print(f"Compared   : {len(both)} statement(s) — {ok} match to the cent, "
          f"{len(both) - ok} differ")
    print(f"Unmatched  : {len(only)}")
    print(f"Totals     : VRT {both[f'VRT {vrt_col}'].sum():,.2f}   "
          f"ours {both[our_col].sum():,.2f}   "
          f"diff {both['Difference'].sum():,.2f}   "
          f"sum|diff| {both['Difference'].abs().sum():,.2f}")
    print(f"  -> {out}")
    d = both[both["Match"] != "OK"]
    if len(d):
        print("\nDifferences:")
        print(d[["VRT Listing(s)", f"VRT {vrt_col}", our_col, "Difference"]]
              .head(15).to_string(index=False))


if __name__ == "__main__":
    main()
