"""Rebuild the summary tabs from a hand-corrected "Bookings + Cleaning" tab.

The combined tab is the source of truth and is left exactly as it is. Every other derived tab
(Summary, By Owner, Whole Owner Cleans by Month, Fraction Cleans by Month, one Invoice tab per party
in INVOICE_PARTIES (cleaning lines that carry an amount - $0 rows such as the uninvoiced fraction
weeks are left off), Invoice_HOA_supply (HOA's supplies, one line per booking), Cleaning Allocation,
Exceptions) is regenerated from it. Bookings, Dedup Log and Prev Holds Excluded are not touched.

Reading the corrected tab:
  * A stay row is one with stay_check_in. Its supplies are stay_supplies_cost; the owner and party
    come from the row's week columns.
  * A clean row is one with clean_cleaning_paid_by. That includes $0 rows the user marked as HOA or
    owner cleans. When paid_by is HOA, hoa_paid = clean_Amount; otherwise owner_paid = clean_Amount.
    The stored clean_hoa_paid / clean_owner_paid columns are ignored because edits can leave them stale.
  * A trimmed extract (a single tab, no ownership_type / party / supplies columns, as in
    yacinde_expense_allocation_2026jul_aug.xlsx) is handled too: the ownership type comes from the
    fractional master week (or the whole-owner file), the party from config, and supplies are left out.
    A paid_by written as a correction ("NuGrowth->HOA") counts as the party after the arrow.
  * A clean's month is clean_Date, falling back to stay_check_out and then week_end. A fractional
    clean counts toward its row's week (the week the stay checks in) for the HOA one-per-week check.

    python -m src.resummarize [--file output/yacinde_expense_allocation_20260701_20260915.xlsx]
"""
from __future__ import annotations

import argparse
import datetime
from pathlib import Path

import openpyxl
import pandas as pd
import yaml

ROOT = Path(__file__).resolve().parents[1]
COMBINED = "Bookings + Cleaning"
REBUILT = ["Summary", "By Owner", "Whole Owner Cleans by Month", "Fraction Cleans by Month",
           "Cleaning Allocation", "Exceptions"]
INVOICE_PARTIES = ["HOA", "NuGrowth", "Yacinde Holdings"]      # one cleaning invoice tab each
SUPPLY_TAB = "Invoice_HOA_supply"                              # HOA's supplies, one line per booking


def source_sheet(sheets: dict[str, pd.DataFrame]) -> str:
    """The combined tab, whatever it is called: the one carrying both stay_ and clean_ columns."""
    if COMBINED in sheets:
        return COMBINED
    for name, df in sheets.items():
        if name not in REBUILT and not name.startswith("Invoice - ") \
                and {"stay_check_in", "clean_Date"} <= set(df.columns):
            return name
    return next(iter(sheets))


def _excel_date(v):
    if isinstance(v, (int, float)) and not pd.isna(v):        # a cell Excel stored as a serial number
        return pd.Timestamp("1899-12-30") + pd.Timedelta(days=int(v))
    return v


def add_supplies(d: pd.DataFrame, cfg: dict) -> pd.DataFrame:
    """Fill stay_supplies_cost (from the full workbook when the extract lacks it) and say who pays it.

    HOA bears the supplies of every booking checking in on/after `supplies.hoa_pays_from`, whatever
    the unit's owner or party. Stays checking in earlier are marked "not charged" and appear on no invoice.
    """
    d = d.copy()
    if "stay_supply_guests" not in d:
        d["stay_supply_guests"] = None
    if "stay_supplies_cost" not in d:
        src = pd.read_excel(ROOT / cfg["supplies"]["source_workbook"], sheet_name=COMBINED)
        src = src[src.stay_check_in.notna()]
        per_stay = list(zip(src.unit, pd.to_datetime(src.stay_check_in), pd.to_datetime(src.stay_check_out)))
        for col in ["stay_supplies_cost", "stay_supply_guests"]:
            per = {k: v for k, v in zip(per_stay, src[col])}
            d[col] = [per.get(k) for k in zip(d.unit, d.stay_check_in, d.stay_check_out)]
        missing = d.stay_check_in.notna() & d.stay_supplies_cost.isna()
        if missing.any():
            print(f"WARNING: no supplies found for {int(missing.sum())} stays "
                  + ", ".join(f"{r.unit} {r.stay_check_in:%m/%d}" for r in d[missing].itertuples()))
    frm = pd.Timestamp(str(cfg["supplies"]["hoa_pays_from"]))
    d["supplies_paid_by"] = pd.Series("HOA", index=d.index).where(d.stay_check_in >= frm, "not charged")
    return d


def label(d: pd.DataFrame, cfg: dict) -> pd.DataFrame:
    """Fill ownership_type / party on an extract that does not carry them."""
    if "ownership_type" in d and "party" in d:
        return d
    from src.build import load_master, load_whole_owners                      # same source files as the build
    inp = cfg["inputs"]
    m = load_master(ROOT / inp["fractional_master"], inp["fractional_sheet"])
    whole = load_whole_owners(ROOT / inp["whole_owners"])
    wk = dict(zip(zip(m.Listing, m.checkin), m["Ownership.Type"]))
    d = d.copy()
    d["ownership_type"] = [wk.get((u, w), "Whole owner" if u in whole else "-")
                           for u, w in zip(d.unit, d.week_start)]
    d["party"] = d.ownership_type.map(cfg["parties"]).fillna("UNASSIGNED")
    return d


def read_combined(path: Path, cfg: dict) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    sheets = pd.read_excel(path, sheet_name=None)
    d = sheets[source_sheet(sheets)]
    d["clean_Date"] = pd.to_datetime(d.clean_Date.map(_excel_date), errors="coerce")
    for c in ["stay_check_in", "stay_check_out"]:
        d[c] = pd.to_datetime(d[c])
    d = add_supplies(label(d, cfg), cfg)
    for c in ["week_start", "week_end", "stay_check_in", "stay_check_out", "clean_Date"]:
        d[c] = pd.to_datetime(d[c])
    d["clean_Invoice #"] = d["clean_Invoice #"].map(lambda v: None if pd.isna(v) else str(v).removesuffix(".0").strip())
    d["clean_cleaning_paid_by"] = d.clean_cleaning_paid_by.map(
        lambda v: None if pd.isna(v) else str(v).split("->")[-1].strip())      # "NuGrowth->HOA" -> HOA
    d["owner"] = d.owner.fillna("(no owner)")
    d["ownership_type"] = d.ownership_type.fillna("-")
    d["row"] = d.index + 2                                         # Excel row number, for exceptions

    stays = d[d.stay_check_in.notna()].copy()
    stays["stay_supplies_cost"] = stays.stay_supplies_cost.fillna(0.0)
    stays["stay_nights"] = stays.stay_nights.fillna((stays.stay_check_out - stays.stay_check_in).dt.days)

    cl = d[d.clean_cleaning_paid_by.notna()].copy()
    cl["Amount"] = cl.clean_Amount.fillna(0.0)
    cl["paid_by"] = cl.clean_cleaning_paid_by
    cl["hoa_paid"] = cl.Amount.where(cl.paid_by == "HOA", 0.0)
    cl["owner_paid"] = cl.Amount - cl.hoa_paid
    cl["date"] = cl.clean_Date.fillna(cl.stay_check_out).fillna(cl.week_end)
    cl["month"] = cl.date.dt.strftime("%Y-%m")
    cl["invoiced"] = cl["clean_Invoice #"].notna() & (cl["clean_Invoice #"] != "not invoiced") & (cl.Amount > 0)
    return d, stays, cl


def build_tabs(d: pd.DataFrame, stays: pd.DataFrame, cl: pd.DataFrame, hoa: dict) -> dict[str, pd.DataFrame]:
    st = stays.groupby("party").agg(stays=("unit", "size"), nights=("stay_nights", "sum"))
    charged = stays[stays.supplies_paid_by != "not charged"]
    sup = charged.groupby("supplies_paid_by").agg(supplies_stays=("unit", "size"),
                                                  supplies_cost=("stay_supplies_cost", "sum"))
    sup.index.name = "party"
    cc = cl.groupby(cl.paid_by).agg(cleans=("Amount", "size"), invoiced_cleans=("invoiced", "sum"),
                                    cleaning_cost=("Amount", "sum"))
    cc.index.name = "party"
    summary = st.join([cc, sup], how="outer").fillna(0).reset_index()
    summary["total_cost"] = summary.supplies_cost + summary.cleaning_cost
    summary.loc[len(summary)] = ["TOTAL", *summary.iloc[:, 1:].sum().tolist()]

    stays = stays.assign(_supplies_hoa=stays.stay_supplies_cost.where(stays.supplies_paid_by == "HOA", 0.0))
    ogrp = ["party", "ownership_type", "owner", "unit"]
    by_owner = stays.groupby(ogrp).agg(
        stays=("unit", "size"), nights=("stay_nights", "sum"), supplies_cost=("stay_supplies_cost", "sum"),
        supplies_hoa_paid=("_supplies_hoa", "sum")).join(
        cl.groupby(ogrp).agg(cleans=("Amount", "size"), cleaning_cost=("Amount", "sum"),
                             cleans_hoa_paid=("paid_by", lambda s: int((s == "HOA").sum())),
                             hoa_paid=("hoa_paid", "sum"),
                             cleans_owner_paid=("paid_by", lambda s: int((s != "HOA").sum())),
                             owner_paid=("owner_paid", "sum")), how="outer").fillna(0).reset_index()
    by_owner["supplies_not_charged"] = by_owner.supplies_cost - by_owner.supplies_hoa_paid
    by_owner["owner_total_cost"] = by_owner.owner_paid          # owners bear cleaning only; supplies are HOA's
    by_owner.loc[len(by_owner)] = ["TOTAL", "", "", "", *by_owner.iloc[:, 4:].sum().tolist()]

    def monthly(x: pd.DataFrame) -> pd.DataFrame:
        return x.groupby(["unit", "month"]).agg(
            cleans=("Amount", "size"), invoiced_cleans=("invoiced", "sum"), cleaning_cost=("Amount", "sum"),
            hoa_cleans=("paid_by", lambda s: int((s == "HOA").sum())), hoa_paid=("hoa_paid", "sum"),
            owner_cleans=("paid_by", lambda s: int((s != "HOA").sum())), owner_paid=("owner_paid", "sum"))

    whole_m = monthly(cl[cl.ownership_type == "Whole owner"]).reset_index()
    fr = cl[cl.week_start.notna()]
    weeks = (d[d.week_start.notna()].drop_duplicates(["unit", "week_start"])
             .assign(month=lambda x: x.week_end.dt.strftime("%Y-%m")).groupby(["unit", "month"]).size()
             .rename("fraction_weeks"))
    frac_m = weeks.to_frame().join(monthly(fr), how="outer").fillna(0).reset_index()

    alloc = cl[["row", "date", "unit", "clean_Invoice #", "Amount", "paid_by", "hoa_paid", "owner_paid",
                "party", "owner", "ownership_type", "week_start", "week_end", "stay_check_in", "stay_check_out",
                "stay_guest", "clean_match", "clean_Notes", "month"]].rename(
        columns={"row": "combined_row", "clean_Invoice #": "invoice", "paid_by": "cleaning_paid_by"})

    wk = d[d.week_start.notna()].drop_duplicates(["unit", "week_start"])
    exc = []
    def add(t, r, detail):
        exc.append({"type": t, "combined_row": r.row, "unit": r.unit,
                    "date": r.clean_Date if pd.notna(r.clean_Date) else r.stay_check_out, "detail": detail})
    for r in cl[(cl.paid_by != "HOA") & (cl.paid_by != cl.party)].itertuples():
        add("paid_by is neither HOA nor the row's party", r, f"paid_by {r.paid_by}, party {r.party}")
    for r in d[d.clean_cleaning_paid_by.isna() & (d.clean_Amount.fillna(0) > 0)].itertuples():
        add("clean amount with no cleaning_paid_by (not counted)", r, f"${r.clean_Amount:,.2f}")
    for r in stays[stays.clean_cleaning_paid_by.isna()].itertuples():
        add("stay with no clean", r, f"{r.stay_guest} {r.stay_check_in:%m/%d}-{r.stay_check_out:%m/%d}")
    h = cl[cl.paid_by == "HOA"]
    lim_w = hoa["free_cleans_per_month"]
    for (u, m), g in h[h.ownership_type == "Whole owner"].groupby(["unit", "month"]):
        if len(g) > lim_w:
            exc.append({"type": f"whole owner: more than {lim_w} HOA cleans in month", "combined_row": ", ".join(map(str, g.row)),
                        "unit": u, "date": m, "detail": f"{len(g)} HOA cleans"})
    lim_f = hoa["fraction_free_cleans_per_week"]
    for (u, w), g in h[h.week_start.notna()].groupby(["unit", "week_start"]):
        if len(g) > lim_f:
            exc.append({"type": f"fraction week: more than {lim_f} HOA clean", "combined_row": ", ".join(map(str, g.row)),
                        "unit": u, "date": w, "detail": f"{len(g)} HOA cleans"})
    hoa_weeks = set(zip(h.unit, h.week_start))
    for r in wk[[k not in hoa_weeks for k in zip(wk.unit, wk.week_start)]].itertuples():
        exc.append({"type": "fraction week with no HOA clean", "combined_row": r.row, "unit": r.unit,
                    "date": r.week_start, "detail": f"week {r.week_start:%m/%d}-{r.week_end:%m/%d} ({r.owner})"})

    if not stays.stay_supplies_cost.any():                     # nothing to report: drop the zero columns
        summary = summary.drop(columns=["supplies_stays", "supplies_cost", "total_cost"])
        by_owner = by_owner.drop(columns=["supplies_cost", "supplies_hoa_paid", "supplies_not_charged"])
    tabs = {}
    for party in INVOICE_PARTIES:                                 # cleaning only; supplies get their own tab
        lines = cl[(cl.paid_by == party) & (cl.Amount > 0)].copy()
        cols = {"date": "date", "month": "month", "unit": "unit", "owner": "owner",
                "ownership_type": "ownership_type", "week_start": "week_start", "week_end": "week_end",
                "clean_Invoice #": "aa_invoice", "stay_guest": "guest", "stay_check_in": "check_in",
                "stay_check_out": "check_out", "stay_nights": "nights", "stay_supply_guests": "guests",
                "Amount": "amount", "clean_Notes": "notes", "row": "combined_row"}
        inv = lines[[c for c in cols if c in lines]].rename(columns=cols).sort_values(
            ["unit", "date"], na_position="last").reset_index(drop=True)
        inv["combined_row"] = inv.combined_row.astype("Int64")
        inv.loc[len(inv)] = {"unit": "TOTAL", "amount": inv.amount.sum()}   # date left blank: keeps it a date
        tabs[f"Invoice - {party}"] = inv

    sup_lines = stays[(stays.supplies_paid_by == "HOA") & (stays.stay_supplies_cost > 0)].copy()
    sup_lines["date"] = sup_lines.stay_check_out
    sup_lines["month"] = sup_lines.stay_check_out.dt.strftime("%Y-%m")
    scols = {"date": "date", "month": "month", "unit": "unit", "owner": "owner", "ownership_type": "ownership_type",
             "week_start": "week_start", "week_end": "week_end", "stay_guest": "guest",
             "stay_check_in": "check_in", "stay_check_out": "check_out", "stay_nights": "nights",
             "stay_supply_guests": "guests", "stay_supplies_cost": "supplies_cost", "row": "combined_row"}
    sup_inv = sup_lines[[c for c in scols if c in sup_lines]].rename(columns=scols).sort_values(
        ["unit", "date"]).reset_index(drop=True)
    sup_inv["combined_row"] = sup_inv.combined_row.astype("Int64")
    sup_inv.loc[len(sup_inv)] = {"unit": "TOTAL", "guests": sup_inv.guests.sum(),
                                 "nights": sup_inv.nights.sum(), "supplies_cost": sup_inv.supplies_cost.sum()}
    tabs[SUPPLY_TAB] = sup_inv

    return {"Summary": summary, "By Owner": by_owner, "Whole Owner Cleans by Month": whole_m,
            "Fraction Cleans by Month": frac_m, **tabs,
            "Cleaning Allocation": alloc, "Exceptions": pd.DataFrame(exc)}


def write_tabs(path: Path, tabs: dict[str, pd.DataFrame]) -> None:
    """Replace the derived sheets in place, keeping their order. Other sheets are not touched."""
    wb = openpyxl.load_workbook(path)
    for name, df in tabs.items():
        pos = wb.sheetnames.index(name) if name in wb.sheetnames else len(wb.sheetnames)
        if name in wb.sheetnames:
            del wb[name]
        ws = wb.create_sheet(name, pos)
        ws.append(list(df.columns))
        for row in df.itertuples(index=False):
            vals = []
            for v in row:
                if isinstance(v, pd.Timestamp) or isinstance(v, datetime.datetime):
                    v = None if pd.isna(v) else v.date()
                elif not isinstance(v, str) and pd.isna(v):
                    v = None
                elif hasattr(v, "item"):
                    v = v.item()
                vals.append(v)
            ws.append(vals)
        ws.freeze_panes = "A2"
        for cell in ws[1]:
            cell.font = openpyxl.styles.Font(bold=True)
        for col in ws.columns:
            for c in col[1:]:
                if isinstance(c.value, datetime.date):
                    c.number_format = "yyyy-mm-dd"
            ws.column_dimensions[col[0].column_letter].width = min(45, max(10, *(len(str(c.value or "")) for c in col[:200])) + 2)
    order = ["Summary", "Invoice - HOA", SUPPLY_TAB,                         # summary, invoices, source, rest
             *(f"Invoice - {p}" for p in INVOICE_PARTIES if p != "HOA")]
    rest = [n for n in wb.sheetnames if n not in order]
    wb._sheets = [wb[n] for n in order if n in wb.sheetnames] + [wb[n] for n in rest]
    wb.active = 0
    wb.save(path)


def split_by_month(path: Path, cfg: dict) -> list[Path]:
    """Write one workbook per month, each with that month's source rows plus its own summaries.

    A row belongs to the month of its charge: the clean date, or the checkout / week end when there
    is no clean. Files are named <stem minus any trailing period>_<yyyymon>.xlsx next to the original,
    which is left alone.
    """
    sheets = pd.read_excel(path, sheet_name=None)
    name = source_sheet(sheets)
    raw = sheets[name]
    d, _, _ = read_combined(path, cfg)
    for col in ["stay_supply_guests", "stay_supplies_cost"]:      # carry them into each month's source tab
        if col not in raw:
            raw.insert(raw.columns.get_loc("clean_Date"), col, d[col].values)
    month = d.clean_Date.fillna(d.stay_check_out).fillna(d.week_end).dt.strftime("%Y-%m")
    stem = path.stem
    for suffix in ("_2026jul_aug", f"_{path.stem.split('_')[-1]}"):                # drop an old period suffix
        stem = stem.removesuffix(suffix)
    out = []
    for m in sorted(month.dropna().unique()):
        f = path.with_name(f"{stem}_{pd.Timestamp(m + '-01'):%Y%b}".lower() + ".xlsx")
        with pd.ExcelWriter(f, engine="openpyxl") as xw:
            raw[month == m].to_excel(xw, sheet_name=name, index=False)
        write_tabs(f, {name: raw[month == m]})                                     # re-write with date formats
        dm, stays, cl = read_combined(f, cfg)
        write_tabs(f, build_tabs(dm, stays, cl, cfg["hoa_cleaning"]))
        print(f"{f.name}: {int((month == m).sum())} rows, {len(cl)} cleans, ${cl.Amount.sum():,.2f}")
        out.append(f)
    return out


def main():
    cfg = yaml.safe_load((ROOT / "config" / "allocation_rules.yml").read_text())
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--file", default=str(ROOT / "output" / "yacinde_expense_allocation_20260701_20260915.xlsx"))
    ap.add_argument("--split-by-month", action="store_true",
                    help="write one workbook per month instead of summarising in place")
    args = ap.parse_args()
    path = Path(args.file)
    if args.split_by_month:
        split_by_month(path, cfg)
        return
    d, stays, cl = read_combined(path, cfg)
    tabs = build_tabs(d, stays, cl, cfg["hoa_cleaning"])
    write_tabs(path, tabs)
    pd.set_option("display.width", 200)
    print(f"{len(d)} rows, {len(stays)} stays, {len(cl)} cleans ({int(cl.invoiced.sum())} invoiced)")
    print(tabs["Summary"].to_string(index=False))
    for party in INVOICE_PARTIES:
        inv = tabs[f"Invoice - {party}"]
        print(f"Invoice - {party}: {len(inv) - 1} lines, ${inv.amount.iloc[-1]:,.2f}")
    sup = tabs[SUPPLY_TAB]
    print(f"{SUPPLY_TAB}: {len(sup) - 1} bookings, ${sup.supplies_cost.iloc[-1]:,.2f}")
    e = tabs["Exceptions"]
    print(e.type.value_counts().to_string() if len(e) else "no exceptions")
    print(f"-> {path}")


if __name__ == "__main__":
    main()
