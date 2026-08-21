"""Channel pricing calculator — "what does one accommodation fare net me on each channel?"

Builds ``output/channel_pricing_calculator.xlsx``: a LIVE-FORMULA workbook where the
owner types one accommodation fare (the Guesty/Wheelhouse listing price) plus cleaning /
add-ons / listing, and every channel row recomputes its markup, channel fee, Guesty fee,
Stripe fee, tax, guest price, host payout and NET RENTAL REVENUE — using the exact
waterfall of ``config/payment_structure.xlsx`` as transcribed in
``src/breakdown/payment_model.py``.

Two rate families feed it:
  * **Markup %** — Guesty ACCOUNT-level channel markups, pulled from the Open API
    (``GET /accounts/me`` -> ``markups``) and cached to ``config/_channel_markups.json``.
    Every listing has ``useAccountMarkups = true``, so these apply account-wide (14
    listings carry a stale per-listing ``markups`` block that Guesty ignores while that
    flag is on — verified against ~1,600 bookings).
  * **Channel-fee / Guesty / Stripe rates** — from payment_structure.xlsx (same constants
    as payment_model.RATE).

Usage (from the project root, venv active):

    python -m src.onetime.build_channel_calculator              # refresh markups from Guesty
    python -m src.onetime.build_channel_calculator --offline    # reuse the cached JSON

The Guesty pull is one GET on the cached 24h token (no extra token mint).
"""
import argparse
import json

import pandas as pd
from openpyxl import Workbook, load_workbook
from openpyxl.styles import Alignment, Border, Font, PatternFill, Side
from openpyxl.formatting.rule import ColorScaleRule
from openpyxl.worksheet.datavalidation import DataValidation

from .. import paths

# --- channel rows -----------------------------------------------------------
# key             : payment_model row_group
# markup_key      : key inside the Guesty account `markups` object
# cf              : channel-fee rate from payment_structure.xlsx (payment_model.RATE)
CHANNELS = [
    dict(key="airbnb", label="Airbnb", markup_key="airbnb2", cf=0.155,
         sources="Airbnb, Airbnb2",
         note="Airbnb collects & remits tax (not in InvoiceItem). Host-only fee 15.5% of the pre-tax subtotal."),
    dict(key="airbnb_hawaii", label="Airbnb — Hawaii", markup_key="airbnb2", cf=0.155,
         sources="Airbnb (HI listings)",
         note="Same as Airbnb but tax lands in the host ledger, so it sits inside InvoiceItem and is subtracted from net."),
    dict(key="homeaway", label="Vrbo / HomeAway", markup_key="homeaway2", cf=0.05,
         sources="VRBO, HomeAway CA/DE/UK, Vrbo Canada, expediaIntegrated",
         note="Both-sides fee: host 5% + guest service fee 10% on top. Guesty + Stripe processing apply."),
    dict(key="expedia_pcm", label="Expedia — virtual card", markup_key="expedia", cf=0.18,
         sources="Expedia paid by virtual card",
         note="Channel fee = (accommodation+markup)/82% x 18% (the host's amount is the 82% left after Expedia's cut). Fee is already inside InvoiceItem, so net does NOT subtract it again."),
    dict(key="expedia_grp", label="Expedia — group", markup_key="expedia", cf=0.18,
         sources="Expedia, Expedia Affiliate Network, Hotels.com, Travelocity, Orbitz, American Express Travel",
         note="Channel fee = (InvoiceItem - tax) x 18% and IS subtracted from net revenue."),
    dict(key="booking", label="Booking.com", markup_key="bookingCom", cf=0.15,
         sources="Booking.com",
         note="Host-only 15%. Guesty/Stripe do not process this channel."),
    dict(key="hopper", label="Hopper / Capital One", markup_key="hopper", cf=0.14,
         sources="Capital One, hopper, Hopper web/app",
         note="Host-only fee already deducted inside InvoiceItem; net does not subtract it again."),
    dict(key="manual", label="Direct / Manual", markup_key="manual", cf=0.0,
         sources="BE-API, Booking Engine, Direct, manual, owner, owner-guest, website",
         note="No channel commission. 'Channel fee' column = the direct-booking service fee (insurance) input B10. Guesty + Stripe apply."),
    dict(key="marriott", label="Homes & Villas by Marriott", markup_key="homesVillasByMarriott", cf=0.185,
         sources="homesVillasByMarriott",
         note="Fee = (InvoiceItem - tax + channel fee) x 18.5% = pre-tax subtotal x 18.5%; already inside InvoiceItem."),
    dict(key="tripcom", label="Trip.com", markup_key="tripCom", cf=0.111,
         sources="TripCom, Trip.com",
         note="RATE MISMATCH: payment_structure.xlsx row 11 says 15%; the live pipeline (payment_model.RATE) uses 11.1%. Rate cell D is editable — confirm which is current."),
    dict(key="whimstay", label="Whimstay", markup_key="whimstay", cf=0.05,
         sources="Whimstay",
         note="Fee = pre-tax subtotal x 5%; already inside InvoiceItem."),
    dict(key="blueground", label="bluegroundNestpick", markup_key="bluegroundNestpick", cf=0.07,
         sources="bluegroundNestpick",
         note="Fee = pre-tax subtotal x 7%; already inside InvoiceItem."),
    dict(key="bnbfinder", label="bnbFinder", markup_key="bnbFinder", cf=0.0,
         sources="bnbFinder",
         note="No channel commission, but Guesty 1% + Stripe processing apply (host payout is not reduced by them; net revenue is)."),
]

# Channels whose money runs through Guesty/Stripe (payment_model.has_processing).
PROCESSING = {"homeaway", "expedia_pcm", "expedia_grp", "manual", "bnbfinder"}

# Observed markup in real bookings (check-in May–Aug 2026 + all bookings created since
# 2026-06-20; pulled 2026-08-20, n≈2,400). Bookings keep the markup that was configured
# when they were made, so an older booking can carry a superseded rate.
OBSERVED = {
    "airbnb2": "25% (all bookings)",
    "homeaway2": "2% through Jul-2026; 8% on bookings created from Aug-2026",
    "bookingCom": "18% (all bookings)",
    "expedia": "21% (all bookings)",
    "hopper": "11% through Jul-2026; 14% appearing Aug-2026",
    "manual": "~3% (per-night rounding shows 3.0–3.5% of the fare)",
    "homesVillasByMarriott": "18% through Jul-2026; 28% Aug-2026",
    "tripCom": "18% (all bookings)",
    "whimstay": "5% (all bookings)",
    "bluegroundNestpick": "8% (1 older booking; config now 10%)",
    "bnbFinder": "0% (3 older bookings; config now 4%)",
}

# --- styling ---------------------------------------------------------------
MONEY = '#,##0.00'
PCT = '0.0%'
HDR_FILL = PatternFill("solid", fgColor="1F3864")
IN_FILL = PatternFill("solid", fgColor="FFF2CC")
CALC_FILL = PatternFill("solid", fgColor="F2F2F2")
NET_FILL = PatternFill("solid", fgColor="E2EFDA")
THIN = Side(style="thin", color="BFBFBF")
BOX = Border(left=THIN, right=THIN, top=THIN, bottom=THIN)


def fetch_markups(offline: bool) -> dict:
    """Return the Guesty account-level `markups` object (cached to config/)."""
    if offline:
        if not paths.CHANNEL_MARKUPS.exists():
            raise SystemExit(f"--offline but no cache at {paths.CHANNEL_MARKUPS}")
        return json.loads(paths.CHANNEL_MARKUPS.read_text())["markups"]
    from ..guesty.client import GuestyClient
    acct = GuestyClient().request("GET", "/accounts/me")
    payload = {
        "_source": "Guesty Open API GET /accounts/me -> markups (account level)",
        "accountId": acct.get("_id"),
        "accountName": acct.get("name"),
        "currency": acct.get("currency"),
        "markups": acct.get("markups") or {},
    }
    paths.CHANNEL_MARKUPS.write_text(json.dumps(payload, indent=2))
    print(f"markups cached -> {paths.CHANNEL_MARKUPS}")
    return payload["markups"]


def _rate(markups: dict, key: str):
    """Markup fraction for a Guesty channel key (PERCENTAGE units), or None."""
    m = markups.get(key)
    if not m:
        return None
    amt = float(m.get("amount", 0))
    return amt / 100.0 if str(m.get("units", "PERCENTAGE")).upper() == "PERCENTAGE" else amt


# --- per-channel formulas ---------------------------------------------------
# All reference: H = pre-tax subtotal (accommodation+markup+cleaning+add-ons),
# I = tax, J = channel fee, K = InvoiceItem, M = Guesty fee, N = Stripe fee.
def formulas(key: str, r: int) -> dict:
    H, I, J, K, E, F, M, N, D = (f"H{r}", f"I{r}", f"J{r}", f"K{r}", f"E{r}",
                                 f"F{r}", f"M{r}", f"N{r}", f"D{r}")
    # channel fee
    cf = {
        "expedia_pcm": f"=ROUND({E}/(1-{D})*{D},2)",
        "manual": "=$B$10",
        "bnbfinder": "=0",
    }.get(key, f"=ROUND({H}*{D},2)")
    # InvoiceItem
    ii = {
        "airbnb": f"={H}-{J}",                      # Airbnb remits tax -> not in II
        "airbnb_hawaii": f"={H}+{I}-{J}",
        "homeaway": f"={H}+{I}",
        "expedia_pcm": f"={H}+{I}-{J}",
        "expedia_grp": f"={H}+{I}",
        "booking": f"={H}+{I}",
        "hopper": f"={H}+{I}-{J}",
        "manual": f"={H}+{I}+{J}",                  # service fee is billed on top
        "marriott": f"={H}+{I}-{J}",
        "tripcom": f"={H}+{I}",
        "whimstay": f"={H}+{I}-{J}",
        "blueground": f"={H}+{I}-{J}",
        "bnbfinder": f"={H}+{I}",
    }[key]
    # guest pays
    guest = {
        "airbnb": f"={K}+{J}+{I}",
        "airbnb_hawaii": f"={K}+{J}+{I}",
        "homeaway": f"={K}+ROUND({H}*$B$14,2)",     # + guest service fee 10% of (II-tax)
        "expedia_pcm": f"={K}+{J}",
        "hopper": f"={K}+{J}",
        "marriott": f"={K}+{J}",
        "whimstay": f"={K}+{J}",
        "blueground": f"={K}+{J}",
    }.get(key, f"={K}")
    # host payout
    payout = {
        "booking": f"={K}-{J}",
        "homeaway": f"={K}-{N}-{M}",
        "expedia_pcm": f"={K}-{N}-{M}",
        "expedia_grp": f"={K}-{N}-{M}",
        "manual": f"={K}-{N}-{M}",
    }.get(key, f"={K}")
    # net rental revenue (cleaning is always pulled out — it is a separate owner credit)
    net = {
        "airbnb": f"={K}-{F}",
        "airbnb_hawaii": f"={K}-{F}-{I}",
        "homeaway": f"={K}-{F}-{I}-{J}-{N}-{M}",
        "expedia_pcm": f"={K}-{F}-{I}-{N}-{M}",
        "expedia_grp": f"={K}-{F}-{I}-{J}-{N}-{M}",
        "booking": f"={K}-{F}-{I}-{J}",
        "hopper": f"={K}-{F}-{I}",
        "manual": f"={K}-{F}-{I}-{J}-{N}-{M}",
        "marriott": f"={K}-{F}-{I}",
        "tripcom": f"={K}-{F}-{I}-{J}",
        "whimstay": f"={K}-{F}-{I}",
        "blueground": f"={K}-{F}-{I}",
        "bnbfinder": f"={K}-{F}-{I}-{M}-{N}",
    }[key]
    proc = key in PROCESSING
    return dict(
        accm=f"=ROUND($B$4*(1+C{r}),2)", clean="=$B$5", addon="=$B$6",
        sub=f"={E}+{F}+G{r}", tax=f"=ROUND({H}*$B$8,2)",
        cf=cf, ii=ii, guest=guest,
        guesty=(f"=ROUND({K}*$B$11,2)" if proc else "=0"),
        stripe=(f"=ROUND({K}*$B$12+$B$9*$B$13,2)" if proc else "=0"),
        payout=payout, net=net, pct=f'=IF(L{r}=0,"",P{r}/L{r})',
    )


COLS = [
    ("A", "Platform", 30), ("B", "Guesty markup key", 20), ("C", "Markup %", 10),
    ("D", "Channel fee rate", 11), ("E", "Accommodation + markup", 14),
    ("F", "Cleaning fee", 11), ("G", "Add-ons", 10), ("H", "Subtotal (pre-tax)", 13),
    ("I", "Tax", 11), ("J", "Channel fee", 12), ("K", "Invoice Item (Guesty)", 14),
    ("L", "Guest pays", 12), ("M", "Guesty fee", 11), ("N", "Stripe fee", 11),
    ("O", "Host payout", 12), ("P", "NET RENTAL REVENUE", 16),
    ("Q", "Net % of guest pay", 11), ("R", "Formula notes", 90),
]


def build_calculator(ws, markups: dict, first_row: int) -> None:
    """Write the header + one live-formula row per channel starting at `first_row`."""
    hdr = first_row - 1
    for col, title, width in COLS:
        c = ws[f"{col}{hdr}"]
        c.value = title
        c.font = Font(bold=True, color="FFFFFF", size=10)
        c.fill = HDR_FILL
        c.alignment = Alignment(wrap_text=True, vertical="center", horizontal="center")
        ws.column_dimensions[col].width = width
    ws.row_dimensions[hdr].height = 34

    for i, ch in enumerate(CHANNELS):
        r = first_row + i
        f = formulas(ch["key"], r)
        mk = _rate(markups, ch["markup_key"])
        ws[f"A{r}"] = ch["label"]
        ws[f"B{r}"] = ch["markup_key"]
        # Markup % points at the Guesty_Markups sheet so a config refresh flows through.
        ws[f"C{r}"] = (f"=IFERROR(VLOOKUP(B{r},Guesty_Markups!$A:$B,2,FALSE),0)"
                       if mk is not None else 0)
        ws[f"D{r}"] = ch["cf"]
        for col, key in (("E", "accm"), ("F", "clean"), ("G", "addon"), ("H", "sub"),
                         ("I", "tax"), ("J", "cf"), ("K", "ii"), ("L", "guest"),
                         ("M", "guesty"), ("N", "stripe"), ("O", "payout"),
                         ("P", "net"), ("Q", "pct")):
            ws[f"{col}{r}"] = f[key]
        ws[f"R{r}"] = ch["note"]
        for col, _t, _w in COLS:
            cell = ws[f"{col}{r}"]
            cell.border = BOX
            if col in ("C", "D", "Q"):
                cell.number_format = PCT
            elif col not in ("A", "B", "R"):
                cell.number_format = MONEY
            if col == "P":
                cell.fill = NET_FILL
                cell.font = Font(bold=True)
            elif col in ("E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O"):
                cell.fill = CALC_FILL
        ws[f"R{r}"].alignment = Alignment(wrap_text=True, vertical="top")
        ws.row_dimensions[r].height = 30


FOOTER = [
    "HOW TO READ THIS",
    "  * Markup % (col C) comes from the Guesty account settings (Guesty_Markups sheet) and applies to the ACCOMMODATION FARE ONLY.",
    "    'Accommodation + markup' (col E) is what the guest is quoted for the stay itself on that channel.",
    "  * NET RENTAL REVENUE (col P) is the owner-statement figure: it always excludes the cleaning fee (a separate, non-commissioned owner",
    "    credit) and excludes tax. It ties to Section 2 'Net Rental Revenue' on the owner statement — verified against the live pipeline",
    "    (src/breakdown/payment_model.compute_breakdown) for all 13 rows.",
    "  * Channel fee (col J) is the TRUE fee the channel charges = pre-tax subtotal x rate. For Airbnb / Hopper / Marriott / Whimstay /",
    "    Blueground the fee is already deducted before Guesty sees the money, so the statement's Section-1 'Channel Fee' column back-computes",
    "    it from the post-fee InvoiceItem and therefore prints a SMALLER number. InvoiceItem, host payout and net revenue are unaffected.",
    "  * Guesty 1% + Stripe 2.9%+$0.30 apply only where Guesty/Stripe process the payment: Vrbo, Expedia (both), Direct/Manual, bnbFinder.",
    "  * Trip.com: payment_structure.xlsx row 11 says a 15% channel fee, the live pipeline uses 11.1%. Cell D28 is editable — set the right one.",
    "  * Tax base = accommodation + markup + cleaning + add-ons. Per-listing taxability of cleaning/pet varies in Guesty, so treat tax as an",
    "    estimate for listings whose Guesty tax config excludes some fees.",
]


def build_footer(ws, first_row: int) -> None:
    """Explanatory notes under the waterfall table."""
    start = first_row + len(CHANNELS) + 1
    for i, line in enumerate(FOOTER):
        c = ws.cell(row=start + i, column=1, value=line)
        c.font = Font(bold=(i == 0), size=9,
                      color="1F3864" if i == 0 else "595959",
                      italic=(i != 0))


def build_inputs(ws, tax_df: pd.DataFrame) -> None:
    ws["A1"] = "Channel pricing calculator — one accommodation fare across every channel"
    ws["A1"].font = Font(bold=True, size=14, color="1F3864")
    ws["A2"] = ("Type your numbers in the yellow cells. Every grey/green cell is a live formula built from "
                "config/payment_structure.xlsx (see the Payment_Structure sheet) and the Guesty account markups "
                "(Guesty_Markups sheet).")
    ws["A2"].font = Font(italic=True, size=9)

    rows = [
        ("Accommodation fare (Guesty / Wheelhouse listing price, whole stay)", 1000.0, MONEY,
         "The base rate BEFORE any channel markup — 'Accommodation = listing price on Guesty/Wheelhouse'."),
        ("Cleaning fee", 150.0, MONEY, "Non-commissioned; pulled out of net rental revenue on every channel."),
        ("Add-ons (pet + extra guest - promotions/discounts)", 0.0, MONEY, "Enter net of discounts (can be negative)."),
        ("Listing (for the tax rate)", tax_df.iloc[0]["nickname"], None,
         "Pick a listing — the tax rate below looks itself up from config/_listing_tax_rates.csv."),
        ("Tax rate", None, '0.00%',
         "Auto-filled from the listing. Type over it to test another rate. Taxed base = accommodation + markup + cleaning + add-ons."),
        ("Stripe transactions (successful charges)", 2.0, '0',
         "Deposit + balance = 2; each charge carries its own $0.30."),
        ("Direct-booking service fee (insurance, $)", 0.0, MONEY,
         "Only used by the Direct/Manual row (payment_structure row 9: 'service fee - insurance')."),
        ("Guesty fee rate", 0.01, PCT, "InvoiceItem x 1% — Vrbo / Expedia / Direct / bnbFinder only."),
        ("Stripe % rate", 0.029, PCT, "InvoiceItem x 2.9% + transactions x $0.30."),
        ("Stripe per-transaction fee", 0.30, MONEY, ""),
        ("Vrbo guest service fee rate", 0.10, PCT, "Guest-side add-on: (InvoiceItem - tax) x 10%; raises guest price, not host revenue."),
    ]
    ws["A3"] = "INPUTS"
    ws["A3"].font = Font(bold=True, size=11, color="FFFFFF")
    ws["A3"].fill = HDR_FILL
    for i, (label, val, fmt, note) in enumerate(rows):
        r = 4 + i
        ws[f"A{r}"] = label
        ws[f"A{r}"].font = Font(bold=True, size=10)
        cell = ws[f"B{r}"]
        cell.value = val
        cell.fill = IN_FILL
        cell.border = BOX
        cell.font = Font(bold=True)
        if fmt:
            cell.number_format = fmt
        ws[f"C{r}"] = note
        ws[f"C{r}"].font = Font(italic=True, size=9, color="595959")
    # tax rate lookup + listing dropdown
    ws["B8"] = "=IFERROR(VLOOKUP($B$7,Tax_Rates!$A:$C,3,FALSE)/100,0)"
    dv = DataValidation(type="list", formula1=f"=Tax_Rates!$A$2:$A${len(tax_df) + 1}", allow_blank=True)
    ws.add_data_validation(dv)
    dv.add(ws["B7"])


def build_markups_sheet(ws, markups: dict) -> None:
    ws["A1"] = "Guesty channel key"
    ws["B1"] = "Markup (fraction)"
    ws["C1"] = "As set in Guesty"
    ws["D1"] = "Units / status"
    ws["E1"] = "Payment-structure platform(s)"
    ws["F1"] = "Observed in real bookings (pulled 2026-08-20)"
    for col in "ABCDEF":
        c = ws[f"{col}1"]
        c.font = Font(bold=True, color="FFFFFF")
        c.fill = HDR_FILL
        c.alignment = Alignment(wrap_text=True, vertical="center")
    ws.row_dimensions[1].height = 30
    plat = {}
    for ch in CHANNELS:
        plat.setdefault(ch["markup_key"], []).append(ch["label"])
    r = 2
    for key, m in markups.items():
        ws[f"A{r}"] = key
        ws[f"B{r}"] = _rate(markups, key)
        ws[f"B{r}"].number_format = PCT
        ws[f"C{r}"] = f'{m.get("amount")}%'
        ws[f"D{r}"] = f'{m.get("units", "")} / {m.get("status", "")}'
        ws[f"E{r}"] = ", ".join(plat.get(key, [])) or "— not used by any payment-structure row —"
        ws[f"F{r}"] = OBSERVED.get(key, "")
        for col in "ABCDEF":
            ws[f"{col}{r}"].border = BOX
        r += 1
    for col, w in zip("ABCDEF", (24, 16, 14, 26, 34, 56)):
        ws.column_dimensions[col].width = w
    notes = [
        "",
        "SOURCE: Guesty Open API — GET /accounts/me -> `markups` (ACCOUNT level). Cached to config/_channel_markups.json.",
        "The markup is applied to the ACCOMMODATION FARE ONLY (verified: the reservation's MAR invoice line = accommodation x rate, exactly).",
        "It is NOT applied to the cleaning fee or add-ons — but it IS inside the taxable base and inside every channel-fee base.",
        "All 132 listings have useAccountMarkups = true, so these account rates apply everywhere.",
        "14 listings (Seattle 7434 / 906 / 115 / 710 / 206 / 4201 / 8415, Bellevue 2323 Whole/Main/ADU, Bellevue 701, Bellevue 16237) carry a",
        "   per-listing `markups` block (airbnb2 0%, bookingCom 15%, homeaway2 10%, manual 5%, Marriott 18%). Guesty IGNORES it while",
        "   useAccountMarkups is true — confirmed against their bookings, which all carry the account rates.",
        "Bookings keep the markup that was configured when they were MADE, so historical statements can show a superseded rate (column F).",
        "To refresh: python -m src.onetime.build_channel_calculator",
    ]
    for i, n in enumerate(notes):
        c = ws.cell(row=r + i, column=1, value=n)
        c.font = Font(italic=True, size=9, color="595959")


def build_structure_sheet(ws) -> None:
    """Copy config/payment_structure.xlsx verbatim so the workbook is self-documenting."""
    src = load_workbook(paths.PAYMENT_STRUCTURE)[
        load_workbook(paths.PAYMENT_STRUCTURE).sheetnames[0]]
    for row in src.iter_rows():
        for cell in row:
            if cell.value is not None:
                t = ws.cell(row=cell.row, column=cell.column, value=cell.value)
                t.alignment = Alignment(wrap_text=True, vertical="top")
                t.font = Font(size=9, bold=cell.row == 1)
                if cell.row == 1:
                    t.fill = HDR_FILL
                    t.font = Font(size=9, bold=True, color="FFFFFF")
    for col, w in zip("ABCDEFGHIJK", (22, 26, 42, 30, 10, 30, 16, 24, 26, 46, 26)):
        ws.column_dimensions[col].width = w


def build_tax_sheet(ws, tax_df: pd.DataFrame) -> None:
    ws.append(["nickname", "listingId", "tax_rate_pct", "useAccountTaxes", "n_taxes"])
    for c in "ABCDE":
        ws[f"{c}1"].font = Font(bold=True, color="FFFFFF")
        ws[f"{c}1"].fill = HDR_FILL
    for _, row in tax_df.iterrows():
        ws.append([row["nickname"], row["listingId"], float(row["tax_rate_pct"]),
                   bool(row["useAccountTaxes"]), int(row["n_taxes"])])
    for col, w in zip("ABCDE", (30, 28, 13, 16, 10)):
        ws.column_dimensions[col].width = w


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--offline", action="store_true",
                    help="reuse config/_channel_markups.json instead of calling Guesty")
    ap.add_argument("--out", default=str(paths.CHANNEL_CALCULATOR))
    args = ap.parse_args()

    markups = fetch_markups(args.offline)
    tax_df = pd.read_csv(paths.LISTING_TAX_RATES).sort_values("nickname").reset_index(drop=True)

    wb = Workbook()
    calc = wb.active
    calc.title = "Calculator"
    build_inputs(calc, tax_df)
    build_calculator(calc, markups, first_row=19)
    build_footer(calc, first_row=19)
    last = 19 + len(CHANNELS) - 1
    calc.conditional_formatting.add(
        f"P19:P{last}",
        ColorScaleRule(start_type="min", start_color="F8696B",
                       mid_type="percentile", mid_value=50, mid_color="FFEB84",
                       end_type="max", end_color="63BE7B"))
    calc.freeze_panes = "A19"
    calc["A17"] = "PER-CHANNEL WATERFALL  (every cell below is a formula — change an input and this recalculates)"
    calc["A17"].font = Font(bold=True, size=11, color="1F3864")

    build_markups_sheet(wb.create_sheet("Guesty_Markups"), markups)
    build_structure_sheet(wb.create_sheet("Payment_Structure"))
    build_tax_sheet(wb.create_sheet("Tax_Rates"), tax_df)

    out = paths.PROJECT_ROOT / args.out if not str(args.out).startswith("/") else args.out
    wb.save(out)
    print(f"wrote {out}")


if __name__ == "__main__":
    main()
