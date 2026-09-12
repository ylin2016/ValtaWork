from pathlib import Path
from datetime import datetime
from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment
from openpyxl.comments import Comment

from .booking_breakdown import natural_key

# ── palette ──────────────────────────────────────────────────────────────────
_DARK_BLUE  = "1F4E79"
_MED_BLUE   = "2E74B5"
_GREEN_FILL = "E2EFDA"
_ALT_FILL   = "F5F5F5"
_WHITE      = "FFFFFF"

_SECTION_FILL = PatternFill("solid", fgColor=_DARK_BLUE)
_COL_HDR_FILL = PatternFill("solid", fgColor=_MED_BLUE)
_TOTAL_FILL   = PatternFill("solid", fgColor=_GREEN_FILL)
_GRAND_FILL   = PatternFill("solid", fgColor="CFE0C3")  # darker green for the grand Total Net Revenue
_ALT          = PatternFill("solid", fgColor=_ALT_FILL)


CURRENCY = '$#,##0.00'
FONT_SIZE = 14

# ── helpers ───────────────────────────────────────────────────────────────────
def _cell(ws, row, col, value=None, bold=False, size=FONT_SIZE, color=None,
          bg=None, fmt=None, align="left", wrap=False, merge_to=None):
    c = ws.cell(row=row, column=col, value=value)
    kw = {"bold": bold, "size": size}
    if color:
        kw["color"] = color
    c.font = Font(**kw)
    if bg:
        c.fill = PatternFill("solid", fgColor=bg) if isinstance(bg, str) else bg
    if fmt:
        c.number_format = fmt
    c.alignment = Alignment(horizontal=align, vertical="center", wrap_text=wrap)
    if merge_to and merge_to > col:
        ws.merge_cells(start_row=row, start_column=col, end_row=row, end_column=merge_to)
    return c

def _section_row(ws, row, title, ncols):
    ws.row_dimensions[row].height = 18
    c = ws.cell(row=row, column=1, value=title)
    c.font = Font(bold=True, color=_WHITE, size=FONT_SIZE)
    c.fill = _SECTION_FILL
    c.alignment = Alignment(horizontal="left", vertical="center", indent=1)
    ws.merge_cells(start_row=row, start_column=1, end_row=row, end_column=ncols)
    return row + 1

def _col_header_row(ws, row, headers, ncols):
    ws.row_dimensions[row].height = 16
    for i in range(1, ncols + 1):
        c = ws.cell(row=row, column=i)
        if i <= len(headers):
            c.value = headers[i - 1]
        c.font = Font(bold=True, color=_WHITE, size=FONT_SIZE)
        c.fill = _COL_HDR_FILL
        c.alignment = Alignment(horizontal="center", vertical="center")
    return row + 1

def _total_row(ws, row, label, amounts_by_col, ncols):
    ws.row_dimensions[row].height = 16
    for col in range(1, ncols + 1):
        ws.cell(row=row, column=col).fill = _TOTAL_FILL
    _cell(ws, row, 1, label, bold=True, bg=_TOTAL_FILL)
    for col, val in amounts_by_col.items():
        _cell(ws, row, col, val, bold=True, fmt=CURRENCY, align="right", bg=_TOTAL_FILL)
    return row + 1

def _spacer(ws, row, height=6):
    ws.row_dimensions[row].height = height
    return row + 1

# Excel Booking Breakdown columns: (header, row-key). The dashboard/PDF fold channel/Guesty/
# Stripe into one "Fees" column; Excel splits them into the three components the rows already
# carry (_ch/_gu/_st, per reporting.booking_breakdown.FEE_PARTS). Everything else matches
# booking_breakdown.DISP.
_BD_XLS_COLS = [
    ("Conf Code", "Conf Code"),
    ("Bookings", "Bookings"),
    ("Guest Pay", "Guest Pay"),
    ("Accommodation", "Accommodation"),
    ("Markup", "Markup"),
    ("Add-ons", "Add-ons"),
    ("Channel Fee", "_ch"),
    ("Guesty Fee", "_gu"),
    ("Stripe Fee", "_st"),
    # Cohost handling fee out of an Airbnb Resolution Center line (payment_model.
    # ARC_NOT_OWNER_INCOME) — $0.00 on every ordinary booking, but it has to have a
    # column of its own or the Excel fee split stops adding up to "Fees".
    ("Cohost Handling", "_ar"),
    ("Cleaning Fee", "Cleaning Fee"),
    ("Tax", "Tax"),
    ("Net Rental Revenue", "Net Rental Revenue"),
]
_BD_XLS_NUMKEYS = {"Guest Pay", "Accommodation", "Markup", "Add-ons",
                   "_ch", "_gu", "_st", "Cleaning Fee", "Tax", "Net Rental Revenue"}


def _write_breakdown(ws, row, unit_rows, title="Booking Breakdown", ncols=None):
    """Render a Section-1 Booking Breakdown sub-table with a highlighted TOTAL row. The folded
    "Fees" column is split into Channel Fee / Guesty Fee / Stripe Fee (Excel-only); all other
    columns come from reporting.booking_breakdown so the figures match the dashboard/PDF."""
    ncols = len(_BD_XLS_COLS)
    headers = [h for h, _ in _BD_XLS_COLS]
    row = _section_row(ws, row, title, ncols)
    row = _col_header_row(ws, row, headers, ncols)
    for i, r in enumerate(unit_rows):
        is_total = bool(r.get("_total"))
        bg = _TOTAL_FILL if is_total else (_ALT if i % 2 else None)
        ws.row_dimensions[row].height = 15
        if is_total:
            for c in range(1, ncols + 1):
                ws.cell(row=row, column=c).fill = _TOTAL_FILL
        for col, (_hdr, key) in enumerate(_BD_XLS_COLS, start=1):
            v = r.get(key, "")
            if key in _BD_XLS_NUMKEYS:
                if v is None:
                    _cell(ws, row, col, "", bold=is_total, bg=bg, align="right")
                else:
                    _cell(ws, row, col, round(float(v or 0), 2), bold=is_total, bg=bg,
                          fmt=CURRENCY, align="right")
                    # Excel has no hover, so name the add-on in a real cell note —
                    # "Add-ons $50.00" alone does not tell the owner what it was.
                    if key == "Add-ons" and str(r.get("_addon_detail") or ""):
                        ws.cell(row=row, column=col).comment = Comment(
                            str(r["_addon_detail"]), "Valta Realty")
            else:
                _cell(ws, row, col, str(v), bold=is_total, bg=bg,
                      align=("center" if key == "Conf Code" else "left"))
        row += 1
    return _spacer(ws, row)

def _fmt_dates(checkin_str, checkout_str):
    try:
        ci = datetime.strptime(str(checkin_str)[:10], "%Y-%m-%d")
    except Exception:
        return str(checkin_str or "")
    if checkout_str:
        try:
            co = datetime.strptime(str(checkout_str)[:10], "%Y-%m-%d")
            nights = (co - ci).days
            # Same-day credit entries (e.g. OSBR-RV Hipcamp bookings) show a single date.
            if nights == 0:
                return co.strftime("%d. %b. %Y")
            return f"{ci.strftime('%d. %b.')} - {co.strftime('%d. %b. %Y')} / {nights} nights"
        except Exception:
            pass
    return ci.strftime("%d. %b. %Y")


def _nights_between(checkin_str, checkout_str):
    try:
        ci = datetime.strptime(str(checkin_str)[:10], "%Y-%m-%d")
        co = datetime.strptime(str(checkout_str)[:10], "%Y-%m-%d")
        return max(0, (co - ci).days)
    except Exception:
        return 0

# ── legacy template creator (used by init-db) ────────────────────────────────
def create_template(path: str):
    wb = Workbook()
    ws = wb.active
    ws.title = "Setup"
    for i, (k, v) in enumerate([
        ("Statement Period Start", ""), ("Statement Period End", ""),
        ("Property ID", ""), ("Property Name", ""), ("Owner Name", ""),
        ("Owner Email", ""), ("QBO Class ID", ""), ("Statement Version", "v1"),
    ], start=1):
        ws[f"A{i}"] = k
        ws[f"B{i}"] = v
    ws.column_dimensions["A"].width = 28
    ws.column_dimensions["B"].width = 40
    wb.save(path)

# ── main statement writer ────────────────────────────────────────────────────
def write_statement(output_path: str, period: str, property_info: dict,
                    owner_info: dict, pm_fee_rate: float, bookings: list,
                    other_income: list, expenses_by_subcat: dict, totals: dict,
                    owner_pays_cleaning: bool = False,
                    owner_pays_supplies: bool = False,
                    owner_pays_taxes: bool = False,
                    property_names: dict = None,
                    multi_listing: bool = False,
                    booking_breakdown: dict = None,
                    booking_breakdown_grand: dict = None):
    """
    Generate a single-sheet owner statement matching the PDF format.

    bookings: list of dicts with keys:
      - booking_id, guest_name, checkin, checkout, net_revenue
      - owner_cleaning_cost, owner_tax_cost (negative values)
      - commission (optional; per-ledger-line rounded — see booking_breakdown)
    """
    wb = Workbook()
    ws = wb.active
    ws.title = "Statement"

    # Determine number of columns based on owner flags
    ncols_revenue = 7  # Dates, Code, Guest, Net Rental Revenue, Management Commission, Net Owner Proceeds, Commission %
    if owner_pays_cleaning:
        ncols_revenue += 1
    if owner_pays_taxes:
        ncols_revenue += 1

    # Column widths
    ws.column_dimensions["A"].width = 18
    ws.column_dimensions["B"].width = 15
    ws.column_dimensions["C"].width = 28
    ws.column_dimensions["D"].width = 18
    ws.column_dimensions["E"].width = 18
    ws.column_dimensions["F"].width = 18
    ws.column_dimensions["G"].width = 18

    period_dt    = datetime.strptime(period, "%Y-%m")
    period_label = period_dt.strftime("%B %Y")
    starting_bal = float(totals.get("starting_balance", 0))
    net_income   = float(totals.get("net_income", 0))
    current_bal  = starting_bal + net_income

    row = 1

    # ── HEADER ────────────────────────────────────────────────────────────────
    ws.row_dimensions[row].height = 22
    _cell(ws, row, 1, "Valta Realty", bold=True, size=16)
    _cell(ws, row, 5, f"{period_label} - Summary",
          bold=True, color=_WHITE, size=14, bg=_DARK_BLUE, align="center", merge_to=7)
    row += 1

    _cell(ws, row, 1, "4027 Beach Drive Southwest, Seattle, WA 98116", size=FONT_SIZE, color="595959")
    _cell(ws, row, 5, "Starting Balance", size=FONT_SIZE)
    _cell(ws, row, 7, starting_bal, size=FONT_SIZE, fmt=CURRENCY, align="right")
    row += 1

    _cell(ws, row, 1, "contact@valtarealty.com", size=FONT_SIZE, color="595959")
    _cell(ws, row, 5, "Net Income", bold=True, size=FONT_SIZE)
    _cell(ws, row, 7, net_income, bold=True, size=FONT_SIZE, fmt=CURRENCY, align="right")
    row += 1

    _cell(ws, row, 5, "Current Balance", size=FONT_SIZE)
    _cell(ws, row, 7, current_bal, size=FONT_SIZE, fmt=CURRENCY, align="right")
    row += 1

    ws.row_dimensions[row].height = 18
    _cell(ws, row, 1, property_info.get("property_name", ""), bold=True, size=14, merge_to=4)
    _cell(ws, row, 5, "Owner Payout", size=FONT_SIZE)
    _cell(ws, row, 7, net_income, size=FONT_SIZE, fmt=CURRENCY, align="right")
    row += 1

    _cell(ws, row, 1, "Owner", bold=True, size=FONT_SIZE, color="595959")
    _cell(ws, row, 2, owner_info.get("owner_name", ""), size=FONT_SIZE)
    _cell(ws, row, 5, "Ending Balance", bold=True, size=FONT_SIZE)
    _cell(ws, row, 7, current_bal - net_income, bold=True, size=FONT_SIZE, fmt=CURRENCY, align="right")
    row += 1

    _cell(ws, row, 1, "Email", bold=True, size=FONT_SIZE, color="595959")
    _cell(ws, row, 2, owner_info.get("owner_email", ""), size=FONT_SIZE)
    row += 1

    row = _spacer(ws, row)

    # ── NET REVENUE SECTION (one sub-table per listing) ───────────────────────
    if bookings:
        row = _section_row(ws, row, "Net Revenue", ncols_revenue)

        headers = ["Reservation Dates", "Confirmation Code", "Guest Name", "Net Rental Revenue"]
        if owner_pays_cleaning:
            headers.append("Owner Cleaning Fee")
        headers.append("Management Commission")
        if owner_pays_taxes:
            headers.append("Tax Paid to Owner")  # after MCR, per owner request
        headers.extend(["Net Owner Proceeds", "Commission %"])

        names = property_names or {}

        def _nr_booking_row(r, b, bg):
            # Round each figure to cents at source so totals equal the sum of shown cells.
            net_rental = round(float(b.get("net_revenue") or 0), 2)
            cleaning_fee = round(float(b.get("owner_cleaning_cost") or 0), 2) if owner_pays_cleaning else 0.0
            tax_paid = round(float(b.get("owner_tax_cost") or 0), 2) if owner_pays_taxes else 0.0
            # Prefer the commission the caller computed: booking_breakdown.net_revenue_rows
            # rounds it PER LEDGER LINE (a booking plus any merged add-on is two lines),
            # exactly as netrevenue.engine sums the stored PM fee. Recomputing it here from
            # the merged net rounds once and drifts a penny (elektra_1413 2026-07).
            comm = (round(float(b["commission"]), 2) if b.get("commission") is not None
                    else round(-net_rental * pm_fee_rate, 2))
            owner_rev = round(net_rental + cleaning_fee + tax_paid + comm, 2)
            col = 1
            _cell(ws, r, col, _fmt_dates(b["checkin"], b["checkout"]), bg=bg); col += 1
            _cell(ws, r, col, b["booking_id"] or "", bg=bg, align="center"); col += 1
            _cell(ws, r, col, b["guest_name"] or "", bg=bg); col += 1
            _cell(ws, r, col, net_rental, fmt=CURRENCY, align="right", bg=bg); col += 1
            if owner_pays_cleaning:
                _cell(ws, r, col, cleaning_fee, fmt=CURRENCY, align="right", bg=bg); col += 1
            _cell(ws, r, col, comm, fmt=CURRENCY, align="right", bg=bg); col += 1
            if owner_pays_taxes:
                _cell(ws, r, col, tax_paid, fmt=CURRENCY, align="right", bg=bg); col += 1
            _cell(ws, r, col, owner_rev, fmt=CURRENCY, align="right", bg=bg); col += 1
            _cell(ws, r, col, -pm_fee_rate, fmt="0%", align="center", bg=bg)
            return net_rental, cleaning_fee, tax_paid, comm, owner_rev, _nights_between(b["checkin"], b["checkout"])

        def _nr_total_row(r, nights, net, clean, tax, comm, owner, fill):
            ws.row_dimensions[r].height = 16
            for c in range(1, ncols_revenue + 1):
                ws.cell(row=r, column=c).fill = fill
            _cell(ws, r, 1, f"{int(nights)} nights", bold=True, bg=fill)
            col = 4
            _cell(ws, r, col, net, bold=True, fmt=CURRENCY, align="right", bg=fill); col += 1
            if owner_pays_cleaning:
                _cell(ws, r, col, clean, bold=True, fmt=CURRENCY, align="right", bg=fill); col += 1
            _cell(ws, r, col, comm, bold=True, fmt=CURRENCY, align="right", bg=fill); col += 1
            if owner_pays_taxes:
                _cell(ws, r, col, tax, bold=True, fmt=CURRENCY, align="right", bg=fill); col += 1
            _cell(ws, r, col, owner, bold=True, fmt=CURRENCY, align="right", bg=fill); col += 1
            _cell(ws, r, col, -pm_fee_rate, bold=True, fmt="0%", align="center", bg=fill)

        from collections import OrderedDict
        groups = OrderedDict()
        for b in bookings:
            groups.setdefault(b.get("property_id"), []).append(b)
        # Render units in natural listing order (osbr_2 before osbr_10, osbr_rv last),
        # matching the Booking-Breakdown section above so the two never drift.
        groups = OrderedDict(sorted(groups.items(), key=lambda kv: natural_key(kv[0])))
        multi = len(groups) > 1

        gt_net = gt_clean = gt_tax = gt_comm = gt_owner = 0.0
        gt_nights = 0

        for gpid, gb in groups.items():
            if multi:
                row = _section_row(ws, row, names.get(gpid, gpid or ""), ncols_revenue)
            # Booking Breakdown for this unit, directly above its Net Revenue table.
            if booking_breakdown and gpid in booking_breakdown:
                row = _write_breakdown(ws, row, booking_breakdown[gpid])
            row = _col_header_row(ws, row, headers, ncols_revenue)
            s_net = s_clean = s_tax = s_comm = s_owner = 0.0
            s_nights = 0
            for i, b in enumerate(gb):
                ws.row_dimensions[row].height = 15
                bg = _ALT if i % 2 else None
                n, cl, tx, cm, ow, ni = _nr_booking_row(row, b, bg)
                s_net += n; s_clean += cl; s_tax += tx; s_comm += cm; s_owner += ow; s_nights += ni
                row += 1
            _nr_total_row(row, s_nights, s_net, s_clean, s_tax, s_comm, s_owner, _TOTAL_FILL)
            row += 1
            gt_net += s_net; gt_clean += s_clean; gt_tax += s_tax
            gt_comm += s_comm; gt_owner += s_owner; gt_nights += s_nights
            row = _spacer(ws, row)

        # Grand total across all listings (only meaningful for multi-listing/rollups).
        if multi:
            if booking_breakdown_grand:
                row = _write_breakdown(ws, row, [booking_breakdown_grand],
                                       title="Booking Breakdown — Total (All Units)")
            row = _section_row(ws, row, "Total Net Revenue", ncols_revenue)
            _nr_total_row(row, gt_nights, gt_net, gt_clean, gt_tax, gt_comm, gt_owner, _GRAND_FILL)
            row += 1
            row = _spacer(ws, row)

    # ── OTHER INCOME ──────────────────────────────────────────────────────────
    if other_income:
        row = _section_row(ws, row, "Other Credits", 7)
        # Rollup statements show which listing each credit belongs to; single omit it.
        amt_col = 4 if multi_listing else 3
        oi_headers = (["Date", "Description", "Listing", "Amount"] if multi_listing
                      else ["Date", "Description", "Amount"])
        row = _col_header_row(ws, row, oi_headers, 7)

        total_oi = 0.0
        for i, oi in enumerate(other_income):
            ws.row_dimensions[row].height = 15
            bg = _ALT if i % 2 else None
            amt = round(float(oi["amount"] or 0), 2)
            _cell(ws, row, 1, oi["posting_date"], bg=bg)
            _cell(ws, row, 2, (oi["description"] or "")[:200], bg=bg, wrap=True)
            if multi_listing:
                _cell(ws, row, 3, (property_names or {}).get(oi["property_id"], oi["property_id"] or ""), bg=bg)
            _cell(ws, row, amt_col, amt, fmt=CURRENCY, align="right", bg=bg)
            total_oi += amt
            row += 1

        row = _total_row(ws, row, "Total:", {amt_col: total_oi}, 7)
        row = _spacer(ws, row)

    # ── EXPENSE SECTIONS ──────────────────────────────────────────────────────
    ORDER = ["Repairs", "Maintenance", "Cleaning Labor", "Supplies", "Utilities",
             "Internet/Cable", "HOA", "Insurance", "Property Taxes", "Other Expense"]

    def _sort_key(s):
        try:
            return ORDER.index(s)
        except ValueError:
            return len(ORDER)

    for subcat in sorted(expenses_by_subcat.keys(), key=_sort_key):
        lines = expenses_by_subcat[subcat]
        if not lines:
            continue

        # Skip supplies for owner-paid properties
        if subcat == "Supplies" and owner_pays_supplies:
            continue

        row = _section_row(ws, row, subcat, 7)

        ws.row_dimensions[row].height = 16
        for col in range(1, 8):
            c = ws.cell(row=row, column=col)
            c.fill = _COL_HDR_FILL
            c.font = Font(bold=True, color=_WHITE, size=FONT_SIZE)
            c.alignment = Alignment(horizontal="center", vertical="center")
        ws.cell(row=row, column=1).value = "Date"
        if multi_listing:
            # Rollup statements show which listing each cost belongs to.
            ws.merge_cells(start_row=row, start_column=2, end_row=row, end_column=3)
            ws.cell(row=row, column=2).value = "Description"
            ws.merge_cells(start_row=row, start_column=4, end_row=row, end_column=5)
            ws.cell(row=row, column=4).value = "Listing"
        else:
            ws.merge_cells(start_row=row, start_column=2, end_row=row, end_column=6)
            ws.cell(row=row, column=2).value = "Description"
        ws.cell(row=row, column=7).value = "Amount"
        row += 1

        total_exp = 0.0
        for i, exp in enumerate(lines):
            ws.row_dimensions[row].height = 15
            bg = _ALT if i % 2 else None
            amt = round(float(exp["amount"] or 0), 2)

            _cell(ws, row, 1, exp["posting_date"], bg=bg)

            desc_end = 3 if multi_listing else 6
            ws.merge_cells(start_row=row, start_column=2, end_row=row, end_column=desc_end)
            c = ws.cell(row=row, column=2)
            c.value = (exp["description"] or "")[:200]
            c.font = Font(size=FONT_SIZE)
            c.alignment = Alignment(wrap_text=True, vertical="center")
            if bg:
                c.fill = bg

            if multi_listing:
                # 'Listing' shows which unit each cost belongs to (per-unit on rollups).
                vendor_type = (property_names or {}).get(exp["property_id"], exp["property_id"] or "")
                ws.merge_cells(start_row=row, start_column=4, end_row=row, end_column=5)
                c = ws.cell(row=row, column=4)
                c.value = vendor_type
                c.font = Font(size=FONT_SIZE)
                c.alignment = Alignment(vertical="center")
                if bg:
                    c.fill = bg
                if bg:
                    ws.cell(row=row, column=6).fill = bg

            _cell(ws, row, 7, amt, fmt=CURRENCY, align="right", bg=bg)
            total_exp += amt
            row += 1

        # Total row
        ws.row_dimensions[row].height = 16
        for col in range(1, 8):
            ws.cell(row=row, column=col).fill = _TOTAL_FILL
        _cell(ws, row, 1, "Total:", bold=True, bg=_TOTAL_FILL)
        _cell(ws, row, 7, total_exp, bold=True, fmt=CURRENCY, align="right", bg=_TOTAL_FILL)
        row += 1
        row = _spacer(ws, row)

    Path(output_path).parent.mkdir(parents=True, exist_ok=True)
    wb.save(output_path)
