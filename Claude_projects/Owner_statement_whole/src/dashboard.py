"""
Web dashboard for owner statements with property selector and PDF export.
"""
import streamlit as st
from pathlib import Path
import sqlite3
from datetime import datetime
import pandas as pd
from openpyxl import load_workbook
from reportlab.lib.pagesizes import letter, A4
from reportlab.lib import colors
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import inch
from reportlab.platypus import SimpleDocTemplate, Table, TableStyle, Paragraph, Spacer, PageBreak
from reportlab.lib.enums import TA_RIGHT, TA_CENTER, TA_LEFT
import io
from html import escape as _esc
import sys, os
sys.path.insert(0, os.path.dirname(__file__))
import paths
from ltr.records import (build_records as ltr_build_records,
                         is_rent_income as ltr_is_rent_income,
                         ltr_claimed_codes)
from reporting.booking_breakdown import (build_by_unit as _bd_build, natural_key as _natural_key,
                                         DISP as _pb_disp, NUM as _pb_num, FEE_PARTS as _fee_parts)
from scope.listing_filter import allowed_property_ids
from scope.pm_rate import resolve_pm_fee_rate

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

BASE = Path(__file__).parent.parent
DB_PATH = paths.DB_PATH
STATEMENTS_DIR = paths.OUTPUT_DIR

# ─────────────────────────────────────────────────────────────────────────────
# Page Setup
# ─────────────────────────────────────────────────────────────────────────────

st.set_page_config(
    page_title="Owner Statements",
    page_icon="📊",
    layout="wide",
    initial_sidebar_state="expanded"
)

st.markdown("""
<style>
    [data-testid="stSidebar"] { background-color: #f5f5f5; }
    .main { padding: 2rem; }
    h1 { color: #1f4e79; margin-bottom: 1rem; }
    h2 { color: #2e74b5; margin-top: 1.5rem; margin-bottom: 0.5rem; }
    .summary-card { background: #bdd7ee; padding: 1rem; border-radius: 0.5rem; margin: 0.5rem 0; }
    .metric-row { display: flex; gap: 2rem; margin: 1rem 0; flex-wrap: wrap; }
    .metric { flex: 1; min-width: 200px; }
    .metric-label { font-size: 0.9rem; color: #666; }
    .metric-value { font-size: 1.5rem; font-weight: bold; color: #1f4e79; }
    table { font-size: 0.95rem; }
    /* Section-1 Booking Breakdown: static full-width table, wrapping headers,
       and a hover tooltip on the combined Fees cell. table-layout:fixed +
       colgroup so the detail table (inside the expander) and the always-visible
       total table (below it) line up column-for-column. */
    .pb-table { width: 100%; border-collapse: collapse; font-size: 0.9rem; margin-bottom: 0.5rem; table-layout: fixed; }
    .pb-table th, .pb-table td { border: 1px solid #d9d9d9; padding: 4px 8px; white-space: normal; overflow-wrap: anywhere; vertical-align: top; }
    .pb-table th { background: #f2f2f2; }
    .pb-fees { border-bottom: 1px dotted #888; cursor: help; }
</style>
""", unsafe_allow_html=True)

# ─────────────────────────────────────────────────────────────────────────────
# Data Loading
# ─────────────────────────────────────────────────────────────────────────────

@st.cache_data
@st.cache_data
def load_mapping_with_addresses():
    """Load property addresses and flags from mapping_classes.yml."""
    import yaml
    mapping_path = paths.MAPPING_CLASSES
    addresses = {}
    owner_flags = {}

    if mapping_path.exists():
        with open(mapping_path, 'r') as f:
            items = yaml.safe_load(f)
            for item in items or []:
                prop_id = item.get('property_id', '')
                if prop_id:
                    # Tolerate key-casing variants ('address' vs 'Address') so a
                    # stray capital never blanks an address to N/A.
                    address = item.get('address') or item.get('Address') or ''
                    if address:
                        addresses[prop_id] = address
                    owner_flags[prop_id] = {
                        'owner_pays_cleaning': item.get('owner_pays_cleaning', False),
                        'owner_pays_taxes': item.get('owner_pays_taxes', False),
                        'owner_pays_supplies': item.get('owner_pays_supplies', False),
                    }

    return addresses, owner_flags

@st.cache_data
def load_rollups():
    """Load statement_rollups from config.yml: child property_id -> parent, and parent -> [members]."""
    import yaml
    cfg_path = paths.CONFIG_YML
    rollups = {}
    if cfg_path.exists():
        with open(cfg_path, 'r') as f:
            cfg = yaml.safe_load(f) or {}
            rollups = cfg.get('statement_rollups') or {}
    children = {child for kids in rollups.values() for child in kids}
    return rollups, children

@st.cache_data
def load_payment_breakdown(period):
    """Section 1 source: per-booking Guesty fee waterfall from Task 1's
    inputs/<period>/payment_breakdown_<period>.csv. Returns None if absent."""
    p = paths.payment_breakdown_csv(period)
    if not p.exists():
        return None
    return pd.read_csv(p)

def _member_ids(property_id: str) -> list:
    """A rollup parent expands to itself + its members; everything else is just itself."""
    rollups, _ = load_rollups()
    return [property_id] + list(rollups.get(property_id, []))

@st.cache_data
def load_property_names():
    """property_id -> property_name, for labelling per-listing sections."""
    conn = sqlite3.connect(DB_PATH)
    names = {pid: name for pid, name in conn.execute("SELECT property_id, property_name FROM properties")}
    conn.close()
    return names

# LTR/deferred Net-Revenue lines are sourced from the period's LTR CSV via the
# shared ltr_records module (same logic the Excel build uses).
def build_ltr_records(property_id: str, period: str, pm_fee_rate: float, owner_pays_cleaning: bool):
    """Return (display_records, covered_pids).

    display_records: Net-Revenue-style rows (formatted like Guesty bookings) for LTR
    rents + deferred bookings — one line per CSV row (full monthly rent). Each carries
    numeric helpers (_net/_gross/_comm/_owner) for totalling.
    covered_pids: property_ids that got a line (their rent is moved out of Other Credits).
    """
    conn = sqlite3.connect(DB_PATH)
    raw, covered = ltr_build_records(
        BASE, period, _member_ids(property_id),
        lambda code: conn.execute(
            "SELECT 1 FROM ledger_lines WHERE source='guesty' AND source_txn_id=? LIMIT 1",
            (code,)).fetchone() is not None)
    conn.close()

    out = []
    for x in raw:
        # Round to cents at source so each column's TOTAL equals the sum of the
        # rounded cells shown (no penny drift from summing unrounded values).
        net, gross = round(float(x["net_revenue"]), 2), round(float(x["gross_revenue"]), 2)
        cleaning = round(float(x.get("cleaning_fee", 0.0)), 2)
        comm = round(-net * pm_fee_rate, 2)
        # LTR cleaning fee is non-commissioned owner income (Total Payout = fare + cleaning),
        # so it is added to Net Owner Proceeds AFTER commission — mirrors owner_pays_cleaning.
        owner = round(net + comm + cleaning, 2)
        ltr_nights = 0
        try:
            ci = datetime.strptime(x["checkin"], "%Y-%m-%d"); co = datetime.strptime(x["checkout"], "%Y-%m-%d")
            ltr_nights = (co - ci).days
            dates = f"{ci.strftime('%d. %b.')} - {co.strftime('%d. %b. %Y')} / {ltr_nights} nights"
        except Exception:
            dates = ""
        rec = {
            "Guest Name": x["guest_name"],
            "Confirmation Code": x["booking_id"],
            "Reservation Dates": dates,
            "Gross Revenue": f"${gross:,.2f}",
            "Net Rental Revenue": f"${net:,.2f}",
        }
        if owner_pays_cleaning:
            rec["Owner Cleaning Fee"] = f"${cleaning:,.2f}"
            rec["cleaning_fee_value"] = cleaning
        rec["Management Commission"] = f"${comm:,.2f}"
        rec["Net Owner Proceeds"] = f"${owner:,.2f}"
        rec["Commission %"] = f"{-pm_fee_rate:.0%}"
        rec.update(_net=net, _gross=gross, _comm=comm, _owner=owner, _nights=ltr_nights,
                   _pid=x.get("property_id"), _cleaning=cleaning, _code=x["booking_id"],
                   _is_ltr=bool(x.get("is_ltr", True)))
        out.append(rec)
    return out, covered

@st.cache_data
def load_properties_and_periods():
    """Load available properties and periods from database."""
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row

    periods = conn.execute("""
        SELECT DISTINCT
            strftime('%Y-%m', period_start) as period
        FROM statement_runs
        ORDER BY period DESC
    """).fetchall()

    # Listings present in Listing_contacts.csv (shared with the build).
    allowed = allowed_property_ids(conn, BASE)

    properties = conn.execute("""
        SELECT p.property_id, p.property_name, o.owner_name
        FROM properties p
        LEFT JOIN owners o ON p.owner_id = o.owner_id
        WHERE p.is_active = 1
        ORDER BY p.property_name
    """).fetchall()

    conn.close()

    # Convert to dictionaries for pickle serialization
    periods_list = [dict(p) for p in periods]

    # Rollup members are consolidated into their parent — hide them from the picker.
    _, rollup_children = load_rollups()

    # Filter properties to only those in Listing_contacts.csv
    properties_list = []
    for p in properties:
        prop_dict = dict(p)
        if prop_dict['property_id'] in rollup_children:
            continue
        if prop_dict['property_id'] in allowed:
            properties_list.append(prop_dict)

    return properties_list, periods_list

def load_statement_data(property_id: str, period: str):
    """Load statement data from Excel or database."""
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row

    # Parse period
    year, month = period.split('-')
    period_start = f"{year}-{month}-01"

    # Get property info. PM fee rate is resolved separately through the shared
    # resolve_pm_fee_rate() so the dashboard/PDF use the SAME effective-dated rate
    # (and config fallback) the stored amount_due was built with — otherwise the
    # per-booking commission column won't foot to the Net Income read from amount_due.
    prop = conn.execute("""
        SELECT p.*, o.owner_name, o.owner_email
        FROM properties p
        LEFT JOIN owners o ON p.owner_id = o.owner_id
        WHERE p.property_id = ?
    """, (property_id,)).fetchone()

    # Get statement totals (get LATEST run for this period)
    tot = conn.execute("""
        SELECT spt.* FROM statement_property_totals spt
        JOIN statement_runs sr ON spt.run_id = sr.run_id
        WHERE spt.property_id = ? AND sr.period_start = ?
        ORDER BY sr.created_at DESC
        LIMIT 1
    """, (property_id, period_start)).fetchone()

    # Rollup parents aggregate their members' ledger lines.
    members = _member_ids(property_id)
    ph = ",".join("?" * len(members))

    # Get bookings (guesty income)
    bookings = conn.execute(f"""
        SELECT property_id, source_txn_id as booking_id, vendor_customer as guest_name,
               posting_date as checkin, service_date as checkout, amount as net_revenue, status
        FROM ledger_lines
        WHERE property_id IN ({ph}) AND posting_date >= ? AND posting_date < date(?, '+1 month')
          AND source = 'guesty' AND category = 'INCOME' AND include_in_statement = 1
        ORDER BY property_id, posting_date
    """, (*members, period_start, period_start)).fetchall()

    # Get other income (rent/deferred for properties shown in Net Revenue is filtered
    # out later in Python, using the LTR-covered property set)
    other_income = conn.execute(f"""
        SELECT posting_date, description, vendor_customer, subcategory, amount,
               property_id, source_object
        FROM ledger_lines
        WHERE property_id IN ({ph}) AND posting_date >= ? AND posting_date < date(?, '+1 month')
          AND source = 'qbo' AND category = 'INCOME' AND include_in_statement = 1
        ORDER BY posting_date
    """, (*members, period_start, period_start)).fetchall()

    # Get expenses by category
    expenses = conn.execute(f"""
        SELECT property_id, posting_date, description, vendor_customer, qbo_account, subcategory, amount
        FROM ledger_lines
        WHERE property_id IN ({ph}) AND posting_date >= ? AND posting_date < date(?, '+1 month')
          AND source IN ('qbo','manual') AND category = 'EXPENSE' AND include_in_statement = 1
          AND qbo_account LIKE '%Owner Expenses%'
        ORDER BY subcategory, posting_date
    """, (*members, period_start, period_start)).fetchall()

    # Transient occupancy / lodging tax passed THROUGH to the owner (QBO account
    # 'Owner Income:Taxes Paid to Owners'), matched per booking by the stay's date
    # range (the lines carry "<checkin> to <checkout>" rather than the booking code).
    booking_dicts = [dict(b) for b in bookings]
    for b in booking_dicts:
        trow = conn.execute(f"""
            SELECT COALESCE(SUM(amount), 0) FROM ledger_lines
            WHERE property_id IN ({ph}) AND posting_date >= ? AND posting_date < date(?, '+1 month')
              AND qbo_account LIKE '%Taxes Paid to Owners%'
              AND description LIKE ?
        """, (*members, period_start, period_start, f"%{b['checkin']} to {b['checkout']}%")).fetchone()
        b['owner_tax'] = float(trow[0] or 0)

    prop_dict = dict(prop) if prop else {}
    if prop_dict:
        # None when no contract rate is configured — the caller halts rather than
        # substituting a default (see resolve_pm_fee_rate).
        prop_dict['pm_fee_rate'] = resolve_pm_fee_rate(conn, property_id, period_start)

    conn.close()

    return {
        'property': prop_dict,
        'totals': dict(tot) if tot else None,
        'bookings': booking_dicts,
        'other_income': [dict(i) for i in other_income],
        'expenses': [dict(e) for e in expenses],
    }

def generate_pdf(property_id: str, period: str, data: dict, booking_records: list = None,
                 pb_by_unit: dict = None, pb_grand: dict = None) -> bytes:
    """Generate PDF statement.

    booking_records (optional): the dashboard's per-booking records (Guesty + LTR,
    each carrying _pid/_net/_comm/_owner/_nights). When provided, the Net Revenue
    section is rendered one sub-table per listing from these (so LTR rent is
    included); otherwise it falls back to the Guesty-only data['bookings'].

    pb_by_unit / pb_grand (optional): the SHARED Booking-Breakdown (Section 1) tables
    ({property_id: [rows..., TOTAL]} and the across-units TOTAL row) from
    reporting.booking_breakdown.build_by_unit — rendered above each unit's Net Revenue
    table so the PDF matches the dashboard.
    """
    # Use the SAME rate load_statement_data already resolved via resolve_pm_fee_rate
    # (effective-dated contract) so the PDF commission matches the on-screen table and
    # the stored amount_due. No separate query and no silent default: the main view
    # halts before offering the PDF when the rate is missing.
    _pm = data.get('property', {}).get('pm_fee_rate')
    if _pm is None:
        raise ValueError(f"No PM fee rate configured for {property_id}; set pm_fee_rate in mapping_classes.yml.")
    pm_fee_rate = float(_pm)

    # property_id -> name, for per-listing Net Revenue sub-sections + expense 'Listing'.
    _pconn = sqlite3.connect(DB_PATH)
    pdf_names = {pid: name for pid, name in _pconn.execute("SELECT property_id, property_name FROM properties")}
    _pconn.close()

    # Rollup parents show a per-listing column in the Expenses section; single-listing omit it.
    _rmap, _ = load_rollups()
    pdf_multi = property_id in _rmap

    buffer = io.BytesIO()
    doc = SimpleDocTemplate(buffer, pagesize=letter,
                           rightMargin=0.75*inch, leftMargin=0.75*inch,
                           topMargin=0.5*inch, bottomMargin=0.5*inch)

    story = []
    styles = getSampleStyleSheet()

    # Custom styles
    title_style = ParagraphStyle(
        'CustomTitle',
        parent=styles['Heading1'],
        fontSize=16,
        textColor=colors.HexColor('#1F4E79'),
        spaceAfter=0.2*inch,
    )

    heading_style = ParagraphStyle(
        'CustomHeading',
        parent=styles['Heading2'],
        fontSize=12,
        textColor=colors.HexColor('#2E74B5'),
        spaceAfter=0.15*inch,
        spaceBefore=0.2*inch,
    )

    normal_style = ParagraphStyle(
        'CustomNormal',
        parent=styles['Normal'],
        fontSize=9,
    )

    # Header
    story.append(Paragraph("Valta Realty", title_style))
    period_dt = datetime.strptime(period, '%Y-%m')
    period_label = period_dt.strftime('%B %Y')
    story.append(Paragraph(f"{period_label} - Owner Statement", heading_style))
    story.append(Spacer(1, 0.1*inch))

    # Property & Owner Info
    prop = data['property']
    info_data = [
        ['Property', prop['property_name']],
        ['Owner', prop['owner_name'] or ''],
        ['Email', prop['owner_email'] or ''],
    ]
    info_table = Table(info_data, colWidths=[1.5*inch, 3.5*inch])
    info_table.setStyle(TableStyle([
        ('FONT', (0, 0), (-1, -1), 'Helvetica', 9),
        ('TEXTCOLOR', (0, 0), (0, -1), colors.HexColor('#666666')),
        ('FONTNAME', (0, 0), (0, -1), 'Helvetica-Bold'),
        ('VALIGN', (0, 0), (-1, -1), 'TOP'),
        ('ROWBACKGROUND', (0, 0), (-1, -1), colors.white),
    ]))
    story.append(info_table)
    story.append(Spacer(1, 0.2*inch))

    # Summary — same decomposition as the dashboard top-right: Net Income (pre-expense)
    # − Expense = Owner Payout (== amount_due). data['expenses'] amounts are negative.
    tot = data['totals']
    if tot:
        _amount_due = float(tot['amount_due_to_owner'] or 0)
        _exp_signed = round(sum(float(e['amount'] or 0) for e in data['expenses']), 2)
        _expense_mag = round(-_exp_signed, 2) + 0.0
        _net_income = round(_amount_due - _exp_signed, 2)
        summary_data = [
            ['Starting Balance', f"${0:,.2f}"],
            ['Net Income', f"${_net_income:,.2f}"],
            ['Expense', f"${_expense_mag:,.2f}"],
            ['Owner Payout', f"${_amount_due:,.2f}"],
            ['Ending Balance', f"${_amount_due:,.2f}"],
        ]
        summary_table = Table(summary_data, colWidths=[2*inch, 2*inch])
        summary_table.setStyle(TableStyle([
            ('FONT', (0, 0), (-1, -1), 'Helvetica', 10),
            ('FONTNAME', (0, 0), (-1, -1), 'Helvetica-Bold'),
            ('BACKGROUND', (1, 0), (1, -1), colors.HexColor('#BDD7EE')),
            ('ALIGN', (1, 0), (1, -1), 'RIGHT'),
            ('GRID', (0, 0), (-1, -1), 0.5, colors.grey),
            ('ROWBACKGROUND', (0, 0), (-1, -1), colors.white),
        ]))
        story.append(summary_table)
        story.append(Spacer(1, 0.2*inch))

    # Booking Breakdown (Section 1) — one static table per listing, rendered ABOVE that
    # listing's Net Revenue table (mirrors the dashboard). Shared builder → identical rows.
    _bd_cols = [1.5*inch, 1.0*inch, 0.9*inch, 0.8*inch, 0.9*inch, 0.7*inch, 1.0*inch]
    _bd_numset = set(_pb_num)

    def _bd_fmt_row(r):
        out = []
        for c in _pb_disp:
            v = r.get(c, "")
            out.append(f"${float(v):,.2f}" if c in _bd_numset else str(v))
        return out

    def _bd_table(rows):
        t = Table([list(_pb_disp)] + [_bd_fmt_row(r) for r in rows], colWidths=_bd_cols)
        t.setStyle(TableStyle([
            ('FONT', (0, 0), (-1, -1), 'Helvetica', 7),
            ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
            ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#2E74B5')),
            ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
            ('FONTNAME', (0, -1), (-1, -1), 'Helvetica-Bold'),
            ('BACKGROUND', (0, -1), (-1, -1), colors.HexColor('#E2EFDA')),
            ('ALIGN', (2, 0), (-1, -1), 'RIGHT'),
            ('GRID', (0, 0), (-1, -1), 0.5, colors.grey),
        ]))
        return t

    # Net Revenue - one table per listing (not combined), + grand total.
    if booking_records or data['bookings']:
        story.append(Paragraph("Net Revenue", heading_style))

        _nr_cols = [1.2*inch, 1.2*inch, 1.2*inch, 1.1*inch, 1.1*inch, 1.1*inch]
        _nr_header = ['Reservation Dates', 'Confirmation Code', 'Guest Name',
                      'Net Rental Revenue', 'Management Commission', 'Net Owner Proceeds']

        def _nr_table(rows, grand=False):
            t = Table(rows, colWidths=_nr_cols)
            t.setStyle(TableStyle([
                ('FONT', (0, 0), (-1, -1), 'Helvetica', 8),
                ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
                ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#2E74B5')),
                ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
                ('FONTNAME', (0, -1), (-1, -1), 'Helvetica-Bold'),
                ('BACKGROUND', (0, -1), (-1, -1), colors.HexColor('#CFE0C3' if grand else '#E2EFDA')),
                ('ALIGN', (3, 0), (-1, -1), 'RIGHT'),
                ('GRID', (0, 0), (-1, -1), 0.5, colors.grey),
            ]))
            return t

        # Prefer the pre-built records (Guesty + LTR) so LTR rent is included; else
        # fall back to the Guesty-only data['bookings'].
        if booking_records:
            src = booking_records
            def _vals(r):
                return (r.get('Reservation Dates', ''), r.get('Confirmation Code', ''), r.get('Guest Name', ''),
                        r.get('Net Rental Revenue', ''), r.get('Management Commission', ''), r.get('Net Owner Proceeds', ''),
                        r.get('_net', 0), r.get('_comm', 0), r.get('_owner', 0), int(r.get('_nights', 0)), r.get('_pid'))
        else:
            src = data['bookings']
            def _vals(b):
                net_rental = round(float(b['net_revenue'] or 0), 2)
                comm = round(-net_rental * pm_fee_rate, 2)
                owner_rev = round(net_rental + comm, 2)
                ci = datetime.strptime(b['checkin'], '%Y-%m-%d'); co = datetime.strptime(b['checkout'], '%Y-%m-%d')
                nights = (co - ci).days
                # Same-day credit entries (e.g. OSBR-RV Hipcamp bookings) show a single date.
                dr = (co.strftime('%d. %b. %Y') if nights == 0
                      else f"{ci.strftime('%d. %b.')} - {co.strftime('%d. %b. %Y')} / {nights} nights")
                return (dr, b['booking_id'] or '', b['guest_name'] or '',
                        f"${net_rental:,.2f}", f"${comm:,.2f}", f"${owner_rev:,.2f}",
                        net_rental, comm, owner_rev, nights, b.get('property_id'))

        from collections import OrderedDict
        groups = OrderedDict()
        for b in src:
            try:
                groups.setdefault(_vals(b)[10], []).append(_vals(b))
            except Exception:
                continue
        # Natural listing order (osbr_2 before osbr_10, osbr_rv last) — matches the
        # Booking-Breakdown section above and the Excel/PDF outputs.
        groups = OrderedDict(sorted(groups.items(), key=lambda kv: _natural_key(kv[0])))
        multi = len(groups) > 1
        gt_net = gt_comm = gt_owner = 0.0
        gt_nights = 0

        for gpid, gv in groups.items():
            if multi:
                story.append(Paragraph(f"<b>{pdf_names.get(gpid, gpid or '')}</b>", normal_style))
            # Booking Breakdown for this unit, directly above its Net Revenue table.
            if pb_by_unit and gpid in pb_by_unit:
                story.append(Paragraph("<b>Booking Breakdown</b>", normal_style))
                story.append(_bd_table(pb_by_unit[gpid]))
                story.append(Spacer(1, 0.08*inch))
            rows = [list(_nr_header)]
            s_net = s_comm = s_owner = 0.0
            s_nights = 0
            for v in gv:
                rows.append([v[0], v[1], v[2], v[3], v[4], v[5]])
                s_net += v[6]; s_comm += v[7]; s_owner += v[8]; s_nights += v[9]
            rows.append([f"{s_nights} nights", '', '', f"${s_net:,.2f}", f"${s_comm:,.2f}", f"${s_owner:,.2f}"])
            story.append(_nr_table(rows))
            story.append(Spacer(1, 0.15*inch))
            gt_net += s_net; gt_comm += s_comm; gt_owner += s_owner; gt_nights += s_nights

        if multi:
            if pb_grand:
                story.append(Paragraph("<b>Booking Breakdown — Total (All Units)</b>", normal_style))
                story.append(_bd_table([pb_grand]))
                story.append(Spacer(1, 0.1*inch))
            story.append(Paragraph("<b>Total Net Revenue</b>", normal_style))
            grand_rows = [list(_nr_header),
                          [f"{gt_nights} nights", '', '', f"${gt_net:,.2f}", f"${gt_comm:,.2f}", f"${gt_owner:,.2f}"]]
            story.append(_nr_table(grand_rows, grand=True))
            story.append(Spacer(1, 0.2*inch))

    # Expenses Section
    if data['expenses']:
        story.append(Paragraph("Expenses", heading_style))

        if pdf_multi:
            expense_data = [['Date', 'Description', 'Listing', 'Amount']]
        else:
            expense_data = [['Date', 'Description', 'Amount']]
        total_exp = 0.0

        for exp in data['expenses']:
            amt = round(float(exp['amount'] or 0), 2)
            if pdf_multi:
                expense_data.append([
                    exp['posting_date'],
                    exp['description'][:50] if exp['description'] else '',
                    pdf_names.get(exp['property_id'], exp['property_id'] or ''),
                    f"${amt:,.2f}",
                ])
            else:
                expense_data.append([
                    exp['posting_date'],
                    exp['description'][:70] if exp['description'] else '',
                    f"${amt:,.2f}",
                ])
            total_exp += amt

        if pdf_multi:
            expense_data.append(['', '', 'Total', f"${total_exp:,.2f}"])
            expense_colwidths = [1*inch, 2*inch, 1.5*inch, 1*inch]
        else:
            expense_data.append(['', 'Total', f"${total_exp:,.2f}"])
            expense_colwidths = [1*inch, 3.5*inch, 1*inch]

        expense_table = Table(expense_data, colWidths=expense_colwidths)
        expense_table.setStyle(TableStyle([
            ('FONT', (0, 0), (-1, -1), 'Helvetica', 8),
            ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
            ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#2E74B5')),
            ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
            ('FONTNAME', (0, -1), (-1, -1), 'Helvetica-Bold'),
            ('BACKGROUND', (0, -1), (-1, -1), colors.HexColor('#E2EFDA')),
            ('ALIGN', (-1, 0), (-1, -1), 'RIGHT'),
            ('GRID', (0, 0), (-1, -1), 0.5, colors.grey),
        ]))
        story.append(expense_table)

    doc.build(story)
    buffer.seek(0)
    return buffer.getvalue()

# ─────────────────────────────────────────────────────────────────────────────
# Sidebar
# ─────────────────────────────────────────────────────────────────────────────

st.sidebar.title("📋 Owner Statements")
st.sidebar.markdown("---")

properties, periods = load_properties_and_periods()

# Check if data is available
if not periods:
    st.error("❌ No statement periods found in database")
    st.stop()

if not properties:
    st.error("❌ No properties found in Listing_contacts.csv or database")
    st.stop()

period_opts = [p['period'] for p in periods]
selected_period = st.sidebar.selectbox(
    "Period",
    period_opts,
    format_func=lambda x: datetime.strptime(x, '%Y-%m').strftime('%B %Y')
)

property_opts = [(p['property_id'], p['property_name']) for p in properties]
selected_prop = st.sidebar.selectbox(
    "Property",
    property_opts,
    format_func=lambda x: x[1]
)

st.sidebar.markdown("---")

# ─────────────────────────────────────────────────────────────────────────────
# Main Content
# ─────────────────────────────────────────────────────────────────────────────

if not selected_prop:
    st.error("❌ No property selected")
    st.stop()

property_id = selected_prop[0]
period = selected_period

# Load data
data = load_statement_data(property_id, period)
prop = data['property']
tot = data['totals']

# Load property addresses and flags from mapping
addresses, owner_flags = load_mapping_with_addresses()
pid_names = load_property_names()
# Rollup parents (beachwood, osbr, bellevue_14507, …) show a per-listing column in
# the Expenses section; single-listing statements omit it.
_rollups_map, _ = load_rollups()
is_rollup_parent = property_id in _rollups_map
prop_address = addresses.get(property_id, 'N/A')
owner_pays_cleaning = owner_flags.get(property_id, {}).get('owner_pays_cleaning', False)
owner_pays_taxes = owner_flags.get(property_id, {}).get('owner_pays_taxes', False)

# PM fee rate: resolved in load_statement_data via resolve_pm_fee_rate (effective-dated
# contract), the same rate the stored amount_due used. No silent default — if a property
# has no configured rate, halt and tell the user to set it (matching the build's behavior).
_pm_rate_raw = prop.get('pm_fee_rate') if prop else None
if _pm_rate_raw is None:
    st.error(
        f"No PM fee rate is configured for **{property_id}**. "
        f"Set `pm_fee_rate` for it in `mapping_classes.yml` (then run `sync-mappings`) "
        f"so commission can be calculated. Refusing to guess a default."
    )
    st.stop()
pm_fee_rate = float(_pm_rate_raw)
ltr_records, ltr_covered_pids = build_ltr_records(property_id, period, pm_fee_rate, owner_pays_cleaning)
has_net_revenue = bool(data['bookings'] or ltr_records)

# Move rent out of "Other Credits" only for properties that got a Net Revenue line
# (rent for properties without a CSV line stays in Other Credits so it isn't hidden).
data['other_income'] = [
    oi for oi in data['other_income']
    if not (oi.get('property_id') in ltr_covered_pids
            and ltr_is_rent_income(oi.get('source_object'), oi.get('description')))
]

# Header with property info and summary side-by-side
period_dt = datetime.strptime(period, '%Y-%m')

col_left, col_right = st.columns([1, 1], gap="large")

with col_left:
    st.markdown("### Property Management")
    st.title("Valta Realty")

    col_a, col_b = st.columns([0.3, 0.7])
    with col_a:
        st.markdown("**Address**")
    with col_b:
        st.markdown("4027 Beach Drive Southwest, Seattle, WA 98116")

    col_a, col_b = st.columns([0.3, 0.7])
    with col_a:
        st.markdown("**Email**")
    with col_b:
        st.markdown("contact@valtarealty.com")

    col_a, col_b = st.columns([0.3, 0.7])
    with col_a:
        st.markdown("**Phone**")
    with col_b:
        st.markdown("[Add phone]()")

    st.divider()

    st.markdown(f"**{prop['property_name']}**")
    st.markdown(f"{prop_address}")

    col_a, col_b = st.columns([0.3, 0.7])
    with col_a:
        st.markdown("**Owner**")
    with col_b:
        st.markdown(prop['owner_name'] or 'N/A')

    col_a, col_b = st.columns([0.3, 0.7])
    with col_a:
        st.markdown("**Email**")
    with col_b:
        st.markdown(prop['owner_email'] or 'N/A')

with col_right:
    st.markdown(f"### {period_dt.strftime('%B %Y')} - Summary")

    # This will be calculated in the Net Revenue below
    # and we'll reference it here via a placeholder that gets updated
    if has_net_revenue:
        st.markdown("*(See Net Revenue below for calculation details)*")

st.divider()

# Always defined so the Download PDF handler can reference it even with no net revenue.
booking_records = []

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 1 — Guesty payment breakdown (per-booking fee waterfall)
# What the guest paid → every cost (channel/Guesty/Stripe/cleaning/tax) → what the
# host received → Net Revenue (Guesty basis). This is a DIFFERENT lens from the
# owner-statement Net Owner Proceeds in Section 2 below, so the two "Net" figures
# intentionally differ (Section 1 is pre-PM-commission, Guesty-side only).
# ─────────────────────────────────────────────────────────────────────────────
_member_pids = set(_rollups_map.get(property_id, [])) | {property_id}
_pb = load_payment_breakdown(period)

# Dedup after the Guesty pull (owner's pull-steps model: pull Guesty → pull LTR → dedup
# → make booking breakdown → create revenue sections). On a confirmation-code collision
# LTR/DEFERRED wins; `_claimed` = codes now represented ONLY by an LTR/DEFERRED ledger
# row. build_by_unit drops those stale Guesty breakdown rows so Section 1 foots with
# Section 2 (the booking is re-shown once as an LTR/deferred row).
_c = sqlite3.connect(DB_PATH)
_claimed = ltr_claimed_codes(_c, period)
_c.close()

# Booking-breakdown tables are rendered PER UNIT inside the Net Revenue below
# (each unit's fee waterfall sits directly above its statement). Here we only prepare
# the per-unit DataFrames + a render helper; nothing is drawn yet.
#
# The three fees (channel / Guesty / Stripe) are FOLDED into one "Fees" column — hover
# a value to see the split. The old "Invoice Amount" column is dropped (owner request).
# Component values are kept as hidden _ch/_gu/_st columns to build the hover tooltip.
# Columns (owner request): the Booking Breakdown is a pure per-booking rental
# waterfall — Conf Code, Bookings (channel; its TOTAL cell shows the booking count),
# guest pay, fees, cleaning, tax, Net Rental Revenue. PM commission and Net Owner
# Proceeds are NOT shown here — they live in the Net Revenue table below (which folds
# in owner cleaning/tax credits). The total row leaves the Conf Code cell blank.
# Display columns/widths (the column SET + fold logic live in reporting.booking_breakdown,
# imported above as _pb_disp/_pb_num/_fee_parts so dashboard/Excel/PDF share one source).
# Fixed widths (sum to 100%) so the detail table and the total table align column-for-column.
_pb_widths = {"Conf Code": "20%", "Bookings": "18%", "Guest Pay": "13%", "Fees": "12%",
              "Cleaning Fee": "13%", "Tax": "10%", "Net Rental Revenue": "14%"}
_pb_caption = (
    "Per-booking payment breakdown — guest pay, **Fees** (channel + Guesty + "
    "Stripe combined; hover a value to see the split), cleaning, tax, and **Net Rental "
    "Revenue** (carried into the statement below, where PM commission is subtracted to "
    "reach Net Owner Proceeds).  \n"
    "For **HomeAway, Booking.com, Expedia, and Trip.com**, the channel fee is baked into "
    "the booking amount — the host has to pay it back, so it is included in Fees here.  \n"
    "**LTR** bookings are fee-free: Guest Pay (Total Payout) = accommodation fare + "
    "cleaning; the cleaning fee is non-commissioned, so Net Rental Revenue = accommodation fare."
)
# Build the per-unit breakdown from BOTH sources (Guesty fee waterfall + LTR/deferred,
# fee-free), deduped, via the SHARED builder so dashboard/Excel/PDF match exactly.
_bd_by_unit, _bd_grand = _bd_build(_pb, ltr_records, _member_pids, _claimed)
_pb_by_unit = {pid: pd.DataFrame(rows) for pid, rows in _bd_by_unit.items()}
_pb_grand = pd.DataFrame([_bd_grand]) if _bd_grand else None

def _pb_render(df_rows, highlight_bg="#e2efda"):
    """Static full-width HTML table for the booking breakdown: fits the page width so
    all columns show (headers wrap, so 'Net Rental Revenue' stays narrow), grows
    vertically (no internal scroll). Any TOTAL row (hidden `_total` flag) is highlighted
    — so this renders detail-only (unhighlighted) and the standalone total row
    (highlighted) identically and aligned. The 'Fees' cell carries a hover tooltip
    breaking out channel/Guesty/Stripe."""
    _fmt = lambda v: f"${float(v):,.2f}"
    _h = ['<table class="pb-table"><colgroup>']
    for c in _pb_disp:
        _h.append(f'<col style="width:{_pb_widths[c]}">')
    _h.append('</colgroup><thead><tr>')
    for c in _pb_disp:
        _h.append(f'<th style="text-align:{"right" if c in _pb_num else "left"}">{c}</th>')
    _h.append('</tr></thead><tbody>')
    for _idx, row in df_rows.iterrows():
        _style = (f' style="background-color:{highlight_bg};font-weight:bold"'
                  if bool(row.get("_total")) else '')
        _h.append(f'<tr{_style}>')
        for c in _pb_disp:
            v = row[c]
            if c in _pb_num:
                if c == "Fees":
                    _tip = _esc(f"Channel {_fmt(row['_ch'])}  +  Guesty {_fmt(row['_gu'])}  +  "
                                f"Stripe {_fmt(row['_st'])}", quote=True)
                    _h.append(f'<td style="text-align:right" title="{_tip}">'
                              f'<span class="pb-fees">{_fmt(v)}</span></td>')
                else:
                    _h.append(f'<td style="text-align:right">{_fmt(v)}</td>')
            else:
                _h.append(f'<td style="text-align:left">{"" if pd.isna(v) else _esc(str(v))}</td>')
        _h.append('</tr>')
    _h.append('</tbody></table>')
    st.markdown('\n'.join(_h), unsafe_allow_html=True)

# NET REVENUE SECTION - Calculate totals for summary
if has_net_revenue:
    # Load Guesty CSV to get total_payout (gross revenue before fees) and cleaning fees
    guesty_csv = paths.guesty_converted_csv(period)
    gross_revenue_map = {}
    cleaning_fee_map = {}
    if guesty_csv.exists():
        df_guesty = pd.read_csv(str(guesty_csv), encoding='utf-8-sig')
        for _, row in df_guesty.iterrows():
            booking_id = str(row['booking_id'])
            total_payout = float(row.get('total_payout', 0))
            cleaning_fee = float(row.get('cleaning_fee', 0))
            gross_revenue_map[booking_id] = total_payout
            cleaning_fee_map[booking_id] = cleaning_fee

    booking_records = []
    total_gross_revenue = 0.0
    total_net_rental = 0.0
    total_comm = 0.0
    total_owner = 0.0
    total_expenses = 0.0
    total_nights = 0

    for b in data['bookings']:
        # Round every displayed dollar figure to cents at source so each column's
        # TOTAL equals the sum of the rounded cells shown (no penny drift from
        # summing unrounded values and rounding only the total).
        net_rental = round(float(b['net_revenue'] or 0), 2)
        booking_id = b['booking_id']

        # Gross revenue from converted Guesty CSV (total_payout before any fees)
        gross_revenue = round(gross_revenue_map.get(booking_id, net_rental), 2)

        # Check booking status
        booking_status = str(b.get('status', 'confirmed')).lower()

        # For canceled bookings, cleaning fee is 0; otherwise get from Guesty
        if booking_status == 'canceled':
            cleaning_fee = 0.0
        else:
            cleaning_fee = round(cleaning_fee_map.get(booking_id, 0.0), 2)

        comm = round(-net_rental * pm_fee_rate, 2)
        owner_rev = net_rental + comm

        # Add owner cleaning fee if owner_pays_cleaning is true
        if owner_pays_cleaning:
            owner_rev += cleaning_fee

        # Tax passed to owner (transient occupancy tax) — owner income, added after MCR
        tax_paid = round(float(b.get('owner_tax') or 0), 2) if owner_pays_taxes else 0.0
        if owner_pays_taxes:
            owner_rev += tax_paid

        owner_rev = round(owner_rev, 2)

        checkin = datetime.strptime(b['checkin'], '%Y-%m-%d')
        checkout = datetime.strptime(b['checkout'], '%Y-%m-%d')

        # For canceled bookings, show 0 nights instead of actual days
        if booking_status == 'canceled':
            nights = 0
        else:
            nights = (checkout - checkin).days

        # Same-day credit entries (e.g. OSBR-RV Hipcamp bookings) show a single date.
        if checkin == checkout:
            reservation_dates = checkout.strftime('%d. %b. %Y')
        else:
            reservation_dates = f"{checkin.strftime('%d. %b.')} - {checkout.strftime('%d. %b. %Y')} / {nights} nights"

        record = {
            'Guest Name': b['guest_name'],
            'Confirmation Code': b['booking_id'],
            'Reservation Dates': reservation_dates,
            'Gross Revenue': f"${gross_revenue:,.2f}",
            'Net Rental Revenue': f"${net_rental:,.2f}",
        }

        # Add Owner Cleaning Fee column if applicable
        if owner_pays_cleaning:
            record['Owner Cleaning Fee'] = f"${cleaning_fee:,.2f}"
            record['cleaning_fee_value'] = cleaning_fee  # Store numeric value for TOTAL calculation

        record['Management Commission'] = f"${comm:,.2f}"
        if owner_pays_taxes:
            record['Tax Paid to Owner'] = f"${tax_paid:,.2f}"
            record['tax_paid_value'] = tax_paid  # numeric, for TOTAL row
        record['Net Owner Proceeds'] = f"${owner_rev:,.2f}"
        record['Commission %'] = f"{-pm_fee_rate:.0%}"
        record.update(_pid=b.get('property_id'), _gross=gross_revenue, _net=net_rental,
                      _comm=comm, _owner=owner_rev, _nights=nights)

        booking_records.append(record)

        total_gross_revenue += gross_revenue
        total_net_rental += net_rental
        total_comm += comm
        total_owner += owner_rev
        total_nights += nights

    # Append LTR rents + deferred bookings (from the LTR CSV) as Net Revenue lines
    for rec in ltr_records:
        booking_records.append(rec)
        total_gross_revenue += rec['_gross']
        total_net_rental += rec['_net']
        total_comm += rec['_comm']
        total_owner += rec['_owner']
        total_nights += rec.get('_nights', 0)

    # Sum all expenses (already negative values)
    for exp in data['expenses']:
        total_expenses += float(exp['amount'] or 0)

    # Net Income = the authoritative build total (statement_property_totals.
    # amount_due_to_owner) for ALL properties, so the dashboard summary always matches
    # the Excel statement and the end_balances comparison. That total already reflects:
    # guesty net (idempotent, fee-adjusted), Other Credits at full value, PM commission
    # on bookings + LTR/deferred only (NOT Other Credits), and owner expenses.
    # Fall back to the line-by-line sum only if no build total exists for this period yet.
    # Owner Cleaning Fee (the Guesty cleaning charged to the guest) is returned to the
    # owner as income for owner_pays_cleaning properties and shown per-booking in the Net
    # Revenue table (added into each Net Owner Proceeds cell and the TOTAL row). The build
    # now folds this same credit into amount_due_to_owner via a non-commissioned OWNER_ADJ
    # line (_apply_owner_cleaning_credit), so Net Income reads amount_due directly — do NOT
    # add the cleaning fee again here or it would double-count.
    total_cleaning_fee = sum(
        float(r.get('cleaning_fee_value', 0)) for r in booking_records if 'cleaning_fee_value' in r
    )
    if tot:
        amount_due = float(tot['amount_due_to_owner'] or 0)
    else:
        amount_due = total_owner + total_expenses

    # Summary decomposition (owner request): show owner Expense on its own line and
    # derive Owner Payout = Net Income − Expense. `amount_due` already nets owner
    # expenses and `total_expenses` is stored negative, so the pre-expense Net Income is
    # (amount_due − total_expenses); subtracting the expense magnitude returns the true
    # payout (== amount_due). Expense is 0 → Net Income == Owner Payout (no visible change).
    expense_mag = round(-total_expenses, 2) + 0.0           # owner expenses as a positive magnitude (+0.0 kills -0.0)
    net_income = round(amount_due - total_expenses, 2)      # income before owner expenses
    owner_payout = round(net_income - expense_mag, 2)       # == amount_due, the actual payout

    # Update summary with calculated values
    with col_right:
        for _lbl, _val, _bold in [
            ("Starting Balance", 0.0, False),
            ("Net Income", net_income, True),
            ("Expense", expense_mag, False),
            ("Owner Payout", owner_payout, False),
            ("Ending Balance", owner_payout, False),
        ]:
            col_a, col_b = st.columns([0.6, 0.4])
            with col_a:
                st.markdown(f"**{_lbl}**")
            with col_b:
                if _bold:
                    st.markdown(f"<span style='color: #2e74b5; font-weight: bold;'>${_val:,.2f}</span>", unsafe_allow_html=True)
                else:
                    st.markdown(f"${_val:,.2f}")

        st.markdown("")
        if st.button("📥 Download PDF", use_container_width=True, key="pdf_btn"):
            pdf_bytes = generate_pdf(property_id, period, data, booking_records=booking_records,
                                     pb_by_unit=_bd_by_unit, pb_grand=_bd_grand)
            st.download_button(
                label="PDF Statement",
                data=pdf_bytes,
                file_name=f"{property_id}_{period}_statement.pdf",
                mime="application/pdf"
            )

    st.markdown("")

# Summary for properties with no Guesty bookings (LTR / deferred / rollup parents):
# use the authoritative statement_property_totals from the build (includes other
# income, PM commission, and expenses).
elif tot:
    amount_due = float(tot['amount_due_to_owner'] or 0)
    # Same decomposition as the Guesty path: Net Income (pre-expense) − Expense = Owner
    # Payout (== amount_due). data['expenses'] amounts are stored negative.
    _exp_signed = round(sum(float(e['amount'] or 0) for e in data['expenses']), 2)
    expense_mag = round(-_exp_signed, 2) + 0.0
    net_income = round(amount_due - _exp_signed, 2)
    owner_payout = amount_due
    with col_right:
        for _lbl, _val, _bold in [
            ("Starting Balance", 0.0, False),
            ("Net Income", net_income, True),
            ("Expense", expense_mag, False),
            ("Owner Payout", owner_payout, False),
            ("Ending Balance", owner_payout, False),
        ]:
            col_a, col_b = st.columns([0.6, 0.4])
            with col_a:
                st.markdown(f"**{_lbl}**")
            with col_b:
                if _bold:
                    st.markdown(f"<span style='color: #2e74b5; font-weight: bold;'>${_val:,.2f}</span>", unsafe_allow_html=True)
                else:
                    st.markdown(f"${_val:,.2f}")

st.divider()

# Net Revenue - one table PER LISTING (not combined), each with its own subtotal.
if has_net_revenue:
    st.header("Net Revenue")
    if _pb_by_unit:
        st.caption(_pb_caption)

    from collections import OrderedDict
    groups = OrderedDict()
    for r in booking_records:
        groups.setdefault(r.get('_pid'), []).append(r)
    # Natural listing order (osbr_2 before osbr_10, osbr_rv last) — matches the
    # Booking-Breakdown section and the Excel/PDF outputs.
    groups = OrderedDict(sorted(groups.items(), key=lambda kv: _natural_key(kv[0])))
    multi_listing = len(groups) > 1
    _helper_cols = ('cleaning_fee_value', 'tax_paid_value')

    # Money columns are rendered as NUMBERS (not pre-formatted strings) so st.dataframe
    # right-aligns them and column_config formats them as currency / percent. The string
    # display records (booking_records) are left untouched for the PDF.
    _nr_money = ['Net Rental Revenue']
    if owner_pays_cleaning:
        _nr_money.append('Owner Cleaning Fee')
    _nr_money.append('Management Commission')
    if owner_pays_taxes:
        _nr_money.append('Tax Paid to Owner')
    _nr_money.append('Net Owner Proceeds')
    _nr_colcfg = {c: st.column_config.NumberColumn(format="dollar") for c in _nr_money}
    _nr_colcfg['Commission %'] = st.column_config.NumberColumn(format="percent")

    def _nr_num_row(guest, conf, dates, net, clean, comm, tax, owner):
        row = {'Guest Name': guest, 'Confirmation Code': conf, 'Reservation Dates': dates,
               'Net Rental Revenue': round(float(net), 2)}
        if owner_pays_cleaning:
            row['Owner Cleaning Fee'] = round(float(clean), 2)
        row['Management Commission'] = round(float(comm), 2)
        if owner_pays_taxes:
            row['Tax Paid to Owner'] = round(float(tax), 2)
        row['Net Owner Proceeds'] = round(float(owner), 2)
        row['Commission %'] = -pm_fee_rate
        return row

    for _gpid, _recs in groups.items():
        if multi_listing:
            st.markdown(f"### {pid_names.get(_gpid, _gpid or '')}")
        # Each unit's Guesty booking breakdown sits directly above its statement, in a
        # collapsible section (like Expenses). Detail rows live inside the expander; the
        # TOTAL row is rendered just below it and stays visible even when collapsed.
        if _gpid in _pb_by_unit:
            _dfu = _pb_by_unit[_gpid]
            with st.expander("**Booking Breakdown**", expanded=False):
                _pb_render(_dfu[~_dfu["_total"]])
            _pb_render(_dfu[_dfu["_total"]])

        g_nights = sum(int(r.get('_nights', 0)) for r in _recs)
        detail_rows = [_nr_num_row(r.get('Guest Name', ''), r.get('Confirmation Code', ''),
                                   r.get('Reservation Dates', ''), r.get('_net', 0),
                                   r.get('cleaning_fee_value', 0), r.get('_comm', 0),
                                   r.get('tax_paid_value', 0), r.get('_owner', 0))
                       for r in _recs]
        total_row = _nr_num_row('', '', f"{g_nights} nights",
                                sum(r.get('_net', 0) for r in _recs),
                                sum(float(r.get('cleaning_fee_value', 0)) for r in _recs),
                                sum(r.get('_comm', 0) for r in _recs),
                                sum(float(r.get('tax_paid_value', 0)) for r in _recs),
                                sum(r.get('_owner', 0) for r in _recs))

        # Detail rows go inside the collapsible section; the TOTAL row is rendered just
        # below it and stays visible even when the section is collapsed.
        df_detail = pd.DataFrame(detail_rows)
        df_total = pd.DataFrame([total_row])
        with st.expander("**Net Revenue**", expanded=False):
            st.dataframe(df_detail, use_container_width=True, hide_index=True,
                         column_config=_nr_colcfg, height=(len(df_detail) + 1) * 35 + 3)
        st.dataframe(df_total.style.apply(
                        lambda row: ['background-color: #e2efda; font-weight: bold'] * len(row), axis=1),
                     use_container_width=True, hide_index=True, column_config=_nr_colcfg, height=2 * 35 + 3)

    # Grand totals across all listings (only meaningful when there is more than one).
    # These are total-only tables, so they are always visible (not collapsible).
    if multi_listing:
        if _pb_grand is not None:
            st.markdown("**Booking Breakdown — Total (All Units)**")
            _pb_render(_pb_grand, "#cfe0c3")
        grand_row = _nr_num_row('', '', f"{total_nights} nights", total_net_rental,
                                total_cleaning_fee, total_comm,
                                sum(float(r.get('tax_paid_value', 0)) for r in booking_records),
                                total_owner)
        df_grand = pd.DataFrame([grand_row])
        st.markdown("**Total Net Revenue**")
        st.dataframe(df_grand.style.apply(lambda row: ['background-color: #cfe0c3; font-weight: bold'] * len(row), axis=1),
                     use_container_width=True, hide_index=True, column_config=_nr_colcfg, height=2 * 35 + 3)

st.divider()

# Other Income
if data['other_income']:
    st.header("Other Credits")

    income_records = []
    total_oi = 0.0

    for inc in data['other_income']:
        amt = round(float(inc['amount'] or 0), 2)
        rec = {
            'Date': inc['posting_date'],
            'Description': inc['description'][:100] if inc['description'] else '',
        }
        # 'Listing' (rollup parents only) shows which unit each credit belongs to.
        if is_rollup_parent:
            rec['Listing'] = pid_names.get(inc['property_id'], inc['property_id'] or '')
        rec['Amount'] = f"${amt:,.2f}"
        income_records.append(rec)
        total_oi += amt

    total_rec = {'Date': '', 'Description': '' if is_rollup_parent else 'Total'}
    if is_rollup_parent:
        total_rec['Listing'] = 'Total'
    total_rec['Amount'] = f"${total_oi:,.2f}"
    income_records.append(total_rec)

    df_income = pd.DataFrame(income_records)

    # Style the dataframe to bold the last (Total) row.
    def highlight_last_income_row(col):
        return ['background-color: #e2efda; font-weight: bold'
                if i == len(col) - 1 else '' for i in range(len(col))]

    income_col_config = {
        'Date': st.column_config.Column(width=80),
        'Description': st.column_config.Column(width='large'),
        'Amount': st.column_config.Column(width=80),
    }
    if is_rollup_parent:
        income_col_config['Listing'] = st.column_config.Column(width='medium')

    styled_income = df_income.style.apply(highlight_last_income_row)
    st.dataframe(styled_income, column_config=income_col_config,
                 use_container_width=True, hide_index=True,
                 height=(len(df_income) + 1) * 35 + 3)

st.divider()

# Expenses - Show all categories even if empty
st.header("Expenses")

# Define expense categories to display
all_categories = [
    'Cleaning Labor',
    'Supplies',
    'Repairs & Maintenance',
    'Utilities',
    'Other Expense'
]

# Group by category and filter out supplies for canceled bookings
expense_categories = {}
for exp in data['expenses']:
    cat = exp['subcategory'] or 'Other Expense'

    # Skip supplies for canceled bookings
    if cat == 'Supplies' and exp['description'] and 'Cancelled' in exp['description']:
        continue

    if cat not in expense_categories:
        expense_categories[cat] = []
    expense_categories[cat].append(exp)

# Display all categories (even empty ones)
for category in all_categories:
        records = []
        total_cat = 0.0

        # Get expenses for this category, or empty list if none
        exps = expense_categories.get(category, [])
        for exp in exps:
            amt = round(float(exp['amount'] or 0), 2)
            rec = {
                'Date': exp['posting_date'],
                'Description': exp['description'][:100] if exp['description'] else '',
            }
            # 'Listing' (rollup parents only) shows which unit each cost belongs to.
            if is_rollup_parent:
                rec['Listing'] = pid_names.get(exp['property_id'], exp['property_id'] or '')
            rec['Amount'] = f"${amt:,.2f}"
            records.append(rec)
            total_cat += amt

        total_rec = {'Date': '', 'Description': '' if is_rollup_parent else 'Total'}
        if is_rollup_parent:
            total_rec['Listing'] = 'Total'
        total_rec['Amount'] = f"${total_cat:,.2f}"

        col_config = {
            'Date': st.column_config.Column(width=80),
            'Description': st.column_config.Column(width='large'),
            'Amount': st.column_config.Column(width=80),
        }
        if is_rollup_parent:
            col_config['Listing'] = st.column_config.Column(width='medium')

        # Detail rows go inside the collapsible section; the Total row is rendered just
        # below it and stays visible even when the section is collapsed. Empty categories
        # show a short note (not an empty grid) so they don't look broken.
        df_detail = pd.DataFrame(records, columns=list(total_rec.keys()))
        df_total = pd.DataFrame([total_rec])
        with st.expander(f"**{category}**", expanded=False):
            if len(df_detail):
                st.dataframe(df_detail, column_config=col_config,
                             use_container_width=True, hide_index=True,
                             height=(len(df_detail) + 1) * 35 + 3)
            else:
                st.caption("No expenses in this category.")
        st.dataframe(df_total.style.apply(
                        lambda row: ['background-color: #e2efda; font-weight: bold'] * len(row), axis=1),
                     column_config=col_config, use_container_width=True, hide_index=True,
                     height=2 * 35 + 3)
