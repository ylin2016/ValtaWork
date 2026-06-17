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

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

BASE = Path(__file__).parent.parent
DB_PATH = BASE / "data/owner_statement.sqlite"
STATEMENTS_DIR = BASE / "output"

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
</style>
""", unsafe_allow_html=True)

# ─────────────────────────────────────────────────────────────────────────────
# Data Loading
# ─────────────────────────────────────────────────────────────────────────────

@st.cache_data
def load_listing_contacts():
    """Load property names from Listing_contacts.csv."""
    import csv
    import re

    csv_path = Path(__file__).parent.parent / "data/Listing_contacts.csv"
    contact_props = set()

    if csv_path.exists():
        with open(csv_path, encoding='utf-8-sig') as f:
            reader = csv.DictReader(f)
            for row in reader:
                prop_name = (row.get('Property') or '').strip().lower()
                if prop_name:
                    # Normalize property name for matching
                    normalized = re.sub(r'[^a-z0-9]+', '_', prop_name)
                    normalized = re.sub(r'_+', '_', normalized).strip('_')
                    contact_props.add(normalized)

    return contact_props

@st.cache_data
def load_mapping_with_addresses():
    """Load property addresses and flags from mapping_classes.yml."""
    import yaml
    mapping_path = Path(__file__).parent.parent / "mapping_classes.yml"
    addresses = {}
    owner_flags = {}

    if mapping_path.exists():
        with open(mapping_path, 'r') as f:
            items = yaml.safe_load(f)
            for item in items or []:
                prop_id = item.get('property_id', '')
                if prop_id:
                    address = item.get('address', '')
                    if address:
                        addresses[prop_id] = address
                    owner_flags[prop_id] = {
                        'owner_pays_cleaning': item.get('owner_pays_cleaning', False),
                        'owner_pays_taxes': item.get('owner_pays_taxes', False),
                        'owner_pays_supplies': item.get('owner_pays_supplies', False),
                    }

    return addresses, owner_flags

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

    # Load properties from Listing_contacts.csv
    contact_props = load_listing_contacts()

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

    # Filter properties to only those in Listing_contacts.csv
    properties_list = []
    for p in properties:
        prop_dict = dict(p)
        # Check if property_id matches any contact property
        if prop_dict['property_id'] in contact_props:
            properties_list.append(prop_dict)

    return properties_list, periods_list

def load_statement_data(property_id: str, period: str):
    """Load statement data from Excel or database."""
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row

    # Parse period
    year, month = period.split('-')
    period_start = f"{year}-{month}-01"

    # Get property info with PM fee rate
    prop = conn.execute("""
        SELECT p.*, o.owner_name, o.owner_email, COALESCE(oc.pm_fee_rate, 0.18) as pm_fee_rate
        FROM properties p
        LEFT JOIN owners o ON p.owner_id = o.owner_id
        LEFT JOIN owner_contracts oc ON p.property_id = oc.property_id
        WHERE p.property_id = ?
        ORDER BY oc.effective_start DESC LIMIT 1
    """, (property_id,)).fetchone()

    # Get statement totals (get LATEST run for this period)
    tot = conn.execute("""
        SELECT spt.* FROM statement_property_totals spt
        JOIN statement_runs sr ON spt.run_id = sr.run_id
        WHERE spt.property_id = ? AND sr.period_start = ?
        ORDER BY sr.created_at DESC
        LIMIT 1
    """, (property_id, period_start)).fetchone()

    # Get bookings (guesty income)
    bookings = conn.execute("""
        SELECT source_txn_id as booking_id, vendor_customer as guest_name,
               posting_date as checkin, service_date as checkout, amount as net_revenue, status
        FROM ledger_lines
        WHERE property_id = ? AND posting_date >= ? AND posting_date < date(?, '+1 month')
          AND source = 'guesty' AND category = 'INCOME' AND include_in_statement = 1
        ORDER BY posting_date
    """, (property_id, period_start, period_start)).fetchall()

    # Get other income
    other_income = conn.execute("""
        SELECT posting_date, description, vendor_customer, subcategory, amount
        FROM ledger_lines
        WHERE property_id = ? AND posting_date >= ? AND posting_date < date(?, '+1 month')
          AND source = 'qbo' AND category = 'INCOME' AND include_in_statement = 1
        ORDER BY posting_date
    """, (property_id, period_start, period_start)).fetchall()

    # Get expenses by category
    expenses = conn.execute("""
        SELECT posting_date, description, vendor_customer, qbo_account, subcategory, amount
        FROM ledger_lines
        WHERE property_id = ? AND posting_date >= ? AND posting_date < date(?, '+1 month')
          AND source = 'qbo' AND category = 'EXPENSE' AND include_in_statement = 1
          AND qbo_account LIKE '%Owner Expenses%'
        ORDER BY subcategory, posting_date
    """, (property_id, period_start, period_start)).fetchall()

    conn.close()

    return {
        'property': dict(prop) if prop else {},
        'totals': dict(tot) if tot else None,
        'bookings': [dict(b) for b in bookings],
        'other_income': [dict(i) for i in other_income],
        'expenses': [dict(e) for e in expenses],
    }

def generate_pdf(property_id: str, period: str, data: dict) -> bytes:
    """Generate PDF statement."""
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

    # Summary
    tot = data['totals']
    if tot:
        summary_data = [
            ['Starting Balance', f"${0:,.2f}"],
            ['Net Income', f"${float(tot['amount_due_to_owner']):,.2f}"],
            ['Current Balance', f"${float(tot['amount_due_to_owner']):,.2f}"],
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

    # Net Revenue Section
    if data['bookings']:
        story.append(Paragraph("Net Revenue Section", heading_style))

        booking_data = [['Reservation Dates', 'Confirmation Code', 'Guest Name', 'Net Rental Revenue', 'Management Commission', 'Net Owner Revenue']]

        total_net_rental = 0.0
        total_comm = 0.0
        total_owner = 0.0

        for b in data['bookings']:
            try:
                net_rental = float(b['net_revenue'] or 0)
                comm = -net_rental * 0.18  # Assuming 18% PM fee
                owner_rev = net_rental + comm

                checkin = datetime.strptime(b['checkin'], '%Y-%m-%d')
                checkout = datetime.strptime(b['checkout'], '%Y-%m-%d')
                nights = (checkout - checkin).days
                date_range = f"{checkin.strftime('%d. %b.')} - {checkout.strftime('%d. %b. %Y')} / {nights} nights"

                booking_data.append([
                    date_range,
                    b['booking_id'] or '',
                    b['guest_name'] or '',
                    f"${net_rental:,.2f}",
                    f"${comm:,.2f}",
                    f"${owner_rev:,.2f}",
                ])

                total_net_rental += net_rental
                total_comm += comm
                total_owner += owner_rev
            except Exception as e:
                continue

        booking_data.append(['', '', 'Total', f"${total_net_rental:,.2f}", f"${total_comm:,.2f}", f"${total_owner:,.2f}"])

        booking_table = Table(booking_data, colWidths=[1.2*inch, 1.2*inch, 1.2*inch, 1.1*inch, 1.1*inch, 1.1*inch])
        booking_table.setStyle(TableStyle([
            ('FONT', (0, 0), (-1, -1), 'Helvetica', 8),
            ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
            ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#2E74B5')),
            ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
            ('FONTNAME', (0, -1), (-1, -1), 'Helvetica-Bold'),
            ('BACKGROUND', (0, -1), (-1, -1), colors.HexColor('#E2EFDA')),
            ('ALIGN', (3, 0), (-1, -1), 'RIGHT'),
            ('GRID', (0, 0), (-1, -1), 0.5, colors.grey),
        ]))
        story.append(booking_table)
        story.append(Spacer(1, 0.2*inch))

    # Expenses Section
    if data['expenses']:
        story.append(Paragraph("Expenses", heading_style))

        expense_data = [['Date', 'Description', 'Type', 'Amount']]
        total_exp = 0.0

        for exp in data['expenses']:
            amt = float(exp['amount'] or 0)
            expense_data.append([
                exp['posting_date'],
                exp['description'][:50] if exp['description'] else '',
                exp['qbo_account'] or '',
                f"${amt:,.2f}",
            ])
            total_exp += amt

        expense_data.append(['', '', 'Total', f"${total_exp:,.2f}"])

        expense_table = Table(expense_data, colWidths=[1*inch, 2*inch, 1.5*inch, 1*inch])
        expense_table.setStyle(TableStyle([
            ('FONT', (0, 0), (-1, -1), 'Helvetica', 8),
            ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
            ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#2E74B5')),
            ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
            ('FONTNAME', (0, -1), (-1, -1), 'Helvetica-Bold'),
            ('BACKGROUND', (0, -1), (-1, -1), colors.HexColor('#E2EFDA')),
            ('ALIGN', (3, 0), (-1, -1), 'RIGHT'),
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
prop_address = addresses.get(property_id, 'N/A')
owner_pays_cleaning = owner_flags.get(property_id, {}).get('owner_pays_cleaning', False)

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

    # This will be calculated in the Net Revenue Section below
    # and we'll reference it here via a placeholder that gets updated
    if data['bookings']:
        st.markdown("*(See Net Revenue Section below for calculation details)*")

st.divider()

# NET REVENUE SECTION - Calculate totals for summary
if data['bookings']:
    # Load Guesty CSV to get total_payout (gross revenue before fees) and cleaning fees
    guesty_csv = BASE / "data/guesty_converted.csv"
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

    pm_fee_rate = float(prop.get('pm_fee_rate', 0.18)) if prop else 0.18
    booking_records = []
    total_gross_revenue = 0.0
    total_net_rental = 0.0
    total_comm = 0.0
    total_owner = 0.0
    total_expenses = 0.0

    for b in data['bookings']:
        net_rental = float(b['net_revenue'] or 0)
        booking_id = b['booking_id']

        # Gross revenue from converted Guesty CSV (total_payout before any fees)
        gross_revenue = gross_revenue_map.get(booking_id, net_rental)

        # Check booking status
        booking_status = str(b.get('status', 'confirmed')).lower()

        # For canceled bookings, cleaning fee is 0; otherwise get from Guesty
        if booking_status == 'canceled':
            cleaning_fee = 0.0
        else:
            cleaning_fee = cleaning_fee_map.get(booking_id, 0.0)

        comm = -net_rental * pm_fee_rate
        owner_rev = net_rental + comm

        # Add owner cleaning fee if owner_pays_cleaning is true
        if owner_pays_cleaning:
            owner_rev += cleaning_fee

        checkin = datetime.strptime(b['checkin'], '%Y-%m-%d')
        checkout = datetime.strptime(b['checkout'], '%Y-%m-%d')

        # For canceled bookings, show 0 nights instead of actual days
        if booking_status == 'canceled':
            nights = 0
        else:
            nights = (checkout - checkin).days

        record = {
            'Guest Name': b['guest_name'],
            'Confirmation Code': b['booking_id'],
            'Reservation Dates': f"{checkin.strftime('%d. %b.')} - {checkout.strftime('%d. %b. %Y')} / {nights} nights",
            'Gross Revenue': f"${gross_revenue:,.2f}",
            'Net Rental Revenue': f"${net_rental:,.2f}",
        }

        # Add Owner Cleaning Fee column if applicable
        if owner_pays_cleaning:
            record['Owner Cleaning Fee'] = f"${cleaning_fee:,.2f}"
            record['cleaning_fee_value'] = cleaning_fee  # Store numeric value for TOTAL calculation

        record['Management Commission'] = f"${comm:,.2f}"
        record['Net Owner Revenue'] = f"${owner_rev:,.2f}"
        record['Commission %'] = f"{-pm_fee_rate:.0%}"

        booking_records.append(record)

        total_gross_revenue += gross_revenue
        total_net_rental += net_rental
        total_comm += comm
        total_owner += owner_rev

    # Sum all expenses (already negative values)
    for exp in data['expenses']:
        total_expenses += float(exp['amount'] or 0)

    # NET INCOME = Total Net Owner Revenue + Total Expenses
    # (expenses are negative, so adding them subtracts the amount)
    net_income = total_owner + total_expenses
    current_balance = net_income

    # Update summary with calculated values
    with col_right:
        col_a, col_b = st.columns([0.6, 0.4])
        with col_a:
            st.markdown("**Starting Balance**")
        with col_b:
            st.markdown("$0.00")

        col_a, col_b = st.columns([0.6, 0.4])
        with col_a:
            st.markdown("**Net Income**")
        with col_b:
            st.markdown(f"<span style='color: #2e74b5; font-weight: bold;'>${net_income:,.2f}</span>", unsafe_allow_html=True)

        col_a, col_b = st.columns([0.6, 0.4])
        with col_a:
            st.markdown("**Current Balance**")
        with col_b:
            st.markdown(f"${current_balance:,.2f}")

        col_a, col_b = st.columns([0.6, 0.4])
        with col_a:
            st.markdown("**Owner Payout**")
        with col_b:
            st.markdown(f"${current_balance:,.2f}")

        col_a, col_b = st.columns([0.6, 0.4])
        with col_a:
            st.markdown("**Ending Balance**")
        with col_b:
            st.markdown(f"${current_balance:,.2f}")

        st.markdown("")
        if st.button("📥 Download PDF", use_container_width=True, key="pdf_btn"):
            pdf_bytes = generate_pdf(property_id, period, data)
            st.download_button(
                label="PDF Statement",
                data=pdf_bytes,
                file_name=f"{property_id}_{period}_statement.pdf",
                mime="application/pdf"
            )

    st.markdown("")

st.divider()

# Net Revenue Section - Display the calculated data
if data['bookings']:
    st.header("Net Revenue Section")

    total_record = {
        'Guest Name': '',
        'Confirmation Code': '',
        'Reservation Dates': 'TOTAL',
        'Gross Revenue': f"${total_gross_revenue:,.2f}",
        'Net Rental Revenue': f"${total_net_rental:,.2f}",
    }

    if owner_pays_cleaning:
        # Calculate total cleaning fee from all bookings
        total_cleaning_fee = sum(float(r.get('cleaning_fee_value', 0)) for r in booking_records if 'cleaning_fee_value' in r)
        total_record['Owner Cleaning Fee'] = f"${total_cleaning_fee:,.2f}"

    total_record['Management Commission'] = f"${total_comm:,.2f}"
    total_record['Net Owner Revenue'] = f"${total_owner:,.2f}"
    total_record['Commission %'] = f"{-pm_fee_rate:.0%}"

    booking_records.append(total_record)

    df_bookings = pd.DataFrame(booking_records)
    # Remove temporary cleaning_fee_value column used for calculations
    if 'cleaning_fee_value' in df_bookings.columns:
        df_bookings = df_bookings.drop(columns=['cleaning_fee_value'])

    # Style the dataframe to bold the last row
    def highlight_total(row):
        if row['Reservation Dates'] == 'TOTAL':
            return ['background-color: #e2efda; font-weight: bold'] * len(row)
        return [''] * len(row)

    styled_df = df_bookings.style.apply(highlight_total, axis=1)
    st.dataframe(styled_df, use_container_width=True, hide_index=True)

st.divider()

# Other Income
if data['other_income']:
    st.header("Other Credits")

    income_records = []
    total_oi = 0.0

    for inc in data['other_income']:
        amt = float(inc['amount'] or 0)
        income_records.append({
            'Date': inc['posting_date'],
            'Description': inc['description'][:100] if inc['description'] else '',
            'Type': inc['subcategory'] or '',
            'Amount': f"${amt:,.2f}",
        })
        total_oi += amt

    income_records.append({
        'Date': '',
        'Description': '',
        'Type': 'Total',
        'Amount': f"${total_oi:,.2f}",
    })

    df_income = pd.DataFrame(income_records)

    # Style the dataframe to bold the last row
    def highlight_income_total(row):
        if row['Type'] == 'Total':
            return ['background-color: #e2efda; font-weight: bold'] * len(row)
        return [''] * len(row)

    styled_income = df_income.style.apply(highlight_income_total, axis=1)
    st.dataframe(styled_income, column_config={
        'Date': st.column_config.Column(width=80),
        'Description': st.column_config.Column(width='large'),
        'Type': st.column_config.Column(width='medium'),
        'Amount': st.column_config.Column(width=80),
    }, use_container_width=True, hide_index=True)

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
        with st.expander(f"**{category}**", expanded=True):
            records = []
            total_cat = 0.0

            # Get expenses for this category, or empty list if none
            exps = expense_categories.get(category, [])
            for exp in exps:
                amt = float(exp['amount'] or 0)
                # Extract just the last part of the account path (after the last dash)
                account = exp['qbo_account'] or ''
                if ' - ' in account:
                    type_display = account.split(' - ')[-1]
                else:
                    type_display = account
                records.append({
                    'Date': exp['posting_date'],
                    'Description': exp['description'][:100] if exp['description'] else '',
                    'Type': type_display,
                    'Amount': f"${amt:,.2f}",
                })
                total_cat += amt

            records.append({
                'Date': '',
                'Description': '',
                'Type': 'Total',
                'Amount': f"${total_cat:,.2f}",
            })

            df_exp = pd.DataFrame(records)

            # Style the dataframe to bold the last row
            def highlight_exp_total(row):
                if row['Type'] == 'Total':
                    return ['background-color: #e2efda; font-weight: bold'] * len(row)
                return [''] * len(row)

            styled_exp = df_exp.style.apply(highlight_exp_total, axis=1)
            st.dataframe(styled_exp, column_config={
                'Date': st.column_config.Column(width=80),
                'Description': st.column_config.Column(width='large'),
                'Type': st.column_config.Column(width='medium'),
                'Amount': st.column_config.Column(width=80),
            }, use_container_width=True, hide_index=True)
