# Owner Statement Dashboard

A web-based dashboard for viewing and downloading Valta Realty owner statements.

## Features

✅ **Property Selector** - Browse all active properties with sidebar navigation
✅ **Period Selector** - View statements for any available month (2026-05 onwards)
✅ **Summary Cards** - Quick overview of net income, balances, and payouts
✅ **Net Revenue Section** - Detailed booking-by-booking breakdown with fees and commission
✅ **Expense Categories** - Collapsible sections for repairs, supplies, utilities, etc.
✅ **PDF Download** - Generate and download statements as PDF files

## Installation

```bash
pip install -r requirements.txt
```

## Running the Dashboard

```bash
streamlit run src/dashboard.py
```

Then open your browser to `http://localhost:8501`

## Features Details

### Sidebar Navigation
- **Period Dropdown**: Select statement month (displayed as "Month Year")
- **Property Dropdown**: Select from all active properties (displayed as property name)
- Auto-loaded from database - updates as new statements are generated

### Summary Cards
- **Starting Balance**: Previous month's ending balance
- **Net Income**: Amount due to owner (gross revenue - PM fee - expenses)
- **Current Balance**: Starting + Net Income
- **Owner Payout**: Amount owner receives this period

### Net Revenue Section
Displays all bookings for the property in the selected period:
- **Reservation Dates**: Check-in to checkout with night count
- **Confirmation Code**: Booking ID from Guesty (HA-*, BC-*, HVBM-*, etc.)
- **Guest Name**: Booking guest
- **Net Rental Revenue**: Accommodation + pet + extra guest - channel/stripe fees
- **Management Commission**: PM fee (18% by default)
- **Net Owner Revenue**: Net rental + commission (negative)
- **Commission %**: -18% (shown for reference)

### Other Credits
Shows QBO deposits (lease rent, credits, refunds, etc.)

### Expenses
Grouped by category with collapsible sections:
- Repairs & Maintenance
- Cleaning Labor
- Supplies
- Utilities
- Internet/Cable
- HOA
- Insurance
- Property Taxes
- Other Expense

Each section shows date, description, type, and amount. Totals calculated per category.

### PDF Download
- Button in top-right corner
- Generates clean PDF with header, summary, bookings, and expenses
- File named: `{property_id}_{period}_statement.pdf`
- Includes all key data from the web view

## Data Sources

Dashboard reads from:
- **SQLite Database** (`data/owner_statement.sqlite`): Property info, statement totals, ledger lines
- **Excel Files** (optional): `output/{period}/statements/{property_id}_owner_statement_{period}.xlsx`

Database is populated by the CLI pipeline:
```bash
python -m src.run_month_close build --period 2026-05
```

## Styling

- Color scheme matches Excel statements (#1F4E79 dark blue, #2E74B5 medium blue, #BDD7EE light blue)
- Font size 9-10pt for readability on screens
- Responsive layout with collapsible sections for large datasets
- Summary cards use custom styling for quick visual scanning

## Customization

Edit `dashboard.py` to modify:
- PM fee rate (currently hardcoded as 0.18)
- Period format (currently 'YYYY-MM')
- Expense category ordering
- PDF styling (fonts, colors, layout)

## Troubleshooting

**"No properties found"**
- Run the CLI build command first: `python -m src.run_month_close build --period 2026-05`
- Ensure database at `data/owner_statement.sqlite` exists

**"PDF download fails"**
- Install reportlab: `pip install reportlab>=4.0.0`
- Check file permissions for output directory

**"Sidebar not showing"**
- Use `--logger.level=debug` flag: `streamlit run dashboard.py --logger.level=debug`
- Try refreshing browser (Cmd+R or Ctrl+R)

## Future Enhancements

- [ ] Multi-property comparison view
- [ ] Expense trend charts (month-over-month)
- [ ] Owner email integration (send statement link)
- [ ] Payment status tracking (paid, pending, etc.)
- [ ] Custom date range selector
- [ ] Excel export with formatting preserved
