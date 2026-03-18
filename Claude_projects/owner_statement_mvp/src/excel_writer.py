from pathlib import Path
import pandas as pd
from openpyxl import Workbook, load_workbook
from openpyxl.utils import get_column_letter

def _autosize(ws, max_width=60):
    for col in ws.columns:
        maxlen = 0
        col_letter = get_column_letter(col[0].column)
        for cell in col:
            val = "" if cell.value is None else str(cell.value)
            maxlen = max(maxlen, len(val))
        ws.column_dimensions[col_letter].width = min(max_width, max(10, maxlen + 2))

def create_template(path: str):
    wb = Workbook()

    ws = wb.active
    ws.title = "Setup"
    setup_rows = [
        ("Statement Period Start", ""),
        ("Statement Period End", ""),
        ("Property ID", ""),
        ("Property Name", ""),
        ("Owner Name", ""),
        ("Owner Email", ""),
        ("QBO Class ID", ""),
        ("PM Fee Type", ""),
        ("PM Fee Rate", ""),
        ("PM Flat Fee", ""),
        ("Reserve Target", ""),
        ("Beginning Reserve Balance", 0),
        ("Beginning Owner Balance", 0),
        ("Cleaning Fee to Owner?", "Yes"),
        ("Tax Included in Revenue?", "No"),
        ("Statement Version", "v1"),
    ]
    for i, (k, v) in enumerate(setup_rows, start=1):
        ws[f"A{i}"] = k
        ws[f"B{i}"] = v
    ws.column_dimensions["A"].width = 28
    ws.column_dimensions["B"].width = 40

    bd = wb.create_sheet("Booking Detail (Guesty)")
    bd_cols = ["Property ID","Booking ID","Channel","Guest Name","Check-in","Check-out","Booked Date","Status",
               "Rent","Cleaning Fee","Other Fees","Discount","Refund","Taxes","Channel Fee","Net Booking Revenue","Notes"]
    bd.append(bd_cols)

    ex = wb.create_sheet("Expenses (QBO)")
    ex_cols = ["Property ID","QBO Class ID","Txn Type","Txn ID","Line ID","Txn Date","Vendor","Payee",
               "Account","Category","Subcategory","Description / Memo","Amount","Include?","Notes"]
    ex.append(ex_cols)

    fa = wb.create_sheet("Fees & Adjustments")
    fa_cols = ["Property ID","Type","Subcategory","Description","Amount","Source","Include?"]
    fa.append(fa_cols)

    s = wb.create_sheet("Summary")
    s["A1"] = "Owner Statement Summary"
    s["A3"] = "Property"
    s["B3"] = "=Setup!B4"
    s["A4"] = "Period"
    s["B4"] = '=Setup!B1&" to "&Setup!B2'

    s["A6"] = "Gross Booking Revenue"
    s["B6"] = "=SUMIF('Booking Detail (Guesty)'!A:A, Setup!B3, 'Booking Detail (Guesty)'!P:P)"
    s["A7"] = "Taxes (informational)"
    s["B7"] = "=SUMIF('Booking Detail (Guesty)'!A:A, Setup!B3, 'Booking Detail (Guesty)'!N:N)"

    s["A9"] = "Total Operating Expenses"
    s["B9"] = "=SUMIFS('Expenses (QBO)'!M:M, 'Expenses (QBO)'!A:A, Setup!B3, 'Expenses (QBO)'!N:N, \"Y\")"

    s["A11"] = "Total Fees"
    s["B11"] = "=SUMIFS('Fees & Adjustments'!E:E, 'Fees & Adjustments'!A:A, Setup!B3, 'Fees & Adjustments'!B:B, \"FEE\", 'Fees & Adjustments'!G:G, \"Y\")"
    s["A12"] = "Owner Adjustments"
    s["B12"] = "=SUMIFS('Fees & Adjustments'!E:E, 'Fees & Adjustments'!A:A, Setup!B3, 'Fees & Adjustments'!B:B, \"OWNER_ADJ\", 'Fees & Adjustments'!G:G, \"Y\")"
    s["A13"] = "Reserve (net)"
    s["B13"] = "=SUMIFS('Fees & Adjustments'!E:E, 'Fees & Adjustments'!A:A, Setup!B3, 'Fees & Adjustments'!B:B, \"RESERVE\", 'Fees & Adjustments'!G:G, \"Y\")"

    s["A15"] = "Net Proceeds"
    s["B15"] = "=B6 + B9 + B11 + B12 + B13"

    s.column_dimensions["A"].width = 26
    s.column_dimensions["B"].width = 30
    wb.save(path)

def write_statement(output_path: str, template_path: str, setup: dict, booking_df: pd.DataFrame, expenses_df: pd.DataFrame, fees_df: pd.DataFrame):
    wb = load_workbook(template_path)

    ws = wb["Setup"]
    key_to_row = {ws[f"A{i}"].value: i for i in range(1, 40) if ws[f"A{i}"].value}
    for k, v in setup.items():
        if k in key_to_row:
            ws[f"B{key_to_row[k]}"] = v

    bd = wb["Booking Detail (Guesty)"]
    if booking_df is not None and len(booking_df) > 0:
        for _, row in booking_df.iterrows():
            bd.append(list(row.values))

    ex = wb["Expenses (QBO)"]
    if expenses_df is not None and len(expenses_df) > 0:
        for _, row in expenses_df.iterrows():
            ex.append(list(row.values))

    fa = wb["Fees & Adjustments"]
    if fees_df is not None and len(fees_df) > 0:
        for _, row in fees_df.iterrows():
            fa.append(list(row.values))

    _autosize(bd)
    _autosize(ex)
    _autosize(fa)

    Path(output_path).parent.mkdir(parents=True, exist_ok=True)
    wb.save(output_path)
