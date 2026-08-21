import uuid
import re
from .mappings import apply_account_rules
from .utils import now_iso

def _parse_date(d: str) -> str:
    return d[:10] if d else None

def classify_deposit_line(description: str, entity_name: str | None = None) -> tuple[str, str]:
    text = f"{description or ''} {entity_name or ''}".lower().strip()

    if "security deposit" in text:
        return "TRANSFER", "Security Deposit Received"

    if "deposit refund" in text or "security deposit refund" in text:
        return "TRANSFER", "Security Deposit Refunded"

    if "deposit applied" in text or "applied deposit" in text:
        return "OWNER_ADJ", "Security Deposit Applied"

    if "rent" in text or "lease" in text:
        return "INCOME", "Lease Rent"

    return "INCOME", "Other Deposit"

def _all_pages(qbo, base_query: str, entity: str, page_size: int = 500) -> list:
    """Fetch all pages of a QBO query result."""
    results, pos = [], 1
    while True:
        res = qbo.query(base_query, start_position=pos, max_results=page_size)
        page = res.get("QueryResponse", {}).get(entity, []) or []
        results.extend(page)
        if len(page) < page_size:
            break
        pos += page_size
    return results


def sync_qbo_expenses(conn, qbo, property_map: dict, account_rules: list[dict], start_date: str, end_date: str, exceptions_cb=None):
    cur = conn.cursor()

    def exc(sev, code, msg, obj, line_id=None, property_id=None):
        if exceptions_cb:
            exceptions_cb(sev, code, msg, obj, line_id=line_id, property_id=property_id)

    # Expense
    q = f"select * from Purchase where TxnDate >= '{start_date}' and TxnDate <= '{end_date}'"
    expenses = _all_pages(qbo, q, "Purchase")
    for e in expenses:
        txn_id = e.get("Id")
        txn_date = _parse_date(e.get("TxnDate"))
        vendor = (e.get("VendorRef") or {}).get("name") or (e.get("PayeeRef") or {}).get("name")
        # Credit=True marks a vendor refund/return (e.g. a returned supply). Its amount
        # is a credit back to the owner, so it must be POSITIVE (reduces the expense
        # subtotal). A normal Purchase is a cost and stays negative.
        is_credit = bool(e.get("Credit"))
        lines = e.get("Line", []) or []
        for idx, ln in enumerate(lines, start=1):
            detail = ln.get("AccountBasedExpenseLineDetail") or {}
            class_ref = (detail.get("ClassRef") or {}).get("value")
            acct_name = ((detail.get("AccountRef") or {}).get("name"))
            amount = float(ln.get("Amount") or 0.0)

            if not class_ref:
                exc("error", "MISSING_CLASS", "Expense line missing ClassRef; excluded until classified.", e, line_id=str(idx))
                continue
            if class_ref not in property_map:
                exc("error", "CLASS_NOT_MAPPED", f"QBO Class {class_ref} not mapped to property_id.", e, line_id=str(idx))
                continue

            property_id = property_map[class_ref]
            category, subcat = apply_account_rules(acct_name or "", vendor or "", account_rules)

            cur.execute(
                """INSERT INTO ledger_lines
                   (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                    posting_date, category, subcategory, description, vendor_customer, qbo_account, amount,
                    include_in_statement, status, last_updated_at)
                   VALUES (?, 'qbo', 'Purchase', ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1, 'posted', ?)""",
                (
                    str(uuid.uuid4()),
                    str(txn_id),
                    str(idx),
                    property_id,
                    txn_date,
                    category,
                    subcat,
                    ln.get("Description"),
                    vendor,
                    acct_name,
                    abs(amount) if is_credit else -abs(amount),
                    now_iso(),
                ),
            )

    # Bill (accrual lines; if you want strict cash basis, add BillPayment linking next)
    qb = f"select * from Bill where TxnDate >= '{start_date}' and TxnDate <= '{end_date}'"
    bills = _all_pages(qbo, qb, "Bill")
    for b in bills:
        txn_id = b.get("Id")
        txn_date = _parse_date(b.get("TxnDate"))
        vendor = (b.get("VendorRef") or {}).get("name")
        lines = b.get("Line", []) or []
        for idx, ln in enumerate(lines, start=1):
            # Try AccountBasedExpenseLineDetail first (regular expenses)
            detail = ln.get("AccountBasedExpenseLineDetail") or {}
            class_ref = (detail.get("ClassRef") or {}).get("value")
            acct_name = ((detail.get("AccountRef") or {}).get("name"))
            amount = float(ln.get("Amount") or 0.0)

            # If no AccountBasedExpenseLineDetail, try other line types (e.g., tax lines)
            if not detail:
                # Try ItemBasedExpenseLineDetail (for tax items, etc.)
                detail = ln.get("ItemBasedExpenseLineDetail") or {}
                class_ref = (detail.get("ClassRef") or {}).get("value")
                item_ref = (detail.get("ItemRef") or {}).get("name")
                acct_name = item_ref  # Use item name as account name for tax
                amount = float(ln.get("Amount") or 0.0)

            if not class_ref:
                exc("warning", "MISSING_CLASS", "Bill line missing ClassRef; excluded until classified.", b, line_id=str(idx))
                continue
            if class_ref not in property_map:
                exc("warning", "CLASS_NOT_MAPPED", f"QBO Class {class_ref} not mapped to property_id.", b, line_id=str(idx))
                continue

            property_id = property_map[class_ref]
            category, subcat = apply_account_rules(acct_name or "", vendor or "", account_rules)

            cur.execute(
                """INSERT INTO ledger_lines
                   (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                    posting_date, category, subcategory, description, vendor_customer, qbo_account, amount,
                    include_in_statement, status, last_updated_at)
                   VALUES (?, 'qbo', 'Bill', ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1, 'posted', ?)""",
                (
                    str(uuid.uuid4()),
                    str(txn_id),
                    str(idx),
                    property_id,
                    txn_date,
                    category,
                    subcat,
                    ln.get("Description"),
                    vendor,
                    acct_name,
                    -abs(amount),
                    now_iso(),
                ),
            )

    # JournalEntry
    qj = f"select * from JournalEntry where TxnDate >= '{start_date}' and TxnDate <= '{end_date}'"
    jes = _all_pages(qbo, qj, "JournalEntry")
    for j in jes:
        txn_id = j.get("Id")
        txn_date = _parse_date(j.get("TxnDate"))
        lines = j.get("Line", []) or []
        for idx, ln in enumerate(lines, start=1):
            d = ln.get("JournalEntryLineDetail") or {}
            class_ref = (d.get("ClassRef") or {}).get("value")
            acct_name = ((d.get("AccountRef") or {}).get("name"))
            posting_type = d.get("PostingType")  # Debit/Credit
            amount = float(ln.get("Amount") or 0.0)

            if not class_ref:
                continue
            if class_ref not in property_map:
                exc("warning", "CLASS_NOT_MAPPED", f"QBO Class {class_ref} not mapped to property_id.", j, line_id=str(idx))
                continue

            property_id = property_map[class_ref]
            category, subcat = apply_account_rules(acct_name or "", "", account_rules)
            signed = -abs(amount) if posting_type == "Debit" else abs(amount)

            cur.execute(
                """INSERT INTO ledger_lines
                   (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                    posting_date, category, subcategory, description, vendor_customer, qbo_account, amount,
                    include_in_statement, status, last_updated_at)
                   VALUES (?, 'qbo', 'JournalEntry', ?, ?, ?, ?, ?, ?, ?, NULL, ?, ?, 1, 'posted', ?)""",
                (
                    str(uuid.uuid4()),
                    str(txn_id),
                    str(idx),
                    property_id,
                    txn_date,
                    category,
                    subcat,
                    ln.get("Description"),
                    acct_name,
                    signed,
                    now_iso(),
                ),
            )

    # Invoice (for tax charges and other guest-facing charges)
    qi = f"select * from Invoice where TxnDate >= '{start_date}' and TxnDate <= '{end_date}'"
    invoices = _all_pages(qbo, qi, "Invoice")
    for inv in invoices:
        txn_id = inv.get("Id")
        txn_date = _parse_date(inv.get("TxnDate"))
        customer = (inv.get("CustomerRef") or {}).get("name")
        lines = inv.get("Line", []) or []
        for idx, ln in enumerate(lines, start=1):
            # Try SalesItemLineDetail first (item-based lines like tax items)
            detail = ln.get("SalesItemLineDetail") or {}
            class_ref = (detail.get("ClassRef") or {}).get("value")
            item_ref = (detail.get("ItemRef") or {}).get("name")
            acct_name = item_ref
            amount = float(ln.get("Amount") or 0.0)

            # If no SalesItemLineDetail, try other types
            if not detail:
                detail = ln.get("DescriptionLineDetail") or {}
                acct_name = "Description Line"
                amount = 0.0

            if not class_ref:
                exc("warning", "MISSING_CLASS", "Invoice line missing ClassRef; excluded until classified.", inv, line_id=str(idx))
                continue
            if class_ref not in property_map:
                exc("warning", "CLASS_NOT_MAPPED", f"QBO Class {class_ref} not mapped to property_id.", inv, line_id=str(idx))
                continue

            property_id = property_map[class_ref]
            category, subcat = apply_account_rules(acct_name or "", customer or "", account_rules)

            cur.execute(
                """INSERT INTO ledger_lines
                   (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                    posting_date, category, subcategory, description, vendor_customer, qbo_account, amount,
                    include_in_statement, status, last_updated_at)
                   VALUES (?, 'qbo', 'Invoice', ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1, 'posted', ?)""",
                (
                    str(uuid.uuid4()),
                    str(txn_id),
                    str(idx),
                    property_id,
                    txn_date,
                    category,
                    subcat,
                    ln.get("Description"),
                    customer,
                    acct_name,
                    amount,
                    now_iso(),
                ),
            )

    # Deposit = bank deposits / receipts, useful for lease rent and security deposits
    qd = f"select * from Deposit where TxnDate >= '{start_date}' and TxnDate <= '{end_date}'"
    deposits = _all_pages(qbo, qd, "Deposit")

    for d in deposits:
        txn_id = d.get("Id")
        txn_date = _parse_date(d.get("TxnDate"))
        lines = d.get("Line", []) or []

        for idx, ln in enumerate(lines, start=1):
            detail = ln.get("DepositLineDetail") or {}
            linked = detail.get("LinkedTxn") or []

            # Class can be on the line detail
            class_ref = (detail.get("ClassRef") or {}).get("value")

            # Entity name can vary depending on source data
            entity_name = None
            entity_obj = detail.get("Entity") or {}
            if isinstance(entity_obj, dict):
                entity_name = entity_obj.get("name")

            if not entity_name:
                entity_name = (d.get("CustomerRef") or {}).get("name")

            amount = float(ln.get("Amount") or 0.0)
            desc = (ln.get("Description") or "").strip()

            if not class_ref:
                exc("warning", "MISSING_CLASS", "Deposit line missing ClassRef; excluded until classified.", d, line_id=str(idx))
                continue

            if class_ref not in property_map:
                exc("warning", "CLASS_NOT_MAPPED", f"QBO Class {class_ref} not mapped to property_id.", d, line_id=str(idx))
                continue

            property_id = property_map[class_ref]

            category, subcat = classify_deposit_line(desc, entity_name)

            cur.execute(
                """INSERT INTO ledger_lines
                   (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                    posting_date, category, subcategory, description, vendor_customer, qbo_account, amount,
                    include_in_statement, status, last_updated_at)
                   VALUES (?, 'qbo', 'Deposit', ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1, 'posted', ?)""",
                (
                    str(uuid.uuid4()),
                    str(txn_id),
                    str(idx),
                    property_id,
                    txn_date,
                    category,
                    subcat,
                    desc,
                    entity_name,
                    None,
                    abs(amount),
                    now_iso(),
                ),
            )

    conn.commit()
