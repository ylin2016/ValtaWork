"""Owner-borne cleaning and pass-through tax for one booking, read from the QBO ledger.

The channel/Guesty/Stripe fee lookup that used to live here is GONE: the payment
breakdown (breakdown/payment_model, a transcription of payment_structure.xlsx) IS the
fee model, and it is the only place a fee is defined. Never reintroduce a QBO fee
lookup here — see CLAUDE.md, "STR net comes from the payment breakdown ONLY".
"""

def get_owner_costs(conn, property_id: str, booking_id: str, period_start: str, period_end: str,
                    checkin: str = None, checkout: str = None) -> dict:
    """
    Fetch owner cleaning and tax costs from QBO.
    Returns: {
        'owner_cleaning_cost': float (negative — a cost the owner bears),
        'owner_tax_cost': float (transient-occupancy tax PASSED TO the owner — positive),
    }
    """
    result = {'owner_cleaning_cost': 0.0, 'owner_tax_cost': 0.0}

    # Owner cleaning
    for row in conn.execute("""
        SELECT amount FROM ledger_lines
        WHERE property_id=? AND posting_date>=? AND posting_date<=?
          AND source='qbo' AND category='EXPENSE'
          AND qbo_account LIKE '%Owner Expenses%Cleaning%'
          AND description LIKE ?
    """, (property_id, period_start, period_end, f'%{booking_id}%')).fetchall():
        result['owner_cleaning_cost'] += float(row[0])

    # Owner tax (legacy: category='TAX' matched by booking_id)
    for row in conn.execute("""
        SELECT amount FROM ledger_lines
        WHERE property_id=? AND posting_date>=? AND posting_date<=?
          AND source='qbo' AND category='TAX'
          AND description LIKE ?
    """, (property_id, period_start, period_end, f'%{booking_id}%')).fetchall():
        result['owner_tax_cost'] += float(row[0])

    # Transient occupancy / lodging tax passed THROUGH to the owner (QBO account
    # 'Owner Income:Taxes Paid to Owners'). These lines carry the stay's date range
    # (e.g. "... | 2026-05-13 to 2026-05-19 | 6 nights") rather than the booking code,
    # so match on the booking's check-in/out dates. Stored positive (income to owner).
    if checkin and checkout:
        for row in conn.execute("""
            SELECT amount FROM ledger_lines
            WHERE property_id=? AND posting_date>=? AND posting_date<=?
              AND qbo_account LIKE '%Taxes Paid to Owners%'
              AND description LIKE ?
        """, (property_id, period_start, period_end, f'%{checkin} to {checkout}%')).fetchall():
            result['owner_tax_cost'] += float(row[0])

    return result
