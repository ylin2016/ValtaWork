"""
Look up QBO fees (channel + stripe) and owner costs from QBO bills.
"""

def get_qbo_fees(conn, property_id: str, booking_id: str, checkin: str, checkout: str, period_start: str, period_end: str) -> dict:
    """
    Fetch channel fees and stripe/credit card fees from QBO.
    Tries direct booking_id match first, then date range match for platforms with numeric IDs.
    Returns: {
        'channel_fee': float (negative),
        'stripe_fee': float (negative),
    }
    """
    result = {'channel_fee': 0.0, 'stripe_fee': 0.0}

    # Try direct booking_id match (works for VRBO/HA-*, Airbnb/HM*, etc.)
    for row in conn.execute("""
        SELECT qbo_account, amount FROM ledger_lines
        WHERE property_id=? AND posting_date>=? AND posting_date<=?
          AND source='qbo' AND source_object='Bill' AND category='EXPENSE'
          AND description LIKE ?
    """, (property_id, period_start, period_end, f'%{booking_id}%')).fetchall():
        acct = str(row[0] or '')
        amt = float(row[1])
        if 'Channel Fees' in acct or 'channel' in acct.lower():
            result['channel_fee'] += amt
        elif 'Credit Card Fees' in acct or 'stripe' in acct.lower():
            result['stripe_fee'] += amt

    # For platforms with numeric IDs in QBO (Booking.com), match by date range
    if result['channel_fee'] == 0.0 and result['stripe_fee'] == 0.0 and checkin and checkout:
        for row in conn.execute("""
            SELECT qbo_account, amount FROM ledger_lines
            WHERE property_id=? AND posting_date=?
              AND source='qbo' AND source_object='Bill' AND category='EXPENSE'
              AND (description LIKE ? OR description LIKE ?)
        """, (property_id, checkin, f'%{checkin}%{checkout}%', f'%{checkin}%')).fetchall():
            acct = str(row[0] or '')
            amt = float(row[1])
            if 'Channel Fees' in acct or 'channel' in acct.lower():
                result['channel_fee'] += amt
            elif 'Credit Card Fees' in acct or 'stripe' in acct.lower():
                result['stripe_fee'] += amt

    return result


def get_owner_costs(conn, property_id: str, booking_id: str, period_start: str, period_end: str) -> dict:
    """
    Fetch owner cleaning and tax costs from QBO.
    Returns: {
        'owner_cleaning_cost': float (negative),
        'owner_tax_cost': float (negative),
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

    # Owner tax
    for row in conn.execute("""
        SELECT amount FROM ledger_lines
        WHERE property_id=? AND posting_date>=? AND posting_date<=?
          AND source='qbo' AND category='TAX'
          AND description LIKE ?
    """, (property_id, period_start, period_end, f'%{booking_id}%')).fetchall():
        result['owner_tax_cost'] += float(row[0])

    return result
