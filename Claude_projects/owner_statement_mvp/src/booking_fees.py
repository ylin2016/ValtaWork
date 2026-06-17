"""
Look up QBO fees (channel + stripe) and owner costs from QBO bills.
"""

def get_qbo_fees(conn, property_id: str, booking_id: str, checkin: str, checkout: str, period_start: str, period_end: str) -> dict:
    """
    Fetch channel fees, stripe/credit card fees, tax (Booking.com only), and fee deductions from QBO.
    Tries direct booking_id match first, then date range match for platforms with numeric IDs.
    Returns: {
        'channel_fee': float (negative),
        'stripe_fee': float (negative),
        'tax': float (negative, for Booking.com only from QBO Invoices),
        'channel_fee_deduction': float (negative),
        'stripe_fee_deduction': float (negative),
    }
    Note: Deductions are deduplicated per type (only counted once even if multiple QBO entries exist).
    Tax is summed for all entries matching the booking date range (Booking.com Invoices have multiple tax line items).
    """
    result = {'channel_fee': 0.0, 'stripe_fee': 0.0, 'tax': 0.0, 'channel_fee_deduction': 0.0, 'stripe_fee_deduction': 0.0}
    seen_deductions = set()  # Track which deduction amounts we've already added

    # Try direct booking_id match (works for VRBO/HA-*, Airbnb/HM*, etc.)
    # Also try date range match for Booking.com (which uses numeric IDs in QBO)
    query_results = conn.execute("""
        SELECT qbo_account, amount, description, category FROM ledger_lines
        WHERE property_id=? AND posting_date>=? AND posting_date<=?
          AND source='qbo' AND source_object='Bill'
          AND description LIKE ?
    """, (property_id, period_start, period_end, f'%{booking_id}%')).fetchall()

    # If no direct match, try date range (for Booking.com which has numeric IDs in QBO)
    if not query_results and checkin and checkout:
        query_results = conn.execute("""
            SELECT qbo_account, amount, description, category FROM ledger_lines
            WHERE property_id=? AND posting_date>=? AND posting_date<=?
              AND source='qbo' AND source_object='Bill'
              AND (description LIKE ? OR description LIKE ?)
        """, (property_id, checkin, checkout, f'%{checkin}%{checkout}%', f'%Booking.com%')).fetchall()

    seen_fees = set()  # Track fees to avoid duplicates
    for row in query_results:
        acct = str(row[0] or '')
        amt = float(row[1])
        desc = str(row[2] or '').lower()
        cat = str(row[3] or '')

        if 'deduction' in desc:
            deduction_key = (desc.split('|')[0].strip(), abs(amt))  # Group by deduction type and amount
            if deduction_key not in seen_deductions:
                seen_deductions.add(deduction_key)
                if 'channel' in desc or 'vrbo' in desc:
                    result['channel_fee_deduction'] += amt
                elif 'stripe' in desc:
                    result['stripe_fee_deduction'] += amt
        else:
            fee_key = (desc.split('|')[0].strip(), abs(amt))  # Deduplicate by description and amount
            if fee_key not in seen_fees:
                seen_fees.add(fee_key)
                if 'channel' in desc:
                    result['channel_fee'] += amt
                elif 'stripe' in desc:
                    result['stripe_fee'] += amt

    # For Booking.com: also match tax from Invoices by date range (tax items don't include booking_id)
    if checkin and checkout:
        for row in conn.execute("""
            SELECT qbo_account, amount, description, category FROM ledger_lines
            WHERE property_id=? AND posting_date>=? AND posting_date<=?
              AND source='qbo' AND source_object='Invoice'
              AND (description LIKE ? OR description LIKE ?)
        """, (property_id, checkin, checkout, f'%{checkin}%{checkout}%', f'%{checkin}%')).fetchall():
            acct = str(row[0] or '')
            amt = float(row[1])
            desc = str(row[2] or '').lower()
            cat = str(row[3] or '')

            # Only count if description mentions tax
            if 'tax' in desc.lower():
                result['tax'] += amt

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
