"""
Look up channel fees and owner costs (cleaning, tax) for each booking from QBO.
"""
import re

def extract_booking_id_from_description(desc):
    """Extract booking confirmation code from QBO bill description."""
    if not desc:
        return None
    # Try patterns: "code | 2026-05-01" or "code | 2026-05 to"
    match = re.search(r'\b([A-Z0-9]{8,})\b', desc)
    return match.group(1) if match else None

def get_booking_fees(conn, property_id: str, booking_id: str, period_start: str, period_end: str) -> dict:
    """
    Fetch channel fees, stripe fees, owner cleaning, owner tax for a booking.
    Returns: {
        'booking_channel_fee': float (negative),
        'booking_stripe_fee': float (negative),
        'owner_cleaning_cost': float (negative),
        'owner_tax_cost': float (negative),
    }
    """
    result = {
        'booking_channel_fee': 0.0,
        'booking_stripe_fee': 0.0,
        'owner_cleaning_cost': 0.0,
        'owner_tax_cost': 0.0,
    }

    # Channel fees (Booking.com, VRBO, etc.)
    for row in conn.execute("""
        SELECT amount FROM ledger_lines
        WHERE property_id=? AND posting_date>=? AND posting_date<=?
          AND source='qbo' AND source_object='Bill' AND category='EXPENSE'
          AND (qbo_account LIKE '%Booking.com Commission%' OR qbo_account LIKE '%VRBO Commission%')
          AND (description LIKE ? OR description LIKE ?)
    """, (property_id, period_start, period_end, f'%{booking_id}%', f'%{booking_id}%')).fetchall():
        result['booking_channel_fee'] += float(row[0])

    # Stripe fees
    for row in conn.execute("""
        SELECT amount FROM ledger_lines
        WHERE property_id=? AND posting_date>=? AND posting_date<=?
          AND source='qbo' AND category='EXPENSE'
          AND qbo_account LIKE '%Stripe%'
          AND description LIKE ?
    """, (property_id, period_start, period_end, f'%{booking_id}%')).fetchall():
        result['booking_stripe_fee'] += float(row[0])

    # Owner cleaning costs (if owner pays)
    for row in conn.execute("""
        SELECT amount FROM ledger_lines
        WHERE property_id=? AND posting_date>=? AND posting_date<=?
          AND source='qbo' AND category='EXPENSE'
          AND qbo_account LIKE '%Cleaning%'
          AND description LIKE ?
    """, (property_id, period_start, period_end, f'%{booking_id}%')).fetchall():
        result['owner_cleaning_cost'] += float(row[0])

    # Owner tax (if owner pays)
    for row in conn.execute("""
        SELECT amount FROM ledger_lines
        WHERE property_id=? AND posting_date>=? AND posting_date<=?
          AND source='qbo' AND category='TAX'
          AND description LIKE ?
    """, (property_id, period_start, period_end, f'%{booking_id}%')).fetchall():
        result['owner_tax_cost'] += float(row[0])

    return result
