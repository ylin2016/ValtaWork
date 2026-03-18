import uuid
import pandas as pd
from .utils import now_iso

def import_guesty_bookings_csv(conn, csv_path: str):
    df = pd.read_csv(csv_path)
    required = ["property_id","booking_id","channel","guest_name","checkin","checkout",
                "rent","cleaning_fee","other_fees","discount","refund","taxes","net_booking_revenue"]
    missing = [c for c in required if c not in df.columns]
    if missing:
        raise ValueError(f"Missing columns in Guesty CSV: {missing}")

    cur = conn.cursor()
    for _, r in df.iterrows():
        prop = str(r["property_id"])
        booking_id = str(r["booking_id"])
        posting_date = str(r["checkin"])[:10]
        amount = float(r["net_booking_revenue"])
        cur.execute(
            """INSERT INTO ledger_lines
               (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                booking_id, posting_date, category, subcategory, description, vendor_customer,
                qbo_account, amount, include_in_statement, status, last_updated_at)
               VALUES (?, 'guesty', 'Reservation', ?, NULL, ?, ?, ?, 'INCOME', 'Net Booking Revenue',
                       ?, ?, NULL, ?, 1, 'posted', ?)""",
            (
                str(uuid.uuid4()),
                booking_id,
                prop,
                booking_id,
                posting_date,
                f"{r.get('channel','')} {r.get('guest_name','')}".strip(),
                r.get("channel"),
                float(amount),
                now_iso(),
            ),
        )
    conn.commit()
