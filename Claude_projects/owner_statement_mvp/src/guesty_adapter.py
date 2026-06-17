import uuid
import pandas as pd
from .utils import now_iso

def import_guesty_bookings_csv(conn, csv_path: str):
    df = pd.read_csv(csv_path, encoding="utf-8-sig")
    required = ["property_id", "booking_id", "channel", "guest_name", "checkin", "checkout",
                "net_revenue"]
    missing = [c for c in required if c not in df.columns]
    if missing:
        raise ValueError(f"Missing columns in Guesty CSV: {missing}")

    known_props = {row[0] for row in conn.execute("SELECT property_id FROM properties").fetchall()}

    cur = conn.cursor()
    imported = skipped = unknown = 0

    for _, r in df.iterrows():
        prop = str(r["property_id"])
        booking_id = str(r["booking_id"])
        posting_date = str(r["checkin"])[:10]
        service_date = str(r["checkout"])[:10] if pd.notna(r.get("checkout")) else None
        amount = float(r["net_revenue"])
        guest_name = str(r.get("guest_name") or "").strip()
        channel = str(r.get("channel") or "").strip()
        status = str(r.get("status") or "confirmed").strip().lower()  # "confirmed" or "canceled"

        if prop not in known_props:
            unknown += 1
            continue

        if cur.execute("SELECT 1 FROM ledger_lines WHERE source='guesty' AND source_txn_id=?",
                       (booking_id,)).fetchone():
            skipped += 1
            continue

        cur.execute(
            """INSERT INTO ledger_lines
               (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                booking_id, posting_date, service_date, category, subcategory, description,
                vendor_customer, qbo_account, amount, include_in_statement, status, last_updated_at)
               VALUES (?, 'guesty', 'Reservation', ?, NULL, ?, ?, ?, ?, 'INCOME', ?,
                       'Booking', ?, NULL, ?, 1, ?, ?)""",
            (str(uuid.uuid4()), booking_id, prop, booking_id, posting_date, service_date,
             channel, guest_name, float(amount), status, now_iso()),
        )
        imported += 1

    conn.commit()
    print(f"Guesty import: {imported} imported, {skipped} duplicates skipped, {unknown} unknown properties skipped.")
