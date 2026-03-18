import uuid
from .utils import now_iso

def create_run(conn, period_start: str, period_end: str, basis: str) -> str:
    run_id = str(uuid.uuid4())
    conn.execute(
        """INSERT INTO statement_runs(run_id, period_start, period_end, basis, status)
           VALUES (?, ?, ?, ?, 'draft')""",
        (run_id, period_start, period_end, basis),
    )
    conn.commit()
    return run_id

def _sum(conn, sql: str, params: tuple) -> float:
    row = conn.execute(sql, params).fetchone()
    return float(row[0] or 0.0)

def build_statements(conn, run_id: str, period_start: str, period_end: str, default_pm_fee_rate: float = 0.171, default_reserve_target: float = 0.0):
    props = conn.execute("SELECT property_id FROM properties WHERE is_active=1").fetchall()
    for pr in props:
        pid = pr["property_id"]

        gross_rev = _sum(conn, """SELECT SUM(amount) FROM ledger_lines
                              WHERE property_id=? AND posting_date>=? AND posting_date<=?
                                AND include_in_statement=1 AND category='INCOME'""", (pid, period_start, period_end))

        taxes = _sum(conn, """SELECT SUM(amount) FROM ledger_lines
                          WHERE property_id=? AND posting_date>=? AND posting_date<=?
                            AND include_in_statement=1 AND category='TAX'""", (pid, period_start, period_end))

        expenses = _sum(conn, """SELECT SUM(amount) FROM ledger_lines
                             WHERE property_id=? AND posting_date>=? AND posting_date<=?
                               AND include_in_statement=1 AND category='EXPENSE'""", (pid, period_start, period_end))

        fees_other = _sum(conn, """SELECT SUM(amount) FROM ledger_lines
                               WHERE property_id=? AND posting_date>=? AND posting_date<=?
                                 AND include_in_statement=1 AND category='FEE'""", (pid, period_start, period_end))

        owner_adj = _sum(conn, """SELECT SUM(amount) FROM ledger_lines
                               WHERE property_id=? AND posting_date>=? AND posting_date<=?
                                 AND include_in_statement=1 AND category='OWNER_ADJ'""", (pid, period_start, period_end))

        c = conn.execute(
            """SELECT * FROM owner_contracts
               WHERE property_id=? AND effective_start<=?
                 AND (effective_end IS NULL OR effective_end>=?)
               ORDER BY effective_start DESC LIMIT 1""",
            (pid, period_start, period_start),
        ).fetchone()

        pm_fee_rate = float(c["pm_fee_rate"]) if c and c["pm_fee_rate"] is not None else float(default_pm_fee_rate)
        reserve_target = float(c["reserve_target"]) if c and c["reserve_target"] is not None else float(default_reserve_target)

        pm_fee = -abs(gross_rev) * pm_fee_rate

        if abs(pm_fee) > 0.0001:
            conn.execute(
                """INSERT INTO ledger_lines
                   (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                    posting_date, category, subcategory, description, amount, include_in_statement, status, last_updated_at)
                   VALUES (?, 'manual', 'Calc', ?, NULL, ?, ?, 'FEE', 'Property Management Fee', ?, ?, 1, 'posted', ?)""",
                (str(uuid.uuid4()), f"pm_fee_{run_id}_{pid}", pid, period_end, "Property Management Fee", pm_fee, now_iso()),
            )

        net_before_reserve = gross_rev + taxes + expenses + fees_other + owner_adj + pm_fee

        begin_reserve = 0.0
        needed = max(0.0, reserve_target - begin_reserve)
        reserve_withheld = min(net_before_reserve, needed) if (net_before_reserve > 0 and needed > 0) else 0.0
        reserve_line = -abs(reserve_withheld) if reserve_withheld > 0 else 0.0

        if abs(reserve_line) > 0.0001:
            conn.execute(
                """INSERT INTO ledger_lines
                   (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                    posting_date, category, subcategory, description, amount, include_in_statement, status, last_updated_at)
                   VALUES (?, 'manual', 'Calc', ?, NULL, ?, ?, 'RESERVE', 'Reserve Withheld', ?, ?, 1, 'posted', ?)""",
                (str(uuid.uuid4()), f"reserve_{run_id}_{pid}", pid, period_end, "Reserve Withheld", reserve_line, now_iso()),
            )

        net_after_reserve = net_before_reserve + reserve_line
        amount_due = net_after_reserve

        conn.execute(
            """INSERT OR REPLACE INTO statement_property_totals
               (run_id, property_id, gross_booking_revenue, taxes, total_expenses, total_fees,
                owner_adjustments, reserve_withheld, reserve_released, net_before_reserve,
                net_after_reserve, amount_due_to_owner)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, 0, ?, ?, ?)""",
            (
                run_id, pid,
                gross_rev, taxes, expenses,
                fees_other + pm_fee,
                owner_adj,
                reserve_withheld,
                net_before_reserve, net_after_reserve, amount_due
            ),
        )

    conn.commit()
