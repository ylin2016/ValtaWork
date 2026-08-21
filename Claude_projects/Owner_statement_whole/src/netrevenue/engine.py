import uuid
from ..common.utils import now_iso
from ..scope.pm_rate import resolve_pm_fee_rate

# Commissionable rent: import_ltr LTR/DEFERRED rows OR plain QBO rent Deposits
# ("July Rent" etc.), but NOT garage/parking rent (excluded by name). Utilities/misc
# credits lack 'rent' in the description so they're excluded automatically. This is the
# ONE definition of commissionable rent — used for both the commission base and the
# per-listing PM-fee sum so they can't diverge.
_RENT_PRED = (
    "source='qbo' AND category='INCOME' AND ("
    "source_object IN ('LTR','DEFERRED') "
    "OR (lower(description) LIKE '%rent%' "
    "AND lower(description) NOT LIKE '%garage%' "
    "AND lower(description) NOT LIKE '%parking%'))"
)

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

def build_statements(conn, run_id: str, period_start: str, period_end: str, default_reserve_target: float = 0.0, rollups: dict = None, allowed: set = None, tax_props: set = None):
    # Clear any previous PM fees and reserves for this period before rebuilding
    conn.execute("""DELETE FROM ledger_lines
                    WHERE posting_date>=? AND posting_date<=?
                      AND category IN ('FEE', 'RESERVE') AND source='manual'""",
                 (period_start, period_end))
    conn.commit()

    rollups = rollups or {}
    rollup_children = {child for kids in rollups.values() for child in kids}

    props = conn.execute("SELECT property_id FROM properties WHERE is_active=1").fetchall()
    for pr in props:
        pid = pr["property_id"]

        # Only build listings present in Listing_contacts.csv (see listing_filter).
        if allowed is not None and pid not in allowed:
            continue

        # Member listings of a rollup are folded into their parent's statement.
        if pid in rollup_children:
            continue
        member_ids = [pid] + list(rollups.get(pid, []))
        ph = ",".join("?" * len(member_ids))

        # STR booking revenue (Guesty) — PM commission applies to GROSS (before channel fees)
        guesty_rev = _sum(conn, f"""SELECT SUM(amount) FROM ledger_lines
                              WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
                                AND include_in_statement=1 AND source='guesty' AND category='INCOME'""", (*member_ids, period_start, period_end))

        # Other income (all QBO INCOME): plain deposits/credits (garage/parking rent,
        # misc credits) PLUS the LTR/deferred rent rows imported as source='qbo'.
        other_income = _sum(conn, f"""SELECT SUM(amount) FROM ledger_lines
                              WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
                                AND include_in_statement=1 AND source='qbo' AND category='INCOME'""", (*member_ids, period_start, period_end))

        # Of that other income, RENT is commissionable — whether it arrived as an
        # import_ltr LTR/DEFERRED row OR as a plain QBO rent Deposit ("July Rent" etc.).
        # (Owner decision 2026-07: commission ALL rent, LTR or STR.) Non-rent credits —
        # utilities, garage/parking rent, misc/insurance/payroll deposits — are NOT
        # commissioned. `_RENT_PRED` is the single definition of commissionable rent,
        # reused for the per-line PM-fee sum below so the two can't drift.
        commissionable_other = _sum(conn, f"""SELECT SUM(amount) FROM ledger_lines
                              WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
                                AND include_in_statement=1 AND {_RENT_PRED}""", (*member_ids, period_start, period_end))

        gross_rev = guesty_rev + other_income

        taxes = _sum(conn, f"""SELECT SUM(amount) FROM ledger_lines
                          WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
                            AND include_in_statement=1 AND category='TAX'""", (*member_ids, period_start, period_end))

        # Transient occupancy / lodging tax collected from guests and passed THROUGH to
        # the owner (QBO account 'Owner Income:Taxes Paid to Owners'). It is owner income
        # (stored positive), shown as the "Tax Paid to Owner" column, and NOT commissioned.
        # Folded into `taxes` so net_before_reserve and the stored totals stay reconciled.
        # GATED on owner_pays_taxes: the dashboard/Excel only show the offsetting per-booking
        # "Tax Paid to Owner" column for flagged properties, so the engine must fold it in
        # only for those same properties or amount_due won't reconcile to the displayed rows.
        if tax_props is None or pid in tax_props:
            tax_to_owner = _sum(conn, f"""SELECT SUM(amount) FROM ledger_lines
                              WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
                                AND include_in_statement=1 AND qbo_account LIKE '%Taxes Paid to Owners%'""", (*member_ids, period_start, period_end))
            taxes = taxes + tax_to_owner

        # Only owner-responsibility expenses (account contains 'Owner')
        expenses = _sum(conn, f"""SELECT SUM(amount) FROM ledger_lines
                             WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
                               AND include_in_statement=1 AND category='EXPENSE'
                               AND qbo_account LIKE '%Owner Expenses%'""", (*member_ids, period_start, period_end))

        fees_other = _sum(conn, f"""SELECT SUM(amount) FROM ledger_lines
                               WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
                                 AND include_in_statement=1 AND category='FEE'""", (*member_ids, period_start, period_end))

        owner_adj = _sum(conn, f"""SELECT SUM(amount) FROM ledger_lines
                               WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
                                 AND include_in_statement=1 AND category='OWNER_ADJ'""", (*member_ids, period_start, period_end))

        pm_fee_rate = resolve_pm_fee_rate(conn, pid, period_start)
        c = conn.execute(
            """SELECT reserve_target FROM owner_contracts
               WHERE property_id=? AND effective_start<=?
                 AND (effective_end IS NULL OR effective_end>=?)
               ORDER BY effective_start DESC LIMIT 1""",
            (pid, period_start, period_start),
        ).fetchone()
        reserve_target = float(c["reserve_target"]) if c and c["reserve_target"] is not None else float(default_reserve_target)

        # PM fee applies to STR (guesty) booking revenue + ALL rent (LTR/deferred rows
        # AND plain QBO rent deposits). It is NOT charged on non-rent QBO credits
        # (utilities, garage/parking rent, misc/insurance/payroll deposits).
        commission_base = guesty_rev + commissionable_other
        # PM fee must equal the SUM of the per-line commissions shown on the statement —
        # the dashboard and Excel round each row's commission (round(-net_i*rate)) and sum
        # those. Applying the rate to the aggregate and rounding once drifts a penny from
        # that sum, which then makes stored Net Income disagree with (Net Owner Revenue −
        # Expenses) by $0.01. STR bookings commission PER BOOKING; rent commissions PER
        # LISTING (the statement shows one full-rent row per listing), so the granularity
        # of the rounded sum matches what each product displays.
        pm_fee = 0.0
        if commission_base > 0:
            if pm_fee_rate is None:
                raise ValueError(
                    f"No PM fee rate configured for property '{pid}' (period {period_start}), "
                    f"but it has ${commission_base:,.2f} of commissionable revenue. "
                    f"Set pm_fee_rate for it in mapping_classes.yml (then sync-mappings) / owner_contracts."
                )
            guesty_lines = conn.execute(f"""SELECT amount FROM ledger_lines
                              WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
                                AND include_in_statement=1 AND source='guesty' AND category='INCOME'""",
                              (*member_ids, period_start, period_end)).fetchall()
            rent_by_listing = conn.execute(f"""SELECT SUM(amount) amt FROM ledger_lines
                              WHERE property_id IN ({ph}) AND posting_date>=? AND posting_date<=?
                                AND include_in_statement=1 AND {_RENT_PRED}
                              GROUP BY property_id""",
                              (*member_ids, period_start, period_end)).fetchall()
            pm_fee = round(
                sum(round(-float(a or 0) * pm_fee_rate, 2) for (a,) in guesty_lines)
                + sum(round(-float(r["amt"] or 0) * pm_fee_rate, 2) for r in rent_by_listing),
                2)

        if abs(pm_fee) > 0.0001:
            conn.execute(
                """INSERT INTO ledger_lines
                   (ledger_id, source, source_object, source_txn_id, source_line_id, property_id,
                    posting_date, category, subcategory, description, amount, include_in_statement, status, last_updated_at)
                   VALUES (?, 'manual', 'Calc', ?, NULL, ?, ?, 'FEE', 'Property Management Fee', ?, ?, 1, 'posted', ?)""",
                (str(uuid.uuid4()), f"pm_fee_{run_id}_{pid}", pid, period_end, "Property Management Fee", pm_fee, now_iso()),
            )

        net_before_reserve = round(gross_rev + taxes + expenses + fees_other + owner_adj + pm_fee, 2)

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

        net_after_reserve = round(net_before_reserve + reserve_line, 2)
        amount_due = net_after_reserve

        # Store every total at cent precision so Excel, dashboard, and end_balances all
        # read penny-clean figures (each component is already ~2dp; round guards float drift).
        conn.execute(
            """INSERT OR REPLACE INTO statement_property_totals
               (run_id, property_id, gross_booking_revenue, taxes, total_expenses, total_fees,
                owner_adjustments, reserve_withheld, reserve_released, net_before_reserve,
                net_after_reserve, amount_due_to_owner)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, 0, ?, ?, ?)""",
            (
                run_id, pid,
                round(gross_rev, 2), round(taxes, 2), round(expenses, 2),
                round(fees_other + pm_fee, 2),
                round(owner_adj, 2),
                round(reserve_withheld, 2),
                net_before_reserve, net_after_reserve, amount_due
            ),
        )

    conn.commit()
