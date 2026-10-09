"""Code shared by every Claude_projects project (install once: pip install -e shared).

    valta_common.paths       where the shared data, reference tables and secrets live
    valta_common.guesty      Guesty Open API client (+ one cached token for all projects),
                             per-reservation financials, the fee-category summary frame
    valta_common.fees        payment_model: the fee rules (= the user-owned payment_structure.xlsx)
    valta_common.wheelhouse  Wheelhouse RM API client

The QuickBooks clients stay in their projects on purpose (Owner_statement_whole's is
read-only, QBO_operations' writes); only their token and credentials are shared.
"""
