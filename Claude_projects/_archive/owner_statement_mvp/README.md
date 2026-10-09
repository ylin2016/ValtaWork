Owner Statement MVP (SQLite + QBO Class-based)

What you can do in v1:
- Sync QBO Expenses/Bills/JEs (line-level) into a canonical ledger using ClassRef => property_id
- Import Guesty booking economics via CSV (stub) into the same ledger
- Calculate PM fee + reserve (config/contract-driven)
- Generate per-property Excel owner statements from a template

Quick start
1) pip install -r requirements.txt
2) python -m src.run_month_close init-db
3) Edit config.yml and set qbo.realm_id to your realmId
4) Fill mapping_classes.yml with your real classes/properties/owners
5) python -m src.run_month_close sync-mappings
6) QBO OAuth (one-time):
   python -m src.run_month_close qbo-auth
   Then:
   python -m src.run_month_close qbo-exchange --code <CODE_FROM_REDIRECT_URL>
7) Sync QBO:
   python -m src.run_month_close qbo-sync --start 2026-02-01 --end 2026-02-29
8) Guesty CSV import:
   python -m src.run_month_close guesty-import --csv ./templates/guesty_booking_example.csv
9) Build statements:
   python -m src.run_month_close build --period 2026-02

Output:
- output/YYYY-MM/statements/{property_id}_owner_statement_YYYY-MM.xlsx

Notes:
- This MVP focuses on getting a correct class-based expense ledger + statement math.
- Payout reconciliation and bill-payment cash basis linkage can be added next.

Steps in details:

source venv/bin/activate
python -m pip install -r requirements.txt
python -m src.run_month_close init-db
    This created:
        ./data/owner_statement.sqlite
python -m src.run_month_close qbo-exchange --code "REAL_CODE"
    Result:
        tokens saved to ./data/qbo_tokens.json

python -m src.list_classes    # pull classes from qbo
python -m src.export_class_stubs # write mapping_classes yml
cp mapping_classes_stub.yml mapping_classes.yml
python -m src.run_month_close sync-mappings
python -m src.run_month_close qbo-sync --start 2026-02-01 --end 2026-02-28
python -m src.check_counts

python -m src.convert_guesty_export \
  --input ./templates/Guesty_bookings_2026-20260310.csv \
  --output ./data/guesty_converted.csv
