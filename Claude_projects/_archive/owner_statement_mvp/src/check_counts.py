from src.config import load_config
from src.db import connect

cfg = load_config("config.yml")
conn = connect(cfg["app"]["db_path"])

tables = ["owners", "properties", "ledger_lines", "exceptions"]
for t in tables:
    n = conn.execute(f"select count(*) from {t}").fetchone()[0]
    print(f"{t}: {n}")

print("\nLedger by source_object:")
rows = conn.execute("""
    select source_object, count(*) as n, round(sum(amount), 2) as total
    from ledger_lines
    group by source_object
    order by n desc
""").fetchall()
for r in rows:
    print(dict(r))

print("\nLedger by property:")
rows = conn.execute("""
    select property_id, count(*) as n, round(sum(amount), 2) as total
    from ledger_lines
    group by property_id
    order by n desc
    limit 20
""").fetchall()
for r in rows:
    print(dict(r))

print("\nExceptions by code:")
rows = conn.execute("""
    select code, count(*) as n
    from exceptions
    group by code
    order by n desc
""").fetchall()
for r in rows:
    print(dict(r))