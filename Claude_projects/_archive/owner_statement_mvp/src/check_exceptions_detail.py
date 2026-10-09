from src.config import load_config
from src.db import connect

cfg = load_config("config.yml")
conn = connect(cfg["app"]["db_path"])

rows = conn.execute("""
    select exception_id, source, source_object, source_txn_id, severity, code, message
    from exceptions
    limit 20
""").fetchall()

for r in rows:
    print(dict(r))