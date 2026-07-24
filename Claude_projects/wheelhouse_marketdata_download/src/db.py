import sqlite3
from pathlib import Path

SCHEMA_PATH = str(Path(__file__).resolve().parent.parent / "schema.sql")


def connect(db_path: str) -> sqlite3.Connection:
    """Open (creating parent dirs if needed) and initialize the SQLite DB."""
    Path(db_path).parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row
    conn.execute("PRAGMA foreign_keys = ON;")
    init_schema(conn)
    return conn


def init_schema(conn: sqlite3.Connection, schema_path: str = SCHEMA_PATH) -> None:
    conn.executescript(Path(schema_path).read_text(encoding="utf-8"))
    conn.commit()


def upsert(conn: sqlite3.Connection, table: str, rows: list[dict]) -> int:
    """INSERT OR REPLACE a list of uniform dict rows. Returns rows written.

    Idempotent because every table's PRIMARY KEY includes snapshot_date, so
    re-running a pull for the same week overwrites rather than duplicates.
    """
    if not rows:
        return 0
    cols = list(rows[0].keys())
    placeholders = ", ".join(["?"] * len(cols))
    collist = ", ".join(cols)
    sql = f"INSERT OR REPLACE INTO {table} ({collist}) VALUES ({placeholders})"
    conn.executemany(sql, [tuple(r.get(c) for c in cols) for r in rows])
    conn.commit()
    return len(rows)


def tables(conn: sqlite3.Connection) -> list[str]:
    cur = conn.execute(
        "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
    )
    return [r[0] for r in cur.fetchall()]


def latest_snapshot_date(conn: sqlite3.Connection) -> str | None:
    cur = conn.execute("SELECT MAX(snapshot_date) FROM raw_responses")
    row = cur.fetchone()
    return row[0] if row else None


def _snapshot_tables(conn: sqlite3.Connection) -> list[str]:
    """Tables that have a snapshot_date column."""
    out = []
    for t in tables(conn):
        cols = [r[1] for r in conn.execute(f"PRAGMA table_info({t})")]
        if "snapshot_date" in cols:
            out.append(t)
    return out


def prune_snapshots(conn: sqlite3.Connection, keep: int | None) -> list[str]:
    """Keep only the newest `keep` snapshot_dates; delete older rows everywhere.

    keep=None (or <=0) keeps everything. Returns the snapshot_dates removed.
    """
    if not keep or keep <= 0:
        return []
    snaps = [r[0] for r in conn.execute(
        "SELECT DISTINCT snapshot_date FROM listings ORDER BY snapshot_date DESC")]
    drop = snaps[keep:]
    if not drop:
        return []
    marks = ",".join("?" * len(drop))
    for t in _snapshot_tables(conn):
        conn.execute(f"DELETE FROM {t} WHERE snapshot_date IN ({marks})", drop)
    conn.commit()
    conn.execute("VACUUM")
    return drop
