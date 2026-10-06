"""Small QBO query helpers."""


def esc(s: str) -> str:
    """Escape a value for a QBO query string literal."""
    return str(s).replace("'", "\\'")
