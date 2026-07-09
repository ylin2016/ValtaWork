"""Map a row of the prepared Excel sheet to a Guesty POST /listings payload.

The sheet has one listing per row. Column names are defined in COLUMNS below;
`make_template.py` writes a blank .xlsx with exactly these headers.

Only `nickname`, `address_full`, and `base_price` are strictly required by
Guesty. Everything else is included only when the cell is non-empty, so a
sparse sheet produces a minimal-but-valid payload.

Docs: https://open-api-docs.guesty.com/docs/create-a-property
"""
import math

# (column, dtype, note) — dtype drives cleaning; note is documentation only.
COLUMNS = [
    ("nickname", "str", "REQUIRED. Internal name, must be unique."),
    ("title", "str", "Public/marketing title."),
    ("is_listed", "bool", "TRUE to publish; default from config (false)."),
    ("address_full", "str", "REQUIRED. Full street address; Guesty parses the rest."),
    ("city", "str", ""),
    ("state", "str", ""),
    ("zipcode", "str", ""),
    ("country", "str", ""),
    ("lat", "float", "Optional latitude."),
    ("lng", "float", "Optional longitude."),
    ("timezone", "str", "e.g. America/New_York; default from config."),
    ("property_type", "str", "Apartment, House, Villa, Studio, …"),
    ("room_type", "str", "Entire home/apartment | Private room | Shared room"),
    ("accommodates", "int", "Guest capacity."),
    ("bedrooms", "float", ""),
    ("bathrooms", "float", ""),
    ("base_price", "float", "REQUIRED. Nightly base price."),
    ("currency", "str", "ISO code; default from config (USD)."),
    ("weekend_base_price", "float", ""),
    ("cleaning_fee", "float", ""),
    ("security_deposit", "float", ""),
    ("extra_person_fee", "float", ""),
    ("guests_included", "int", "Guests included before extra-person fee applies."),
    ("min_nights", "int", ""),
    ("max_nights", "int", ""),
]

REQUIRED = ["nickname", "address_full", "base_price"]


def _blank(v) -> bool:
    if v is None:
        return True
    if isinstance(v, float) and math.isnan(v):
        return True
    return str(v).strip() == ""


def _num(v, integer=False):
    n = float(v)
    return int(round(n)) if integer else n


def _bool(v) -> bool:
    return str(v).strip().lower() in ("true", "1", "yes", "y", "t")


def row_to_payload(row: dict, defaults: dict) -> dict:
    """Build a Guesty listing payload from one sheet row. Raises on missing required fields."""
    missing = [c for c in REQUIRED if _blank(row.get(c))]
    if missing:
        raise ValueError(f"Missing required field(s): {', '.join(missing)}")

    def has(col):
        return not _blank(row.get(col))

    # --- address ---
    address = {"full": str(row["address_full"]).strip()}
    for col, key in [("city", "city"), ("state", "state"), ("zipcode", "zipcode"), ("country", "country")]:
        if has(col):
            address[key] = str(row[col]).strip()
    if has("lat"):
        address["lat"] = _num(row["lat"])
    if has("lng"):
        address["lng"] = _num(row["lng"])

    # --- prices ---
    prices = {
        "basePrice": _num(row["base_price"]),
        "currency": str(row["currency"]).strip() if has("currency") else defaults.get("currency", "USD"),
    }
    for col, key in [
        ("weekend_base_price", "weekendBasePrice"),
        ("cleaning_fee", "cleaningFee"),
        ("security_deposit", "securityDepositFee"),
        ("extra_person_fee", "extraPersonFee"),
    ]:
        if has(col):
            prices[key] = _num(row[col])
    if has("guests_included"):
        prices["guestsIncludedInRegularFee"] = _num(row["guests_included"], integer=True)

    # --- top-level ---
    payload = {
        "nickname": str(row["nickname"]).strip(),
        "address": address,
        "prices": prices,
        "isListed": _bool(row["is_listed"]) if has("is_listed") else bool(defaults.get("is_listed", False)),
        "timezone": str(row["timezone"]).strip() if has("timezone") else defaults.get("timezone", "America/New_York"),
    }
    if has("title"):
        payload["title"] = str(row["title"]).strip()
    if has("property_type"):
        payload["propertyType"] = str(row["property_type"]).strip()
    if has("room_type"):
        payload["roomType"] = str(row["room_type"]).strip()
    if has("accommodates"):
        payload["accommodates"] = _num(row["accommodates"], integer=True)
    if has("bedrooms"):
        payload["bedrooms"] = _num(row["bedrooms"])
    if has("bathrooms"):
        payload["bathrooms"] = _num(row["bathrooms"])

    # --- terms (min/max nights) ---
    terms = {}
    if has("min_nights"):
        terms["minNights"] = _num(row["min_nights"], integer=True)
    if has("max_nights"):
        terms["maxNights"] = _num(row["max_nights"], integer=True)
    if terms:
        payload["terms"] = terms

    return payload
