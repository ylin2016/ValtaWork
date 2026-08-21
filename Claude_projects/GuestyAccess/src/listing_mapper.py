"""Map a row of the prepared Excel sheet to a Guesty POST /listings payload.

The sheet has one listing per row. Columns are defined in COLUMNS below and
grouped into the same sections as the Guesty listing editor:
Details & layout, Pricing & policies, Marketing. `make_template.py` writes a
blank .xlsx with exactly these headers plus a `column_guide` sheet.

Only `nickname`, `address_full`, and `base_price` are strictly required. Every
other field is included only when its cell is non-empty, so a sparse sheet
produces a minimal-but-valid payload.

Multi-value cells (amenities, picture URLs, weekend days) are **`|`-separated**
(pipe), because descriptions and amenity names contain commas.

Notes on values that must match Guesty exactly (copy from an existing listing
in data/listings.json, NOT the API docs):
  - room_type      e.g. "Entire home/apt" (NOT "Entire home/apartment")
  - property_type  e.g. "Apartment", "House"
  - amenities      from Guesty's fixed vocabulary, e.g. "Beach access"
Times are 24h "HH:MM" (e.g. "16:00"). weekend_days are day numbers (Fri=5, Sat=6).

`checkInInstructions` / taxes / per-channel settings are complex nested objects
and are intentionally NOT exposed as flat columns — set those in the Guesty UI.

Docs: https://open-api-docs.guesty.com/docs/create-a-property
"""
import math

# (column, dtype, section, note). dtype drives cleaning; section/note document.
DETAILS = "Details & layout"
PRICING = "Pricing & policies"
MARKETING = "Marketing"
CORE = "Core / address"

COLUMNS = [
    # --- core identity + address ---
    ("nickname", "str", CORE, "REQUIRED. Internal name, unique."),
    ("is_listed", "bool", CORE, "TRUE to publish; default from config (false)."),
    ("address_full", "str", CORE, "REQUIRED. Full street address; Guesty parses the rest."),
    ("city", "str", CORE, "Optional override (parsed from address_full otherwise)."),
    ("state", "str", CORE, ""),
    ("zipcode", "str", CORE, ""),
    ("country", "str", CORE, ""),
    ("lat", "float", CORE, "Optional latitude."),
    ("lng", "float", CORE, "Optional longitude."),
    ("timezone", "str", CORE, "e.g. America/Los_Angeles; default from config."),

    # --- Details & layout ---
    ("property_type", "str", DETAILS, "Apartment, House, Villa, … (match a live listing)."),
    ("room_type", "str", DETAILS, 'Exact Guesty value, e.g. "Entire home/apt".'),
    ("accommodates", "int", DETAILS, "Guest capacity."),
    ("bedrooms", "float", DETAILS, ""),
    ("bathrooms", "float", DETAILS, ""),
    ("beds", "int", DETAILS, ""),
    ("area_square_feet", "int", DETAILS, "Living area in sq ft."),
    ("minimum_age", "int", DETAILS, "Minimum guest age, if enforced."),
    ("default_check_in_time", "str", DETAILS, '24h "HH:MM", e.g. 16:00.'),
    ("default_check_in_end_time", "str", DETAILS, '24h "HH:MM", e.g. 23:00.'),
    ("default_check_out_time", "str", DETAILS, '24h "HH:MM", e.g. 11:00.'),
    ("wifi_name", "str", DETAILS, "WiFi SSID."),
    ("wifi_password", "str", DETAILS, ""),
    ("door_code", "str", DETAILS, ""),
    ("lock_code", "str", DETAILS, ""),
    ("parking_instructions", "str", DETAILS, ""),
    ("house_manual", "str", DETAILS, "Free text (may be multi-line)."),
    ("amenities", "list", DETAILS, "'|'-separated. Must match Guesty vocab exactly."),
    ("amenities_not_included", "list", DETAILS, "'|'-separated amenities explicitly NOT offered."),

    # --- Pricing & policies ---
    ("base_price", "float", PRICING, "REQUIRED. Nightly base price."),
    ("currency", "str", PRICING, "ISO code; default from config (USD)."),
    ("weekend_base_price", "float", PRICING, ""),
    ("weekend_days", "listint", PRICING, "'|'-separated day numbers (Fri=5, Sat=6)."),
    ("cleaning_fee", "float", PRICING, ""),
    ("extra_person_fee", "float", PRICING, ""),
    ("guests_included", "int", PRICING, "Guests included before extra-person fee."),
    ("pet_fee", "float", PRICING, ""),
    ("security_deposit", "float", PRICING, ""),
    ("weekly_price_factor", "float", PRICING, "e.g. 0.9 = 10% weekly discount."),
    ("monthly_price_factor", "float", PRICING, "e.g. 0.9 = 10% monthly discount."),
    ("min_nights", "int", PRICING, ""),
    ("max_nights", "int", PRICING, ""),
    ("use_account_taxes", "bool", PRICING, "TRUE to inherit account-level taxes."),
    ("use_account_additional_fees", "bool", PRICING, "TRUE to inherit account-level fees."),

    # --- Marketing (publicDescription) ---
    ("title", "str", MARKETING, "Public/marketing title."),
    ("summary", "str", MARKETING, "publicDescription.summary — main blurb."),
    ("space", "str", MARKETING, "publicDescription.space — the space."),
    ("access", "str", MARKETING, "publicDescription.access — guest access."),
    ("neighborhood", "str", MARKETING, "publicDescription.neighborhood."),
    ("transit", "str", MARKETING, "publicDescription.transit — getting around."),
    ("notes", "str", MARKETING, "publicDescription.notes — other notes."),
    ("house_rules", "str", MARKETING, "publicDescription.houseRules."),
    ("interaction_with_guests", "str", MARKETING, "publicDescription.interactionWithGuests."),
    ("picture_urls", "list", MARKETING, "'|'-separated image URLs (>=5 recommended)."),
]

REQUIRED = ["nickname", "address_full", "base_price"]

# Excel column -> publicDescription.<key>
_PUBLIC_DESC = {
    "summary": "summary",
    "space": "space",
    "access": "access",
    "neighborhood": "neighborhood",
    "transit": "transit",
    "notes": "notes",
    "house_rules": "houseRules",
    "interaction_with_guests": "interactionWithGuests",
}


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


def _list(v, integer=False):
    """Split a '|'-separated cell into a clean list (ints if integer=True)."""
    parts = [p.strip() for p in str(v).split("|")]
    parts = [p for p in parts if p != ""]
    return [int(round(float(p))) for p in parts] if integer else parts


def row_to_payload(row: dict, defaults: dict, custom_field_map: dict | None = None) -> dict:
    """Build a Guesty listing payload from one sheet row. Raises on missing required fields.

    custom_field_map: optional {"cf_<displayName>": fieldId} so any non-empty
    `cf_*` cell becomes a customFields entry. See custom_fields.column_to_id.
    """
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

    # --- prices (Pricing & policies) ---
    prices = {
        "basePrice": _num(row["base_price"]),
        "currency": str(row["currency"]).strip() if has("currency") else defaults.get("currency", "USD"),
    }
    for col, key in [
        ("weekend_base_price", "weekendBasePrice"),
        ("cleaning_fee", "cleaningFee"),
        ("extra_person_fee", "extraPersonFee"),
        ("pet_fee", "petFee"),
        ("security_deposit", "securityDepositFee"),
        ("weekly_price_factor", "weeklyPriceFactor"),
        ("monthly_price_factor", "monthlyPriceFactor"),
    ]:
        if has(col):
            prices[key] = _num(row[col])
    if has("guests_included"):
        prices["guestsIncludedInRegularFee"] = _num(row["guests_included"], integer=True)
    if has("weekend_days"):
        prices["weekendDays"] = _list(row["weekend_days"], integer=True)

    # --- terms ---
    terms = {}
    if has("min_nights"):
        terms["minNights"] = _num(row["min_nights"], integer=True)
    if has("max_nights"):
        terms["maxNights"] = _num(row["max_nights"], integer=True)

    # --- publicDescription (Marketing) ---
    public_description = {}
    for col, key in _PUBLIC_DESC.items():
        if has(col):
            public_description[key] = str(row[col]).strip()

    # --- top-level ---
    payload = {
        "nickname": str(row["nickname"]).strip(),
        "address": address,
        "prices": prices,
        "isListed": _bool(row["is_listed"]) if has("is_listed") else bool(defaults.get("is_listed", False)),
        "timezone": str(row["timezone"]).strip() if has("timezone") else defaults.get("timezone", "America/New_York"),
    }
    if terms:
        payload["terms"] = terms
    if public_description:
        payload["publicDescription"] = public_description

    # simple str top-level fields
    for col, key in [
        ("title", "title"),
        ("property_type", "propertyType"),
        ("room_type", "roomType"),
        ("default_check_in_time", "defaultCheckInTime"),
        ("default_check_in_end_time", "defaultCheckInEndTime"),
        ("default_check_out_time", "defaultCheckOutTime"),
        ("wifi_name", "wifiName"),
        ("wifi_password", "wifiPassword"),
        ("door_code", "doorCode"),
        ("lock_code", "lockCode"),
        ("parking_instructions", "parkingInstructions"),
        ("house_manual", "houseManual"),
    ]:
        if has(col):
            payload[key] = str(row[col]).strip()

    # numeric top-level fields
    for col, key, integer in [
        ("accommodates", "accommodates", True),
        ("bedrooms", "bedrooms", False),
        ("bathrooms", "bathrooms", False),
        ("beds", "beds", True),
        ("area_square_feet", "areaSquareFeet", True),
        ("minimum_age", "minimumAge", True),
    ]:
        if has(col):
            payload[key] = _num(row[col], integer=integer)

    # boolean top-level fields
    for col, key in [
        ("use_account_taxes", "useAccountTaxes"),
        ("use_account_additional_fees", "useAccountAdditionalFees"),
    ]:
        if has(col):
            payload[key] = _bool(row[col])

    # list top-level fields
    if has("amenities"):
        payload["amenities"] = _list(row["amenities"])
    if has("amenities_not_included"):
        payload["amenitiesNotIncluded"] = _list(row["amenities_not_included"])
    if has("picture_urls"):
        payload["pictures"] = [{"original": u} for u in _list(row["picture_urls"])]

    # --- custom fields (cf_* columns -> customFields: [{fieldId, value}]) ---
    if custom_field_map:
        custom = []
        for col, val in row.items():
            if str(col).startswith("cf_") and not _blank(val):
                field_id = custom_field_map.get(col)
                if field_id:
                    custom.append({"fieldId": field_id, "value": str(val).strip()})
        if custom:
            payload["customFields"] = custom

    return payload
