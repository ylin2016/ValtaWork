"""Listing-label -> property_id, and nothing else.

Split out of ``ltr.records`` so the map can be imported without pulling in pandas.
``records`` re-exports it, so every existing caller is unaffected and there is
still exactly ONE definition -- the rule that keeps the importer, the Booking
Breakdown and the QBO_operations JE builders agreeing on which unit a label means.

Dependency-free on purpose (only ``re``), like ``common/income_rules.py``: it is
imported by the display-only deploy bundle and by the sibling QBO_operations
project, neither of which should need the pipeline's dependencies.
"""
import re

_ALIASES = {"bellevue 14507u3": "bellevue_14507_unit_3"}


def to_property_id(listing) -> str:
    """LTR CSV ``Listing`` label -> property_id. THE single definition — the
    importer (``ltr.import_ltr``) reads it from here, so a label that lands in the
    ledger always lands in the Booking Breakdown too. (They used to be two copies;
    only one knew "Cottage 8", so Cottage 8's rent was commissioned in Section 2
    but had no Section-1 row and the two sections stopped footing.)"""
    s = str(listing).strip().lower()
    if s in _ALIASES:
        return _ALIASES[s]
    s = re.sub(r"[^a-z0-9]+", "_", s)
    s = re.sub(r"_+", "_", s).strip("_")
    # "Cottage 8" -> osbr_8, the same rule scope/listing_filter._alias applies.
    m = re.fullmatch(r"cottage_(\d+)", s)
    if m:
        return f"osbr_{m.group(1)}"
    return s
