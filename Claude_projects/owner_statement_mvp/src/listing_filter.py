"""Single source of truth for which listings receive owner statements.

Owner statements are generated only for listings present in
``data/Listing_contacts.csv``. This module maps the contacts file's "Property"
labels to ``property_id``s (handling the few cases where the contacts label
differs from the QBO-derived property name/id) and is imported by the build
(``run_month_close``, ``statement_engine``) and the dashboard so the two can't
drift.
"""
import csv
import os
import re

# Listings kept even though they are absent from Listing_contacts.csv because
# they still carry real ledger activity that would otherwise vanish from every
# statement (building-level shared costs / owner activity booked to a parent
# class). Revisit allocation to individual listings separately.
KEEP_EXTRA = {"beachwood", "seatac_12834"}


def _norm(s):
    return re.sub(r"[^a-z0-9]", "", (s or "").lower())


def _alias(n):
    """Map a normalized contacts label to the normalized property name/id it
    actually refers to, for the cases where the contacts label and the
    QBO-derived listing differ."""
    m = re.fullmatch(r"cottage(\d+)", n)
    if m:
        return f"osbr{m.group(1)}"
    return {
        "cottageallosbr": "osbr",
        "osbrall": "osbr",
        "cottage11tiny": "osbr11",
        "seattle7434": "seattle7434whole",
        # VRP lists this unit as bare "Seattle 906"; map it to the Lower unit
        # (Upper is LTR). Contacts use "Seattle 906 Lower/Upper" directly.
        "seattle906": "seattle906lower",
        # "Bellevue 2323 Whole" is the contacts label for the parent class
        # (property_id bellevue_2323); Main/ADU resolve to their own listings.
        "bellevue2323whole": "bellevue2323",
    }.get(n, n)


def allowed_property_ids(conn, base_dir):
    """Return the set of ``property_id``s that should receive owner statements:
    every active property whose name (or id) matches a row in
    ``Listing_contacts.csv`` (with aliases), plus the KEEP_EXTRA listings."""
    by_norm = {}
    active_ids = set()
    for pid, name in conn.execute(
        "SELECT property_id, property_name FROM properties WHERE is_active=1"
    ):
        by_norm[_norm(name)] = pid
        by_norm.setdefault(_norm(pid), pid)
        active_ids.add(pid)

    allowed = {p for p in KEEP_EXTRA if p in active_ids}

    csv_path = os.path.join(str(base_dir), "data", "Listing_contacts.csv")
    with open(csv_path, newline="", encoding="utf-8-sig") as f:
        for row in csv.DictReader(f):
            label = (row.get("Property") or "").strip()
            if not label:
                continue
            n = _norm(label)
            pid = by_norm.get(n) or by_norm.get(_alias(n))
            if pid:
                allowed.add(pid)
    return allowed
