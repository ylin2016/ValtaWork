"""Single source of truth for the STATEMENT SCOPE: which owner statements exist,
and which listings roll into each one.

``config/Listing_contacts.csv`` is the owner-maintained standard. It has two
identity columns:

    Listing   the individual unit — matches a QBO class / Guesty listing
              (``property_id`` in the ledger).
    Property  the statement it belongs to. Every Listing sharing a Property is
              folded into ONE statement for that Property.

So "Seattle 10057 Lower / Upper / Whole" all report under the single "Seattle
10057" statement, "Yacinde B3/B4" under "Yacinde Holdings" and the rest of the
Yacinde units under "Yacinde NuGrowth". This module turns that file into the two
things the pipeline needs — the allowed statement set and the parent -> members
rollup map — so the build (``run_month_close``, ``netrevenue.engine``) and the
dashboard can never drift from the file or from each other.

The contacts labels do not always match the QBO class names exactly, so Listing
labels resolve through ``_norm``/``_alias`` (e.g. "Cottage 4" -> ``osbr_4``).
Property labels resolve by EXACT normalized match on an existing property name/id
and otherwise become a slug (``"Yacinde NuGrowth"`` -> ``yacinde_nugrowth``) — a
statement-only parent that carries no ledger lines of its own; see
``config/mapping_classes.yml`` for the rows that create them.

Older single-column contacts files (``Property`` only, one row per unit) still
parse: ``Listing`` falls back to ``Property``, which makes every row its own
statement exactly as before.
"""
import csv
import re

# Works both as a package module (run_month_close) and as a top-level import
# (the Streamlit dashboard inserts src/ on sys.path).
try:
    from ..paths import LISTING_CONTACTS
except ImportError:
    from paths import LISTING_CONTACTS

# Listings kept even though they are absent from Listing_contacts.csv because
# they still carry real ledger activity that would otherwise vanish from every
# statement (building-level shared costs / owner activity booked to a parent
# class). Revisit allocation to individual listings separately.
KEEP_EXTRA = {"beachwood", "seatac_12834"}


def _norm(s):
    return re.sub(r"[^a-z0-9]", "", (s or "").lower())


def _slug(s):
    """Contacts Property label -> a statement-parent property_id, for parents that
    are not themselves a QBO class ("Seattle 10057" -> ``seattle_10057``)."""
    return re.sub(r"[^a-z0-9]+", "_", (s or "").strip().lower()).strip("_")


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


def _label_to_pid(conn, active_only=False):
    """Normalized label -> property_id.

    Includes INACTIVE properties by default: a deactivated listing (``osbr_rv``)
    is still a valid rollup MEMBER whose ledger lines belong in its parent's
    statement. Only the statement parents themselves are restricted to active
    properties (see ``allowed_property_ids``).
    """
    sql = "SELECT property_id, property_name, is_active FROM properties"
    by_norm = {}
    for pid, name, is_active in conn.execute(sql):
        if active_only and not is_active:
            continue
        by_norm[_norm(name)] = pid
        by_norm.setdefault(_norm(pid), pid)
    return by_norm


def load_contacts():
    """Rows of Listing_contacts.csv, in file order."""
    with open(str(LISTING_CONTACTS), newline="", encoding="utf-8-sig") as f:
        return list(csv.DictReader(f))


def _listing_label(row):
    """The unit. Falls back to Property for the old single-column contacts file."""
    return (row.get("Listing") or row.get("Property") or "").strip()


def _property_label(row):
    """The statement the unit reports under."""
    return (row.get("Property") or row.get("Listing") or "").strip()


def resolve_scope(conn):
    """Turn Listing_contacts.csv into the statement scope.

    Returns (parents, rollups, parent_of):
      parents    {parent_property_id} — one per distinct contacts ``Property``.
      rollups    {parent_property_id: [member unit property_id, ...]} — the parent
                 itself is never listed as its own member.
      parent_of  {unit property_id: parent_property_id} for every resolved unit.

    A Listing that resolves to no known property_id is skipped (it has no ledger
    lines yet); a Property with no matching property either — the caller's
    ``allowed`` set only ever contains properties that exist and are active.
    """
    by_norm = _label_to_pid(conn)
    members, parents, parent_of = {}, [], {}

    for row in load_contacts():
        plabel = _property_label(row)
        if not plabel:
            continue
        pn = _norm(plabel)
        # Parent: EXACT match only — an alias would fold "Seattle 906" into the
        # Lower unit instead of giving the pair their own shared statement.
        parent = by_norm.get(pn) or _slug(plabel)
        if parent not in members:
            members[parent] = []
            parents.append(parent)

        llabel = _listing_label(row)
        ln = _norm(llabel)
        unit = by_norm.get(ln) or by_norm.get(_alias(ln))
        if unit is None:
            continue
        parent_of[unit] = parent
        if unit != parent and unit not in members[parent]:
            members[parent].append(unit)

    return set(parents), {p: m for p, m in members.items() if m}, parent_of


def statement_rollups(conn, config_rollups=None):
    """parent property_id -> member property_ids, derived from Listing_contacts.csv
    and MERGED with any legacy ``config.yml: statement_rollups`` entries.

    The contacts file wins wherever it speaks: a unit it places under a Property
    reports there, and a pid it lists as a member can no longer be a parent
    (``seattle_10057_whole`` becomes a member of ``seattle_10057``, so the legacy
    entry parented on it is re-pointed at ``seattle_10057``).

    Legacy members the file does not mention are KEPT rather than dropped —
    ``beachwood_6`` and the ``bellevue_14507_unit_*`` listings carry real ledger
    lines but have no contacts row, and silently un-rolling them would delete
    their money from the parent's statement.
    """
    _, derived, parent_of = resolve_scope(conn)
    out = {p: list(m) for p, m in derived.items()}

    for legacy_parent, legacy_members in (config_rollups or {}).items():
        # If the contacts file made this parent a member of something else, its
        # legacy members belong to that same statement now.
        target = parent_of.get(legacy_parent, legacy_parent)
        bucket = out.setdefault(target, [])
        for m in legacy_members:
            # Skip members the contacts file assigned to a DIFFERENT statement.
            if parent_of.get(m, target) != target:
                continue
            if m != target and m not in bucket:
                bucket.append(m)

    return {p: sorted(m) for p, m in out.items() if m}


def central_supply_property_ids(conn, base_dir):
    """property_ids whose Listing_contacts.csv ``Supplies`` column is 'central':
    these are charged a per-booking formula supply fee (0.9 * guests * nights)
    instead of the QBO per-booking 'Supplies Charge' lines.

    Resolved per LISTING, not per Property: the charge is posted against the unit
    that took the booking, and Supplies is set per unit in the file.
    """
    by_norm = _label_to_pid(conn)
    out = set()
    for row in load_contacts():
        if (row.get("Supplies") or "").strip().lower() != "central":
            continue
        n = _norm(_listing_label(row))
        pid = by_norm.get(n) or by_norm.get(_alias(n))
        if pid:
            out.add(pid)
    return out


# Listing_contacts.csv ``Status`` values that mean "this listing has ended".
RETIRED_STATUS = {"inactive", "retired", "ended", "terminated"}


def retired_parents(conn):
    """Statement parents whose contacts rows are ALL retired (``Status`` in
    RETIRED_STATUS). A Property with even one still-active Listing is NOT retired —
    the statement carries on for the units that remain."""
    by_norm = _label_to_pid(conn)
    seen, live = set(), set()
    for row in load_contacts():
        plabel = _property_label(row)
        if not plabel:
            continue
        parent = by_norm.get(_norm(plabel)) or _slug(plabel)
        seen.add(parent)
        if (row.get("Status") or "Active").strip().lower() not in RETIRED_STATUS:
            live.add(parent)
    return seen - live


def allowed_property_ids(conn, base_dir, period_start=None, period_end=None):
    """The set of ``property_id``s that receive an owner statement: one per
    distinct ``Property`` in Listing_contacts.csv (that exists and is active),
    plus the KEEP_EXTRA listings.

    Rollup MEMBERS are deliberately absent — they are folded into their parent's
    statement by ``statement_rollups`` and must not also produce one of their own.

    Pass `period_start`/`period_end` to apply RETIREMENT. The contacts file has no
    time dimension, so DELETING a row would erase that property's statement from
    every past month too (removing "Kirkland 10219" wiped $32,202.67 of statements
    across 17 historical periods). Marking it ``Status=Inactive`` instead keeps its
    history intact and stops it going forward: a retired Property is in scope only
    for periods where it has REAL activity — at least one ledger line with a
    nonzero amount. A wound-down listing keeps trailing $0 bookkeeping rows for
    months (Kirkland 10219 carries 17 zero-amount expense lines in 2026-07), and
    those must not keep minting all-zero statements. Income alone is too strict: a
    renovation month with costs and no bookings is still a statement the owner needs.

    Without the period arguments retired properties stay in scope, so a caller that
    has no period (a scope listing, a sanity check) still sees the full set.
    """
    active_ids = {pid for (pid,) in
                  conn.execute("SELECT property_id FROM properties WHERE is_active=1")}
    parents, rollups, _ = resolve_scope(conn)
    allowed = (parents | KEEP_EXTRA) & active_ids
    if period_start is None or period_end is None:
        return allowed

    for parent in sorted(retired_parents(conn) & allowed):
        ids = [parent] + list(rollups.get(parent, []))
        ph = ",".join("?" * len(ids))
        active = conn.execute(
            f"""SELECT COUNT(*) FROM ledger_lines
                 WHERE property_id IN ({ph}) AND include_in_statement=1
                   AND amount<>0
                   AND posting_date>=? AND posting_date<=?""",
            (*ids, period_start, period_end)).fetchone()[0]
        if not active:
            allowed.discard(parent)
    return allowed
