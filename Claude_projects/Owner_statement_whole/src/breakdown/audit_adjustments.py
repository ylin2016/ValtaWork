"""Audit: is every manual fee adjustment actually present in the stored breakdown CSVs?

    python -m src.breakdown.audit_adjustments

`payment_breakdown_<p>.csv` is the stored fee model and `fetch_month` is its ONLY writer —
it overwrites the file from a fresh Guesty pull. So a correction that lives only in the CSV
is destroyed by the next pull of that period, silently: the CSVs are gitignored, so there is
no diff to review and no history to recover from. Every manual adjustment therefore belongs
in `payment_model`'s override tables, which `compute_breakdown` re-applies on every pull.

This checks the other direction — that what is ON DISK today matches those tables. It needs
no API token, so run it after any `payment_model` change and after any `fetch_month`:

  * MISSING   the override's booking is in the CSV but the CSV disagrees with the model.
              Usually means that period was pulled before the override existed -> re-pull it,
              or backfill the row (the value the model wants is printed).
  * dangling  an override code that appears in no stored CSV at all. Either its period was
              never pulled, or the booking was re-keyed/cancelled and the entry is dead.

Exit code is 1 when anything is MISSING, so it can gate a close.
"""
import sys

import pandas as pd

try:
    from .. import paths
    from .payment_model import (MANUAL_SERVICE_FEES, NO_PROCESSING_FEE_CODES,
                                STRIPE_FEE_OVERRIDES)
except ImportError:  # flat run
    import paths
    from breakdown.payment_model import (MANUAL_SERVICE_FEES, NO_PROCESSING_FEE_CODES,
                                         STRIPE_FEE_OVERRIDES)


def _stored_rows():
    """{confirmationCode: (period, row)} across every stored payment_breakdown CSV."""
    out = {}
    for path in sorted(paths.INPUTS_DIR.glob("*/payment_breakdown_*.csv")):
        period = path.parent.name
        for _, r in pd.read_csv(path).iterrows():
            out[str(r["confirmationCode"])] = (period, r)
    return out


def _checks(rows):
    """Yield (code, period, label, ok, detail) for every override that has a stored row."""
    for code, fee in sorted(STRIPE_FEE_OVERRIDES.items()):
        if code not in rows:
            continue
        period, r = rows[code]
        ok = abs(float(r["stripe_fee"]) - fee) < 0.005
        yield code, period, "Stripe fee from QBO", ok, f"stored {r['stripe_fee']}, want {fee}"

    for code in sorted(NO_PROCESSING_FEE_CODES):
        if code not in rows:
            continue
        period, r = rows[code]
        ok = abs(float(r["stripe_fee"])) < 0.005 and abs(float(r["guesty_fee"])) < 0.005
        yield (code, period, "no processing fee", ok,
               f"stored stripe {r['stripe_fee']} / guesty {r['guesty_fee']}, want 0.00 / 0.00")

    for code, fee in sorted(MANUAL_SERVICE_FEES.items()):
        if code not in rows:
            continue
        period, r = rows[code]
        # A manual booking's service fee IS its channel fee (owner, 2026-09-03), so it is
        # already inside the stored `channel_fee` — subtracting `fee` again here would
        # double it and report a correct row as MISSING. Check two things instead: the
        # waterfall foots, and channel_fee actually carries the fee.
        want = round(float(r["InvoiceItem"]) - float(r["cleaning_fee"]) - float(r["tax"])
                     - float(r["damage_waiver"]) - float(r["channel_fee"])
                     - float(r["stripe_fee"]) - float(r["guesty_fee"]), 2)
        in_cf = float(r["channel_fee"]) >= fee - 0.005
        ok = abs(float(r["net_revenue"]) - want) < 0.005 and in_cf
        yield (code, period, "manual service fee", ok,
               f"stored net {r['net_revenue']}, waterfall {want}; "
               f"channel_fee {r['channel_fee']} {'carries' if in_cf else 'MISSING'} the {fee} fee")


def main():
    rows = _stored_rows()
    if not rows:
        print("No stored payment_breakdown CSVs found — nothing to audit.")
        return 0

    all_codes = set(STRIPE_FEE_OVERRIDES) | set(NO_PROCESSING_FEE_CODES) | set(MANUAL_SERVICE_FEES)
    results = list(_checks(rows))
    bad = [x for x in results if not x[3]]

    print(f"Manual fee adjustments: {len(all_codes)} defined, {len(results)} with a stored row.\n")
    for code, period, label, ok, detail in results:
        print(f"  [{'ok ' if ok else 'MISSING'}] {period}  {code:24s} {label:22s} {detail}")

    dangling = sorted(all_codes - {x[0] for x in results})
    if dangling:
        print(f"\n  dangling ({len(dangling)}) — no stored CSV row; period not pulled, "
              f"or the booking is gone:")
        for code in dangling:
            print(f"    {code}")

    if bad:
        print(f"\n!! {len(bad)} adjustment(s) NOT reflected on disk — re-pull those periods "
              f"(python -m src.breakdown.fetch_month --period <p>) or backfill the row.")
        return 1
    print("\nEvery manual adjustment with a stored row is correctly reflected on disk.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
