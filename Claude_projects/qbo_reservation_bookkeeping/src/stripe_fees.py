"""The Stripe fee a reservation actually paid (owner decision 2026-10-04: actual fees).

Source of truth is the itemized Stripe payout files the owner drops in inputs/stripe/
(`<amount> Payout.csv`: Charge / Refund / Adjustment rows, `Fees` = Stripe + Guesty 1%).
Each Guesty SUCCEEDED payment carries its Stripe charge id, so payments are matched to
those rows by `ch_…` id -- exact, no amount guessing.

A payment whose charge is not in any payout file yet (an older deposit, or one not yet
paid out) is estimated from its own data: amount x 2.9% (4.4% on a non-US card -- the card
country is on the charge) + $0.30 + the Guesty application fee actually taken. An unpaid
balance is estimated the same way, so the owner's bill is not understated meanwhile. Every
re-run replaces estimates with actuals as payout files arrive; the review CSV says which
reservation is fully actual.
"""
from __future__ import annotations

import csv
import re
from dataclasses import dataclass, field
from functools import lru_cache

from . import bridge
from .paths import INPUTS_DIR

STRIPE_DIR = INPUTS_DIR / "stripe"
PER_CHARGE = 0.30
GUESTY_RATE = 0.01


@dataclass
class StripeFiles:
    charges: dict[str, dict] = field(default_factory=dict)        # ch_ id -> row
    app_fee_refunds: dict[str, float] = field(default_factory=dict)  # ch_ id -> refunded fee
    by_code: dict[str, list[str]] = field(default_factory=dict)   # code -> [ch_ ids]


@lru_cache(maxsize=1)
def load() -> StripeFiles:
    sf = StripeFiles()
    for p in sorted(STRIPE_DIR.glob("*Payout.csv")):
        with p.open(encoding="utf-8-sig") as fh:
            for row in csv.DictReader(fh):
                typ, cid = row["Type"], row["ID"]
                if typ == "Charge":
                    code = (row.get("confirmationCode (metadata)") or row.get("Description") or "").strip()
                    sf.charges[cid] = {"code": code, "amount": float(row["Amount"]),
                                       "fees": float(row["Fees"]), "file": p.name}
                    sf.by_code.setdefault(code, []).append(cid)
                elif typ == "Adjustment":
                    m = re.search(r"Application fee refund for (ch_\w+)", row.get("Description") or "")
                    if m:
                        sf.app_fee_refunds[m.group(1)] = sf.app_fee_refunds.get(m.group(1), 0.0) + float(row["Amount"])
    return sf


def _charges(payment: dict) -> list[dict]:
    out = []
    for a in payment.get("attempts") or []:
        pl = a.get("payload") or {}
        out += (pl.get("charges") or {}).get("data") or []
        if pl.get("object") == "charge":
            out.append(pl)
        # Newer PaymentIntents no longer embed the charge -- only its id.
        if pl.get("object") == "payment_intent" and pl.get("latest_charge") \
                and not (pl.get("charges") or {}).get("data"):
            out.append({"id": pl["latest_charge"],
                        "application_fee_amount": pl.get("application_fee_amount")})
    return [c for c in out if c.get("id")]


def _stripe_rate(card_country: str | None, intl_default: bool) -> float:
    pm = bridge.payment_model()
    if card_country:
        return pm.STRIPE_RATE if card_country.upper() == "US" else pm.STRIPE_RATE_INTERNATIONAL
    return pm.STRIPE_RATE_INTERNATIONAL if intl_default else pm.STRIPE_RATE


def _estimate(amount: float, rate: float, guesty_fee: float | None = None) -> float:
    g = round(amount * GUESTY_RATE, 2) if guesty_fee is None else guesty_fee
    return round(amount * rate + PER_CHARGE, 2) + g


def _charged_at(payment: dict) -> str:
    """UTC ISO time the card was charged: the Stripe object's own `created`, else Guesty's."""
    from datetime import datetime, timezone
    for a in payment.get("attempts") or []:
        ts = (a.get("payload") or {}).get("created")
        if ts:
            return datetime.fromtimestamp(int(ts), timezone.utc).strftime("%Y-%m-%dT%H:%M:%S")
    return str(payment.get("paidAt") or payment.get("createdAt") or "")


def reservation_fee(r: dict, model_fee: float, cutoff: str) -> dict:
    """The Stripe fee Bill amount, in three named parts:

    pre_cutoff  payments charged BEFORE VRP's cutoff and absent from our payout files --
                fees computed with VRP's own per-payment formula (what VRP posted, where it
                kept up; it missed some -- the builder compares against VRP's posted Bill)
    actual      charges found in the Stripe payout files
    estimate    charged after the cutoff but not yet in a payout file, plus any unpaid
                balance -- replaced by actuals on a later run
    """
    out = {"fee": 0.0, "pre_cutoff": 0.0, "actual": 0.0, "estimate": 0.0, "source": "none",
           "detail": ""}
    if model_fee <= 0.005:
        out["detail"] = "no card processing (payment_model)"
        return out
    sf = load()
    code = r["confirmationCode"]
    pm = bridge.payment_model()
    intl_default = (code in pm.STRIPE_INTERNATIONAL_CARD
                    or str(r.get("source", "")).lower() in pm.STRIPE_INTERNATIONAL_SOURCES) \
        and code not in pm.STRIPE_DOMESTIC_CARD
    money = r.get("money") or {}
    seen: set[str] = set()
    notes = []
    for p in money.get("payments") or []:
        if str(p.get("status")).upper() != "SUCCEEDED":
            continue
        amt = float(p.get("amount") or 0)
        chs = _charges(p)
        hit = [c for c in chs if c["id"] in sf.charges]
        if hit:
            for c in hit:
                seen.add(c["id"])
                out["actual"] += sf.charges[c["id"]]["fees"] - sf.app_fee_refunds.get(c["id"], 0.0)
                notes.append(f"{amt:.2f} actual")
            continue
        c = chs[0] if chs else {}
        country = ((c.get("payment_method_details") or {}).get("card") or {}).get("country")
        app = c.get("application_fee_amount")
        fee = _estimate(amt, _stripe_rate(country, intl_default), None if app is None else app / 100.0)
        when = _charged_at(p)
        bucket = "pre_cutoff" if when and when < cutoff else "estimate"
        out[bucket] += fee
        notes.append(f"{amt:.2f} {when[:10]} {'pre-cutoff' if bucket == 'pre_cutoff' else 'est.'}")
    for cid in sf.by_code.get(code, []):          # a charge Guesty does not list
        if cid not in seen:
            out["actual"] += sf.charges[cid]["fees"] - sf.app_fee_refunds.get(cid, 0.0)
            notes.append(f"{sf.charges[cid]['amount']:.2f} actual (not in Guesty)")
    balance = float(money.get("balanceDue") or 0)
    if balance > 0.005:
        out["estimate"] += _estimate(balance, _stripe_rate(None, intl_default))
        notes.append(f"{balance:.2f} unpaid est.")
    if not notes:
        out.update(fee=round(model_fee, 2), estimate=round(model_fee, 2), source="model",
                   detail="no payment data -- payment_model estimate")
        return out
    for k in ("pre_cutoff", "actual", "estimate"):
        out[k] = round(out[k], 2)
    out["fee"] = round(out["pre_cutoff"] + out["actual"] + out["estimate"], 2)
    out["source"] = "final" if out["estimate"] == 0 else "has estimate"
    out["detail"] = "; ".join(notes)
    return out
