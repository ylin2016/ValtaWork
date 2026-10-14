"""Void named QBO Payments / Invoices (a one-off correction). DRY RUN unless --confirm.

    python -m src.void_txn --code X Payment:120595=502.42 Invoice:120594=502.42
    python -m src.void_txn --code X Payment:120595=502.42 Invoice:120594=502.42 --confirm

Each target carries the amount the owner reviewed; it is refused if the live object's total
differs or it is already voided. Targets are voided in the order given -- put a Payment
before the Invoice it pays (an Invoice with a payment linked is refused). Voiding keeps the
record (amount 0, "Voided" memo) -- nothing is deleted. Logged to review/CHANGE_LOG.csv.
"""
from __future__ import annotations

import argparse

from .post_invoices import Poster, Refused


def void(poster: Poster, code: str, entity: str, oid: str, amount: float) -> str:
    live = poster.read(entity, oid)
    if abs(float(live["TotalAmt"]) - amount) > 0.005:
        raise Refused(f"{entity} {oid} total is {live['TotalAmt']}, not the reviewed {amount:.2f}")
    if "Voided" in (live.get("PrivateNote") or ""):
        raise Refused(f"{entity} {oid} is already voided")
    linked = {x["TxnId"] for x in live.get("LinkedTxn") or [] if x["TxnType"] == "Payment"}
    if entity == "Invoice" and linked - poster.voided:      # a dry run has not voided it yet
        raise Refused(f"Invoice {oid} still has {live['LinkedTxn']} linked -- void the payment first")
    if entity == "Payment":
        poster.voided.add(oid)
    doc = live.get("DocNumber") or live.get("PaymentRefNum") or ""
    if not poster.confirm:
        return f"would VOID {entity} {oid} {doc} {amount:.2f} ({live['TxnDate']}, {live['CustomerRef']['name']})"
    if entity == "Payment":
        params, body = {"operation": "update", "include": "void"}, {"Id": oid, "SyncToken": live["SyncToken"],
                                                                     "sparse": True}
    else:
        params, body = {"operation": "void"}, {"Id": oid, "SyncToken": live["SyncToken"]}
    r = poster.qbo.request("POST", f"/v3/company/{poster.qbo.realm_id}/{entity.lower()}",
                           params=params, json_body=body)[entity]
    poster.log({"code": code}, entity, "VOID", r["Id"], doc, amount)
    return f"VOID {entity} {oid} {doc} {amount:.2f}"


def main():
    ap = argparse.ArgumentParser(description="Void QBO Payments / Invoices (dry run unless --confirm).")
    ap.add_argument("targets", nargs="+", help="Entity:Id=amount, e.g. Payment:120595=502.42")
    ap.add_argument("--code", required=True, help="reservation code, for the change log")
    ap.add_argument("--confirm", action="store_true")
    a = ap.parse_args()
    poster = Poster(a.confirm)
    poster.voided = set()
    for t in a.targets:
        ent_id, amt = t.split("=")
        ent, oid = ent_id.split(":")
        if ent not in ("Payment", "Invoice"):
            raise SystemExit(f"unsupported entity {ent}")
        try:
            print(" ", void(poster, a.code, ent, oid, float(amt)))
        except Refused as e:
            print(f"  REFUSED: {e}")
            raise SystemExit(1)


if __name__ == "__main__":
    main()
