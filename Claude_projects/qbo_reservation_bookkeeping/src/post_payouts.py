"""Post a reviewed payout plan (review/payouts_plan_<stamp>.json). DRY RUN unless --confirm.

    python -m src.post_payouts --id M-XXXX                 # dry run, one payout
    python -m src.post_payouts --id M-XXXX --confirm       # write it
    python -m src.post_payouts --confirm                   # every CREATE payout in the plan

Per payout: its reservation Payments (Dr clearing / Cr A/R, applied to the invoice), then
its payout JE (Dr Chase 9967 / Cr clearing). Re-checked live before writing:
  * the JE: refused if its DocNumber exists or the (date, bank debit) is already booked
  * each Payment: skipped if a payment for this payout id already sits on that customer;
    refused if the invoice's open balance is now below the amount
Re-running after a partial failure is safe: written Payments are found and skipped.
Every write is appended to review/CHANGE_LOG.csv.
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path

import yaml

from . import bridge
from .paths import CONFIG_DIR, REVIEW_DIR
from .post_invoices import Poster, Refused
from .resolver_util import esc

CFG = yaml.safe_load((CONFIG_DIR / "vrp_format.yml").read_text())


class PayoutPoster(Poster):
    def payout(self, p) -> list[str]:
        out = []
        doc = p["DocNumber"]
        if self.one("JournalEntry", f"DocNumber = '{esc(doc)}'"):
            raise Refused(f"JE {doc} already exists")
        je_post = bridge._sub("qbo_ops", bridge.QBO_OPS_ROOT, "je.post")
        hit = je_post.existing_payouts(self.qbo, start=p["date"], end=p["date"]).get((p["date"], p["amount"]))
        if hit:
            raise Refused(f"payout {p['date']} {p['amount']:.2f} already booked as {hit[0]} (JE {hit[1]})")
        for x in p["payments"]:
            out.append(self._payment(p, x))
        out.append(self._je(p))
        return out

    def _payment(self, p, x) -> str:
        cust = x["customer_id"]
        existing = self.qbo.query_all(
            f"SELECT * FROM Payment WHERE CustomerRef = '{cust}' AND TxnDate >= '{p['date']}'", "Payment")
        if any(p["id"] in (e.get("PrivateNote") or "") and abs(e["TotalAmt"] - x["amount"]) < 0.005
               for e in existing):
            return f"Payment {x['code']} {x['amount']:.2f} already there -- skipped"
        inv = self.read("Invoice", x["invoice_id"])
        if inv["Balance"] + 0.005 < x["amount"]:
            raise Refused(f"invoice {x['invoice_doc']} open {inv['Balance']} < payment {x['amount']}")
        if not self.confirm:
            return f"would PAY {x['invoice_doc']} {x['amount']:.2f}"
        body = {"CustomerRef": {"value": cust}, "TotalAmt": x["amount"], "TxnDate": x["TxnDate"],
                "PaymentRefNum": x["PaymentRefNum"], "PrivateNote": x["PrivateNote"],
                "DepositToAccountRef": {"value": self.account(x["deposit_to"])},
                "Line": [{"Amount": x["amount"],
                          "LinkedTxn": [{"TxnId": x["invoice_id"], "TxnType": "Invoice"}]}]}
        r = self.qbo.post("payment", body)
        self.log({"code": x["code"]}, "Payment", "CREATE", r["Id"], x["PaymentRefNum"], x["amount"], p["id"])
        return f"PAY {x['invoice_doc']} {x['amount']:.2f} Id {r['Id']}"

    def _je(self, p) -> str:
        lines = []
        for l in p["lines"]:
            d = {"PostingType": l["posting"], "AccountRef": {"value": self.account(l["account"])},
                 "DepartmentRef": {"value": self.trust}}
            if l.get("class"):
                d["ClassRef"] = {"value": self.klass(l["class"])}
            if l.get("customer_id"):
                d["Entity"] = {"Type": "Customer", "EntityRef": {"value": l["customer_id"]}}
            lines.append({"DetailType": "JournalEntryLineDetail", "Amount": l["amount"],
                          "Description": l["description"], "JournalEntryLineDetail": d})
        if not self.confirm:
            return f"would JE {p['DocNumber']} {p['amount']:.2f} ({len(lines)} lines)"
        r = self.qbo.post("journalentry", {"DocNumber": p["DocNumber"], "TxnDate": p["date"],
                                           "PrivateNote": p["memo"], "Line": lines})
        self.log({"code": ""}, "JournalEntry", "CREATE", r["Id"], p["DocNumber"], p["amount"], p["id"])
        return f"JE {p['DocNumber']} Id {r['Id']} {p['amount']:.2f}"


def main():
    ap = argparse.ArgumentParser(description="Post a reviewed payout plan (dry run unless --confirm).")
    ap.add_argument("--plan", help="review/payouts_plan_<stamp>.json (default: latest)")
    ap.add_argument("--id", action="append", help="only these payout ids (repeatable)")
    ap.add_argument("--confirm", action="store_true")
    a = ap.parse_args()
    path = Path(a.plan) if a.plan else max(REVIEW_DIR.glob("payouts_plan_*.json"))
    plans = [p for p in json.loads(path.read_text()) if p["action"] == "CREATE"]
    if a.id:
        plans = [p for p in plans if p["id"] in set(a.id)]
    print(f"{path.name}: {len(plans)} payout(s){'' if a.confirm else '  [DRY RUN]'}")
    poster = PayoutPoster(a.confirm)
    bad = []
    for p in plans:
        try:
            for s in poster.payout(p):
                print(f"  {p['id'][:22]:22} {s}")
        except Exception as e:                    # noqa: BLE001
            bad.append((p["id"], str(e)))
            print(f"  {p['id'][:22]:22} REFUSED/ERROR: {e}")
    print(f"\n{len(plans) - len(bad)} ok, {len(bad)} refused/failed")
    for i, e in bad:
        print(f"  {i}: {e}")


if __name__ == "__main__":
    main()
