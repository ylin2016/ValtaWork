"""Post a reviewed plan (review/plan_<stamp>.json) to QuickBooks. DRY RUN unless --confirm.

    python -m src.post_invoices --code HMXXXX                 # dry run, one reservation
    python -m src.post_invoices --code HMXXXX --confirm       # write it
    python -m src.post_invoices --confirm                     # write the whole plan

Posts exactly what the review CSV showed -- it reads the plan file, never rebuilds.
Every object is re-checked live immediately before it is written:

  CREATE  refused if the DocNumber now exists (someone posted it since the snapshot)
  UPDATE  refused if the object's SyncToken moved since the snapshot (changed since);
          otherwise the object is READ BACK and echoed whole with its lines replaced --
          QBO full-updates blank any field a payload omits
  VOID    refused if a payment is linked to the invoice (voiding would strand it)
  ZERO    fee Bill lines set to $0.00 with the cancelled memo, as VRP did

Every write is appended to review/CHANGE_LOG.csv. A refused or failed reservation stops
only that reservation; the run carries on and the summary lists it.
"""
from __future__ import annotations

import argparse
import csv
import json
from datetime import datetime, timezone
from pathlib import Path

import yaml

from . import bridge
from .paths import CONFIG_DIR, REVIEW_DIR
from .resolver_util import esc

CFG = yaml.safe_load((CONFIG_DIR / "vrp_format.yml").read_text())
LOG = REVIEW_DIR / "CHANGE_LOG.csv"
LOG_COLS = ["at", "code", "entity", "action", "Id", "DocNumber", "amount", "note"]


class Refused(Exception):
    pass


class Poster:
    def __init__(self, confirm: bool):
        self.confirm = confirm
        self.qbo = bridge.qbo_client()
        self.res = bridge.resolver(self.qbo)
        self.trust = self._need(self.res.department(CFG["location"]), "Location", CFG["location"])
        self.ap = self._need(self.res.account(CFG["ap_account"]), "A/P account", CFG["ap_account"])

    # -- lookups ------------------------------------------------------------------
    @staticmethod
    def _need(v, what, name):
        if not v:
            raise Refused(f"{what} {name!r} not found in QBO")
        return v

    def item(self, n):    return self._need(self.res.item(n), "item", n)
    def account(self, n): return self._need(self.res.account(n), "account", n)
    def vendor(self, n):  return self._need(self.res.vendor(n), "vendor", n)
    def klass(self, n):   return self._need(self.res.klass(n), "class", n)

    def one(self, entity: str, where: str) -> dict | None:
        got = self.qbo.query(f"SELECT * FROM {entity} WHERE {where}").get("QueryResponse", {}).get(entity, [])
        return got[0] if got else None

    def read(self, entity: str, oid: str) -> dict:
        return self.qbo.request("GET", f"/v3/company/{self.qbo.realm_id}/{entity.lower()}/{oid}")[entity]

    def log(self, p, entity, action, obj_id, doc, amount, note=""):
        new = not LOG.exists()
        with LOG.open("a", newline="") as fh:
            w = csv.DictWriter(fh, fieldnames=LOG_COLS)
            if new:
                w.writeheader()
            w.writerow({"at": datetime.now(timezone.utc).isoformat(timespec="seconds"), "code": p["code"],
                        "entity": entity, "action": action, "Id": obj_id, "DocNumber": doc,
                        "amount": amount, "note": note})

    # -- customer -------------------------------------------------------------------
    def customer_id(self, p) -> str:
        if p.get("customer_id"):
            return p["customer_id"]
        cid = self.res.customer(p["customer"])        # created since the build?
        if cid:
            return cid
        if not self.confirm:
            return "(new)"
        c = self.qbo.post("customer", {"DisplayName": p["customer"]})
        self.log(p, "Customer", "CREATE", c["Id"], "", "", p["customer"])
        return c["Id"]

    # -- payloads -------------------------------------------------------------------
    def invoice_lines(self, p) -> list[dict]:
        k = self.klass(p["class"])
        return [{"DetailType": "SalesItemLineDetail", "Amount": l["amount"], "Description": l["description"],
                 "SalesItemLineDetail": {"ItemRef": {"value": self.item(l["item"])}, "ClassRef": {"value": k},
                                         "Qty": 1, "UnitPrice": l["amount"], "TaxCodeRef": {"value": "NON"}}}
                for l in p["invoice"]["lines"]]

    def bill_lines(self, p, b, cust) -> list[dict]:
        k = self.klass(p["class"])
        return [{"DetailType": "AccountBasedExpenseLineDetail", "Amount": l["amount"], "Description": l["description"],
                 "AccountBasedExpenseLineDetail": {
                     "AccountRef": {"value": self.account(l["account"])}, "ClassRef": {"value": k},
                     "CustomerRef": {"value": cust}, "BillableStatus": "NotBillable",
                     "TaxCodeRef": {"value": "NON"}}}
                for l in b["lines"]]

    # -- one reservation --------------------------------------------------------------
    def post(self, p) -> list[str]:
        done = []
        if p["action"] in ("SKIP", "SAME", "BLOCKED"):
            return done
        if p["action"] == "VOID":
            done.append(self._void(p))
            for b in p["bills"]:
                done.append(self._zero(p, b))
            return done
        cust = self.customer_id(p)
        inv = p["invoice"]
        if inv["action"] in ("CREATE", "UPDATE"):
            done.append(self._invoice(p, inv, cust))
        for b in p["bills"]:
            if b["action"] in ("CREATE", "UPDATE"):
                done.append(self._bill(p, b, cust))
            elif b["action"] == "ZERO":
                done.append(self._zero(p, b))
        return done

    def _invoice(self, p, inv, cust) -> str:
        doc = inv["DocNumber"]
        live = self.one("Invoice", f"DocNumber = '{esc(doc)}'")
        if inv["action"] == "CREATE":
            if live:
                raise Refused(f"Invoice {doc} now exists (Id {live['Id']}) -- rebuild the plan")
            body = {"CustomerRef": {"value": cust}, "DocNumber": doc, "TxnDate": inv["TxnDate"],
                    "DueDate": inv["TxnDate"], "DepartmentRef": {"value": self.trust},
                    "Line": self.invoice_lines(p)}
        else:
            ex = p["existing_invoice"]
            if not live or live["Id"] != ex["Id"] or live["SyncToken"] != ex["SyncToken"]:
                raise Refused(f"Invoice {doc} changed in QBO since the snapshot -- rebuild the plan")
            body = self.read("Invoice", ex["Id"])
            body.update(TxnDate=inv["TxnDate"], DueDate=inv["TxnDate"], Line=self.invoice_lines(p))
            body.pop("TotalAmt", None); body.pop("Balance", None)
        if not self.confirm:
            return f"would {inv['action']} Invoice {doc} {inv['total']:.2f}"
        r = self.qbo.post("invoice", body)
        if abs(float(r["TotalAmt"]) - inv["total"]) > 0.005:
            self.log(p, "Invoice", inv["action"], r["Id"], doc, r["TotalAmt"], f"TOTAL MISMATCH plan {inv['total']}")
            raise Refused(f"Invoice {doc} posted as {r['TotalAmt']} but plan says {inv['total']} -- CHECK IT")
        self.log(p, "Invoice", inv["action"], r["Id"], doc, r["TotalAmt"])
        return f"{inv['action']} Invoice {doc} Id {r['Id']} {r['TotalAmt']}"

    def _bill(self, p, b, cust) -> str:
        doc = b["DocNumber"]
        live = self.one("Bill", f"DocNumber = '{esc(doc)}'")
        lines = self.bill_lines(p, b, cust)
        if b["action"] == "CREATE":
            if live:
                raise Refused(f"Bill {doc} now exists (Id {live['Id']}) -- rebuild the plan")
            body = {"VendorRef": {"value": self.vendor(b["vendor"])}, "APAccountRef": {"value": self.ap},
                    "DocNumber": doc, "TxnDate": b["TxnDate"], "DueDate": b["TxnDate"],
                    "DepartmentRef": {"value": self.trust}, "PrivateNote": b["memo"], "Line": lines}
        else:
            ex = b["existing"]
            if not live or live["Id"] != ex["Id"] or live["SyncToken"] != ex["SyncToken"]:
                raise Refused(f"Bill {doc} changed in QBO since the snapshot -- rebuild the plan")
            body = self.read("Bill", ex["Id"])
            body.update(TxnDate=b["TxnDate"], DueDate=b["TxnDate"], PrivateNote=b["memo"], Line=lines)
            body.pop("TotalAmt", None); body.pop("Balance", None)
        if not self.confirm:
            return f"would {b['action']} Bill {doc} {b['amount']:.2f}"
        r = self.qbo.post("bill", body)
        if abs(float(r["TotalAmt"])) > 0.005:
            self.log(p, "Bill", b["action"], r["Id"], doc, r["TotalAmt"], "NONZERO TOTAL")
            raise Refused(f"Bill {doc} total {r['TotalAmt']} -- fee Bills must net to $0.00 -- CHECK IT")
        self.log(p, "Bill", b["action"], r["Id"], doc, b["amount"])
        return f"{b['action']} Bill {doc} Id {r['Id']} ({b['amount']:.2f})"

    def _zero(self, p, b) -> str:
        doc, ex = b["DocNumber"], b["existing"]
        live = self.one("Bill", f"DocNumber = '{esc(doc)}'")
        if not live or live["SyncToken"] != ex["SyncToken"]:
            raise Refused(f"Bill {doc} changed in QBO since the snapshot -- rebuild the plan")
        if not self.confirm:
            return f"would ZERO Bill {doc} (was {ex['gross']:.2f})"
        body = self.read("Bill", ex["Id"])
        for ln in body["Line"]:
            ln["Amount"] = 0
        body["PrivateNote"] = b["memo"]
        body.pop("TotalAmt", None); body.pop("Balance", None)
        r = self.qbo.post("bill", body)
        self.log(p, "Bill", "ZERO", r["Id"], doc, 0, f"was {ex['gross']}")
        return f"ZERO Bill {doc} Id {r['Id']}"

    def _void(self, p) -> str:
        ex, doc = p["existing_invoice"], p["invoice_doc"]
        live = self.read("Invoice", ex["Id"])
        if live.get("LinkedTxn"):
            raise Refused(f"Invoice {doc} has a payment linked {live['LinkedTxn']} -- void would strand it; "
                          f"handle at month end (cancellation fee / refund)")
        if live["SyncToken"] != ex["SyncToken"]:
            raise Refused(f"Invoice {doc} changed in QBO since the snapshot -- rebuild the plan")
        if not self.confirm:
            return f"would VOID Invoice {doc} ({ex['TotalAmt']:.2f})"
        r = self.qbo.request("POST", f"/v3/company/{self.qbo.realm_id}/invoice",
                             params={"operation": "void"},
                             json_body={"Id": ex["Id"], "SyncToken": live["SyncToken"]})["Invoice"]
        self.log(p, "Invoice", "VOID", r["Id"], doc, ex["TotalAmt"])
        return f"VOID Invoice {doc} Id {r['Id']}"


def main():
    ap = argparse.ArgumentParser(description="Post a reviewed plan to QBO (dry run unless --confirm).")
    ap.add_argument("--plan", help="review/plan_<stamp>.json (default: latest non-parity plan)")
    ap.add_argument("--code", action="append", help="only these confirmation codes (repeatable)")
    ap.add_argument("--confirm", action="store_true", help="actually write to QuickBooks")
    a = ap.parse_args()

    path = Path(a.plan) if a.plan else max(REVIEW_DIR.glob("plan_2*.json"))
    plans = json.loads(path.read_text())
    if a.code:
        plans = [p for p in plans if p["code"] in set(a.code)]
        missing = set(a.code) - {p["code"] for p in plans}
        if missing:
            raise SystemExit(f"not in {path.name}: {sorted(missing)}")
    todo = [p for p in plans if p["action"] not in ("SKIP", "SAME", "BLOCKED")]
    print(f"{path.name}: {len(todo)} reservation(s) with changes"
          f"{'' if a.confirm else '  [DRY RUN -- nothing is written; add --confirm]'}")

    poster = Poster(a.confirm)
    refused = []
    for p in todo:
        try:
            for line in poster.post(p):
                print(f"  {p['code']:24} {line}")
        except Refused as e:
            refused.append((p["code"], str(e)))
            print(f"  {p['code']:24} REFUSED: {e}")
        except Exception as e:                       # noqa: BLE001 -- keep going, report it
            refused.append((p["code"], f"ERROR {e}"))
            print(f"  {p['code']:24} ERROR: {e}")
            if a.confirm:
                print("    !! check QBO for anything this reservation wrote before re-running")
    print(f"\n{len(todo) - len(refused)} ok, {len(refused)} refused/failed")
    for c, why in refused:
        print(f"  {c}: {why}")


if __name__ == "__main__":
    main()
