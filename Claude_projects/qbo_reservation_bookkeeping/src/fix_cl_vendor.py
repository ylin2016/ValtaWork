"""Put the Elektra `_CL` owner cleaning rebills under vendor Valta Realty. DRY RUN unless --confirm.

    python -m src.fix_cl_vendor              # show what would change
    python -m src.fix_cl_vendor --confirm    # write; appends review/CHANGE_LOG.csv

Owner rule 2026-10-14: a cleaner (e.g. Maria Rangel) is the vendor only on a bill that actually
pays them. A `_CL` bill only passes the cleaning on to the owner, so its vendor is Valta Realty.
The five open `_CL` bills were closed by $0 bill payments against the A/P lines of the
`<date>_<unit>MCRADJ` journals (fix_owner_paid_airbnb); a bill's vendor cannot change while a
payment is linked, so: void those bill payments, change the vendor on every Elektra `_CL` bill
and on the journals' A/P lines, then close the open bills again under Valta Realty.
Only the vendor changes -- no amount, account, class or date -- so owner statements don't move.
"""
from __future__ import annotations

import argparse

from .fix_owner_paid_airbnb import BANK, UNITS
from .post_invoices import Poster, Refused

VALTA = "495"                                       # vendor Valta Realty
ADJ_DOCS = ["20261014_1004MCRADJ", "20261014_1108MCRADJ", "20261014_1115MCRADJ"]
UNIT_TAGS = tuple(UNITS.values())


class VendorFix(Poster):
    def run(self) -> list[str]:
        out = []
        bills = [b for b in self.qbo.query_all("SELECT * FROM Bill WHERE TxnDate >= '2026-01-01'", "Bill")
                 if (b.get("DocNumber") or "").endswith("_CL") and any(t in b["DocNumber"] for t in UNIT_TAGS)]
        jes = [self.one("JournalEntry", f"DocNumber = '{d}'") for d in ADJ_DOCS]
        # 1. void the $0 bill payments that link these bills
        bp_ids = {x["TxnId"] for b in bills for x in b.get("LinkedTxn", []) if x["TxnType"] == "BillPayment"}
        for bid in sorted(bp_ids):
            bp = self.read("BillPayment", bid)
            if bp["TotalAmt"] != 0:
                raise Refused(f"bill payment {bid} is {bp['TotalAmt']}, not a $0 application -- look first")
            if not self.confirm:
                out.append(f"would VOID $0 bill payment {bid} ({bp['VendorRef']['name']}, {bp['TxnDate']})")
                continue
            r = self.qbo.request("POST", f"/v3/company/{self.qbo.realm_id}/billpayment",
                                 params={"operation": "update", "include": "void"},
                                 json_body={"Id": bid, "SyncToken": bp["SyncToken"], "sparse": True})["BillPayment"]
            self.log({"code": "_CL vendor"}, "BillPayment", "VOID", r["Id"], "", 0, "re-done under Valta Realty")
            out.append(f"VOIDED bill payment {bid}")
        # 2. bills -> Valta Realty
        for b in bills:
            if b["VendorRef"]["value"] == VALTA:
                continue
            if not self.confirm:
                out.append(f"would set vendor of bill {b['DocNumber']} ({b['TxnDate']}, {b['TotalAmt']:.2f}) "
                           f"{b['VendorRef']['name']} -> Valta Realty")
                continue
            live = self.read("Bill", b["Id"])
            was = live["VendorRef"]["name"]
            live["VendorRef"] = {"value": VALTA}
            r = self.qbo.post("bill", live)
            self.log({"code": "_CL vendor"}, "Bill", "UPDATE", r["Id"], b["DocNumber"], r["TotalAmt"], f"vendor {was} -> Valta Realty")
            out.append(f"bill {b['DocNumber']} vendor -> Valta Realty")
        # 3. journals' A/P lines -> Valta Realty (re-read: the voids above change their SyncToken)
        jes = [self.read("JournalEntry", j["Id"]) for j in jes]
        for j in jes:
            n = 0
            for l in j["Line"]:
                e = l["JournalEntryLineDetail"].get("Entity") or {}
                if e.get("Type") == "Vendor" and e["EntityRef"]["value"] != VALTA:
                    old = e["EntityRef"].get("name", "")
                    l["JournalEntryLineDetail"]["Entity"] = {"Type": "Vendor", "EntityRef": {"value": VALTA}}
                    l["Description"] = l["Description"].replace(f"(A/P {old})", "(A/P Valta Realty)")
                    n += 1
            if not n:
                continue
            if not self.confirm:
                out.append(f"would set vendor on {n} A/P lines of {j['DocNumber']} -> Valta Realty")
                continue
            r = self.qbo.post("journalentry", j)
            self.log({"code": "_CL vendor"}, "JournalEntry", "UPDATE", r["Id"], j["DocNumber"], r["TotalAmt"], f"{n} A/P lines -> Valta Realty")
            out.append(f"JE {j['DocNumber']}: {n} A/P lines -> Valta Realty")
        # 4. close the open bills again: each bill's A/P debits in the journals
        for b in bills:
            if not b["TotalAmt"]:
                continue
            credits = [(j["Id"], l["Amount"]) for j in jes for l in j["Line"]
                       if (l.get("Description") or "").split(" | ")[-1].startswith(f"reverses {b['DocNumber']} ")
                       and (l["JournalEntryLineDetail"].get("Entity") or {}).get("Type") == "Vendor"]
            per_je = {}
            for jid, a in credits:
                per_je[jid] = round(per_je.get(jid, 0) + a, 2)
            amt = round(sum(per_je.values()), 2)
            if abs(amt - b["TotalAmt"]) > 0.005:
                raise Refused(f"bill {b['DocNumber']} {b['TotalAmt']} vs journal A/P lines {amt}")
            if not self.confirm:
                out.append(f"would close bill {b['DocNumber']} {amt:.2f} with a $0 bill payment (Valta Realty)")
                continue
            r = self.qbo.post("billpayment", {
                "VendorRef": {"value": VALTA}, "PayType": "Check", "TotalAmt": 0, "TxnDate": "2026-10-14",
                "CheckPayment": {"BankAccountRef": {"value": BANK}},
                "PrivateNote": "Applies the owner-paid Airbnb adjustment: owner cleaning rebill, not a payable",
                "Line": [{"Amount": amt, "LinkedTxn": [{"TxnId": b["Id"], "TxnType": "Bill"}]}] +
                        [{"Amount": a, "LinkedTxn": [{"TxnId": jid, "TxnType": "JournalEntry"}]} for jid, a in per_je.items()]})
            self.log({"code": b["DocNumber"]}, "BillPayment", "CREATE", r["Id"], b["DocNumber"], 0, "applies A/P adjustment (Valta Realty)")
            out.append(f"closed bill {b['DocNumber']} {amt:.2f}, bill payment {r['Id']}")
        return out


def main():
    ap = argparse.ArgumentParser(description="Elektra _CL bills -> vendor Valta Realty (dry run unless --confirm).")
    ap.add_argument("--confirm", action="store_true")
    a = ap.parse_args()
    for s in VendorFix(a.confirm).run():
        print(" ", s)


if __name__ == "__main__":
    main()
