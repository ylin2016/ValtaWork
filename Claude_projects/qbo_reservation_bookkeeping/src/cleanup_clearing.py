"""Clear legacy hand-booking errors out of a channel clearing account. DRY RUN unless --confirm.

    python -m src.cleanup_clearing                 # build: review/cleanup_plan_<stamp>.json + .csv
    python -m src.cleanup_clearing --confirm       # post the LATEST plan the owner reviewed

Owner decisions 2026-10-14: corrections are dated TODAY (closed months and issued owner
statements stay as they are); old guest refunds are Valta's, not the owners' (items 7-9, not
built yet: no Valta guest-refund account chosen). Items 10-14 are being researched.

Each item names the original clearing transaction it offsets, so the plan re-checks live that
the clearing line is still there and unchanged before writing:
  APPLY    a Payment out of clearing onto the invoice the channel money was meant for
  RECLASS  one JE line pair: Dr clearing (the reservation) / Cr the income account it belonged in
All RECLASS lines go in ONE journal entry (`CLR-<channel>-<date>`). Logged to review/CHANGE_LOG.csv.
"""
from __future__ import annotations

import argparse
import csv
import json
from datetime import date, datetime, timezone
from pathlib import Path

from . import bridge
from .paths import REVIEW_DIR
from .post_invoices import Poster, Refused
from .resolver_util import esc

CLEARING = {"marriott": "1688"}
# Account Ids = the income account of the Guesty item each fee belongs to (Item.IncomeAccountRef).
MISC_GUEST_FEE_OWNER, PET_FEE_OWNER, PARKING_FEE_OWNER, CANCELLATION_FEE = "1640", "1675", "1150040017", "1635"
ROUNDING = "1514"                  # Reconciliation Discrepancies (company)

ITEMS = [  # (no, kind, customer, original txn(s), amount, target, note)
    (1, "APPLY", "homesvillasbymarriott - MARSHALL GUILLORY - HVBM-M1707855601194-AV1KU",
     ["Deposit:22673"], 468.04, "Invoice:21335",
     "Marriott ACH 2/22/2024 deposited to clearing instead of applied; $0.02 left is rounding"),
    (2, "RECLASS", "homesvillasbymarriott - Anthony Esparza - HVBM-M1704598691824-I827W",
     ["Deposit:23311"], 40.00, MISC_GUEST_FEE_OWNER, "Guest Zelle payment 2/3/2024"),
    (3, "RECLASS", "homesvillasbymarriott - THOMAS POSAUNER - HVBM-M1724515010303-ZEWMB",
     ["Deposit:57856"], 460.00, MISC_GUEST_FEE_OWNER, "Extra guest fee 11/7/2024"),
    (4, "RECLASS", "homesvillasbymarriott - POPPY MILLINGTON - HVBM-M1764725345577-Q4J6T",
     ["Deposit:92885"], 300.00, PET_FEE_OWNER, "Pet fee 1/1/2026"),
    (5, "RECLASS", "homesvillasbymarriott - BENJAMIN NICHTER - HVBM-M1781476191074-HRDZY",
     ["Deposit:111213"], 105.00, PARKING_FEE_OWNER, "Parking fee 8/1/2026"),
    (6, "RECLASS", "homesvillasbymarriott - CHAD NITTA - HVBM-M1704162320138-L7REC",
     ["Deposit:26480", "Purchase:29376"], 663.68, CANCELLATION_FEE,
     "Kept from cancellation: $1,386.59 in 3/12/2024 less $722.91 refunded 4/1/2024"),
]


def clearing_lines(qbo, ref: str, clearing: str) -> list[dict]:
    ent, oid = ref.split(":")
    r = qbo.request("GET", f"/v3/company/{qbo.realm_id}/{ent.lower()}/{oid}")[ent]
    out = []
    for l in r["Line"]:
        d = l.get("DepositLineDetail") or l.get("AccountBasedExpenseLineDetail") or {}
        if (d.get("AccountRef") or {}).get("value") == clearing:
            out.append({"txn": ref, "amount": l["Amount"], "class": d.get("ClassRef"),
                        "sign": -1 if ent == "Deposit" else 1})
    return out


def build(channel: str) -> list[dict]:
    qbo, today = bridge.qbo_client(), date.today().isoformat()
    clearing = CLEARING[channel]
    plan = []
    for no, kind, cust, origs, amt, target, note in ITEMS:
        lines = [x for o in origs for x in clearing_lines(qbo, o, clearing)]
        net = round(sum(x["sign"] * x["amount"] for x in lines), 2)
        c = qbo.query_all(f"SELECT * FROM Customer WHERE DisplayName = '{esc(cust)}'", "Customer")
        item = {"no": no, "kind": kind, "customer": cust, "customer_id": c[0]["Id"] if c else None,
                "originals": origs, "clearing_net": net, "amount": amt, "target": target, "note": note,
                "class": next((x["class"] for x in lines if x["class"]), None), "date": today, "block": []}
        if not c:
            item["block"].append("customer not found")
        if abs(abs(net) - amt) > 0.03:
            item["block"].append(f"clearing lines now net {net:.2f}, not {amt:.2f} -- re-check")
        if kind == "APPLY":
            inv = qbo.request("GET", f"/v3/company/{qbo.realm_id}/invoice/{target.split(':')[1]}")["Invoice"]
            item["invoice_doc"], item["invoice_balance"] = inv["DocNumber"], inv["Balance"]
            if inv["Balance"] + 0.005 < amt:
                item["block"].append(f"invoice open {inv['Balance']} < {amt}")
            item["rounding"] = round(abs(net) - amt, 2)
        plan.append(item)
    return plan


def write(plan, stamp):
    (REVIEW_DIR / f"cleanup_plan_{stamp}.json").write_text(json.dumps(plan, indent=1))
    with (REVIEW_DIR / f"cleanup_{stamp}.csv").open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["no", "kind", "date", "customer", "class", "debit", "credit", "amount", "originals", "note", "block"])
        for p in plan:
            cls = (p["class"] or {}).get("name", "")
            if p["kind"] == "APPLY":
                w.writerow([p["no"], "Payment", p["date"], p["customer"], cls, "Payments Clearing (deposit-to)",
                            f"Invoice {p['invoice_doc']}", p["amount"], " ".join(p["originals"]), p["note"], " | ".join(p["block"])])
                if p["rounding"]:
                    w.writerow([p["no"], "JE line", p["date"], p["customer"], cls, "Payments Clearing",
                                "Reconciliation Discrepancies", p["rounding"], "", "rounding", ""])
            else:
                w.writerow([p["no"], "JE line", p["date"], p["customer"], cls, "Payments Clearing",
                            f"account {p['target']}", p["amount"], " ".join(p["originals"]), p["note"], " | ".join(p["block"])])


class CleanupPoster(Poster):
    def post(self, plan, channel):
        clearing, out = CLEARING[channel], []
        if any(p["block"] for p in plan):
            raise Refused("plan has BLOCKED items -- rebuild")
        today = plan[0]["date"]
        doc = f"CLR-{channel[:3].upper()}-{today.replace('-', '')}"
        if self.one("JournalEntry", f"DocNumber = '{doc}'"):
            raise Refused(f"JE {doc} already exists")
        lines = []

        def pair(p, amt, credit_acct, desc):
            base = {"DepartmentRef": {"value": self.trust},
                    "Entity": {"Type": "Customer", "EntityRef": {"value": p["customer_id"]}}}
            if p["class"]:
                base["ClassRef"] = {"value": p["class"]["value"]}
            for acct, side in ((clearing, "Debit"), (credit_acct, "Credit")):
                lines.append({"DetailType": "JournalEntryLineDetail", "Amount": amt, "Description": desc,
                              "JournalEntryLineDetail": {**base, "PostingType": side, "AccountRef": {"value": acct}}})

        for p in plan:
            if p["kind"] == "APPLY":
                out.append(self._apply(p, clearing))
                if p["rounding"]:
                    pair(p, p["rounding"], ROUNDING, f"Rounding left by {' '.join(p['originals'])} | {p['customer']}")
            else:
                pair(p, p["amount"], p["target"], f"Reclass from clearing: {p['note']} ({' '.join(p['originals'])})")
        total = round(sum(l["Amount"] for l in lines if l["JournalEntryLineDetail"]["PostingType"] == "Debit"), 2)
        if not self.confirm:
            return out + [f"would JE {doc} {total:.2f} ({len(lines)} lines)"]
        r = self.qbo.post("journalentry", {"DocNumber": doc, "TxnDate": today, "Line": lines,
                                           "PrivateNote": f"Legacy {channel} clearing cleanup (owner, 2026-10-14)"})
        self.log({"code": ""}, "JournalEntry", "CREATE", r["Id"], doc, total, "clearing cleanup")
        return out + [f"JE {doc} Id {r['Id']} {total:.2f}"]

    def _apply(self, p, clearing):
        inv = self.read("Invoice", p["target"].split(":")[1])
        if inv["Balance"] + 0.005 < p["amount"]:
            raise Refused(f"invoice {inv['DocNumber']} open {inv['Balance']} < {p['amount']}")
        if not self.confirm:
            return f"would PAY {inv['DocNumber']} {p['amount']:.2f} from clearing"
        r = self.qbo.post("payment", {"CustomerRef": {"value": p["customer_id"]}, "TotalAmt": p["amount"],
                                      "TxnDate": p["date"], "DepositToAccountRef": {"value": clearing},
                                      "PrivateNote": f"Clearing cleanup: applies {' '.join(p['originals'])}",
                                      "Line": [{"Amount": p["amount"],
                                                "LinkedTxn": [{"TxnId": inv["Id"], "TxnType": "Invoice"}]}]})
        self.log({"code": inv["DocNumber"]}, "Payment", "CREATE", r["Id"], inv["DocNumber"], p["amount"], "clearing cleanup")
        return f"PAY {inv['DocNumber']} {p['amount']:.2f} Id {r['Id']}"


def main():
    ap = argparse.ArgumentParser(description="Legacy clearing cleanup (dry run unless --confirm).")
    ap.add_argument("--channel", default="marriott", choices=sorted(CLEARING))
    ap.add_argument("--confirm", action="store_true")
    a = ap.parse_args()
    if a.confirm:
        path = max(REVIEW_DIR.glob("cleanup_plan_*.json"))
        plan = json.loads(path.read_text())
        print(f"{path.name}: posting")
    else:
        plan = build(a.channel)
        stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        write(plan, stamp)
        print(f"-> review/cleanup_{stamp}.csv, cleanup_plan_{stamp}.json  [DRY RUN]")
        for p in plan:
            print(f"  {p['no']:>2} {p['kind']:7} {p['amount']:>8.2f}  net-in-clearing {p['clearing_net']:>9.2f}  "
                  f"{(p['class'] or {}).get('name', ''):28} {p['customer'][22:60]}" + (f"  BLOCK {p['block']}" if p["block"] else ""))
    for s in CleanupPoster(a.confirm).post(plan, a.channel):
        print(" ", s)


if __name__ == "__main__":
    main()
