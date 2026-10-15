"""Owner-paid Airbnb bookings (Elektra 1004/1108/1115): month-end MCR journals + payments, and
corrections of earlier months. DRY RUN unless --confirm.

    python -m src.fix_owner_paid_airbnb                     # build: review/owner_paid_<stamp>.csv + plan json
    python -m src.fix_owner_paid_airbnb --date 2026-10-31   # month-end run once October has ended
    python -m src.fix_owner_paid_airbnb --backlog --date 2026-10-14   # 12/2024-12/2025: payments only
    python -m src.fix_owner_paid_airbnb --confirm           # post the LATEST plan the owner reviewed

These three listings take Airbnb bookings on the owner's own Airbnb account, so Airbnb pays the
owner, never Valta. The rule from October 2026 (owner, 2026-10-14; Accounting SOP Part E), per
reservation:

  Invoice      from Guesty as usual; the guest's $88 cleaning fee is a Cleaning Fee (PM) line --
               Valta's fee, which pays the cleaner, exactly as on any unit Valta's cleaner cleans
  MCR journal  <YYYYMMDD>_<unit>MCR at the end of the check-in month, ONE pair per reservation:
                 Dr 1C Other Owner Expenses       full payout   "Airbnb payout received directly"
                 Cr Payments Clearing - Airbnb    full payout   (customer, class, location Trust)
  Payment      invoice total into Payments Clearing - Airbnb, dated the MCR journal
  no cleaning  no Cleaning Expense - Owner line and no _CL bill: the $88 is Valta's fee the owner
               collected, not a cost charged to the owner
A stay whose check-in month has ended (month end <= --date) and has no MCR journal gets one.

Corrections: published months are NOT edited (owner, 2026-10-14) -- nothing dated in a published
month is changed or deleted. A stay that has an MCR journal but departs from the rule (Jan-Sep 2026:
cleaning charged on _CL bills, no payment) is corrected by ADJUSTMENTS dated --date, the open
statement month (posted 2026-10-14 for Jan-Sep; a stay on an existing MCRADJ journal is skipped):

  PAY    a Payment for each invoice's open balance into Airbnb clearing
  ADJ    one journal <YYYYMMDD>_<unit>MCRADJ, per reservation:
           Cr Payments Clearing - Airbnb      what is left in clearing after the payment (the
                                              cleaning the MCR journal did not charge)
           Dr <account its _CL lines credited> cancels the _CL offset: Cleaning Payout,
                                              Cleaning Fee Revenue, or A/P to the vendor
           Cr/Dr 1C Cleaning Expense - Owner  refunds (or charges) the difference, so the owner
                                              ends up charged the invoice total exactly once
  BILLPAY  a $0 bill payment applying each A/P line to its open _CL bill, closing it
           (Maria Rangel is paid for these cleans through her monthly cleaning journal; the
           _CL bills to her were never real payables)
Logged to review/CHANGE_LOG.csv.
"""
from __future__ import annotations

import argparse
import calendar
import csv
import json
import re
from collections import defaultdict
from datetime import date, datetime, timezone

from .paths import REVIEW_DIR
from .post_invoices import Poster, Refused

UNITS = {"Listings:Elektra 1004": "1004", "Listings:Elektra 1108": "1108", "Listings:Elektra 1115": "1115"}
YEAR = "2026"
CLEARING = "1626"                                   # Payments Clearing - Airbnb
BANK = "800"                                        # Chase Trust Checking 9967 (a $0 bill payment needs one)
CLEAN_ITEM = "Cleaning Fee (PM)"
CORRECTION_INVOICES = {"HM9XZ4SCFZ_C": "HM9XZ4SCFZ"}  # $0 invoice that moved fare -> cleaning
# backlog journal lines with no customer, by (DocNumber, amount) -> the stay they charge. The amount ties
# to that stay's total - cleaning; the description names the wrong guest (checked 2026-10-14).
UNTAGGED = {("20250331_1004MCR", 538.86): "HM3ZBWSDBY"}
MON = {m: i + 1 for i, m in enumerate("Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split())}


def month_end(d: str) -> str:
    y, m = int(d[:4]), int(d[5:7])
    return f"{y}-{m:02d}-{calendar.monthrange(y, m)[1]:02d}"


def code_of(name: str) -> str | None:
    return name.rsplit(" - ", 1)[-1].strip() if " - " in name else None


class Fixer(Poster):
    def acct(self, name):
        a = self.one("Account", f"Name = '{name}'")
        return self._need(a and a["Id"], "account", name)

    # -- build ----------------------------------------------------------------------
    def build(self, on: str) -> dict:
        q = self.qbo
        clean_acct = self.acct("Cleaning Expense - Owner")
        res, cinv = {}, {}
        for i in q.query_all(f"SELECT * FROM Invoice WHERE TxnDate >= '{YEAR}-01-01' AND TxnDate <= '{YEAR}-12-31'", "Invoice"):
            sl = [l for l in i["Line"] if l.get("DetailType") == "SalesItemLineDetail"]
            unit = next((UNITS[c] for c in {(l["SalesItemLineDetail"].get("ClassRef") or {}).get("name") for l in sl} if c in UNITS), None)
            if not unit or not i["DocNumber"].startswith("HM"):
                continue
            clean = round(sum(l["Amount"] for l in sl if l["SalesItemLineDetail"]["ItemRef"]["name"].endswith(CLEAN_ITEM)), 2)
            if i["DocNumber"] in CORRECTION_INVOICES:
                cinv[CORRECTION_INVOICES[i["DocNumber"]]] = clean
                continue
            res[i["DocNumber"]] = {
                "code": i["DocNumber"], "unit": unit, "checkin": i["TxnDate"], "total": i["TotalAmt"],
                "balance": i["Balance"], "clean": clean, "customer": i["CustomerRef"], "invoice_id": i["Id"],
                "class": next(l["SalesItemLineDetail"]["ClassRef"] for l in sl if l["SalesItemLineDetail"].get("ClassRef")),
                "paid_clearing": 0.0, "mcr": [], "mcr_charge": 0.0, "mcr_clearing": 0.0, "cl": [], "block": []}
        for code, moved in cinv.items():
            res[code]["clean"] = round(res[code]["clean"] + moved, 2)
        for r in res.values():
            for t in q.query_all(f"SELECT * FROM Payment WHERE CustomerRef = '{r['customer']['value']}'", "Payment"):
                for l in t["Line"]:
                    if any(x["TxnId"] == r["invoice_id"] for x in l["LinkedTxn"]) and t["TotalAmt"]:
                        if (t.get("DepositToAccountRef") or {}).get("value") != CLEARING:
                            r["block"].append(f"payment {t['Id']} not into Airbnb clearing")
                        r["paid_clearing"] = round(r["paid_clearing"] + l["Amount"], 2)

        jes = q.query_all(f"SELECT * FROM JournalEntry WHERE TxnDate >= '{YEAR}-01-01'", "JournalEntry")
        adjusted = {}                                   # stays already corrected on an MCRADJ journal
        for j in jes:
            if (j.get("DocNumber") or "").endswith("MCRADJ"):
                for l in j["Line"]:
                    code = code_of(((l["JournalEntryLineDetail"].get("Entity") or {}).get("EntityRef") or {}).get("name") or "")
                    code = code or code_of((l.get("Description") or "").split(" | ")[0])
                    if code in res:
                        adjusted[code] = j["DocNumber"]
        for j in jes:
            if not (j.get("DocNumber") or "").endswith("MCR"):
                continue
            loose, codes = 0.0, set()
            for l in j["Line"]:
                d = l["JournalEntryLineDetail"]
                if (d.get("ClassRef") or {}).get("name") not in UNITS:
                    continue
                code = code_of(((d.get("Entity") or {}).get("EntityRef") or {}).get("name") or "")
                if code in res:
                    r = res[code]
                    codes.add(code)
                    if j["DocNumber"] not in r["mcr"]:
                        r["mcr"].append(j["DocNumber"])
                    if d["PostingType"] == "Debit":
                        r["mcr_charge"] = round(r["mcr_charge"] + l["Amount"], 2)
                    elif d["AccountRef"]["value"] == CLEARING:
                        r["mcr_clearing"] = round(r["mcr_clearing"] + l["Amount"], 2)
                elif d["PostingType"] == "Debit" and d["AccountRef"]["value"] == clean_acct:
                    loose += l["Amount"]                  # Aug: cleaning line with no customer
                else:
                    raise Refused(f"{j['DocNumber']}: a line belongs to no {YEAR} reservation -- look first")
            if loose:                                   # one cleaning line per stay on that journal
                stays = [res[c] for c in codes]
                if abs(loose - sum(r["clean"] for r in stays)) > 0.005:
                    raise Refused(f"{j['DocNumber']}: customer-less cleaning lines {loose} don't match its stays")
                for r in stays:
                    r["mcr_charge"] = round(r["mcr_charge"] + r["clean"], 2)

        cl_docs = {}
        for ent in ("Bill", "JournalEntry"):
            for o in q.query_all(f"SELECT * FROM {ent} WHERE TxnDate >= '{YEAR}-01-01'", ent):
                if not (o.get("DocNumber") or "").endswith("_CL"):
                    continue
                offset = None
                if ent == "Bill" and o["TotalAmt"]:
                    offset = {"kind": "AP", "account": o["APAccountRef"]["value"], "vendor": o["VendorRef"],
                              "bill_id": o["Id"], "balance": o["Balance"]}
                for l in o["Line"]:
                    d = l.get("AccountBasedExpenseLineDetail") or l.get("JournalEntryLineDetail") or {}
                    if (d.get("ClassRef") or {}).get("name") in UNITS and d["AccountRef"]["value"] != clean_acct \
                            and (l["Amount"] < 0 or d.get("PostingType") == "Credit"):
                        offset = {"kind": "account", "account": d["AccountRef"]["value"], "name": d["AccountRef"]["name"].split(":")[-1]}
                for l in o["Line"]:
                    d = l.get("AccountBasedExpenseLineDetail") or l.get("JournalEntryLineDetail") or {}
                    unit = UNITS.get((d.get("ClassRef") or {}).get("name"))
                    if not unit or d["AccountRef"]["value"] != clean_acct or d.get("PostingType") == "Credit":
                        continue
                    m = re.search(r"(\d{1,2})\.?\s+([A-Z][a-z]{2})", l.get("Description") or "")
                    day = f"{YEAR}-{MON[m.group(2)]:02d}-{int(m.group(1)):02d}" if m else None
                    hit = [r for r in res.values() if r["unit"] == unit and r["checkin"] == day and r["total"]]
                    if not hit or not offset:
                        raise Refused(f"{ent} {o['DocNumber']}: line {l.get('Description')!r} -- no stay or no offset")
                    hit[0]["cl"].append({"entity": ent, "doc": o["DocNumber"], "date": o["TxnDate"],
                                         "amount": l["Amount"], "offset": offset})
                    cl_docs[(ent, o["DocNumber"])] = offset

        # what each reservation needs
        pays, adj, mcr = [], defaultdict(list), defaultdict(list)
        for r in sorted(res.values(), key=lambda r: (r["checkin"], r["unit"])):
            me = month_end(r["checkin"])
            if r["total"] == 0:
                r["note"] = "cancelled"
                continue
            if r["code"] in adjusted:
                r["note"] = f"corrected on {adjusted[r['code']]}"
                continue
            if r["block"]:
                continue
            if not r["mcr"]:
                if me > on:
                    r["note"] = f"month not ended: {me} run"
                    continue
                if r["paid_clearing"] or r["cl"]:
                    r["block"].append("no MCR journal but already paid or on a _CL bill -- look first")
                    continue
                mcr[f"{me}|{r['unit']}"].append({"code": r["code"], "customer": r["customer"], "class": r["class"],
                                                 "amount": r["total"]})
                if r["balance"]:
                    pays.append({"code": r["code"], "amount": round(r["balance"], 2), "date": me,
                                 "customer": r["customer"]["value"], "invoice_id": r["invoice_id"]})
                r.update(pay=round(r["balance"], 2), left=0.0, charged=r["total"], refund=0.0,
                         new_mcr=f"{me.replace('-', '')}_{r['unit']}MCR")
                continue
            pay = round(r["balance"], 2)
            if pay:
                pays.append({"code": r["code"], "amount": pay, "customer": r["customer"]["value"],
                             "invoice_id": r["invoice_id"]})
            left = round(r["paid_clearing"] + pay - r["mcr_clearing"], 2)        # in clearing after payment
            charged = round(r["mcr_charge"] + sum(c["amount"] for c in r["cl"]), 2)
            refund = round(charged - r["total"], 2)                              # >0: owner over-charged
            r.update(pay=pay, left=left, charged=charged, refund=refund)
            lines = []
            if left:
                lines.append(("Credit", CLEARING, None, left, "Airbnb payout paid to owner: cleaning not on the MCR journal"))
            for c in r["cl"]:
                o = c["offset"]
                lines.append(("Debit", o["account"], o.get("vendor"), c["amount"],
                              f"reverses {c['doc']} cleaning offset ({'A/P ' + o['vendor']['name'] if o['kind'] == 'AP' else o['name']})"))
            if refund:
                lines.append(("Credit" if refund > 0 else "Debit", clean_acct, None, abs(refund),
                              "cleaning charged twice: refund to owner" if refund > 0 else "cleaning not charged: charge owner"))
            dr = round(sum(x[3] for x in lines if x[0] == "Debit"), 2)
            cr = round(sum(x[3] for x in lines if x[0] == "Credit"), 2)
            if abs(dr - cr) > 0.005:
                r["block"].append(f"adjustment does not balance (Dr {dr} / Cr {cr})")
                continue
            if lines:
                adj[r["unit"]].append({"code": r["code"], "customer": r["customer"], "class": r["class"], "lines": lines})
        billpays = [{"bill_doc": d, **o} for (e, d), o in cl_docs.items()
                    if o["kind"] == "AP" and o["balance"] > 0.005 and adj]
        return {"date": on, "res": res, "pays": pays, "adj": adj, "mcr": mcr, "billpays": billpays,
                "cl_docs": [[e, d, o] for (e, d), o in cl_docs.items()]}

    # -- backlog: 12/2024-12/2025 ----------------------------------------------------
    def build_backlog(self, on: str, start="2024-11-01", end="2025-12-31") -> dict:
        """Close the pre-2026 invoices with a Payment into Airbnb clearing dated `on` -- nothing else.

        Those stays were charged to the owner (Dr Other Owner Expenses / Cr clearing, on MCR or other
        journals) for the invoice total LESS the cleaning fee; the cleaning was never charged anywhere.
        Owner, 2026-10-14: payments only -- the cleaning left in clearing is held for a later decision.
        A stay is paid only when its customer-tagged charge lines tie to total - cleaning exactly and
        no journal line mentions its code without being tagged to it (mislabelled lines -> block)."""
        q = self.qbo
        res = {}
        for i in q.query_all(f"SELECT * FROM Invoice WHERE TxnDate >= '{start}' AND TxnDate <= '{end}'", "Invoice"):
            sl = [l for l in i["Line"] if l.get("DetailType") == "SalesItemLineDetail"]
            unit = next((UNITS[c] for c in {(l["SalesItemLineDetail"].get("ClassRef") or {}).get("name") for l in sl} if c in UNITS), None)
            if not unit or not i["DocNumber"].startswith("HM") or not i["TotalAmt"]:
                continue
            clean = round(sum(l["Amount"] for l in sl if l["SalesItemLineDetail"]["ItemRef"]["name"].endswith(CLEAN_ITEM)), 2)
            res[i["DocNumber"]] = {
                "code": i["DocNumber"], "unit": unit, "checkin": i["TxnDate"], "total": i["TotalAmt"],
                "balance": i["Balance"], "clean": clean, "customer": i["CustomerRef"], "invoice_id": i["Id"],
                "mcr": [], "mcr_charge": 0.0, "mcr_clearing": 0.0, "cl": [], "block": []}
        for j in q.query_all(f"SELECT * FROM JournalEntry WHERE TxnDate >= '{start}'", "JournalEntry"):
            for l in j["Line"]:
                d = l.get("JournalEntryLineDetail") or {}
                if (d.get("ClassRef") or {}).get("name") not in UNITS:
                    continue
                tagged = code_of(((d.get("Entity") or {}).get("EntityRef") or {}).get("name") or "")
                tagged = tagged or UNTAGGED.get((j["DocNumber"], l["Amount"]))
                named = re.findall(r"\bHM[A-Z0-9]{8}\b", l.get("Description") or "")
                for c in set(named) - {tagged} if not tagged else ():   # a tagged line: the customer wins
                    msg = f"{j['DocNumber']} line {l['Amount']} names it but is tagged {tagged or 'no customer'}"
                    if c in res and msg not in res[c]["block"]:
                        res[c]["block"].append(msg)
                if tagged not in res:
                    continue
                r = res[tagged]
                if j["DocNumber"] not in r["mcr"]:
                    r["mcr"].append(j["DocNumber"])
                if d["PostingType"] == "Debit":
                    r["mcr_charge"] = round(r["mcr_charge"] + l["Amount"], 2)
                elif d["AccountRef"]["value"] == CLEARING:
                    r["mcr_clearing"] = round(r["mcr_clearing"] + l["Amount"], 2)
        pays = []
        for r in sorted(res.values(), key=lambda r: (r["checkin"], r["unit"])):
            want = round(r["total"] - r["clean"], 2)
            if not r["balance"]:
                r["note"] = "already closed"
                continue
            if abs(r["balance"] - r["total"]) > 0.005:
                r["block"].append(f"partly paid (balance {r['balance']})")
            if not r["mcr"]:
                r["block"].append("no charge journal")
            elif abs(r["mcr_charge"] - want) > 0.005 or abs(r["mcr_clearing"] - want) > 0.005:
                r["block"].append(f"charged {r['mcr_charge']} / clearing {r['mcr_clearing']}, expected {want}")
            if r["block"]:
                continue
            pays.append({"code": r["code"], "amount": round(r["balance"], 2), "customer": r["customer"]["value"],
                         "invoice_id": r["invoice_id"]})
            r.update(pay=round(r["balance"], 2), left=r["clean"], charged=r["mcr_charge"], refund=0.0,
                     note="cleaning held in clearing (owner decision pending)")
        return {"date": on, "res": res, "pays": pays, "adj": {}, "mcr": {}, "billpays": [], "cl_docs": []}

    # -- post -----------------------------------------------------------------------
    def post(self, plan) -> list[str]:
        out, on = [], plan["date"]
        blocked = {c for c, r in plan["res"].items() if r["block"]}
        for key, stays in sorted(plan.get("mcr", {}).items()):
            out.append(self._mcr(key, stays))
        for p in plan["pays"]:
            out.append(self._pay(p, p.get("date", on)))
        ap_lines = defaultdict(list)                     # bill doc -> JE ids that debit A/P for it
        for unit, stays in sorted(plan["adj"].items()):
            stays = [s for s in stays if s["code"] not in blocked]
            if not stays:
                continue
            msg, je_id = self._adj(unit, stays, on)
            out.append(msg)
            for s in stays:
                for side, acct, vendor, amt, desc in s["lines"]:
                    if vendor:
                        ap_lines[desc.split()[1]].append((je_id, amt))
        for b in plan["billpays"]:
            out.append(self._billpay(b, ap_lines.get(b["bill_doc"], []), on))
        return out

    def _mcr(self, key, stays):
        me, unit = key.split("|")
        doc = f"{me.replace('-', '')}_{unit}MCR"
        if self.one("JournalEntry", f"DocNumber = '{doc}'"):
            raise Refused(f"JE {doc} already exists")
        other = self.acct("Other Owner Expenses")
        lines = []
        for s in stays:
            base = {"DepartmentRef": {"value": self.trust}, "ClassRef": {"value": s["class"]["value"]},
                    "Entity": {"Type": "Customer", "EntityRef": {"value": s["customer"]["value"]}}}
            for side, acct in (("Debit", other), ("Credit", CLEARING)):
                lines.append({"DetailType": "JournalEntryLineDetail", "Amount": s["amount"],
                              "Description": f"{s['customer']['name']} | Airbnb payout received directly",
                              "JournalEntryLineDetail": {**base, "PostingType": side, "AccountRef": {"value": acct}}})
        total = round(sum(s["amount"] for s in stays), 2)
        if not self.confirm:
            return f"would JE {doc} {me} {total:.2f} ({len(stays)} stays: {' '.join(s['code'] for s in stays)})"
        r = self.qbo.post("journalentry", {"DocNumber": doc, "TxnDate": me, "Line": lines,
                                           "PrivateNote": f"Owner-paid Airbnb payouts, Elektra {unit}, {me[:7]} (owner's own Airbnb account)"})
        self.log({"code": " ".join(s["code"] for s in stays)}, "JournalEntry", "CREATE", r["Id"], doc, total,
                 "owner-paid Airbnb MCR")
        return f"JE {doc} Id {r['Id']} {total:.2f}"

    def _pay(self, p, on):
        inv = self.read("Invoice", p["invoice_id"])
        if abs(inv["Balance"] - p["amount"]) > 0.005:
            raise Refused(f"invoice {p['code']} balance is now {inv['Balance']}, not {p['amount']} -- rebuild")
        if not self.confirm:
            return f"would PAY {p['code']} {p['amount']:.2f} into Airbnb clearing, {on}"
        r = self.qbo.post("payment", {
            "CustomerRef": {"value": p["customer"]}, "TotalAmt": p["amount"], "TxnDate": on,
            "DepositToAccountRef": {"value": CLEARING},
            "PrivateNote": "Owner-paid Airbnb: payout received directly by the owner (charged on the MCR journal)",
            "Line": [{"Amount": p["amount"], "LinkedTxn": [{"TxnId": p["invoice_id"], "TxnType": "Invoice"}]}]})
        self.log(p, "Payment", "CREATE", r["Id"], p["code"], p["amount"], "owner-paid Airbnb")
        return f"PAY {p['code']} {p['amount']:.2f} Id {r['Id']}"

    def _adj(self, unit, stays, on):
        doc = f"{on.replace('-', '')}_{unit}MCRADJ"
        if self.one("JournalEntry", f"DocNumber = '{doc}'"):
            raise Refused(f"JE {doc} already exists")
        lines = []
        for s in stays:
            for side, acct, vendor, amt, desc in s["lines"]:
                ent = {"Type": "Vendor", "EntityRef": {"value": vendor["value"]}} if vendor else \
                      {"Type": "Customer", "EntityRef": {"value": s["customer"]["value"]}}
                lines.append({"DetailType": "JournalEntryLineDetail", "Amount": amt,
                              "Description": f"{s['customer']['name']} | {desc}",
                              "JournalEntryLineDetail": {"PostingType": side, "AccountRef": {"value": acct}, "Entity": ent,
                                                         "ClassRef": {"value": s["class"]["value"]},
                                                         "DepartmentRef": {"value": self.trust}}})
        total = round(sum(l["Amount"] for l in lines if l["JournalEntryLineDetail"]["PostingType"] == "Debit"), 2)
        if not self.confirm:
            return f"would JE {doc} {total:.2f} ({len(stays)} stays, {len(lines)} lines)", None
        r = self.qbo.post("journalentry", {"DocNumber": doc, "TxnDate": on, "Line": lines,
                                           "PrivateNote": f"Owner-paid Airbnb corrections, Elektra {unit}, {YEAR} (Accounting SOP Part E)"})
        self.log({"code": " ".join(s["code"] for s in stays)}, "JournalEntry", "CREATE", r["Id"], doc, total,
                 "owner-paid Airbnb adjustment")
        return f"JE {doc} Id {r['Id']} {total:.2f}", r["Id"]

    def _billpay(self, b, credits, on):
        bill = self.read("Bill", b["bill_id"])
        amt = round(sum(a for _, a in credits), 2)
        if abs(bill["Balance"] - amt) > 0.005:
            return f"SKIP bill {b['bill_doc']}: open {bill['Balance']}, adjustments cover {amt} -- apply in Pay Bills"
        if not self.confirm:
            return f"would apply {amt:.2f} of A/P adjustment to bill {b['bill_doc']} ({b['vendor']['name']}), $0 bill payment"
        try:
            r = self.qbo.post("billpayment", {
                "VendorRef": {"value": b["vendor"]["value"]}, "PayType": "Check", "TotalAmt": 0, "TxnDate": on,
                "CheckPayment": {"BankAccountRef": {"value": BANK}},
                "PrivateNote": "Applies the owner-paid Airbnb adjustment: not a real payable",
                "Line": [{"Amount": amt, "LinkedTxn": [{"TxnId": b["bill_id"], "TxnType": "Bill"}]}] +
                        [{"Amount": a, "LinkedTxn": [{"TxnId": j, "TxnType": "JournalEntry"}]} for j, a in credits]})
        except Exception as e:                           # noqa: BLE001 -- the UI route still works
            return f"bill {b['bill_doc']}: API refused the $0 bill payment ({str(e)[:120]}) -- apply in Pay Bills"
        self.log({"code": b["bill_doc"]}, "BillPayment", "CREATE", r["Id"], b["bill_doc"], 0, "applies A/P adjustment")
        return f"BILLPAY {b['bill_doc']} {amt:.2f} applied, Id {r['Id']}"


def write(plan, stamp):
    (REVIEW_DIR / f"owner_paid_plan_{stamp}.json").write_text(json.dumps(plan, indent=1))
    with (REVIEW_DIR / f"owner_paid_{stamp}.csv").open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["code", "unit", "checkin", "customer", "invoice_total", "cleaning", "mcr_journal",
                    "owner_charged_now", "refund_to_owner", "CL_docs", "payment_now", "left_in_clearing", "block_or_note"])
        for r in sorted(plan["res"].values(), key=lambda r: (r["checkin"], r["unit"])):
            if r["total"] == 0:
                continue
            w.writerow([r["code"], r["unit"], r["checkin"], r["customer"]["name"], r["total"], r["clean"],
                        " ".join(r["mcr"]) or r.get("new_mcr", ""), r.get("charged", ""), r.get("refund", ""),
                        " ".join(sorted({c["doc"] + ("(JE)" if c["entity"] == "JournalEntry" else "") for c in r["cl"]})),
                        r.get("pay", ""), r.get("left", ""), " | ".join(r["block"]) or r.get("note", "")])
        w.writerow([])
        w.writerow(["adjustment journal", "unit", "", "customer", "side", "account", "amount", "description"])
        for unit, stays in sorted(plan["adj"].items()):
            for s in stays:
                for side, acct, vendor, amt, desc in s["lines"]:
                    w.writerow([f"{plan['date'].replace('-', '')}_{unit}MCRADJ", unit, "", s["customer"]["name"], side,
                                acct + (f" (vendor {vendor['name']})" if vendor else ""), amt, desc])


def summary(plan):
    res = [r for r in plan["res"].values() if "charged" in r]
    print(f"\n{len(res)} stays checked; adjustments dated {plan['date']}")
    by = defaultdict(lambda: [0.0, 0.0, 0])
    for r in res:
        b = by[r["unit"]]
        b[0] += r["refund"]; b[1] += r["pay"]; b[2] += 1 if r["pay"] else 0
    for u, (ref, pay, n) in sorted(by.items()):
        print(f"  Elektra {u}: {n} payments {pay:,.2f}   owner refund on the {plan['date'][:7]} statement {ref:+,.2f}")
    for r in plan["res"].values():
        for b in r["block"]:
            print(f"  BLOCK {r['code']}: {b}")


def main():
    ap = argparse.ArgumentParser(description="Owner-paid Airbnb (Elektra) corrections; dry run unless --confirm.")
    ap.add_argument("--date", default=date.today().isoformat(), help="adjustment date (an unpublished month)")
    ap.add_argument("--backlog", action="store_true", help="12/2024-12/2025 stays: payments only")
    ap.add_argument("--confirm", action="store_true")
    a = ap.parse_args()
    fx = Fixer(a.confirm)
    if a.confirm:
        path = max(REVIEW_DIR.glob("owner_paid_plan_*.json"))
        plan = json.loads(path.read_text())
        print(f"{path.name}: posting")
    else:
        plan = fx.build_backlog(a.date) if a.backlog else fx.build(a.date)
        stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        write(plan, stamp)
        print(f"-> review/owner_paid_{stamp}.csv, owner_paid_plan_{stamp}.json  [DRY RUN]")
        summary(plan)
    print()
    for s in fx.post(plan):
        print(" ", s)


if __name__ == "__main__":
    main()
