"""Payout files -> planned QBO Payments + payout JEs, for review. WRITES NOTHING TO QBO.

    python -m src.build_payouts

Two-step clearing (QBO_operations' model, owner decisions 2026-10-02 / 10-06):

    Payment     Dr Payments Clearing - <channel>  Cr A/R     one per reservation per payout
    Payout JE   Dr Chase Trust Checking 9967      Cr clearing  one per payout (bank feed matches it)

Reads the verified payout files (src.payout_inbox output):
    inputs/airbnb/airbnb_<account>_<from>_<to>.csv     one Payout row + its detail rows
    inputs/bookingcom/Payout_Statement__*.csv           grouped by Statement Descriptor
    inputs/stripe/<amount> Payout.csv                   one payout each; po_ id from the
                                                        `Stripe platform payment*.csv` list
Hopper stays manual (owner, 2026-10-06) and is not built here.

Line routing (VRP's / QBO_operations' format):
    reservation money        Cr clearing, Payment applies it to the invoice
    refund / adjustment      stay happened -> Dr Resolutions; booking CANCELLED -> clearing
    resolution payout        Cr Resolutions (Airbnb pays the owner side directly)
    Stripe fees              Dr Fee - Stripe Processing, split Stripe / Guesty application fee
    application fee refund   Cr Fee - Stripe Processing

Writes review/payouts_plan_<stamp>.json (what the poster will post), payouts_<stamp>.csv
(one row per payout) and payout_lines_<stamp>.csv (every Payment and JE line).
A payout with any BLOCK is planned but will not post until the cause is fixed.
"""
from __future__ import annotations

import csv
import json
import re
from collections import defaultdict
from datetime import datetime, timezone
from decimal import Decimal

import yaml

from . import bridge
from .codes import invoice_doc
from .guesty_store import latest_pull
from .paths import CONFIG_DIR, INPUTS_DIR, REVIEW_DIR
from .payout_inbox import airbnb_payouts
from .resolver_util import esc
from .stripe_fees import charge_code

CFG = yaml.safe_load((CONFIG_DIR / "vrp_format.yml").read_text())
PCFG = CFG["payouts"]
START = str(yaml.safe_load((CONFIG_DIR / "payout_accounts.yml").read_text())["start_date"])
CENT = 0.005


def r2(x) -> float:
    return round(float(x) + 0.0, 2)


def money(s) -> float:
    s = str(s or "").replace(",", "").replace("$", "").strip()
    return float(Decimal(s)) if s else 0.0


def mdy(s: str) -> str:
    m, d, y = s.split("/")
    return f"{y}-{int(m):02d}-{int(d):02d}"


def read_csv(p):
    with open(p, encoding="utf-8-sig", newline="") as fh:
        return list(csv.DictReader(fh))


# --- payout files -------------------------------------------------------------------------
def airbnb_files() -> list[dict]:
    out = []
    for p in sorted((INPUTS_DIR / "airbnb").glob("airbnb_*_*.csv")):
        acct = re.match(r"airbnb_(\d+)_", p.name).group(1)
        payouts, _ = airbnb_payouts(read_csv(p))
        for po in payouts:
            head = po["rows"][0]
            out.append({"channel": "airbnb", "id": po["ref"], "date": po["date"].isoformat(),
                        "amount": r2(po["amount"]), "account": acct, "file": p.name,
                        "bank_desc": f"Payout | {head['Details']} | {po['ref']}",
                        "rows": [{"type": r["Type"], "code": r["Confirmation code"],
                                  "amount": money(r["Amount"]), "details": r.get("Details", ""),
                                  "guest": r.get("Guest", "")} for r in po["rows"][1:]]})
    return out


def booking_files() -> list[dict]:
    by: dict[str, dict] = {}
    for p in sorted((INPUTS_DIR / "bookingcom").glob("*.csv")):
        for r in read_csv(p):
            sd = r["Statement Descriptor"]
            po = by.setdefault(sd, {"channel": "bookingcom", "id": sd, "date": r["Payout date"],
                                    "file": p.name, "rows": [],
                                    "bank_desc": f"Payout | Bank Account | {sd}"})
            po["rows"].append({"type": r["Transaction type"], "code": r["Reservation number"],
                               "amount": money(r["Total amount (gross)"]), "details": ""})
    for po in by.values():
        po["amount"] = r2(sum(x["amount"] for x in po["rows"]))
    return list(by.values())


def stripe_files() -> list[dict]:
    lists = [r for p in (INPUTS_DIR / "stripe").glob("Stripe platform payment*.csv") for r in read_csv(p)]
    out = []
    for p in sorted((INPUTS_DIR / "stripe").glob("*Payout.csv")):
        rows = read_csv(p)
        net = r2(sum(money(r["Net"]) for r in rows))
        po = [x for x in lists if abs(money(x["Amount"]) - net) < CENT]
        pid = po[0]["id"] if len(po) == 1 else None
        date = (po[0]["Arrival Date (UTC)"] or po[0]["Created (UTC)"])[:10] if pid else None
        out.append({"channel": "stripe", "id": pid, "date": date, "amount": net, "file": p.name,
                    "bank_desc": f"Payout | •••••9967 (USD) | {pid}",
                    "rows": [{"type": r["Type"], "id": r["ID"], "amount": money(r["Amount"]),
                              "fees": money(r["Fees"]), "net": money(r["Net"]),
                              "details": r.get("Description", ""),
                              "code": charge_code(r) if r["Type"] in ("Charge", "Refund") else ""}
                             for r in rows]})
    return out


# --- planner ------------------------------------------------------------------------------
class Planner:
    def __init__(self, payouts: list[dict]):
        self.qbo = bridge.qbo_client()
        self.payouts = payouts
        _, pull = latest_pull()
        self.guesty = {}
        for r in pull["results"]:
            if r.get("confirmationCode"):
                self.guesty[r["confirmationCode"]] = r
                bc = ((r.get("integration") or {}).get("bookingCom") or {}).get("reservationId")
                if bc:
                    self.guesty[str(bc)] = r
        self.app_fee = self._guesty_app_fees()
        # Per booking, across every payout in the run: reservation money and Airbnb
        # adjustments. When Guesty already netted an adjustment into the invoice
        # (invoice total == reservations + adjustments), the adjustment is the same money
        # coming back out of clearing -- not a refund to charge the owner again.
        self.res_sum, self.adj_sum = defaultdict(float), defaultdict(float)
        for po in payouts:
            for r in po["rows"]:
                if po["channel"] == "airbnb" and r["type"] in ("Reservation", "Pass Through Tot"):
                    self.res_sum[r["code"]] += r["amount"]
                elif po["channel"] == "airbnb" and r["type"] == "Adjustment":
                    self.adj_sum[r["code"]] += r["amount"]
        self._load_invoices()
        self._load_existing()

    def _guesty_app_fees(self) -> dict[str, float]:
        """Stripe charge id -> the Guesty application fee actually taken (from Guesty)."""
        from .stripe_fees import _charges
        out = {}
        for r in self.guesty.values():
            for p in (r.get("money") or {}).get("payments") or []:
                for c in _charges(p):
                    if c.get("application_fee_amount") is not None:
                        out[c["id"]] = c["application_fee_amount"] / 100.0
        return out

    def _doc(self, channel: str, code: str) -> str:
        if channel == "bookingcom":
            return code[:21]
        g = self.guesty.get(code)
        return invoice_doc(g) if g else code[:21]

    def _load_invoices(self):
        docs = sorted({self._doc(po["channel"], r["code"]) for po in self.payouts
                       for r in po["rows"] if r.get("code") and not r["code"].startswith("Application fee")})
        self.invoices = {}
        for i in range(0, len(docs), 40):
            part = ", ".join(f"'{esc(d)}'" for d in docs[i:i + 40])
            for inv in self.qbo.query_all(f"SELECT * FROM Invoice WHERE DocNumber IN ({part})", "Invoice"):
                self.invoices[inv["DocNumber"]] = inv

    def _load_existing(self):
        je_post = bridge._sub("qbo_ops", bridge.QBO_OPS_ROOT, "je.post")
        self.bank_index = je_post.existing_payouts(self.qbo, start=START)
        self.je_docs = {j.get("DocNumber") for j in self.qbo.query_all(
            f"SELECT * FROM JournalEntry WHERE TxnDate >= '{START}'", "JournalEntry")}
        self.payments = self.qbo.query_all(f"SELECT * FROM Payment WHERE TxnDate >= '{START}'", "Payment")

    # -- per invoice facts
    def facts(self, channel, code):
        inv = self.invoices.get(self._doc(channel, code))
        g = self.guesty.get(code)
        if not inv:
            return None, g
        ln = next((l for l in inv["Line"] if l.get("DetailType") == "SalesItemLineDetail"), None)
        span = (ln.get("Description") or "").split(" | ", 1)[1] if ln and " | " in (ln.get("Description") or "") else ""
        return {"Id": inv["Id"], "doc": inv["DocNumber"], "balance": r2(inv["Balance"]),
                "total": r2(inv["TotalAmt"]), "customer": inv["CustomerRef"]["name"],
                "customer_id": inv["CustomerRef"]["value"],
                "class": ((ln or {}).get("SalesItemLineDetail", {}).get("ClassRef") or {}).get("name"),
                "span": span, "voided": "Voided" in (inv.get("PrivateNote") or "")}, g

    def netted(self, code: str, f: dict) -> bool:
        a = self.adj_sum.get(code, 0.0)
        return a < -CENT and abs(self.res_sum.get(code, 0.0) + a - f["total"]) < 0.02

    def plan(self) -> list[dict]:
        plans, applied = [], defaultdict(float)
        for po in sorted(self.payouts, key=lambda x: (x["channel"], x["date"] or "", x["id"] or "")):
            plans.append(self._plan_one(po, applied))
        return plans

    def _plan_one(self, po, applied) -> dict:
        ch = po["channel"]
        p = {**{k: po[k] for k in ("channel", "id", "date", "amount", "file")},
             "payments": [], "lines": [], "block": [], "warn": [], "action": "CREATE"}
        if not po["id"] or not po["date"]:
            p["block"].append("no payout id/date (Stripe file not in the payout list?)")
        if po["date"] and po["date"] < START:
            p["action"] = "SKIP"
            p["warn"].append(f"before {START} (VRP era)")
            return p
        doc = (po["id"] or "")[:21]
        p["DocNumber"], p["memo"] = doc, f"External Payment ID: {po['id']}"
        if doc in self.je_docs or (po["date"], po["amount"]) in self.bank_index:
            p["action"] = "SKIP"
            hit = self.bank_index.get((po["date"], po["amount"]))
            p["warn"].append(f"already booked in QBO ({hit[0] if hit else doc})")
            return p
        clearing = PCFG["clearing"][ch]
        pay_by_code = defaultdict(float)

        def line(acct, amt, desc, f=None, cls=None):
            # amt > 0 = credit, < 0 = debit (the payout's sign convention)
            p["lines"].append({"account": acct, "posting": "Credit" if amt >= 0 else "Debit",
                               "amount": r2(abs(amt)), "description": desc,
                               "customer": (f or {}).get("customer"),
                               "customer_id": (f or {}).get("customer_id"),
                               "class": cls if cls is not None else (f or {}).get("class")})

        for r in po["rows"]:
            typ, code, amt = r["type"], r.get("code") or "", r["amount"]
            if ch == "stripe" and typ == "Adjustment":
                m = re.search(r"(ch_\w+)", r["details"])
                src = next((x for x in po["rows"] if m and x.get("id") == m.group(1)), None)
                f, _ = self.facts(ch, src["code"]) if src else (None, None)
                if not f:                       # the charge itself sits in an earlier payout
                    f = self._facts_for_charge(m.group(1) if m else "")
                line(PCFG["stripe_fee"], amt, f"{r['details']} | {self._tail(ch, f)}", f)
                continue
            f, g = self.facts(ch, code)
            if not f:
                p["block"].append(f"{typ} {code} {amt:.2f}: no invoice in QBO")
                continue
            cancelled = (g or {}).get("status") == "canceled" or f["voided"]
            tail = self._tail(ch, f)
            if ch == "stripe" and typ == "Charge":
                line(clearing, amt, f"{code} | {tail}", f)
                guesty_fee = r2(self.app_fee.get(r["id"], round(amt * 0.01, 2)))
                stripe_fee = r2(r["fees"] - guesty_fee)
                line(PCFG["stripe_fee"], -stripe_fee, f"Stripe processing fees | {tail}", f)
                line(PCFG["stripe_fee"], -guesty_fee, f"Guesty application fee | {tail}", f)
                if not cancelled:
                    pay_by_code[code] += amt
            elif ch == "stripe" and typ == "Refund":
                acct = clearing if cancelled else PCFG["resolutions"]
                line(acct, amt, f"REFUND FOR CHARGE ({code}) | {tail}", f)
            elif ch == "airbnb" and typ in ("Reservation", "Pass Through Tot"):
                line(clearing, amt, f"{typ} | airbnb | {f['span']}" + (f" | {r['details']}" if r["details"] else ""), f)
                if not cancelled:
                    pay_by_code[code] += amt
            elif ch == "airbnb" and typ in ("Adjustment", "Resolution Payout", "Resolution Adjustment"):
                acct = (clearing if typ == "Adjustment" and (cancelled or self.netted(code, f))
                        else PCFG["resolutions"])
                line(acct, amt, f"{typ} | airbnb | {f['span']}" + (f" | {r['details']}" if r["details"] else ""), f)
            elif ch == "airbnb" and typ == "Misc Credit":
                line(PCFG["commission_revenue"], amt, f"{typ} | airbnb | {r['details']}", f, PCFG["company_class"])
            elif ch == "bookingcom" and typ == "RESERVATION":
                line(clearing, amt, f"RESERVATION | bookingcom | {f['span']}", f)
                if not cancelled:
                    pay_by_code[code] += amt
            elif ch == "bookingcom" and typ == "REFUND":
                line(clearing if cancelled else PCFG["resolutions"], amt,
                     f"REFUND | bookingcom | {f['span']}", f)
            else:
                p["block"].append(f"no rule for {ch} row type {typ!r} ({code} {amt:.2f}) -- ask")
                continue
            if cancelled and typ in ("Charge", "Reservation", "RESERVATION"):
                p["warn"].append(f"{code} is CANCELLED: {amt:.2f} credited to clearing, no Payment "
                                 f"(its refund washes there); existing payments on the invoice need voiding")

        # the bank line: the whole payout
        line(PCFG["bank"], -po["amount"], po["bank_desc"])
        dr = sum(l["amount"] for l in p["lines"] if l["posting"] == "Debit")
        cr = sum(l["amount"] for l in p["lines"] if l["posting"] == "Credit")
        if abs(dr - cr) > CENT:
            p["block"].append(f"JE does not balance: debits {dr:.2f} credits {cr:.2f}")

        # payments, one per reservation in this payout
        for code, amt in pay_by_code.items():
            f, _ = self.facts(ch, code)
            dup = [x for x in self.payments if x["CustomerRef"]["value"] == f["customer_id"]
                   and po["id"] and po["id"] in (x.get("PrivateNote") or "")]
            if dup:
                p["warn"].append(f"{code}: payment for {po['id']} already exists (Id {dup[0]['Id']}) -- skipped")
                continue
            room = r2(f["balance"] - applied[f["Id"]])
            if amt > room + CENT:
                excess = r2(amt - max(room, 0.0))
                amt = r2(max(room, 0.0))
                if ch == "airbnb" and self.netted(code, f):
                    p["warn"].append(f"{code}: payment capped at the open balance {room:.2f}; the "
                                     f"{excess:.2f} over it comes back out of clearing with its Airbnb "
                                     f"adjustment (already netted on the invoice)")
                else:
                    # Money beyond what the invoice asks for is the owner's (typically a
                    # re-charge after a refund that was booked to Resolutions): move it there.
                    line(clearing, -excess, f"Excess over invoice {f['doc']} -> Resolutions | {self._tail(ch, f)}", f)
                    line(PCFG["resolutions"], excess, f"Received beyond invoice {f['doc']} | {self._tail(ch, f)}", f)
                    p["warn"].append(f"{code}: {excess:.2f} received beyond invoice {f['doc']} (open "
                                     f"{room:.2f}) -> credited to Resolutions")
            if amt < CENT:
                continue
            applied[f["Id"]] += amt
            p["payments"].append({"code": code, "invoice_id": f["Id"], "invoice_doc": f["doc"],
                                  "customer": f["customer"], "customer_id": f["customer_id"],
                                  "amount": r2(amt), "TxnDate": po["date"], "PaymentRefNum": code[:21],
                                  "PrivateNote": p["memo"], "deposit_to": clearing})
        if p["block"]:
            p["action"] = "BLOCKED"
        return p

    def _tail(self, ch, f) -> str:
        src = {"stripe": "", "airbnb": "airbnb", "bookingcom": "bookingcom"}[ch]
        if ch == "stripe":
            src = (f["customer"] or "").split(" - ")[0]
        return f"{src} | {f['span']}"

    def _facts_for_charge(self, chid: str):
        for r in self.guesty.values():
            from .stripe_fees import _charges
            for pmt in (r.get("money") or {}).get("payments") or []:
                if any(c["id"] == chid for c in _charges(pmt)):
                    return self.facts("stripe", r["confirmationCode"])[0]
        return None


# --- output -------------------------------------------------------------------------------
def write(plans: list[dict], stamp: str) -> None:
    REVIEW_DIR.mkdir(exist_ok=True)
    (REVIEW_DIR / f"payouts_plan_{stamp}.json").write_text(json.dumps(plans, indent=1, default=str))
    with (REVIEW_DIR / f"payouts_{stamp}.csv").open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["action", "channel", "payout_id", "date", "amount", "payments", "payments_total",
                    "je_lines", "file", "block", "warn"])
        for p in plans:
            w.writerow([p["action"], p["channel"], p["id"], p["date"], p["amount"], len(p["payments"]),
                        r2(sum(x["amount"] for x in p["payments"])), len(p["lines"]), p["file"],
                        " | ".join(p["block"]), " | ".join(p["warn"])])
    with (REVIEW_DIR / f"payout_lines_{stamp}.csv").open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["payout_id", "date", "object", "account_or_invoice", "debit", "credit", "customer",
                    "class", "description"])
        for p in plans:
            if p["action"] == "SKIP":
                continue
            for x in p["payments"]:
                w.writerow([p["id"], x["TxnDate"], "Payment", f"Invoice {x['invoice_doc']} -> {x['deposit_to'].split(':')[-1]}",
                            x["amount"], "", x["customer"], "", x["PrivateNote"]])
            for l in p["lines"]:
                w.writerow([p["id"], p["date"], "JE", l["account"].split(":")[-1],
                            l["amount"] if l["posting"] == "Debit" else "",
                            l["amount"] if l["posting"] == "Credit" else "",
                            l["customer"] or "", l["class"] or "", l["description"]])


def main():
    payouts = airbnb_files() + booking_files() + stripe_files()
    plans = Planner(payouts).plan()
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    write(plans, stamp)
    from collections import Counter
    print(f"{len(plans)} payout(s):", dict(Counter((p['channel'], p['action']) for p in plans)))
    for p in plans:
        if p["action"] != "SKIP":
            print(f"  {p['action']:8} {p['channel']:10} {p['date']} {str(p['id']):22} {p['amount']:>10,.2f}"
                  f"  payments {len(p['payments'])} ({sum(x['amount'] for x in p['payments']):,.2f})")
            for b in p["block"]:
                print(f"      BLOCK {b}")
            for wn in p["warn"]:
                print(f"      warn  {wn}")
    print(f"-> review/payouts_{stamp}.csv, payout_lines_{stamp}.csv, payouts_plan_{stamp}.json")


if __name__ == "__main__":
    main()
