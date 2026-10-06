"""Guesty reservations -> planned QBO Invoice + fee Bills, for review. WRITES NOTHING TO QBO.

    python -m src.build_invoices                 # reservations changed since VRP stopped
    python -m src.build_invoices --parity        # rebuild what VRP already posted, diff it

Reads the latest stored Guesty pull (src.guesty_pull) and QBO snapshot (src.qbo_snapshot),
and writes to review/:

    plan_<stamp>.json          every object exactly as it would be posted -- the poster
                               reads THIS, so what posts is what was reviewed
    reservations_<stamp>.csv   one row per reservation: actions, totals, WARN / BLOCK
    lines_<stamp>.csv          every line of every Invoice / Bill to be written
    parity_<stamp>.csv         (--parity) ours vs VRP's, per object

Actions: CREATE (none in QBO), UPDATE (exists, differs), SAME, VOID (cancelled; live
invoice), ZERO (fee Bill no longer applies: lines to $0 as VRP did), SKIP, BLOCKED.
A BLOCKED reservation gets no objects at all -- fix the cause and rebuild.
"""
from __future__ import annotations

import argparse
import csv
import json
import warnings
from collections import Counter, defaultdict
from datetime import datetime, timezone
from decimal import ROUND_HALF_EVEN, Decimal

import yaml

from . import bridge, stripe_fees
from .codes import bill_doc, customer_name, doc_code, invoice_doc
from .guesty_store import latest_pull
from .paths import CONFIG_DIR, QBO_INPUTS, REVIEW_DIR
from .vrp_items import ITEM, item_for

CFG = yaml.safe_load((CONFIG_DIR / "vrp_format.yml").read_text())
CENT = 0.005


def r2(x) -> float:
    return round(float(x) + 0.0, 2)


def pct(rate: float, base: float) -> float:
    """rate x base to the cent, half to EVEN -- VRP's rounding. Measured on its posted
    Bills 2026-10-04: half-even ties 151/151 Deductions and 364/366 commissions; float
    round() misses 3 Deductions (6.49 for VRP's 6.50) and half-up misses 6."""
    return float((Decimal(str(rate)) * Decimal(str(base))).quantize(Decimal("0.01"), ROUND_HALF_EVEN))


# --- inputs ---------------------------------------------------------------------------
def load_snapshot(pulled_at: str) -> dict:
    p = QBO_INPUTS / f"snapshot_{pulled_at}.json"
    if not p.exists():
        raise SystemExit(f"No QBO snapshot for this pull ({p.name}). Run: python -m src.qbo_snapshot")
    return json.loads(p.read_text())


def breakdown_by_code(resvs: list[dict]) -> dict:
    """payment_model's channel / Stripe / Guesty fee per reservation -- the statement's
    own numbers, including its per-reservation override tables."""
    fm, pm = bridge.fetch_month(), bridge.payment_model()
    S = fm.build_summary_frame(resvs)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        bd, _ = pm.compute_breakdown(S, bridge.tax_rates_csv())
    return {row["confirmationCode"]: row for row in bd.to_dict("records")}


def cleaning_history(invoices: list[dict]) -> dict[str, str]:
    """Class -> the cleaning item VRP used on its latest invoice for that listing.

    VRP's owner/PM cleaning split is per listing and does NOT follow the statement
    project's owner_pays_cleaning flag (it books every OSBR and Yacinde unit as Owner).
    The books have run on VRP's split, so it is read from VRP's own invoices."""
    latest: dict[str, tuple[str, str]] = {}
    for inv in invoices:
        for ln in inv.get("Line", []):
            d = ln.get("SalesItemLineDetail")
            if not d or "Cleaning Fee (" not in d["ItemRef"]["name"]:
                continue
            k = (d.get("ClassRef") or {}).get("name")
            if k and (k not in latest or inv["TxnDate"] > latest[k][0]):
                latest[k] = (inv["TxnDate"], d["ItemRef"]["name"])
    return {k: v[1] for k, v in latest.items()}


# --- planning ---------------------------------------------------------------------------
class Planner:
    def __init__(self, pull: dict, snap: dict):
        self.qbo = bridge.qbo_client()
        self.res = bridge.resolver(self.qbo)
        self.conn = bridge.statements_db()
        self.pmr = bridge.pm_rate()
        self.cmap = bridge.class_map()
        self.invoices = {i["DocNumber"]: i for i in snap["invoices"]}
        self.bills = {b["DocNumber"]: b for b in snap["bills"]}
        self.clean_hist = cleaning_history(snap["invoices"])
        live = [r for r in pull["results"] if r.get("status") in ("confirmed", "canceled")]
        self.bd = breakdown_by_code(live)

    # -- lookups
    def klass(self, nickname: str, pid: str) -> str | None:
        """Live class FQN: the statement class map first, else `Listings:<nickname>`."""
        cand = [(self.cmap.get(pid) or {}).get("qbo_class_name"), f"Listings:{nickname}"]
        for c in filter(None, cand):
            fqn = self.res.klass_fqn(c)
            if fqn:
                return fqn
        return None

    def customer(self, r: dict, existing: dict | None) -> tuple[str, str | None]:
        if existing:
            return existing["CustomerRef"]["name"], existing["CustomerRef"]["value"]
        name = customer_name(r)
        return name, self.res.customer(name)

    # -- one reservation
    def plan(self, r: dict) -> dict:
        code, src = r["confirmationCode"], r.get("source") or ""
        nick = (r.get("listing") or {}).get("nickname") or ""
        ci, co = r.get("checkInDateLocalized") or r["checkIn"][:10], r.get("checkOutDateLocalized") or r["checkOut"][:10]
        nights = int(r.get("nightsCount") or 0)
        span = f"{ci[:10]} to {co[:10]} | {nights} nights"
        p = {"code": code, "doc_code": doc_code(r), "invoice_doc": invoice_doc(r),
             "source": src, "status": r["status"], "listing": nick, "check_in": ci[:10],
             "check_out": co[:10], "nights": nights, "guests": r.get("guestsCount"),
             "guesty_updated": r.get("lastUpdatedAt"), "warn": [], "block": [],
             "invoice": None, "bills": [], "guesty_payout": r2((r.get("money") or {}).get("hostPayout") or 0)}
        p["all_bill_docs"] = {k: bill_doc(v["prefix"], r) for k, v in CFG["bills"].items()}
        existing = self.invoices.get(p["invoice_doc"])
        p["existing_invoice"] = existing and {"Id": existing["Id"], "TotalAmt": existing["TotalAmt"],
                                              "SyncToken": existing["SyncToken"]}

        if src.lower() in CFG["skip_sources"]:
            p["action"] = "SKIP"
            p["warn"].append(f"source {src!r} is an owner stay -- not invoiced")
            return p

        pid = bridge.to_property_id(nick)
        p["property_id"] = pid
        klass = self.klass(nick, pid)
        p["class"] = klass
        if not klass:
            p["block"].append(f"no QBO class for listing {nick!r} (property_id {pid})")
        cust_name, cust_id = self.customer(r, existing)
        p["customer"], p["customer_id"] = cust_name, cust_id
        if not cust_id:
            p["warn"].append("new customer will be created")

        if r["status"] == "canceled":
            return self._plan_cancel(r, p)

        # -- invoice lines
        hist = self.clean_hist.get(klass or "")
        if hist:
            cleaning_owner = hist.endswith("(Owner)")
        else:
            cleaning_owner = self.pmr.owner_pays_cleaning_at(self.cmap.get(pid) or {}, ci[:10])
            p["warn"].append(f"no VRP cleaning history for {klass}: cleaning item from "
                             f"owner_pays_cleaning={cleaning_owner}")
        lines, skipped = [], 0.0
        for it in (r.get("money") or {}).get("invoiceItems") or []:
            amt = r2(it.get("amount") or 0)
            if abs(amt) < CENT:
                continue
            item, why = item_for(src, it, cleaning_owner)
            if item is None:
                if why.startswith("skip:"):
                    skipped += amt
                    p["warn"].append(f"{it.get('title')} {amt:.2f} not invoiced ({why[5:]})")
                else:
                    p["block"].append(why)
                continue
            lines.append({"item": item, "amount": amt,
                          "description": f"{(it.get('title') or '').strip()} | {span}"})
        total = r2(sum(l["amount"] for l in lines))
        if abs(total + skipped - p["guesty_payout"]) > CENT:
            p["warn"].append(f"invoice {total:.2f} + skipped {skipped:.2f} != Guesty payout {p['guesty_payout']:.2f}")
        p["invoice"] = {"DocNumber": p["invoice_doc"], "TxnDate": ci[:10], "total": total,
                        "lines": lines}

        # -- rates and fees
        rate = self.pmr.resolve_pm_fee_rate(self.conn, pid, ci[:10])
        p["pm_rate"] = rate
        if rate is None:
            p["block"].append(f"no PM rate for {pid} at {ci[:10]} (owner_contracts) -- ask")
        base_items = set(CFG["commission_base_items"])
        base = r2(sum(l["amount"] for l in lines if l["item"] in base_items))
        b = self.bd.get(code) or {}
        if not b:
            p["warn"].append("payment_model returned no row (zero payout?) -- no channel/Stripe fee")
        fees = {
            "commission": pct(rate or 0, base),
            "supplies": self._supplies(r, pid, p),
            CFG["channel_fee_bill"].get(b.get("row_group"), "_none"): r2(b.get("channel_fee") or 0),
            "stripe_fee": 0.0,
        }
        # Stripe + Guesty 1%: the actual fees from the Stripe payout files (owner,
        # 2026-10-04); payment_model only decides WHETHER the booking carries card fees.
        model_fee = r2((b.get("stripe_fee") or 0) + (b.get("guesty_fee") or 0))
        sfee = stripe_fees.reservation_fee(r, model_fee, CFG["vrp_cutoff_utc"].replace("Z", ""))
        fees["stripe_fee"] = sfee["fee"]
        p.update(stripe_pre_cutoff=sfee["pre_cutoff"], stripe_actual=sfee["actual"],
                 stripe_estimate=sfee["estimate"], stripe_fee_source=sfee["source"],
                 stripe_fee_detail=sfee["detail"])
        fees.pop("_none", None)
        p["row_group"] = b.get("row_group")
        p["commission_base"] = base
        p["fees"] = fees
        for kind, spec in CFG["bills"].items():
            amt = fees.get(kind, 0.0)
            p["bills"].append(self._bill(r, p, kind, spec, amt, rate or 0, span))
        sb = next(b for b in p["bills"] if b["kind"] == "stripe_fee")
        if sb["existing"]:
            p["stripe_vrp_posted"] = sb["existing"]["gross"]
            if abs(sb["existing"]["gross"] - p["stripe_pre_cutoff"]) > CENT:
                p["warn"].append(f"VRP's Stripe Bill {sb['existing']['gross']:.2f} != fees on payments "
                                 f"charged before its cutoff {p['stripe_pre_cutoff']:.2f} (VRP missed a payment)")

        return self._finish(p, existing)

    def _supplies(self, r: dict, pid: str, p: dict) -> float:
        row = bridge.contacts_row(pid)
        if row is None:
            p["block"].append(f"{pid} is not in Listing_contacts.csv -- supplies central/owner unknown")
            return 0.0
        if (row.get("Supplies") or "").strip().lower() != "central":
            return 0.0
        guests = int(r.get("guestsCount") or 0)
        if not guests:
            p["warn"].append("guestsCount is 0 -- supplies $0")
        n = min(int(r.get("nightsCount") or 0), CFG["supplies_max_nights"])
        return r2(CFG["supplies_rate_per_guest_night"] * guests * n)

    def _bill(self, r, p, kind, spec, amt, rate, span, cancelled=False) -> dict:
        doc = bill_doc(spec["prefix"], r)
        label = spec["label"]
        memo = f"{label} | {p['doc_code']} | {'Cancelled | ' if cancelled else ''}{span}"
        lines = []
        if abs(amt) >= CENT:
            lines = [{"account": spec["credit"], "amount": -amt, "description": memo},
                     {"account": spec["debit"], "amount": amt, "description": memo}]
            if spec.get("deduction"):
                # VRP keeps the pair even at $0 (a 0% listing), so do we.
                ded = pct(rate, amt)
                dmemo = f"{label} Deduction | {p['doc_code']} | {span}"
                lines += [{"account": CFG["deduction"]["debit"], "amount": ded, "description": dmemo},
                          {"account": CFG["deduction"]["credit"], "amount": -ded, "description": dmemo}]
        ex = self.bills.get(doc)
        return {"kind": kind, "DocNumber": doc, "vendor": spec["vendor"], "TxnDate": p["check_in"],
                "memo": memo, "amount": amt, "lines": lines,
                "existing": ex and {"Id": ex["Id"], "SyncToken": ex["SyncToken"],
                                    "gross": _bill_gross(ex)}}

    def _plan_cancel(self, r: dict, p: dict) -> dict:
        ex = p["existing_invoice"]
        kept = r2((r.get("money") or {}).get("totalPaid") or 0)
        if kept > CENT:
            p["warn"].append(f"cancelled but {kept:.2f} kept -- month-end: re-open with Cancellation Fee")
        if ex and abs(ex["TotalAmt"]) > CENT:
            p["action"] = "VOID"
        else:
            p["action"] = "SKIP"
            p["warn"].append("cancelled; no live invoice in QBO -- nothing to do")
        # Any fee Bill still carrying money goes to $0, as VRP did on a cancellation.
        span = f"{p['check_in']} to {p['check_out']} | {p['nights']} nights"
        for kind, spec in CFG["bills"].items():
            b = self._bill(r, p, kind, spec, 0.0, 0.0, span, cancelled=True)
            if b["existing"] and b["existing"]["gross"] > CENT:
                b["action"] = "ZERO"
                p["bills"].append(b)
        if p["block"] and p["action"] != "SKIP":
            p["action"] = "BLOCKED"
        return p

    def _finish(self, p: dict, existing: dict | None) -> dict:
        if p["block"]:
            p["action"] = "BLOCKED"
            p["bills"] = []
            return p
        inv = p["invoice"]
        if not existing:
            inv["action"] = "CREATE"
        elif _same_invoice(existing, inv, p["class"]):
            inv["action"] = "SAME"
        else:
            inv["action"] = "UPDATE"
        keep = []
        for b in p["bills"]:
            ex = b["existing"]
            if not b["lines"]:
                if ex and ex["gross"] > CENT:
                    b["action"] = "ZERO"
                    keep.append(b)
                continue                       # only applicable Bills (owner decision 2)
            if not ex:
                b["action"] = "CREATE"
            elif _same_bill(self.bills[b["DocNumber"]], b, p["class"]):
                b["action"] = "SAME"
            else:
                b["action"] = "UPDATE"
            keep.append(b)
        p["bills"] = keep
        acts = {inv["action"]} | {b["action"] for b in keep}
        p["action"] = "SAME" if acts == {"SAME"} else "CHANGE"
        return p


def _bill_gross(b: dict) -> float:
    return r2(sum(l["Amount"] for l in b.get("Line", [])
                  if l["Amount"] > 0 and "Deduction" not in (l.get("Description") or "")))


def _same_invoice(ex: dict, inv: dict, klass: str | None) -> bool:
    theirs = Counter((l["SalesItemLineDetail"]["ItemRef"]["name"], r2(l["Amount"]), l.get("Description"),
                      (l["SalesItemLineDetail"].get("ClassRef") or {}).get("name"))
                     for l in ex.get("Line", []) if l.get("DetailType") == "SalesItemLineDetail")
    ours = Counter((l["item"], l["amount"], l["description"], klass) for l in inv["lines"])
    return theirs == ours and ex.get("TxnDate") == inv["TxnDate"]


def _same_bill(ex: dict, b: dict, klass: str | None) -> bool:
    theirs = Counter((l["AccountBasedExpenseLineDetail"]["AccountRef"]["name"], r2(l["Amount"]),
                      l.get("Description"), (l["AccountBasedExpenseLineDetail"].get("ClassRef") or {}).get("name"))
                     for l in ex.get("Line", []) if l.get("AccountBasedExpenseLineDetail"))
    ours = Counter((l["account"], r2(l["amount"]), l["description"], klass) for l in b["lines"])
    return theirs == ours and ex.get("TxnDate") == b["TxnDate"]


# --- parity ------------------------------------------------------------------------------
def parity_rows(p: dict, inv_ex: dict | None, bills: dict) -> list[dict]:
    rows = []
    if p.get("invoice") is not None:
        theirs = r2(inv_ex["TotalAmt"]) if inv_ex else None
        rows.append({"code": p["code"], "object": "Invoice", "doc": p["invoice_doc"],
                     "ours": p["invoice"]["total"], "vrp": theirs, "status": p["invoice"].get("action")})
    seen = set()
    for b in p.get("bills", []):
        seen.add(b["DocNumber"])
        ex = bills.get(b["DocNumber"])
        rows.append({"code": p["code"], "object": b["kind"], "doc": b["DocNumber"], "ours": b["amount"],
                     "vrp": _bill_gross(ex) if ex else None, "status": b.get("action")})
    # A VRP Bill carrying money where we plan none.
    for kind, doc in p.get("all_bill_docs", {}).items():
        ex = bills.get(doc)
        if doc not in seen and ex and _bill_gross(ex) > CENT:
            rows.append({"code": p["code"], "object": kind, "doc": doc, "ours": 0.0,
                         "vrp": _bill_gross(ex), "status": "NOT PLANNED"})
    return rows


# --- output ------------------------------------------------------------------------------
def write_outputs(plans: list[dict], stamp: str, tag: str, snap: dict) -> None:
    REVIEW_DIR.mkdir(parents=True, exist_ok=True)
    (REVIEW_DIR / f"plan_{tag}{stamp}.json").write_text(json.dumps(plans, indent=1, default=str))

    cols = ["action", "code", "invoice_doc", "source", "status", "listing", "class", "check_in",
            "check_out", "nights", "guests", "pm_rate", "guesty_payout", "invoice_total",
            "invoice_action", "commission", "supplies", "channel_fee", "stripe_fee", "stripe_vrp_posted", "stripe_pre_cutoff",
            "stripe_actual", "stripe_estimate", "stripe_fee_source", "stripe_fee_detail", "bill_actions",
            "customer", "block", "warn"]
    with (REVIEW_DIR / f"reservations_{tag}{stamp}.csv").open("w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=cols)
        w.writeheader()
        for p in plans:
            f = p.get("fees") or {}
            chan = sum(v for k, v in f.items() if k.endswith("_fee") and k != "stripe_fee")
            w.writerow({**{k: p.get(k) for k in cols if k in p},
                        "invoice_total": (p.get("invoice") or {}).get("total"),
                        "invoice_action": (p.get("invoice") or {}).get("action") or
                                          ("VOID" if p["action"] == "VOID" else ""),
                        "commission": f.get("commission"), "supplies": f.get("supplies"),
                        "channel_fee": chan or None, "stripe_fee": f.get("stripe_fee"),
                        "bill_actions": "; ".join(f"{b['DocNumber']}:{b['action']}" for b in p.get("bills", [])),
                        "block": " | ".join(p["block"]), "warn": " | ".join(p["warn"])})

    with (REVIEW_DIR / f"lines_{tag}{stamp}.csv").open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["object", "doc", "action", "date", "customer", "class", "item_or_account",
                    "amount", "description"])
        for p in plans:
            if p.get("invoice") and p["invoice"].get("action") in ("CREATE", "UPDATE"):
                for l in p["invoice"]["lines"]:
                    w.writerow(["Invoice", p["invoice_doc"], p["invoice"]["action"], p["check_in"],
                                p["customer"], p["class"], l["item"], l["amount"], l["description"]])
            for b in p.get("bills", []):
                if b["action"] in ("CREATE", "UPDATE", "ZERO"):
                    for l in b["lines"] or [{"account": "(all lines to $0)", "amount": 0, "description": b["memo"]}]:
                        w.writerow([f"Bill {b['vendor']}", b["DocNumber"], b["action"], b["TxnDate"],
                                    p["customer"], p["class"], l["account"], l["amount"], l["description"]])

    if tag:
        inv = {i["DocNumber"]: i for i in snap["invoices"]}
        bills = {b["DocNumber"]: b for b in snap["bills"]}
        with (REVIEW_DIR / f"parity_{stamp}.csv").open("w", newline="") as fh:
            w = csv.DictWriter(fh, fieldnames=["code", "object", "doc", "ours", "vrp", "diff", "status"])
            w.writeheader()
            for p in plans:
                for row in parity_rows(p, inv.get(p["invoice_doc"]), bills):
                    d = None if row["vrp"] is None else r2(row["ours"] - row["vrp"])
                    w.writerow({**row, "diff": d})


def main():
    ap = argparse.ArgumentParser(description="Plan QBO Invoices + fee Bills from Guesty (dry run).")
    ap.add_argument("--parity", action="store_true",
                    help="build reservations VRP already posted (updated before its cutoff) and diff")
    ap.add_argument("--pull", help="a specific inputs/guesty/reservations_*.json")
    a = ap.parse_args()

    path, pull = latest_pull(a.pull)
    snap = load_snapshot(pull["pulled_at"])
    cutoff = CFG["vrp_cutoff_utc"].replace("Z", "")
    resvs = [r for r in pull["results"]
             if r.get("confirmationCode") and r.get("status") in ("confirmed", "canceled")]
    if a.parity:
        inv_docs = {i["DocNumber"] for i in snap["invoices"]}
        resvs = [r for r in resvs if r["lastUpdatedAt"] < cutoff and invoice_doc(r) in inv_docs]
    else:
        # Changed since VRP stopped -- plus anything charged in a Stripe payout file, whose
        # Stripe fee Bill now has an actual amount even if Guesty itself did not change.
        charged = set(stripe_fees.load().by_code)
        resvs = [r for r in resvs if r["lastUpdatedAt"] >= cutoff or r["confirmationCode"] in charged]
    print(f"{path.name}: planning {len(resvs)} reservation(s){' (parity)' if a.parity else ''}")

    planner = Planner(pull, snap)
    plans = [planner.plan(r) for r in resvs]
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    write_outputs(plans, stamp, "parity_" if a.parity else "", snap)

    acts = Counter(p["action"] for p in plans)
    objs = Counter()
    for p in plans:
        if p.get("invoice"):
            objs[("Invoice", p["invoice"].get("action"))] += 1
        if p["action"] == "VOID":
            objs[("Invoice", "VOID")] += 1
        for b in p.get("bills", []):
            objs[(b["kind"], b["action"])] += 1
    print("reservations:", dict(acts))
    for k in sorted(objs, key=str):
        print(f"  {k[0]:12} {str(k[1]):8} {objs[k]}")
    blocks = Counter(b.split(" (")[0].split(" at ")[0] for p in plans for b in p["block"])
    for b, n in blocks.most_common():
        print(f"  BLOCK x{n}: {b}")
    print(f"-> review/*_{'parity_' if a.parity else ''}{stamp}.*")


if __name__ == "__main__":
    main()
