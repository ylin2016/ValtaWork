"""Offline tests for invoices/build_form_expenses + the shared poster, against a fake QBO.

    venv/bin/python -m unittest tests.test_form_expenses -v
"""
from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from src.invoices import build_form_expenses as B
from src.invoices import post as P
from src.resolver import Resolver
from tests.test_guest_fees import FakeQBO, write_csv

SUPPLIES = "Trust Liabilities:Owner Payables:1C - Owner Expenses:Supplies - Owner"


class FakeQBO2(FakeQBO):
    def __init__(self):
        super().__init__()
        self.db["Account"] += [{"Id": "1617", "FullyQualifiedName": "CitiCostco3104-Supplies"},
                               {"Id": "884", "FullyQualifiedName": "Chase Checking 7197"},
                               {"Id": "1695", "FullyQualifiedName": SUPPLIES},
                               {"Id": "77", "FullyQualifiedName": "All units supplies"}]
        self.db["Class"] += [{"Id": "c9", "FullyQualifiedName": "Valta Realty", "Name": "Valta Realty"}]
        self.db["Department"] += [{"Id": "d2", "FullyQualifiedName": "Valta Realty"}]
        self.db["Vendor"] = [{"Id": "v1", "DisplayName": "Costco Wholesale"},
                             {"Id": "v2", "DisplayName": "Target"}]
        self.db["Purchase"] = []

    def _match(self, entity, where):
        if entity == "Vendor" and where:                 # live QBO: DisplayName = ignores case
            val = where.split("'")[1].replace("\\'", "'").lower()
            return [v for v in self.db["Vendor"] if v["DisplayName"].lower() == val]
        return super()._match(entity, where)

    def post(self, entity, body):
        if entity != "purchase":
            return super().post(entity, body)
        self._id += 1
        obj = {**body, "Id": str(self._id), "TotalAmt": sum(l["Amount"] for l in body["Line"])}
        self.db["Purchase"].append(obj)
        self.posted.append((entity, obj))
        return obj


def sub(id_, store="Costco", acct="credit-3104", cat="Supplies", prop="renton_18823",
        pname="Renton 18823", amount="75.25", status="approved", typed="", kind="expense"):
    return {"id": id_, "kind": kind, "status": status, "txn_date": "2026-09-21", "property_code": prop,
            "property_name": pname, "submitter": "Yi Lin", "amount": amount,
            "description": f"{store}_Stuff", "money_account": acct, "category": cat, "vendor": store,
            "typed_in": typed, "qbo_txn_id": "", "qbo_posted_at": ""}


class FormExpenses(unittest.TestCase):
    def setUp(self):
        self.exports = Path(tempfile.mkdtemp()) / "exports"
        write_csv(self.exports / "config" / "money_accounts.csv", [
            {"code": c, "label": c, "qb_account": q, "kind": k, "in_qbo": i} for c, q, k, i in (
                ("credit-3104", "CitiCostco3104-Supplies", "card", "TRUE"),
                ("ops-7197", "Chase Checking 7197", "bank", "TRUE"),
                ("personal", "", "personal", "TRUE"),
                ("valta-homes", "", "company", "FALSE"))])
        write_csv(self.exports / "config" / "expense_categories.csv", [
            {"code": "Supplies", "qb_account": SUPPLIES},
            {"code": "All unit supplies", "qb_account": "All units supplies"}])
        write_csv(self.exports / "config" / "properties.csv", [
            {"code": "renton_18823", "name": "Renton 18823", "qbo_class_name": "Listings:Renton 18823"}])
        self.qbo = FakeQBO2()

    def run_build(self, subs, files=None):
        write_csv(self.exports / "submissions.csv", subs)
        write_csv(self.exports / "attachments.csv", files or [
            {"submission_id": s["id"], "seq": "1", "upload_status": "uploaded",
             "file_name": f"20260921_{s['property_name']}_9.21_{s['vendor']}_Stuff_{s['amount']}.jpg",
             "drive_url": "https://drive/x"} for s in subs])
        return B.build(self.exports, self.qbo)

    def test_what_is_drafted_and_how(self):
        rows, warns = self.run_build([
            sub("1"),                                                     # card, Costco via payees.yml
            sub("2", store="target", acct="ops-7197", cat="All unit supplies", prop="", pname="Valta Realty"),
            sub("3", acct="personal"), sub("4", acct="valta-homes"),       # not here
            sub("5", status="pending"), sub("6", typed="category"),
            sub("7", store="Joe's Hardware")])
        by = {r["SubmissionId"]: r for r in rows}
        self.assertEqual(sorted(by), ["1", "2", "7"])
        r1 = by["1"]
        self.assertEqual((r1["Vendor"], r1["PaidFrom"], r1["PaymentType"], r1["Location"], r1["Class"], r1["DocNumber"]),
                         ("Costco Wholesale", "CitiCostco3104-Supplies", "CreditCard", "Trust",
                          "Listings:Renton 18823", "EF1"))
        self.assertEqual(r1["Memo"], "20260921_Renton 18823_9.21_Costco_Stuff_75.25")
        r2 = by["2"]
        self.assertEqual((r2["Vendor"], r2["PaymentType"], r2["Location"], r2["Class"]),
                         ("Target", "Cash", "Valta Realty", "Valta Realty"))
        self.assertIn("NO VENDOR", by["7"]["Vendor"])
        self.assertTrue(any("typed in" in w for w in warns))
        self.assertTrue(any("paid personally" in w for w in warns))
        self.assertTrue(any("not in this QBO" in w for w in warns))

    def test_post_dry_run_confirm_rerun(self):
        rows, _ = self.run_build([sub("1")])
        res = Resolver(self.qbo)
        st, _ = P.post_one("EF1", rows, self.qbo, res, {}, confirm=False, verbose=False)
        self.assertEqual(st, "ready")
        self.assertEqual(self.qbo.posted, [])

        st, _ = P.post_one("EF1", rows, self.qbo, res, {}, confirm=True, verbose=False)
        self.assertEqual(st, "posted")
        p = self.qbo.posted[-1][1]
        self.assertEqual(p["PaymentType"], "CreditCard")
        self.assertEqual(p["AccountRef"]["value"], "1617")
        self.assertEqual(p["EntityRef"], {"value": "v1", "type": "Vendor"})
        self.assertEqual(p["DepartmentRef"]["value"], "d1")
        self.assertEqual(p["PrivateNote"], "20260921_Renton 18823_9.21_Costco_Stuff_75.25")
        self.assertEqual(p["Line"][0]["AccountBasedExpenseLineDetail"]["ClassRef"]["value"], "c1")

        st, detail = P.post_one("EF1", rows, self.qbo, res, {}, confirm=True, verbose=False)
        self.assertEqual((st, len(self.qbo.posted)), ("skipped", 1), detail)

    def test_card_feed_already_booked_it(self):
        rows, _ = self.run_build([sub("1")])
        booked = {("CitiCostco3104-Supplies", "2026-09-22", 75.25): ("9/22 COSTCO", "555")}
        st, detail = P.post_one("EF1", rows, self.qbo, Resolver(self.qbo), booked, confirm=True, verbose=False)
        self.assertEqual(st, "skipped")
        self.assertIn("555", detail)
        self.assertEqual(self.qbo.posted, [])

    def test_unknown_store_blocks_the_post(self):
        rows, _ = self.run_build([sub("7", store="Joe's Hardware")])
        st, _ = P.post_one("EF7", rows, self.qbo, Resolver(self.qbo), {}, confirm=True, verbose=False)
        self.assertEqual(st, "error")
        self.assertEqual(self.qbo.posted, [])


if __name__ == "__main__":
    unittest.main()
