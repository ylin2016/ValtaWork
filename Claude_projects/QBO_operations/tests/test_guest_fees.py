"""Offline tests for deposits/build_guest_fees + post_guest_fees against a fake QBO.

    venv/bin/python -m unittest tests.test_guest_fees -v
"""
from __future__ import annotations

import csv
import re
import tempfile
import unittest
from pathlib import Path

from src.deposits import build_guest_fees as B
from src.deposits import post_guest_fees as P
from src.resolver import Resolver


class FakeQBO:
    """Just the queries these two scripts make, over in-memory entities."""

    def __init__(self):
        self.db = {
            "Account": [{"Id": "35", "FullyQualifiedName": "Chase Trust Checking 9967 - STR"},
                        {"Id": "4", "FullyQualifiedName": "Undeposited Funds"}],
            "Class": [{"Id": "c1", "FullyQualifiedName": "Listings:Renton 18823"}],
            "Item": [{"Id": "i-pet", "FullyQualifiedName": "Pet Fee (Owner)"},
                     {"Id": "i-park", "FullyQualifiedName": "Parking Fee"}],
            "Department": [{"Id": "d1", "FullyQualifiedName": "Trust"}],
            "Customer": [{"Id": "900", "DisplayName": "John Smith - HMABC123"},
                         {"Id": "901", "DisplayName": "Ann Lee - HMPAID99"},
                         {"Id": "902", "DisplayName": "Bo Wu - HMNEW777"}],
            "Invoice": [
                {"Id": "5001", "DocNumber": "HMABC123", "CustomerRef": {"value": "900"}, "Balance": 150,
                 "Line": [{"Amount": 900, "SalesItemLineDetail": {"ItemRef": {"value": "rent"}}},
                          {"Amount": 150, "SalesItemLineDetail": {"ItemRef": {"value": "i-pet"}}}]},
                {"Id": "5002", "DocNumber": "HMPAID99", "CustomerRef": {"value": "901"}, "Balance": 0,
                 "Line": [{"Amount": 100, "SalesItemLineDetail": {"ItemRef": {"value": "i-pet"}}}]},
                {"Id": "5003", "DocNumber": "HMNEW777", "CustomerRef": {"value": "902"}, "Balance": 0,
                 "Line": [{"Amount": 700, "SalesItemLineDetail": {"ItemRef": {"value": "rent"}}}]}],
            "Payment": [], "Deposit": [], "JournalEntry": []}
        self.posted = []
        self._id = 7000

    def _match(self, entity, where):
        rows = self.db.get(entity, [])
        for field, op, val in re.findall(r"(\w+)\s*(=|>=|<=|LIKE)\s*'((?:[^'\\]|\\.)*)'", where or ""):
            val = val.replace("\\'", "'")
            def get(r, f=field):
                v = r.get(f)
                return v.get("value") if isinstance(v, dict) else v
            if op == "=":
                rows = [r for r in rows if str(get(r)) == val]
            elif op == ">=":
                rows = [r for r in rows if str(get(r)) >= val]
            elif op == "<=":
                rows = [r for r in rows if str(get(r)) <= val]
            else:
                rows = [r for r in rows if val.strip("%") in str(get(r))]
        return rows

    def query(self, q, **_):
        m = re.match(r"SELECT .+? FROM (\w+)(?: WHERE (.*))?$", q.strip())
        return {"QueryResponse": {m.group(1): self._match(m.group(1), m.group(2))}}

    def query_all(self, select, entity, page_size=1000):
        return self.query(select)["QueryResponse"][entity]

    def post(self, entity, body):
        self._id += 1
        obj = {**body, "Id": str(self._id)}
        if entity == "invoice":
            obj["Balance"] = sum(l["Amount"] for l in body["Line"])
        if entity in ("payment", "deposit"):
            obj["TotalAmt"] = body.get("TotalAmt", sum(l["Amount"] for l in body["Line"]))
        self.db[{"invoice": "Invoice", "payment": "Payment", "deposit": "Deposit"}[entity]].append(obj)
        self.posted.append((entity, obj))
        return obj


def write_csv(path: Path, rows: list[dict]):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8-sig") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)


def sub(id_, code, amount, dtype="Pet-Fee", bank="1", method="zelle", status="approved", typed=""):
    return {"id": id_, "kind": "deposit", "status": status, "txn_date": "2026-09-14", "property_code": "renton_18823",
            "property_name": "Renton 18823", "amount": amount, "deposit_type": dtype, "collection_method": method,
            "payer_name": "Guest", "reservation_code": code, "check_in_date": "2026-09-12",
            "bank_deposit_id": bank, "typed_in": typed, "qbo_payment_id": ""}


class GuestFees(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp())
        self.exports = self.tmp / "exports"
        P.CHANGE_LOG = self.tmp / "CHANGE_LOG.csv"
        P.POSTED = self.tmp / "posted.csv"
        write_csv(self.exports / "config" / "deposit_types.csv", [
            {"code": "Pet-Fee", "label_en": "Pet fee", "qbo_item": "Pet Fee (Owner)", "per_reservation": "TRUE"},
            {"code": "Parking-Fee", "label_en": "Parking fee", "qbo_item": "Parking Fee", "per_reservation": "TRUE"}])
        write_csv(self.exports / "config" / "properties.csv", [
            {"code": "renton_18823", "name": "Renton 18823", "qbo_class_name": "Listings:Renton 18823"}])
        self.qbo = FakeQBO()

    def run_build(self, subs, banks):
        write_csv(self.exports / "submissions.csv", subs)
        write_csv(self.exports / "bank_deposits.csv", banks)
        return B.build(self.exports, self.qbo)

    def test_treatments(self):
        rows, warns = self.run_build(
            [sub("1", "HMABC123", "150.00"), sub("2", "HMPAID99", "100.00", bank="2"),
             sub("3", "HMNEW777", "40.00", dtype="Parking-Fee", bank="3"), sub("4", "NOPE0000", "10.00", bank="4"),
             sub("5", "HMABC123", "25.00", method="cash", bank=""), sub("6", "HMABC123", "9.00", status="pending")],
            [{"id": b, "bank_date": "2026-09-15", "total": t} for b, t in
             (("1", "150.00"), ("2", "100.00"), ("3", "40.00"), ("4", "10.00"))])
        got = {r["SubmissionId"]: r["Treatment"] for r in rows}
        self.assertEqual(got, {"1": "PAY_EXISTING", "2": "HELD", "3": "NEW_INVOICE", "4": "ERROR"})
        self.assertEqual(rows[0]["InvoiceId"], "5001")
        self.assertTrue(any("cash" in w and "no bank deposit" in w for w in warns))
        self.assertFalse(any("submission 6" in w for w in warns), "pending rows are simply not built")

    def test_post_dry_run_then_confirm_then_rerun_is_idempotent(self):
        rows, _ = self.run_build([sub("1", "HMABC123", "150.00"), sub("3", "HMNEW777", "40.00", dtype="Parking-Fee")],
                                 [{"id": "1", "bank_date": "2026-09-15", "total": "190.00"}])
        res = Resolver(self.qbo)
        out = []
        msg = P.post_group("1", rows, self.qbo, res, confirm=False, out=out)
        self.assertIn("WOULD create Invoice HMNEW777-PARK", msg)
        self.assertIn("WOULD create Deposit $190.00", msg)
        self.assertEqual(self.qbo.posted, [], "dry run writes nothing")

        msg = P.post_group("1", rows, self.qbo, Resolver(self.qbo), confirm=True, out=[])
        kinds = [e for e, _ in self.qbo.posted]
        self.assertEqual(kinds, ["payment", "invoice", "payment", "deposit"])
        pays = [o for e, o in self.qbo.posted if e == "payment"]
        self.assertTrue(all(p["DepositToAccountRef"]["value"] == "4" for p in pays), "payments -> Undeposited Funds")
        dep = self.qbo.posted[-1][1]
        self.assertEqual(dep["DepositToAccountRef"]["value"], "35")
        self.assertEqual(sorted(l["LinkedTxn"][0]["TxnId"] for l in dep["Line"]), sorted(p["Id"] for p in pays))
        self.assertEqual(sum(l["Amount"] for l in dep["Line"]), 190.0)
        inv = self.qbo.posted[1][1]
        self.assertEqual(inv["DocNumber"], "HMNEW777-PARK")
        self.assertEqual(inv["Line"][0]["SalesItemLineDetail"]["ItemRef"]["value"], "i-park")
        self.assertEqual(inv["Line"][0]["SalesItemLineDetail"]["ClassRef"]["value"], "c1")
        self.assertTrue(P.CHANGE_LOG.exists())

        n = len(self.qbo.posted)
        msg = P.post_group("1", rows, self.qbo, Resolver(self.qbo), confirm=True, out=[])
        self.assertEqual(len(self.qbo.posted), n, "a re-run finds everything and creates nothing")
        self.assertIn("Deposit exists", msg)

    def test_group_with_a_held_fee_is_not_deposited(self):
        rows, _ = self.run_build([sub("1", "HMABC123", "150.00"), sub("2", "HMPAID99", "100.00")],
                                 [{"id": "1", "bank_date": "2026-09-15", "total": "250.00"}])
        msg = P.post_group("1", rows, self.qbo, Resolver(self.qbo), confirm=True, out=[])
        self.assertIn("SKIPPED", msg)
        self.assertEqual(self.qbo.posted, [])

    def test_cash_already_in_bank_another_way_blocks_the_deposit(self):
        self.qbo.db["Payment"].append({"Id": "p9", "TxnDate": "2026-09-15", "TotalAmt": 150,
                                       "DepositToAccountRef": {"value": "35"}, "CustomerRef": {"value": "1"}, "Line": []})
        rows, _ = self.run_build([sub("1", "HMABC123", "150.00")],
                                 [{"id": "1", "bank_date": "2026-09-15", "total": "150.00"}])
        msg = P.post_group("1", rows, self.qbo, Resolver(self.qbo), confirm=True, out=[])
        self.assertIn("Deposit NOT made", msg)
        self.assertNotIn("deposit", [e for e, _ in self.qbo.posted])


if __name__ == "__main__":
    unittest.main()
