"""Shapes for the JournalEntry write payloads.

``je_update`` was copy-pasted into three scripts; getting it wrong (dropping
SyncToken, dropping DocNumber) either fails the call or silently renumbers the
entry, so it lives in one place.
"""
from __future__ import annotations


def je_update(je: dict) -> dict:
    """A full-update payload for an existing JournalEntry, lines already mutated.

    QBO full-updates BLANK any field the payload omits, so DocNumber has to be
    carried over explicitly -- and SyncToken must be the one just read back, or
    the write is rejected as stale.
    """
    payload = {
        "Id": je["Id"],
        "SyncToken": je["SyncToken"],
        "TxnDate": je["TxnDate"],
        "Adjustment": je.get("Adjustment", False),
        "Line": je["Line"],
    }
    if je.get("DocNumber"):
        payload["DocNumber"] = je["DocNumber"]
    return payload
