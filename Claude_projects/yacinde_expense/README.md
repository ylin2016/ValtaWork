# Yacinde Expense Allocation

Splits Yacinde cleaning and supplies costs among **Fractional share**, **NuGrowth**,
**Yacinde Holdings** and **HOA**. It combines bookings from the previous
company and Guesty, labels each booking with its fractional-master week owner, matches
the cleaning records, and computes supplies.

```bash
source /Users/ylin/ValtaWork/.venv/bin/activate
python -m src.fetch_guesty     # pull Guesty confirmed + canceled (incl. owner / owner-guest)
python -m src.build            # -> output/yacinde_expense_allocation_<start>_<end>.xlsx
```

Supplies cost = guests × nights × $0.90. Timeshare (individual owner) stays use the unit's
max sleeps instead of the guest count. Settings: `config/allocation_rules.yml`.
HOA pays the first 4 cleans a month on whole-owner units (B6 C1 E1 E3 F5) and one clean per timeshare owner week.
See `CLAUDE.md` for the dedup and matching rules.
