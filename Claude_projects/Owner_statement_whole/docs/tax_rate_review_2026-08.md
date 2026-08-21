# Lodging tax rate review — August 2026

**Date:** 2026-08-20
**File reviewed / updated:** `config/_listing_tax_rates.csv`
**Pre-change copy retained:** `config/_listing_tax_rates_pre-2026-08-20.csv`
**Result:** 69 of 131 rows corrected (12 jurisdiction groups). 62 rows already correct.

---

## 1. Why the rates were re-derived from statute

An initial pass validated the configured rates against the tax actually charged on
20 months of bookings (host-collected Vrbo + direct rows in `payment_breakdown_*.csv`,
where the invoice carries the real itemized tax). Every rate reconciled to the cent.

**That check was circular and was discarded.** Guesty's per-listing tax settings were
configured by Valta years ago, so "collections match config" only proves the CSV agrees
with Guesty — it says nothing about whether either matches current law.

The rates below therefore come from primary sources only:

| Source | Used for |
|---|---|
| [WA DOR *Lodging Rates and Changes*, **Q3 2026**](https://dor.wa.gov/sites/default/files/2026-05/Q326%20_Lodging-flyer.xlsx) (eff. Jul 1 – Sep 30, 2026) | All WA rates — column *Total lodging tax rate* |
| [WA DOR *Lodging Rates and Changes*, **Q4 2026**](https://dor.wa.gov/sites/default/files/2026-08/Q426%20_Lodging-flyer.xlsx) (eff. Oct 1, 2026) | Forward check |
| [HI DOTAX Announcement **2025-03**](https://files.hawaii.gov/tax/news/announce/ann25-03.pdf) (Act 96) | Hawaii TAT increase |
| [Hawaii County TAT](https://www.hawaiicounty.gov/departments/finance/transient-accommodations-tax-tat) | County TAT surcharge |
| [Seattle — Short-Term Rentals, business regulations](https://www.seattle.gov/business-regulations/short-term-rentals) | Seattle per-night item |
| `config/Listing_contacts.csv` → `Address` | Mapping each listing to its taxing jurisdiction |

**Q3 → Q4 diff:** no rate changes in any county Valta operates in. The rates below hold
through **2026-12-31**. Re-check when the Q1 2027 flyer publishes.

The DOR *Total lodging tax rate* already bundles combined sales tax (state 6.5% + local +
RTA), special hotel/motel tax, and convention center tax — it is the full rate to charge a
guest. Stays of **30+ consecutive days are exempt** from lodging tax.

---

## 2. Two listing groups were filed under the wrong jurisdiction

Mapping addresses rather than nicknames surfaced two misfilings that account for the two
largest corrections:

- **Cottages 1–12 + "Cottages All OSBR"** — `1757 WA-105, Grayland, WA 98547`.
  That is **unincorporated Grays Harbor County** (11.9%), not the 10.1% they carried.
- **Yacinde B1–F5** — `1 Yacinde Dr, Manson, WA`.
  That is **unincorporated Chelan County** (10.5%), not the 12.3% they carried.

Also confirmed: the three **"Microsoft" units are Bellevue 98007 addresses**, not Redmond,
so their 14.5% was already right.

---

## 3. Changes applied

| DOR / DOTAX row | Listings | Old | **New** | Δ pp |
|---|---|---|---|---|
| Seattle – Lodging, **"Short-term rental"** tier | 29 (Seattle ×22, Beachwood ×7) | 15.85 | **15.7** | −0.15 |
| Grays Harbor County Unincorp. | 13 (Cottage 1–12, Cottages All OSBR) | 10.1 | **11.9** | **+1.80** |
| Chelan County Unincorp. — *new rate 7/1/2026* | 10 (Yacinde B1–F5) | 12.3 | **10.5** | **−1.80** |
| Poulsbo (Kitsap) | 4 (Poulsbo 563 / 3866 / 3956 / 13976) | 11.2 | **11.3** | +0.10 |
| Mason County Unincorp. | 5 (Shelton 250 / 310 / 310 Upper †, Hoodsport 26060, Lilliwaup 28610) | 10.5 | **10.6** | +0.10 |
| Pierce County Unincorp. Non-RTA, ≤25 units — *new rate 7/1/2026* | 3 (Longbranch 6821 ×3) † | 10.0 | **10.2** | +0.20 |
| King Co. cities – Lodging RTA, ≤59 units | 1 (Woodinville 19319) | 11.2 | **12.5** | +1.30 |
| Bainbridge Island (Kitsap) | 1 (Bainbridge 11143) | 10.5 | **11.2** | +0.70 |
| Bellevue – Lodging RTA, ≤39 units | 1 (Bellevue 1420) | 13.8 | **14.5** | +0.70 |
| Snohomish – Lynnwood, ≤49 units | 1 (Lynnwood 17506) | 12.7 | **12.5** | −0.20 |
| HI: TAT 11% + Hawaii Co. 3% + GET pass-on 4.7120% | 1 (Keaau 15-1542) | 18.5 | **18.712** | +0.212 |

† = assumption applied, see §6.

**Seattle note.** The DOR flyer carries a dedicated **"Short-term rental"** property tier for
Seattle at **15.7%**. The nine Elektra units already at 15.70 were the correct ones; the other
29 Seattle-jurisdiction listings at 15.85 were 0.15 pp high.

**Hawaii note.** Act 96 raised the state TAT from 10.25% to **11.00%** effective **2026-01-01**.
Big Island total = 11.00 (state TAT) + 3.00 (Hawaii County TAT) + 4.7120 (max visible GET
pass-on) = **18.712%**.

### Unchanged — verified correct (62 rows)

Elektra ×9 @ 15.7 · Bellevue + the three Microsoft units @ 14.5 · all King County suburbs
(Redmond, Kirkland, Sammamish, Issaquah, Bothell, Burien, SeaTac, Shoreline, Mercer Island)
@ 12.5 · Tacoma 7811 @ 12.5 · `Seattle 9750 Monthly` @ 0 (30+ day stays are exempt) ·
9 listings with `useAccountTaxes=True` (see §5).

---

## 4. Estimated exposure — 2026 check-ins to date

Base ≈ rent + cleaning on 2026 bookings already in `inputs/*/payment_breakdown_*.csv`.
Positive = tax was being **under**-collected.

| Group | Taxable base | Δ pp | Effect |
|---|---:|---:|---:|
| Cottages (Grayland) | $178,912 | +1.80 | **~$3,220 under** |
| Bainbridge | $73,496 | +0.70 | ~$514 under |
| Keaau (HI) | $32,068 | +0.212 | ~$68 under |
| Yacinde (Manson) | $34,997 | −1.80 | ~$630 over |
| Seattle / Beachwood | $495,201 | −0.15 | ~$743 over |

---

## 5. Open items — not fixed by this change

1. **Guesty is the system that actually collects the tax.** `_listing_tax_rates.csv` only
   drives the *imputed Airbnb display tax* in `src/breakdown/payment_model.py`
   (`tax_sub` is 0 for mainland Airbnb rows, so this file does **not** affect net revenue or
   commission). The same 12 corrections must be made to the per-listing tax settings in
   Guesty, or the under/over-collection in §4 continues.

2. **The `$1.00/night` on Seattle bookings is not a statutory tax.** Every host-collected
   Seattle/Elektra booking carries `rate × base + exactly $1.00/night`. The DOR flyer lists
   no TPA fee for Seattle, and Seattle repealed its own STR tax in June 2018 (Ordinance
   125442). The $4.00/night that does exist is a **platform licensing fee** paid by
   Airbnb/Vrbo to the city — not a guest tax. This $1/night is configured on Valta's side
   and is being remitted as tax. Needs a decision.

3. **`useAccountTaxes=True` listings (9) display $0 Airbnb tax.** None currently has
   bookings, so there is no live exposure. Correct rates when they go live:
   `Sammamish 5124-2` 12.5 · `Auburn 29123` 12.5 · `Redmond 10761` 12.5 ·
   `Bellevue 16237` 14.5 · `Elektra 1204` 15.7 · `Seattle 3617 Origin` 15.7 ·
   `Beachwood 2 Airbnb` 15.7 · `Hoquiam 21` 11.9 · `Ashford 137` 10.2 (see §6).

4. **New listings onboard with a 9.30% default.** A 9.30% rate — not a WA lodging rate —
   appears on the first Vrbo bookings of several listings, then clears once the listing's
   Guesty tax profile is set. Seen on: Bellevue 1420, Redmond 14707 (2025, resolved);
   Elektra 1203 (last 2026-05-22), Elektra 1212 (2026-06-14), Seattle 7434 Lower
   (2026-05-29, fixed by 07-04), **Bellevue 701 (2026-04-30)**, **Seattle 7434 whole
   (2026-06-26)** and **Seattle 7434 Upper (2026-07-01)** — the last three look unresolved.
   Worth adding a tax-setup step to listing onboarding.

5. **Model gap: this file cannot express a flat per-night charge.** `payment_model.py`
   applies `rate_frac` as a pure percentage. If item 2 resolves to "keep charging it", the
   CSV needs a `flat_per_night` column and `payment_model` a matching term.

---

## 6. Assumptions applied (verify when convenient)

Both were wrong at their previous values under *either* reading, so the more likely value
was applied rather than leaving a known-bad rate in place.

- **Shelton 250 / 310 / 310 Upper → 10.6%** (Mason County Unincorp.). Addresses are
  `250 SE Dogwood Acres Rd` and `310 SE Sells Dr`, Shelton 98584 — SE-prefixed county roads
  that read as outside city limits. If they are inside Shelton city limits the rate is
  **10.9%**.
- **Longbranch 6821 ×3 → 10.2%** (Pierce County Unincorp. **Non-RTA**, ≤25 units).
  Longbranch is on the Key Peninsula, which is outside both the Sound Transit RTA and the
  Pierce Transit PTBA. If it is inside the RTA the rate is **11.6%**.
- **`Ashford 137` → 10.2%** in §5 rests on the same Pierce non-RTA reading.

DOR's address rate lookup (`webgis.dor.wa.gov`) was returning errors during this review;
these three resolve by running the addresses through it, or via the DOR tax rate lookup app.

---

## 7. Reproducing / next review

- Flyers are archived at
  [DOR → lodging sales rate history](https://dor.wa.gov/taxes-rates/sales-use-tax-rates/local-sales-use-tax/lodging-sales-rate-history).
  Download the current quarter's XLSX, read the **Total lodging tax rate** column, and match
  on county + location + the smallest *Property information* tier (STRs are always the
  low-unit tier; Seattle has an explicit "Short-term rental" row).
- Re-run when the **Q1 2027** flyer publishes (~Nov 2026), and any quarter DOR flags a change
  in Grays Harbor, Chelan, Pierce, Mason, Kitsap, Snohomish, or King.
- To restore: `cp config/_listing_tax_rates_pre-2026-08-20.csv config/_listing_tax_rates.csv`
