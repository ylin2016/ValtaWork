"""Per-reservation payment waterfall — the Section 1 fee breakdown.

Faithful transcription of GuestyFinancials/payment_breakdown.py (which itself
mirrors the user-owned ``config/payment_structure.xlsx``). Refactored from a
flat script into ``compute_breakdown(summary_df, tax_rates_csv)`` so the whole
pipeline can call it in-memory on a freshly-pulled month.

Columns of the returned DataFrame (one row per reservation):
    confirmationCode, channel, row_group, listing, guest, checkIn, checkOut,
    status, InvoiceItem, guest_pay, cleaning_fee, damage_waiver, tax, channel_fee,
    guesty_fee, stripe_fee, host_payout, net_revenue, refund, note

InvoiceItem = Guesty hostPayout (Σ invoiceItems) exactly as shown; never
re-derived. If the user edits payment_structure.xlsx, update the rates/formulas
here to match — the sheet is the source of truth (see GuestyFinancials CLAUDE.md).
"""
import numpy as np
import pandas as pd

# Itemized tax lines — Guesty's money.totalTaxes UNDERCOUNTS (omits e.g.
# tax_destination), so sum these instead.
TAXCOLS = ["tax_occupancy", "tax_state", "tax_county", "tax_city", "tax_local",
           "tax_reservation_total", "tax_other", "tax_destination", "tax_residential"]

# Channel-fee rate per payment-structure row group (column E of the sheet).
# Trip.com went 11.1% -> 15% when the owner updated the sheet (row 11, 2026-08).
RATE = {"airbnb": .155, "airbnb_hawaii": .155, "homeaway": .05, "expedia_pcm": .18,
        "expedia_grp": .18, "booking": .15, "hopper": .14, "marriott": .185,
        "tripcom": .15, "whimstay": .05, "blueground": .07,
        "manual": 0.0, "bnbfinder": 0.0, "unknown": 0.0}

# Channels whose InvoiceItem is already NET of the channel fee — their sheet column C
# ends in "− channel fee" / "− Host channel Fee" (Airbnb, Airbnb_Hawaii, Hopper,
# Marriott, Whimstay, bluegroundNestpick). The rate is levied on the GROSS booking, so
# the fee has to be grossed up out of the net InvoiceItem:
#     cf = (II - tax + cf) x r   =>   cf = (II - tax) x r/(1-r)
# which is literally how the sheet writes rows 10/12/13, "(InvoiceItem-tax+ channel
# fee) x r". Verified against Guesty's own reported Host channel Fee: the ratio
# reported/(II-tax) is exactly r/(1-r) for every one of these groups — Airbnb .18343
# (= .155/.845), Hopper 1/.86, Whimstay 1/.95, Marriott 1/.815 — e.g. Keaau Airbnb
# HMJ9P9WMKS: (1302.00 - 203.50) x .18343 = 201.50 = Guesty's CHANNEL COMMISSION.
#
# The other channels (HomeAway, Expedia group, Booking.com, Trip.com) keep the plain
# "(InvoiceItem - tax) x rate": their InvoiceItem INCLUDES the fee and the host pays it
# back, which is also why they are the ones that subtract it again in net revenue.
# expedia_pcm is excluded here — it carries its own equivalent gross-up formula,
# "(accommodation + markup)/82% x 18%" (sheet row 5).
CF_OUTSIDE_II = ("airbnb", "airbnb_hawaii", "hopper", "marriott", "whimstay", "blueground")

# --- Stripe: the card's issuing country changes the rate ----------------------------
# Owner decision 2026-08-30. Stripe charges 2.9% on a US-issued card and **4.4%** on one
# issued outside the US (2.9% + its 1.5% cross-border surcharge). The per-transaction
# $0.30 is unchanged, and so is the Guesty 1%.
#
# Verified against QuickBooks' own `Credit Card Fees` bills across all 20 months. QBO folds
# the Guesty 1% into that line, so the true Stripe charge is (bill - guesty_fee); fitting
# `base x rate + n x $0.30` to 1,526 billed bookings gives 1,438 exact at 2.9%, 24 exact at
# 4.4%, and NOTHING in between -- the implied-rate histogram is cleanly bimodal. Worth
# $482.23 of understated Stripe fee over 20 months (~$289/yr), which was inflating net
# revenue and commission by the same.
STRIPE_RATE = 0.029
STRIPE_RATE_INTERNATIONAL = 0.044

# Guesty does NOT report the card's issuing country, so "international" needs two detectors:
#
# 1. **The non-US HomeAway storefronts.** 15 of the 16 CA/DE/UK bookings in 20 months are
#    international -- booking through vrbo.ca/.de/.co.uk all but implies a foreign card.
#    The lone exception is listed in STRIPE_DOMESTIC_CARD below.
STRIPE_INTERNATIONAL_SOURCES = ("homeaway ca", "homeaway de", "homeaway uk")

# 2. **An owner-verified per-reservation set**, for a foreign guest who booked through a US
#    channel (vrbo.com, the direct booking engine). Nothing in our pull distinguishes those
#    -- 9 of the 24 are VRBO or BE-API -- so each is confirmed against its QBO card-fee bill
#    and listed here, the same way PET_FEE_NOT_IN_CHANNEL_FEE handles its exceptions. ADD
#    NEW CODES HERE as they surface; the sweep that finds them compares (QBO Credit Card
#    Fees bill - guesty_fee) against InvoiceItem x 2.9% + n x $0.30.
STRIPE_INTERNATIONAL_CARD = {
    "HA-IhNJbIF",   # 2025-10 elektra_1004        VRBO
    "HA-aNkxaFP",   # 2025-11 burien_14407_top    VRBO
    "HA-dCtCqhs",   # 2026-02 burien_14407_middle VRBO
    "HA-KXi45m4",   # 2026-04 poulsbo_3956        VRBO
    "HA-6iOczj1",   # 2026-05 burien_14407_top    VRBO
    "GY-237WbUR9",  # 2026-05 redmond_14707       BE-API
    "HA-aLWsozx",   # 2026-06 bellevue_2243       VRBO
    "GY-kkunrZgm",  # 2026-08 osbr_7              BE-API
    "HA-pwCch41",   # 2026-08 elektra_1115        VRBO
}

# The reverse exception: a US card that happened to book through a non-US HomeAway site,
# so rule 1 would over-charge it. QBO billed osbr_7 HA-xWjkcHr at the plain 2.9%.
STRIPE_DOMESTIC_CARD = {"HA-xWjkcHr"}

# --- Bookings paid OUTSIDE the card rails -----------------------------------------
# Guest paid by bank transfer / check / cash, so NEITHER processing fee applies: no Stripe
# fee and no Guesty 1% (owner, 2026-08-30). Guesty's API does not surface the payment
# method on the reservation -- `money.payments[]` is read only for its SUCCEEDED count
# (`n_transactions`) -- so these are owner-verified per reservation, like the sets above.
# QuickBooks corroborates: it posts NO `Credit Card Fees` bill for such a booking while
# billing commission normally, and with both fees removed our commission ties QBO's Bill to
# the cent (GY-CQiSs4Nu: 389.50 - 135.00 cleaning = 254.50 x 20% = $50.90 = QBO).
# The durable fix is to thread the payment method through `fetch_month.build_summary_frame`
# on the next pull; until then, add codes here.
NO_PROCESSING_FEE_CODES = {
    "GY-CQiSs4Nu",   # 2026-07 hoodsport_26060, direct booking paid by bank transfer
    "GY-c2Q3YukP",   # 2026-07 yacinde_b6, direct-collected (no card rails)
}

# Owner-verified per-reservation exceptions: confirmation codes where the channel did NOT levy
# its commission on the pet fee, so the pet fee is dropped from that booking's channel-fee base.
# Scoped to the exact reservation (NOT a channel-wide rule) at the owner's instruction; add codes
# here as they are verified. Affects only the (InvoiceItem - tax) channel-fee formula.
PET_FEE_NOT_IN_CHANNEL_FEE = {"EXP-MyQbvyEn"}

# Owner-supplied service fee for a MANUAL/direct booking, keyed by confirmation code.
# `service_fee` normally arrives from Guesty's own fee categorisation, but a fee agreed
# off-platform on a direct booking never reaches the API — Guesty reports 0 and the model
# would leave it in the owner's net. These override that 0. Only the `manual` row group
# subtracts service_fee in net revenue (see the net_revenue formula), which is exactly the
# booking shape this applies to.
# It lives here rather than as a hand-edit to `payment_breakdown_<p>.csv` so a re-pull of
# the period reproduces it — a CSV-only correction is invisible to `git diff` and is
# silently lost the next time fetch_month overwrites the file (see CLAUDE.md).
MANUAL_SERVICE_FEES = {
    "GY-XuX8SMcC": 6.43,   # 2026-08 bellevue_1326, direct booking (owner, 2026-09-02)
}

# Bookings where the channel ALREADY took its commission before paying out, so net revenue
# must not subtract the channel fee a second time (owner, 2026-09-03). Normally the groups
# in `cf_in_net` do subtract it — their InvoiceItem includes the fee and the host pays it
# back — but on these reservations it was netted out up front.
#
# Owner-verified PER RESERVATION, deliberately not a channel-wide rule: the sibling
# expedia_grp bookings still subtract it. Nothing in the pull distinguishes the two shapes
# (both report `money.hostServiceFee` outside the invoice items), so there is nothing to
# detect on — add codes here as the owner confirms them.
#
#   EXP-2519622255-6E2WE  osbr_10 2026-08. QuickBooks agrees: our commission ties QBO's own
#                         (gross Bill less its fee Deductions) at $45.02 only with the fee
#                         left out; subtracting it gave $35.01 against QBO's $45.02.
CHANNEL_FEE_ALREADY_DEDUCTED = {
    "EXP-2519622255-6E2WE",   # osbr_10 2026-08
    "EXP-D1BeXYGv",           # osbr_11 2026-08 (owner, 2026-09-03)
}

# Airbnb Resolution Center amounts that are NOT the owner's money: a handling fee the
# cohost keeps (owner, 2026-09-03). The ARC line sits INSIDE InvoiceItem as a positive
# item, so by default it flows straight into the owner's net revenue and is commissioned.
# For these reservations it comes back out, and is shown with the fees instead.
#
# Per reservation, NOT a blanket rule: an ARC is normally a genuine guest charge or credit
# on the stay and does belong to the owner. Nothing in the pull distinguishes the two, so
# these are owner-verified one at a time.
#
#   HMH5WYQQ3Z  osbr_11 2026-08, $60.14 — cohost handling fee.
ARC_NOT_OWNER_INCOME = {"HMH5WYQQ3Z"}

# Bookings that carry NO cleaning fee, keyed by confirmation code (owner-verified).
# A stay EXTENDED from an earlier booking is cleaned once, on the original reservation —
# Guesty still stamps a cleaning fee on the extension, which would charge the guest's
# cleaning twice and pull it out of the owner's net a second time.
NO_CLEANING_FEE_CODES = {
    "GY-Eaz7YxLd",   # 2026-08 yacinde_e1, extension of an earlier booking (owner, 2026-09-03)
}

# Stripe fee taken from QuickBooks' own charge, for the handful of bookings whose fee the
# `base x rate + n x $0.30` formula cannot reproduce. QBO is the authority here: it is the
# actual amount Stripe billed, and the formula's base is simply not recoverable from what
# Guesty reports (see the per-code notes). Set to QBO's `Credit Card Fees` bill MINUS the
# Guesty 1%, which QBO folds into the same line.
#
# These three were hand-edits to the stored `payment_breakdown_<p>.csv` files until
# 2026-09-02, which meant they existed ONLY in a gitignored CSV: invisible to `git diff`,
# with no history, and silently wiped the next time `fetch_month` re-pulled the period.
# Moving them here is what makes them survive a re-pull.
STRIPE_FEE_OVERRIDES = {
    "HA-PLHAdi3":  45.99,   # 2025-04 kirkland_8017; QBO well above 2.9% x gross, base unknown
    "HA-sebniGp": 176.75,   # 2025-06 kirkland_8017; QBO ~$5.00 above 2.9% x gross
    "GY-MuLckM3e": 190.14,  # 2025-06 seattle_1117; QBO's base ~$6,556 (neither II nor gross)
}

# Every per-reservation manual adjustment in this module, for the summary `fetch_month`
# prints after each pull. Keeping the registry next to the tables is what makes "did my
# correction survive the re-pull?" answerable by reading the run's own output.
MANUAL_ADJUSTMENT_TABLES = {
    "no processing fee (paid off card rails)": NO_PROCESSING_FEE_CODES,
    "pet fee outside channel-fee base":        PET_FEE_NOT_IN_CHANNEL_FEE,
    "international Stripe card":               STRIPE_INTERNATIONAL_CARD,
    "domestic Stripe card (override)":         STRIPE_DOMESTIC_CARD,
    "manual service fee":                      MANUAL_SERVICE_FEES,
    "Stripe fee from QBO":                     STRIPE_FEE_OVERRIDES,
}


def applied_adjustments(codes):
    """Which manual adjustments touch `codes` — [(table name, code, value or None)].

    `fetch_month` calls this after every pull and prints the result, so a correction that
    silently stopped matching (a cancelled booking, a re-keyed confirmation code) shows up
    as a missing line in the run output instead of as a number quietly reverting."""
    have = {str(c) for c in codes}
    out = []
    for name, table in MANUAL_ADJUSTMENT_TABLES.items():
        for code in sorted(table):
            if str(code) in have:
                out.append((name, code, table[code] if isinstance(table, dict) else None))
    return out


def _assign_row_group(src: pd.Series, tot_tax: pd.Series, has_pcm: pd.Series,
                      is_vcard: pd.Series, index) -> pd.Series:
    """Map each reservation's source string to a payment-structure row group."""
    is_ab = src.str.contains("airbnb")
    is_haw = is_ab & (tot_tax > 0.01)              # Airbnb-Hawaii: tax in host ledger (Keaau 15-1542)
    grp = pd.Series("unknown", index=index)
    grp[is_ab & ~is_haw] = "airbnb"
    grp[is_haw] = "airbnb_hawaii"
    # PREFIX-matched, not an exact list: Guesty numbers its storefront variants the way it
    # does Airbnb ("airbnb2", matched by `contains` above), and an exact list silently drops
    # the new one into `unknown` — where the passthrough charges NO channel/Guesty/Stripe
    # fee at all and quietly overstates the owner's net. `homeaway2` appeared in 2026-08 and
    # cost $314.77 across 5 bookings before it was caught; QuickBooks bills every one of them
    # as VRBO (its `VRBO channel fee` ties our 5% to the cent on all 5).
    grp[src.str.startswith(("vrbo", "homeaway")) | src.eq("expediaintegrated")] = "homeaway"
    # Expedia family splits by PAYMENT METHOD, mapping to the two payment-structure rows:
    #   virtual card -> "expedia_pcm" = the Expedia_virtual card row (net does NOT subtract the
    #     channel fee; guest pay = II + channel fee; channel fee = (accom+markup)/82%*18%)
    #   everything else -> "expedia_grp" = the Expedia row (net subtracts the channel fee;
    #     guest pay = II; channel fee = (II-tax)*18%).
    # Split on the virtual-card flag, NOT the PCM invoice line — the PCM line is present on only
    # some virtual-card bookings, so it under-detects (Hotels.com etc. can also be virtual card).
    exp_family = (src.eq("expedia")
                  | src.isin(["expedia affiliate network", "hotels.com", "travelocity", "orbitz", "american express travel"]))
    grp[exp_family & ~is_vcard] = "expedia_grp"
    grp[exp_family & is_vcard] = "expedia_pcm"
    grp[src.eq("booking.com")] = "booking"
    grp[src.isin(["capital one", "hopper", "hopper web/app"])] = "hopper"
    grp[src.isin(["be-api", "booking engine", "direct", "direct bookings", "manual", "owner", "owner-guest", "website"])] = "manual"
    grp[src.eq("homesvillasbymarriott")] = "marriott"
    grp[src.isin(["tripcom", "trip.com"])] = "tripcom"
    grp[src.eq("whimstay")] = "whimstay"
    grp[src.eq("bluegroundnestpick")] = "blueground"
    grp[src.eq("bnbfinder")] = "bnbfinder"
    return grp, is_ab


def compute_breakdown(S: pd.DataFrame, tax_rates_csv) -> pd.DataFrame:
    """Compute the per-reservation payment waterfall from a batch `summary` frame.

    `S` must carry the columns produced by reservations_batch.summary_row plus the
    folded fee-category columns (host_channel_fee, service_fee, tax_*, ...).
    `tax_rates_csv` is config/_listing_tax_rates.csv (per-listing Airbnb tax rate).
    """
    S = S.copy().reset_index(drop=True)

    num = ["host_payout", "host_service_fee", "host_channel_fee", "guest_service_fee", "cleaning",
           "additional_fees", "accommodation", "markup", "pet_fee", "damage_protection",
           "total_taxes", "payment_fees", "total_refunded", "total_paid", "balance_due",
           "pm_commission", "service_fee", "management_fee",
           "airbnb_resolution_center"] + TAXCOLS
    for c in num:
        if c not in S:
            S[c] = 0.0
    S[num] = S[num].fillna(0.0)

    # ---------------- building blocks ----------------
    II = S["host_payout"].astype(float)             # InvoiceItem = Σ invoiceItems, as Guesty shows it
    # Refund handling: for a booking with a refund, base net revenue on the cash the host actually
    # received (total_paid) instead of InvoiceItem, and refund the percentage fees (Guesty 1% and
    # the channel fee) on that retained amount. total_paid is ALREADY net of refunds (gross charged
    # = total_paid + total_refunded), so use it directly — never subtract total_refunded again.
    # This handles the edge cases: a refund exceeding the payout, or a guest refunded then re-paid
    # in full (balanceDue 0 -> total_paid == II -> no reduction). Non-refunded bookings keep II.
    is_refunded = (S["total_refunded"].astype(float) > 0.005).values
    net_base = np.where(is_refunded, S["total_paid"].astype(float).values, II.values)
    clean = S["cleaning"].astype(float)
    # Guesty's guest-side "Cleaning fee" summary field and its own line items can DISAGREE:
    # Trip.com CT-1658113358878313-4DEOF reports cleaning 0.00 in the summary while carrying a
    # $45.00 line item titled "Cleaning". Left alone the fee stays inside net revenue and the
    # owner is commissioned on it. Fill from the categorised line item only where the summary
    # says nothing — never override a non-zero summary value, so this can only add a cleaning
    # fee that was missing, not restate one that was already reported.
    if "cleaning_fee" in S.columns:
        _li_clean = pd.to_numeric(S["cleaning_fee"], errors="coerce").fillna(0.0)
        clean = clean.where(clean.abs() > 0.005, _li_clean)
    svcf = S["service_fee"].astype(float)           # manual/direct add-on "service fee" (kept as revenue)
    # Owner-supplied service fee on a direct booking Guesty never saw (MANUAL_SERVICE_FEES).
    # `.replace(0, ...)` would be wrong here — this is an override, not a fill.
    _svc_override = S["confirmationCode"].astype(str).map(MANUAL_SERVICE_FEES)
    svcf = svcf.where(_svc_override.isna(), _svc_override).astype(float)
    # Damage-waiver / damage-protection fee: a PASS-THROUGH the guest pays and Valta remits,
    # NOT owner revenue (owner, 2026-08-29). It sits inside InvoiceItem, so it has to come
    # back out of net revenue -- otherwise the owner is commissioned on money that was never
    # theirs. QuickBooks says the same thing structurally: it books the line to
    # `Guest Charges:PM Income:Damage Waiver`, alongside Cleaning Fee (PM), while the owner's
    # own money goes to `Guest Charges:Owner Income:*` -- and QBO's commission Bill is exactly
    # (Owner Income) x rate. Bellevue 514 HA-GuqUzQq 2026-07: InvoiceItem 2571.90 - cleaning
    # 125.00 - tax 321.90 - damage waiver 30.00 = 2095.00, and QBO billed 419.00 = 2095.00 x 20%.
    # Its no-waiver neighbour HA-EdegGLc ties with no adjustment (328.00 x 20% = 65.60), so the
    # waiver is the whole of the difference.
    # NOTE `categorize_item` also routes a "deposit"-titled line here; a refundable deposit is
    # likewise not revenue, so subtracting it is the same rule (cf. income_rules.NON_RENTAL_PRED).
    dwf = S["damage_protection"].astype(float)
    tot_tax = S[TAXCOLS].sum(axis=1).astype(float)  # ledger tax = Σ itemized tax lines
    src = S["source"].astype(str).str.lower()
    has_pcm = S["host_channel_fee"].abs() > 0.01    # channel fee already a line inside InvoiceItem?
    # Guest paid by virtual card — set by fetch_month from the reservation's specialRequests
    # ("Payment method: EXPEDIA VIRTUAL CARD"). Drives the Expedia virtual-card vs regular split.
    is_vcard = (S["is_virtual_card"].astype(bool) if "is_virtual_card" in S.columns
                else pd.Series(False, index=S.index))

    grp, is_ab = _assign_row_group(src, tot_tax, has_pcm, is_vcard, S.index)

    # Expedia reports its cleaning fee as an AFE invoice line, NOT as a CF/"cleaning" line,
    # so S["cleaning"] is 0. The label (and thus the category) differs by integration:
    #   - group Expedia (expedia_grp: Expedia, Hotels.com, Orbitz, …) labels it
    #     "Additional Fees & Room Fees"  -> categorized as additional_fees
    #   - integrated Expedia (expedia_pcm) labels it "Service"  -> categorized as service_fee
    # They are mutually exclusive per booking, so sum both and fold into `clean` for the
    # Expedia groups: cleaning is pulled out of net revenue (non-commissioned, like every
    # other channel) and shown in the cleaning_fee column. Neither additional_fees nor
    # service_fee is referenced elsewhere in the Expedia waterfall (service_fee is only
    # subtracted for the `manual` group), so this cannot double-count.
    # Trip.com does the same thing (owner, 2026-09-03): it reports the cleaning fee in the
    # fee bucket rather than as `cleaning`, so a Trip.com booking showed $0.00 cleaning and
    # carried the fee in net revenue instead — CT-1658113358878313-4DEOF, $45.00.
    # `management_fee` is in the fold because that is where Trip.com's cleaning fee actually
    # lands: categorize_item routes a title containing "management"/"admin" there, and until
    # 2026-09-03 NOTHING read that column — the money sat inside InvoiceItem but appeared in
    # no displayed column and was never pulled out of net revenue.
    # An extension of an earlier stay is cleaned once, on the original booking.
    clean = clean.where(~S["confirmationCode"].astype(str).isin(NO_CLEANING_FEE_CODES), 0.0)
    is_fee_is_cleaning = grp.isin(["expedia_grp", "expedia_pcm", "tripcom"])
    exp_clean = (S["additional_fees"].astype(float) + S["service_fee"].astype(float)
                 + S["management_fee"].astype(float))
    clean = clean + exp_clean.where(is_fee_is_cleaning, 0.0)

    # tax that lives INSIDE InvoiceItem (drives channel-fee base & net revenue):
    # non-airbnb + Hawaii-airbnb -> ledger tax; mainland airbnb -> 0 (Airbnb remits tax).
    tax_in_II = np.where(grp.eq("airbnb"), 0.0, tot_tax)

    # Airbnb tax for DISPLAY (property rate; base excludes ARC).
    tmap = pd.read_csv(tax_rates_csv)
    rate_by_nick = dict(zip(tmap["nickname"], tmap["tax_rate_pct"] / 100.0))
    acct_nicks = set(tmap.loc[tmap["useAccountTaxes"] == True, "nickname"])
    arc = np.where(is_ab, S["airbnb_resolution_center"].values, 0.0)   # Airbnb resolution line (signed)
    rate_frac = S["listing"].map(rate_by_nick).mask(S["listing"].isin(acct_nicks))
    ab_tax_known = is_ab & rate_frac.notna()

    # Channel fee = base x rate. Base is normally (InvoiceItem - tax), but a VIRTUAL-CARD Expedia
    # booking (now grp == "expedia_pcm") uses "(accommodation + markup) / 82% x 18%": the host's
    # (accommodation + markup) is the 82% left after Expedia's 18% cut, so gross it up and take
    # the rate — cf_base = (accommodation + markup) / (1 - rate).
    # REFUNDS: VRBO/HomeAway does NOT return its channel fee (owner, 2026-08-29) — the host
    # still pays 5% of the FULL booking, so homeaway keeps the whole (InvoiceItem - tax) base
    # even when refunded. Every other channel drops to (total_paid - tax) on the assumption
    # that the platform returns its commission on the refunded portion; that assumption is
    # currently UNTESTED, because across 20 months every refunded booking carrying a channel
    # fee is homeaway (48 of them). Confirm per channel before relying on it.
    # Verified against VRT: bellevue_2243 HA-fT3tQ4a 2026-07 nets $2,972.5x either way only if
    # the fee stays on the full base — we showed $3,002.53 before this.
    rate = grp.map(RATE).astype(float).values
    is_vcard_exp = grp.eq("expedia_pcm").values
    cf_refunded_base = np.where(grp.eq("homeaway").values,
                                II.values - tax_in_II,
                                np.maximum(net_base - tax_in_II, 0.0))
    cf_std_base = np.where(is_refunded, cf_refunded_base, II.values - tax_in_II)
    # Per-reservation pet-fee exception: drop the pet fee from the channel-fee base for the
    # owner-verified codes above (the channel did not commission the pet fee on those bookings).
    pet_excl = S["confirmationCode"].astype(str).isin(PET_FEE_NOT_IN_CHANNEL_FEE).values
    cf_std_base = cf_std_base - np.where(pet_excl, S["pet_fee"].astype(float).values, 0.0)
    cf_base = np.where(is_vcard_exp,
                       (S["accommodation"].astype(float) + S["markup"].astype(float)).values / (1.0 - rate),
                       cf_std_base)
    # Gross-up for the channels whose InvoiceItem is net of the fee (see CF_OUTSIDE_II).
    # expedia_pcm keeps the plain rate — its base was already grossed up just above.
    cf_rate = np.where(np.isin(grp.values, CF_OUTSIDE_II), rate / (1.0 - rate), rate)
    channel_fee = np.round(cf_base * cf_rate, 2)
    # A MANUAL/direct booking's `service_fee` IS its channel fee (owner, 2026-09-03) — a
    # direct booking has no channel rate, so the fee the guest was charged is the whole of
    # it. Net revenue already subtracted it (the `grp.eq("manual")` term below), but until
    # now it appeared in NO displayed column, so a manual booking carrying one did not foot:
    # GY-XuX8SMcC showed 425.57 - 125.00 cleaning - 12.94 - 4.26 = 283.37 against a stated
    # net of 276.94, the $6.43 simply missing. Showing it as the channel fee closes that.
    channel_fee = np.where(grp.eq("manual").values, channel_fee + svcf.values, channel_fee)

    # Airbnb display tax = ((InvoiceItem - ARC) + channel fee) * property_rate.
    # MAINLAND Airbnb only: Airbnb collects and remits the tax itself, so it never appears
    # in the host ledger and has to be re-derived from the property rate for display.
    # Airbnb-HAWAII is the opposite — the tax IS in the host ledger (and inside
    # InvoiceItem, per sheet row 3), so show that real tax. Re-deriving it there both
    # overstated the tax column and made it disagree with the tax net revenue actually
    # subtracts (tax_in_II), e.g. Keaau HMJ9P9WMKS showed $272.37 against a real $203.50.
    ab_tax = (((II.values - arc) + channel_fee) * rate_frac.values)
    airbnb_tax = np.where(ab_tax_known.values, np.round(ab_tax, 2), 0.0)
    tax_disp = np.where(grp.eq("airbnb").values, airbnb_tax, np.round(tot_tax.values, 2))

    # Guesty fee (1%) + Stripe fee -> host/Guesty-collected channels only.
    # Stripe = InvoiceItem x rate + (successful transactions) x $0.30, where the rate is
    # 2.9% domestic / 4.4% international (per payment_structure.xlsx H4/H5/H6/H9/H14:
    # "InvoiceItem*2.9%+transactions*0.3 (international card: 4.4%)"). `transactions`
    # is the count of SUCCEEDED Stripe charges on the reservation (deposit +
    # balance each incur their own $0.30). Rows lacking payment data (deactivated
    # UI merge) fall back to 1 transaction.
    # NOTE: `.values` on a boolean Series can be a READ-ONLY view, so combine into a new
    # array (`a & b`) rather than mutating in place (`a &= b`, which raises).
    has_processing = (
        grp.isin(["homeaway", "expedia_pcm", "expedia_grp", "manual", "bnbfinder"]).values
        # A booking paid outside the card rails carries neither fee (NO_PROCESSING_FEE_CODES).
        & ~S["confirmationCode"].astype(str).isin(NO_PROCESSING_FEE_CODES).values
    )
    if "n_transactions" in S.columns:
        # Per-row NaN (deactivated UI-merge rows carry no payment data) falls back to 1.
        n_tx = pd.to_numeric(S["n_transactions"], errors="coerce").fillna(1.0).values
    else:
        # Whole column absent = caller didn't thread payment counts -> every Stripe row
        # would silently bill exactly 1 transaction. Warn loudly rather than mis-state fees.
        import warnings
        warnings.warn("compute_breakdown: 'n_transactions' column missing; Stripe fee "
                      "defaults to 1 transaction/booking (per-transaction $0.30 undercounted "
                      "for multi-payment bookings).", stacklevel=2)
        n_tx = np.ones(len(S))
    # Per-row Stripe rate: 4.4% for a card issued outside the US, else 2.9% (see
    # STRIPE_RATE_INTERNATIONAL). Source-driven for the non-US HomeAway storefronts, plus
    # the owner-verified per-reservation set for foreign cards on US channels; a code in
    # STRIPE_DOMESTIC_CARD forces the plain rate back on.
    codes = S["confirmationCode"].astype(str)
    is_intl = (src.isin(STRIPE_INTERNATIONAL_SOURCES) | codes.isin(STRIPE_INTERNATIONAL_CARD))
    is_intl &= ~codes.isin(STRIPE_DOMESTIC_CARD)
    stripe_rate = np.where(is_intl.values, STRIPE_RATE_INTERNATIONAL, STRIPE_RATE)
    # **Stripe does NOT return its fee on a refund** (owner, 2026-08-30) -- the same principle as
    # VRBO's channel fee. The card was charged the full amount, so the fee is levied on the GROSS
    # CHARGED (`total_paid + total_refunded`), not on an InvoiceItem QuickBooks later re-cut down.
    # Only bites when QBO re-cut the invoice: for a refund it left alone, InvoiceItem IS the gross
    # (HA-G5Zh9AS: II 680.63 = paid 611.63 + refunded 69.00), so the `max` is a no-op there.
    # Scoped to refunded rows because a handful of clean bookings carry total_paid slightly ABOVE
    # InvoiceItem (an overpayment, or an invoice that shrank) and QBO bills those on InvoiceItem.
    # Verified: across 1,526 QBO-billed bookings this rule contradicts ZERO of them and fixes 28
    # ($253.25) -- e.g. HA-ADSOY1B 2026-07, gross $1,349.44 x 2.9% + 2x$0.30 = $39.73 = QBO's bill,
    # against an InvoiceItem of only $945.47.
    gross_charged = (S["total_paid"].astype(float) + S["total_refunded"].astype(float)).values
    stripe_base = np.where(is_refunded & (gross_charged > II.values + 0.005),
                           gross_charged, II.values)
    stripe_raw = stripe_base * stripe_rate + n_tx * 0.30
    # Guesty fee on net_base (= total_paid for refunded bookings) so its 1% is refunded on the
    # refunded portion. Stripe fee stays on the full InvoiceItem (not refunded per the sheet).
    guesty_fee = np.where(has_processing, np.round(net_base * 0.01, 2), 0.0)
    stripe_fee = np.where(has_processing, np.round(stripe_raw, 2), 0.0)
    # QBO's own charge wins where the formula cannot reproduce it (STRIPE_FEE_OVERRIDES).
    # Applied AFTER the has_processing gate so a code in NO_PROCESSING_FEE_CODES still
    # ends up at $0 — "paid off the card rails" outranks "QBO billed this much".
    _stripe_ov = S["confirmationCode"].astype(str).map(STRIPE_FEE_OVERRIDES)
    stripe_fee = np.where(_stripe_ov.notna().values & has_processing,
                          _stripe_ov.fillna(0.0).values, stripe_fee)

    # Guest pay (per-row formula).
    gsf10 = np.round((II.values - tax_in_II) * 0.10, 2)     # HomeAway guest service fee = (II-tax) x 10%
    # Sheet column F. MAINLAND Airbnb (row 2) is the only "+ tax" row: its InvoiceItem
    # excludes the tax Airbnb collects from the guest, so the guest's total adds it back.
    # Airbnb-HAWAII (row 3) reads "InvoiceItem + channel fee" with NO tax term — the tax
    # is already inside InvoiceItem, so adding it again double-counted it (owner-reported
    # on the Keaau bookings): HMJ9P9WMKS showed $1,744.64 instead of $1,503.50.
    guest_pay = np.select(
        [grp.eq("airbnb").values,
         grp.eq("homeaway").values,
         grp.isin(["airbnb_hawaii", "expedia_pcm", "hopper", "marriott",
                   "whimstay", "blueground"]).values],
        [II.values + channel_fee + tax_disp,
         II.values + gsf10,
         II.values + channel_fee],
        default=II.values)                                  # expedia_grp, booking, manual, tripcom, unknown

    # Host payout (per-row formula).
    payout_minus_proc = grp.isin(["homeaway", "expedia_pcm", "expedia_grp", "manual"]).values  # NOT bnbfinder
    host_payout = np.select(
        [grp.eq("booking").values,
         payout_minus_proc],
        [II.values - channel_fee,                           # Booking: InvoiceItem - channel fee
         II.values - stripe_fee - guesty_fee],              # HomeAway/Expedia/Manual: - stripe - Guesty
        default=II.values)                                  # channel-collected & bnbFinder: = InvoiceItem

    # Net revenue (per-row formula).
    tax_sub = np.where(is_ab.values, tax_in_II, tot_tax.values)   # tax actually inside InvoiceItem
    # Rows that subtract the channel fee in net revenue. For expedia_grp the sheet's
    # R6 net formula writes "- hostServiceFee", which the user defines as the same
    # (InvoiceItem-tax)×18% channel fee computed above — so subtract channel_fee.
    cf_in_net = np.isin(grp.values, ["homeaway", "expedia_grp", "booking", "tripcom"])
    # ...except where the channel already took its cut before paying out, so subtracting it
    # here would double-count (CHANNEL_FEE_ALREADY_DEDUCTED). Scoped per reservation at the
    # owner's instruction, NOT applied channel-wide.
    cf_in_net &= ~S["confirmationCode"].astype(str).isin(CHANNEL_FEE_ALREADY_DEDUCTED).values
    # Leading term is net_base (= total_paid for refunded bookings, else InvoiceItem): the host
    # gives back the refund. cleaning/tax/channel/stripe stay on the full InvoiceItem; only the
    # base and the Guesty fee (above) drop to the retained amount.
    # An ARC the cohost keeps as a handling fee is not the owner's money — it sits inside
    # InvoiceItem as a positive line, so it has to come back out (ARC_NOT_OWNER_INCOME).
    arc_fee = np.where(S["confirmationCode"].astype(str).isin(ARC_NOT_OWNER_INCOME).values,
                       arc, 0.0)
    net_revenue = (net_base - clean.values - tax_sub - dwf.values
                   - np.where(cf_in_net, channel_fee, 0.0)
                   - np.where(grp.eq("manual").values, svcf.values, 0.0)
                   - arc_fee
                   - stripe_fee - guesty_fee)

    # Floor a REFUNDED booking's net at 0: when the retained cash (total_paid) is less than the
    # booking's cleaning + tax + fees, the InvoiceItem-basis net goes negative (e.g. HA-SDuJSuJ:
    # kept $120 but carries $125 cleaning + $45 tax). Owner decision: show $0, not a negative.
    net_revenue = np.where(is_refunded, np.maximum(net_revenue, 0.0), net_revenue)

    # Canceled bookings: the stay never happened, so there is NO channel fee / cleaning / tax —
    # owner revenue is the retained cash (total_paid, ALREADY net of refunds; never subtract
    # total_refunded again). But that cash was still charged through Stripe/Guesty, so their
    # processing fees still apply on has_processing channels. So a canceled booking's InvoiceItem
    # IS the retained cash and net = retained − Guesty − Stripe. Guesty often zeroes the invoice
    # items on cancellation (hostPayout 0) while total_paid holds the retained fee (e.g. a $200
    # Expedia cancellation fee), so keying the whole waterfall on total_paid is what surfaces it
    # in Section 1 — an InvoiceItem of 0 would otherwise be dropped below.
    is_canceled = S["status"].astype(str).str.lower().isin(["canceled", "cancelled"]).values
    retained = np.clip(S["total_paid"].astype(float).values, 0.0, None)
    canc_guesty = np.where(has_processing, np.round(retained * 0.01, 2), 0.0)
    canc_stripe = np.where(has_processing, np.round(retained * stripe_rate + n_tx * 0.30, 2), 0.0)
    # A STRIPE_FEE_OVERRIDES entry is QBO's actual charge, so it outranks this branch's
    # recomputation too. Without this the cancel branch below would silently overwrite the
    # override — no code needs it today, but the next one added for a cancelled booking
    # would have failed for a reason nothing in the output would explain.
    canc_stripe = np.where(_stripe_ov.notna().values & has_processing,
                           _stripe_ov.fillna(0.0).values, canc_stripe)
    canc_net = retained - canc_guesty - canc_stripe

    II_out      = np.where(is_canceled, retained,    II.values)
    guest_pay   = np.where(is_canceled, retained,    guest_pay)
    clean_out   = np.where(is_canceled, 0.0,         clean.values)
    tax_out     = np.where(is_canceled, 0.0,         tax_disp)
    dw_out      = np.where(is_canceled, 0.0,         dwf.values)
    channel_fee = np.where(is_canceled, 0.0,         channel_fee)
    guesty_fee  = np.where(is_canceled, canc_guesty, guesty_fee)
    stripe_fee  = np.where(is_canceled, canc_stripe, stripe_fee)
    host_payout = np.where(is_canceled, canc_net,    host_payout)
    net_revenue = np.where(is_canceled, canc_net,    net_revenue)

    refund = np.round(-arc, 2)                              # display only (Airbnb resolution); never re-subtracted

    # --- What the InvoiceItem is MADE OF (owner request 2026-09-03) ---------------
    # Section 1 showed only the folded InvoiceItem, so "how much of this booking was
    # actual room rent?" was unanswerable from the statement. These three split it:
    #
    #   accommodation  the room rent itself, NET of its own adjustments/discounts —
    #                  a length-of-stay discount belongs against the rent it discounts,
    #                  not hidden in a residual.
    #   markup         Guesty's channel markup, which rides on top of the rent.
    #   addons         everything the guest paid ON TOP of rent+cleaning+tax: pet fee,
    #                  parking, extra-guest, resort, service and other/additional fees.
    #                  Cleaning and tax stay in their own columns, and damage waiver in
    #                  its own (it is a pass-through, not owner revenue).
    #
    # `_cat` tolerates a category the pull did not produce this month, so adding a new
    # fee type upstream cannot KeyError the whole month's breakdown.
    def _cat(name):
        return (pd.to_numeric(S[name], errors="coerce").fillna(0.0).values
                if name in S.columns else np.zeros(len(S)))

    accommodation = _cat("accommodation_fare") + _cat("accommodation_adjustment")
    # Fall back to summary_row's own `accommodation` when the category pivot is absent
    # (the deactivated-listing UI merge carries no line items to categorise).
    accommodation = np.where(np.abs(accommodation) < 0.005,
                             pd.to_numeric(S.get("accommodation", 0.0),
                                           errors="coerce").fillna(0.0).values,
                             accommodation)
    markup = _cat("markup")
    # `service_fee` is NEVER an add-on (owner, 2026-09-03). Guesty files two different
    # things under that category and each already has its own column:
    #   Expedia / Trip.com — the "Service" line IS the cleaning fee, and `exp_clean` above
    #              already put it (with additional_fees) into the Cleaning Fee column.
    #   manual   — it IS the channel fee, now shown as such just above.
    # On EVERY OTHER channel it is a plain guest-paid extra and belongs in Add-ons. VRBO
    # bills it per night on the Elektra units (HA-m6sqiuF $35 x 3, HA-oh8HVHk $35 x 2); it
    # sits inside InvoiceItem, so leaving it out of all three component columns was what
    # stopped those rows footing. This is display-only — the money already was, and stays,
    # in the owner's net revenue; whether it SHOULD be is an owner question, not a
    # rendering one.
    is_exp = np.isin(grp.values, ["expedia_pcm", "expedia_grp", "tripcom"])
    svc_is_addon = ~(is_exp | grp.eq("manual").values)
    # payment_structure.xlsx B16: `add_on = pet + extra guest - promotion - discount`.
    # The discount/promotion categories arrive already NEGATIVE, so adding them subtracts.
    # They belong here rather than against accommodation because the sheet's column C
    # writes "accommodation + markup + cleaning + add_on", with accommodation being B15's
    # "Listing price on Guesty/Wheelhouse" — the list price, before any discount.
    addons = (_cat("pet_fee") + _cat("extra_person_fee") + _cat("parking_fee")
              + _cat("resort_fee") + _cat("other_fee")
              + _cat("accommodation_discount") + _cat("promotion")
              + _cat("discount_length_of_stay") + _cat("discount_weekly_monthly")
              + _cat("discount_channel")
              # additional_fees and management_fee are genuine add-ons everywhere EXCEPT the
              # Expedia/Trip.com groups, where the cleaning fold above already claimed them.
              + np.where(is_exp, 0.0, _cat("additional_fees") + _cat("management_fee"))
              + np.where(svc_is_addon, _cat("service_fee"), 0.0))

    # WHAT the add-on is, not just how much — "Add-ons $50.00" on a statement is not an
    # answerable line for an owner (owner, 2026-09-03). One compact string per booking,
    # listing only the components that are actually non-zero, which the renderers show
    # next to the number. Ordered as below so the common fees read first.
    _ADDON_LABELS = [
        ("pet_fee", "Pet fee"), ("extra_person_fee", "Extra guest"),
        ("parking_fee", "Parking"), ("resort_fee", "Resort fee"),
        ("service_fee", "Service fee"), ("additional_fees", "Additional fees"),
        ("management_fee", "Management fee"), ("other_fee", "Other fee"),
        ("accommodation_discount", "Fare discount"), ("promotion", "Promotion"),
        ("discount_length_of_stay", "Length-of-stay discount"),
        ("discount_weekly_monthly", "Weekly/monthly discount"),
        ("discount_channel", "Channel discount"),
    ]
    _claimed_by_cleaning = {"additional_fees", "management_fee"}
    detail = []
    for i in range(len(S)):
        parts = []
        for cat, label in _ADDON_LABELS:
            v = float(_cat(cat)[i])
            if abs(v) < 0.005:
                continue
            if is_exp[i] and cat in _claimed_by_cleaning:
                continue                       # folded into Cleaning Fee for Expedia/Trip.com
            if cat == "service_fee" and not svc_is_addon[i]:
                continue                       # it is the cleaning fee / channel fee there
            parts.append(f"{label} {v:,.2f}")
        detail.append("; ".join(parts))
    addons_detail = np.where(is_canceled, "", np.array(detail, dtype=object))
    # A cancelled booking's Section-1 row collapses to the retained cash, so its
    # components no longer describe anything that happened — blank them rather than
    # show a full stay's rent against a $6.10 row.
    accommodation = np.where(is_canceled, 0.0, accommodation)
    markup = np.where(is_canceled, 0.0, markup)
    addons = np.where(is_canceled, 0.0, addons)

    out = pd.DataFrame({
        "confirmationCode": S["confirmationCode"], "channel": S["source"],
        "row_group": grp.values, "listing": S["listing"], "guest": S["guest"],
        "checkIn": S["checkIn"].astype(str).str[:10], "checkOut": S["checkOut"].astype(str).str[:10],
        "status": S["status"],
        "InvoiceItem": np.round(II_out, 2),
        "guest_pay": np.round(guest_pay, 2),
        "accommodation": np.round(accommodation, 2),
        "markup": np.round(markup, 2),
        "addons": np.round(addons, 2),
        "addons_detail": addons_detail,
        # The channel's commission rate for this booking, carried so the DISPLAY layer
        # can charge it on an add-on that arrives later as its own QBO deposit (a pet fee
        # billed after checkout). booking_breakdown ships in the read-only deploy bundle
        # and cannot import this module, so the rate travels with the data instead.
        "channel_fee_rate": grp.map(RATE).fillna(0.0).values,
        "cleaning_fee": np.round(clean_out, 2),
        "damage_waiver": np.round(dw_out, 2),
        "tax": np.round(tax_out, 2),
        "channel_fee": np.round(channel_fee, 2),
        "guesty_fee": np.round(guesty_fee, 2),
        "stripe_fee": np.round(stripe_fee, 2),
        # ARC retained by the cohost — a fee, not owner revenue. Its own column so the
        # Channel/Guesty/Stripe split stays exactly what those three names mean.
        "arc_fee": np.round(np.where(is_canceled, 0.0, arc_fee), 2),
        "host_payout": np.round(host_payout, 2),
        "net_revenue": np.round(net_revenue, 2),
        "refund": refund,
    })

    # Notes (display hints).
    out["note"] = ""

    def add_note(mask, text):
        m = np.asarray(mask)
        out.loc[m, "note"] = (out["note"] + np.where(out["note"] == "", "", "; ") + text)[m]

    add_note(S["status"].astype(str).str.lower().isin(["canceled", "cancelled"]).values, "CANCELED")
    add_note(grp.eq("airbnb_hawaii").values, "Airbnb-Hawaii (tax in ledger)")
    add_note((is_ab & ~ab_tax_known & (II > 0)).values, "AIRBNB TAX: property rate not configured")
    add_note(grp.eq("unknown").values, "UNMAPPED source -> passthrough")
    if "data_source" in S.columns:
        add_note(S["data_source"].astype(str).str.contains("csv", case=False, na=False).values,
                 "from CSV (deactivated listing)")

    # Drop $0 InvoiceItem rows (comp/owner) AND canceled bookings that retained nothing
    # (paid − refunded ≤ 0) — those are not owner revenue and must not appear in Section 1.
    out["_keep"] = ~(is_canceled & (retained <= 0.005))
    n_before = len(out)
    out = out[(out["InvoiceItem"].abs() > 0.005) & out["_keep"]].drop(columns="_keep").copy()
    dropped = n_before - len(out)
    out = out.sort_values(["listing", "checkIn"]).reset_index(drop=True)
    return out, dropped
