"""Per-reservation payment waterfall — the Section 1 fee breakdown.

Faithful transcription of GuestyFinancials/payment_breakdown.py (which itself
mirrors the user-owned ``config/payment_structure.xlsx``). Refactored from a
flat script into ``compute_breakdown(summary_df, tax_rates_csv)`` so the whole
pipeline can call it in-memory on a freshly-pulled month.

Columns of the returned DataFrame (one row per reservation):
    confirmationCode, channel, row_group, listing, guest, checkIn, checkOut,
    status, InvoiceItem, guest_pay, cleaning_fee, tax, channel_fee, guesty_fee,
    stripe_fee, host_payout, net_revenue, refund, note

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

# Channel-fee rate per payment-structure row group.
RATE = {"airbnb": .155, "airbnb_hawaii": .155, "homeaway": .05, "expedia_pcm": .18,
        "expedia_grp": .18, "booking": .15, "hopper": .14, "marriott": .185,
        "tripcom": .111, "whimstay": .05, "blueground": .07,
        "manual": 0.0, "bnbfinder": 0.0, "unknown": 0.0}

# Owner-verified per-reservation exceptions: confirmation codes where the channel did NOT levy
# its commission on the pet fee, so the pet fee is dropped from that booking's channel-fee base.
# Scoped to the exact reservation (NOT a channel-wide rule) at the owner's instruction; add codes
# here as they are verified. Affects only the (InvoiceItem - tax) channel-fee formula.
PET_FEE_NOT_IN_CHANNEL_FEE = {"EXP-MyQbvyEn"}


def _assign_row_group(src: pd.Series, tot_tax: pd.Series, has_pcm: pd.Series,
                      is_vcard: pd.Series, index) -> pd.Series:
    """Map each reservation's source string to a payment-structure row group."""
    is_ab = src.str.contains("airbnb")
    is_haw = is_ab & (tot_tax > 0.01)              # Airbnb-Hawaii: tax in host ledger (Keaau 15-1542)
    grp = pd.Series("unknown", index=index)
    grp[is_ab & ~is_haw] = "airbnb"
    grp[is_haw] = "airbnb_hawaii"
    grp[src.isin(["vrbo", "homeaway ca", "homeaway de", "homeaway uk", "vrbo canada", "expediaintegrated"])] = "homeaway"
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
           "additional_fees", "accommodation", "markup", "pet_fee",
           "total_taxes", "payment_fees", "total_refunded", "total_paid", "balance_due",
           "pm_commission", "service_fee", "airbnb_resolution_center"] + TAXCOLS
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
    svcf = S["service_fee"].astype(float)           # manual/direct add-on "service fee" (kept as revenue)
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
    is_expedia = grp.isin(["expedia_grp", "expedia_pcm"])
    exp_clean = S["additional_fees"].astype(float) + S["service_fee"].astype(float)
    clean = clean + exp_clean.where(is_expedia, 0.0)

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
    # For a refunded booking the channel fee is refunded too, so its (InvoiceItem - tax) base
    # drops to (total_paid - tax) — the platform returns its commission on the refunded portion.
    rate = grp.map(RATE).astype(float).values
    is_vcard_exp = grp.eq("expedia_pcm").values
    cf_std_base = np.where(is_refunded, np.maximum(net_base - tax_in_II, 0.0), II.values - tax_in_II)
    # Per-reservation pet-fee exception: drop the pet fee from the channel-fee base for the
    # owner-verified codes above (the channel did not commission the pet fee on those bookings).
    pet_excl = S["confirmationCode"].astype(str).isin(PET_FEE_NOT_IN_CHANNEL_FEE).values
    cf_std_base = cf_std_base - np.where(pet_excl, S["pet_fee"].astype(float).values, 0.0)
    cf_base = np.where(is_vcard_exp,
                       (S["accommodation"].astype(float) + S["markup"].astype(float)).values / (1.0 - rate),
                       cf_std_base)
    channel_fee = np.round(cf_base * rate, 2)

    # Airbnb display tax = ((InvoiceItem - ARC) + channel fee) * property_rate.
    ab_tax = (((II.values - arc) + channel_fee) * rate_frac.values)
    airbnb_tax = np.where(ab_tax_known.values, np.round(ab_tax, 2), 0.0)
    tax_disp = np.where(is_ab.values, airbnb_tax, np.round(tot_tax.values, 2))

    # Guesty fee (1%) + Stripe fee -> host/Guesty-collected channels only.
    # Stripe = InvoiceItem x 2.9% + (successful transactions) x $0.30  (per
    # payment_structure.xlsx: "InvoiceItem*2.9%+transactions*0.3"). `transactions`
    # is the count of SUCCEEDED Stripe charges on the reservation (deposit +
    # balance each incur their own $0.30). Rows lacking payment data (deactivated
    # UI merge) fall back to 1 transaction.
    has_processing = grp.isin(["homeaway", "expedia_pcm", "expedia_grp", "manual", "bnbfinder"]).values
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
    stripe_raw = II.values * 0.029 + n_tx * 0.30
    # Guesty fee on net_base (= total_paid for refunded bookings) so its 1% is refunded on the
    # refunded portion. Stripe fee stays on the full InvoiceItem (not refunded per the sheet).
    guesty_fee = np.where(has_processing, np.round(net_base * 0.01, 2), 0.0)
    stripe_fee = np.where(has_processing, np.round(stripe_raw, 2), 0.0)

    # Guest pay (per-row formula).
    gsf10 = np.round((II.values - tax_in_II) * 0.10, 2)     # HomeAway guest service fee = (II-tax) x 10%
    guest_pay = np.select(
        [grp.isin(["airbnb", "airbnb_hawaii"]).values,
         grp.eq("homeaway").values,
         grp.isin(["expedia_pcm", "hopper", "marriott", "whimstay", "blueground"]).values],
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
    # Leading term is net_base (= total_paid for refunded bookings, else InvoiceItem): the host
    # gives back the refund. cleaning/tax/channel/stripe stay on the full InvoiceItem; only the
    # base and the Guesty fee (above) drop to the retained amount.
    net_revenue = (net_base - clean.values - tax_sub
                   - np.where(cf_in_net, channel_fee, 0.0)
                   - np.where(grp.eq("manual").values, svcf.values, 0.0)
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
    canc_stripe = np.where(has_processing, np.round(retained * 0.029 + n_tx * 0.30, 2), 0.0)
    canc_net = retained - canc_guesty - canc_stripe

    II_out      = np.where(is_canceled, retained,    II.values)
    guest_pay   = np.where(is_canceled, retained,    guest_pay)
    clean_out   = np.where(is_canceled, 0.0,         clean.values)
    tax_out     = np.where(is_canceled, 0.0,         tax_disp)
    channel_fee = np.where(is_canceled, 0.0,         channel_fee)
    guesty_fee  = np.where(is_canceled, canc_guesty, guesty_fee)
    stripe_fee  = np.where(is_canceled, canc_stripe, stripe_fee)
    host_payout = np.where(is_canceled, canc_net,    host_payout)
    net_revenue = np.where(is_canceled, canc_net,    net_revenue)

    refund = np.round(-arc, 2)                              # display only (Airbnb resolution); never re-subtracted

    out = pd.DataFrame({
        "confirmationCode": S["confirmationCode"], "channel": S["source"],
        "row_group": grp.values, "listing": S["listing"], "guest": S["guest"],
        "checkIn": S["checkIn"].astype(str).str[:10], "checkOut": S["checkOut"].astype(str).str[:10],
        "status": S["status"],
        "InvoiceItem": np.round(II_out, 2),
        "guest_pay": np.round(guest_pay, 2),
        "cleaning_fee": np.round(clean_out, 2),
        "tax": np.round(tax_out, 2),
        "channel_fee": np.round(channel_fee, 2),
        "guesty_fee": np.round(guesty_fee, 2),
        "stripe_fee": np.round(stripe_fee, 2),
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
