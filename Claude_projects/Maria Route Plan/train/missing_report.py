"""List the data the planner model is missing, so it can be filled in by hand (nothing is guessed).

    .venv/bin/python train/missing_report.py      → data/Missing_data.xlsx + a summary

Tabs:
  1 No address            Listings.xlsx rows with a blank Address
  2 Unmatched names       dispatch-card names that match no listing exactly
  3 No coordinates        listings used by the dispatch card with no Guesty / building / Lat-Lng
                          location (fill Lat and Lng on the listing's row in Listings.xlsx)
  4 Residential details   residential clients without bedrooms / bathrooms / EstMin_2ppl
(Back-to-back status is not used by the model, so Guesty coverage is not listed.)
Re-run after filling; the tabs shrink as gaps close.
"""
import pandas as pd

import dataset as d


def main():
    st, props = d.build()
    book = pd.ExcelFile(d.DATA / 'Listings.xlsx')

    def stops(k): return st[st.pk == k]

    no_addr = []
    for tab in ('STR', 'Residential'):
        for r in book.parse(tab).itertuples():
            if pd.isna(r.Address) or not str(r.Address).strip():
                s = stops(d.key(r.Listing))
                no_addr.append(dict(Tab=tab, Listing=r.Listing, Stops=len(s),
                                    LastSeen=s.day.max().date() if len(s) else None, Address=''))
    no_addr = pd.DataFrame(no_addr, columns=['Tab', 'Listing', 'Stops', 'LastSeen', 'Address'])

    unmatched = (st[st.pk.isna()].groupby('name')
                 .agg(Stops=('day', 'size'), Dates=('day', lambda s: ', '.join(sorted({x.strftime('%Y-%m-%d') for x in s}))))
                 .reset_index().rename(columns={'name': 'Dispatch name'}).sort_values('Stops', ascending=False))
    unmatched['Is the same property as (listing name)'] = ''

    no_coord = pd.DataFrame([dict(Listing=k, Tab='STR' if p['kind'] == 'turnover' else 'Residential',
                                  Address=p['address'], Stops=len(stops(k)), Lat='', Lng='')
                             for k, p in props.items() if (p['lat'] is None or pd.isna(p['lat'])) and len(stops(k))],
                            columns=['Listing', 'Tab', 'Address', 'Stops', 'Lat', 'Lng']).sort_values('Stops', ascending=False)

    res = st[st.kind == 'residential'].groupby('pk').agg(Stops=('day', 'size'), Bedrooms=('beds', 'first'),
                                                         Bathrooms=('baths', 'first'), EstMin_2ppl=('est_min', 'first'))
    res = res[res.Bedrooms.isna() | res.EstMin_2ppl.isna()].reset_index().rename(columns={'pk': 'Listing'})
    res = res.sort_values('Stops', ascending=False)

    out = d.DATA / 'Missing_data.xlsx'
    with pd.ExcelWriter(out) as w:
        for name, df in (('1 No address', no_addr), ('2 Unmatched names', unmatched), ('3 No coordinates', no_coord),
                         ('4 Residential details', res)):
            df.to_excel(w, sheet_name=name, index=False)
            ws = w.sheets[name]
            for col in ws.columns:
                ws.column_dimensions[col[0].column_letter].width = min(60, max(10, max(len(str(c.value or '')) for c in col) + 2))
            ws.freeze_panes = 'B2'
    n = len(st)
    print(f'{n} historical stops. Gaps:')
    print(f'  1 no address:          {len(no_addr)} listings ({no_addr.Stops.sum()} stops)')
    print(f'  2 unmatched names:     {len(unmatched)} names ({unmatched.Stops.sum()} stops)')
    print(f'  3 no coordinates:      {len(no_coord)} listings ({no_coord.Stops.sum()} stops, {no_coord.Stops.sum() / n:.0%})')
    print(f'  4 residential details: {len(res)} clients ({res.Stops.sum()} stops; '
          f'{res.Bedrooms.isna().sum()} without bedrooms, {res.EstMin_2ppl.isna().sum()} without a time estimate)')
    print(f'→ {out}')


if __name__ == '__main__':
    main()
