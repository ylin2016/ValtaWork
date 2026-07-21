/**
 * CleaningReminder — listing directory (for Maria's route planning)
 *
 * Maps a listing NAME (the prefix of a calendar unit, e.g. "Bellevue 2243" from
 * a title like "Bellevue 2243(6->0)") to its address, bedrooms and bathrooms.
 * The route planner uses the address to geocode + order stops, and beds/baths to
 * estimate cleaning time (see CONFIG.ROUTING).
 *
 * SOURCE OF TRUTH: this table is generated from data/Listings.xlsx. To update,
 * either edit the rows below, OR point CONFIG.ROUTING.LISTINGS_SHEET_ID at a
 * Google Sheet with the same columns (Listing, Address, Bedrooms, Bathrooms) and
 * the sheet wins. Run `checkListings` to see which of tomorrow's units resolve.
 *
 * Columns: [ name, address, bedrooms, bathrooms ]  (bathrooms may be fractional)
 */
const LISTINGS = [
  ['Beachwood 1', '4027 Beach Dr SW, Seattle, WA 98116, USA', 2, 2],
  ['Bellevue 10409', '10409 NE 32nd Place, Bellevue, WA 98004, USA', 2, 1.5],
  ['Bellevue 1326', '1326 Bellevue Way NE, Bellevue, WA 98004, USA', 2, 2],
  ['Bellevue 1420', '1420 109th Ave SE, Bellevue, WA 98004, United States', 3, 1],
  ['Bellevue 1621', '1621 107th Ave SE, Bellevue, WA 98004, USA', 2, 2.5],
  ['Bellevue 2243', '2243 W Lake Sammamish Pkwy SE, WA 98008', 4, 3],
  ['Bellevue 2323 Whole', '2323 167th Ave NE, Bellevue, WA 98008', 4, 3],
  ['Bellevue 2323 Main', '2323 167th Ave NE, Bellevue, WA 98008', 3, 2],
  ['Bellevue 2323 ADU', '2323 167th Ave NE, Bellevue, WA 98008', 1, 1],
  ['Bellevue 242', '242 West Lake Sammamish Pkwy SE, WA 98008', 4, 2.5],
  ['Bellevue 321', '321 Bellevue Way SE, Bellevue, Washington 98004', 2, 1],
  ['Bellevue 514', '514 142nd Ave SE, Bellevue, WA 98007, USA', 2, 1.5],
  ['Bellevue 701', '701 122nd Avenue Northeast, Bellevue, Washington 98005', 2, 1],
  ['Burien 14407 middle', '14407 2nd Ave SW, Burien, WA 98166, USA', 3, 1],
  ['Burien 14407 top', '14407 2nd Ave SW, Burien, WA 98166, USA', 1, 1],
  ['Elektra 1004', '1400 Hubbell Pl, Seattle, WA 98101, USA', 1, 1],
  ['Elektra 1108', '1400 Hubbell Pl, Seattle, WA 98101, USA', 1, 1],
  ['Elektra 1115', '1400 Hubbell Pl, Seattle, WA 98101, USA', 1, 1],
  ['Elektra 1203', '1400 Hubbell Pl, Seattle, WA 98101, USA', 1, 1],
  ['Elektra 1212', '1400 Hubbell Pl, Seattle, WA 98101, USA', 2, 1],
  ['Elektra 1305', '1400 Hubbell Pl, Seattle, WA 98101, USA', 0, 1],
  ['Elektra 1314', '1400 Hubbell Pl, Seattle, WA 98101, USA', 0, 1],
  ['Elektra 1413', '1400 Hubbell Pl, Seattle, WA 98101, USA', 1, 1],
  ['Elektra 1514', '1400 Hubbell Pl, Seattle, WA 98101, USA', 0, 1],
  ['Elektra 510', '1400 Hubbell Pl, Seattle, WA 98101, USA', 2, 1],
  ['Elektra 609', '1400 Hubbell Pl, Seattle, WA 98101, USA', 2, 1],
  ['Elektra 703', '1400 Hubbell Pl, Seattle, WA 98101, USA', 2, 1],
  ['Elektra 809', '1400 Hubbell Pl, Seattle, WA 98101, USA', 2, 1],
  ['Elektra 909', '1400 Hubbell Pl, Seattle, WA 98101, USA', 2, 1],
  ['Kirkland 11321', '11321 106th Avenue Northeast, Kirkland, WA 98033, USA', 5, 3.5],
  ['Kirkland 13070', '13070 134th Avenue Northeast, Kirkland, WA 98034, USA', 4, 2.5],
  ['Kirkland 8017', '8017 Northeast 122nd Place, Kirkland, Washington 98034', 6, 3.5],
  ['Lynnwood 17506', '17506 57th Ave W, Lynnwood, WA 98037', 5, 2.5],
  ['Mercer 2449', '2449 63rd Ave SE, Mercer Island, WA 98040, USA', 4, 3],
  ['Mercer 3627 ADU', '3627 80th Ave SE, Mercer Island, WA 98040, USA', 1, 1],
  ['Mercer 3627 Lower', '3627 80th Ave SE, Mercer Island, WA 98040, USA', 3, 2],
  ['Mercer 3925', '3925 95th Court Southeast, Mercer Island, Washington 98040, United States', 1, 1],
  ['Microsoft 14615-D303', '14615 Northeast 32nd Street , Bellevue, Washington 98007', 1, 1],
  ['Microsoft 14620-E205', '14620 Northeast 31st Street, Bellevue, Washington 98007', 1, 1],
  ['Microsoft 14645-C19', '14645 NE 34th St, Bellevue, WA 98007, USA', 1, 1],
  ['Redmond 11641', '11641 204th Avenue Northeast, Redmond, Washington 98053, United States', 3, 2],
  ['Redmond 14707', '14707 Northeast 61st Court, Redmond, Washington 98052', 5, 3],
  ['Redmond 15357', '15357 NE 107th Way, Redmond, WA 98052, USA', 4, 3.5],
  ['Redmond 18641', '18641 NE 55th Wy, Redmond, WA 98052', 3, 2.5],
  ['Redmond 7579', '7579 Old Redmond Rd, Redmond, WA 98052, USA', 2, 2],
  ['Sammamish 2009', '2009 NE 216th Pl, Sammamish, WA 98074,USA', 3, 2.5],
  ['Sammamish 5124-1', '5124 188th Place Northeast, Sammamish, Washington 98074', 1, 1],
  ['Seattle 10057 Lower', '10057 17th Avenue Southwest, Seattle, WA 98146, USA', 2, 1],
  ['Seattle 10057 Upper', '10057 17th Avenue Southwest, Seattle, WA 98146, USA', 2, 1],
  ['Seattle 10057 Whole', '10057 17th Avenue Southwest, Seattle, WA 98146, USA', 4, 2],
  ['Seattle 1117', '1117 NW 11th Ave, Seattle, WA 98107, USA', 2, 2],
  ['Seattle 11331', '11331 24th Ave NE,Seattle, WA 98125', 4, 2.75],
  ['Seattle 115', '115 North 46th Street,Seattle, WA 98103, USA', 4, 2.5],
  ['Seattle 1424C', '1424 24th Ave, Seattle, WA 98122, USA', 3, 2],
  ['Seattle 1502', '1502 S Walker St., Seattle, WA 98144, USA', 2, 2],
  ['Seattle 1623', '1623 S Lane St, Seattle, WA 98144, USA', 3, 2.5],
  ['Seattle 206', '206 Northwest 87th Street, Seattle, WA98117, USA', 4, 2],
  ['Seattle 4201', '4201 51st Avenue South, Seattle, Washington 98118', 3, 2],
  ['Seattle 710', '710 N 97th St, Seattle, WA 98103, USA', 4, 3],
  ['Seattle 710 ADU', '710 N 97th St, Seattle, WA 98103, USA', 1, 1],
  ['Seattle 7434', '7434 Keen Way N, Seattle, WA, 98103', 4, 2],
  ['Seattle 7434 Upper', '7434 Keen Way N, Seattle, WA, 98103', 2, 1],
  ['Seattle 7434 Lower', '7434 Keen Way N, Seattle, WA, 98103', 2, 1],
  ['Seattle 8415', '8415 Linden Ave N, Seattle, WA 98103, USA', 3, 3.5],
  ['Seattle 9021', '9021 8th Ave S, Seattle, WA 98108, USA', 2, 1],
  ['Seattle 906 Lower', '906 North 101st Street, Seattle, Washington 98133', 2, 1],
  ['Seattle 906 Upper', '906 North 101st Street, Seattle, Washington 98133', 3, 2],
  ['Shoreline 15510', '15510 Meridian Avenue North, Shoreline, Washington 98133', 3, 2],
];

/** Lazily-built name → { name, address, beds, baths } index (embedded + optional Sheet). */
var _LISTING_INDEX = null;

/** Normalize a listing/unit name for matching: drop "(6->0)" suffix, lowercase, collapse spaces. */
function listingKey_(name) {
  return String(name)
    .replace(/\([^)]*\)\s*$/, '')   // strip a trailing "(out->in)" arrow group
    .replace(/\s+/g, ' ')
    .trim()
    .toLowerCase();
}

/** Build (once) the listing index from the embedded table, overlaid by the Sheet if configured. */
function listingIndex_() {
  if (_LISTING_INDEX) return _LISTING_INDEX;
  const idx = {};
  LISTINGS.forEach(function (r) {
    idx[listingKey_(r[0])] = { name: r[0], address: r[1], beds: Number(r[2]) || 0, baths: Number(r[3]) || 0 };
  });
  sheetListings_().forEach(function (r) {   // Sheet rows override embedded ones
    if (r.name) idx[listingKey_(r.name)] = r;
  });
  _LISTING_INDEX = idx;
  return idx;
}

/** Look up a unit by its calendar name (with or without the arrow suffix). null if unknown. */
function lookupListing_(unitName) {
  return listingIndex_()[listingKey_(unitName)] || null;
}

/**
 * Read listings from the optional override Sheet (CONFIG.ROUTING.LISTINGS_SHEET_ID).
 * Expects a header row containing Listing/Name, Address, Bedrooms, Bathrooms (any order,
 * case-insensitive). Returns [] if not configured or unreadable.
 */
function sheetListings_() {
  const id = CONFIG.ROUTING && CONFIG.ROUTING.LISTINGS_SHEET_ID;
  if (!id) return [];
  try {
    const sheet = SpreadsheetApp.openById(id).getSheets()[0];
    const values = sheet.getDataRange().getValues();
    if (!values.length) return [];
    const head = values[0].map(function (h) { return String(h).trim().toLowerCase(); });
    const col = function (names) {
      for (var i = 0; i < head.length; i++) if (names.indexOf(head[i]) !== -1) return i;
      return -1;
    };
    const cName = col(['listing', 'name']);
    const cAddr = col(['address']);
    const cBed = col(['bedrooms', 'beds']);
    const cBath = col(['bathrooms', 'baths']);
    if (cName === -1 || cAddr === -1) return [];
    return values.slice(1).map(function (row) {
      return {
        name: String(row[cName]).trim(),
        address: String(row[cAddr]).trim(),
        beds: cBed === -1 ? 0 : Number(row[cBed]) || 0,
        baths: cBath === -1 ? 0 : Number(row[cBath]) || 0,
      };
    }).filter(function (r) { return r.name; });
  } catch (e) {
    Logger.log('Listings sheet %s unreadable: %s', id, e.message);
    return [];
  }
}
