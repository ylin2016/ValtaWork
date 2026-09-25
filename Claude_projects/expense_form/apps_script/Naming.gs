/**
 * Receipt file naming + Drive folder path — the owner's EXISTING Drive convention, which
 * QBO_operations (src/invoices/build_zelle.py) parses:
 *
 *   <YYYYMMDD>_<Property>_<Payee>_<middle ...>_<line amount>[_<Zelle total>]
 *   e.g. 20260919_Renton 18823_Valta Homes_08.08_Jing_Landscaping_440.25_6874.83.pdf  (by hand)
 *
 * The second, total amount is written ONLY on reimbursed receipts (user, 2026-09-24: "except zelle
 * payments, no need to add the second total amount"): there build_zelle reads field 2 as the property
 * and the SECOND-TO-LAST field as the line amount, the last being the person's Zelle total. Every other
 * name ends with its one amount. '_' is never allowed inside a field.
 *
 *   expense, paid at purchase (card / bank / owner / Valta Homes): pay date = purchase date
 *            20260921_Valta Realty_9.21_Costco_Supplies_75.25.jpg
 *   expense, reimbursed by Zelle: <PAY date>_<property>_<purchase M.DD>_<store>_<stuff>_<amount>_<Zelle total>
 *            20260914_Renton 18823_9.10_Home Depot_baseboard moulding and nails_302.74_1110.5.jpg
 *            The description is typed as store_stuff.
 *   pending: PENDING_<property>_<purchase M.DD>_<store>_<stuff>_<amount>  — personal receipts waiting for
 *            the weekly reimbursement run, in Expense_processing/Pending/
 *   deposit: 20260914_Renton 18823_John Smith_Zelle_Pet fee_HMABC123_-150.png   (money in: amount written negative;
 *            the ledger's amount column stays positive)
 *
 * Pure functions (no Apps Script services) so tests/*.test.js run them in node.
 */

var COMPANY_PROPERTY = 'Valta Realty';   // property field when none applies (QBO_operations maps it)
var FIELD_MAX_LEN = 30;
var ILLEGAL_RE = /[\/\\:*?"<>|#%{}$`=+_]/g;  // '_' included: it separates fields

/** An error the submitter can fix; its message is shown to them as-is. */
function UserError(message) {
  this.name = 'UserError';
  this.message = message;
  this.stack = (new Error(message)).stack;
}
UserError.prototype = Object.create(Error.prototype);
UserError.prototype.constructor = UserError;

function cleanField(s) {
  return String(s == null ? '' : s).replace(ILLEGAL_RE, ' ').replace(/\s+/g, ' ').trim();
}

function isIsoDate(s) {
  var m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(String(s || ''));
  if (!m) return false;
  var d = new Date(Date.UTC(+m[1], +m[2] - 1, +m[3]));
  return d.getUTCFullYear() === +m[1] && d.getUTCMonth() === +m[2] - 1 && d.getUTCDate() === +m[3];
}

/** '$1,234.50' -> 1234.5. At most 2 decimals, > 0. */
function parseAmount(a) {
  var s = String(a == null ? '' : a).replace(/[$,\s]/g, '');
  if (!/^\d+(\.\d{1,2})?$/.test(s) || !(Number(s) > 0)) throw new UserError('Invalid amount: ' + a);
  return Number(s);
}

/** Short form used by the existing files and QBO_operations DocNumbers: 150, 440.25, 1234.5 */
function formatAmount(a) {
  return parseAmount(a).toFixed(2).replace(/\.?0+$/, '');
}

function normalizeReservationCode(code) {
  return String(code == null ? '' : code).replace(/\s+/g, '').toUpperCase();
}

/** o = {date:'YYYY-MM-DD', propertyName, payee, middle:[...], amount, total?, ext?}; total only for Zelle reimbursements */
function buildFileName(o) {
  if (!isIsoDate(o.date)) throw new UserError('Date must be YYYY-MM-DD: ' + o.date);
  var payee = cleanField(o.payee).slice(0, FIELD_MAX_LEN).trim();
  if (!payee) throw new UserError('Payee / payer is required for the file name');
  var fields = [o.date.replace(/-/g, ''), cleanField(o.propertyName) || COMPANY_PROPERTY, payee];
  (o.middle || []).forEach(function (m) {
    var f = cleanField(m).slice(0, FIELD_MAX_LEN).trim();
    if (f) fields.push(f);
  });
  fields.push((o.negative ? '-' : '') + formatAmount(o.amount));
  if (o.total != null && o.total !== '') fields.push(formatAmount(o.total));
  var ext = String(o.ext || '').replace(/^\./, '').toLowerCase();
  return fields.join('_') + (ext ? '.' + ext : '');
}

/**
 * An expense description is 'store_stuff': the store before the FIRST '_', what was bought after it
 * ('Home Depot_baseboard moulding and nails'). Further '_' in the stuff part are kept as text.
 */
function splitStoreStuff(description) {
  var s = String(description == null ? '' : description);
  var i = s.indexOf('_');
  var store = i < 0 ? '' : cleanField(s.slice(0, i));
  var stuff = i < 0 ? '' : cleanField(s.slice(i + 1));
  if (!store || !stuff) {
    throw new UserError('Write the description as store_stuff, e.g. Home Depot_baseboard moulding and nails');
  }
  return { store: store.slice(0, FIELD_MAX_LEN).trim(), stuff: stuff };
}

/** '2026-09-10' -> '9.10', '2026-09-05' -> '9.05': month unpadded, day padded (user, 2026-09-23). */
function monthDay(date) {
  if (!isIsoDate(date)) throw new UserError('Date must be YYYY-MM-DD: ' + date);
  return String(Number(date.slice(5, 7))) + '.' + date.slice(8, 10);
}

/** o = {payDate, purchaseDate, propertyName, description ('store_stuff'), amount, total?, ext} */
function expenseFileName(o) {
  var ss = splitStoreStuff(o.description);
  return buildFileName({ date: o.payDate, propertyName: o.propertyName, payee: monthDay(o.purchaseDate),
                         middle: [ss.store, ss.stuff], amount: o.amount, total: o.total, ext: o.ext });
}

/**
 * A personal receipt before the weekly reimbursement (user, 2026-09-24): no pay date and no total yet,
 * since neither is decided until the reimbursement run —
 *   PENDING_<property>_<purchase M.DD>_<store>_<stuff>_<amount>   e.g. PENDING_Valta Realty_9.21_Costco_Supplies_75.25.jpg
 * The reimbursement run renames it to the final expenseFileName.
 */
function pendingFileName(o) {
  var ss = splitStoreStuff(o.description);
  var fields = ['PENDING', cleanField(o.propertyName) || COMPANY_PROPERTY, monthDay(o.purchaseDate),
                ss.store, ss.stuff.slice(0, FIELD_MAX_LEN).trim(), formatAmount(o.amount)];
  var ext = String(o.ext || '').replace(/^\./, '').toLowerCase();
  return fields.join('_') + (ext ? '.' + ext : '');
}

function depositFileName(o) {
  return buildFileName({ date: o.date, propertyName: o.propertyName, payee: o.payer,
                         middle: [o.method, o.depositType, normalizeReservationCode(o.reservationCode)],
                         amount: o.amount, negative: true, ext: o.ext });   // money in: written negative (user, 2026-09-24)
}

/** 'a.jpg' taken -> 'a-2.jpg', 'a-3.jpg' ... (suffix lands on the last field; on a Zelle name that is the total) */
function dedupeName(name, exists) {
  if (!exists(name)) return name;
  var dot = name.lastIndexOf('.');
  var stem = dot > 0 ? name.slice(0, dot) : name;
  var ext = dot > 0 ? name.slice(dot) : '';
  for (var i = 2; ; i++) {
    var candidate = stem + '-' + i + ext;
    if (!exists(candidate)) return candidate;
  }
}

/** Drive folder ID of the year root for `date` (drive_layout.json year_roots); loud when missing. */
function yearRoot(layout, date) {
  var root = ((layout || {}).year_roots || {})[String(date).slice(0, 4)];
  if (!root) throw new UserError('No Drive folder is set up for year ' + String(date).slice(0, 4) +
                                 ' — add it to year_roots in drive_layout.json');
  return root;
}

/** Render a drive_layout.json template, e.g. '{yyyy_mm}/{account}' -> ['2026-09', '9967']. */
function buildFolderPath(template, v) {
  if (!isIsoDate(v.date)) throw new UserError('Date must be YYYY-MM-DD: ' + v.date);
  var values = {
    account: cleanField(v.account), property: cleanField(v.propertyName) || COMPANY_PROPERTY,
    yyyy: v.date.slice(0, 4), mm: v.date.slice(5, 7), yyyy_mm: v.date.slice(0, 7),
    category: cleanField(v.category), deposit_type: cleanField(v.depositType), submitter: cleanField(v.submitter)
  };
  var rendered = String(template).replace(/\{(\w+)\}/g, function (_, key) {
    if (!(key in values)) throw new Error('Unknown placeholder {' + key + '} in drive layout ' + template);
    return values[key];
  });
  var parts = rendered.split('/').map(function (p) { return p.trim(); });
  if (parts.some(function (p) { return !p; })) {
    throw new Error('Drive layout ' + template + ' rendered an empty folder name: ' + rendered);
  }
  return parts;
}

if (typeof module !== 'undefined') {
  module.exports = { UserError: UserError, cleanField: cleanField, isIsoDate: isIsoDate, parseAmount: parseAmount,
    formatAmount: formatAmount, normalizeReservationCode: normalizeReservationCode, buildFileName: buildFileName,
    expenseFileName: expenseFileName, pendingFileName: pendingFileName, monthDay: monthDay,
    splitStoreStuff: splitStoreStuff, depositFileName: depositFileName,
    dedupeName: dedupeName, yearRoot: yearRoot, buildFolderPath: buildFolderPath };
}
