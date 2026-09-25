/**
 * CSV copies for the Mac, in Expense_processing/exports/ (a Google Sheet reaches the Mac only as a
 * .gsheet link, which local tools cannot read). The user, 2026-09-24: Google Sheets and CSV are fine
 * "as long as it is easy to read" — so plain CSV, one file per tab, rewritten in place (same Drive file,
 * so Drive for desktop just updates it):
 *   exports/submissions.csv, attachments.csv, bank_deposits.csv, zelle_payouts.csv   (no token hashes)
 *   exports/zelle_expense2qbo.csv   one row per reimbursed receipt in the columns QBO_operations'
 *                                   build_zelle reads (Date, Pay, Zelle content, JE Amount, Amount,
 *                                   Property, Category, qbo_account, filename, submission_id)
 *   exports/config/<tab>.csv        every tab of the Expense Config sheet (NOT Expense_processing/config/,
 *                                   whose properties.csv is an input the script imports)
 * UTF-8 with a BOM so Excel shows accents and Chinese correctly. Read them as text (pandas dtype=str):
 * codes like 1234E5 must not become numbers.
 *
 * Written by a one-off trigger about a minute after a review or reimbursement (scheduleExport_), and
 * daily at 2am. Run exportLedger() from the editor to refresh by hand.
 */

var EXPORT_TABS = ['submissions', 'attachments', 'bank_deposits', 'zelle_payouts'];
var EXPORT_OMIT = { upload_token_sha256: true };
var OLD_XLSX_EXPORTS = ['Expense Ledger export.xlsx', 'Expense Config export.xlsx'];   // before 2026-09-24

function exportLedger(e) {
  ownerOnly_(e);
  exportLedger_();
}

function exportLedger_() {
  var dir = childFolder_(processingFolder_(), 'exports');
  EXPORT_TABS.forEach(function (tab) {
    var cols = TABS[tab].filter(function (c) { return !EXPORT_OMIT[c]; });
    writeCsv_(dir, tab + '.csv', [cols].concat(Ledger.all(tab).map(function (r) {
      return cols.map(function (c) { return r[c]; });
    })));
  });
  writeCsv_(dir, 'zelle_expense2qbo.csv', expense2qboRows_());

  var cfgDir = childFolder_(dir, 'config');
  configSheet_().getSheets().forEach(function (sh) {
    writeCsv_(cfgDir, sh.getName() + '.csv', sh.getLastRow() ? sh.getDataRange().getDisplayValues() : []);
  });

  OLD_XLSX_EXPORTS.forEach(function (name) {                 // replaced by the CSVs; to the trash, recoverable
    var f = liveFile_(dir, name);
    if (f) f.setTrashed(true);
  });
}

// Read by QBO_operations build_zelle (--src .../zelle_expense2qbo.csv). 'Paid from' is the QBO bank
// account name; 'typed_in' non-empty = the row still has a typed-in value, and build_zelle drops it.
var EXPENSE2QBO_COLS = ['Date', 'Pay', 'Zelle content', 'JE Amount', 'Amount', 'Property', 'Category',
                        'qbo_account', 'filename', 'submission_id', 'Paid from', 'typed_in'];

/** Reimbursed receipts in build_zelle's input shape: one row per submission, grouped by Zelle payout. */
function expense2qboRows_() {
  var payouts = {};
  Ledger.all('zelle_payouts').forEach(function (p) { payouts[p.id] = p; });
  var cats = {}, accounts = {};
  try { var L = lookups_(loadConfig_()); cats = L.categories; accounts = L.accounts; } catch (e) { /* export without accounts */ }
  var files = {};
  Ledger.all('attachments').forEach(function (a) { if (!files[a.submission_id]) files[a.submission_id] = a.file_name; });
  var rows = Ledger.all('submissions').filter(function (s) { return s.payout_id && payouts[s.payout_id]; })
    .map(function (s) {
      var p = payouts[s.payout_id];
      return [p.paid_date, p.payee, p.zelle_reference, p.total, s.amount, s.property_name,
              s.category, (cats[s.category] || {}).qb_account || '', files[s.id] || '', s.id,
              (accounts[p.paid_from] || {}).qb_account || '', s.typed_in || ''];
    });
  return [EXPENSE2QBO_COLS].concat(rows);
}

/** Rewrite dir/fileName in place (create it the first time). */
function writeCsv_(dir, fileName, rows) {
  var text = '﻿' + rows.map(function (r) { return r.map(csvCell_).join(','); }).join('\r\n') + '\r\n';
  var f = liveFile_(dir, fileName);
  if (f) f.setContent(text);
  else dir.createFile(fileName, text, 'text/csv');
}

function csvCell_(v) {
  var s = v === undefined || v === null ? '' : String(v);
  return /[",\r\n]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
}

function exportLedgerSafe_() {
  try { exportLedger_(); } catch (e) { console.error('export failed: ' + (e && e.stack || e)); }
}

/**
 * A review click or the reimbursement page never waits for the export (user, 2026-09-24: approve was
 * "really slow"). A one-off trigger runs exportLedgerSoon about a minute later; several clicks in a row
 * share one run.
 */
var EXPORT_DELAY_MS = 60 * 1000;

function scheduleExport_() {
  try {
    var pending = ScriptApp.getProjectTriggers().some(function (t) { return t.getHandlerFunction() === 'exportLedgerSoon'; });
    if (!pending) ScriptApp.newTrigger('exportLedgerSoon').timeBased().after(EXPORT_DELAY_MS).create();
  } catch (e) {
    console.error('could not schedule the export (the daily 2am one still runs): ' + (e && e.stack || e));
  }
}

/** Trigger target for scheduleExport_: remove its own one-off trigger, then export. */
function exportLedgerSoon(e) {
  ownerOnly_(e);
  ScriptApp.getProjectTriggers().forEach(function (t) {
    if (t.getHandlerFunction() === 'exportLedgerSoon') ScriptApp.deleteTrigger(t);
  });
  exportLedgerSafe_();
}
