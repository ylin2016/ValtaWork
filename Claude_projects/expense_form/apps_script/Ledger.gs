/**
 * The "Expense Ledger" Google Sheet — the tracking table. One tab per table, header row 1,
 * one record per row, no merged cells. Every column is plain text ('@') so Sheets never turns a
 * confirmation code like 1234E5 into a number or a date into a serial; money columns are 0.00.
 *
 * Script Properties:
 *   PROCESSING_FOLDER_ID  the Drive folder "Expense_processing" (holds the ledger, config/, exports/);
 *                         set by hand before running setup()
 *   LEDGER_SHEET_ID       written by setup()
 */

function processingFolder_() {
  var id = PropertiesService.getScriptProperties().getProperty('PROCESSING_FOLDER_ID');
  if (!id) throw new Error('Script Property PROCESSING_FOLDER_ID is not set (Project Settings → Script Properties)');
  return DriveApp.getFolderById(id);
}

var TABS = {
  submissions: ['id', 'created_at', 'status', 'kind', 'txn_date', 'property_code', 'property_name',
    'submitter', 'amount', 'description', 'money_account',
    'category', 'vendor', 'owner_billable', 'reimburse_to',                          // expense
    'deposit_type', 'payer_name', 'reservation_code', 'check_in_date',               // deposit
    'collection_method', 'payment_reference', 'bank_deposit_id',
    'payout_id', 'completed_at', 'file_count', 'first_file_url',
    'reviewed_by', 'reviewed_at', 'review_note',
    // written back from QBO_operations (via exports/imports CSV), never by the form
    'qbo_txn_type', 'qbo_txn_id', 'qbo_customer_id', 'qbo_invoice_id', 'qbo_treatment',
    'qbo_fee_invoice_id', 'qbo_payment_id', 'qbo_posted_at',
    'upload_token_sha256', 'client_info',
    'typed_in',         // fields typed under "Other" (not in the lists): map them before QBO drafting
    'client_ref'],      // random id the form sends with submit, so a retried submit is not a second row
  attachments: ['submission_id', 'seq', 'file_name', 'drive_folder_path', 'drive_file_id', 'drive_url',
    'upload_status', 'upload_error', 'mime_type', 'size_bytes', 'sha256', 'uploaded_at'],
  bank_deposits: ['id', 'bank_date', 'money_account', 'total', 'reference', 'created_at', 'created_by'],
  zelle_payouts: ['id', 'paid_date', 'payee', 'paid_from', 'total', 'zelle_reference', 'created_at', 'created_by'],
  review_tokens: ['token_sha256', 'submission_id', 'reviewer_email', 'expires_at', 'used_at', 'kind'],
  log: ['at', 'submission_id', 'actor', 'action', 'detail']
};
var MONEY_COLUMNS = { amount: true, total: true };
var HIDDEN_TABS = { review_tokens: true };

var Ledger = {
  ss_: null,

  open: function () {
    if (!this.ss_) {
      var id = PropertiesService.getScriptProperties().getProperty('LEDGER_SHEET_ID');
      if (!id) throw new Error('LEDGER_SHEET_ID is not set — run setup() once from the editor');
      this.ss_ = SpreadsheetApp.openById(id);
    }
    return this.ss_;
  },

  sheet_: function (tab) {
    var sh = this.open().getSheetByName(tab);
    if (!sh) throw new Error('Ledger tab missing: ' + tab + ' — run setup()');
    return sh;
  },

  /** All rows as objects of strings (display values), each with _row = its sheet row number. */
  all: function (tab) {
    var cols = TABS[tab], sh = this.sheet_(tab), last = sh.getLastRow();
    if (last < 2) return [];
    return sh.getRange(2, 1, last - 1, cols.length).getDisplayValues().map(function (vals, i) {
      var o = { _row: i + 2 };
      cols.forEach(function (c, j) { o[c] = vals[j]; });
      return o;
    });
  },

  where: function (tab, col, value) {
    value = String(value);
    return this.all(tab).filter(function (r) { return r[col] === value; });
  },

  /** Returns the new row's number (call inside the script lock). */
  append: function (tab, obj) {
    var sh = this.sheet_(tab);
    sh.appendRow(TABS[tab].map(function (c) { return cellValue_(c, obj[c]); }));
    return sh.getLastRow();
  },

  /** Patch some columns of one row: a single write covering just the span of changed columns. */
  update: function (tab, rowNumber, patch) {
    var cols = TABS[tab], sh = this.sheet_(tab);
    var idx = Object.keys(patch).map(function (c) {
      var j = cols.indexOf(c);
      if (j < 0) throw new Error('No column ' + c + ' in ' + tab);
      return j;
    });
    if (!idx.length) return;
    var lo = Math.min.apply(null, idx), hi = Math.max.apply(null, idx);
    var range = sh.getRange(rowNumber, lo + 1, 1, hi - lo + 1);
    // untouched cells in the span are written back as displayed (TRUE stays TRUE, dates stay text)
    var vals = hi > lo ? range.getDisplayValues()[0].map(function (v, k) {
      return cellValue_(cols[lo + k], MONEY_COLUMNS[cols[lo + k]] && v === '' ? undefined : v);
    }) : [null];
    Object.keys(patch).forEach(function (c) { vals[cols.indexOf(c) - lo] = cellValue_(c, patch[c]); });
    range.setValues([vals]);
  },

  /** Next integer id for a tab with an 'id' column. Call inside the script lock. */
  nextId: function (tab) {
    return this.all(tab).reduce(function (m, r) { return Math.max(m, parseInt(r.id, 10) || 0); }, 0) + 1;
  }
};

function cellValue_(col, v) {
  if (v === undefined || v === null) return '';
  if (MONEY_COLUMNS[col] && v !== '') return Number(v);
  if (typeof v === 'boolean') return v ? 'TRUE' : 'FALSE';
  return String(v);
}

function log_(submissionId, actor, action, detail) {
  Ledger.append('log', { at: nowIso_(), submission_id: submissionId, actor: actor, action: action,
                         detail: detail ? JSON.stringify(detail) : '' });
}

function nowIso_() {
  return Utilities.formatDate(new Date(), 'America/Los_Angeles', "yyyy-MM-dd'T'HH:mm:ssXXX");
}

function todayIso_() {
  return Utilities.formatDate(new Date(), 'America/Los_Angeles', 'yyyy-MM-dd');
}

/**
 * One-time setup, run by hand from the Apps Script editor. Idempotent: creates the Expense Ledger
 * sheet inside Expense_processing (or reuses LEDGER_SHEET_ID), adds missing tabs and headers,
 * formats columns, creates exports/, creates/fills the Expense Config sheet (Config.gs), and
 * installs the daily export trigger.
 */
/**
 * Time triggers (replaced each run, so never duplicated):
 *   exportLedger      daily ~2am   — CSV copies reach the Mac even on days with no review
 *   sendWeeklyDigest  Friday ~noon — the one weekly review email (Mail.gs); script time zone
 *                                    America/Los_Angeles; Apps Script runs it within about 15 minutes
 */
function installTriggers_() {
  var handlers = { exportLedger: true, sendWeeklyDigest: true };
  ScriptApp.getProjectTriggers().forEach(function (t) {
    if (handlers[t.getHandlerFunction()]) ScriptApp.deleteTrigger(t);
  });
  ScriptApp.newTrigger('exportLedger').timeBased().everyDays(1).atHour(2).create();
  ScriptApp.newTrigger('sendWeeklyDigest').timeBased().onWeekDay(ScriptApp.WeekDay.FRIDAY).atHour(12).nearMinute(0).create();
}

function setup() {
  ownerOnly_();
  var props = PropertiesService.getScriptProperties();
  var folder = processingFolder_();
  var id = props.getProperty('LEDGER_SHEET_ID');
  var ss;
  if (id) {
    ss = SpreadsheetApp.openById(id);
  } else {
    ss = SpreadsheetApp.create('Expense Ledger');
    DriveApp.getFileById(ss.getId()).moveTo(folder);
    props.setProperty('LEDGER_SHEET_ID', ss.getId());
  }
  Object.keys(TABS).forEach(function (tab) {
    var cols = TABS[tab];
    var sh = ss.getSheetByName(tab) || ss.insertSheet(tab);
    sh.getRange(1, 1, 1, cols.length).setValues([cols]).setFontWeight('bold');
    sh.setFrozenRows(1);
    cols.forEach(function (c, j) {
      sh.getRange(2, j + 1, sh.getMaxRows() - 1, 1).setNumberFormat(MONEY_COLUMNS[c] ? '0.00' : '@');
    });
    if (HIDDEN_TABS[tab]) sh.hideSheet();
  });
  var blank = ss.getSheetByName('Sheet1');
  if (blank && ss.getSheets().length > 1) ss.deleteSheet(blank);
  childFolder_(folder, 'exports');
  Ledger.ss_ = ss;
  setupConfig_(folder);

  installTriggers_();
  childFolder_(folder, PENDING_FOLDER);
  reimbursementLink();                                      // creates ADMIN_KEY once; logs the private page link
  return ss.getUrl();
}
