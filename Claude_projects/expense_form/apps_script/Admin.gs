/**
 * EVERYTHING YOU RUN BY HAND IS IN THIS FILE.
 * Open Admin.gs, pick a function in the toolbar's function menu (next to ▶ Run), click Run.
 * Results appear in the Execution log at the bottom. Each one is safe to run again.
 *
 *   showReimbursementLink   the private weekly reimbursement page (keep the link to yourself)
 *   sendReviewSummaryNow    email the Friday review summary now (each reviewer, only if something waits)
 *   exportCsvNow            rewrite exports/*.csv now (otherwise ~1 min after a review, and nightly)
 *   runSetup                after a code change that adds columns or tabs; also re-installs the triggers
 *   applyConfigUpdates      one-time Expense Config sheet updates (reviewer, Valta Homes, ...); skips what is done
 *   clearTestData           BEFORE GO-LIVE ONLY: empty the ledger and trash the test receipts
 */

/**
 * Any page this web app serves lets its visitor call ANY public (no trailing _) server function through
 * google.script.run -- and the form is public. So every function that is run by hand or by a trigger
 * starts with ownerOnly_(e): it passes in the editor (the active user is billing@, the script's own
 * account) and in its own time trigger (e.triggerUid is one of this project's triggers, which a visitor
 * cannot know), and refuses an anonymous visitor, whose active user is blank.
 */
function ownerOnly_(e) {
  if (e && e.triggerUid) {
    var uid = String(e.triggerUid);
    if (ScriptApp.getProjectTriggers().some(function (t) { return String(t.getUniqueId()) === uid; })) return;
  }
  var who = String(Session.getActiveUser().getEmail() || '').toLowerCase();
  var me = String(Session.getEffectiveUser().getEmail() || '').toLowerCase();
  if (!who || who !== me) throw new UserError('Only the script owner can run this, from the Apps Script editor.');
}

function showReimbursementLink() {
  return reimbursementLink();
}

function sendReviewSummaryNow() {
  return sendWeeklyDigest();
}

function exportCsvNow() {
  exportLedger();
  console.log('exports/*.csv rewritten');
}

function runSetup() {
  return setup();
}

function applyConfigUpdates() {
  var done = fixConfig20260923().concat(fixConfig20260924(), fixConfig20260924b());
  console.log(done.length ? done.join('\n') : 'Expense Config is up to date.');
  return done;
}

/**
 * clearTestData — BEFORE GO-LIVE ONLY. Empties every ledger tab (headers stay), moves every receipt
 * the ledger filed to Drive's Trash (recoverable for 30 days), and rewrites the CSV exports.
 * Refuses once there is real data: more than 30 submissions, or anything already posted to QBO.
 */
function clearTestData() {
  ownerOnly_();
  var subs = Ledger.all('submissions');
  if (subs.length > 30) throw new Error('Refused: ' + subs.length + ' submissions — this looks like real data');
  if (subs.some(function (s) { return s.qbo_txn_id || s.qbo_posted_at; })) throw new Error('Refused: some rows are already in QuickBooks');
  var layout = lookups_(loadConfig_()).layout, trashed = 0, skipped = [];
  withLock_(function () {
    Ledger.all('attachments').forEach(function (a) {
      if (!a.drive_file_id) return;
      try {
        var f = DriveApp.getFileById(a.drive_file_id);
        if (!underYearRoots_(f, layout)) { skipped.push(a.file_name); return; }
        if (!f.isTrashed()) { f.setTrashed(true); trashed++; }
      } catch (e) { skipped.push(a.file_name + ' (' + e.message + ')'); }
    });
    ['submissions', 'attachments', 'bank_deposits', 'zelle_payouts', 'review_tokens', 'log'].forEach(function (tab) {
      var sh = Ledger.sheet_(tab), last = sh.getLastRow();
      if (last > 1) sh.deleteRows(2, last - 1);
    });
  });
  exportLedgerSafe_();
  console.log('Cleared ' + subs.length + ' test submissions; ' + trashed + ' receipt file(s) moved to Trash.' +
              (skipped.length ? '\nNot touched (outside the bookkeeping folders or missing): ' + skipped.join(', ') : ''));
}
