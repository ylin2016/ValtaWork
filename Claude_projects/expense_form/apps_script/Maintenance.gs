/**
 * One-off fixes to the Expense Config sheet, run by hand from the editor. Each is idempotent (a
 * second run changes nothing) and logs what it did. Remove a fix once it has been run.
 */

/**
 * 2026-09-23: the reviewers tab was seeded from an old reviewers.csv (vacation@); the user wants
 * approvals to go to billing@. Also: add the missing reimburse_from setting, and drop the misspelled
 * 'Maintainence' category (a duplicate of 'Maintenance', same account).
 */
function fixConfig20260923() {
  ownerOnly_();
  var ss = configSheet_();
  var done = [];

  var rv = ss.getSheetByName('reviewers');
  var rows = rv.getDataRange().getDisplayValues();
  for (var i = 1; i < rows.length; i++) {
    if (rows[i][0] === 'vacation@valtarealty.com') {
      rv.getRange(i + 1, 1, 1, 2).setValues([['billing@valtarealty.com', 'Billing']]);
      done.push('reviewer row ' + (i + 1) + ': vacation@ -> billing@');
    }
  }

  var st = ss.getSheetByName('settings');
  var keys = st.getDataRange().getDisplayValues().map(function (r) { return r[0]; });
  if (keys.indexOf('reimburse_from') < 0) {
    st.getRange(keys.length + 1, 1, 1, 2).setValues([['reimburse_from', 'chase-9967']]);
    done.push('settings: added reimburse_from = chase-9967');
  }

  var cat = ss.getSheetByName('expense_categories');
  var codes = cat.getDataRange().getDisplayValues().map(function (r) { return r[0]; });
  for (var j = codes.length - 1; j >= 1; j--) {             // bottom-up: deleting shifts rows below
    if (codes[j] === 'Maintainence') {
      cat.deleteRow(j + 1);
      done.push('expense_categories: deleted row ' + (j + 1) + ' (Maintainence)');
    }
  }

  CacheService.getScriptCache().remove(CONFIG_CACHE_KEY);    // take effect now, not in 5 minutes
  console.log(done.length ? done.join('\n') : 'Nothing to fix — already done.');
  return done;
}

/**
 * 2026-09-24: "Paid by Valta Homes" (user). Adds the money_accounts column drive_path, the valta-homes
 * account (filed under <year>/Valta Homes Unprocessed Invoices/Testing, not drafted in Valta Realty's
 * QBO), and the reviewer contact@valtahomes.com who alone reviews it (scope account:valta-homes).
 */
function fixConfig20260924() {
  ownerOnly_();
  var ss = configSheet_();
  var done = [];

  var ma = ss.getSheetByName('money_accounts');
  var head = ma.getRange(1, 1, 1, ma.getLastColumn()).getDisplayValues()[0];
  var pathCol = head.indexOf('drive_path') + 1;
  if (!pathCol) {
    pathCol = head.length + 1;
    ma.getRange(1, pathCol).setNumberFormat('@').setValue('drive_path').setFontWeight('bold');
    done.push('money_accounts: added column drive_path');
  }
  head = ma.getRange(1, 1, 1, pathCol).getDisplayValues()[0];
  var codes = ma.getDataRange().getDisplayValues().map(function (r) { return r[0]; });
  if (codes.indexOf('valta-homes') < 0) {
    var row = { code: 'valta-homes', label: 'Paid by Valta Homes', qb_account: '', kind: 'company',
                drive_folder: '', in_qbo: 'FALSE', drive_path: 'Valta Homes Unprocessed Invoices/Testing' };
    var r = ma.getLastRow() + 1;
    ma.getRange(r, 1, 1, head.length).setNumberFormat('@')
      .setValues([head.map(function (h) { return row[String(h).trim().toLowerCase().replace(/\s+/g, '_')] || ''; })]);
    done.push('money_accounts: added valta-homes (row ' + r + ')');
  }

  var rv = ss.getSheetByName('reviewers');
  var emails = rv.getDataRange().getDisplayValues().map(function (r) { return r[0]; });
  if (emails.indexOf('contact@valtahomes.com') < 0) {
    rv.getRange(rv.getLastRow() + 1, 1, 1, 4).setNumberFormat('@')
      .setValues([['contact@valtahomes.com', 'Valta Homes', 'account:valta-homes', 'TRUE']]);
    done.push('reviewers: added contact@valtahomes.com (account:valta-homes)');
  }

  CacheService.getScriptCache().remove(CONFIG_CACHE_KEY);
  console.log(done.length ? done.join('\n') : 'Nothing to fix — already done.');
  return done;
}

/**
 * 2026-09-24: QuickBooks account names for the card / bank accounts (user's QBO chart of accounts
 * screenshots), so QBO_operations can draft card/bank expenses. Fills money_accounts.qb_account only
 * where it is still blank, so a name already typed in the sheet is never overwritten.
 */
var QB_ACCOUNTS_20260924 = {
  'ops-7197': 'Chase Checking 7197',
  'ltr-3038': 'Chase Trust Checking 3038 - monthly',
  'credit-7439': 'Credit Card 7439',
  'credit-3104': 'CitiCostco3104-Supplies',
  'credit-6305': 'CitiCostco3104-Supplies',     // user: 6305 posts to the same Citi Costco account
  'credit-5565': 'Chase Credit Card 5565',
  'amazon-4783': 'Business Credit Card (4783)'
};

function fixConfig20260924b() {
  ownerOnly_();
  var sh = configSheet_().getSheetByName('money_accounts');
  var data = sh.getDataRange().getDisplayValues();
  var col = data[0].map(function (h) { return String(h).trim().toLowerCase(); }).indexOf('qb_account');
  var done = [];
  if (col < 0) throw new Error('money_accounts has no qb_account column');
  for (var i = 1; i < data.length; i++) {
    var want = QB_ACCOUNTS_20260924[data[i][0]];
    if (want && !String(data[i][col]).trim()) {
      sh.getRange(i + 1, col + 1).setNumberFormat('@').setValue(want);
      done.push('money_accounts: ' + data[i][0] + ' qb_account = ' + want);
    }
  }
  CacheService.getScriptCache().remove(CONFIG_CACHE_KEY);
  console.log(done.length ? done.join('\n') : 'Nothing to fix — already done.');
  return done;
}
