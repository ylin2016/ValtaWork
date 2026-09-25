/**
 * Weekly reimbursement run (user, 2026-09-23: "reimburse once a week … pay date is when I
 * reimburse/processing"). Only expenses paid PERSONALLY wait for this; everything else is filed final
 * at submission.
 *
 *   GET ?action=reimburse&k=ADMIN_KEY   private page (ReimbursePage.html): approved, unpaid personal expenses grouped by
 *                                       person, with each person's Zelle total
 *   processReimbursements(key, date)    after the Zelles are sent: per person one zelle_payouts row
 *                                       (= one QBO Purchase from 9967 in QBO_operations' build_zelle
 *                                       shape), and every receipt renamed to its final name — pay date
 *                                       first, that person's Zelle total last — and moved from Pending/
 *                                       to <pay year>/<pay month>/<account of reimburse_from>/.
 *
 * ADMIN_KEY is a Script Property created by setup(); run reimbursementLink() in the editor to see the
 * page's address. Only 'approved' submissions are paid; ones still waiting for review stay queued.
 */

var PENDING_FOLDER = 'Pending';
var DEFAULT_REIMBURSE_FROM = 'chase-9967';

function reimbursementLink() {
  ownerOnly_();
  var url = ScriptApp.getService().getUrl() + '?action=reimburse&k=' + adminKey_();
  console.log('Reimbursements page (keep private): ' + url);
  return url;
}

function adminKey_() {
  var props = PropertiesService.getScriptProperties();
  var key = props.getProperty('ADMIN_KEY');
  if (!key) { key = newToken_(); props.setProperty('ADMIN_KEY', key); }
  return key;
}

function checkAdmin_(key) {
  var want = PropertiesService.getScriptProperties().getProperty('ADMIN_KEY');
  if (!want || !key || String(key) !== want) throw new UserError('Not allowed');
}

function cents_(amount) { return Math.round(Number(amount) * 100); }

/** Approved, unpaid personal expenses grouped by the person to reimburse. */
function reimbursementQueue_(L) {
  var personal = Ledger.all('submissions').filter(function (s) {
    return s.kind === 'expense' && !s.payout_id && (L.accounts[s.money_account] || {}).kind === 'personal';
  });
  var groups = {};
  personal.filter(function (s) { return s.status === 'approved'; }).forEach(function (s) {
    var g = groups[s.reimburse_to] || (groups[s.reimburse_to] = {
      person: s.reimburse_to, payee: (L.people[s.reimburse_to] || {}).qbo_payee || s.reimburse_to,
      items: [], total_cents: 0 });
    g.items.push(s);
    g.total_cents += cents_(s.amount);
  });
  return {
    groups: Object.keys(groups).sort().map(function (k) { return groups[k]; }),
    waiting_review: personal.filter(function (s) { return s.status === 'pending'; }).length
  };
}

function reimbursePage_(key) {
  var t = HtmlService.createTemplateFromFile('ReimbursePage');
  t.key = key || '';
  t.today = todayIso_();
  try {
    checkAdmin_(key);
    var q = reimbursementQueue_(lookups_(loadConfig_()));
    t.error = '';
    t.waiting = q.waiting_review;
    t.groups = q.groups.map(function (g) {
      return { person: g.person, payee: g.payee, total: (g.total_cents / 100).toFixed(2),
               items: g.items.map(function (s) {
                 return { id: s.id, date: s.txn_date, property: s.property_name, description: s.description,
                          amount: Number(s.amount).toFixed(2) };
               }) };
    });
  } catch (err) {
    t.error = errorResult_(err).error;
    t.groups = [];
    t.waiting = 0;
  }
  return t.evaluate().setTitle('Reimbursements').addMetaTag('viewport', 'width=device-width, initial-scale=1');
}

/** Called from ReimbursePage.html after the Zelle payments have been sent. */
function processReimbursements(key, paidDate) {
  checkAdmin_(key);
  if (!isIsoDate(paidDate)) throw new UserError('Pay date must be YYYY-MM-DD');
  if (paidDate > todayIso_()) throw new UserError('Pay date cannot be in the future');
  var cfg = loadConfig_(), L = lookups_(cfg), layout = L.layout;
  var rootId = yearRoot(layout, paidDate);
  var fromCode = (cfg.settings_map || {}).reimburse_from || DEFAULT_REIMBURSE_FROM;
  var from = L.accounts[fromCode];
  if (!from || !from.drive_folder) throw new Error('Reimbursements are paid from ' + fromCode + ', which has no drive_folder');
  var folderPath = buildFolderPath(layout.expense, { date: paidDate, account: from.drive_folder });

  var payouts = withLock_(function () {
    var q = reimbursementQueue_(L);
    if (!q.groups.length) throw new UserError('Nothing approved is waiting to be reimbursed');
    return q.groups.map(function (g) {
      var total = (g.total_cents / 100).toFixed(2);
      var id = Ledger.nextId('zelle_payouts');
      var reference = [paidDate.replace(/-/g, ''), cleanField(g.payee), formatAmount(total)].join('_');
      Ledger.append('zelle_payouts', { id: id, paid_date: paidDate, payee: g.payee, paid_from: fromCode, total: total,
                                       zelle_reference: reference, created_at: nowIso_(), created_by: 'reimburse page' });
      g.items.forEach(function (s) {
        Ledger.update('submissions', s._row, { payout_id: id });
        Ledger.where('attachments', 'submission_id', s.id).forEach(function (a) {
          if (a.upload_status !== 'uploaded') return;
          var ext = (a.file_name.match(/\.([A-Za-z0-9]+)$/) || [])[1] || '';
          var name = expenseFileName({ payDate: paidDate, purchaseDate: s.txn_date, propertyName: s.property_name,
            description: s.description, amount: s.amount, total: total, ext: ext });
          var taken = namesFiledIn_(Ledger.all('attachments'), folderPath.join('/'), a._row);
          var moved = moveFile_(layout, a.drive_file_id, rootId, folderPath, name, taken);
          Ledger.update('attachments', a._row, { file_name: moved.fileName, drive_url: moved.url,
                                                 drive_folder_path: folderPath.join('/') });
        });
        log_(s.id, 'reimburse page', 'reimbursed', { payout_id: id, paid_date: paidDate, total: total });
      });
      return { person: g.person, payee: g.payee, total: total, count: g.items.length, reference: reference };
    });
  });
  scheduleExport_();
  return { ok: true, payouts: payouts };
}
