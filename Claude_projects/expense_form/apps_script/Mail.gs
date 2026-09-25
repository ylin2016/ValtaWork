/**
 * The ONE review email: a weekly summary every Friday around noon (trigger in Ledger.gs), to each
 * active reviewer (Expense Config "reviewers" tab, scope 'all' or a property code) that has something
 * waiting. Nothing is emailed per submission (user, 2026-09-23).
 *
 * NEVER email submitters or anyone outside the reviewers list (user, 2026-09-23, after a cleaner
 * received a confirmation for a test entered under her name). Don't add any other recipient without
 * the user's explicit OK.
 *
 * Every message is sent with the name MAIL_FROM_NAME and ends with MAIL_FOOTER, a fixed phrase a
 * Gmail filter can match ("Has the words") to label them. Changing either breaks that filter.
 */

var MAIL_FROM_NAME = 'Valta Expense Form';
var MAIL_FOOTER = 'Sent by Valta Expense Form';

function sendWeeklyDigest(e) {
  ownerOnly_(e);
  var cfg = loadConfig_(), L = lookups_(cfg);
  var base = ScriptApp.getService().getUrl();
  var sent = [];
  activeReviewers_(cfg.reviewers).forEach(function (r) {
    var items = reviewQueue_(r);
    if (!items.length) return;                                // nothing waiting: no email
    var token = newToken_();
    Ledger.append('review_tokens', { token_sha256: sha256Hex_(token), submission_id: '', reviewer_email: r.email,
      expires_at: new Date(Date.now() + REVIEW_LINK_DAYS * 864e5).toISOString(), kind: 'queue' });
    var total = items.reduce(function (c, s) { return c + Math.round(Number(s.amount) * 100); }, 0) / 100;
    var subject = '[Expense review] ' + items.length + ' waiting · $' + total.toFixed(2);
    var rows = items.map(function (s) {
      var what = s.kind === 'expense' ? ((L.categories[s.category] || {}).label_en || s.category)
                                      : ((L.depositTypes[s.deposit_type] || {}).label_en || s.deposit_type);
      return '<tr>' + [ '#' + s.id, s.txn_date, s.property_name, s.kind, what, s.submitter,
                        s.description + (s.typed_in ? '  ⚠ typed in, not in list: ' + s.typed_in : '')
                      ].map(function (v) { return '<td style="padding:4px 10px 4px 0">' + esc_(v) + '</td>'; }).join('') +
             '<td style="padding:4px 0;text-align:right">$' + Number(s.amount).toFixed(2) + '</td></tr>';
    }).join('');
    sendSafe_('', { to: r.email, subject: subject, htmlBody:
      '<p style="font-family:sans-serif;font-size:14px">' + items.length + ' submission(s) are waiting for your review.</p>' +
      '<table style="border-collapse:collapse;font-family:sans-serif;font-size:13px">' + rows + '</table>' +
      '<p style="margin-top:20px">' + button_(base + '?action=queue&t=' + token, 'Review all', '#0969da') + '</p>' +
      '<p style="color:#666;font-size:12px;font-family:sans-serif">The link works for ' + REVIEW_LINK_DAYS +
      ' days and can be used more than once. Opening it changes nothing.</p>' });
    log_('', 'system', 'digest_sent', { to: r.email, count: items.length });
    sent.push(r.email + ': ' + items.length);
  });
  console.log(sent.length ? 'Weekly summary sent — ' + sent.join(', ') : 'Nothing waiting for review; no email sent.');
  return sent;
}

function button_(href, label, color) {
  return '<a href="' + esc_(href) + '" style="background:' + color + ';color:#fff;padding:10px 18px;' +
         'border-radius:6px;text-decoration:none;font-family:sans-serif;font-weight:bold">' + esc_(label) + '</a>';
}

function sendSafe_(submissionId, msg) {
  msg.name = MAIL_FROM_NAME;
  msg.htmlBody += '<p style="color:#888;font-size:11px;font-family:sans-serif;margin-top:24px">' + MAIL_FOOTER + '</p>';
  try {
    MailApp.sendEmail(msg);
  } catch (e) {
    log_(submissionId, 'system', 'mail_failed', { to: msg.to, error: String(e && e.message || e) });
  }
}

function esc_(s) {
  return String(s == null ? '' : s).replace(/[&<>"']/g, function (c) {
    return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c];
  });
}
