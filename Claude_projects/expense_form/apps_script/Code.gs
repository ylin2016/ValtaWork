/**
 * Web App entry points. Deploy: Execute as ME, access ANYONE — submitters need no Google account;
 * files, the ledger sheet and mail all act as the owner.
 *
 *   GET  ?action=config                 -> JSON lists for the form's dropdowns
 *   GET  ?action=queue&t=TOKEN          -> weekly review queue page (link in the Friday summary email)
 *   GET  ?action=review&t=TOKEN[&a=...] -> single-submission review page (links sent before the weekly summary)
 *   GET  ?action=reimburse&k=ADMIN_KEY  -> weekly reimbursement page (Reimburse.gs)
 *   POST {action:'submit',   data:{...}, client:'<user agent>'}      -> {ok, id, upload_token}
 *   POST {action:'file',     data:{id, upload_token, mime_type, base64}} -> {ok, seq, file_name, url}
 *   POST {action:'complete', data:{id, upload_token}}                -> {ok}
 *
 * The form must POST with Content-Type: text/plain (a JSON body), or the browser sends a CORS
 * preflight that Apps Script cannot answer. Errors come back as {ok:false, error:'...'}.
 */

function doGet(e) {
  var p = (e && e.parameter) || {};
  try {
    if (p.action === 'config') return json_(publicConfig());
    if (p.action === 'queue') return queuePage_(p.t);
    if (p.action === 'review') return reviewPage_(p.t, p.a);
    if (p.action === 'reimburse') return reimbursePage_(p.k);
    if (p.action === 'ping') return json_({ ok: true, service: 'expense_form' });
    return formPage_();                                     // the plain /exec link is the form itself
  } catch (err) {
    return json_(errorResult_(err));
  }
}

function doPost(e) {
  var body;
  try {
    body = JSON.parse(e.postData.contents);
  } catch (x) {
    return json_({ ok: false, error: 'Request body must be JSON' });
  }
  try {
    switch (body.action) {
      case 'submit':   return json_(submitExpense(body.data, body.client));
      case 'file':     return json_(addFile(body.data));
      case 'complete': return json_(completeSubmission(body.data));
      default:         throw new UserError('Unknown action: ' + body.action);
    }
  } catch (err) {
    return json_(errorResult_(err));
  }
}

/**
 * The form, served by the web app itself (user, 2026-09-24: "the real form on web app" for others to test).
 * The lists are baked into the page (no second request), and the page talks to formApi() through
 * google.script.run, which skips the POST redirect behind Google's intermittent "Not JSON (HTTP 404)".
 */
function formPage_() {
  var t = HtmlService.createTemplateFromFile('Form');
  try {
    t.cfgJson = JSON.stringify(publicConfig()).replace(/</g, '\\u003c');   // safe inside <script>
    t.error = '';
  } catch (err) {
    t.cfgJson = 'null';
    t.error = errorResult_(err).error;
  }
  return t.evaluate().setTitle('Valta Expense Form').addMetaTag('viewport', 'width=device-width, initial-scale=1, viewport-fit=cover');
}

/** google.script.run entry for Form.html: the same actions and answers as doPost. */
function formApi(action, body) {
  try {
    body = body || {};
    switch (action) {
      case 'submit':   return submitExpense(body.data, body.client);
      case 'file':     return addFile(body.data);
      case 'complete': return completeSubmission(body.data);
      default:         throw new UserError('Unknown action: ' + action);
    }
  } catch (err) {
    return errorResult_(err);
  }
}

function reviewPage_(token, preset) {
  var t = HtmlService.createTemplateFromFile('Review');
  t.token = token || '';
  t.preset = preset === 'reject' ? 'reject' : 'approve';
  try {
    t.ctx = reviewContext_(token);
    t.error = '';
  } catch (err) {
    t.ctx = null;
    t.error = errorResult_(err).error;
  }
  return t.evaluate().setTitle('Review submission').addMetaTag('viewport', 'width=device-width, initial-scale=1');
}

function queuePage_(token) {
  var t = HtmlService.createTemplateFromFile('Queue');
  t.token = token || '';
  try {
    t.ctx = queueContext_(token);
    t.error = '';
  } catch (err) {
    t.ctx = null;
    t.error = errorResult_(err).error;
  }
  return t.evaluate().setTitle('Review expenses').addMetaTag('viewport', 'width=device-width, initial-scale=1');
}

function errorResult_(err) {
  if (err && err.name === 'UserError') return { ok: false, error: err.message };
  console.error(err && err.stack || err);
  return { ok: false, error: 'Something went wrong on the server. Please try again, or tell the office.' };
}

function json_(obj) {
  return ContentService.createTextOutput(JSON.stringify(obj)).setMimeType(ContentService.MimeType.JSON);
}
