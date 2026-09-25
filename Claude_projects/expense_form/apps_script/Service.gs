/**
 * The three calls the form makes, and the reviewer's decision.
 *
 *   submitExpense(data)  -> {ok, id, upload_token}      row appended, status 'pending'
 *   addFile(data)        -> {ok, seq, file_name, url}   one file per call (id + upload_token)
 *   completeSubmission() -> {ok}                        "all files sent" (no email; weekly summary instead)
 *   decideFromQueue(token, ids, action, note)          from the weekly review queue page
 *
 * All the checks the database used to enforce live here (validateSubmission_), since a Sheet
 * cannot refuse a bad row. QBO_operations re-checks rows before drafting anything.
 */

var ALLOWED_MIME = { 'image/jpeg': 'jpg', 'image/png': 'png', 'image/heic': 'heic', 'image/heif': 'heif',
                     'image/webp': 'webp', 'image/gif': 'gif', 'application/pdf': 'pdf' };
var MAX_FILE_BYTES = 10 * 1024 * 1024;
var MAX_FILES = 10;
var UPLOAD_WINDOW_HOURS = 24;
var REVIEW_LINK_DAYS = 14;
var LOCK_MS = 30000;

// ── submit ──────────────────────────────────────────────────────────────────────────────

function submitExpense(data, clientInfo) {
  var L = lookups_(loadConfig_());
  var rec = validateSubmission_(data || {}, L);
  var token = newToken_();
  // Google's front end sometimes drops a response (user, 2026-09-24: "Not JSON (HTTP 404)"), so the
  // form retries. A retry carries the same client_ref: answer with the SAME submission (and a fresh
  // upload token, since only its hash is stored) instead of adding a second row.
  var ref = String((data || {}).client_ref || '').trim();
  if (ref && !/^[A-Za-z0-9-]{8,64}$/.test(ref)) throw new UserError('Bad client_ref');
  var again = null;
  withLock_(function () {
    var prev = ref ? Ledger.where('submissions', 'client_ref', ref)[0] : null;
    if (prev) {
      again = { ok: true, id: Number(prev.id), already_complete: !!prev.completed_at };
      if (!prev.completed_at) {
        Ledger.update('submissions', prev._row, { upload_token_sha256: sha256Hex_(token) });
        again.upload_token = token;
      }
      log_(prev.id, prev.submitter, 'submit_retried', null);
      return;
    }
    rec.client_ref = ref;
    rec.id = Ledger.nextId('submissions');
    rec.created_at = nowIso_();
    rec.status = 'pending';
    rec.file_count = 0;
    rec.upload_token_sha256 = sha256Hex_(token);
    rec.client_info = String(clientInfo || '').slice(0, 200);
    Ledger.append('submissions', rec);
    log_(rec.id, rec.submitter, 'created', null);
  });
  return again || { ok: true, id: rec.id, upload_token: token };
}

function validateSubmission_(d, L) {
  function need(v, what) {
    if (v === undefined || v === null || String(v).trim() === '') throw new UserError(what + ' is required');
    return String(v).trim();
  }
  var kind = need(d.kind, 'Type (expense or deposit)');
  if (kind !== 'expense' && kind !== 'deposit') throw new UserError('Type must be expense or deposit');

  var date = need(d.txn_date, kind === 'expense' ? 'Purchase date' : 'Date');
  if (!isIsoDate(date)) throw new UserError('Date must be YYYY-MM-DD');
  if (date > todayIso_()) throw new UserError('Date cannot be in the future');
  yearRoot(L.layout, date);                                   // a year with no Drive folder fails now, not at upload

  // Each list also takes "Other — type it" (user, 2026-09-24): the form sends <field>_other instead.
  // Typed text that matches a list entry is snapped to it; anything else is kept as typed and listed in
  // typed_in, so the reviewer maps it (and QBO_operations skips the row) before it can be drafted.
  var typed = [];
  var prop = pickOrTyped_(d.property_code, d.property_other, L.properties, ['code', 'name'], 'Property', typed, 'property');
  var submitter = pickOrTyped_(d.submitter, d.submitter_other, L.people, ['name'], 'Your name', typed, 'submitter').name;

  var rec = {
    kind: kind, txn_date: date, property_code: prop.code || '', property_name: prop.name, submitter: submitter,
    amount: parseAmount(need(d.amount, 'Amount')).toFixed(2),
    description: need(d.description, 'Description').slice(0, 1000)
  };

  if (kind === 'expense') {
    var cat = pickOrTyped_(d.category, d.category_other, L.categories, ['code', 'label_en'], 'Category', typed, 'category');
    var acct = L.accounts[need(d.money_account, 'Paid with')];
    if (!acct) throw new UserError('Unknown payment account: ' + d.money_account);
    rec.category = cat.code || cat.name;
    rec.vendor = splitStoreStuff(rec.description).store;     // store_stuff: the store is the QBO vendor
    rec.money_account = acct.code;
    rec.owner_billable = (d.owner_billable === undefined || d.owner_billable === '')
      ? truthy_(cat.owner_billable_default) : truthy_(d.owner_billable);
    if (acct.kind === 'personal') {
      var hasOther = String(d.reimburse_to_other || '').trim() !== '';
      rec.reimburse_to = (d.reimburse_to || hasOther)
        ? pickOrTyped_(d.reimburse_to, d.reimburse_to_other, L.people, ['name'], 'Reimburse to', typed, 'reimburse_to').name
        : submitter;
    }
  } else {
    var dt = L.depositTypes[need(d.deposit_type, 'Fee type')];
    if (!dt) throw new UserError('Unknown fee type: ' + d.deposit_type);
    var method = L.methods[need(d.collection_method, 'Paid by')];
    if (!method) throw new UserError('Unknown payment method: ' + d.collection_method);
    if (!L.accounts[method.lands_in]) throw new Error('collection_methods.csv: ' + method.code +
                                                      ' lands in unknown account ' + method.lands_in);
    rec.deposit_type = dt.code;
    rec.collection_method = method.code;
    rec.money_account = method.lands_in;                      // decided by the method, not the submitter
    rec.payer_name = need(d.payer_name, 'Guest name').slice(0, 100);
    rec.payment_reference = String(d.payment_reference || '').trim().slice(0, 100);
    if (truthy_(dt.per_reservation)) {
      var code = normalizeReservationCode(need(d.reservation_code, 'Confirmation code'));
      if (!/^[A-Z0-9-]+$/.test(code)) throw new UserError('Confirmation code can only have letters, digits and -');
      var checkIn = need(d.check_in_date, 'Check-in date');
      if (!isIsoDate(checkIn)) throw new UserError('Check-in date must be YYYY-MM-DD');
      rec.reservation_code = code;
      rec.check_in_date = checkIn;
    }
  }
  rec.typed_in = typed.join(',');
  return rec;
}

/**
 * A list choice, or free text typed under "Other". Returns the list row, or {name: text} (no code)
 * for text that matches nothing — then `field` is added to `typed`.
 */
function pickOrTyped_(value, other, map, matchCols, what, typed, field) {
  var text = cleanField(other || '').slice(0, 60);
  if (!text) {
    var v = String(value == null ? '' : value).trim();
    if (!v) throw new UserError(what + ' is required');
    var noun = { 'Your name': 'name', 'Reimburse to': 'person to reimburse' }[what] || what.toLowerCase();
    if (!map[v]) throw new UserError('Unknown ' + noun + ': ' + v + ' — choose "Other" to type it');
    return map[v];
  }
  var want = text.toLowerCase();
  for (var k in map) {
    var row = map[k];
    if (matchCols.some(function (c) { return String(row[c] || '').trim().toLowerCase() === want; })) return row;
  }
  typed.push(field);
  return { name: text };
}

// ── files ───────────────────────────────────────────────────────────────────────────────

function addFile(data) {
  data = data || {};
  var mime = String(data.mime_type || '').toLowerCase();
  if (!ALLOWED_MIME[mime]) throw new UserError('Only photos (JPG, PNG, HEIC, WEBP, GIF) and PDFs can be uploaded');
  var bytes;
  try { bytes = Utilities.base64Decode(String(data.base64 || '')); } catch (e) { throw new UserError('The file could not be read'); }
  if (!bytes.length) throw new UserError('The file is empty');
  if (bytes.length > MAX_FILE_BYTES) throw new UserError('The file is larger than 10 MB');

  var L = lookups_(loadConfig_());
  return withLock_(function () {
    var sub = authorizeUpload_(data.id, data.upload_token, true);
    var finished = sub.status !== 'pending' || !!sub.completed_at;
    var all = Ledger.all('attachments');                      // read once: count + duplicate check
    var existing = all.filter(function (a) { return a.submission_id === sub.id; });
    var sha = sha256Hex_(bytes);
    var same = existing.filter(function (a) { return a.sha256 === sha && a.upload_status === 'uploaded'; })[0];
    var last = data.last === true || data.last === 'true';
    if (finished && !same) throw new UserError('This submission is already finished');
    if (same) {                                                // a retry of a file that already arrived
      if (last && !finished) {
        Ledger.update('submissions', sub._row, { completed_at: nowIso_() });
        log_(sub.id, sub.submitter, 'completed', { files: existing.length });
      }
      return { ok: true, seq: Number(same.seq), file_name: same.file_name, url: same.drive_url,
               possible_duplicate_of: [], completed: last, retried: true };
    }
    if (existing.length >= MAX_FILES) throw new UserError('At most ' + MAX_FILES + ' files per submission');
    var seq = existing.length + 1;
    var target = fileTarget_(sub, L, ALLOWED_MIME[mime]);
    var dupOf = all.filter(function (a) { return a.sha256 === sha && a.submission_id !== sub.id; })
      .map(function (a) { return a.submission_id; });

    var taken = namesFiledIn_(all, target.folderPath.join('/'), null);
    var row = Ledger.append('attachments', { submission_id: sub.id, seq: seq, file_name: target.fileName,
      drive_folder_path: target.folderPath.join('/'), upload_status: 'pending', mime_type: mime,
      size_bytes: bytes.length, sha256: sha });

    var saved;
    try {
      saved = saveFile_(target.rootId, target.folderPath, target.fileName, mime, bytes, taken);
    } catch (e) {
      Ledger.update('attachments', row, { upload_status: 'failed', upload_error: String(e && e.message || e).slice(0, 300) });
      log_(sub.id, sub.submitter, 'file_failed', { seq: seq, error: String(e && e.message || e) });
      throw new Error('Drive save failed: ' + (e && e.message || e));
    }
    Ledger.update('attachments', row, { file_name: saved.fileName, drive_file_id: saved.fileId, drive_url: saved.url,
                                        upload_status: 'uploaded', uploaded_at: nowIso_() });
    var patch = { file_count: seq };
    if (seq === 1) patch.first_file_url = saved.url;
    // "last": true on the final file also completes the submission — one request fewer for the form.
    if (last) patch.completed_at = nowIso_();
    Ledger.update('submissions', sub._row, patch);
    log_(sub.id, sub.submitter, 'file_uploaded', { seq: seq, file: saved.fileName, same_file_as: dupOf });
    if (last) log_(sub.id, sub.submitter, 'completed', { files: seq });
    return { ok: true, seq: seq, file_name: saved.fileName, url: saved.url, possible_duplicate_of: dupOf,
             completed: last };
  });
}

/** Name + folder for a submission's file, from the rules in Naming.gs and drive_layout.json. */
function fileTarget_(sub, L, ext) {
  var layout = L.layout, acct = L.accounts[sub.money_account] || {};
  var fileName, label;
  if (sub.kind === 'expense') {
    var cat = L.categories[sub.category] || {};
    label = cat.label_en || sub.category;
    if (acct.kind === 'personal') {
      // Not paid yet: wait in Expense_processing/Pending/ for the weekly reimbursement run (Reimburse.gs),
      // which gives the file its final name (pay date first) and moves it into the bookkeeping folders.
      return { fileName: pendingFileName({ purchaseDate: sub.txn_date, propertyName: sub.property_name,
                 description: sub.description, amount: sub.amount, ext: ext }),
               folderPath: [PENDING_FOLDER], rootId: processingFolder_().getId() };
    }
    // Paid at purchase (card / bank / owner): the pay date IS the purchase date; file it final now.
    fileName = expenseFileName({ payDate: sub.txn_date, purchaseDate: sub.txn_date, propertyName: sub.property_name,
      description: sub.description, amount: sub.amount, ext: ext });
  } else {
    var dt = L.depositTypes[sub.deposit_type] || {}, method = L.methods[sub.collection_method] || {};
    label = dt.label_en || sub.deposit_type;
    fileName = depositFileName({ date: sub.txn_date, propertyName: sub.property_name, payer: sub.payer_name,
      method: method.label || sub.collection_method, depositType: label,
      reservationCode: sub.reservation_code, amount: sub.amount, ext: ext });
  }
  // money_accounts.drive_path (optional) replaces the settings template for that account, e.g.
  // valta-homes -> 'Valta Homes Unprocessed Invoices/Testing' under the year folder (user, 2026-09-24).
  var folderPath = buildFolderPath(acct.drive_path || layout[sub.kind], { date: sub.txn_date,
    account: acct.drive_folder || layout.no_account_folder, propertyName: sub.property_name,
    category: sub.kind === 'expense' ? label : '', depositType: sub.kind === 'deposit' ? label : '',
    submitter: sub.submitter });
  return { fileName: fileName, folderPath: folderPath, rootId: yearRoot(layout, sub.txn_date) };
}

/** Names the ledger has already filed in a folder (path as in attachments.drive_folder_path). */
function namesFiledIn_(attachments, folderPath, exceptRow) {
  var taken = {};
  attachments.forEach(function (a) {
    if (a.drive_folder_path === folderPath && a._row !== exceptRow && a.upload_status !== 'failed') taken[a.file_name] = true;
  });
  return taken;
}

/** allowFinished: a retry may arrive after the submission was completed (checked by the caller). */
function authorizeUpload_(id, token, allowFinished) {
  var sub = findSubmission_(id);
  if (!token || sha256Hex_(String(token)) !== sub.upload_token_sha256) throw new UserError('Not allowed');
  var finished = sub.status !== 'pending' || !!sub.completed_at;
  if (finished && allowFinished) return sub;
  if (finished) throw new UserError('This submission is already finished');
  var ageHours = (Date.now() - new Date(sub.created_at).getTime()) / 36e5;
  if (!(ageHours <= UPLOAD_WINDOW_HOURS)) throw new UserError('This submission is too old to add files; submit again');
  return sub;
}

function findSubmission_(id) {
  var hit = Ledger.where('submissions', 'id', String(parseInt(id, 10)))[0];
  if (!hit) throw new UserError('Submission not found');
  return hit;
}

// ── complete ────────────────────────────────────────────────────────────────────────────

/** "All files sent". No email: reviewers get the weekly Friday summary instead (Mail.gs). */
function completeSubmission(data) {
  data = data || {};
  var sub = withLock_(function () {
    var s = authorizeUpload_(data.id, data.upload_token, true);
    if (s.completed_at) return s;                             // a retried "complete": already done
    if (s.status !== 'pending') throw new UserError('This submission is already finished');
    var files = Ledger.where('attachments', 'submission_id', s.id)
      .filter(function (a) { return a.upload_status === 'uploaded'; });
    if (!files.length) throw new UserError('Attach at least one receipt or screenshot');
    Ledger.update('submissions', s._row, { completed_at: nowIso_() });
    log_(s.id, s.submitter, 'completed', { files: files.length });
    return s;
  });
  return { ok: true, id: sub.id };
}

// ── review ──────────────────────────────────────────────────────────────────────────────
//
// Weekly (user, 2026-09-23: "I don't want to receive approving email every record, send me by the
// noon of Friday"): sendWeeklyDigest emails each reviewer ONE link to the review queue page
// (?action=queue&t=TOKEN). The token is kind 'queue', reusable until it expires, and only shows
// submissions in that reviewer's scope. Single-submission tokens (kind '') are what emails before
// the change carried; decideReview still honours them.

/** Completed, unreviewed submissions this reviewer may decide. */
function reviewQueue_(reviewer) {
  var claimed = claimedAccounts_(loadConfig_().reviewers || []);
  var scopes = reviewer.scopes || [reviewer.scope];
  return Ledger.all('submissions').filter(function (s) {
    return s.status === 'pending' && s.completed_at &&
           scopes.some(function (sc) { return inScope_(sc, s, claimed); });
  });
}

/**
 * Active reviewers by email. One person may have several rows (user, 2026-09-24: billing@ temporarily
 * also on account:valta-homes): they get ONE summary and one link covering every row's scope.
 */
function activeReviewers_(reviewerRows) {
  var by = {}, order = [];
  reviewerRows.forEach(function (r) {
    var email = String(r.email || '').trim().toLowerCase();
    if (!email || !truthy_(r.active)) return;
    if (!by[email]) { by[email] = { email: email, name: r.name, scopes: [] }; order.push(email); }
    by[email].scopes.push(r.scope);
  });
  return order.map(function (e) { return by[e]; });
}

/**
 * Reviewer scope (Expense Config "reviewers" tab):
 *   all                  everything EXCEPT accounts another active reviewer owns with account:<code>
 *   account:<code>       only submissions paid with that money account, e.g. account:valta-homes ->
 *                        contact@valtahomes.com (user, 2026-09-24: Valta Homes is reviewed only there)
 *   <property code>      only that property
 */
function inScope_(scope, s, claimed) {
  scope = String(scope || '').trim();
  var m = /^account:(.+)$/.exec(scope);
  if (m) return s.money_account === m[1].trim();
  if (scope === 'all') return !claimed[s.money_account];
  return scope === s.property_code;
}

function claimedAccounts_(reviewers) {
  var claimed = {};
  reviewers.forEach(function (r) {
    var m = /^account:(.+)$/.exec(String(r.scope || '').trim());
    if (m && truthy_(r.active)) claimed[m[1].trim()] = true;
  });
  return claimed;
}

function queueToken_(token) {
  var tok = findToken_(token);
  if (tok.kind !== 'queue') throw new UserError('This link is not valid');
  var reviewer = activeReviewers_(loadConfig_().reviewers || []).filter(function (r) {
    return r.email === String(tok.reviewer_email).toLowerCase();
  })[0];
  if (!reviewer) throw new UserError(tok.reviewer_email + ' is no longer a reviewer');
  return { tok: tok, reviewer: reviewer };
}

/** What the queue page shows. Opening it changes nothing. */
function queueContext_(token) {
  var q = queueToken_(token);
  var files = {};
  Ledger.all('attachments').forEach(function (a) {
    if (a.upload_status !== 'uploaded') return;
    (files[a.submission_id] = files[a.submission_id] || []).push({ name: a.file_name, url: a.drive_url });
  });
  return { reviewer: q.tok.reviewer_email, items: reviewQueue_(q.reviewer).map(function (s) {
    return { sub: publicSubmission_(s), files: files[s.id] || [] };
  }) };
}

/** From Queue.html: decide one submission, or several (ids). The link stays usable. */
function decideFromQueue(token, ids, action, note) {
  if (action !== 'approve' && action !== 'reject') throw new UserError('Unknown action');
  ids = [].concat(ids).map(String);
  var L = lookups_(loadConfig_());
  var results = withLock_(function () {
    var q = queueToken_(token);
    var allowed = {};
    reviewQueue_(q.reviewer).forEach(function (s) { allowed[s.id] = s; });
    return ids.map(function (id) {
      var sub = allowed[id];
      if (!sub) {
        var cur = Ledger.where('submissions', 'id', id)[0];
        return { id: id, ok: false, error: cur && cur.status !== 'pending'
          ? 'Already ' + cur.status + ' by ' + cur.reviewed_by : 'Not in your review list' };
      }
      applyDecision_(sub, q.tok.reviewer_email, action, note, L);
      return { id: id, ok: true, status: action === 'approve' ? 'approved' : 'rejected' };
    });
  });
  scheduleExport_();
  return { ok: true, results: results };
}

/** What the single-submission page shows (links sent before the weekly summary). */
function reviewContext_(token) {
  var tok = findToken_(token);
  if (tok.kind === 'queue') throw new UserError('This link is not valid');
  var sub = findSubmission_(tok.submission_id);
  var files = Ledger.where('attachments', 'submission_id', sub.id)
    .filter(function (a) { return a.upload_status === 'uploaded'; })
    .map(function (a) { return { name: a.file_name, url: a.drive_url }; });
  return { reviewer: tok.reviewer_email, sub: publicSubmission_(sub), files: files,
           decided: sub.status !== 'pending', status: sub.status, reviewed_by: sub.reviewed_by };
}

/** Called from Review.html via google.script.run (single-submission links). */
function decideReview(token, action, note) {
  if (action !== 'approve' && action !== 'reject') throw new UserError('Unknown action');
  var L = lookups_(loadConfig_());
  var result = withLock_(function () {
    var tok = findToken_(token);
    if (tok.kind === 'queue') throw new UserError('This link is not valid');
    if (tok.used_at) throw new UserError('This link was already used');
    var sub = findSubmission_(tok.submission_id);
    if (sub.status !== 'pending') throw new UserError('Already ' + sub.status + ' by ' + sub.reviewed_by);
    applyDecision_(sub, tok.reviewer_email, action, note, L);
    return { ok: true, status: action === 'approve' ? 'approved' : 'rejected', id: sub.id };
  });
  scheduleExport_();
  return result;
}

/** Record a decision. Call inside the lock with a pending submission row. */
function applyDecision_(sub, reviewerEmail, action, note, L) {
  var status = action === 'approve' ? 'approved' : 'rejected';
  var now = nowIso_();
  Ledger.update('submissions', sub._row, { status: status, reviewed_by: reviewerEmail, reviewed_at: now,
                                           review_note: String(note || '').slice(0, 500) });
  Ledger.where('review_tokens', 'submission_id', sub.id).forEach(function (t) {
    if (!t.used_at) Ledger.update('review_tokens', t._row, { used_at: now });
  });
  log_(sub.id, reviewerEmail, status, note ? { note: String(note).slice(0, 500) } : null);

  // A Zelle / Cash App payment is its own bank-feed line: its bank deposit exists the moment it is approved.
  var method = L.methods[sub.collection_method];
  if (status === 'approved' && sub.kind === 'deposit' && method && truthy_(method.one_per_bank_line)) {
    var bid = Ledger.nextId('bank_deposits');
    Ledger.append('bank_deposits', { id: bid, bank_date: sub.txn_date, money_account: sub.money_account,
      total: sub.amount, reference: 'auto: submission ' + sub.id, created_at: now, created_by: 'system' });
    Ledger.update('submissions', sub._row, { bank_deposit_id: bid });
  }
}

function findToken_(token) {
  if (!token) throw new UserError('This link is not valid');
  var tok = Ledger.where('review_tokens', 'token_sha256', sha256Hex_(String(token)))[0];
  if (!tok) throw new UserError('This link is not valid');
  if (new Date(tok.expires_at).getTime() < Date.now()) throw new UserError('This link has expired; review it in the Expense Ledger sheet');
  return tok;
}

function publicSubmission_(s) {
  var o = {};
  ['id', 'kind', 'status', 'txn_date', 'property_name', 'submitter', 'amount', 'description', 'category', 'vendor',
   'money_account', 'owner_billable', 'reimburse_to', 'deposit_type', 'payer_name', 'reservation_code',
   'check_in_date', 'collection_method', 'payment_reference', 'typed_in'].forEach(function (k) { if (s[k] !== '') o[k] = s[k]; });
  return o;
}

// ── helpers ─────────────────────────────────────────────────────────────────────────────

function withLock_(fn) {
  var lock = LockService.getScriptLock();
  lock.waitLock(LOCK_MS);
  try { return fn(); } finally { lock.releaseLock(); }
}

function newToken_() {
  return (Utilities.getUuid() + Utilities.getUuid()).replace(/-/g, '');
}

function sha256Hex_(value) {
  return Utilities.computeDigest(Utilities.DigestAlgorithm.SHA_256, value)
    .map(function (b) { return ('0' + (b & 0xff).toString(16)).slice(-2); }).join('');
}
