// End-to-end: the real apps_script/*.gs files against in-memory Sheets / Drive / Gmail.
// node --test tests/
const test = require('node:test');
const assert = require('node:assert');
const { createGas } = require('./gas_harness');

const PNG = Buffer.from('fake png bytes').toString('base64');
const PNG2 = Buffer.from('another receipt').toString('base64');

const EXPENSE = { kind: 'expense', txn_date: '2026-09-14', property_code: 'renton_18823', submitter: 'Jing Li',
                  amount: '302.74', description: 'Home Depot_baseboard moulding', category: 'Repairs',
                  money_account: 'chase-9967' };
const DEPOSIT = { kind: 'deposit', txn_date: '2026-09-14', property_code: 'renton_18823', submitter: 'Jing Li',
                  amount: '150', description: 'pet fee', deposit_type: 'Pet-Fee', collection_method: 'zelle',
                  payer_name: 'John Smith', reservation_code: 'hm abc123', check_in_date: '2026-09-12' };

function fresh(opts) {
  const g = createGas(opts);
  g.ctx.setup();
  return g;
}
function submitWithFile(g, data, b64 = PNG) {
  const s = g.post({ action: 'submit', data });
  assert.ok(s.ok, JSON.stringify(s));
  const f = g.post({ action: 'file', data: { id: s.id, upload_token: s.upload_token, mime_type: 'image/png', base64: b64 } });
  assert.ok(f.ok, JSON.stringify(f));
  return { s, f };
}
const plain = (x) => JSON.parse(JSON.stringify(x));      // values made inside the vm sandbox -> host objects
// The Friday summary: send it, then pull the queue token out of the reviewer's email.
function digestToken(g, to = 'billing@valtarealty.com') {
  g.ctx.sendWeeklyDigest();
  const mail = g.outbox.filter((m) => m.to === to).pop();
  assert.ok(mail, 'no summary email to ' + to);
  return new URL(mail.htmlBody.match(/href="([^"]+action=queue[^"]*)"/)[1].replace(/&amp;/g, '&')).searchParams.get('t');
}
// Decisions only schedule the export (one-off trigger); run it the way the trigger would.
function runScheduledExport(g) {
  assert.strictEqual(g.triggers.filter((t) => t.getHandlerFunction() === 'exportLedgerSoon').length, 1, 'one export scheduled');
  g.ctx.exportLedgerSoon();
  assert.ok(!g.triggers.some((t) => t.getHandlerFunction() === 'exportLedgerSoon'), 'one-off trigger removed');
}
const approve = (g, ids) => plain(g.ctx.decideFromQueue(digestToken(g), ids, 'approve', ''));
const complete = (g, s) => assert.ok(g.post({ action: 'complete', data: { id: s.id, upload_token: s.upload_token } }).ok);

test('setup creates the ledger tabs and folders; idempotent', () => {
  const g = fresh();
  const url1 = g.ctx.setup();
  assert.ok(url1.includes('spreadsheets'));
  for (const tab of ['submissions', 'attachments', 'bank_deposits', 'zelle_payouts', 'review_tokens', 'log']) {
    assert.strictEqual(g.tab(tab).length, 0, tab);
  }
  const names = [...g.nodes.values()].filter((n) => n.type === 'folder').map((n) => n.name);
  assert.ok(names.includes('exports') && names.includes('config'));
});

test('config for the form: active lists only, nothing sensitive', () => {
  const g = fresh();
  const c = JSON.parse(g.get({ action: 'config' }).content);
  assert.ok(c.ok);
  assert.strictEqual(c.properties.length, 131);
  assert.ok(c.properties.some((p) => p.name === 'Renton 18823'));
  assert.ok(!c.properties.some((p) => p.name === 'Seattle 906'));          // excluded by the user
  assert.ok(c.people.includes('Jing Li'));
  assert.ok(!JSON.stringify(c).includes('@'), 'no emails leak to the public form');
  assert.deepStrictEqual(c.deposit_types.map((d) => d.code).sort(), ['Parking-Fee', 'Pet-Fee']);
  assert.strictEqual(c.default_payment_account, 'personal', 'Paid personally is the default');
  assert.strictEqual(c.payment_accounts[0].code, 'personal', 'and listed first');
});

test('expense: submit, file named + filed by date and account, row tracked', () => {
  const g = fresh();
  const { s, f } = submitWithFile(g, EXPENSE);
  assert.strictEqual(f.file_name, '20260914_Renton 18823_9.14_Home Depot_baseboard moulding_302.74.png',
                     'paid by bank at purchase: pay date = purchase date');
  assert.deepStrictEqual(g.filesUnder('2026/'), ['2026/2026-09/9967/' + f.file_name]);
  const row = g.tab('submissions')[0];
  assert.strictEqual(row.id, String(s.id));
  assert.strictEqual(row.status, 'pending');
  assert.strictEqual(row.amount, '302.74');
  assert.strictEqual(row.owner_billable, 'TRUE');                          // category default
  assert.strictEqual(row.vendor, 'Home Depot', 'store from store_stuff, for the QBO vendor');
  assert.strictEqual(row.file_count, '1');
  assert.strictEqual(g.tab('attachments')[0].upload_status, 'uploaded');
  assert.notStrictEqual(row.upload_token_sha256, s.upload_token, 'only the hash is stored');
});

test('second receipt gets -2; same bytes on another submission flagged', () => {
  const g = fresh();
  const { s } = submitWithFile(g, EXPENSE);
  const f2 = g.post({ action: 'file', data: { id: s.id, upload_token: s.upload_token, mime_type: 'image/png', base64: PNG2 } });
  assert.ok(f2.file_name.endsWith('_302.74-2.png'));
  const other = submitWithFile(g, { ...EXPENSE, amount: '10' });           // same PNG bytes as the first
  assert.deepStrictEqual(other.f.possible_duplicate_of, [String(s.id)]);
});

test('deposit: code normalised, lands in 9967 regardless of input, name carries the code', () => {
  const g = fresh();
  const { f } = submitWithFile(g, { ...DEPOSIT, money_account: 'credit-7439' });
  assert.strictEqual(f.file_name, '20260914_Renton 18823_John Smith_Zelle_Pet fee_HMABC123_-150.png');
  assert.deepStrictEqual(g.filesUnder('2026/'), ['2026/2026-09/9967/' + f.file_name]);
  const row = g.tab('submissions')[0];
  assert.strictEqual(row.money_account, 'chase-9967');
  assert.strictEqual(row.reservation_code, 'HMABC123');
});

test('validation: every rule the database used to enforce', () => {
  const g = fresh();
  const bad = [
    [{ ...EXPENSE, property_code: 'seattle_906' }, /Unknown property/],     // excluded property
    [{ ...EXPENSE, submitter: 'Nobody' }, /Unknown name/],
    [{ ...EXPENSE, txn_date: '2099-01-01' }, /future/],
    [{ ...EXPENSE, txn_date: '2025-06-01' }, /No Drive folder .* 2025/],
    [{ ...EXPENSE, amount: '12.345' }, /Invalid amount/],
    [{ ...EXPENSE, amount: '0' }, /Invalid amount/],
    [{ ...EXPENSE, description: 'baseboard moulding' }, /store_stuff/],
    [{ ...EXPENSE, description: 'Home Depot_' }, /store_stuff/],
    [{ ...EXPENSE, txn_date: '' }, /Purchase date is required/],
    [{ ...EXPENSE, category: 'Nope' }, /Unknown category/],
    [{ ...EXPENSE, money_account: 'personal', reimburse_to: 'Nobody' }, /Unknown person to reimburse/],
    [{ ...DEPOSIT, reservation_code: '' }, /Confirmation code is required/],
    [{ ...DEPOSIT, check_in_date: '' }, /Check-in date is required/],
    [{ ...DEPOSIT, reservation_code: 'HM#1' }, /letters, digits/],
    [{ ...DEPOSIT, collection_method: 'venmo' }, /Unknown payment method/],
    [{ ...DEPOSIT, payer_name: '' }, /Guest name is required/],
    [{ ...EXPENSE, kind: 'refund' }, /expense or deposit/],
  ];
  for (const [data, re] of bad) {
    const r = g.post({ action: 'submit', data });
    assert.strictEqual(r.ok, false, JSON.stringify(data));
    assert.match(r.error, re);
  }
  assert.strictEqual(g.tab('submissions').length, 0, 'nothing written for a refused submission');
});

test('"Other — type it": unknown name/property/category kept, flagged, shown to the reviewer', () => {
  const g = fresh();
  const { s, f } = submitWithFile(g, { ...EXPENSE, submitter: '', submitter_other: 'New Cleaner',
    property_code: '', property_other: 'Tacoma_99 New', category: '', category_other: 'Pest control',
    money_account: 'personal', txn_date: '2026-09-10' });
  const row = g.tab('submissions')[0];
  assert.strictEqual(row.submitter, 'New Cleaner');
  assert.strictEqual(row.reimburse_to, 'New Cleaner', 'a typed name is also the default payee');
  assert.strictEqual(row.property_code, '');
  assert.strictEqual(row.property_name, 'Tacoma 99 New', 'no _ inside a filename field');
  assert.strictEqual(row.category, 'Pest control');
  assert.strictEqual(row.typed_in, 'property,submitter,category');
  assert.strictEqual(f.file_name, 'PENDING_Tacoma 99 New_9.10_Home Depot_baseboard moulding_302.74.png');
  complete(g, s);
  g.ctx.sendWeeklyDigest();
  assert.match(g.outbox.pop().htmlBody, /typed in, not in list: property,submitter,category/);
});

test('"Other" text matching a list entry snaps to it (not flagged); blank or unknown choices refused', () => {
  const g = fresh();
  submitWithFile(g, { ...EXPENSE, submitter: '', submitter_other: ' jing li ', property_code: '',
    property_other: 'renton 18823', category: '', category_other: 'repairs' });
  const row = g.tab('submissions')[0];
  assert.deepStrictEqual([row.submitter, row.property_code, row.category, row.typed_in],
                         ['Jing Li', 'renton_18823', 'Repairs', '']);
  for (const [bad, msg] of [[{ submitter: '', submitter_other: '  ' }, /Your name is required/],
                            [{ submitter: 'Nobody' }, /choose "Other"/],
                            [{ category: 'Nope' }, /choose "Other"/]]) {
    assert.match(g.post({ action: 'submit', data: { ...EXPENSE, ...bad } }).error, msg);
  }
});

test('personal expense: waits in Pending/, reimburse_to defaults to the submitter', () => {
  const g = fresh();
  const { f } = submitWithFile(g, { ...EXPENSE, money_account: 'personal', txn_date: '2026-09-10' });
  assert.strictEqual(g.tab('submissions')[0].reimburse_to, 'Jing Li');
  assert.strictEqual(f.file_name, 'PENDING_Renton 18823_9.10_Home Depot_baseboard moulding_302.74.png');
  assert.deepStrictEqual(g.filesUnder('Expense_processing/Pending/'), ['Expense_processing/Pending/' + f.file_name]);
  assert.strictEqual(g.filesUnder('2026/').length, 0, 'nothing in the bookkeeping folders until reimbursed');
});

test('weekly reimbursement: per-person Zelle totals, receipts renamed to the pay date and moved', () => {
  const g = fresh();
  const key = g.props.get('ADMIN_KEY');
  const people = [['Jing Li', '2026-09-10', '302.74'], ['Jing Li', '2026-09-12', '10.00'],
                  ['Griselda Ramirez', '2026-09-11', '150'], ['Jing Li', '2026-09-13', '5']];
  people.forEach(([who, date, amount], i) => {
    const { s } = submitWithFile(g, { ...EXPENSE, submitter: who, money_account: 'personal', txn_date: date, amount,
                                       description: 'Home Depot_item ' + i }, Buffer.from('r' + i).toString('base64'));
    g.post({ action: 'complete', data: { id: s.id, upload_token: s.upload_token } });
  });
  approve(g, ['1', '2', '3']);                                                      // #4 stays unreviewed

  const page = g.get({ action: 'reimburse', k: key }).html;
  assert.ok(page.includes('$312.74') && page.includes('$150.00') && page.includes('Camila - service'));
  assert.ok(page.includes('1 more still waiting for review'));
  assert.ok(g.get({ action: 'reimburse', k: 'wrong' }).html.includes('Not allowed'));
  assert.throws(() => g.ctx.processReimbursements('wrong', '2026-09-14'), /Not allowed/);
  assert.throws(() => g.ctx.processReimbursements(key, '2099-01-01'), /future/);

  const r = g.ctx.processReimbursements(key, '2026-09-14');
  assert.deepStrictEqual(plain(r.payouts.map((p) => [p.person, p.total, p.count])),
                         [['Griselda Ramirez', '150.00', 1], ['Jing Li', '312.74', 2]]);
  assert.deepStrictEqual(g.filesUnder('2026/').sort(), [
    '2026/2026-09/9967/20260914_Renton 18823_9.10_Home Depot_item 0_302.74_312.74.png',
    '2026/2026-09/9967/20260914_Renton 18823_9.11_Home Depot_item 2_150_150.png',
    '2026/2026-09/9967/20260914_Renton 18823_9.12_Home Depot_item 1_10_312.74.png']);
  assert.strictEqual(g.filesUnder('Expense_processing/Pending/').length, 1, 'the unreviewed one is still pending');
  const zp = g.tab('zelle_payouts');
  assert.deepStrictEqual(plain(zp.map((p) => [p.payee, p.total, p.paid_from, p.zelle_reference])),
    [['Camila - service', '150.00', 'chase-9967', '20260914_Camila - service_150'],
     ['Jing Li', '312.74', 'chase-9967', '20260914_Jing Li_312.74']]);
  const att = g.tab('attachments').find((a) => a.submission_id === '1');
  assert.strictEqual(att.drive_folder_path, '2026-09/9967');
  assert.ok(att.file_name.startsWith('20260914_'));

  runScheduledExport(g);
  const x = g.csvAt('zelle_expense2qbo.csv');
  assert.deepStrictEqual(x[0], ['Date', 'Pay', 'Zelle content', 'JE Amount', 'Amount', 'Property', 'Category',
                                'qbo_account', 'filename', 'submission_id', 'Paid from', 'typed_in']);
  assert.strictEqual(x.length, 4, 'header + 3 reimbursed receipts');
  assert.strictEqual(x[1][x[0].indexOf('Paid from')], 'Chase Trust Checking 9967 - STR', 'QBO bank name, not the code');
  assert.throws(() => g.ctx.processReimbursements(key, '2026-09-14'), /Nothing approved/);
});

test('Drive lagging behind: identical receipts still get distinct names (seen live 2026-09-24)', () => {
  const g = createGas({ driveLag: true });
  g.ctx.setup();
  const key = g.props.get('ADMIN_KEY');
  const P = { ...EXPENSE, money_account: 'personal', txn_date: '2026-09-21', description: 'Costco_Supplies', amount: '75.25' };
  const a = submitWithFile(g, P), b = submitWithFile(g, P, PNG2);
  assert.strictEqual(a.f.file_name, 'PENDING_Renton 18823_9.21_Costco_Supplies_75.25.png');
  assert.strictEqual(b.f.file_name, 'PENDING_Renton 18823_9.21_Costco_Supplies_75.25-2.png', 'ledger knew the first name');
  complete(g, a.s); complete(g, b.s);
  approve(g, [a.s.id, b.s.id]);
  g.ctx.processReimbursements(key, '2026-09-24');
  const names = g.filesUnder('2026/').sort();
  assert.deepStrictEqual(names, ['2026/2026-09/9967/20260924_Renton 18823_9.21_Costco_Supplies_75.25_150.5-2.png',
                                 '2026/2026-09/9967/20260924_Renton 18823_9.21_Costco_Supplies_75.25_150.5.png']);
  const monthFolders = [...g.nodes.values()].filter((n) => n.type === 'folder' && n.name === '2026-09');
  assert.strictEqual(monthFolders.length, 1, 'no duplicate month folder even though lookups lag');
});

test('a row update keeps the other cells as they were (TRUE stays TRUE)', () => {
  const g = fresh();
  const { s } = submitWithFile(g, EXPENSE);
  complete(g, s);
  approve(g, [s.id]);
  const row = g.tab('submissions')[0];
  assert.strictEqual(row.owner_billable, 'TRUE');
  assert.strictEqual(row.amount, '302.74');
  assert.strictEqual(row.txn_date, '2026-09-14');
});

test('owner-paid (no account folder) goes to the fallback folder', () => {
  const g = fresh();
  const { f } = submitWithFile(g, { ...EXPENSE, money_account: 'owner-paid' });
  assert.deepStrictEqual(g.filesUnder('2026/'), ['2026/2026-09/Owner paid/' + f.file_name]);
  assert.ok(f.file_name.startsWith('20260914_Renton 18823_9.14_'), 'owner-paid: pay date = purchase date');
});

test('Paid by Valta Homes: filed under the year/Valta Homes Unprocessed Invoices/Testing; only contact@valtahomes.com reviews it', () => {
  const g = fresh();
  const { s, f } = submitWithFile(g, { ...EXPENSE, money_account: 'valta-homes', txn_date: '2026-09-21', description: 'Costco_Supplies', amount: '75.25' });
  assert.strictEqual(f.file_name, '20260921_Renton 18823_9.21_Costco_Supplies_75.25.png', 'paid at purchase: final name now');
  assert.deepStrictEqual(g.filesUnder('2026/'), ['2026/Valta Homes Unprocessed Invoices/Testing/' + f.file_name]);
  complete(g, s);
  const other = submitWithFile(g, EXPENSE).s;           // a normal 9967 expense
  complete(g, other);

  g.ctx.sendWeeklyDigest();
  const byTo = Object.fromEntries(g.outbox.map((m) => [m.to, m.subject]));
  assert.match(byTo['contact@valtahomes.com'], /1 waiting · \$75\.25/);
  assert.match(byTo['billing@valtarealty.com'], /1 waiting · \$302\.74/, 'billing@ does not see Valta Homes');

  const billing = digestToken(g, 'billing@valtarealty.com');
  assert.match(plain(g.ctx.decideFromQueue(billing, [s.id], 'approve', '')).results[0].error, /Not in your review list/);
  const vh = digestToken(g, 'contact@valtahomes.com');
  assert.ok(plain(g.ctx.decideFromQueue(vh, [s.id], 'approve', '')).results[0].ok);
  assert.strictEqual(g.tab('submissions')[0].reviewed_by, 'contact@valtahomes.com');
});

test('fixConfig20260924 adds Valta Homes to a live config seeded before it existed; idempotent', () => {
  const oldAccounts = 'code,label,qb_account,kind,drive_folder,in_qbo\nchase-9967,Chase 9967,,bank,9967,TRUE\npersonal,Paid personally,,personal,9967,TRUE\n';
  const g = fresh({ configOverrides: { 'money_accounts.csv': oldAccounts,
                                       'reviewers.csv': 'email,name,scope,active\nbilling@valtarealty.com,Billing,all,TRUE\n' } });
  assert.ok(!JSON.parse(g.get({ action: 'config' }).content).payment_accounts.some((a) => a.code === 'valta-homes'));
  assert.strictEqual(g.ctx.fixConfig20260924().length, 3);
  assert.strictEqual(g.ctx.fixConfig20260924().length, 0, 'second run changes nothing');
  const c = JSON.parse(g.get({ action: 'config' }).content);
  assert.deepStrictEqual(c.payment_accounts.find((a) => a.code === 'valta-homes'), { code: 'valta-homes', label: 'Paid by Valta Homes', kind: 'company' });
  const { f } = submitWithFile(g, { ...EXPENSE, money_account: 'valta-homes' });
  assert.ok(g.filesUnder('2026/Valta Homes Unprocessed Invoices/Testing/').length === 1, f.file_name);
});

test('"last": true on the final file also completes the submission (one request fewer)', () => {
  const g = fresh();
  const s = g.post({ action: 'submit', data: EXPENSE });
  const f = g.post({ action: 'file', data: { id: s.id, upload_token: s.upload_token, mime_type: 'image/png', base64: PNG, last: true } });
  assert.ok(f.ok && f.completed);
  assert.ok(g.tab('submissions')[0].completed_at);
  assert.match(g.post({ action: 'file', data: { id: s.id, upload_token: s.upload_token, mime_type: 'image/png', base64: PNG2 } }).error,
               /already finished/);
});

test('file upload guarded by the token, type, and state', () => {
  const g = fresh();
  const s = g.post({ action: 'submit', data: EXPENSE });
  const up = (d) => g.post({ action: 'file', data: { id: s.id, upload_token: s.upload_token, mime_type: 'image/png', base64: PNG, ...d } });
  assert.match(up({ upload_token: 'wrong' }).error, /Not allowed/);
  assert.match(up({ mime_type: 'text/html' }).error, /Only photos/);
  assert.match(up({ base64: '' }).error, /empty/);
  assert.match(up({ id: 999 }).error, /not found/);
  assert.match(g.post({ action: 'complete', data: { id: s.id, upload_token: s.upload_token } }).error, /at least one/);
  assert.ok(up({}).ok);
  assert.ok(g.post({ action: 'complete', data: { id: s.id, upload_token: s.upload_token } }).ok);
  assert.match(up({ base64: PNG2 }).error, /already finished/, 'a NEW file after finishing is refused');
  assert.ok(up({}).retried, 'the same file again is a harmless retry');
});

test('retries after a dropped response: same submission, same file, no duplicates', () => {
  const g = fresh();
  const data = { ...EXPENSE, client_ref: 'r-abc12345' };
  const s1 = g.post({ action: 'submit', data });
  const s2 = g.post({ action: 'submit', data });           // the form never saw s1's reply
  assert.strictEqual(s2.id, s1.id);
  assert.strictEqual(g.tab('submissions').length, 1, 'one row');
  assert.match(g.post({ action: 'file', data: { id: s1.id, upload_token: s1.upload_token, mime_type: 'image/png', base64: PNG } }).error,
               /Not allowed/, 'the lost token no longer works');
  const file = (last) => g.post({ action: 'file', data: { id: s2.id, upload_token: s2.upload_token, mime_type: 'image/png', base64: PNG, last } });
  const f1 = file(true);
  const f2 = file(true);                                     // retried last file after it completed
  assert.ok(f1.ok && f1.completed && !f1.retried);
  assert.ok(f2.ok && f2.retried && f2.file_name === f1.file_name);
  assert.strictEqual(g.tab('attachments').length, 1, 'one file');
  assert.strictEqual(g.filesUnder('2026/').length, 1);
  assert.ok(g.post({ action: 'complete', data: { id: s2.id, upload_token: s2.upload_token } }).ok, 'retried complete is ok');
  const s3 = g.post({ action: 'submit', data });              // retried after everything finished
  assert.ok(s3.ok && s3.already_complete && !s3.upload_token && s3.id === s1.id);
  assert.match(g.post({ action: 'submit', data: { ...EXPENSE, client_ref: 'bad ref!' } }).error, /client_ref/);
});

test('complete sends NO email; the Friday summary is the only review email', () => {
  const g = fresh();
  const { s } = submitWithFile(g, EXPENSE);
  complete(g, s);
  assert.strictEqual(g.outbox.length, 0, 'nothing per submission');
  assert.strictEqual(g.csvAt('submissions.csv'), null, 'no export on submit: cleaners should not wait for it');
  submitWithFile(g, { ...EXPENSE, amount: '10' }, PNG2);          // never completed: not in the summary

  g.ctx.sendWeeklyDigest();
  assert.strictEqual(g.outbox.length, 1);
  const m = g.outbox[0];
  assert.strictEqual(m.to, 'billing@valtarealty.com');
  assert.strictEqual(m.subject, '[Expense review] 1 waiting · $302.74');
  assert.ok(m.htmlBody.includes('action=queue') && m.htmlBody.includes('Home Depot_baseboard moulding'));
  assert.strictEqual(m.name, 'Valta Expense Form');                 // what the Gmail filter matches on
  assert.ok(m.htmlBody.includes('Sent by Valta Expense Form'));
  assert.strictEqual(g.tab('review_tokens')[0].kind, 'queue');
});

test('Friday summary: nothing waiting -> no email; reviewer scope respected', () => {
  const g = fresh({ configOverrides: { 'reviewers.csv':
    'email,name,scope,active\nall@valtarealty.com,A,all,TRUE\nosbr@valtarealty.com,O,osbr,TRUE\nold@valtarealty.com,X,all,FALSE\n' } });
  plain(g.ctx.sendWeeklyDigest());
  assert.strictEqual(g.outbox.length, 0, 'nothing waiting: no email');
  const { s } = submitWithFile(g, EXPENSE);                          // Renton, not OSBR
  complete(g, s);
  g.ctx.sendWeeklyDigest();
  assert.deepStrictEqual(g.outbox.map((m) => m.to), ['all@valtarealty.com'], 'OSBR-only and inactive reviewers skipped');
});

test('review queue: opening changes nothing; approve/reject per item; link reusable; approve all', () => {
  const g = fresh();
  for (const amount of ['1', '2', '3']) {
    const { s } = submitWithFile(g, { ...EXPENSE, amount }, Buffer.from(amount).toString('base64'));
    complete(g, s);
  }
  const token = digestToken(g);
  const page = g.get({ action: 'queue', t: token }).html;
  assert.ok(page.includes('Waiting for review: 3') && page.includes('Approve all 3'));
  assert.ok(g.tab('submissions').every((r) => r.status === 'pending'), 'opening changes nothing');

  let r = plain(g.ctx.decideFromQueue(token, ['1'], 'approve', 'ok'));
  assert.deepStrictEqual(r.results, [{ id: '1', ok: true, status: 'approved' }]);
  r = plain(g.ctx.decideFromQueue(token, ['2'], 'reject', 'wrong property'));
  assert.strictEqual(g.tab('submissions')[1].review_note, 'wrong property');
  r = plain(g.ctx.decideFromQueue(token, ['1', '3'], 'approve', ''));   // same link, again
  assert.deepStrictEqual(r.results.map((x) => x.ok), [false, true]);
  assert.match(r.results[0].error, /Already approved by billing@/);
  const rows = g.tab('submissions');
  assert.deepStrictEqual(plain(rows.map((x) => [x.status, x.reviewed_by])),
    [['approved', 'billing@valtarealty.com'], ['rejected', 'billing@valtarealty.com'], ['approved', 'billing@valtarealty.com']]);
  assert.ok(g.get({ action: 'queue', t: token }).html.includes('Nothing is waiting'));
  assert.ok(g.get({ action: 'queue', t: 'garbage' }).html.includes('not valid'));
  assert.throws(() => g.ctx.decideFromQueue('garbage', ['1'], 'approve'), /not valid/);

  assert.strictEqual(g.csvAt('submissions.csv'), null, 'the click did not wait for the export');
  runScheduledExport(g);
  for (const t of ['submissions', 'attachments', 'bank_deposits', 'zelle_payouts', 'zelle_expense2qbo'])
    assert.ok(g.csvAt(t + '.csv'), t + '.csv exported');
  const [head, first] = g.csvAt('submissions.csv');
  assert.ok(!head.includes('upload_token_sha256'), 'token hash never exported');
  assert.ok(!g.csvAt('review_tokens.csv'), 'review tokens never exported');
  assert.strictEqual(first[head.indexOf('status')], 'approved');
  assert.match(first[head.indexOf('amount')], /^\d+\.\d{2}$/, 'money as 0.00');
  assert.ok(g.csvAt('config/people.csv').length > 30 && g.csvAt('config/settings.csv').length === 5, 'config exported too');
});

test('a queue link stops working when its reviewer is deactivated', () => {
  const g = fresh();
  const { s } = submitWithFile(g, EXPENSE);
  complete(g, s);
  const token = digestToken(g);
  g.configSheet().getSheetByName('reviewers').getRange(2, 4).setValue('FALSE');
  g.clearCache();
  assert.throws(() => g.ctx.decideFromQueue(token, ['1'], 'approve'), /no longer a reviewer/);
});

test('single-submission links sent before the change still work once', () => {
  const g = fresh();
  const { s } = submitWithFile(g, EXPENSE);
  complete(g, s);
  const token = 'legacy-token';
  g.ctx.Ledger.append('review_tokens', { token_sha256: g.ctx.sha256Hex_(token), submission_id: '1',
    reviewer_email: 'vacation@valtarealty.com', expires_at: new Date(Date.now() + 864e5).toISOString() });
  assert.ok(g.get({ action: 'review', t: token }).html.includes('Submission #1'));
  assert.deepStrictEqual({ ...g.ctx.decideReview(token, 'approve', '') }, { ok: true, status: 'approved', id: '1' });
  assert.throws(() => g.ctx.decideReview(token, 'reject'), /already used/);
  const next = submitWithFile(g, { ...EXPENSE, amount: '5' }, PNG2).s;
  complete(g, next);
  assert.throws(() => g.ctx.decideReview(digestToken(g), 'approve'), /not valid/, 'a queue link is not a single link');
});

test('approving a Zelle deposit creates its bank deposit; cash does not', () => {
  const g = fresh();
  for (const method of ['zelle', 'cash']) {
    const { s } = submitWithFile(g, { ...DEPOSIT, collection_method: method }, Buffer.from(method).toString('base64'));
    complete(g, s);
  }
  approve(g, ['1', '2']);
  const [zelle, cash] = g.tab('submissions');
  assert.strictEqual(zelle.bank_deposit_id, '1');
  assert.strictEqual(cash.bank_deposit_id, '');
  const bd = g.tab('bank_deposits');
  assert.strictEqual(bd.length, 1);
  assert.deepStrictEqual([bd[0].bank_date, bd[0].money_account, bd[0].total], ['2026-09-14', 'chase-9967', '150.00']);
});

test('export rewrites the same CSV files in place; old xlsx exports go to the trash', () => {
  const g = fresh();
  g.addExportFile('Expense Ledger export.xlsx', 'old');
  for (let i = 0; i < 2; i++) {
    const { s } = submitWithFile(g, { ...EXPENSE, amount: String(10 + i) }, Buffer.from('r' + i).toString('base64'));
    g.post({ action: 'complete', data: { id: s.id, upload_token: s.upload_token } });
  }
  approve(g, ['1']);
  runScheduledExport(g);
  const id = g.exportFileId('submissions.csv');
  approve(g, ['2']);
  runScheduledExport(g);
  assert.strictEqual(g.exportFileId('submissions.csv'), id, 'same Drive file, updated in place');
  assert.strictEqual(g.csvAt('submissions.csv').length, 3, 'header + 2 rows');
  assert.strictEqual(g.liveFiles('Expense_processing/exports/Expense Ledger export.xlsx').length, 0, 'old xlsx trashed');
  assert.strictEqual(g.trashedSpreadsheets(), 0, 'no temporary sheets any more');
});

test('Expense Config sheet: seeded once from the CSVs, then the sheet is the source', () => {
  const g = fresh();
  const ss = g.configSheet();
  const tabs = ss.getSheets().map((s) => s.getName());
  for (const t of ['properties', 'people', 'expense_categories', 'deposit_types', 'money_accounts',
                   'collection_methods', 'reviewers', 'year_roots', 'settings']) assert.ok(tabs.includes(t), t);
  assert.ok(!tabs.includes('Sheet1'));
  assert.strictEqual(ss.getSheetByName('properties').getLastRow(), 132, 'header + 131');
  assert.deepStrictEqual([...ss.getSheetByName('year_roots').getDataRange().getDisplayValues()[1]],
                         ['2026', '1Hz-76uPBm6fEvggje8ekdYcDfKisj_Ju']);

  // edit the reviewers tab in the "browser", then run setup again: the edit survives and is used
  ss.getSheetByName('reviewers').getRange(2, 1).setValue('boss@valtarealty.com');
  g.ctx.setup();
  g.clearCache();
  assert.strictEqual(ss.getSheetByName('reviewers').getDataRange().getDisplayValues()[1][0], 'boss@valtarealty.com');
  const { s } = submitWithFile(g, EXPENSE);
  complete(g, s);
  g.ctx.sendWeeklyDigest();
  assert.ok(g.outbox.some((m) => m.to === 'boss@valtarealty.com'));
  assert.deepStrictEqual(plain(g.triggers.map((t) => [t.getHandlerFunction(), t.spec])).sort(),
    [['exportLedger', { everyDays: 1, hour: 2 }], ['sendWeeklyDigest', { weekDay: 'FRIDAY', hour: 12, minute: 0 }]],
    'daily export + Friday-noon summary, each once even after setup twice');
});

test('a newer properties.csv in Drive replaces the properties tab; older or empty ones do not', () => {
  const g = fresh();
  const file = g.configFile('properties.csv');
  const tab = () => g.configSheet().getSheetByName('properties');
  file.text = 'code,name,qbo_class_id,qbo_class_name,term,active\nnew_one,New One 1,99,Listings:New One 1,STR,TRUE\n';
  g.clearCache();
  g.get({ action: 'config' });
  assert.strictEqual(tab().getLastRow(), 132, 'same timestamp: not imported');
  file.updated = Date.now();
  g.clearCache();
  const c = JSON.parse(g.get({ action: 'config' }).content);
  assert.deepStrictEqual(c.properties.map((p) => p.name), ['New One 1']);
  file.text = 'code,name\n'; file.updated = Date.now() + 1000;
  g.clearCache();
  assert.strictEqual(JSON.parse(g.get({ action: 'config' }).content).properties.length, 1, 'empty file ignored');
});

test('a trashed config file is ignored in favour of the live one', () => {
  const g = createGas();
  const cfg = [...g.nodes.values()].find((n) => n.name === 'reviewers.csv');
  cfg.trashed = true;
  g.nodes.set('newcfg', { id: 'newcfg', name: 'reviewers.csv', type: 'file', parent: cfg.parent,
                          text: 'email,name,scope,active\nnew@valtarealty.com,New,all,TRUE\n' });
  g.ctx.setup();
  const { s } = submitWithFile(g, EXPENSE);
  complete(g, s);
  g.ctx.sendWeeklyDigest();
  assert.ok(g.outbox.some((m) => m.to === 'new@valtarealty.com'));
  assert.ok(!g.outbox.some((m) => m.to === 'billing@valtarealty.com'));
});

test('fixConfig20260923: reviewer -> billing@, reimburse_from added, Maintainence dropped; idempotent', () => {
  const reviewers = 'email,name,scope,active\nvacation@valtarealty.com,Owner,all,TRUE\n';
  const g = fresh({ configOverrides: { 'reviewers.csv': reviewers } });
  const ss = g.configSheet();
  ss.getSheetByName('settings').deleteRow(5);                  // as live: no reimburse_from row
  const done = plain(g.ctx.fixConfig20260923());
  assert.strictEqual(done.length, 3, done.join('; '));
  assert.deepStrictEqual(plain(ss.getSheetByName('reviewers').getDataRange().getDisplayValues()[1]),
                         ['billing@valtarealty.com', 'Billing', 'all', 'TRUE']);
  const c = JSON.parse(g.get({ action: 'config' }).content);
  assert.ok(!c.expense_categories.some((x) => x.code === 'Maintainence'));
  assert.ok(c.expense_categories.some((x) => x.code === 'Maintenance'));
  assert.strictEqual(plain(g.ctx.fixConfig20260923()).length, 0, 'second run changes nothing');
});

test('mail failure is logged and never loses the submission', () => {
  const g = fresh({ mailFails: true });
  const { s } = submitWithFile(g, EXPENSE);
  complete(g, s);
  g.ctx.sendWeeklyDigest();
  assert.ok(g.tab('log').some((l) => l.action === 'mail_failed'));
  assert.strictEqual(g.tab('submissions')[0].status, 'pending');
});

test('router: bad JSON, unknown action, internal errors hidden', () => {
  const g = fresh();
  assert.match(g.post('not json').error, /must be JSON/);
  assert.match(g.post({ action: 'drop' }).error, /Unknown action/);
  g.props.delete('LEDGER_SHEET_ID'); g.ctx.Ledger.ss_ = null;
  const r = g.post({ action: 'submit', data: EXPENSE });
  assert.strictEqual(r.error, 'Something went wrong on the server. Please try again, or tell the office.');
});

test('Admin.gs: every hand-run function in one place, and each runs', () => {
  const g = fresh();
  for (const fn of ['showReimbursementLink', 'sendReviewSummaryNow', 'exportCsvNow', 'runSetup', 'applyConfigUpdates'])
    assert.strictEqual(typeof g.ctx[fn], 'function', fn);
  assert.match(g.ctx.showReimbursementLink(), /action=reimburse&k=/);
  g.ctx.exportCsvNow();
  assert.ok(g.csvAt('submissions.csv'));
  g.ctx.applyConfigUpdates();
  assert.deepStrictEqual(plain(g.ctx.applyConfigUpdates()), [], 'a second run changes nothing');
  g.ctx.runSetup();
});

test('clearTestData: empties the ledger, trashes the filed receipts, refuses real data', () => {
  const g = fresh();
  const { s } = submitWithFile(g, EXPENSE);
  complete(g, s);
  submitWithFile(g, { ...EXPENSE, money_account: 'personal' });
  g.ctx.clearTestData();
  for (const t of ['submissions', 'attachments', 'log', 'review_tokens']) assert.strictEqual(g.tab(t).length, 0, t);
  assert.strictEqual(g.liveFiles('2026/2026-09/9967/20260914_Renton 18823_9.14_Home Depot_baseboard moulding_302.74.png').length, 0);
  assert.strictEqual(g.filesUnder('Expense_processing/Pending/').filter((p) => g.liveFiles(p).length).length, 0);
  assert.strictEqual(g.csvAt('submissions.csv').length, 1, 'export rewritten: header only');
  const h = fresh();
  for (let i = 0; i < 31; i++) h.post({ action: 'submit', data: EXPENSE });
  assert.throws(() => h.ctx.clearTestData(), /real data/);
});

test('one person on two reviewer rows (all + account:valta-homes) gets ONE summary covering both', () => {
  const g = fresh({ configOverrides: { 'reviewers.csv':
    'email,name,scope,active\nbilling@valtarealty.com,Billing,all,TRUE\nbilling@valtarealty.com,Valta Homes (temp),account:valta-homes,TRUE\n' } });
  const vh = submitWithFile(g, { ...EXPENSE, money_account: 'valta-homes' }).s;
  const bank = submitWithFile(g, EXPENSE).s;
  complete(g, vh); complete(g, bank);
  g.ctx.sendWeeklyDigest();
  assert.strictEqual(g.outbox.length, 1, 'one email, not two');
  assert.match(g.outbox[0].subject, /2 waiting/);
  const t = digestToken(g);
  const r = plain(g.ctx.decideFromQueue(t, [vh.id, bank.id], 'approve', ''));
  assert.deepStrictEqual(r.results.map((x) => x.ok), [true, true], 'both approvable from the one link');
});

test('fixConfig20260924b fills blank card/bank qb_account names, never overwrites', () => {
  const blank = 'code,label,qb_account,kind,drive_folder,in_qbo\nchase-9967,Chase,Chase Trust Checking 9967 - STR,bank,9967,TRUE\nops-7197,Ops,,bank,7197,TRUE\ncredit-7439,C,My own name,card,7439,TRUE\n';
  const g = fresh({ configOverrides: { 'money_accounts.csv': blank } });
  assert.deepStrictEqual(plain(g.ctx.fixConfig20260924b()), ['money_accounts: ops-7197 qb_account = Chase Checking 7197']);
  const rows = g.configSheet().getSheetByName('money_accounts').getDataRange().getDisplayValues();
  assert.strictEqual(rows[3][2], 'My own name', 'a typed-in name is kept');
  assert.deepStrictEqual(plain(g.ctx.fixConfig20260924b()), []);
});

test('the /exec link serves the form with the lists baked in; formApi runs the same flow as POST', () => {
  const g = fresh();
  const page = g.get({}).html;
  assert.ok(page.includes('Valta Expense Form') && page.includes('google.script.run'));
  const cfg = JSON.parse(page.match(/var cfg = (.*);\n/)[1]);
  assert.ok(cfg.properties.length > 0 && cfg.default_payment_account === 'personal');
  assert.ok(!/reviewer|@valta/i.test(page), 'no reviewer emails on the public page');
  assert.strictEqual(JSON.parse(g.get({ action: 'ping' }).content).service, 'expense_form');
  const s = g.ctx.formApi('submit', { data: { ...EXPENSE, client_ref: 'formref123' } });
  assert.ok(s.ok, s.error);
  const f = g.ctx.formApi('file', { data: { id: s.id, upload_token: s.upload_token, mime_type: 'image/png', base64: PNG, last: true } });
  assert.ok(f.ok && f.completed, f.error);
  assert.strictEqual(g.ctx.formApi('nope', {}).ok, false);
});

test('an anonymous visitor cannot run hand-run or trigger functions through google.script.run', () => {
  const g = fresh();
  g.session.active = '';                                    // a web-app visitor
  for (const fn of ['clearTestData', 'showReimbursementLink', 'reimbursementLink', 'sendReviewSummaryNow',
                    'sendWeeklyDigest', 'exportCsvNow', 'exportLedger', 'exportLedgerSoon', 'runSetup', 'setup',
                    'applyConfigUpdates', 'fixConfig20260923', 'fixConfig20260924', 'fixConfig20260924b']) {
    assert.throws(() => g.ctx[fn]({ triggerUid: 'guess' }), /Only the script owner/, fn);
  }
  assert.strictEqual(typeof g.ctx.loadConfig, 'undefined', 'loadConfig (reviewer emails) is private');
  // its own trigger still works with nobody signed in
  const t = g.triggers.find((x) => x.getHandlerFunction() === 'sendWeeklyDigest');
  g.ctx.sendWeeklyDigest({ triggerUid: t.getUniqueId() });
});
