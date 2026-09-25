// node --test tests/
const test = require('node:test');
const assert = require('node:assert');
const n = require('../apps_script/Naming.gs');

const EXP = { payDate: '2026-09-14', purchaseDate: '2026-09-10', propertyName: 'Renton 18823',
              description: 'Home Depot_baseboard moulding and nails', amount: '302.74', ext: 'JPG' };

// What QBO_operations' build_zelle.property_of / amount_of read from a filename.
function qboOperationsParse(name) {
  const parts = name.replace(/\.[^.]+$/, '').split('_').map((t) => t.trim());
  return [parts[1], parts[parts.length - 2]];
}

test('expense paid at purchase: one amount, no total (user, 2026-09-24)', () => {
  assert.strictEqual(n.expenseFileName(EXP), '20260914_Renton 18823_9.10_Home Depot_baseboard moulding and nails_302.74.jpg');
  assert.strictEqual(n.expenseFileName({ ...EXP, payDate: '2026-09-21', purchaseDate: '2026-09-21', propertyName: 'Valta Realty',
    description: 'Costco_Supplies', amount: '75.25' }), '20260921_Valta Realty_9.21_Costco_Supplies_75.25.jpg');
});

test('purchase date: month unpadded, day padded', () => {
  assert.strictEqual(n.monthDay('2026-09-05'), '9.05');
  assert.strictEqual(n.monthDay('2026-12-31'), '12.31');
});

test('pending name: no pay date, no total (both decided at reimbursement) — user example', () => {
  assert.strictEqual(n.pendingFileName({ purchaseDate: '2026-09-21', propertyName: 'Valta Realty',
    description: 'Costco_Supplies', amount: '75.25', ext: 'JPG' }), 'PENDING_Valta Realty_9.21_Costco_Supplies_75.25.jpg');
  assert.strictEqual(n.pendingFileName({ purchaseDate: '2026-09-10', propertyName: 'Renton 18823',
    description: 'Home Depot_nails', amount: '11.80', ext: 'png' }), 'PENDING_Renton 18823_9.10_Home Depot_nails_11.8.png');
});

test('reimbursed: the total is the Zelle total', () => {
  const name = n.expenseFileName({ ...EXP, total: '1110.50' });
  assert.strictEqual(name, '20260914_Renton 18823_9.10_Home Depot_baseboard moulding and nails_302.74_1110.5.jpg');
  assert.deepStrictEqual(qboOperationsParse(name), ['Renton 18823', '302.74']);
});

test('store_stuff: split at the FIRST underscore; both parts required', () => {
  assert.deepStrictEqual({ ...n.splitStoreStuff(' Home Depot _ nails_screws ') }, { store: 'Home Depot', stuff: 'nails screws' });
  for (const bad of ['Home Depot nails', '_nails', 'Home Depot_', '', '  _  '])
    assert.throws(() => n.splitStoreStuff(bad), /store_stuff/);
});

test('deposit name carries the confirmation code', () => {
  const name = n.depositFileName({ date: '2026-09-14', propertyName: 'Renton 18823', payer: 'John Smith',
    method: 'Zelle', depositType: 'Pet fee', reservationCode: ' hmabc 123', amount: '150.00', ext: 'png' });
  assert.strictEqual(name, '20260914_Renton 18823_John Smith_Zelle_Pet fee_HMABC123_-150.png');
});

test('QBO_operations reads property and amount of a Zelle-reimbursed name, even with _ in input and a -2 suffix', () => {
  const Z = { ...EXP, total: '1110.50' };
  for (const name of [n.expenseFileName(Z),
                      n.expenseFileName({ ...Z, description: 'Lowes_A_B_C stuff' }),
                      n.dedupeName(n.expenseFileName(Z), (x) => !x.endsWith('-2.jpg'))]) {
    assert.deepStrictEqual(qboOperationsParse(name), ['Renton 18823', '302.74'], name);
  }
});

test('underscore never inside a field; spaces kept', () => {
  assert.strictEqual(n.cleanField('Lowe_s  Home/Improvement'), 'Lowe s Home Improvement');
});

test('no property -> Valta Realty', () => {
  assert.ok(n.expenseFileName({ ...EXP, propertyName: '' }).includes('_Valta Realty_'));
});

test('amount short form; bad amounts refused', () => {
  assert.strictEqual(n.formatAmount('$1,234.50'), '1234.5');
  assert.strictEqual(n.formatAmount('150.00'), '150');
  assert.strictEqual(n.formatAmount('100'), '100');
  assert.strictEqual(n.formatAmount(440.25), '440.25');
  for (const bad of ['abc', '0', '-5', '12.345', '']) assert.throws(() => n.formatAmount(bad), n.UserError);
});

test('bad dates refused', () => {
  for (const bad of ['08/14/2026', '2026-02-30']) {
    assert.throws(() => n.expenseFileName({ ...EXP, payDate: bad }));
    assert.throws(() => n.expenseFileName({ ...EXP, purchaseDate: bad }));
  }
});

test('collision suffix -2, -3', () => {
  const taken = new Set(['a.jpg', 'a-2.jpg']);
  assert.strictEqual(n.dedupeName('a.jpg', (x) => taken.has(x)), 'a-3.jpg');
  assert.strictEqual(n.dedupeName('b.jpg', (x) => taken.has(x)), 'b.jpg');
});

test('folder path from the layout; account used verbatim', () => {
  const layout = require('../config/drive_layout.json');
  assert.deepStrictEqual(n.buildFolderPath(layout.expense, { date: '2026-09-14', account: '9967' }), ['2026-09', '9967']);
  assert.deepStrictEqual(n.buildFolderPath(layout.deposit, { date: '2026-09-14', account: 'Credit 3104' }), ['2026-09', 'Credit 3104']);
  assert.throws(() => n.buildFolderPath('{yyyy_mm}/{account}', { date: '2026-09-14', account: '' }));
  assert.throws(() => n.buildFolderPath('X/{nope}', { date: '2026-09-14', account: '9967' }));
});

test('year root: loud when the year has none', () => {
  const layout = require('../config/drive_layout.json');
  assert.strictEqual(n.yearRoot(layout, '2026-09-14'), '1Hz-76uPBm6fEvggje8ekdYcDfKisj_Ju');
  assert.throws(() => n.yearRoot(layout, '2027-01-02'), n.UserError);
});
