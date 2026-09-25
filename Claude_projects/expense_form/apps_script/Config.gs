/**
 * Reference lists live in the "Expense Config" Google Sheet in Expense_processing (user,
 * 2026-09-23): one tab per list, edited in the browser. Script Property CONFIG_SHEET_ID (written by
 * setup()). Read with a 5-minute cache, so an edit takes effect within 5 minutes.
 *
 *   properties, people, expense_categories, deposit_types, money_accounts, collection_methods,
 *   reviewers                 one row per item, header row 1 (headers normalised: "qbo Payee" -> qbo_payee)
 *   year_roots                year | folder_id          (Drive folder per year; add next year each January)
 *   settings                  key | value               (expense_template, deposit_template, no_account_folder,
 *                                                        reimburse_from = money_accounts code, default chase-9967)
 *
 * setup() fills an empty tab once from Expense_processing/config/<tab>.csv (and drive_layout.json);
 * it never overwrites a tab that has content. The one exception is properties: that list is
 * generated on the Mac (src/sync_properties.py), so a newer config/properties.csv in Drive
 * replaces the properties tab automatically (maybeImportProperties_).
 */

var LIST_TABS = ['properties', 'people', 'expense_categories', 'deposit_types', 'money_accounts',
                 'collection_methods', 'reviewers'];
var LIST_HEADERS = {        // used only when a tab has no CSV to start from
  properties: ['code', 'name', 'qbo_class_id', 'qbo_class_name', 'term', 'active'],
  people: ['Name', 'qbo Payee', 'Role', 'Email', 'Team', 'Status'],
  expense_categories: ['code', 'label_en', 'qb_account', 'owner_billable_default'],
  deposit_types: ['code', 'label_en', 'label_zh', 'qbo_item', 'per_reservation'],
  money_accounts: ['code', 'label', 'qb_account', 'kind', 'drive_folder', 'in_qbo', 'drive_path'],
  collection_methods: ['code', 'label', 'lands_in', 'one_per_bank_line'],
  reviewers: ['email', 'name', 'scope', 'active']
};
var CONFIG_CACHE_KEY = 'config_v2';
var CONFIG_CACHE_SECONDS = 300;

function configSheet_() {
  var id = PropertiesService.getScriptProperties().getProperty('CONFIG_SHEET_ID');
  if (!id) throw new Error('CONFIG_SHEET_ID is not set — run setup() once from the editor');
  return SpreadsheetApp.openById(id);
}

function loadConfig_() {
  var cache = CacheService.getScriptCache();
  var hit = cache.get(CONFIG_CACHE_KEY);
  if (hit) return JSON.parse(hit);

  maybeImportProperties_();
  var ss = configSheet_();
  var cfg = {};
  LIST_TABS.forEach(function (tab) { cfg[tab] = readTab_(ss, tab); });
  var settingRows = readTab_(ss, 'settings');
  cfg.drive_layout = layoutFromTabs_(readTab_(ss, 'year_roots'), settingRows);
  cfg.settings_map = {};
  settingRows.forEach(function (r) { if (r.key) cfg.settings_map[r.key] = r.value; });
  try { cache.put(CONFIG_CACHE_KEY, JSON.stringify(cfg), CONFIG_CACHE_SECONDS); } catch (e) { /* >100KB: skip cache */ }
  return cfg;
}

function readTab_(ss, tab) {
  var sh = ss.getSheetByName(tab);
  if (!sh) throw new Error('Expense Config has no "' + tab + '" tab — run setup()');
  if (sh.getLastRow() < 1) return [];
  return rowsToObjects_(sh.getDataRange().getDisplayValues());
}

function rowsToObjects_(rows) {
  if (!rows.length) return [];
  var head = rows[0].map(function (h) { return String(h).trim().toLowerCase().replace(/\s+/g, '_'); });
  return rows.slice(1)
    .filter(function (r) { return r.some(function (v) { return String(v).trim() !== ''; }); })
    .map(function (r) {
      var o = {};
      head.forEach(function (h, i) { if (h) o[h] = String(r[i] == null ? '' : r[i]).trim(); });
      return o;
    });
}

/** year_roots + settings tabs -> the layout object Naming.gs expects. */
function layoutFromTabs_(yearRows, settingRows) {
  var s = {};
  settingRows.forEach(function (r) { s[r.key] = r.value; });
  var roots = {};
  yearRows.forEach(function (r) { if (r.year && r.folder_id) roots[r.year] = r.folder_id; });
  if (!s.expense_template || !s.deposit_template) {
    throw new Error('Expense Config "settings" tab needs expense_template and deposit_template');
  }
  return { year_roots: roots, expense: s.expense_template, deposit: s.deposit_template,
           no_account_folder: s.no_account_folder || '' };
}

// ── seeding + properties import ──────────────────────────────────────────────────────────

/** Create the Expense Config sheet if needed and fill EMPTY tabs from config/*.csv. Called by setup(). */
function setupConfig_(folder) {
  var props = PropertiesService.getScriptProperties();
  var dir = childFolder_(folder, 'config');
  var id = props.getProperty('CONFIG_SHEET_ID');
  var ss;
  if (id) {
    ss = SpreadsheetApp.openById(id);
  } else {
    ss = SpreadsheetApp.create('Expense Config');
    DriveApp.getFileById(ss.getId()).moveTo(folder);
    props.setProperty('CONFIG_SHEET_ID', ss.getId());
  }

  LIST_TABS.forEach(function (tab) {
    var sh = ss.getSheetByName(tab) || ss.insertSheet(tab);
    if (sh.getLastRow() > 0) return;                          // never overwrite edits
    var file = liveFile_(dir, tab + '.csv');
    writeRows_(sh, file ? parseCsvText_(file.getBlob().getDataAsString('UTF-8')) : [LIST_HEADERS[tab]]);
    if (tab === 'properties' && file) props.setProperty('PROPERTIES_IMPORTED_AT', String(file.getLastUpdated().getTime()));
  });

  var layoutFile = liveFile_(dir, 'drive_layout.json');
  var layout = layoutFile ? JSON.parse(layoutFile.getBlob().getDataAsString('UTF-8')) : {};
  var yr = ss.getSheetByName('year_roots') || ss.insertSheet('year_roots');
  if (yr.getLastRow() === 0) {
    writeRows_(yr, [['year', 'folder_id']].concat(Object.keys(layout.year_roots || {}).map(function (y) {
      return [y, layout.year_roots[y]];
    })));
  }
  var st = ss.getSheetByName('settings') || ss.insertSheet('settings');
  if (st.getLastRow() === 0) {
    writeRows_(st, [['key', 'value'],
      ['expense_template', layout.expense || '{yyyy_mm}/{account}'],
      ['deposit_template', layout.deposit || '{yyyy_mm}/{account}'],
      ['no_account_folder', layout.no_account_folder || 'Owner paid'],
      ['reimburse_from', 'chase-9967']]);
  }
  var blank = ss.getSheetByName('Sheet1');
  if (blank && ss.getSheets().length > 1) ss.deleteSheet(blank);
  CacheService.getScriptCache().remove(CONFIG_CACHE_KEY);
  return ss;
}

/**
 * The property list is generated on the Mac. When config/properties.csv in Drive is newer than
 * the last import, it replaces the properties tab (hide a property with properties_exclude.txt on
 * the Mac, not by editing this tab — the next import would bring it back).
 */
function maybeImportProperties_() {
  var props = PropertiesService.getScriptProperties();
  var file = liveFile_(childFolder_(processingFolder_(), 'config'), 'properties.csv');
  if (!file) return false;
  var updated = file.getLastUpdated().getTime();
  if (updated <= Number(props.getProperty('PROPERTIES_IMPORTED_AT') || 0)) return false;
  var rows = parseCsvText_(file.getBlob().getDataAsString('UTF-8'));
  if (rows.length < 2) return false;                          // never replace the list with an empty file
  withLock_(function () {
    var sh = configSheet_().getSheetByName('properties');
    sh.clearContents();
    writeRows_(sh, rows);
    props.setProperty('PROPERTIES_IMPORTED_AT', String(updated));
  });
  return true;
}

/** Plain-text cells so codes, IDs and dates are never converted by Sheets. */
function writeRows_(sh, rows) {
  if (!rows.length) return;
  var width = rows.reduce(function (m, r) { return Math.max(m, r.length); }, 0);
  var padded = rows.map(function (r) { var c = r.slice(); while (c.length < width) c.push(''); return c; });
  sh.getRange(1, 1, sh.getMaxRows(), width).setNumberFormat('@');
  sh.getRange(1, 1, padded.length, width).setValues(padded);
  sh.getRange(1, 1, 1, width).setFontWeight('bold');
  sh.setFrozenRows(1);
}

function parseCsvText_(text) {
  return Utilities.parseCsv(String(text).replace(/^﻿/, ''));
}

/** Drive's getFilesByName also returns trashed files. */
function liveFile_(dir, fileName) {
  var it = dir.getFilesByName(fileName);
  while (it.hasNext()) {
    var f = it.next();
    if (!f.isTrashed()) return f;
  }
  return null;
}

function truthy_(v) {
  return /^(true|t|yes|y|1|active)$/i.test(String(v || '').trim());
}

/** Lookup maps over the lists, active rows only. */
function lookups_(cfg) {
  function byKey(rows, key, activeCol) {
    var m = {};
    rows.forEach(function (r) { if (!activeCol || truthy_(r[activeCol])) m[r[key]] = r; });
    return m;
  }
  return {
    properties: byKey(cfg.properties, 'code', 'active'),
    people: byKey(cfg.people, 'name', 'status'),
    categories: byKey(cfg.expense_categories, 'code'),
    depositTypes: byKey(cfg.deposit_types, 'code'),
    accounts: byKey(cfg.money_accounts, 'code'),
    methods: byKey(cfg.collection_methods, 'code'),
    layout: cfg.drive_layout
  };
}

/** What the form needs to draw its dropdowns. Nothing sensitive (no emails, no payees). */
function publicConfig() {
  var cfg = loadConfig_();
  var L = lookups_(cfg);
  function vals(m, f) { return Object.keys(m).map(function (k) { return f(m[k]); }); }
  return {
    ok: true,
    properties: vals(L.properties, function (r) { return { code: r.code, name: r.name }; })
      .sort(function (a, b) { return a.name.localeCompare(b.name); }),
    people: vals(L.people, function (r) { return r.name; }).sort(),
    expense_categories: vals(L.categories, function (r) {
      return { code: r.code, label: r.label_en || r.code, owner_billable_default: truthy_(r.owner_billable_default) };
    }),
    deposit_types: vals(L.depositTypes, function (r) {
      return { code: r.code, label: r.label_en || r.code, per_reservation: truthy_(r.per_reservation) };
    }),
    collection_methods: vals(L.methods, function (r) { return { code: r.code, label: r.label }; }),
    // Paid personally first: it is the form's default (user, 2026-09-23), so a cleaner who paid out of
    // pocket is never filed as paid from 9967 by leaving the choice untouched.
    payment_accounts: vals(L.accounts, function (r) { return { code: r.code, label: r.label, kind: r.kind }; })
      .sort(function (a, b) { return (b.kind === 'personal') - (a.kind === 'personal'); }),
    default_payment_account: (vals(L.accounts, function (r) { return r; })
      .filter(function (r) { return r.kind === 'personal'; })[0] || {}).code || ''
  };
}
