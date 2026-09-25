// In-memory stand-ins for the Apps Script services the backend uses, and a loader that runs the
// real apps_script/*.gs files against them in a vm context. Only the calls the code makes exist.
const vm = require('node:vm');
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');

const GAS_DIR = path.join(__dirname, '..', 'apps_script');
const CONFIG_DIR = path.join(__dirname, '..', 'config');
const PROCESSING_FOLDER_ID = '1_hc7D9EDnCgaj3NZYB4Ss68m0RvVhO5Y';
const YEAR_2026_ID = '1Hz-76uPBm6fEvggje8ekdYcDfKisj_Ju';

function iterator(list) {
  let i = 0;
  return { hasNext: () => i < list.length, next: () => list[i++] };
}
const signed = (buf) => Array.from(buf, (b) => (b > 127 ? b - 256 : b));
const unsigned = (arr) => Buffer.from(arr.map((b) => b & 0xff));

function parseCsv(text) {
  const rows = []; let row = []; let cell = ''; let q = false;
  for (let i = 0; i < text.length; i++) {
    const c = text[i];
    if (q) {
      if (c === '"' && text[i + 1] === '"') { cell += '"'; i++; }
      else if (c === '"') q = false;
      else cell += c;
    } else if (c === '"') q = true;
    else if (c === ',') { row.push(cell); cell = ''; }
    else if (c === '\n' || c === '\r') {
      if (c === '\r' && text[i + 1] === '\n') i++;
      row.push(cell); rows.push(row); row = []; cell = '';
    } else cell += c;
  }
  if (cell !== '' || row.length) { row.push(cell); rows.push(row); }
  return rows;
}

function formatDate(date, tz, fmt) {
  const parts = Object.fromEntries(new Intl.DateTimeFormat('en-CA', {
    timeZone: tz, year: 'numeric', month: '2-digit', day: '2-digit', hour: '2-digit', minute: '2-digit',
    second: '2-digit', hour12: false, timeZoneName: 'longOffset' }).formatToParts(date).map((p) => [p.type, p.value]));
  const d = `${parts.year}-${parts.month}-${parts.day}`;
  if (fmt === 'yyyy-MM-dd') return d;
  const off = (parts.timeZoneName || 'GMT').replace('GMT', '') || '+00:00';
  return `${d}T${parts.hour === '24' ? '00' : parts.hour}:${parts.minute}:${parts.second}${off}`;
}

// Minimal HtmlService template engine: <? code ?>, <?= escaped ?>, <?!= raw ?>
function compileTemplate(src) {
  let code = 'var __o=[];\n'; let i = 0;
  const esc = (s) => String(s).replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  while (i < src.length) {
    const start = src.indexOf('<?', i);
    if (start < 0) { code += `__o.push(${JSON.stringify(src.slice(i))});\n`; break; }
    code += `__o.push(${JSON.stringify(src.slice(i, start))});\n`;
    const end = src.indexOf('?>', start);
    let body = src.slice(start + 2, end);
    if (body.startsWith('!=')) code += `__o.push(String(${body.slice(2)}));\n`;
    else if (body.startsWith('=')) code += `__o.push(__esc(${body.slice(1)}));\n`;
    else code += body + '\n';
    i = end + 2;
  }
  code += 'return __o.join("");';
  return (vars) => new Function('__esc', ...Object.keys(vars), code)(esc, ...Object.values(vars));
}

// driveLag: file lookups by name find nothing (Drive's search index lagging behind a create/rename)
function createGas({ configOverrides = {}, mailFails = false, driveLag = false } = {}) {
  const session = { active: 'billing@valtarealty.com' };   // the editor; set '' for an anonymous web-app visitor
  let seq = 0;
  const newId = (p) => `${p}${++seq}`;
  const nodes = new Map();       // id -> {id, name, type, parent, text, bytes, mime}
  const props = new Map([['PROCESSING_FOLDER_ID', PROCESSING_FOLDER_ID]]);
  const cache = new Map();
  const outbox = [];
  const spreadsheets = new Map();
  const triggers = [];

  function addNode(n) { nodes.set(n.id, n); return n; }
  function childrenOf(id, type) { return [...nodes.values()].filter((n) => n.parent === id && n.type === type); }

  function wrapFolder(n) {
    return {
      getId: () => n.id, getName: () => n.name, isTrashed: () => !!n.trashed,
      getFoldersByName: (name) => iterator(childrenOf(n.id, 'folder').filter((c) => c.name === name).map(wrapFolder)),
      getFilesByName: (name) => iterator(driveLag && n.id !== 'cfg' ? [] : childrenOf(n.id, 'file').filter((c) => c.name === name).map(wrapFile)),
      createFolder: (name) => wrapFolder(addNode({ id: newId('folder'), name, type: 'folder', parent: n.id })),
      createFile: (blobOrName, text, mime) => {
        const f = typeof blobOrName === 'string'
          ? { name: blobOrName, text, mime }
          : { name: blobOrName.getName(), bytes: blobOrName.getBytes(), mime: blobOrName.getContentType() };
        return wrapFile(addNode({ id: newId('file'), type: 'file', parent: n.id, ...f }));
      },
      getParents: () => iterator(n.parent && nodes.has(n.parent) ? [wrapFolder(nodes.get(n.parent))] : []),
    };
  }
  function wrapFile(n) {
    return {
      getId: () => n.id, getName: () => n.name, setName: (v) => { n.name = v; },
      getUrl: () => `https://drive.google.com/file/d/${n.id}/view`,
      moveTo: (folder) => { n.parent = folder.getId(); },
      getParents: () => iterator(n.parent && nodes.has(n.parent) ? [wrapFolder(nodes.get(n.parent))] : []),
      getBlob: () => ({ getDataAsString: () => n.text ?? unsigned(n.bytes).toString('utf8') }),
      setContent: (t) => { n.text = t; n.updated = Date.now(); },
      isTrashed: () => !!n.trashed, setTrashed: (v) => { n.trashed = !!v; },
      getLastUpdated: () => new Date(n.updated ?? 1000),
      getMimeType: () => n.mime,
    };
  }

  // Seed: the year root, the processing folder and its config/ with the project's config files.
  addNode({ id: YEAR_2026_ID, name: '2026', type: 'folder', parent: null });
  addNode({ id: PROCESSING_FOLDER_ID, name: 'Expense_processing', type: 'folder', parent: null });
  const cfgFolder = addNode({ id: 'cfg', name: 'config', type: 'folder', parent: PROCESSING_FOLDER_ID });
  for (const f of fs.readdirSync(CONFIG_DIR)) {
    if (!/\.(csv|json)$/.test(f)) continue;
    const text = configOverrides[f] ?? fs.readFileSync(path.join(CONFIG_DIR, f), 'utf8');
    addNode({ id: newId('file'), name: f, type: 'file', parent: cfgFolder.id, text });
  }

  function makeSheet(name) {
    const sh = { name, rows: [], formats: {}, hidden: false };
    const display = (v, col) => (v === undefined || v === null ? '' :
      (typeof v === 'number' && sh.formats[col] === '0.00' ? v.toFixed(2) : String(v)));
    const range = (r, c, nr = 1, nc = 1) => ({
      setValues: (vals) => { vals.forEach((row, i) => row.forEach((v, j) => { (sh.rows[r - 1 + i] ||= [])[c - 1 + j] = v; })); return range(r, c, nr, nc); },
      setValue: (v) => { (sh.rows[r - 1] ||= [])[c - 1] = v; return range(r, c, nr, nc); },
      getValues: () => Array.from({ length: nr }, (_, i) => Array.from({ length: nc }, (_, j) => (sh.rows[r - 1 + i] || [])[c - 1 + j] ?? '')),
      getDisplayValues: () => Array.from({ length: nr }, (_, i) => Array.from({ length: nc }, (_, j) => display((sh.rows[r - 1 + i] || [])[c - 1 + j], c + j))),
      setNumberFormat: (f) => { for (let j = 0; j < nc; j++) sh.formats[c + j] = f; return range(r, c, nr, nc); },
      setFontWeight: () => range(r, c, nr, nc),
    });
    return {
      _sh: sh,
      getName: () => name, getLastRow: () => sh.rows.length, getMaxRows: () => 1000,
      getLastColumn: () => Math.max(0, ...sh.rows.map((r) => r.length)),
      getRange: range,
      getDataRange: () => range(1, 1, Math.max(1, sh.rows.length), Math.max(1, ...sh.rows.map((r) => r.length))),
      clearContents: () => { sh.rows = []; },
      deleteRow: (r) => { sh.rows.splice(r - 1, 1); },
      deleteRows: (r, n) => { sh.rows.splice(r - 1, n); },
      appendRow: (vals) => { sh.rows.push([...vals]); },
      setFrozenRows: () => {}, hideSheet: () => { sh.hidden = true; },
    };
  }

  const SpreadsheetApp = {
    create: (name) => {
      const id = newId('sheet');
      const sheets = [makeSheet('Sheet1')];
      const ss = {
        getId: () => id, getUrl: () => `https://docs.google.com/spreadsheets/d/${id}`,
        getSheetByName: (n) => sheets.find((s) => s.getName() === n) || null,
        insertSheet: (n) => { const s = makeSheet(n); sheets.push(s); return s; },
        getSheets: () => sheets.slice(),
        deleteSheet: (s) => sheets.splice(sheets.indexOf(s), 1),
      };
      spreadsheets.set(id, ss);
      addNode({ id, name, type: 'file', parent: 'root' });
      return ss;
    },
    openById: (id) => { if (!spreadsheets.has(id)) throw new Error('no spreadsheet ' + id); return spreadsheets.get(id); },
    flush: () => {},
  };

  // Drive's "export as xlsx" URL: returns the spreadsheet as JSON {tab: [[display values]]} so tests can read it.
  const UrlFetchApp = {
    fetch: (url, opts) => {
      const m = /spreadsheets\/d\/([^/]+)\/export\?format=xlsx/.exec(url);
      if (!m || !String(opts?.headers?.Authorization || '').startsWith('Bearer ')) throw new Error('unexpected fetch ' + url);
      const ss = spreadsheets.get(m[1]);
      const book = Object.fromEntries(ss.getSheets().map((sh) => {
        const rows = sh._sh.rows;
        const width = Math.max(0, ...rows.map((r) => r.length));
        return [sh.getName(), rows.length ? sh.getRange(1, 1, rows.length, width).getDisplayValues() : []];
      }));
      let bytes = signed(Buffer.from(JSON.stringify(book)));
      let name = 'export'; let type = 'application/octet-stream';
      const blob = { getBytes: () => bytes, getName: () => name, getContentType: () => type,
                     setName(v) { name = v; return blob; }, setContentType(v) { type = v; return blob; } };
      return { getResponseCode: () => 200, getBlob: () => blob };
    },
  };

  const gas = {
    console: { log() {}, error() {}, warn() {} },
    PropertiesService: { getScriptProperties: () => ({ getProperty: (k) => props.get(k) ?? null, setProperty: (k, v) => props.set(k, v) }) },
    CacheService: { getScriptCache: () => ({ get: (k) => cache.get(k) ?? null, put: (k, v) => cache.set(k, v), remove: (k) => cache.delete(k) }) },
    LockService: { getScriptLock: () => ({ waitLock() {}, releaseLock() {} }) },
    DriveApp: {
      getFolderById: (id) => { const n = nodes.get(id); if (!n || n.type !== 'folder') throw new Error('No folder ' + id); return wrapFolder(n); },
      getFileById: (id) => { const n = nodes.get(id); if (!n || n.type !== 'file') throw new Error('No file ' + id); return wrapFile(n); },
    },
    SpreadsheetApp, UrlFetchApp,
    Session: { getActiveUser: () => ({ getEmail: () => session.active }), getEffectiveUser: () => ({ getEmail: () => 'billing@valtarealty.com' }) },
    MailApp: { sendEmail: (m) => { if (mailFails) throw new Error('quota'); outbox.push(m); } },
    ScriptApp: {
      getService: () => ({ getUrl: () => 'https://script.google.com/macros/s/TEST/exec' }), getOAuthToken: () => 'tok',
      getProjectTriggers: () => triggers.slice(),
      deleteTrigger: (t) => triggers.splice(triggers.indexOf(t), 1),
      WeekDay: { FRIDAY: 'FRIDAY' },
      newTrigger: (fn) => { const spec = {}; const b = { timeBased: () => b, everyDays: (n) => { spec.everyDays = n; return b; },
        onWeekDay: (d) => { spec.weekDay = d; return b; }, atHour: (h) => { spec.hour = h; return b; }, nearMinute: (m) => { spec.minute = m; return b; }, after: (ms) => { spec.afterMs = ms; return b; },
        create: () => { const uid = 'trig-' + (++seq); const t = { getHandlerFunction: () => fn, getUniqueId: () => uid, spec }; triggers.push(t); return t; } }; return b; },
    },
    ContentService: { MimeType: { JSON: 'json' }, createTextOutput: (s) => ({ content: s, setMimeType() { return this; } }) },
    HtmlService: {
      createTemplateFromFile: (name) => {
        const render = compileTemplate(fs.readFileSync(path.join(GAS_DIR, name + '.html'), 'utf8'));
        const t = { evaluate: () => { const vars = { ...t }; delete vars.evaluate; const html = render(vars);
          return { html, setTitle() { return this; }, addMetaTag() { return this; } }; } };
        return t;
      },
    },
    Utilities: {
      DigestAlgorithm: { SHA_256: 'sha256' },
      getUuid: () => crypto.randomUUID(),
      computeDigest: (alg, v) => signed(crypto.createHash(alg).update(typeof v === 'string' ? Buffer.from(v, 'utf8') : unsigned(v)).digest()),
      base64Decode: (s) => { if (/[^A-Za-z0-9+/=]/.test(s)) throw new Error('bad base64'); return signed(Buffer.from(s, 'base64')); },
      newBlob: (bytes, mime, name) => ({ getBytes: () => bytes, getContentType: () => mime, getName: () => name }),
      formatDate, parseCsv,
    },
  };
  const ctx = vm.createContext(gas);
  for (const f of fs.readdirSync(GAS_DIR).filter((f) => f.endsWith('.gs')).sort()) {
    vm.runInContext(fs.readFileSync(path.join(GAS_DIR, f), 'utf8'), ctx, { filename: f });
  }

  const pathOf = (id) => { const out = []; let n = nodes.get(id); while (n) { out.unshift(n.name); n = nodes.get(n.parent); } return out.join('/'); };
  return {
    ctx, outbox, nodes, props, session,
    filesUnder: (folderName) => [...nodes.values()].filter((n) => n.type === 'file' && pathOf(n.id).startsWith(folderName)).map((n) => pathOf(n.id)),
    fileText: (p) => [...nodes.values()].find((n) => n.type === 'file' && pathOf(n.id) === p)?.text,
    liveFiles: (p) => [...nodes.values()].filter((n) => n.type === 'file' && !n.trashed && pathOf(n.id) === p),
    // the exported workbook, as {tab: [[cells]]} (see UrlFetchApp above)
    exportBook: () => { const f = [...nodes.values()].filter((n) => n.type === 'file' && !n.trashed && pathOf(n.id) === 'Expense_processing/exports/Expense Ledger export.xlsx');
                        return f.length ? { count: f.length, mime: f[0].mime, book: JSON.parse(unsigned(f[0].bytes).toString('utf8')) } : null; },
    triggers,
    clearCache: () => cache.clear(),
    configSheet: () => spreadsheets.get(props.get('CONFIG_SHEET_ID')),
    configFile: (name) => [...nodes.values()].find((n) => n.type === 'file' && !n.trashed && n.name === name && n.parent === 'cfg'),
    bookAt: (fileName) => { const f = [...nodes.values()].filter((n) => n.type === 'file' && !n.trashed && pathOf(n.id) === 'Expense_processing/exports/' + fileName);
                            return f.length ? JSON.parse(unsigned(f[0].bytes).toString('utf8')) : null; },
    // an exported CSV (path under Expense_processing/exports/) as rows of cells, or null
    csvAt: (rel) => { const f = [...nodes.values()].filter((n) => n.type === 'file' && !n.trashed && pathOf(n.id) === 'Expense_processing/exports/' + rel);
                      if (f.length > 1) throw new Error('more than one live ' + rel);
                      return f.length ? parseCsv(String(f[0].text).replace(/^\uFEFF/, '')) : null; },
    exportFileId: (rel) => [...nodes.values()].find((n) => n.type === 'file' && !n.trashed && pathOf(n.id) === 'Expense_processing/exports/' + rel)?.id,
    addExportFile: (name, text) => addNode({ id: newId('file'), name, type: 'file', text,
      parent: [...nodes.values()].find((n) => n.type === 'folder' && pathOf(n.id) === 'Expense_processing/exports').id }),
    trashedSpreadsheets: () => [...spreadsheets.keys()].filter((id) => nodes.get(id).trashed).length,
    post: (body) => JSON.parse(ctx.doPost({ postData: { contents: typeof body === 'string' ? body : JSON.stringify(body) } }).content),
    get: (params) => ctx.doGet({ parameter: params }),
    tab: (name) => ctx.Ledger.all(name),
  };
}

module.exports = { createGas, YEAR_2026_ID };
