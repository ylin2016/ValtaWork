/** Drive writes. The script runs as the owner, so files land in the owner's Drive. */

/**
 * Drive's by-name lookups lag a few seconds behind a create/rename (seen live 2026-09-24: two receipts
 * moved seconds apart got the same name). So a folder we create is remembered in the script cache, and
 * name collisions also consult `taken` — the names the ledger already filed in that folder.
 */
var FOLDER_CACHE_SECONDS = 6 * 3600;

function childFolder_(parent, name) {
  var cache = CacheService.getScriptCache(), key = 'folder:' + parent.getId() + ':' + name;
  var id = cache.get(key);
  if (id) {
    try { var f = DriveApp.getFolderById(id); if (!f.isTrashed()) return f; } catch (e) { /* deleted: look again */ }
  }
  var it = parent.getFoldersByName(name), folder = null;
  while (it.hasNext()) { var c = it.next(); if (!c.isTrashed()) { folder = c; break; } }
  folder = folder || parent.createFolder(name);
  cache.put(key, folder.getId(), FOLDER_CACHE_SECONDS);
  return folder;
}

function nameTaken_(folder, taken) {
  return function (n) { return !!(taken && taken[n]) || liveFileNamed_(folder, n); };
}

/** Create folderPath under the year root (existing folders are reused by exact name) and save. */
function saveFile_(rootId, folderPath, fileName, mimeType, bytes, taken) {
  var folder = DriveApp.getFolderById(rootId);
  folderPath.forEach(function (name) { folder = childFolder_(folder, name); });
  var name = dedupeName(fileName, nameTaken_(folder, taken));
  var file = folder.createFile(Utilities.newBlob(bytes, mimeType, name));
  return { fileId: file.getId(), url: file.getUrl(), fileName: name };
}

/**
 * Rename + refile an uploaded file (weekly reimbursement: Pending/ -> the pay month's folder, name
 * gains the pay date and the Zelle total). Refuses files outside the year roots and Expense_processing.
 */
function moveFile_(layout, fileId, rootId, folderPath, newName, taken) {
  var file = DriveApp.getFileById(fileId);
  if (!underYearRoots_(file, layout)) throw new Error('File ' + fileId + ' is not under a year folder or Expense_processing');
  var folder = DriveApp.getFolderById(rootId);
  folderPath.forEach(function (name) { folder = childFolder_(folder, name); });
  var parents = file.getParents();
  var inPlace = parents.hasNext() && parents.next().getId() === folder.getId() && file.getName() === newName;
  var name = inPlace ? newName : dedupeName(newName, nameTaken_(folder, taken));
  file.setName(name);
  file.moveTo(folder);
  return { fileId: file.getId(), url: file.getUrl(), fileName: name };
}

/** Drive's getFilesByName also returns trashed files; only a live one makes a name taken. */
function liveFileNamed_(folder, name) {
  return liveFile_(folder, name) !== null;
}

function underYearRoots_(file, layout) {
  var ids = {};
  ids[processingFolder_().getId()] = true;                    // Pending/ receipts live here
  Object.keys(layout.year_roots || {}).forEach(function (y) { ids[layout.year_roots[y]] = true; });
  var level = file.getParents();
  for (var depth = 0; depth < 6 && level.hasNext(); depth++) {
    var parent = level.next();
    if (ids[parent.getId()]) return true;
    level = parent.getParents();
  }
  return false;
}
