// Local stand-in for the deployed web app, for trying tools/test_form.html without touching
// Google: runs the real .gs files on the in-memory harness.   node tests/mock_server.js [port]
//   page:    http://localhost:8787/test_form.html     web app URL to paste: http://localhost:8787/exec
const http = require('node:http');
const fs = require('node:fs');
const path = require('node:path');
const { createGas } = require('./gas_harness');

const port = Number(process.argv[2] || 8787);
const g = createGas();
g.ctx.setup();
let posts = 0;

http.createServer((req, res) => {
  const u = new URL(req.url, `http://localhost:${port}`);
  const send = (code, type, body) => {
    res.writeHead(code, { 'Content-Type': type, 'Access-Control-Allow-Origin': '*' });
    res.end(body);
  };
  if (u.pathname === '/test_form.html') {
    return send(200, 'text/html', fs.readFileSync(path.join(__dirname, '..', 'tools', 'test_form.html')));
  }
  if (u.pathname === '/files') {
    return send(200, 'application/json', JSON.stringify({ drive: g.filesUnder('2026/'), mail: g.outbox.map((m) => m.subject),
                                                          submissions: g.tab('submissions').map((r) => ({ id: r.id, status: r.status, file_count: r.file_count })) }));
  }
  if (u.pathname !== '/exec') return send(404, 'text/plain', 'not found');
  if (req.method === 'GET') {
    const out = g.get(Object.fromEntries(u.searchParams));
    return out.html ? send(200, 'text/html', out.html) : send(200, 'application/json', out.content);
  }
  let body = '';
  req.on('data', (c) => { body += c; });
  // FLAKY=1: every other POST runs but its reply is "lost" (Google's 404 page) — the form must retry
  // without creating duplicates.
  req.on('end', () => {
    const out = JSON.stringify(g.post(body));
    if (process.env.FLAKY && (++posts % 2 === 1)) return send(404, 'text/html', '<!DOCTYPE html><html>Google 404</html>');
    send(200, 'application/json', out);
  });
}).listen(port, () => console.log(`mock web app on http://localhost:${port}/exec — page http://localhost:${port}/test_form.html`));
