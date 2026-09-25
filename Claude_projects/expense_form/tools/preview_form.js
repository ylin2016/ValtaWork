// Local preview of apps_script/Form.html (the page the web app serves at /exec), rendered by the test
// harness with the seed config and a fake google.script.run (every call succeeds after 0.7 s).
// Nothing reaches Google.   node tools/preview_form.js [port]   (launch config "form-preview")
const http = require('http');
const { createGas } = require('../tests/gas_harness');

const STUB = `<script>window.google = { script: { run: new Proxy({}, { get: function (_, k) {
  var ok = null, chain = { withSuccessHandler: function (f) { ok = f; return chain; }, withFailureHandler: function () { return chain; },
    formApi: function (action, body) { setTimeout(function () {
      ok(action === 'submit' ? { ok: true, id: 12, upload_token: 't' } : { ok: true, completed: !!(body.data && body.data.last), file_name: 'x.jpg' });
    }, 700); } };
  return chain[k]; } }) } };</script>`;

http.createServer((req, res) => {
  const g = createGas(); g.ctx.setup();                     // re-rendered per request: edits show on reload
  const html = g.get({}).html.replace('<head>', '<head><meta name="viewport" content="width=device-width, initial-scale=1">' + STUB);
  res.writeHead(200, { 'Content-Type': 'text/html; charset=utf-8' });
  res.end(html);
}).listen(Number(process.argv[2]) || 8789, () => console.log('form preview on http://localhost:' + (Number(process.argv[2]) || 8789)));
