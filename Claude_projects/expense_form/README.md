# Valta expense form — backend

A no-login receipt and deposit form. Your own form page sends each submission to a **Google Apps
Script web app**. The script checks it, adds a row to the **Expense Ledger** Google Sheet, files the
receipt in Drive (`2026/<month>/<account>/`), and emails the reviewer Approve/Reject links.

## One-time setup (about 15 minutes) — everything as **billing@valtarealty.com**

The script runs as the account that deploys it. Files it creates (the ledger sheet, receipts,
exports) are owned by **billing@valtarealty.com**, and emails come from billing@. **Editor access to
the folders is enough; billing@ doesn't need to own them.** But if a folder's owner removes billing@
or deletes the folder, submissions start failing.

1. **Project folder.** Use a folder named `Expense_processing` that billing@ can **edit**; billing@ doesn't
   need to own it. The existing one (`1_hc7D9EDnCgaj3NZYB4Ss68m0RvVhO5Y`, owned by vacation@) works:
   share it with billing@ as **Editor**. Then, signed in as billing@, open **Shared with me**, right-click
   it and choose **Organize → Add shortcut → My Drive**, so it also appears on the Mac (that's only
   needed for the `exports/` CSV files). Inside it, create a folder `config` and upload every file
   from this project's `config/` folder. **This is only the starting point:** `setup` copies them into
   the **Expense Config** Google Sheet, and from then on you edit the lists in that sheet.
2. **Year folder access.** billing@ needs **Editor** access to the `2026` receipts folder
   (`1Hz-76uPBm6fEvggje8ekdYcDfKisj_Ju`, owned by sairacruz0203@gmail.com). It already created
   `2026-09/9967` there, so this is most likely already true.
3. **Create the script.** Go to [script.google.com](https://script.google.com) as **billing@** and
   choose **New project**, named `Valta Expense Form`. For each file in `apps_script/`, create a file
   with the same name and paste the contents in (`+` → Script for `.gs`, `+` → HTML for `Review.html`).
   For `appsscript.json`, first turn on **Project Settings → Show "appsscript.json"**, then paste over it.
   *Or use the command line (what this project uses):* clasp is installed locally (`npm install`),
   so run it with `npx`. First, as billing@, turn on the Apps Script API at
   https://script.google.com/home/usersettings (free). Then run `npx clasp login` (choose billing@).
   `.clasp.json` already points at the project (`115tBrGk…`, root `apps_script/`), so
   `npx clasp push --force` uploads the code. Don't use `clasp clone`: it would overwrite the
   local files with the online copy.
4. **Point it at the folder.** Go to **Project Settings → Script Properties → Add**, and add
   `PROCESSING_FOLDER_ID` = the ID in the `Expense_processing` folder's link (the part after `/folders/`).
5. **Run `setup` once.** In the editor, pick `setup` and press **Run**, then **Allow** access to
   Drive, Sheets, Gmail, external requests and triggers. This creates the **Expense Ledger** and
   **Expense Config** sheets, the `exports` folder, and a daily export. It's safe to run again: it
   never overwrites lists you've edited.
6. **Publish it.** Go to **Deploy → New deployment → Web app**, set **Execute as: Me** and
   **Who has access: Anyone**, then press **Deploy**. Copy the **Web app URL** into your form and the
   test page. Check it in an incognito window: it must answer without a sign-in.

**If you already deployed under vacation@:** open that project and choose **Deploy → Manage
deployments → Archive**, so there's only one live address.

To change the code later, go to **Deploy → Manage deployments → Edit → Version: New version**.
The URL stays the same.

## Testing with the test page

Open `tools/test_form.html` in Chrome (double-click it). Paste your `/exec` URL and press **Load lists**,
fill in a test (the description is pre-filled with "TEST - delete me"), attach a photo or PDF, and
press **Send**. It runs submit → upload → finish and shows each step. **It's real**: it adds a ledger
row and saves the file in Drive (it appears in Friday's review summary), so delete test rows and files afterwards.
If "Load lists" says it can't connect when the page is opened as a file, serve it instead:
`python3 -m http.server 8000 -d tools`, then open http://localhost:8000/test_form.html.

The page is also a working example of what your real form has to do (see the `<script>` at the bottom).

## What your form sends

Always send `POST` requests with the header **`Content-Type: text/plain`** and a JSON body. Any other
content type makes the browser send a preflight request, and Apps Script can't answer it.
Every reply is JSON: `{ok: true, ...}` or `{ok: false, error: "message to show the user"}`.

**1. Load the dropdown lists:** `GET <url>?action=config` returns
`{properties:[{code,name}], people:[name], expense_categories:[{code,label,owner_billable_default}],
deposit_types:[{code,label,per_reservation}], collection_methods:[{code,label}], payment_accounts:[{code,label,kind}] (Paid personally first), default_payment_account:"personal"}` — preselect default_payment_account in "Paid with"

**2. Submit the fields:**

```json
{"action": "submit", "client": "<navigator.userAgent>", "data": {
  "kind": "expense",
  "txn_date": "2026-09-14", "property_code": "renton_18823", "submitter": "Jing Li",
  "amount": "302.74", "description": "Home Depot_baseboard moulding and nails",
  "category": "Repairs", "money_account": "chase-9967",
  "owner_billable": true, "reimburse_to": "Jing Li"
}}
```

For an expense, `txn_date` is the **purchase date**, and `description` must be **`store_stuff`**:
the store, an underscore, then what was bought. The store becomes the vendor, and both parts go into
the file name (`20260914_Renton 18823_9.14_Home Depot_baseboard moulding and nails_302.74.jpg`).
There is no separate vendor field; your form should show that format as a hint under the description.
For a deposit, use `"kind": "deposit"` and send `deposit_type`, `collection_method`, `payer_name`
(the guest), `reservation_code`, `check_in_date` and `payment_reference` (optional) in place of the
expense fields. Leave out `owner_billable` to use the category's default, and leave out
`reimburse_to` to reimburse the submitter.
**Other — type it:** each of the name, property, category and reimburse-to lists should end with an
"Other — type it" choice that shows a text box. Send the typed text as `submitter_other`,
`property_other`, `category_other` or `reimburse_to_other`, and leave the usual field empty. Text that
matches a list entry (ignoring case) is treated as that entry. Anything else is accepted as typed and
listed in the row's `typed_in` column; the weekly summary and review page show a ⚠ for it.
The reply is `{ok, id, upload_token}`. Keep both values for the next two steps.

**Retry every request** (user, 2026-09-24). Google's front end now and then returns an HTML error
page (often HTTP 404) or hangs ~30 s instead of the script's JSON. Give each request a ~25 s timeout
and retry up to 4 times when the reply is not JSON; never retry a JSON reply, even `{ok:false}`.
Retries are safe:
- Send a random `client_ref` (8–64 letters, digits or `-`) with `submit`, created once per
  submission and reused on every retry. The server returns the same `id` with a fresh `upload_token`
  instead of adding a row; if that submission is already finished it returns `already_complete: true`.
- A file with the same bytes sent again to the same submission returns the saved file (`retried: true`).
- A repeated `complete` is `ok`.
`tools/test_form.html` (`withRetry`) is the reference.

**3. Send each file**, one request per file. Shrink phone photos to about 1600 px first.

```json
{"action": "file", "data": {"id": 12, "upload_token": "…", "mime_type": "image/jpeg", "base64": "<file bytes>"}}
```

The reply is `{ok, seq, file_name, url, possible_duplicate_of: [ids]}`. If `possible_duplicate_of`
isn't empty, the same file was already sent with another submission, so warn the user.
**Add `"last": true` to the final file**: that also finishes the submission (the reply has
`completed: true`), so step 4 isn't needed. Each request to Apps Script costs 1–3 s, so this saves one.

**Weekly review:** every Friday around noon (Pacific), each reviewer (the Expense Config `reviewers` tab,
currently billing@) gets **one** email listing what's waiting, with a **Review all** link. The page
has Approve / Reject per submission and Approve all; the link works for 14 days. After `setup` (or
any rerun of it), check **Triggers** (⏰ in the editor): there should be exactly one `sendWeeklyDigest`
and one `exportLedger`. To send the summary right now, run `sendWeeklyDigest` in the editor.

**Weekly reimbursements:** expenses paid personally aren't filed when submitted. They wait in
`Expense_processing/Pending/`. Once a week, open your private **Reimbursements page** (run
`reimbursementLink` in the editor to get its address, and keep it private). It shows each person's
approved total. Send the Zelles, then press **Mark paid & file receipts**. Each receipt gets its final
name (pay date first, that person's Zelle total last) and moves to `2026/<month>/9967/`, and the
payout is added to the export for `QBO_operations`. Card, bank and owner-paid expenses are filed
right away, with the purchase date as the pay date.

**Lists and exports:** you edit the dropdown lists in the **Expense Config** sheet (changes are live
within 5 minutes). The property list is the exception, because it's generated on the Mac. Run
`venv/bin/python -m src.sync_properties`, then copy `config/properties.csv` into Drive
`Expense_processing/config/`, and the script replaces the properties tab with it.
About a minute after an Approve/Reject or a reimbursement run, and once a day at 2am, the script
rewrites plain CSV copies in `exports/`: `submissions.csv`, `attachments.csv`, `bank_deposits.csv`,
`zelle_payouts.csv`, `zelle_expense2qbo.csv`, and `exports/config/<tab>.csv` for every Expense
Config tab. They open in Excel or any editor; read them as text (pandas `dtype=str`) so codes stay
as typed. To refresh them by hand, run `exportLedger` in the editor.

**4. Finish** (only if the last file wasn't sent with `"last": true`): send `{"action": "complete", "data": {"id": 12, "upload_token": "…"}}`. This needs at
least one file. No email is sent; the submission waits for the **Friday review summary**.

## Development

```bash
node --test tests/*.test.js           # runs the real .gs files against in-memory Sheets/Drive/Gmail
venv/bin/python -m src.sync_properties # refresh config/properties.csv from mapping_classes.yml
node tests/mock_server.js              # fake web app on http://localhost:8787/exec for trying the test page without Google
```
