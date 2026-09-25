# CLAUDE.md — expense_form

Guidance for Claude Code working in this project.

## What this is

A no-login form, built by the user, where staff record two kinds of money event:

- **Expense**: money going out, such as supplies, repairs or cleaning. It is drafted later as a **QBO Purchase**.
- **Deposit**: a pet or parking fee collected by hand from a guest by Zelle, Cash App or cash. It is drafted as a **fee Invoice + Payment + bank Deposit** that matches the bank-feed line.

Each submission becomes one row in the **"Expense Ledger" Google Sheet**. Its receipt is renamed and filed in the user's existing Drive bookkeeping folders, at **year folder / `YYYY-MM` / account folder**, and a reviewer is emailed Approve/Reject links. `../QBO_operations` later drafts QuickBooks transactions from the approved rows.

The original plan, `Valta-报销单系统-技术方案.md` (v0.1, in Chinese), still covers fields, notifications and reports. Several of its choices were superseded by user decisions, as recorded below.

## Architecture (user decisions 2026-09-23: **no Neon, no Vercel**)

```
user's own form ──POST (text/plain JSON)──▶ Apps Script Web App (runs as billing@valtarealty.com, access: Anyone)
                                              ├─▶ Google Sheet "Expense Ledger"   (tracking table; tabs below)
                                              ├─▶ Drive 2026/2026-09/9967/<name>  (receipts)
                                              ├─▶ Gmail                           (reviewer links, submitter receipt)
                                              └─▶ Drive Expense_processing/exports/*.csv ──(Drive for desktop sync)──▶ Mac ──▶ QBO_operations
                        Google Sheet "Expense Config" (edited in the browser) ◀── seeded once from config/*.csv; properties.csv re-imported when newer
```

- **Why no Google Form:** a file-upload question forces respondents to sign in. **Why Apps Script:** it runs as the owner, so anonymous submitters can write to the owner's Drive, Sheet and Gmail with no extra account, key or hosting. **Why no Neon:** the user chose the Sheet as the tracking table. The history of pivots was Apps Script → Vercel+Neon → Apps Script+Sheet; don't re-propose a database or hosting.
- **Everything runs as `billing@valtarealty.com`** (user, 2026-09-23): the script project, the deployment, the ledger, the receipts and the sent mail. billing@ is also the Mac's Drive for desktop account, so a folder in billing@'s My Drive syncs to the Mac.
- **`Expense_processing`** (`1_hc7D9EDnCgaj3NZYB4Ss68m0RvVhO5Y`, owned by vacation@) is set by the Script Property `PROCESSING_FOLDER_ID` (not in code). It holds the ledger sheet, `config/` and `exports/`. **billing@ only needs Editor access** (user asked, 2026-09-23), not ownership. For the Mac sync, billing@ adds a shortcut to it in My Drive (Shared with me → Add shortcut); shared-with-me folders don't sync otherwise. Files the script creates are owned by billing@. If the folder owner unshares or deletes it, submissions fail.
- billing@ needs Editor access on the `2026` year folder (owned by sairacruz0203@gmail.com); it already created `2026-09/9967` there.
- **The Mac never calls a Google API.** On Drive, everything is Google Sheets. **Locally, the Mac needs csv or xlsx** (user, 2026-09-23), because Drive for desktop turns a Google Sheet into an unreadable `.gsheet` link. So `exports/` holds **plain CSVs** (user, 2026-09-24: Sheets and CSV are fine "as long as it is easy to read"; the xlsx export needed a temporary Sheet + URL fetch and was slow): `submissions.csv`, `attachments.csv`, `bank_deposits.csv`, `zelle_payouts.csv` (no token hashes, no review_tokens), `zelle_expense2qbo.csv`, and `exports/config/<tab>.csv`. UTF-8 with BOM, rewritten in place (`setContent`, same file ID). The first CSV export trashes the old `Expense … export.xlsx` files. The only file that goes the other way is `config/properties.csv`, generated on the Mac, both carried by billing@'s Drive for desktop. This desktop-app session gets "Operation not permitted" on `~/Library/CloudStorage`, so the user reads those files from their own terminal.
- **This project never writes to QuickBooks.** `QBO_operations` is the only writer (build → review CSV → `--confirm`). Its results (`qbo_*` columns) are meant to come back through an `imports/` CSV that the script applies. **Not built yet.**

## Layout

```
apps_script/          the backend — push to script.google.com (clasp or paste); see README.md
  Admin.gs            EVERY function the user runs by hand, one file (user, 2026-09-24: hard to find them across files): showReimbursementLink, sendReviewSummaryNow, exportCsvNow, runSetup, applyConfigUpdates (calls each fixConfigYYYYMMDD). Pushed first (.clasp.json filePushOrder). Add any new hand-run step HERE and tell the user its Admin.gs name.
  Code.gs             doGet/doPost router, error shape
  Service.gs          submitExpense / addFile / completeSubmission / decideReview, ALL validation
  Naming.gs           filename + folder rules (pure; node-tested)
  Ledger.gs           Sheet tabs/columns (TABS), append/update/where, setup()
  Config.gs           reads the "Expense Config" Sheet (5-min cache); setupConfig_ seeds empty tabs from config/*.csv; maybeImportProperties_; publicConfig()
  Drive.gs            saveFile_, moveFile_ (Pending/ -> bookkeeping folders at reimbursement)
  Mail.gs             sendWeeklyDigest (the only email)
  Queue.html          weekly review queue page
  Export.gs           exports/*.csv + exports/config/*.csv, ~1 min after a review/reimbursement (one-off trigger) + daily 2am
  Review.html         single-submission review page (old links)
  Maintenance.gs      one-off config fixes run by hand (fixConfig20260923)
  Reimburse.gs + ReimbursePage.html  weekly reimbursement page (Apps Script needs distinct names across .gs/.html) + processReimbursements
config/               SEED ONLY for the Expense Config sheet (setup copies a CSV into an EMPTY tab once); after that edit the Sheet, not these files
  properties.csv      GENERATED: venv/bin/python -m src.sync_properties. Put it in Drive config/, and when it's newer than the last import the script REPLACES the properties tab
  properties_exclude.txt  property_ids the form must not offer (user's choice); the sync skips them
  people.csv, expense_categories.csv   the user's originals (now live in the Sheet); read, never rewrite
  deposit_types.csv, collection_methods.csv, money_accounts.csv, reviewers.csv, drive_layout.json
src/sync_properties.py
tests/                node --test tests/*.test.js   (gas_harness.js = in-memory Sheets/Drive/Gmail running the real .gs files)
```

Run the tests after any change to `apps_script/` or `config/`: `node --test tests/*.test.js`. Use `venv/bin/python` for Python; `yaml` is not installed in the system python.

## Ledger tabs (`TABS` in Ledger.gs)

| tab | purpose |
|---|---|
| `submissions` | one row per submission; `kind` = expense / deposit; status `pending → approved/rejected` (`posted` / `void` later) |
| `attachments` | each file: final Drive name, folder, file ID/URL, upload status, sha256 (the same bytes on another submission are flagged as a possible duplicate) |
| `bank_deposits` | one bank-feed line into 9967 grouping one or more deposit submissions → one QBO Deposit |
| `zelle_payouts` | one Zelle transfer reimbursing a person for approved `personal` expenses (weekly run) |
| `review_tokens` | hidden; SHA-256 of each emailed review link token |
| `log` | every state change |

Every column is formatted as plain text (`@`), so Sheets can't turn `1234E5` into a number, and money columns use `0.00`. **A Sheet cannot refuse a bad row**, so all checks live in `validateSubmission_`, and QBO_operations must re-check rows before drafting. A hand edit in the Sheet bypasses every check.

## Rules that must not drift

- **Filename = the owner's existing Drive convention** (user, 2026-09-22): `<YYYYMMDD>_<Property>_<Payee>_<middle>_<amount>_<total>`, with spaces kept.
  - **Expense: there is no vendor field** (user, 2026-09-23). The date is labelled **"Purchase date"**, and the description must be **`store_stuff`** (split at the first `_`; both parts required). The store is saved in the `vendor` column as the QBO vendor.
  - **Expense file name = `<PAY date>_<property>_<purchase M.DD>_<store>_<stuff>_<amount>`** (user, 2026-09-23), e.g. `20260921_Valta Realty_9.21_Costco_Supplies_75.25.jpg`. **The second, total amount is added only on Zelle-reimbursed receipts** (user, 2026-09-24: "except zelle payments, no need to add the second total amount"): `…_302.74_1110.5.jpg`, where the last field is the person's Zelle total. Deposits have no total either, and their amount is written **negative** (money in; user, 2026-09-24): `20260914_Renton 18823_John Smith_Zelle_Pet fee_HMABC123_-150.png`. The ledger `amount` column stays positive. M.DD is written `9.10` / `9.05` (month unpadded, day padded).
    - **Paid at purchase** (money account kind card / bank / owner): pay date = purchase date. The file is filed final at submission, in the purchase month.
    - **"Paid with" defaults to Paid personally** (user, 2026-09-23): `publicConfig` lists it first and returns `default_payment_account`; a form must preselect it.
    - **Paid personally:** the pay date is the **weekly reimbursement date** ("reimburse once a week, don't process at submission"). Until then the file waits as `PENDING_<property>_<purchase M.DD>_<store>_<stuff>_<amount>` (no pay date, no total: neither is decided yet; user, 2026-09-24) in `Expense_processing/Pending/`. Nothing enters the bookkeeping folders unpaid.
  - Deposit: payee = guest, middle `<Method>_<Fee type>_<CONFCODE>`.
  - `QBO_operations/src/invoices/build_zelle.py` reads **field 2 as the property and the second-to-last field as the amount** of a Zelle-reimbursed name, so those always carry the total; `_` can never appear inside a field. The test `QBO_operations reads property and amount of a Zelle-reimbursed name` guards this. Other names end with their single amount, and a `-2` collision suffix lands on it.
  - Amounts use the short form (`150`, `440.25`). A name collision adds `-2`, after the total.
- **Drive location = date + account** (user, 2026-09-22): the year root from `drive_layout.json` `year_roots` (2026 = `1Hz-76uPBm6fEvggje8ekdYcDfKisj_Ju`, folder "2026", owned by sairacruz0203@gmail.com), then `{yyyy_mm}/{account}`, where account is `money_accounts.drive_folder`.
  - Folder names are used **verbatim**, because the folders already exist and are filled by hand.
  - Accounts with no folder (owner-paid) use `no_account_folder` ("Owner paid"). ⚠️ That name is a placeholder.
  - A year without a root **fails at submit**. Add each new year's folder ID to `year_roots` in January.
- **Properties come from `mapping_classes.yml`** through the sync. To hide a property, add its ID to `properties_exclude.txt`; deleting the CSV row doesn't stick, because the next sync brings it back. The user excluded 24 on 2026-09-23. Our properties list is **separate** from KnowledgeDatabase's (user, 2026-09-23): different grain, no link.
- **Reimbursements: weekly, from a private page** (`Reimburse.gs` / `ReimbursePage.html`, `?action=reimburse&k=ADMIN_KEY`; run `reimbursementLink()` in the editor to see the link; `ADMIN_KEY` is a Script Property made by setup).
  - The page lists **approved**, unpaid `personal` expenses grouped by `reimburse_to`, with each person's total. Unreviewed ones are counted but not paid.
  - After the user sends the Zelles, **Mark paid & file** takes a pay date (default today). For each person it writes one `zelle_payouts` row: payee = `people.qbo_payee` (e.g. Griselda Ramirez → `Camila - service`), paid_from = settings `reimburse_from` (default `chase-9967`), total, and reference `<YYYYMMDD>_<payee>_<total>`, the build_zelle DocNumber shape.
  - Every receipt is then renamed with the pay date first and **the person's Zelle total last**, and moved from `Pending/` to `<pay year>/<pay month>/9967/`.
  - `exports/zelle_expense2qbo.csv` has one row per reimbursed receipt in build_zelle's columns (`Date, Pay, Zelle content, JE Amount, Amount, Property, Category, qbo_account, filename, submission_id, Paid from, typed_in`). **Built 2026-09-24:** `python -m src.invoices.build_zelle --src <that csv>` (in QBO_operations) reads it directly: it uses the row's `qbo_account` as-is, `Paid from` as the bank, and drops `typed_in` lines with a warning. One Purchase per payout, same as the hand workbook. Dry-run tested with a stubbed resolver; not yet run against live QBO.
- **Deposits** (user, 2026-09-23): Zelle, Cash App and cash all land in **Chase 9967** (`collection_methods.lands_in`; the submitter can't choose).
  - Zelle and Cash App are **one payment per bank line**, so approval creates the `bank_deposits` row automatically. Cash is batched by hand when it's taken to the bank (not built yet).
  - Pet and parking fees are **per reservation**: the confirmation code (upper-case, no spaces) and the check-in date are required.
  - In QBO_operations, the customer is found from the reservation invoice (`Resolver.customer()`, customers are named `<Guest> - <code>`). Then, per fee:
    - The fee is an open line on the reservation invoice: a **Payment** against it.
    - It isn't on the invoice: **create an Invoice** for the fee (items `Pet Fee (Owner)` / `Parking Fee`, whose income accounts are `Trust Liabilities:Owner Payables:1A - …:Pet Fees Paid to Owner` / `Parking Fee Paid to Owner`, i.e. an owner pass-through with no commission), then a Payment.
    - It's on the invoice but already paid: **held** for the owner.
  - Payments go to **Undeposited Funds**. One QBO Deposit per `bank_deposits` row links them, for the bank-line total. Never deposit a Payment straight to 9967; that caused the $55,972.41 double count.
  - **Built 2026-09-24 in QBO_operations:** `python -m src.deposits.build_guest_fees --exports <Drive>/Expense_processing/exports` → `review/guest_fees.csv` → `python -m src.deposits.post_guest_fees [--confirm]`. Offline-tested (fake QBO); not yet run live.
- **"Other — type it"** (user, 2026-09-24): name, property, category and reimburse-to accept free text (`<field>_other`, max 60, cleanField). Text matching a list entry (case-insensitive: code/name/label) snaps to it; otherwise it is kept as typed (`property_code` blank, `property_name`/`category`/name = the text) and the field goes in `submissions.typed_in`. The digest and Queue.html flag it. **QBO_operations must skip rows with a non-empty `typed_in`** until the reviewer fixes the row by hand (or adds the entry to Expense Config) and clears it. Adding the column needs `setup` re-run (header).
- **Paid by Valta Homes** (user, 2026-09-24): money account `valta-homes`, kind `company` (paid at purchase, so it's filed final at submission), `in_qbo = FALSE` (never drafted in Valta Realty's QBO). The optional column **`money_accounts.drive_path`** replaces the settings template for one account: valta-homes → `<year>/Valta Homes Unprocessed Invoices/Testing/` ("Testing" for now; change the cell when it's final). **Reviewed only by contact@valtahomes.com** (reviewers scope `account:valta-homes`). A person may have several rows: they get ONE summary/link covering all their scopes (`activeReviewers_`). Scopes: `all` = everything except accounts claimed by an active `account:<code>` reviewer; `account:<code>`; or a property code. Live sheet updated by `fixConfig20260924` (Maintenance.gs).
- **Accounts with `in_qbo = FALSE`** (Baselane, owner-paid) are tracked and filed but never drafted.
- **Review = one weekly summary, Friday ~noon Pacific** (user, 2026-09-23: "I don't want to receive approving email every record, send me by the noon of Friday"). Submitting sends **no** email.
  - `sendWeeklyDigest` (trigger from `installTriggers_`, run by setup) emails each active reviewer that has completed, pending submissions in scope. It sends one email with a list and a **Review all** link to `?action=queue&t=TOKEN` (Queue.html), and nothing when nothing is waiting.
  - The queue token (`review_tokens.kind = 'queue'`) is **reusable** for 14 days. The page has Approve / Reject per item plus Approve all, and opening it changes nothing.
  - Reviewers are the Expense Config `reviewers` tab: **billing@valtarealty.com** (user, 2026-09-23), scope `all`; **contact@valtahomes.com**, scope `account:valta-homes` (user, 2026-09-24). A deactivated reviewer's link stops working.
  - Single-submission links (kind blank) from emails sent before this change still work once (`decideReview`).
  - The weekly rhythm is: review on Friday, then run the reimbursement page for what was approved.
- **The form is served by the web app** (user, 2026-09-24: "the real form on web app" for others to test): the plain `/exec` link returns `Form.html` (`formPage_`, lists baked in as JSON, no second request); it calls `formApi(action, body)` through `google.script.run` (same actions/answers as doPost, and no POST redirect, so no "Not JSON (HTTP 404)"). `?action=ping` is the old JSON health check. `tools/test_form.html` still works against doPost. Local look: launch config `form-preview` (`tools/preview_form.js`, port 8789, fake google.script.run, nothing reaches Google). Remembers the submitter's name and "Paid with" per device (localStorage).
- **Every public server function is callable by any visitor** of any page this web app serves (`google.script.run` reaches every function without a trailing `_`). So every hand-run and trigger function starts with **`ownerOnly_(e)`** (Admin.gs): it passes in the editor (active user = billing@) or in the function's own trigger (`e.triggerUid` is one of the project's triggers), and throws for an anonymous visitor. `loadConfig_` is private (it holds reviewer emails). A new hand-run or trigger function MUST call `ownerOnly_(e)`; a test lists them.
- **Retries are safe by design** (user, 2026-09-24: an intermittent "Not JSON (HTTP 404)" from Google's front end, not reproducible). The form retries non-JSON replies; `submit` with the same `client_ref` (new `submissions.client_ref` column) returns the same id and rotates the upload token; the same bytes to the same submission return the saved file, even after completion; a repeated `complete` is ok. `tests/mock_server.js` with `FLAKY=1` (launch config mock-webapp-flaky) drops every other reply to exercise this.
- **Uploads:** the form gets an `upload_token` from `submit` and must present it for each `file` and for `complete`. Uploads are one file per request, at most 10 MB, photos or PDF, at most 10 files, within 24 h. `complete` requires at least one file and sends the emails.
- **Config = the "Expense Config" Google Sheet** (user, 2026-09-23), Script Property `CONFIG_SHEET_ID`. Tabs: properties, people, expense_categories, deposit_types, money_accounts, collection_methods, reviewers, `year_roots` (year | folder_id) and `settings` (expense_template, deposit_template, no_account_folder), which replace drive_layout.json. `setup()` never overwrites a tab that has content. The **properties tab is the exception**: it is replaced whenever a newer `config/properties.csv` appears in Drive, so hand edits there are lost (use properties_exclude.txt).
- **Drive iterators return trashed files.** Every by-name lookup (config files, name collisions, export replacement) skips `isTrashed()` files.
- **The export never runs inside a click** (user, 2026-09-24: approve was "really slow"; one export is 10–30 s). A review decision or the reimbursement page calls `scheduleExport_()`, which adds a one-off trigger for `exportLedgerSoon` about 1 minute later (several clicks share one run; it deletes its own trigger). There's also the daily 2am run. `exportLedger()` can be run by hand. Speed rules: `Ledger.update` writes one row range per call, `addFile` reads attachments once, and `"last": true` on the final file completes the submission (saves a request). It needs the `script.scriptapp` scope (triggers); `script.external_request` was dropped with the xlsx export.
- **Only reviewers are ever emailed. NEVER email a submitter or anyone else** (user, 2026-09-23). The plan's §5.1 "receipt to the submitter" was built at first, a cleaner (Griselda Ramirez) got a confirmation for a test entered under her name and replied, and the user objected strongly. It was removed, and a test asserts the outbox contains only the reviewer. Adding any recipient, or any message to people outside the office, needs the user's explicit OK first.
- **Mail failure never fails a submission.** It is logged as `mail_failed`.
- **Every email is sent as "Valta Expense Form" and ends with the footer "Sent by Valta Expense Form"** (`MAIL_FROM_NAME` / `MAIL_FOOTER` in Mail.gs). billing@'s Gmail filter matches that phrase to label the emails (user, 2026-09-23), so don't change either without updating the filter.
- **Deployed** as billing@: `https://script.google.com/macros/s/AKfycbxSPhD5YWZJhzI8XVH5Y0dPF7A6vT_rpupLz4GP2J5K0ZO6cfCwKm-6voDFbyMK8ic4/exec`. On 2026-09-23 an anonymous GET and `?action=config` worked (131 properties, 36 people, no emails leaked). A code change means `npx clasp push --force` (clasp is a local devDependency; `.clasp.json` holds script ID `115tBrGkdicIg7uOiqzG86IAnZzYtVKRTBhZl_IvoA3re3FD8tmCbBk89`, rootDir `apps_script`; the user's `clasp login` is billing@; never `clasp clone` over local files), **plus** Deploy → Manage deployments → Edit → New version. A config change means editing the Expense Config sheet (live within 5 min, the cache).

## Open questions (ask the user, don't guess)

1. Has the folder been shared with billing@ as Editor and added as a shortcut in billing@'s My Drive? Has the vacation@ deployment been archived?
2. The folder name for owner-paid receipts (currently "Owner paid"), and whether `Maintainence` in `expense_categories.csv` is a duplicate of `Maintenance`
3. The large-amount alert threshold (§5.1), and whether the accountant gets a daily summary (§5.3)
4. ~~QBO account names~~ DONE 2026-09-24 (`fixConfig20260924b`): 7197 `Chase Checking 7197`, 3038 `Chase Trust Checking 3038 - monthly`, 7439 `Credit Card 7439`, 3104 **and 6305** `CitiCostco3104-Supplies`, 5565 `Chase Credit Card 5565`, 4783 `Business Credit Card (4783)`. Amazon 1006 is NOT offered: that card was transferred to 4783 (user). QBO also has `Brian Credit 5565 - 2696` / `Rachel Credit 5565 - 6850`, deliberately not offered (user).
5. Not built yet: recording cash bank deposits, the `imports/` write-back, and the monthly report (§6). QBO_operations builders exist (2026-09-24): `build_zelle` (reimbursements), `deposits/build_guest_fees` (pet/parking), `invoices/build_form_expenses` (card/bank purchases: one Purchase each, vendor = store via payees.yml, DocNumber `EF<id>`) — none run live yet.
