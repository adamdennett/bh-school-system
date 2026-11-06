# TODO: print, save and reload a scenario in the policy simulator

Written 2026-11-06. Nothing here is built yet. To run it, open a session in
this repo and say: "Do `SCENARIO_SAVE_PRINT_TODO.md`, starting at Stage 1."
Delete this file once the work is finished.

**Progress:** Stages 1 to 3 are built and tested locally: `app/R/report.R`,
`app/R/state.R`, and the print, save, load and share controls in the sidebar.
The export gaps below are closed too. `p_catch_map` turned out to be a bar
chart, so the report draws its own map (`static_map()`).

**Still to do:** deploy the app, then open a share link on shinyapps.io. Links
could only be tested on localhost, and they rely on shinyapps.io passing `?s=`
through to the app. Delete this file once that works.

## What Adam asked for

Once someone has moved the levers and has a scenario worth sharing, they
should be able to:

1. **Print it.** This is the priority.
2. **Save it**, with every element written to a document or CSV.
3. **Load it back in** later.
4. Ideally, **email it or put a copy on Google Drive**.

## Where things stand

The app is `app/app.R`, deployed to
<https://adam-dennett.shinyapps.io/brighton-school-simulator/>.

**Already built:** the "Download this run (Excel)" button (`app/app.R:326`,
handler at `:645`). It calls `scenario_export()` in `app/R/export.R`, which
returns a named list of data frames, and `writexl::write_xlsx()` writes them
out. The sheets are README, Scenario, Schools, Outside the city, Catchments,
Flows, Outcomes, Money, Fixed parameters and, when tiers exist, Priorities.
So "save each element" is largely done. **Reuse this and don't build a second
export path.**

**Missing:** a printable report, a scenario file that can be reloaded, and any
way to share.

## Constraints

- **shinyapps.io disk is temporary.** Anything written on the server is lost
  when the instance restarts, so saving has to mean a file in the user's hands
  or a link, not a server-side store.
- **The user's browser does the printing.** Avoid anything needing LaTeX,
  Chrome or pandoc on the server. A self-contained HTML report needs none of
  them.
- **Printing the live page won't work.** The tabs hide most of the content and
  the sidebar would swamp it, so the report has to be built separately.
- **The Leaflet map can't be captured on the server.** It is drawn in the
  browser by `www/map.js`. Use the static `p_catch_map` plot in the report.
- **Keep the warnings on every output.** That means "Every number here is
  modelled", the BETA status text already in `export.R`, and the "What this is
  not" caveats. A printed page gets passed around without the app around it.
- **This is a public repo.** Follow the existing rules on what public
  simulator code, comments and commit messages may say about data sources.
- **Paths:** no absolute drive paths in code (see `CLAUDE.md`).

## Stage 1: Print (do this first)

Add a "Print this scenario" button next to the Excel button. It downloads one
self-contained HTML file that opens in any browser, with a print stylesheet so
Ctrl+P gives clean A4 pages or a PDF.

Report contents, in order:

1. Title, base scenario, its note, date generated, and the modelled/BETA
   warning.
2. An optional title and notes from the user: add a text input and a text area
   so they can say why the scenario matters.
3. **What was changed from the scenario**, shown first and on its own. This is
   the rows of the Scenario and Schools sheets marked "changed".
4. The six headline figures from the KPI strip.
5. Per-school table: places, attractiveness, intake, fill, and changes.
6. Charts: cohort, places, catchments, static catchment map, money, fairness,
   attainment, travel.
7. The caveats from the "What this is not" tab.
8. Full settings table, as an appendix.

How to build it:

- **Refactor the plots first.** Each chart is currently built inside its own
  `renderPlot()`. Move the plot code into `reactive()` builders (for example
  `plot_places <- reactive({...})`), so that `renderPlot()` and the report both
  use the same code and can't disagree. The same applies to tables and any
  text the report reuses. Most are ggplot; `p_catch` and `p_catch_map` have no
  direct `ggplot()` call, so check how they draw before moving them.
- Add `app/R/report.R` with a function such as
  `scenario_report(wb, plots, kpis, notes, file)`. Use `htmltools` (already a
  dependency) and embed each plot as a base64 PNG using `ragg` or `png()`, at
  about 150 dpi. Keep the file to a few MB.
- Add `@media print` CSS with `page-break-inside: avoid` on figures and tables.
- Show progress with `withProgress()`, as a dozen plots won't be instant.

Done when: changing several levers and clicking the button gives a file that
opens offline, prints cleanly to A4, and whose figures match the screen and the
Excel workbook for the same run.

## Stage 2: Save and load

**Save.** Add "Save scenario" to download a small JSON file holding the raw
input values plus a version number and the base preset name. The Excel
workbook can't serve as this file, because these settings aren't recoverable
from it:

| Gap in `scenario_export()` | Why it matters |
|---|---|
| `hp_site` isn't in `settings` or the workbook | Hove Park's campus would be lost on reload, and it is missing from the workbook today, so add it there as well |
| `gamma_now()` multiplies the city slider by the six catchment sliders | Only the product is exported, so the separate slider positions can't be recovered |
| `w_now()` is the slider value multiplied by the `w_basis` factor | Save `w_user()`, the raw slider value, or the factor gets applied twice on reload |

Fields to save: `preset`, `design`, `site`, `hp_site`, `w_basis`, `year`,
`gamma`, the six `gamma_*`, `exclusive`, `rule`, `p6`, `fsm`, `targeted`,
`siblings`, `sib_counts`, and per school (keyed by URN) `w_`, `pan_` and
`closed_`. Also save the user's title and notes.

**Check while here:** `scenario_export()` compares `w_used` (`w_now()`) with a
default that has no basis factor in it. Under "value added" this looks likely
to mark every school as "changed", so test it and fix it if so.

**Load.** Add `fileInput` and a new `apply_state()` modelled on `apply_preset()`
(`app/app.R:465`). This is the risky part, because of two ordering problems:

- `observeEvent(input$preset, ...)` applies the preset whenever the preset
  changes. Setting the preset from a loaded file would therefore overwrite the
  loaded values. Set it first and apply the saved values after that preset
  event has fired, or add a guard that suppresses it during a load.
- Admission numbers must go through `set_pans(base, final, closed)` and not
  direct updates. The `pending` logic (`app/app.R:404-463`) otherwise reads a
  half-applied set as a hand edit and rescales everything.

Validate on load: reject bad JSON or an unknown version with a clear message,
and warn, rather than fail, when a saved preset or school no longer exists.
The file is user input, so never `eval` or `source` anything from it, and clamp
values to each control's range.

Note that a saved scenario stores settings, not results. If the model is later
re-fitted, the same settings will give different numbers, so show a message on
load when the file's model version differs. This needs a model version string,
for instance a hash or date stamped into `sim_inputs.rds`.

**Tests.** Add to `app/tests/check.R`, or a sibling script, a round trip: save,
load, and confirm that the settings and every outcome match. Cover a run with
per-catchment gamma, value-added basis, a closed school, CoMArt open and a
pinned Hove Park campus. `shiny::testServer()` won't exercise browser update
echoes, so also try a real load in the running app.

Done when: a file saved from a heavily edited scenario reloads in a fresh
session and reproduces the same headline figures.

## Stage 3: Sharing

Recommended, because it needs no accounts, secrets or server storage:

- **Share link.** Encode the Stage 2 JSON into the URL, for example
  `?s=<base64>`, and read it at startup with `parseQueryString()`, going
  through the same `apply_state()`. The link *is* the scenario: nothing is
  stored and it can be pasted anywhere. Check the URL length with every lever
  moved, and compress or send only the differences from the preset if needed.
- **Email.** A "mailto:" button that opens the user's own email with the link
  and a short summary already filled in. A mailto link can't attach a file, so
  anyone wanting the PDF attaches it themselves.
- **Google Drive.** Users save the download to Drive themselves, as with any
  other file.

Not recommended without a further decision from Adam:

- **Sending email from the server** needs an SMTP or API key on shinyapps.io,
  and a public form that sends mail can be abused. It would also mean collecting
  email addresses from the public.
- **Direct Drive upload** means either a Google OAuth app, with consent screens
  and Google's verification, or a service account writing to Adam's Drive, which
  lets anonymous visitors add files to it.

If a central collection of submitted scenarios is what's really wanted, that's
a separate design question and worth talking through before building anything.

## Finishing each stage

1. Run `Rscript app/tests/check.R`. It must still show agreement with the
   published figures. Rscript isn't on the Git Bash PATH on the main machine:
   R is at `C:\Program Files\R\R-4.5.2`.
2. Test in the running app (`shiny::runApp("app")`).
3. Update the file table in `app/README.md` and the note under the buttons.
4. Regenerate `app/manifest.json` if a package was added, and check that the
   new package is available on shinyapps.io.
5. Commit and push, staging named files only. The working tree holds unrelated
   uncommitted items that shouldn't go in by accident: `.docx` and `.pdf`
   drafts, `data/*.rds` changes and the `.dcf` file.
6. Redeploy only if Adam asks.

## Decisions for Adam

- Is the HTML report with print-to-PDF enough, or does it need to be a real
  `.docx` or PDF download?
- Is Stage 3's link and mailto approach enough, or is a central store of
  scenarios wanted?
