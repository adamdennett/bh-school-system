---
title: "Brighton & Hove’s secondary school system"
image: docs/model-guide_files/figure-html/fig-cj-map-1.png
---

# The Brighton Secondary School System — A Strategic View

A single document assembling what can be established about Brighton &
Hove's secondary school system from **published data only**, plus a
slide deck drawn from it.

No pupil-level record is used anywhere. Every figure comes from data the
council, the DfE or the ONS has already published, or from a model built
on top of it.

## What is here

| Path | What it is |
|---|---|
| `index.qmd` | The landing page |
| `analysis.qmd` | The analysis — the main document |
| `model-guide.qmd` | The technical guide to every model term |
| `slides.qmd` | The deck |
| `brighton.scss` | Site theme. Leads with UCL's heritage blue rather than the purples, so it reads apart from *How to Pull the Right Lever* |
| `assets/hero-cj-map.png` | Landing-page hero image, cropped from the competing-destinations figure |
| `R/00_core.R` | Paths, palettes, school lookups, map helpers |
| `R/01_assemble.R` | Pulls every input into `data/`. Run once |
| `R/02_accessibility.R` | New analysis: LSOA accessibility, two measures |
| `R/03_flow_regions.R` | Catchments regionalised from the modelled flows |
| `R/extract_foi_allocations.py` | Turns the FOI 26370589 allocation maps into points. The only Python here; its CSV is committed so the R side never needs it |
| `R/17_foi_allocations.R` | Those points as data: joined to LSOA, and checked against the published factsheets |
| `R/18_foi_model_check.R` | The FOI origins against the model’s own flows — the first independent test of the geography the model was never fitted to |
| `R/20_sibling_share.R` | Priority 3, the sibling link: each community school’s share from the council’s 2026 outturn. `R/05_app_inputs.R` uses it to build the sibling tier into the model (October 2026) |
| `R/21_sibling_refit.R` | Re-runs the free school meal fits with and without the sibling tier, checks both against the bundle, and scores them against the outturn |
| `data/` | Self-contained inputs |
| `docs/` | Rendered site, served by GitHub Pages |

## Building it

`R/01_assemble.R` is the only script that reaches outside this
repository. It needs the upstream projects present on the same machine:

- `BH_Pupil_Destinations/public/output` — the open Brightopia bundle
- `school_attainment_tool` — catchment boundaries, school logos, the
  DfE performance panel behind *How to Pull the Right Lever*
- `BH_Schools_2` — pre-2024 catchment boundaries
- `BH_Schools_Consultation` — national LSOA boundary geometry

```r
source("R/01_assemble.R")      # once, to populate data/
source("R/02_accessibility.R") # once, after assembly
source("R/03_flow_regions.R")  # once, after 02
quarto::quarto_render()        # or: quarto render
source("R/99_verify_render.R") # check the output before publishing
```

## Basemaps

Maps use **Esri's World Light Gray Canvas**, which needs no API key.
Nothing has to be set up to render them.

This replaced CARTO Positron, whose raster tiles are keyed. That failed
repeatedly and always *quietly*: a render from a shell that did not have
`CARTO_KEY` in its environment produced watermarked tiles, and the only
symptom was a watermark someone had to notice. Of the keyless
alternatives, Esri's is the one that is actually keyless — Stadia's
tiles now return 401 without a key and Jawg's return 400.

**Every map calls `add_basemap()`** from `R/00_core.R` — never
`leaflet::addProviderTiles()`, which is stubbed there to raise an error.
Going through one function matters for two reasons: leaflet-providers
caps this canvas at zoom 16 rather than upsampling it, so deep-zoom maps
would show a grey wash; and its CARTO entries still render watermarked.

`R/99_verify_render.R` checks the rendered output. It fails if a page
has no basemap, if the canvas and label layers do not pair up, if any
CARTO tile URL survives, if `leaflet-providers` has been loaded, if
`maxNativeZoom` is missing, or if either required attribution is gone.

Once `data/` is populated the repository is self-contained and renders
anywhere.

## Three vocabularies for six catchments

The boundary file, the model tables and the council's published
forecasts each name the six catchments differently. Left unreconciled
this does not error — it silently drops the two paired catchments from
any join. Every crossing goes through `as_boundary_catchment()` or
`as_model_catchment()` in `R/00_core.R`, both of which fail loudly on an
unmapped key.

## Related work

- [How to Pull the Right Lever](https://adamdennett.github.io/school_attainment_tool/index.html) — contextualised attainment analysis
- [The open Brightopia model](https://adamdennett.github.io/bh-school-model/) — the technical companion to section 7

Adam Dennett, UCL Centre for Advanced Spatial Analysis.

## Deploying the simulator

The live app is at <https://apps.adamdennett.co.uk/bh_school_system/>,
served from a Shiny Server in Docker on the NAS. A `rocker/shiny` container
serves every folder in the NAS's `web/apps/` directory, mounted read-only, and
a Cloudflare tunnel publishes it at `apps.adamdennett.co.uk`. Pushing to
GitHub does **not** update the app. To deploy, copy `app/` over to the NAS
share (mapped over Tailscale as `N:`), then touch `restart.txt` so Shiny
Server reloads the app on the next visit:

```powershell
robocopy app N:\web\apps\bh_school_system /MIR
New-Item N:\web\apps\bh_school_system\restart.txt -ItemType File -Force
```

`/MIR` makes the NAS copy match `app/` exactly, deleting files removed here.
The container installs R packages from the list in `web/Dockerfile` on the
NAS. If the app starts using a new package, add it there and rebuild the
`shiny` service (`docker compose up -d --build` in `web/`) before copying the
app across.

`app/manifest.json` and `app/rsconnect/` are kept as a fallback, for
redeploying to Posit Connect Cloud or shinyapps.io. Refresh the manifest with
`rsconnect::writeManifest("app")` only if you go back to one of those hosts.
