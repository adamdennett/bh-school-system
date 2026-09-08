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
| `index.qmd` | The analysis page |
| `slides.qmd` | The deck |
| `R/00_core.R` | Paths, palettes, school lookups, map helpers |
| `R/01_assemble.R` | Pulls every input into `data/`. Run once |
| `R/02_accessibility.R` | New analysis: LSOA accessibility, two measures |
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
quarto::quarto_render()        # or: quarto render
```

Once `data/` is populated the repository is self-contained and renders
anywhere.

## Basemaps

Maps use CARTO raster tiles, which now require an API key. Set
`CARTO_KEY` in `~/.Renviron`. Without it the maps still render, just
with a watermark.

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
