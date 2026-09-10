# R/99_verify_render.R — check the rendered site before publishing
# ======================================================================
# The CARTO basemap key reaches the page through a shim that shadows
# leaflet::addProviderTiles(). Shadowing fails quietly: if the shim does
# not take effect, the real function runs, the tiles come back
# watermarked, and nothing in the render output says so. That has now
# happened twice, both times noticed only by looking at the map.
#
# So check the artefact rather than trusting the process. Run after any
# render, before committing or publishing:
#
#   source("R/99_verify_render.R")
#
# Exits non-zero on failure so it can gate a publish step.
# ======================================================================

ROOT <- here::here()
files <- list.files(file.path(ROOT, "docs"), pattern = "[.]html$",
                    full.names = TRUE, recursive = FALSE)

if (!length(files)) stop("no rendered HTML found in docs/ - render first")

fail <- character(0)
note <- function(...) message("  ", ...)

for (f in files) {
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  base <- basename(f)

  keyed   <- lengths(regmatches(h, gregexpr("key=cb1_", h, fixed = TRUE)))
  unkeyed <- lengths(regmatches(
    h, gregexpr("basemaps\\.cartocdn\\.com/[a-z_/]*/\\{z\\}/\\{x\\}/\\{y\\}\\{?r?\\}?\\.png\"", h)))
  # leaflet only ships these when the real addProviderTiles ran, which
  # is the signature of the shim having been bypassed.
  providers <- grepl("leaflet-providers", h, fixed = TRUE)

  carto <- keyed > 0 || unkeyed > 0 || providers

  if (!carto) {
    note(base, ": no CARTO basemaps found - nothing to check")
    next
  }

  note(sprintf("%s: %d keyed tile layer(s), %d unkeyed, leaflet-providers %s",
               base, keyed, unkeyed, ifelse(providers, "PRESENT", "absent")))

  if (unkeyed > 0)
    fail <- c(fail, sprintf("%s: %d CARTO tile layer(s) without an API key", base, unkeyed))
  if (providers)
    fail <- c(fail, sprintf("%s: leaflet-providers is loaded, so the shim was bypassed", base))
  if (keyed == 0)
    fail <- c(fail, sprintf("%s: no keyed CARTO tile layer at all", base))

  # The free tier is conditional on these staying visible.
  for (a in c("carto.com/attributions", "openstreetmap.org/copyright"))
    if (!grepl(a, h, fixed = TRUE))
      fail <- c(fail, sprintf("%s: missing attribution (%s)", base, a))
}

if (length(fail)) {
  message("\nFAILED:")
  for (f in fail) message("  - ", f)
  message("\nMost likely cause: CARTO_KEY unset in ~/.Renviron, or the render ",
          "did not pick up the shim in R/00_core.R. Re-render with ",
          "`quarto render` rather than through a running preview server, ",
          "and check Sys.getenv(\"CARTO_KEY\").")
  quit(status = 1)
}

message("\nAll rendered pages carry a keyed CARTO basemap and both attributions.")
