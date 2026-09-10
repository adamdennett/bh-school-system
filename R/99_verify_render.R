# R/99_verify_render.R — check the rendered site before publishing
# ======================================================================
# The CARTO basemap key is added by add_carto() in R/00_core.R, which is
# called explicitly at every map. That replaced a shim that shadowed
# leaflet::addProviderTiles() and twice failed silently - the real
# function ran, the maps rendered, and the only symptom was a watermark
# that had to be spotted by eye.
#
# The explicit call makes that failure mode much harder to reach, but
# this check stays: it costs nothing, and it also guards the attribution
# that the free tier is conditional on. Check the artefact rather than
# trusting the process. Run after any render, before publishing:
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
  # leaflet only ships these when its own addProviderTiles() ran, which
  # should now be impossible - R/00_core.R stubs that function to error.
  # If it appears, something is bypassing 00_core.R altogether.
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
    fail <- c(fail, sprintf(
      "%s: leaflet-providers is loaded, so a map bypassed add_carto()", base))
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
  message("\nMost likely cause: CARTO_KEY unset in ~/.Renviron. Check with ",
          "Sys.getenv(\"CARTO_KEY\") and re-render. If leaflet-providers ",
          "is loaded, a map is calling leaflet's addProviderTiles() ",
          "instead of add_carto() from R/00_core.R.")
  quit(status = 1)
}

message("\nAll rendered pages carry a keyed CARTO basemap and both attributions.")
