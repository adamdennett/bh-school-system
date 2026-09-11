# R/99_verify_render.R — check the rendered site before publishing
# ======================================================================
# The basemap is added by add_basemap() in R/00_core.R, called
# explicitly at every map. This checks the rendered artefact rather than
# trusting the process, because the basemap's failure modes are all
# silent: the map renders, and the only symptom is something a reader
# has to notice by eye.
#
# It used to check for a CARTO API key. The basemap is now Esri's World
# Light Gray Canvas, which needs no key, so that failure mode is gone -
# but three others are not:
#
#   no basemap at all        a map that never called add_basemap()
#   leftover CARTO tiles     a map still on the old unkeyed URL, which
#                            renders watermarked
#   missing attribution      Esri's terms require the credit, and
#                            OpenStreetMap's licence requires theirs
#
# Run after any render, before publishing:
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

count <- function(h, pat, fixed = TRUE)
  lengths(regmatches(h, gregexpr(pat, h, fixed = fixed)))

for (f in files) {
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  base <- basename(f)

  esri  <- count(h, "World_Light_Gray_Base")
  lab   <- count(h, "World_Light_Gray_Reference")
  carto <- count(h, "basemaps.cartocdn.com")
  # leaflet only ships this when its own addProviderTiles() ran, which
  # should be impossible - R/00_core.R stubs that function to error. If
  # it appears, something is bypassing 00_core.R altogether.
  providers <- grepl("leaflet-providers", h, fixed = TRUE)
  has_map <- grepl("leaflet", h, fixed = TRUE) &&
             grepl("addTiles", h, fixed = TRUE)

  if (!has_map && esri == 0 && carto == 0 && !providers) {
    note(base, ": no leaflet maps - nothing to check")
    next
  }

  note(sprintf("%s: %d canvas layer(s), %d label layer(s), %d CARTO, leaflet-providers %s",
               base, esri, lab, carto, ifelse(providers, "PRESENT", "absent")))

  if (esri == 0)
    fail <- c(fail, sprintf("%s: no basemap tiles at all", base))
  if (lab != esri)
    fail <- c(fail, sprintf(
      "%s: %d canvas layers but %d label layers - a map is missing one half",
      base, esri, lab))
  if (carto > 0)
    fail <- c(fail, sprintf(
      "%s: %d leftover CARTO tile layer(s), which render watermarked", base, carto))
  if (providers)
    fail <- c(fail, sprintf(
      "%s: leaflet-providers is loaded, so a map bypassed add_basemap()", base))

  # Esri's terms require the first; OpenStreetMap's licence the second.
  for (a in c("esri.com", "openstreetmap.org/copyright"))
    if (!grepl(a, h, fixed = TRUE))
      fail <- c(fail, sprintf("%s: missing attribution (%s)", base, a))

  # Above zoom 16 the tile server returns a grey "Map data not yet
  # available" image rather than a 404, so without maxNativeZoom a deep
  # zoom would show that instead of a basemap, silently.
  if (esri > 0 && !grepl("maxNativeZoom", h, fixed = TRUE))
    fail <- c(fail, sprintf(
      "%s: no maxNativeZoom set - past zoom 16 the tiles read 'Map data not yet available'",
      base))
}

if (length(fail)) {
  message("\nFAILED:")
  for (f in fail) message("  - ", f)
  message("\nMost likely cause: a map is calling leaflet's own ",
          "addProviderTiles() or addTiles() instead of add_basemap() ",
          "from R/00_core.R.")
  quit(status = 1)
}

message("\nAll rendered pages carry the Esri canvas basemap and both attributions.")
