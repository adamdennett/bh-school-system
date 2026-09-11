# R/00_core.R — shared setup for the strategic view
# ======================================================================
# Paths, palettes, school lookups and map helpers used by every other
# script and by index.qmd. Sourced, never run on its own.
#
# This repository is self-contained: everything it needs lives in data/,
# put there by R/01_assemble.R. Nothing here reads pupil-level records.
# ======================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(sf); library(leaflet)
})

ROOT <- here::here()
DATA <- file.path(ROOT, "data")
LOGOS <- file.path(ROOT, "assets", "school_logos")

# ---- Basemaps --------------------------------------------------------
# Esri's World Light Gray Canvas, which needs no API key.
#
# This was CARTO Positron, and CARTO's raster tiles require a key: every
# clone, every CI run and every render from a shell that did not have
# CARTO_KEY in its environment produced watermarked tiles with no other
# symptom. That happened repeatedly and was caught only by a check
# written specially to catch it. A basemap that cannot be misconfigured
# is worth more than a slightly prettier one.
#
# Of the keyless light-canvas options, this is the one that actually is
# keyless. Stadia's tiles return 401 without a key and Jawg's return
# 400; both moved to requiring one.
#
# TWO LAYERS, because Esri splits what CARTO ships as one. The base
# carries the land, water and roads; the reference carries the place
# names. Positron has the labels baked in, so both are added and the
# result matches what these maps looked like before.
#
# MAX NATIVE ZOOM 16, which is the constraint that matters. Esri
# publishes this canvas to level 16, and above that the server returns a
# grey tile reading "Map data not yet available" rather than a 404 - so
# the failure would be a wash of grey with no error anywhere.
# maxNativeZoom tells Leaflet to upsample level 16 instead of asking for
# 17, which keeps the postcode map in section 2 usable at the depth a
# reader will want it. The basemap goes soft past 16; the markers and
# boundaries drawn over it stay sharp.

ESRI_TILE <- paste0("https://server.arcgisonline.com/ArcGIS/rest/services/",
                    "Canvas/World_Light_Gray_%s/MapServer/tile/{z}/{y}/{x}")
ESRI_MAX_NATIVE <- 16

ESRI_ATTRIB <- paste0(
  'Tiles &copy; <a href="https://www.esri.com/">Esri</a> &mdash; Esri, ',
  'HERE, Garmin, &copy; ',
  '<a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
  ' contributors')

#' Add the basemap
#'
#' @param map a leaflet map
#' @param labels whether to draw place names over the canvas (default
#'   TRUE, matching the single labelled layer this replaced)
#' @param ... passed to leaflet::addTiles (group, layerId, options)
add_basemap <- function(map, labels = TRUE, ...) {
  dots <- list(...)
  opts <- leaflet::tileOptions(maxNativeZoom = ESRI_MAX_NATIVE, maxZoom = 20)
  if (!is.null(dots$options)) {
    opts <- utils::modifyList(opts, dots$options); dots$options <- NULL
  }

  map <- do.call(leaflet::addTiles, c(list(
    map,
    urlTemplate = sprintf(ESRI_TILE, "Base"),
    attribution = ESRI_ATTRIB,
    options = opts), dots))

  if (!labels)
    return(map)

  # The label layer carries no attribution of its own: repeating it
  # would print the same credit twice in the corner of every map.
  do.call(leaflet::addTiles, c(list(
    map,
    urlTemplate = sprintf(ESRI_TILE, "Reference"),
    attribution = "",
    options = opts), dots))
}

# A tripwire. leaflet-providers' own Esri.WorldGrayCanvas caps the map
# at zoom 16 rather than upsampling past it, and its CARTO entries
# render watermarked; either would be a silent visual regression, so
# route every map through one function.
addProviderTiles <- function(map, provider, ...) {
  stop("Use add_basemap() in this project, not addProviderTiles().\n",
       "  leaflet-providers caps this canvas at zoom 16 instead of ",
       "upsampling it, and its CARTO entries render watermarked.\n",
       "  See R/00_core.R.", call. = FALSE)
}
# ----------------------------------------------------------------------

# ---- Catchments ------------------------------------------------------
# The six geographic catchments. Two of them contain two schools each,
# which is the fact most of the equity argument in section 5 turns on.

CATCH_COLOURS <- c(
  "BACA"             = "#e41a1c",
  "HoveBlatchington" = "#377eb8",
  "Longhill"         = "#4daf4a",
  "PACA"             = "#984ea3",
  "Patcham"          = "#ff7f00",
  "VarndeanStringer" = "#a65628")

CATCH_LABELS <- c(
  "BACA"             = "BACA (Brighton Aldridge)",
  "HoveBlatchington" = "Hove Park / Blatchington Mill",
  "Longhill"         = "Longhill",
  "PACA"             = "PACA (Portslade Aldridge)",
  "Patcham"          = "Patcham",
  "VarndeanStringer" = "Varndean / Dorothy Stringer")

# The boundary file and the model tables name the same six places
# differently. Left unreconciled this does not error - it silently drops
# the two paired catchments from every join, which is exactly the bug
# that produced two wrong figures in an earlier version of this work.
# Convert explicitly, and assert that nothing fell through.
CATCH_FROM_MODEL <- c(
  "BACA"        = "BACA",
  "Hove_Blatch" = "HoveBlatchington",
  "Longhill"    = "Longhill",
  "PACA"        = "PACA",
  "Patcham"     = "Patcham",
  "DS_Varndean" = "VarndeanStringer")

# Peacehaven is a real catchment in the model but has no polygon in the
# city's boundary file, because it is in Lewes district. That is a
# legitimate absence rather than a mapping error, so it is listed here
# and returns NA quietly; anything else returning NA is a bug.
CATCH_NO_BOUNDARY <- c("Peacehaven")

# A third naming scheme: the council's published forecasts label the six
# catchments with display names. Three vocabularies for six places is
# three chances to lose the two paired catchments in a silent join, so
# every crossing between them goes through one of these functions.
CATCH_FROM_COUNCIL <- c(
  "BACA"                          = "BACA",
  "Hove Park / Blatchington Mill" = "Hove_Blatch",
  "Longhill"                      = "Longhill",
  "PACA"                          = "PACA",
  "Patcham"                       = "Patcham",
  "Varndean / Dorothy Stringer"   = "DS_Varndean")

# The faith schools are grouped as "Religious schools" in the cohort
# tables and have no catchment forecast at all, because they admit
# city-wide. A legitimate absence, not a mapping failure.
CATCH_NO_COUNCIL <- c("Religious schools")

#' Convert council display labels to model-side catchment keys.
as_model_catchment <- function(x) {
  x <- as.character(x)
  out <- unname(CATCH_FROM_COUNCIL[x])
  bad <- !is.na(x) & is.na(out) & !x %in% CATCH_NO_COUNCIL
  if (any(bad))
    stop("unmapped council catchment label(s): ",
         paste(unique(x[bad]), collapse = ", "))
  out
}

#' Convert boundary-file keys back to model-side keys.
#'
#' The third direction. There were converters for model -> boundary and
#' for council label -> model, and reading the boundary file straight
#' into model code needed this one; reaching for as_model_catchment()
#' instead fails, because that takes the council's display labels
#' ("Varndean / Dorothy Stringer") and the boundary file uses its own
#' ("VarndeanStringer").
as_model_from_boundary <- function(x) {
  x <- as.character(x)
  inv <- setNames(names(CATCH_FROM_MODEL), unname(CATCH_FROM_MODEL))
  out <- unname(inv[x])
  bad <- !is.na(x) & is.na(out)
  if (any(bad))
    stop("unmapped boundary catchment key(s): ",
         paste(unique(x[bad]), collapse = ", "))
  out
}

#' Convert model-side catchment keys to boundary-file keys.
#' @param x character vector of model keys. NA is allowed: the two faith
#'   schools admit city-wide and sit outside the geographic framework.
as_boundary_catchment <- function(x) {
  x <- as.character(x)
  out <- unname(CATCH_FROM_MODEL[x])
  bad <- !is.na(x) & is.na(out) & !x %in% CATCH_NO_BOUNDARY
  if (any(bad))
    stop("unmapped catchment key(s): ",
         paste(unique(x[bad]), collapse = ", "),
         "\nAdd to CATCH_FROM_MODEL, or to CATCH_NO_BOUNDARY if the ",
         "catchment genuinely has no polygon in the boundary file.")
  out
}

# ---- Schools ---------------------------------------------------------
# Keyed on URN, not name: the schools table says "Hove Park School" and
# the DfE performance tables say "Hove Park School and Sixth Form
# Centre", so a name-based lookup silently loses that school's logo.
# Peacehaven Community School (144661) is in the model but outside the
# authority, and has no logo here.

LOGO_FILES <- c(
  "114579" = "varndean.png",
  "114580" = "dorothy_stringer.png",
  "114581" = "longhill_high.png",
  "114606" = "blatchington_mill.png",
  "114607" = "hove_park.png",
  "114608" = "patcham_high.png",
  "114611" = "cardinal_newman.png",
  "136164" = "brighton_aldridge.png",
  "137063" = "portslade_aldridge.png",
  "139409" = "kings_school.png")

# CoMArt closed in 2005. Its closure is why the city has catchments with
# a lottery tie-break at all, so it belongs on the orientation map even
# though it has not admitted a pupil in twenty years.
COMART <- list(lon = -0.099449, lat = 50.823301,
               name = "CoMArt (closed 2005)",
               logo = "comart.png")

#' Leaflet icon for a school logo, embedded as a data URI so the
#' rendered HTML is self-contained.
logo_icon <- function(file, size = 36) {
  p <- file.path(LOGOS, file)
  if (!file.exists(p)) return(NULL)
  leaflet::makeIcon(iconUrl = knitr::image_uri(p),
                    iconWidth = size, iconHeight = size,
                    iconAnchorX = -4, iconAnchorY = size + 4)
}

#' A school's name short enough to label a point or head a table row
#'
#' "Portslade Aldridge Community Academy" is four words too long for a
#' chart label and the distinguishing part is always the first one or
#' two. The same trim was written out by hand in half a dozen chunks
#' before it moved here.
short_sch <- function(x)
  stringr::str_remove(
    x, " (School|High School|Community Academy|Catholic School).*")

#' The eleven schools as an sf, in WGS84, with lon/lat columns and a
#' catchment colour attached.
schools_sf <- function(schools = NULL) {
  if (is.null(schools)) schools <- bh_data("open_inputs.rds")$schools
  s <- schools %>%
    dplyr::filter(!is.na(easting), !is.na(northing)) %>%
    sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
    sf::st_transform(4326)
  xy <- sf::st_coordinates(s)
  s$lon <- xy[, 1]; s$lat <- xy[, 2]
  s$boundary_catchment <- as_boundary_catchment(s$catchment)
  s$marker_col <- ifelse(is.na(s$boundary_catchment), "#444444",
                         unname(CATCH_COLOURS[s$boundary_catchment]))
  s
}

#' Add the schools to a leaflet map: a coloured dot per school with its
#' logo above it, both in one layer group so a layers control toggles
#' the pair together.
#'
#' @param map a leaflet map
#' @param s output of schools_sf()
#' @param group layer group name
#' @param logos draw the logo markers as well as the dots
#' @param radius dot radius in pixels
#' @param popup optional character vector, one per school
add_school_layer <- function(map, s = schools_sf(), group = "Schools",
                             logos = TRUE, radius = 6, popup = NULL,
                             logo_size = 36) {
  for (i in seq_len(nrow(s))) {
    row <- s[i, ]
    pu  <- if (is.null(popup)) NULL else popup[i]
    map <- leaflet::addCircleMarkers(
      map, lng = row$lon, lat = row$lat, group = group,
      radius = radius, fillColor = row$marker_col, fillOpacity = 0.95,
      color = "white", weight = 2, popup = pu, label = row$name)
    if (logos) {
      ic <- logo_icon(LOGO_FILES[as.character(row$urn)], size = logo_size)
      if (!is.null(ic))
        map <- leaflet::addMarkers(map, lng = row$lon, lat = row$lat,
                                   icon = ic, group = group,
                                   popup = pu, label = row$name)
    }
  }
  map
}

# The register of what this document is built from, and the scan that
# works out which sections use which of it. Kept in its own file because
# it is a table of metadata rather than code.
source(file.path(ROOT, "R", "00_sources.R"))

# ---- Formatting ------------------------------------------------------
# These were defined in index.qmd, which is fine until something other
# than index.qmd needs them - R/05_app_inputs.R and the app both do.
# index.qmd still defines them identically; harmless, and it saves a
# re-render of a 6,000-line document to remove two lines.

fmt_n   <- function(x, d = 0)
  formatC(round(x, d), big.mark = ",", format = "f", digits = d)

fmt_pct <- function(x, d = 0)
  paste0(formatC(x, format = "f", digits = d), "%")

#' Pounds, at whatever scale reads best, with a typographic minus.
gbp <- function(x, d = 1) {
  a <- abs(x); s <- ifelse(x < 0, "\u2212", "")
  ifelse(a >= 1e6, sprintf("%s\u00a3%.*fm", s, d, a / 1e6),
  ifelse(a >= 1e3, sprintf("%s\u00a3%.0fk", s, a / 1e3),
                   sprintf("%s\u00a3%.0f", s, a)))
}

# ---- Plot theme ------------------------------------------------------

theme_bh <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey30"),
          plot.caption = element_text(colour = "grey45", hjust = 0),
          strip.text = element_text(face = "bold"))
}

#' Read one of the assembled data objects from data/.
bh_data <- function(name) {
  p <- file.path(DATA, name)
  if (!file.exists(p)) stop("missing data file: ", name,
                            " - run R/01_assemble.R first")
  if (grepl("[.]rds$", name)) readRDS(p)
  else if (grepl("[.]csv$", name)) readr::read_csv(p, show_col_types = FALSE)
  else sf::st_read(p, quiet = TRUE)
}
