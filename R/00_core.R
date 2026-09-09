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

# ---- CARTO basemap API key -------------------------------------------
# CARTO now require a key on their raster tiles; without one the tiles
# carry an "API key required" watermark. leaflet-providers has no slot
# for a key, so this wraps addProviderTiles(): CartoDB providers are
# rebuilt as a keyed addTiles() call and everything else is passed
# straight through to leaflet. Attribution is set explicitly, because
# addTiles() does not inherit the provider's and keeping the CARTO and
# OpenStreetMap credits visible is a condition of the free tier.
# The key is read from CARTO_KEY in ~/.Renviron. If it is unset the maps
# still render, just watermarked, so clones and CI are unaffected.
addProviderTiles <- function(map, provider, ...) {
  variants <- c(
    Positron             = "light_all",
    PositronNoLabels     = "light_nolabels",
    PositronOnlyLabels   = "light_only_labels",
    DarkMatter           = "dark_all",
    DarkMatterNoLabels   = "dark_nolabels",
    DarkMatterOnlyLabels = "dark_only_labels",
    Voyager              = "rastertiles/voyager",
    VoyagerNoLabels      = "rastertiles/voyager_nolabels",
    VoyagerOnlyLabels    = "rastertiles/voyager_only_labels",
    VoyagerLabelsUnder   = "rastertiles/voyager_labels_under")
  key <- Sys.getenv("CARTO_KEY", "")
  nm  <- as.character(provider)[1]
  if (startsWith(nm, "CartoDB.")) nm <- substring(nm, 9L)
  if (!nzchar(key) || !nm %in% names(variants))
    return(leaflet::addProviderTiles(map, provider, ...))
  dots <- list(...)
  opts <- leaflet::tileOptions(subdomains = "abcd", maxZoom = 20)
  if (!is.null(dots$options)) {
    opts <- utils::modifyList(opts, dots$options); dots$options <- NULL
  }
  do.call(leaflet::addTiles, c(list(
    map,
    urlTemplate = sprintf(
      "https://{s}.basemaps.cartocdn.com/%s/{z}/{x}/{y}{r}.png?key=%s",
      variants[[nm]], key),
    attribution = paste0(
      '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
      ' contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'),
    options = opts), dots))
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
