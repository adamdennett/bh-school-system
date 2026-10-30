# 17_foi_allocations.R - the FOI allocation maps, as usable data
# ---------------------------------------------------------------------------
# Brighton & Hove released eight maps under FOI 26370589 on 29 October 2026,
# in answer to a request about what data councillors had been given on the
# impact of the admissions changes. Each map shows, for one community
# school, where the children allocated to it on National Offer Day live,
# coloured by the preference rank that got them the place. The request and
# the attachments are public:
#
#   https://www.whatdotheyknow.com/request/evaluation_of_secondary_school_a
#
# R/extract_foi_allocations.py turns those pictures into points; this script
# turns the points into something the rest of the repository can use, and
# checks them against a source the extraction never saw - the council's own
# allocation factsheets.
#
# WHAT THE NUMBERS ARE WORTH. Three separate things, with very different
# reliability, and they should not be confused:
#
#   Position   Good. The grid ticks are live text in the PDF, so the
#              page-to-grid fit is sub-metre, and the raster is about 5 m a
#              pixel. A lone symbol's centre is good to a few metres.
#   Rank       Good. Each symbol colour is a legend swatch embedded in the
#              PDF, so the colours are read rather than guessed.
#   Count      Approximate, and biased DOWN. Where children live close
#              together the symbols overlap and merge, and a symbol wholly
#              underneath another leaves no trace at all. Totals come to
#              about 87% of the published offers, and the shortfall is
#              concentrated in the dense catchments - sparse Longhill comes
#              out slightly over, dense Varndean about a fifth under.
#
# So: use these for WHERE children come from, not HOW MANY. For how many,
# the factsheets are published and exact.
#
# Writes data/foi_allocations.rds. Run from project root.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(sf)
})

CSV  <- here::here("data-raw", "foi_allocations_points.csv")
OOC  <- here::here("data-raw", "foi_out_of_catchment.csv")
LSOA <- here::here("data", "lsoa.geojson")
OUT  <- here::here("data", "foi_allocations.rds")

SOURCE_URL <- "https://www.whatdotheyknow.com/request/evaluation_of_secondary_school_a"


#' Points as sf, on the British National Grid the maps are drawn on
read_points <- function(path = CSV) {
  read_csv(path, show_col_types = FALSE) |>
    mutate(preference = factor(preference,
                               levels = c("1", "2", "3", "4", "0", "Directed"))) |>
    st_as_sf(coords = c("easting", "northing"), crs = 27700, remove = FALSE)
}


#' Which admissions round is this?
#'
#' The maps carry no year. The council's covering letter says only that they
#' illustrate "the impact of the policy on National Offer Day". Rather than
#' assume, the extracted totals are correlated against each year of
#' published offers and the best match is reported - the answer is not close
#' enough to be ambiguous.
infer_year <- function(pts, factsheets) {
  # `name` is count()'s own argument, so the rename happens afterwards.
  ex <- pts |> st_drop_geometry() |>
    count(school, wt = est_children, name = "extracted") |>
    rename(name = school)
  factsheets |>
    filter(name %in% ex$name) |>
    inner_join(ex, by = "name") |>
    group_by(year) |>
    filter(n() == nrow(ex)) |>
    summarise(r = cor(extracted, off_total),
              total_offers = sum(off_total),
              total_extracted = sum(extracted),
              .groups = "drop") |>
    arrange(desc(r))
}


#' Check the extraction against a source it never saw
validate <- function(pts, factsheets, year) {
  ex <- pts |> st_drop_geometry() |>
    group_by(name = school) |>
    summarise(blobs = n(),
              extracted = sum(est_children),
              merged_blobs = sum(merged),
              in_merged = sum(est_children[merged == 1]),
              .groups = "drop")
  factsheets |>
    filter(year == !!year, name %in% ex$name) |>
    select(name, off_total, pref1, pref2, pref3) |>
    inner_join(ex, by = "name") |>
    mutate(recovered = round(100 * extracted / off_total),
           shortfall = off_total - extracted) |>
    arrange(recovered)
}


#' Children by home LSOA, school and preference
#'
#' This is the form the city model wants: origins by neighbourhood, not
#' points. It is also the form worth keeping, because an LSOA is a
#' defensible unit for a count this approximate, where a 5 m point is a
#' house.
to_lsoa <- function(pts) {
  lsoa <- st_read(LSOA, quiet = TRUE) |> st_transform(27700)
  j <- st_join(pts, lsoa, join = st_within)
  list(
    by_lsoa = j |> st_drop_geometry() |>
      filter(!is.na(lsoa21cd)) |>
      count(lsoa21cd, lsoa21nm, school, preference,
            wt = est_children, name = "children"),
    outside = sum(is.na(j$lsoa21cd))
  )
}


#' The table printed on each map, beside the map
#'
#' Every page carries a small cross-tab headed "Pupils allocated from
#' outside the catchment area": out-of-catchment allocations by preference
#' and by the admission criterion that got the child the place - EHCP, LAC
#' (looked after), EX (exceptional), SIB (sibling), FSM OUT, OPEN, OUT, and
#' DIRECTED. None of that is in the dots, which carry preference only.
#'
#' These are raster images with no text layer, so they are transcribed by
#' hand into data-raw/foi_out_of_catchment.csv rather than parsed. Each
#' school's total is checked against the total printed on its own map,
#' which is an arithmetic check on the transcription that the transcriber
#' cannot fudge.
read_out_of_catchment <- function(path = OOC) {
  printed <- c(
    "Brighton Aldridge Community Academy" = 86, "Blatchington Mill School" = 34,
    "Dorothy Stringer School" = 76, "Hove Park School" = 58,
    "Longhill High School" = 21, "Portslade Aldridge Community Academy" = 42,
    "Patcham High School" = 44, "Varndean School" = 35)
  d <- read_csv(path, show_col_types = FALSE)
  chk <- d |> count(school, wt = pupils, name = "transcribed") |>
    mutate(printed = printed[school])
  bad <- chk |> filter(transcribed != printed)
  if (nrow(bad)) {
    stop("transcription does not match the totals printed on the maps:\n",
         paste(sprintf("  %s: %d transcribed, %d printed",
                       bad$school, bad$transcribed, bad$printed), collapse = "\n"))
  }
  attr(d, "totals") <- chk
  d
}


#' Own-catchment share from the published table rather than from the dots
#'
#' This is the figure to use. The dots give an estimate of it; the council
#' prints the exact out-of-catchment count on every map, and the factsheets
#' give the offer total, so the share is arithmetic. Comparing the two says
#' how far the dot-derived shares can be trusted - and the answer is: not
#' very far, school by school, though Longhill happens to come out exact.
own_catchment_published <- function(ooc, factsheets, year) {
  tot <- ooc |> count(school, wt = pupils, name = "out_of_catchment")
  factsheets |>
    filter(year == !!year, name %in% tot$school) |>
    select(school = name, offers = off_total) |>
    inner_join(tot, by = "school") |>
    mutate(own = offers - out_of_catchment,
           own_pct = round(100 * own / offers))
}


build_foi_allocations <- function() {
  pts <- read_points()
  ooc <- read_out_of_catchment()
  fs  <- readRDS(here::here("data", "factsheet_panel.rds"))$factsheets |>
    filter(name != "Total")

  yr <- infer_year(pts, fs)
  message("Year match against published offers:")
  print(as.data.frame(yr |> mutate(r = round(r, 3))), row.names = FALSE)
  year <- yr$year[1]

  val <- validate(pts, fs, year)
  message("\nAgainst ", year, " offers (r = ", round(yr$r[1], 3), "):")
  print(as.data.frame(val), row.names = FALSE)

  ls <- to_lsoa(pts)
  message("\n", nrow(ls$by_lsoa), " school x LSOA x preference rows; ",
          ls$outside, " points fell outside the city's LSOAs")

  ocp <- own_catchment_published(ooc, fs, year)
  message("\nOut-of-catchment allocations, from the tables printed on the maps:")
  print(as.data.frame(ocp |> arrange(own_pct)), row.names = FALSE)

  out <- list(
    points      = pts,
    by_lsoa     = ls$by_lsoa,
    out_of_catchment = ooc,
    own_published    = ocp,
    validation  = val,
    year_match  = yr,
    entry_year  = year,
    outside_city = ls$outside,
    source      = SOURCE_URL,
    foi_ref     = "26370589",
    released    = as.Date("2026-10-29"),
    note        = paste(
      "Extracted from the released map images by",
      "R/extract_foi_allocations.py. Positions are reliable to a few",
      "metres; counts are biased down by overplotting and recover about",
      paste0(round(100 * sum(val$extracted) / sum(val$off_total)), "%"),
      "of published offers. Use for origins, not for totals."),
    built_at    = Sys.time()
  )
  saveRDS(out, OUT)
  message("\nWrote ", OUT)
  invisible(out)
}

if (sys.nframe() == 0) build_foi_allocations()
