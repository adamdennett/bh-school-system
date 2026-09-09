# R/01_assemble.R — pull every input this repository needs into data/
# ======================================================================
# This is the only script that reaches outside the repository. It runs
# once on a machine that has the upstream projects checked out, and
# leaves data/ self-contained so index.qmd renders anywhere.
#
# Sources, all of them open-data analyses of published figures:
#   BH_Pupil_Destinations/public/output  the open Brightopia bundle
#   school_attainment_tool               catchment boundaries, logos
#   BH_Schools_2                         pre-2024 catchment boundaries
#   BH_Schools_Consultation              LSOA boundary geometry
#
# Nothing here touches pupil-level records.
# ======================================================================

suppressPackageStartupMessages({ library(tidyverse); library(sf) })

ROOT <- here::here()
DATA <- file.path(ROOT, "data")
LOGOS <- file.path(ROOT, "assets", "school_logos")
dir.create(DATA, showWarnings = FALSE, recursive = TRUE)
dir.create(LOGOS, showWarnings = FALSE, recursive = TRUE)

SRC <- list(
  open   = "E:/BH_Pupil_Destinations/public/output",
  sat    = "E:/school_attainment_tool",
  bhs2   = "E:/BH_Schools_2/data",
  consult = "E:/BH_Schools_Consultation/data")

missing <- names(SRC)[!dir.exists(unlist(SRC))]
if (length(missing))
  stop("upstream source(s) not found: ", paste(missing, collapse = ", "),
       "\nThis script only runs on a machine with the upstream projects present.")

say <- function(...) message("  ", ...)

# ---- 1. The open Brightopia bundle -----------------------------------
# Everything except the r5r network build, which is ~80 MB of machine
# generated cache and is rebuildable from OSM + GTFS.

message("\n=== open model outputs ===")
out_files <- list.files(SRC$open, pattern = "[.](rds|csv|png)$", full.names = TRUE)
file.copy(out_files, DATA, overwrite = TRUE)
say(length(out_files), " files copied from the open bundle")

dir.create(file.path(DATA, "travel"), showWarnings = FALSE)
file.copy(file.path(SRC$open, "travel", "bn_pcds_sch_travel_extended.csv"),
          file.path(DATA, "travel"), overwrite = TRUE)
say("routed postcode x school travel matrix copied")

# ---- 2. Catchment boundaries -----------------------------------------

message("\n=== catchment boundaries ===")
file.copy(file.path(SRC$sat, "data", "optionZ_Mar25.geojson"),
          file.path(DATA, "catchments_current.geojson"), overwrite = TRUE)
say("current (2025/26) catchments")

pre <- file.path(SRC$bhs2, "BrightonSecondaryCatchments.geojson")
if (file.exists(pre)) {
  file.copy(pre, file.path(DATA, "catchments_pre2024.geojson"), overwrite = TRUE)
  say("pre-2024 catchments")
}

# ---- 3. School logos -------------------------------------------------

message("\n=== school logos ===")
lg <- list.files(file.path(SRC$sat, "output", "school_logos"),
                 pattern = "[.]png$", full.names = TRUE)
file.copy(lg, LOGOS, overwrite = TRUE)
say(length(lg), " logos copied (including CoMArt)")

# ---- 4. LSOA boundary geometry ---------------------------------------
# The national file is 1.4 GB, far too large to carry. Subset it once to
# the LSOAs the model actually covers - Brighton and Hove plus the
# Peacehaven and Telscombe area the catchment system reaches into - and
# keep only that.

message("\n=== LSOA boundaries ===")
lsoa_out <- file.path(DATA, "lsoa.geojson")

if (file.exists(lsoa_out)) {
  say("already extracted; delete data/lsoa.geojson to rebuild")
} else {
  zones <- readRDS(file.path(DATA, "open_inputs.rds"))$zones
  idaci <- readRDS(file.path(DATA, "deprivation_open.rds"))$idaci
  codes <- sort(unique(c(zones$lsoa, idaci$lsoa)))
  codes <- codes[!is.na(codes) & nzchar(codes)]
  say(length(codes), " LSOA codes in scope")

  ew <- file.path(SRC$consult, "EW_LSOA.geojson")
  if (!file.exists(ew)) stop("EW_LSOA.geojson not found at ", ew)

  layer <- sf::st_layers(ew)$name[1]
  q <- sprintf('SELECT lsoa21cd, lsoa21nm FROM "%s" WHERE lsoa21cd IN (%s)',
               layer, paste0("'", codes, "'", collapse = ", "))
  say("scanning the national file (this takes a minute)...")
  lsoa <- sf::st_read(ew, query = q, quiet = TRUE) %>% sf::st_transform(4326)

  found <- nrow(lsoa)
  say(found, " of ", length(codes), " matched")
  if (found < length(codes) * 0.9)
    warning("fewer LSOA polygons than expected - check the geometry vintage")

  sf::st_write(lsoa, lsoa_out, delete_dsn = TRUE, quiet = TRUE)
  say("written to data/lsoa.geojson (",
      round(file.size(lsoa_out) / 1e6, 1), " MB)")
}

# ---- 5. School performance panel -------------------------------------
# The panel behind "How to Pull the Right Lever", built from published
# DfE performance tables. The national file is 16,000 schools x 527
# columns; carry only Brighton and Hove plus the national aggregates
# needed to say where the city sits.

message("\n=== school performance panel ===")
panel_src <- file.path(SRC$sat, "data", "panel_data.rds")

if (!file.exists(panel_src)) {
  say("! panel_data.rds not found; section 2.5 will be text only")
} else {
  KEEP <- c("URN", "LANAME", "SCHNAME", "year_label", "year_numeric",
            "TOTPUPS", "ATT8SCR", "P8MEA", "PTFSM6CLA1A", "PERCTOT",
            "PPERSABS10", "PTPRIORLO", "PTPRIORHI", "PNUMEAL",
            "OFSTEDRATING", "SCHOOLTYPE", "MINORGROUP", "POSTCODE")
  panel <- readRDS(panel_src)
  KEEP <- intersect(KEEP, names(panel))

  bh <- panel %>%
    filter(grepl("Brighton", LANAME, ignore.case = TRUE)) %>%
    select(all_of(KEEP))
  say(nrow(bh), " Brighton & Hove school-years, ",
      dplyr::n_distinct(bh$year_label), " years")

  # National picture, for benchmarking only: one row per year, plus the
  # school-level FSM/absence/attainment triple needed to show that
  # attainment tracks intake rather than to identify any school.
  national <- panel %>%
    filter(!is.na(ATT8SCR)) %>%
    select(any_of(c("URN", "LANAME", "year_label", "ATT8SCR", "P8MEA",
                    "PTFSM6CLA1A", "PERCTOT", "PPERSABS10",
                    "PTPRIORLO", "PTPRIORHI", "TOTPUPS"))) %>%
    filter(year_label == max(year_label, na.rm = TRUE))
  say(nrow(national), " schools nationally in the latest year")

  saveRDS(list(bh = bh, national = national,
               latest = max(bh$year_label, na.rm = TRUE),
               source = "DfE performance tables, via school_attainment_tool",
               built_at = Sys.time()),
          file.path(DATA, "performance_panel.rds"))
  say("saved data/performance_panel.rds")
}

# ---- 6. Postcode-level child population ------------------------------
# Census 2021 output-area household composition apportioned to postcode
# centroids, with IDACI attached. Finer than the LSOA zones used
# everywhere else, which makes it the right basis for a density surface
# and for a dot map that shows the texture of the city rather than its
# administrative blocks.
#
# Brighton & Hove only - it does not extend to the Peacehaven area, so
# anything built from it is a city map, not a study-area map.

message("\n=== postcode child population ===")
pcd_src <- file.path(SRC$consult, "bn_postcodes_pop1.csv")

if (!file.exists(pcd_src)) {
  say("! bn_postcodes_pop1.csv not found; the postcode maps will be skipped")
} else {
  pcd <- readr::read_csv(pcd_src, show_col_types = FALSE) %>%
    transmute(
      postcode   = pcds,
      lsoa       = lsoa21,
      # Northings are zero-padded in the source ("0101938"), so readr
      # types the column as character. Left as-is it silently survives
      # every filter and only fails much later, inside the kernel
      # density estimate. Coerce here.
      easting    = as.numeric(oseast1m),
      northing   = as.numeric(osnrth1m),
      children   = pcd_dep_ch_0_18_total_count,
      hh_with_ch = pcd_dep_ch_hh_count_round,
      hh_total   = total_hh,
      idaci_decile = idaci_decile,
      idaci_score  = idaci_score,
      catchment_2026 = catchment_2026) %>%
    filter(!is.na(easting), !is.na(northing))

  say(format(nrow(pcd), big.mark = ","), " postcodes, ",
      format(round(sum(pcd$children, na.rm = TRUE)), big.mark = ","),
      " children aged 0-18, ", dplyr::n_distinct(pcd$lsoa), " LSOAs")

  readr::write_csv(pcd, file.path(DATA, "postcode_children.csv"))
  say("saved data/postcode_children.csv")
}

message("\nAssembly complete. data/ is now self-contained.\n")
