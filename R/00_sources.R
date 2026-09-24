# R/00_sources.R — what this document is built from, and where to get it
# ======================================================================
# Every figure in this document comes from published data. This file is
# the register of what that data is: one row per published source with a
# link, one row per file in data/ saying which sources it derives from,
# and a scan of index.qmd that works out which sections use which file.
#
# The scan is done by reading index.qmd at render time rather than by
# keeping a hand-written list, because a hand-written list of "section 4
# uses the travel matrix" is wrong within two edits and nobody notices.
#
# Sourced by R/00_core.R; used by the "Sources and data" appendix.
#
# LINKS. These point at the publisher's landing page for the dataset
# rather than at a particular file. Deep links to government statistical
# releases rot within a year or two, and a landing page that has moved a
# file still gets a reader to it. Council committee papers are cited by
# title and date for the same reason: Brighton & Hove's document URLs
# are not stable.
# ======================================================================

# ---- 1. The published sources ---------------------------------------

SOURCES <- tibble::tribble(
  ~key,          ~title,                                      ~publisher,                            ~what,                                                                                          ~url,                                                                                          ~licence,
  "perf",        "School performance tables",                 "Department for Education",            "Attainment 8, Progress 8, absence, cohort characteristics and prior attainment, by school.",   "https://www.compare-school-performance.service.gov.uk/",                                        "OGL v3",
  "fin",         "School income and expenditure",             "Department for Education",            "Consistent Financial Reporting returns for maintained schools and academy accounts returns.",  "https://financial-benchmarking-and-insights-tool.education.gov.uk/",                            "OGL v3",
  "ees",         "Explore education statistics",              "Department for Education",            "The underlying national datasets behind the performance and finance services.",                "https://explore-education-statistics.service.gov.uk/",                                          "OGL v3",
  "gias",        "Get Information About Schools",             "Department for Education",            "The school register: URN, phase, age range, status, religious character, coordinates.",        "https://get-information-schools.service.gov.uk/",                                               "OGL v3",
  "hts",         "Home-to-school travel and transport guidance", "Department for Education",         "The statutory maximum journey times used as a benchmark in section 4.",                        "https://www.gov.uk/government/publications/home-to-school-travel-and-transport-guidance",        "OGL v3",
  "nts",         "National Travel Survey",                    "Department for Transport",            "The England average school trip, used to make a number of minutes interpretable.",             "https://www.gov.uk/government/collections/national-travel-survey-statistics",                    "OGL v3",
  "bods",        "Bus Open Data Service",                     "Department for Transport",            "The Brighton & Hove timetable feed (GTFS) the journey times are routed over.",                 "https://data.bus-data.dft.gov.uk/",                                                             "OGL v3",
  "osm",         "OpenStreetMap",                             "OpenStreetMap contributors",          "The walking and road network the journey times are routed over.",                              "https://www.openstreetmap.org/copyright",                                                        "ODbL",
  "imd",         "English indices of deprivation 2019",       "Ministry of Housing, Communities and Local Government", "IDACI: income deprivation affecting children, by LSOA.",                     "https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019",                   "OGL v3",
  "census",      "Census 2021",                               "Office for National Statistics",      "Household composition, used for households with dependent children by postcode.",              "https://www.ons.gov.uk/census",                                                                  "OGL v3",
  "onspd",       "ONS Postcode Directory and boundaries",     "Office for National Statistics",      "Postcode centroids, LSOA lookups and the 2021 LSOA boundary geometry.",                        "https://geoportal.statistics.gov.uk/",                                                          "OGL v3",
  "sape",        "Small area population estimates",           "Office for National Statistics",      "Population by single year of age, used to split each ward's children between its LSOAs.",      "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates", "OGL v3",
  "bhcc_osa81",  "Catchment-level preferences, evidence to the Schools Adjudicator", "Brighton & Hove City Council", "First, second and third preferences and allocations by home catchment and school, 2023/24 to 2025/26 (item 8.1). Received as a party to the 2026/27 case, not published; aggregated to catchment and non-disclosive. The full model (M5) is calibrated to it.", "https://www.gov.uk/government/organisations/office-of-the-schools-adjudicator", "not published; used as a party to the case",
  "bhcc_foi24",  "Where each catchment's children were offered a place, 2024", "Brighton & Hove City Council", "Offers on national offer day in the 2024 round by home catchment and school, including schools outside the city, with small counts suppressed. The council's answer to a Freedom of Information request, published on WhatDoTheyKnow. The full model's out-of-city destinations are fitted to it.", "https://www.whatdotheyknow.com/request/schools_admissions_breakdowns_fo", "published FOI response",
  "osa",         "Schools Adjudicator determinations",        "Office of the Schools Adjudicator",   "The 2025 determinations on Brighton & Hove's admission arrangements (ADA4423 and others).",    "https://www.gov.uk/government/organisations/office-of-the-schools-adjudicator",                  "OGL v3",
  "bhcc_adm",    "School admissions: allocation factsheets",  "Brighton & Hove City Council",        "Preferences and offers by school and rank, published each year after allocation.",             "https://www.brighton-hove.gov.uk/schools-and-learning/school-admissions",                        "OGL v3",
  "bhcc_plan",   "School place planning reports and forecasts", "Brighton & Hove City Council",      "Catchment forecasts and admission-number proposals, in committee papers and consultation documents.", "https://www.brighton-hove.gov.uk/schools-and-learning/school-admissions",                    "OGL v3",
  "bhcc_catch",  "Secondary catchment boundaries",            "Brighton & Hove City Council",        "The catchment map in force from September 2026 entry, and the map it replaced.",               "https://www.brighton-hove.gov.uk/schools-and-learning/school-admissions",                        "OGL v3",
  "bhcc_eef",    "Educational Equity Framework, Appendix 1",  "Brighton & Hove City Council",        "Key Stage 2 and Key Stage 4 pupils and attainment by home ward and middle super output area, from the January 2025 school census (table 13.1.2). Section 2.7 checks the model's origins against it.", "https://www.brighton-hove.gov.uk/schools-and-learning/school-policies-reports-strategies-and-other-documents", "OGL v3",
  "lever",       "How to Pull the Right Lever",               "Dennett and colleagues, UCL CASA",    "The multilevel model of school-level attainment whose specification and decomposition section 2 follows.", "https://adamdennett.github.io/school_attainment_tool/index.html", "author’s own work",
  "fundstats",   "School funding statistics",                 "Department for Education",            "School-level funding allocations for 2025-26, split into the formula's own components.",      "https://explore-education-statistics.service.gov.uk/find-statistics/school-funding-statistics",  "OGL v3",
  "esri",        "World Light Gray Canvas",                   "Esri",                                "The base cartography under every map in this document. Keyless, unlike the CARTO tiles it replaced.", "https://www.arcgis.com/home/item.html?id=979c6cc89af9449cbeb5342a439c6a76",                  "Esri terms, attribution required")

stopifnot(!any(duplicated(SOURCES$key)), !any(is.na(SOURCES$url)))

# ---- 2. What is in data/, and what it is made of ---------------------
# `from` lists SOURCES keys. `built_by` names the analysis that produced
# the file, so a reader can tell a published dataset from a derived one.

DATASETS <- tibble::tribble(
  ~file,                          ~title,                                  ~from,                                                    ~built_by,
  "open_inputs.rds",              "Model inputs: zones, costs, schools",   c("onspd", "sape", "gias", "bhcc_catch", "bhcc_adm", "osm", "bods"), "open Brightopia bundle",
  "accessibility.rds",            "Accessibility surfaces and benchmarks", c("osm", "bods", "nts", "hts", "imd"),                    "this repository, R/02_accessibility.R",
  "deprivation_open.rds",         "IDACI by neighbourhood",                c("imd", "onspd"),                                        "open Brightopia bundle",
  "brightopia.rds",               "The distance-only model",               c("osm", "bods", "bhcc_adm"),                             "open Brightopia bundle",
  "sensitivity_envelope.rds",     "Sensitivity across the parameter band", c("osm", "bods", "bhcc_adm"),                             "open Brightopia bundle",
  "model_terms.rds",              "The model ladder M0 to M5",             c("osm", "bods", "bhcc_adm", "bhcc_catch", "bhcc_osa81", "bhcc_foi24", "gias"),               "open Brightopia bundle",
  "open_scenarios.rds",           "Scenario runs, and Longhill's accounts", c("osm", "bods", "bhcc_adm", "fin"),                     "open Brightopia bundle",
  "school_finance.rds",           "Every school's finances and exposure",  c("fin", "gias", "bhcc_adm"),                             "open Brightopia bundle",
  "flow_map.rds",                 "Modelled flows along the road network", c("osm", "bods", "bhcc_adm"),                             "open Brightopia bundle",
  "flow_regions.rds",             "Catchments redrawn from the flows",     c("onspd", "imd", "census", "bhcc_catch", "osm", "bods"), "this repository, R/03_flow_regions.R",
  "factsheet_panel.rds",          "Preferences and offers, 2010 to 2026",  c("bhcc_adm"),                                            "open Brightopia bundle",
  "reception_cohort.rds",         "Reception cohorts and the projection",  c("bhcc_adm", "bhcc_plan"),                               "open Brightopia bundle",
  "adjudicator_conversion.rds",   "What the adjudicator determined",       c("osa", "bhcc_plan"),                                    "open Brightopia bundle",
  "performance_panel.rds",        "School attainment panel",               c("perf", "ees"),                                         "school attainment tool",
  "school_effect_decomp.rds",     "Variance decomposition",                c("perf", "lever"),                                       "school attainment tool",
  "school_leverage.rds",          "How much a school can reach",           c("perf", "lever"),                                       "school attainment tool",
  "postcode_children.csv",        "Households with children, by postcode", c("census", "onspd", "imd"),                              "this repository, R/01_assemble.R",
  "school-funding-statistics_2025-26/data/20260129_School_level_data_csv.csv",
                                  "Funding allocations, 2025-26",          c("fundstats"),                                           "published dataset",
  "council_forecast_oct24.csv",   "The council's October 2024 forecast",   c("bhcc_plan"),                                           "transcribed from the published appendix",
  "catchments_current.geojson",   "Catchments in force",                   c("bhcc_catch"),                                          "published boundary file",
  "catchments_pre2024.geojson",   "Catchments in force before 2026 entry", c("bhcc_catch"),                                          "published boundary file",
  "lsoa.geojson",                 "LSOA boundaries",                       c("onspd"),                                               "published boundary file",

  "whitehawk_explained.rds",      "What the Whitehawk redraw does, and why", c("osm", "bods", "bhcc_osa81", "bhcc_foi24", "bhcc_adm", "imd", "census", "onspd", "perf"), "this repository, R/05_app_inputs.R",
  "council_options.rds",          "The council's options, scored on its priorities", c("osm", "bods", "bhcc_osa81", "bhcc_foi24", "bhcc_adm", "imd", "census", "onspd", "fin", "perf"), "this repository, R/05_app_inputs.R",
  "comart_scenarios.rds",         "CoMArt re-opened: the scenario runs",   c("osm", "bods", "bhcc_osa81", "bhcc_foi24", "bhcc_adm", "imd", "census", "onspd", "perf"), "this repository, R/05_app_inputs.R",
  "comart_costs.rds",             "Routed journeys to CoMArt's site",     c("osm", "bods", "onspd"),                            "open Brightopia bundle",
  "priority6_sweep.rds",          "Priority 6 and the social mix of intakes", c("bhcc_osa81", "bhcc_foi24", "bhcc_adm", "imd", "census", "onspd", "osm", "bods", "perf"), "this repository, R/05_app_inputs.R",
  "route_geometries.rds",         "One journey, leg by leg",               c("osm", "bods"),                                         "open Brightopia bundle",
  "guide_routes.rds",             "One neighbourhood routed to every school", c("osm", "bods", "onspd"),                            "open Brightopia bundle",
  "ward_validation.rds",          "The origins checked against the council's ward count", c("bhcc_eef", "sape", "onspd", "bhcc_adm"), "open Brightopia bundle")

stopifnot(!any(duplicated(DATASETS$file)),
          all(unlist(DATASETS$from) %in% SOURCES$key))

# ---- 3. Which object in index.qmd is which file ----------------------
# Nearly every one of these is a list read once at the top of the
# document and then used as `obj$field`, so requiring the dollar makes
# the scan specific. The three that are used bare are geometries and
# tables, and are matched on the word alone.

QMD_VARS <- tibble::tribble(
  ~var,     ~file,                        ~bare,
  "oi",     "open_inputs.rds",            FALSE,
  "acc",    "accessibility.rds",          FALSE,

  "brt",    "brightopia.rds",             FALSE,
  "env",    "sensitivity_envelope.rds",   FALSE,
  "mt",     "model_terms.rds",            FALSE,
  "os",     "open_scenarios.rds",         FALSE,
  "sfin",   "school_finance.rds",         FALSE,
  "fm",     "flow_map.rds",               FALSE,
  "fr",     "flow_regions.rds",           FALSE,
  "fp",     "factsheet_panel.rds",        FALSE,
  "rc",     "reception_cohort.rds",       FALSE,
  "conv",   "adjudicator_conversion.rds", FALSE,
  "perf",   "performance_panel.rds",      FALSE,
  "sed",    "school_effect_decomp.rds",   FALSE,
  "lev",    "school_leverage.rds",        FALSE,

  "p6s",    "priority6_sweep.rds",        FALSE,
  "cms",    "comart_scenarios.rds",       FALSE,
  "cop",    "council_options.rds",        FALSE,
  "whx",    "whitehawk_explained.rds",    FALSE,
  "rg",     "route_geometries.rds",       FALSE,
  "council24", "council_forecast_oct24.csv", TRUE,
  "pcd",    "postcode_children.csv",      TRUE,
  "fund",   "school-funding-statistics_2025-26/data/20260129_School_level_data_csv.csv", FALSE,
  "catch",  "catchments_current.geojson", TRUE,
  "lsoa",   "lsoa.geojson",               TRUE)

stopifnot(all(QMD_VARS$file %in% DATASETS$file))

#' Which datasets each numbered section of the document uses
#'
#' Reads the document's own source and looks, section by section, for
#' the objects each dataset is loaded into. Sections are the top-level
#' `# Heading` lines; everything before the first one is setup and is
#' not attributed to a section.
#'
#' @param qmd path to the Quarto source
#' @return a tibble of section, number, file
section_uses <- function(qmd = here::here("index.qmd")) {
  src <- readLines(qmd, warn = FALSE)

  # A "# " line inside a code chunk is an R comment, not a heading. The
  # first version of this scan did not know the difference and produced
  # seventy "sections" with names like "CoMArt, closed 2005".
  fence <- grepl("^```", src)
  in_chunk <- cumsum(fence) %% 2 == 1 | fence

  h <- which(grepl("^# [^|#]", src) & !in_chunk)
  if (!length(h)) stop("no top-level headings found in ", qmd)
  titles <- sub("^# ", "", src[h])
  titles <- trimws(sub("\\{#.*\\}\\s*$", "", titles))

  bounds <- c(h, length(src) + 1L)

  purrr::map_dfr(seq_along(h), function(i) {
    body <- paste(src[bounds[i]:(bounds[i + 1] - 1)], collapse = "\n")
    hit <- purrr::pmap_lgl(QMD_VARS, function(var, file, bare) {
      pat <- if (bare) sprintf("(?<![A-Za-z0-9._])%s(?![A-Za-z0-9._])", var)
             else      sprintf("(?<![A-Za-z0-9._])%s\\$", var)
      grepl(pat, body, perl = TRUE)
    })
    tibble::tibble(number = i, section = titles[i],
                   file = QMD_VARS$file[hit])
  })
}
