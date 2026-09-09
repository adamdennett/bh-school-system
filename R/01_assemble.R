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
            "PPERSABS10", "PTPRIORLO", "PTPRIORHI", "PNUMEAL", "KS2ASS",
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
    # KS2ASS and PERCTOT matter specifically: the model in "How to Pull
    # the Right Lever" and the RPE paper is
    #   log(ATT8SCR) ~ log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL)
    #                  + ks2_c + ...
    # where ks2_c is KS2ASS centred at 100 and entered linearly. Rates
    # are logged; the prior-attainment score is not. Anything here that
    # mirrors that model has to use these columns, not PPERSABS10 or
    # PTPRIORLO.
    select(any_of(c("URN", "LANAME", "year_label", "ATT8SCR", "P8MEA",
                    "PTFSM6CLA1A", "PERCTOT", "PPERSABS10",
                    "PTPRIORLO", "PTPRIORHI", "TOTPUPS",
                    "KS2ASS", "PNUMEAL"))) %>%
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

# ---- 5b. Variance decomposition -------------------------------------
# Where the variance in school-level Attainment 8 actually sits, from
# the multilevel model behind "How to Pull the Right Lever". This is the
# model-derived version of the school-effects question: what share is
# inherited with the intake, what share is the workforce the school
# directly controls, and what is left over as a persistent school effect.

message("\n=== variance decomposition ===")
sed_src <- file.path(SRC$sat, "data", "cache", "school_effect_decomp.rds")

if (!file.exists(sed_src)) {
  say("! school_effect_decomp.rds not found; section 2.4 will fall back to the literature range")
} else {
  file.copy(sed_src, file.path(DATA, "school_effect_decomp.rds"), overwrite = TRUE)
  sed <- readRDS(sed_src)
  say("copied: ", paste(intersect(names(sed), c("decomp_all", "decomp_dis", "decomp_non")),
                        collapse = ", "))
  wf <- sed$decomp_all[["Share of total %"]][
    grepl("^Endogenous", sed$decomp_all$Component)]
  ex <- sed$decomp_all[["Share of total %"]][
    grepl("^Exogenous", sed$decomp_all$Component)]
  say("all pupils: workforce ", wf, "%, inherited ", ex, "%")
}

# ---- 5c. How much of the variance a school can actually reach --------
# The decomposition above files absence inside the exogenous block, as
# though it were entirely inherited. The two-stage work in the RPE paper
# shows it is not: a first-stage model of absence on intake, area and
# segregation explains only about half its variance, and the residual is
# the part pastoral systems and attendance work can act on.
#
# That matters for how the decomposition reads. The school-reachable
# share is not the workforce term alone; it is the workforce term plus
# the school-controllable half of the absence term. This step refits
# stage 1 to get the split, then apportions the absence term in each
# published model accordingly.
#
# Slow (a multilevel fit on ~12,000 school-years), so it is cached.

message("\n=== school-reachable variance ===")
lev_out <- file.path(DATA, "school_leverage.rds")

if (file.exists(lev_out)) {
  say("already computed; delete data/school_leverage.rds to rebuild")
} else if (!requireNamespace("lme4", quietly = TRUE)) {
  say("! lme4 not available; skipping")
} else {
  md <- file.path(SRC$sat, "data", "model_data_imputed.rds")
  mm <- file.path(SRC$sat, "data", "models_imputed.rds")
  if (!file.exists(md) || !file.exists(mm)) {
    say("! model objects not found; skipping")
  } else {
    suppressPackageStartupMessages(library(lme4))

    # --- stage 1: how much of absence is structural? ---
    s1 <- readRDS(md) %>%
      filter(!is.na(PERCTOT), PERCTOT > 0, !is.na(PTFSM6CLA1A), PTFSM6CLA1A > 0,
             !is.na(PNUMEAL), PNUMEAL > 0, !is.na(ks2_c),
             !is.na(gorard_segregation)) %>%
      droplevels()
    say("fitting the stage-1 absence model on ",
        format(nrow(s1), big.mark = ","), " school-years...")
    m1 <- lmer(log(PERCTOT) ~ log(PTFSM6CLA1A) + log(PNUMEAL) + ks2_c +
                 gorard_segregation + (1 | year_label) + (1 | gor_name/LANAME),
               data = s1, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 20000)))
    fit1 <- predict(m1, re.form = NULL); obs1 <- log(s1$PERCTOT)
    struct_share <- var(fit1) / var(obs1)
    school_share <- 1 - struct_share
    say(sprintf("absence: %.0f%% structural, %.0f%% school-reachable",
                100 * struct_share, 100 * school_share))

    # --- apportion the absence term in each published model ---
    WF <- c("remained_in_the_same_school",
            "teachers_on_leadership_pay_range_percent",
            "log(average_number_of_days_taken)")
    mods <- readRDS(mm)

    one <- function(m, who) {
      X <- getME(m, "X"); b <- fixef(m); cn <- colnames(X)
      eta <- function(sel) as.vector(X[, sel, drop = FALSE] %*% b[sel])
      is_int <- cn == "(Intercept)"; is_wf <- cn %in% WF
      e_abs <- eta(grepl("PERCTOT", cn))
      e_wf  <- eta(is_wf)
      e_exo <- eta(!is_wf & !is_int)
      full  <- e_exo + e_wf
      vc <- as.data.frame(VarCorr(m)) %>% filter(is.na(var2))
      tot <- cov(e_exo, full) + cov(e_wf, full) + sum(vc$vcov)
      sh <- function(x) 100 * cov(x, full) / tot
      tibble(who = who, exogenous = sh(e_exo), absence = sh(e_abs),
             workforce = sh(e_wf),
             absence_structural = sh(e_abs) * struct_share,
             absence_school     = sh(e_abs) * school_share,
             reachable          = sh(e_wf) + sh(e_abs) * school_share)
    }

    leverage <- bind_rows(
      one(mods$all,               "All pupils"),
      one(mods$disadvantaged,     "Disadvantaged pupils"),
      one(mods$non_disadvantaged, "Non-disadvantaged pupils"))

    print(as.data.frame(leverage %>% mutate(across(where(is.numeric), ~ round(.x, 1)))),
          row.names = FALSE)

    saveRDS(list(struct_share = struct_share, school_share = school_share,
                 stage1_n = nrow(s1), leverage = leverage,
                 built_at = Sys.time()), lev_out)
    say("saved data/school_leverage.rds")
  }
}

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
