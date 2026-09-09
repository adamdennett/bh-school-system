# R/02_accessibility.R — how reachable are secondary school places?
# ======================================================================
# Section 4 of the strategic view asks a question the catchment debate
# mostly skips: before any admissions rule is applied, how much school
# is actually within reach of each neighbourhood by public transport?
#
# Two measures, deliberately, because they fail in opposite directions:
#
#   Gravity (Hansen)      A_i = SUM_j  W_j * c_ij^(-beta)
#     Every school counts, discounted by how far away it is. Consistent
#     with Brightopia - same W, same c, same decay - so section 4 and
#     section 7 are describing one model, not two. Hard to state in
#     plain words: the number itself has no units.
#
#   Cumulative opportunity  P_i(t) = SUM_j PAN_j * 1(c_ij <= t)
#     Places reachable inside t minutes. Trivial to explain to a parent
#     or a councillor, but it treats a 29-minute journey and a
#     31-minute journey as completely different things.
#
# Where they agree, the finding is robust to how you define access.
# Where they disagree, the disagreement is itself worth reporting.
#
# c_ij is mean routed walk/bus travel time in minutes, zone to school,
# built by the open model from r5r over merged OSM + GTFS. Only 3 of
# 2,299 zone-school pairs fall back to the distance-time regression.
#
# Output: data/accessibility.rds
# ======================================================================

source(here::here("R", "00_core.R"))

message("\n=== Accessibility ===")

oi    <- bh_data("open_inputs.rds")
env   <- bh_data("sensitivity_envelope.rds")
br    <- bh_data("brightopia.rds")
fp    <- bh_data("factsheet_panel.rds")
idaci <- bh_data("deprivation_open.rds")$idaci

zones <- oi$zones %>% select(zone, lsoa, catchment, area, Oi, zone_e, zone_n)

# ---- Walking is always available -------------------------------------
# The walk fallback is applied upstream now, in the open model's own
# 01_open_inputs.R, so every section of this document and of the
# technical companion uses the same corrected costs. Where the routed
# bus itinerary is slower than simply walking the distance, the walking
# time is used instead. See build_costs_open() there for the reasoning
# and the speed, which comes from Kent et al. (2026).
#
# Read the flag rather than recomputing it, so the two cannot drift.

costs     <- oi$costs_now
costs_elm <- oi$costs_elm

message(sprintf(
  "  Walk fallback (applied upstream) binds on %d of %d pairs (%.0f%%)",
  sum(costs$walk_capped), nrow(costs), 100 * mean(costs$walk_capped)))

# ---- Attractiveness --------------------------------------------------
# W_j is how much wanted school there is at j. An admission number is a
# poor proxy: it is an administrative decision, fixed by the authority,
# and it says nothing about whether families want the places. Longhill
# has the third-largest admission number in the city and the fewest
# first preferences of any school.
#
# Expressed preferences are the better signal, but first preferences
# alone will not do either, because the catchment structure distorts
# them. In the two paired catchments a family must rank the two local
# schools against each other, so the catchment's first preferences are
# split between them while its second preferences pile up. The ratio of
# second to first preferences is about 1.0 to 2.0 for the four schools in
# paired catchments and 0.4 to 0.6 for those in single-school ones. Using
# first preferences alone would therefore mark down exactly the schools
# whose catchment forces families to put them second.
#
# So preferences are counted at every rank with a geometric decay:
#
#   W_j  proportional to  p1 + a*p2 + a^2*p3
#
# The count is deliberately not divided by the admission number. This is
# an accessibility measure, and what should be within reach is *places
# that families want*, which scales with the size of the school. A
# per-place version would make a small popular school look like a large
# one, which is the wrong quantity here.

PREF_DECAY <- 0.5
PREF_YEARS <- 5

prefs <- fp$factsheets %>%
  filter(name != "Total", year >= max(year) - (PREF_YEARS - 1)) %>%
  group_by(name) %>%
  summarise(p1 = mean(pref1, na.rm = TRUE),
            p2 = mean(pref2, na.rm = TRUE),
            p3 = mean(pref3, na.rm = TRUE),
            allocated = mean(off_total, na.rm = TRUE), .groups = "drop")

attr_ <- oi$attract %>%
  select(name, pan, W_pan) %>%
  left_join(prefs, by = "name") %>%
  mutate(pref_score = p1 + PREF_DECAY * p2 + PREF_DECAY^2 * p3,
         # Peacehaven Community School is in Lewes district and does not
         # appear in the council's factsheets, so it has no preference
         # data at all. Dropping it would empty the eastern expansion
         # area of schools; scoring it zero would do the same. Impute
         # from its admission number at the city-average rate, and
         # record that it is imputed.
         imputed = is.na(pref_score))

ppp  <- with(attr_, mean(pref_score / pan, na.rm = TRUE))
apc  <- with(attr_, mean(allocated / pan, na.rm = TRUE))
p1pc <- with(attr_, mean(p1 / pan, na.rm = TRUE))

attr_ <- attr_ %>%
  mutate(
    pref_score = if_else(imputed, pan * ppp, pref_score),
    p1_f       = if_else(imputed, pan * p1pc, p1),
    alloc_f    = if_else(imputed, pan * apc, allocated),
    # Four candidate specifications, each normalised to mean 1 so they
    # are on a common footing:
    #   W_pan    admission number - what the authority decided to offer
    #   W_p1     first preferences - what families asked for first
    #   W_alloc  places actually allocated - what the system delivered
    #   W_pref   rank-weighted preferences - the specification used here
    W_p1    = p1_f / mean(p1_f),
    W_alloc = alloc_f / mean(alloc_f),
    W_pref  = pref_score / mean(pref_score))

message(sprintf("  Attractiveness from %d years of published preferences, decay %.2f",
                PREF_YEARS, PREF_DECAY))
if (any(attr_$imputed))
  message("  ! imputed from PAN (no published preferences): ",
          paste(attr_$name[attr_$imputed], collapse = ", "))
print(as.data.frame(attr_ %>%
  transmute(School = name, PAN = pan,
            p1 = round(p1), p2 = round(p2),
            `p2/p1` = round(p2 / p1, 2),
            W_pan = round(W_pan, 2), W_pref = round(W_pref, 2)) %>%
  arrange(desc(W_pref))), row.names = FALSE)

# The travel table names Hove Park without its sixth-form suffix; the
# schools table carries the full DfE name. Join on what both agree on.
stopifnot(all(costs$name %in% attr_$name))

# ---- Decay parameter -------------------------------------------------
# The open model sweeps beta rather than calibrating it, because
# calibration needs pupil-level flows. Take the sweep's own reference
# value where there is one, and report the surface at the ends of the
# swept range too, so a reader can see how much the choice matters.

# Set explicitly rather than taken from the open model's default of 1.5.
# 1.7 is a reasonable central value for distance decay in a system of
# this kind: journeys are short, alternatives are close together, and
# the published sweep runs from 1.5 to 3.2. It is a judgement, not an
# estimate - calibrating beta needs pupil-level flows the council has
# not released - and the surface is reported across the swept range as
# well, so nothing here rests on the exact figure.
BETA_REF <- 1.7

beta_ref <- BETA_REF
beta_lo <- min(env$betas); beta_hi <- max(env$betas)
message(sprintf("  beta reference %.2f (swept range %.1f to %.1f)",
                beta_ref, beta_lo, beta_hi))

THRESHOLDS <- c(30, 45)

# A finer grid of thresholds, for the interactive version of the
# cumulative measure. Precomputed here because the alternative is
# shipping the whole zone x school cost matrix to the browser.
THRESH_GRID <- seq(10, 75, by = 5)

# ---- National benchmarks ---------------------------------------------
# Context for what these journey times mean. All three are published.
#
#   NTS_MEAN_MIN   average one-way school trip, England. National Travel
#                  Survey five-year average to 2019, ages 5-16, ALL modes
#                  including car. Not secondary-specific, and not
#                  public-transport-specific, so it is a floor rather
#                  than a like-for-like comparator.
#   NTS_MEAN_MILES the same, in distance.
#   DFE_MAX_SEC    the Department for Education's statutory guidance on
#                  home-to-school travel: as a general guide the maximum
#                  journey time each way should be 75 minutes for a child
#                  of secondary age, 45 for primary, including time
#                  walking to a pick-up point.
NTS_MEAN_MIN   <- 19
NTS_MEAN_MILES <- 2.4
DFE_MAX_SEC    <- 75
DFE_MAX_PRI    <- 45

# ---- Zone-level measures ---------------------------------------------

hansen <- function(b, wcol = "W_pref", ct = costs) {
  ct %>%
    left_join(attr_ %>% select(name, w = all_of(wcol)), by = "name") %>%
    group_by(zone) %>%
    summarise(A = sum(w * cij^(-b)), .groups = "drop")
}

cumulative <- function(ct) {
  purrr::map(THRESHOLDS, function(t) {
    ct %>%
      left_join(attr_ %>% select(name, pan), by = "name") %>%
      group_by(zone) %>%
      summarise("places_{t}" := sum(pan[cij <= t]), .groups = "drop")
  }) %>% purrr::reduce(full_join, by = "zone")
}

nearest_of <- function(ct) {
  ct %>%
    group_by(zone) %>%
    slice_min(cij, n = 1, with_ties = FALSE) %>%
    transmute(zone, nearest_school = name, nearest_min = cij)
}

acc_zone <- hansen(beta_ref) %>% rename(A_hansen = A) %>%
  left_join(hansen(beta_lo) %>% rename(A_lo = A), by = "zone") %>%
  left_join(hansen(beta_hi) %>% rename(A_hi = A), by = "zone") %>%
  # The admission-number specification, kept for the sensitivity check
  # below rather than for reporting.
  left_join(hansen(beta_ref, "W_pan") %>% rename(A_pan = A), by = "zone") %>%
  left_join(cumulative(costs), by = "zone") %>%
  left_join(nearest_of(costs), by = "zone") %>%
  left_join(zones, by = "zone")

# ---- The same city with Longhill at Elm Grove ------------------------
# costs_elm is the identical zone x school table with Longhill's column
# recomputed from the alternative site. Everything else is unchanged, so
# any difference in the surface is attributable to the move alone.

elm_zone <- hansen(beta_ref, ct = costs_elm) %>% rename(A_elm = A) %>%
  left_join(cumulative(costs_elm) %>%
              rename_with(~ paste0(.x, "_elm"), starts_with("places_")),
            by = "zone") %>%
  left_join(nearest_of(costs_elm) %>%
              rename(nearest_school_elm = nearest_school,
                     nearest_min_elm = nearest_min), by = "zone")

acc_zone <- acc_zone %>% left_join(elm_zone, by = "zone")

# ---- LSOA level ------------------------------------------------------
# Zones are LSOA x catchment: an LSOA that a catchment boundary runs
# through appears twice. Collapse to LSOA weighting by the child
# population, so a split LSOA is represented by where its children
# actually are rather than by an unweighted average of its halves.

wmean <- function(x, w) if (sum(w, na.rm = TRUE) > 0) weighted.mean(x, w, na.rm = TRUE) else mean(x, na.rm = TRUE)

acc_lsoa <- acc_zone %>%
  group_by(lsoa) %>%
  summarise(
    # Every weighted mean below uses the zone-level Oi vector, so the
    # summed total has to be assigned last: dplyr evaluates these in
    # order and an earlier `Oi = sum(Oi)` would rebind the weights to a
    # scalar before the means are taken.
    A_hansen    = wmean(A_hansen, Oi),
    A_lo        = wmean(A_lo, Oi),
    A_hi        = wmean(A_hi, Oi),
    A_pan       = wmean(A_pan, Oi),
    A_elm       = wmean(A_elm, Oi),
    across(starts_with("places_"), ~ wmean(.x, Oi)),
    nearest_min = wmean(nearest_min, Oi),
    nearest_min_elm = wmean(nearest_min_elm, Oi),
    nearest_school = nearest_school[which.max(Oi)],
    nearest_school_elm = nearest_school_elm[which.max(Oi)],
    catchment   = catchment[which.max(Oi)],
    area        = area[which.max(Oi)],
    Oi          = sum(Oi, na.rm = TRUE),
    .groups = "drop")

# Index the gravity surface so 100 is the child-weighted city average.
# The raw number has no interpretable units; the index does.
base <- wmean(acc_lsoa$A_hansen, acc_lsoa$Oi)
acc_lsoa <- acc_lsoa %>%
  mutate(A_index = 100 * A_hansen / base,
         # Indexed on the same base as the current surface, so the two
         # are directly comparable and a value over 100 in the Elm Grove
         # column means better than today's city average.
         A_index_elm = 100 * A_elm / base,
         A_pct   = 100 * percent_rank(A_hansen),
         P30_pct = 100 * percent_rank(places_30))

# ---- What relocating Longhill would do to the whole city -------------
# One school moving changes every neighbourhood's accessibility a
# little, because the gravity measure sums over all schools. Report the
# city-wide effect, child-weighted, alongside the count of
# neighbourhoods that cross the 30-minute threshold either way.

city <- list(
  A_now  = wmean(acc_lsoa$A_hansen, acc_lsoa$Oi),
  A_elm  = wmean(acc_lsoa$A_elm, acc_lsoa$Oi),
  p30_now = wmean(acc_lsoa$places_30, acc_lsoa$Oi),
  p30_elm = wmean(acc_lsoa$places_30_elm, acc_lsoa$Oi),
  near_now = wmean(acc_lsoa$nearest_min, acc_lsoa$Oi),
  near_elm = wmean(acc_lsoa$nearest_min_elm, acc_lsoa$Oi),
  zero30_now = sum(acc_lsoa$places_30 < 1),
  zero30_elm = sum(acc_lsoa$places_30_elm < 1),
  zero30_children_now = sum(acc_lsoa$Oi[acc_lsoa$places_30 < 1]),
  zero30_children_elm = sum(acc_lsoa$Oi[acc_lsoa$places_30_elm < 1]),
  better = sum(acc_lsoa$A_elm > acc_lsoa$A_hansen),
  worse  = sum(acc_lsoa$A_elm < acc_lsoa$A_hansen),
  n = nrow(acc_lsoa))
city$A_pct_change <- 100 * (city$A_elm - city$A_now) / city$A_now

message(sprintf(
  "\n  Relocating Longhill to Elm Grove, city-wide and child-weighted:\n    gravity accessibility %+.1f%%; places within 30 min %.0f -> %.0f; nearest school %.1f -> %.1f min",
  city$A_pct_change, city$p30_now, city$p30_elm, city$near_now, city$near_elm))
message(sprintf(
  "    neighbourhoods better off %d, worse off %d, of %d",
  city$better, city$worse, city$n))
message(sprintf(
  "    reaching no place within 30 min: %d (%.0f children) -> %d (%.0f children)",
  city$zero30_now, city$zero30_children_now,
  city$zero30_elm, city$zero30_children_elm))

# ---- Against the statutory guidance ----------------------------------
# The DfE limit applies to the journey a child actually has to make, so
# the right test is the time to the school their catchment entitles them
# to - the nearest of that catchment's schools - not to the nearest
# school of any kind.

catch_sch <- tibble::enframe(oi$catchment_schools, name = "catchment",
                             value = "name") %>%
  tidyr::unnest(name) %>%
  filter(lengths(name) > 0 | nzchar(name))

catch_time <- costs %>%
  inner_join(zones %>% select(zone, catchment, Oi), by = "zone") %>%
  inner_join(catch_sch, by = c("catchment", "name")) %>%
  group_by(zone, catchment) %>%
  summarise(catch_min = min(cij), catch_school = name[which.min(cij)],
            Oi = first(Oi), .groups = "drop")

statutory <- catch_time %>%
  summarise(
    zones = n(),
    children = sum(Oi),
    mean_min = weighted.mean(catch_min, Oi),
    over_45 = sum(catch_min > DFE_MAX_PRI),
    over_45_children = sum(Oi[catch_min > DFE_MAX_PRI]),
    over_75 = sum(catch_min > DFE_MAX_SEC),
    over_75_children = sum(Oi[catch_min > DFE_MAX_SEC]),
    worst = max(catch_min))

by_catch_stat <- catch_time %>%
  group_by(catchment) %>%
  summarise(mean_min = weighted.mean(catch_min, Oi),
            max_min = max(catch_min),
            over_45 = sum(catch_min > DFE_MAX_PRI),
            children_over_45 = sum(Oi[catch_min > DFE_MAX_PRI]),
            .groups = "drop") %>%
  arrange(desc(mean_min))

message("\n  Journey to the catchment's own school, against DfE guidance:")
message(sprintf("    child-weighted mean %.1f min; worst zone %.0f min",
                statutory$mean_min, statutory$worst))
message(sprintf("    over 45 min (primary limit): %d zones, %.0f children",
                statutory$over_45, statutory$over_45_children))
message(sprintf("    over 75 min (secondary limit): %d zones, %.0f children",
                statutory$over_75, statutory$over_75_children))
print(as.data.frame(by_catch_stat %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))), row.names = FALSE)

# ---- Cumulative opportunity across a grid of thresholds --------------
# Long format, LSOA x threshold x scenario, for the interactive map.

places_at <- function(ct, scenario) {
  purrr::map_dfr(THRESH_GRID, function(t) {
    ct %>%
      left_join(attr_ %>% select(name, pan), by = "name") %>%
      group_by(zone) %>%
      summarise(places = sum(pan[cij <= t]), .groups = "drop") %>%
      left_join(zones %>% select(zone, lsoa, Oi), by = "zone") %>%
      group_by(lsoa) %>%
      summarise(places = wmean(places, Oi), Oi = sum(Oi), .groups = "drop") %>%
      mutate(t = t, scenario = scenario)
  })
}

# Places per child: the same measure divided by the number of
# cohort-aged children who would be competing for them. A neighbourhood
# with 400 reachable places and 60 children is in a different position
# from one with 400 places and 15, and the unweighted map cannot show
# that.

thresh_long <- bind_rows(places_at(costs, "now"),
                         places_at(costs_elm, "elm")) %>%
  mutate(per_child = places / pmax(Oi, 1))

thresh_city <- thresh_long %>%
  group_by(scenario, t) %>%
  summarise(mean_places = wmean(places, Oi),
            stranded = sum(places < 1),
            stranded_children = sum(Oi[places < 1]),
            .groups = "drop")

message("\n  Cumulative opportunity across thresholds (child-weighted mean places):")
print(as.data.frame(thresh_city %>%
  tidyr::pivot_wider(id_cols = t, names_from = scenario,
                     values_from = c(mean_places, stranded)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))), row.names = FALSE)

# ---- Do the two measures agree? --------------------------------------

rho <- cor(acc_lsoa$A_hansen, acc_lsoa$places_30, method = "spearman")
message(sprintf("  Spearman correlation, gravity vs places within 30 min: %.2f", rho))

# ---- Does the attractiveness specification matter? -------------------
# It changes W a great deal at school level - Longhill falls from 1.08 on
# admission numbers to about 0.3 on preferences - but the surface is a
# sum over eleven schools, so most of that cancels. Report it rather
# than assume it.
w_rho <- cor(acc_lsoa$A_hansen, acc_lsoa$A_pan, method = "spearman")
w_dec <- mean(ntile(acc_lsoa$A_hansen, 10) == ntile(acc_lsoa$A_pan, 10))
w_dec1 <- mean(abs(ntile(acc_lsoa$A_hansen, 10) - ntile(acc_lsoa$A_pan, 10)) <= 1)
message(sprintf(
  "  Preference-weighted vs admission-number W: Spearman %.3f, same decile %.0f%%, within one %.0f%%",
  w_rho, 100 * w_dec, 100 * w_dec1))

# The same comparison across all four specifications, at LSOA level.
w_specs <- c("W_pan", "W_p1", "W_alloc", "W_pref")
w_surface <- purrr::map_dfc(w_specs, function(w) {
  s <- hansen(beta_ref, w) %>%
    inner_join(zones %>% select(zone, lsoa, Oi), by = "zone") %>%
    group_by(lsoa) %>% summarise(A = wmean(A, Oi), .groups = "drop")
  setNames(tibble(s$A), w)
})
w_cor <- cor(w_surface, method = "spearman")
message("\n  Accessibility surface under each attractiveness specification (Spearman):")
print(round(w_cor, 3))

decile_flip <- acc_lsoa %>%
  mutate(d_grav = ntile(A_hansen, 10), d_cum = ntile(places_30, 10)) %>%
  summarise(same = mean(d_grav == d_cum), within1 = mean(abs(d_grav - d_cum) <= 1))
message(sprintf("  Same decile on both: %.0f%%; within one decile: %.0f%%",
                100 * decile_flip$same, 100 * decile_flip$within1))

# ---- Accessibility against deprivation -------------------------------
# The bivariate map in section 4.5. Terciles rather than quintiles: with
# 179 LSOAs a 5 x 5 grid leaves categories with a handful of areas in
# them, which reads as noise.

biv <- acc_lsoa %>%
  left_join(idaci, by = "lsoa") %>%
  filter(!is.na(idaci_score)) %>%
  mutate(
    # Both axes are oriented worst-to-best, so tercile 1 is the bad end
    # on each and the corner of concern is 1-1. Accessibility already
    # runs that way; deprivation does not, because the IDACI score rises
    # with child poverty, so it is inverted here. Without that the grid
    # reads in opposite directions on its two axes and the "bad corner"
    # is a mismatched pair of numbers, which is a good way to misread a
    # bivariate map.
    acc_t   = ntile(A_hansen, 3),
    dep_t   = 4 - ntile(idaci_score, 3),
    biv_key = paste0(acc_t, "-", dep_t))

# The corner that matters: worst access and highest child poverty, now
# the 1-1 cell on both axes.
worst <- biv %>% filter(acc_t == 1, dep_t == 1)
best  <- biv %>% filter(acc_t == 3, dep_t == 3)
message(sprintf("  LSOAs in the low-access / high-deprivation corner: %d (%.0f children)",
                nrow(worst), sum(worst$Oi, na.rm = TRUE)))

acc_dep_cor <- cor(biv$A_hansen, biv$idaci_score, method = "spearman")
message(sprintf("  Spearman, accessibility vs IDACI score: %.2f", acc_dep_cor))

saveRDS(list(
  zone = acc_zone, lsoa = acc_lsoa, bivariate = biv,
  attract = attr_,
  pref = list(decay = PREF_DECAY, years = PREF_YEARS,
              imputed = attr_$name[attr_$imputed]),
  w_check = list(spearman = w_rho, same_decile = w_dec, within_one = w_dec1),
  w_cor = w_cor, w_specs = w_specs,
  benchmarks = list(nts_min = NTS_MEAN_MIN, nts_miles = NTS_MEAN_MILES,
                    dfe_max_sec = DFE_MAX_SEC, dfe_max_pri = DFE_MAX_PRI),
  walk_fallback = list(
    n = sum(costs$walk_capped), total = nrow(costs),
    share = mean(costs$walk_capped),
    kmh = 1.030 / (14.22 / 60), circuity = 1.3,
    review = list(dist_m = 1030, time_min = 14.22,
                  dist_range = c(349, 1800), time_range = c(6.9, 26.7))),
  catch_time = catch_time, statutory = statutory,
  by_catch_stat = by_catch_stat,
  city = city,
  thresh_grid = THRESH_GRID,
  thresh_long = thresh_long,
  thresh_city = thresh_city,
  beta = list(ref = beta_ref, lo = beta_lo, hi = beta_hi),
  thresholds = THRESHOLDS,
  agreement = list(spearman = rho, same_decile = decile_flip$same,
                   within_one = decile_flip$within1),
  acc_dep_spearman = acc_dep_cor,
  worst_corner = worst, best_corner = best,
  index_base = base,
  run_at = Sys.time()),
  file.path(DATA, "accessibility.rds"))

message("\nSaved data/accessibility.rds")
