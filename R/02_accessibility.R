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
idaci <- bh_data("deprivation_open.rds")$idaci

zones <- oi$zones %>% select(zone, lsoa, catchment, area, Oi, zone_e, zone_n)
costs <- oi$costs_now
attr_ <- oi$attract %>% select(name, pan, W_pan)

# The travel table names Hove Park without its sixth-form suffix; the
# schools table carries the full DfE name. Join on what both agree on.
stopifnot(all(costs$name %in% attr_$name))

# ---- Decay parameter -------------------------------------------------
# The open model sweeps beta rather than calibrating it, because
# calibration needs pupil-level flows. Take the sweep's own reference
# value where there is one, and report the surface at the ends of the
# swept range too, so a reader can see how much the choice matters.

beta_ref <- suppressWarnings(as.numeric(env$reference_beta))
if (!length(beta_ref) || !is.finite(beta_ref)) beta_ref <- as.numeric(br$beta_original)
if (!is.finite(beta_ref)) beta_ref <- stats::median(env$betas)
beta_lo <- min(env$betas); beta_hi <- max(env$betas)
message(sprintf("  beta reference %.2f (swept range %.1f to %.1f)",
                beta_ref, beta_lo, beta_hi))

THRESHOLDS <- c(30, 45)

# ---- Zone-level measures ---------------------------------------------

hansen <- function(b) {
  costs %>%
    left_join(attr_, by = "name") %>%
    group_by(zone) %>%
    summarise(A = sum(W_pan * cij^(-b)), .groups = "drop")
}

acc_zone <- hansen(beta_ref) %>% rename(A_hansen = A) %>%
  left_join(hansen(beta_lo) %>% rename(A_lo = A), by = "zone") %>%
  left_join(hansen(beta_hi) %>% rename(A_hi = A), by = "zone")

costs_w <- costs %>% left_join(attr_, by = "name")

cum <- purrr::map(THRESHOLDS, function(t) {
  costs_w %>%
    group_by(zone) %>%
    summarise("places_{t}" := sum(pan[cij <= t]), .groups = "drop")
}) %>% purrr::reduce(full_join, by = "zone")

# Nearest school by routed time - the plainest access statistic there is.
nearest <- costs %>%
  group_by(zone) %>%
  slice_min(cij, n = 1, with_ties = FALSE) %>%
  transmute(zone, nearest_school = name, nearest_min = cij)

acc_zone <- acc_zone %>%
  left_join(cum, by = "zone") %>%
  left_join(nearest, by = "zone") %>%
  left_join(zones, by = "zone")

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
    across(starts_with("places_"), ~ wmean(.x, Oi)),
    nearest_min = wmean(nearest_min, Oi),
    nearest_school = nearest_school[which.max(Oi)],
    catchment   = catchment[which.max(Oi)],
    area        = area[which.max(Oi)],
    Oi          = sum(Oi, na.rm = TRUE),
    .groups = "drop")

# Index the gravity surface so 100 is the child-weighted city average.
# The raw number has no interpretable units; the index does.
base <- wmean(acc_lsoa$A_hansen, acc_lsoa$Oi)
acc_lsoa <- acc_lsoa %>%
  mutate(A_index = 100 * A_hansen / base,
         A_pct   = 100 * percent_rank(A_hansen),
         P30_pct = 100 * percent_rank(places_30))

# ---- Do the two measures agree? --------------------------------------

rho <- cor(acc_lsoa$A_hansen, acc_lsoa$places_30, method = "spearman")
message(sprintf("  Spearman correlation, gravity vs places within 30 min: %.2f", rho))

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
    # Both axes run low to high: acc_t 1 is the least reachable third,
    # dep_t 3 the most deprived third (IDACI score rises with child
    # poverty). The corner of concern is therefore acc_t 1 with dep_t 3,
    # not a matching pair of numbers.
    acc_t   = ntile(A_hansen, 3),
    dep_t   = ntile(idaci_score, 3),
    biv_key = paste0(acc_t, "-", dep_t))

# The corner that matters: worst access and highest child poverty.
worst <- biv %>% filter(acc_t == 1, dep_t == 3)
message(sprintf("  LSOAs in the low-access / high-deprivation corner: %d (%.0f children)",
                nrow(worst), sum(worst$Oi, na.rm = TRUE)))

acc_dep_cor <- cor(biv$A_hansen, biv$idaci_score, method = "spearman")
message(sprintf("  Spearman, accessibility vs IDACI score: %.2f", acc_dep_cor))

saveRDS(list(
  zone = acc_zone, lsoa = acc_lsoa, bivariate = biv,
  beta = list(ref = beta_ref, lo = beta_lo, hi = beta_hi),
  thresholds = THRESHOLDS,
  agreement = list(spearman = rho, same_decile = decile_flip$same,
                   within_one = decile_flip$within1),
  acc_dep_spearman = acc_dep_cor,
  worst_corner = worst,
  index_base = base,
  run_at = Sys.time()),
  file.path(DATA, "accessibility.rds"))

message("\nSaved data/accessibility.rds")
