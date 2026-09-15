# R/05_app_inputs.R — assemble everything the policy simulator needs
# ======================================================================
# The app in app/ runs the spatial interaction model live, on every
# slider move. It does NOT read precomputed scenario outputs, because
# the questions it exists to answer are continuous ones - "how
# attractive would Longhill have to be to fill 210 places?" - and no
# precomputed grid answers those.
#
# What IS precomputed is the ingredients: the routed cost matrices, the
# zone demand, the baseline attractiveness, the catchment designs, the
# fitted parameters, and the finance and deprivation figures the
# outcomes are scored against. The model itself is 165 zones x 10
# schools and runs in milliseconds.
#
# Everything here comes from data/, which R/01_assemble.R fills. Nothing
# new is computed except the reshaping.
#
# Output: app/data/sim_inputs.rds
# ======================================================================

source(here::here("R", "00_core.R"))

message("\n=== Policy simulator inputs ===")

oi  <- bh_data("open_inputs.rds")
mt  <- bh_data("model_terms.rds")
fr  <- bh_data("flow_regions.rds")
sfin <- bh_data("school_finance.rds")
dep <- bh_data("deprivation_open.rds")

APP_DIR <- file.path(ROOT, "app")
dir.create(file.path(APP_DIR, "data"), showWarnings = FALSE, recursive = TRUE)

# ---- 1. Schools ------------------------------------------------------
# The ten schools with a Brighton admission number. Peacehaven is in the
# cost matrix because Brighton children go there, but it is an East
# Sussex school: it is carried as a destination so children can leave
# the city, and it is not something the app lets you adjust.

CITY <- oi$schools$name[oi$schools$name != "Peacehaven Community School"]

# Four East Sussex schools are destinations too: children do leave the
# city, most of them from Longhill's catchment for Priory School in Lewes.
# M5 fits them to the council's published FOI table of 2024 offers by
# catchment and school (section 7.6). They take children; nothing about
# them is adjustable in the app.
OUTSIDE <- mt$calibrated$outside
stopifnot(!is.null(OUTSIDE), length(OUTSIDE$W) == 4, is.finite(OUTSIDE$decay))
OUT_OF_CITY <- OUTSIDE$schools$name
OUT_SHORT <- c(`Priory School` = "Priory (Lewes)", `Peacehaven Community School` = "Peacehaven",
               `Seahaven Academy` = "Seahaven", `Seaford Head School` = "Seaford Head")
stopifnot(all(OUT_OF_CITY %in% names(OUT_SHORT)))

# CoMArt, the East Brighton school closed in 2005, for the scenarios that
# open it again. Routed like every other school in the open model (00e).
# There are no preferences for a school that does not exist, so it starts
# with Brighton Aldridge's attractiveness: the nearest thing to it, a small
# community school in the east of the city. The app lets that be moved,
# and the scenario analysis below tries two other values.
CMC <- bh_data("comart_costs.rds")
CM_SCEN <- list(name = "CoMArt", short = "CoMArt", joins = "Longhill High School",
                w_from = "Brighton Aldridge Community Academy", pan = 150)
message(sprintf("  CoMArt routing check: median |difference| %.1f min against the matrix, r = %.3f",
                CMC$check$median_abs_diff, CMC$check$correlation))

# The six schools the council is the admission authority for, and so the
# only ones its oversubscription priorities apply to.
COMMUNITY <- c("Blatchington Mill School", "Dorothy Stringer School",
               "Hove Park School", "Longhill High School",
               "Patcham High School", "Varndean School")

schools <- oi$schools %>%
  transmute(name, short = short_sch(name), urn = as.character(urn),
            easting, northing, faith,
            pan = pan2026,
            city = name %in% CITY,
            community = name %in% COMMUNITY) %>%
  left_join(oi$attract %>% select(name, W = W_wprefs), by = "name") %>%
  left_join(
    purrr::imap_dfr(oi$catchment_schools, ~ tibble(name = .x, group = .y)),
    by = "name")

schools <- bind_rows(
  schools %>% filter(city),
  OUTSIDE$schools %>%
    transmute(name, short = unname(OUT_SHORT[name]), urn = as.character(urn),
              easting, northing, faith = FALSE, pan = NA_real_,
              city = FALSE, community = FALSE,
              W = unname(OUTSIDE$W[name]), group = NA_character_))

stopifnot(!any(is.na(schools$W)), !any(is.na(schools$pan[schools$city])))

# The Elm Grove site is a different point on the map, so the dot has to
# move when the user moves the school.
elm <- oi$elm_grove
schools$elm_easting  <- schools$easting
schools$elm_northing <- schools$northing
lh <- schools$name == "Longhill High School"
schools$elm_easting[lh]  <- elm$easting
schools$elm_northing[lh] <- elm$northing

message(sprintf("  %d schools, %d of them in the city", nrow(schools), sum(schools$city)))

# ---- 2. Zones and costs ---------------------------------------------
# Zones are LSOA x catchment. The model runs on zones; the map and the
# deprivation scoring work at LSOA level.

zones <- oi$zones %>%
  filter(area != "Expansion area") %>%
  transmute(zone, lsoa, catchment, Oi) %>%
  # IDACI score: the share of children living in income-deprived
  # households, per neighbourhood. It is the raw material for the free
  # school meals priority; the share actually eligible and claiming is
  # calibrated against published offers in section 10.
  left_join(bh_data("deprivation_open.rds")$idaci %>%
              select(lsoa, idaci_score), by = "lsoa")
stopifnot(!any(is.na(zones$idaci_score)))
zones <- zones

# The East Sussex schools' costs come from M5: straight-line km, which is
# what they are chosen on, and routed minutes for reporting. Moving
# Longhill does not move them.
ext_cost <- OUTSIDE$costs %>% filter(zone %in% zones$zone) %>% select(zone, name, cij, km)
# CoMArt's routed costs are carried always and used only when it is open.
cm_cost <- CMC$costs %>% filter(zone %in% zones$zone) %>%
  transmute(zone, name = CM_SCEN$name, cij, km)
stopifnot(nrow(cm_cost) == nrow(zones))
cost <- list(
  now = bind_rows(oi$costs_now %>% filter(zone %in% zones$zone, name %in% CITY) %>%
                    select(zone, name, cij, km), ext_cost, cm_cost),
  elm = bind_rows(oi$costs_elm %>% filter(zone %in% zones$zone, name %in% CITY) %>%
                    select(zone, name, cij, km), ext_cost, cm_cost))

stopifnot(all(vapply(cost, function(x) all(is.finite(x$cij)), logical(1))))

message(sprintf("  %d zones, %s children, %d cost pairs",
                nrow(zones), fmt_n(sum(zones$Oi)), nrow(cost$now)))

# ---- 3. Catchment designs -------------------------------------------
# Each design is a pair of lookups: which group a neighbourhood belongs
# to, and which group a school belongs to. The model's catchment term
# fires when the two match.
#
# The current map is the one in the zone table itself. The rest come out
# of R/03_flow_regions.R, which works in whole LSOAs, so a zone takes
# its LSOA's group.

# The catchment term is looked up per ZONE, not per neighbourhood. 30 of
# the 165 LSOAs straddle a current catchment boundary and exist as two
# zones with different catchments; collapsing them to one moved four
# children at Patcham and put the app out of step with the published
# model.
design_from <- function(assign, groups) {
  sch_of <- setNames(rep(names(groups), lengths(groups)), unlist(groups))
  lsoa_grp <- setNames(unname(sch_of[assign$region]), assign$lsoa)
  list(zone = setNames(unname(lsoa_grp[zones$lsoa]), zones$zone),
       lsoa = lsoa_grp,
       school = sch_of)
}

DESIGNS <- list(
  `Current catchments` = list(
    zone = setNames(zones$catchment, zones$zone),
    lsoa = setNames(zones$catchment[!duplicated(zones$lsoa)],
                    zones$lsoa[!duplicated(zones$lsoa)]),
    school = setNames(schools$group, schools$name)),
  `Power diagram` = design_from(fr$pd_assign, fr$groups_now),
  `Flow regions, pairs kept` = design_from(fr$regions$paired$assign,
                                           fr$groups_paired),
  `Flow regions, one per school` = design_from(fr$regions$single$assign,
                                               fr$groups_single),
  `Flow regions, Longhill at Elm Grove` = design_from(fr$regions$elm$assign,
                                                      fr$groups_paired))

# Two maps to judge the 2024 change by. The 2024 map moved the zones
# around CoMArt's old site in Whitehawk out of Longhill's catchment into
# Stringer / Varndean's, and moved others, further out, the other way. So
# the pre-2024 map is one option, and today's map with only the Whitehawk
# zones put back in Longhill's catchment is another, narrower one. An LSOA
# split between catchments takes the one most of its children are in, for
# drawing; the model itself runs on zones.
pre_catch <- setNames(oi$zones$catch_pre2024, oi$zones$zone)[zones$zone]
wh_catch <- ifelse(zones$catchment == "DS_Varndean" & pre_catch == "Longhill",
                   "Longhill", zones$catchment)
lsoa_major <- function(grp) {
  d <- tibble(lsoa = zones$lsoa, grp = grp, Oi = zones$Oi) %>%
    group_by(lsoa, grp) %>% summarise(Oi = sum(Oi), .groups = "drop") %>%
    group_by(lsoa) %>% slice_max(Oi, n = 1, with_ties = FALSE) %>% ungroup()
  setNames(d$grp, d$lsoa)
}
DESIGNS[["Pre-2024 catchments"]] <- list(
  zone = setNames(unname(pre_catch), zones$zone), lsoa = lsoa_major(unname(pre_catch)),
  school = setNames(schools$group, schools$name))
DESIGNS[["Current catchments, Whitehawk back to Longhill"]] <- list(
  zone = setNames(wh_catch, zones$zone), lsoa = lsoa_major(wh_catch),
  school = setNames(schools$group, schools$name))
stopifnot(!any(is.na(pre_catch)), sum(wh_catch != zones$catchment) > 0)
message(sprintf("  Whitehawk back to Longhill: %d zones, %.0f children, moved from Stringer / Varndean",
                sum(wh_catch != zones$catchment), sum(zones$Oi[wh_catch != zones$catchment])))

# A design that does not cover a neighbourhood would silently switch the
# catchment term off there, so every one is checked against the zones.
for (d in names(DESIGNS)) {
  miss <- setdiff(unique(zones$lsoa), names(DESIGNS[[d]]$lsoa))
  if (length(miss))
    stop(sprintf("design '%s' misses %d neighbourhoods", d, length(miss)))
  if (any(is.na(DESIGNS[[d]]$zone[zones$zone])))
    stop(sprintf("design '%s' leaves zones without a catchment", d))
}
message(sprintf("  %d catchment designs, all covering every neighbourhood",
                length(DESIGNS)))

# ---- 4. Fitted parameters -------------------------------------------

# M5, the open model's calibrated rung: a catchment term per catchment and
# the paired-catchment exclusivity, both fitted to the council's
# catchment x school preference matrix, and attractiveness balanced to
# first preferences. See section 7.6 of the document.
cal <- mt$calibrated
stopifnot(!is.null(cal), length(cal$gamma) == 6, all(is.finite(cal$W)))
params <- list(beta = cal$beta, sigma = cal$sigma, delta = cal$delta,
               gamma = cal$gamma, gamma_m4 = cal$gamma_m4,
               exclusive = cal$exclusive %>%
                 select(catchment, school, share, share_lo, share_hi),
               # Where refused children go: each home catchment's second
               # preferences, as shares.
               overflow = with(cal$overflow, setNames(share, paste(catchment, school))),
               # The East Sussex schools: attractiveness each, decay on km.
               outside = list(W = OUTSIDE$W, decay = OUTSIDE$decay))
stopifnot(length(params$overflow) > 0, all(is.finite(params$overflow)))
message(sprintf("  beta %.2f, delta %.2f, sigma %.0f; catchment terms %s",
                params$beta, params$delta, params$sigma,
                paste(sprintf("%s %.1f", names(params$gamma), params$gamma), collapse = ", ")))

# Attractiveness is M5's balanced W for the city schools. Peacehaven is not
# modelled as a destination and keeps its published-preference value.
schools$W[schools$city] <- unname(cal$W[schools$name[schools$city]])
stopifnot(!any(is.na(schools$W)))

# CoMArt's row: not a city school unless a scenario opens it (run_sim).
schools <- bind_rows(schools %>% mutate(hypothetical = FALSE), tibble(
  name = CM_SCEN$name, short = CM_SCEN$short, urn = "comart",
  easting = CMC$site$easting, northing = CMC$site$northing, faith = FALSE,
  pan = CM_SCEN$pan, city = FALSE, community = FALSE,
  W = schools$W[schools$name == CM_SCEN$w_from], group = NA_character_,
  elm_easting = CMC$site$easting, elm_northing = CMC$site$northing,
  hypothetical = TRUE))
stopifnot(sum(schools$hypothetical) == 1, is.finite(schools$W[schools$hypothetical]))

# ---- 4b. What attractiveness means in Attainment 8 ------------------
# Section 5.4 finds that the published number families respond to is
# headline Attainment 8. The app needs that relationship the other way
# round: given a multiplier on attractiveness, how many Attainment 8
# points is that?
#
# The sliders multiply M5's balanced attractiveness, so the fit is on
# that, not on weighted preferences per place: a multiplier has to be
# converted on the scale it is applied to. Section 5.4 shows the log scale
# is the right one for it (a straight line fits far worse), so the slope
# is a constant proportional effect per point and the inversion is just
# log(m) / b. Fitted on the ten city schools. It is a looser fit than
# preferences per place, largely because the two faith schools have no
# catchment term and so carry high M5 attractiveness.

att_fit <- lm(log(W5) ~ att8,
              data = oi$attract %>% filter(name %in% CITY) %>%
                mutate(W5 = unname(cal$W[name])))
ATT_B <- unname(coef(att_fit)[["att8"]])

stopifnot(ATT_B > 0, summary(att_fit)$r.squared > 0.5)

attain <- list(
  slope = ATT_B,
  r2 = summary(att_fit)$r.squared,
  n = nobs(att_fit),
  se = unname(summary(att_fit)$coefficients["att8", "Std. Error"]),
  # A point of Attainment 8 is worth this much on the attractiveness
  # scale: about 12% more M5 attractiveness.
  per_point = exp(ATT_B) - 1)

# The national distribution, so the app can say where a required score
# would sit rather than just naming it. State-funded mainstream schools
# in the latest year the panel covers.
nat <- readRDS(file.path(DATA, "performance_panel.rds"))
nat_att <- nat$national$ATT8SCR
nat_att <- nat_att[is.finite(nat_att) & nat_att > 0]

attain$national <- list(
  year = nat$latest, n = length(nat_att),
  q = stats::quantile(nat_att, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)),
  ecdf = stats::ecdf(nat_att))

city_att <- oi$attract$att8[oi$attract$name %in% CITY]
attain$city <- list(min = min(city_att), median = stats::median(city_att),
                    max = max(city_att))

message(sprintf("  Attainment 8: %.4f log-W per point (R2 %.2f, n %d); a point is worth %.1f%%",
                attain$slope, attain$r2, attain$n, 100 * attain$per_point))
message(sprintf("  city %.1f to %.1f, median %.1f; nationally %d schools, median %.1f, 90th %.1f",
                attain$city$min, attain$city$max, attain$city$median,
                attain$national$n, attain$national$q[["50%"]],
                attain$national$q[["90%"]]))

# ---- 4c. And what that would mean for absence -----------------------
# Section 2.3 fits the specification from "How to Pull the Right Lever":
# log(Attainment 8) on logged deprivation, absence and EAL rates plus a
# centred prior-attainment score. Absence enters as an elasticity, so
# holding a school's intake still, the absence rate that goes with a
# target score inverts to
#
#   absence_needed = absence_now * (att8_target / att8_now)^(1 / b)
#
# with b the coefficient on log(absence). It is negative, so a higher
# score means a lower rate. This is the same "what it would take"
# framing as the attainment figures, and it carries the same warning:
# absence is not a dial a school turns either.

lever_d <- nat$national %>%
  filter(is.finite(ATT8SCR), ATT8SCR > 0, is.finite(PERCTOT), PERCTOT > 0,
         is.finite(PTFSM6CLA1A), PTFSM6CLA1A > 0,
         is.finite(PNUMEAL), PNUMEAL > 0, is.finite(KS2ASS)) %>%
  mutate(ks2_c = KS2ASS - 100)

lever <- lm(log(ATT8SCR) ~ log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) + ks2_c,
            data = lever_d)
ABS_B <- unname(coef(lever)[["log(PERCTOT)"]])
stopifnot(ABS_B < 0)

nat_abs <- lever_d$PERCTOT

attain$absence <- list(
  elasticity = ABS_B,
  r2 = summary(lever)$r.squared,
  n = nobs(lever),
  national = list(q = stats::quantile(nat_abs, seq(0, 1, 0.1)),
                  ecdf = stats::ecdf(nat_abs), n = length(nat_abs)))

attain$att8_deciles <- stats::quantile(nat_att, seq(0, 1, 0.1))

# Fixed axis limits for the app's two live charts. They have to be
# constant - an axis computed from the data rescales when a slider
# moves, which made the charts look inert - but the national extremes
# are outliers that would squeeze all ten schools into a third of the
# panel. The 1st to 99th percentile, widened to hold every city school,
# is stable and uses the space.
# The 1st percentile of Attainment 8 is 7.9, from schools with a handful
# of entries; anchoring there squeezes every Brighton school into the
# right third of the panel. The 10th to the 98th holds the whole decile
# rug and leaves room above the best score in the city.
lim_for <- function(x, city_vals, lo, hi, pad = 0.04) {
  q <- stats::quantile(x, c(lo, hi))
  r <- range(c(q, city_vals))
  r + c(-1, 1) * diff(r) * pad
}
attain$att8_lims <- lim_for(nat_att, city_att, lo = 0.10, hi = 0.98)

message(sprintf("  absence elasticity %.3f (R2 %.2f, n %s); national median %.1f%%, 90th %.1f%%",
                attain$absence$elasticity, attain$absence$r2,
                fmt_n(attain$absence$n),
                attain$absence$national$q[["50%"]],
                attain$absence$national$q[["90%"]]))

# Each school's own rate, over the same window the attainment mean uses,
# matched on URN because the panel calls Hove Park by its full name and
# the model does not.
school_abs <- nat$bh %>%
  group_by(urn = as.character(URN)) %>%
  summarise(absence = mean(PERCTOT, na.rm = TRUE),
            att8_panel = mean(ATT8SCR, na.rm = TRUE), .groups = "drop")

schools <- schools %>%
  left_join(oi$attract %>% select(name, att8), by = "name") %>%
  left_join(school_abs, by = "urn")

# Absence starts at zero, because an implied rate can land below
# anything any school actually reports and that is exactly the finding
# worth seeing rather than squashing against the edge.
attain$abs_lims <- lim_for(nat_abs, schools$absence[schools$city],
                           lo = 0.02, hi = 0.95)
attain$abs_lims[1] <- 0

stopifnot(!any(is.na(schools$att8[schools$city])),
          !any(is.na(schools$absence[schools$city])),
          # The panel mean and the attractiveness table's score are the
          # same window; if they drift apart the absence pairing is
          # against a different set of years from the attainment one.
          max(abs(schools$att8[schools$city] -
                    schools$att8_panel[schools$city])) < 0.5)

# ---- 5. Demand by year ----------------------------------------------
# The city's projected Year 7 cohort, as an index on 2026. The app
# scales every zone's Oi by it, which holds the geography of demand
# still and moves only its size - the projection is a city-wide one and
# does not claim to know which neighbourhood shrinks.

demand <- readr::read_csv(file.path(DATA, "open_demand_projection.csv"),
                          show_col_types = FALSE) %>%
  filter(area == "Brighton & Hove", entry_year >= 2026) %>%
  transmute(year = entry_year, cohort = state_demand,
            index = state_demand / state_demand[entry_year == 2026],
            extrapolated)

stopifnot(nrow(demand) >= 10, abs(demand$index[1] - 1) < 1e-9)

# The observed Year 7 series and the projection behind figure 11, so the
# app can put the places a user has set against the children there will
# be to fill them.
rc <- bh_data("reception_cohort.rds")
cohort <- list(observed = rc$secondary_city %>% select(year, y7 = y7_offers),
               projected = rc$projection %>% select(year = entry_year, central, lo, hi))
stopifnot(nrow(cohort$observed) > 5, nrow(cohort$projected) > 3)
message(sprintf("  Year 7 offers observed %d to %d, projected to %d",
                min(cohort$observed$year), max(cohort$observed$year),
                max(cohort$projected$year)))
message(sprintf("  demand %d to %d, index falls to %.2f",
                min(demand$year), max(demand$year), min(demand$index)))

# ---- 6. Finance ------------------------------------------------------
# The pupil-led funding rate, the fixed funding, and where each school's
# balance sheet stands - the same figures section 6 uses.

FUND_FIXED <- c("lump_sum_total_funding", "sparsity_total_funding",
                "split_site_total_funding",
                "national_non_domestic_rates_funding",
                "pfi_total_funding", "exceptional_factors_total_funding")

finance <- readr::read_csv(
    file.path(DATA, "school-funding-statistics_2025-26", "data",
              "20260129_School_level_data_csv.csv"),
    show_col_types = FALSE, guess_max = 5000) %>%
  filter(la_name == "Brighton and Hove", education_phase == "Secondary") %>%
  mutate(across(c(all_of(FUND_FIXED), total_schools_block_allocation_post_mfg,
                  total_number_of_pupils),
                ~ suppressWarnings(as.numeric(.x)))) %>%
  transmute(urn = as.character(school_urn),
            funded_roll = total_number_of_pupils,
            fixed = rowSums(across(all_of(FUND_FIXED)), na.rm = TRUE),
            marginal_pp = (total_schools_block_allocation_post_mfg -
                             rowSums(across(all_of(FUND_FIXED)), na.rm = TRUE)) /
                          total_number_of_pupils) %>%
  inner_join(sfin$exposure %>%
               transmute(urn, income, reserve, balance = balance_pct * income,
                         staff_pct, year_label),
             by = "urn") %>%
  inner_join(schools %>% select(urn, name, short), by = "urn")

stopifnot(nrow(finance) == sum(schools$city))
message(sprintf("  finance for %d schools; pupil-led rate £%s to £%s",
                nrow(finance), fmt_n(min(finance$marginal_pp)),
                fmt_n(max(finance$marginal_pp))))

# The observed Year 7 intakes the roll model is seeded from: five
# cohorts already in the building.
seed_intakes <- bh_data("factsheet_panel.rds")$factsheets %>%
  transmute(name, year, intake = off_total) %>%
  filter(year %in% 2022:2026, name %in% CITY)
stopifnot(nrow(seed_intakes) == 5 * sum(schools$city))

# ---- 7. Deprivation --------------------------------------------------
# Households with dependent children by neighbourhood, and how many of
# them are in the three most deprived deciles nationally. Gorard's index
# is computed over school INTAKES in the app, weighting each
# neighbourhood's children by that share.

idaci <- readr::read_csv(file.path(DATA, "postcode_children.csv"),
                         show_col_types = FALSE) %>%
  filter(!is.na(idaci_decile), hh_with_ch > 0) %>%
  group_by(lsoa) %>%
  summarise(hh = sum(hh_with_ch),
            dep3 = sum(hh_with_ch[idaci_decile <= 3]) / sum(hh_with_ch),
            .groups = "drop")

local({
  chk <- inner_join(idaci, dep$idaci %>% select(lsoa, deprived), by = "lsoa")
  stopifnot(all((chk$dep3 > 0.5) == chk$deprived))
})
message(sprintf("  %d neighbourhoods with an IDACI share, %d of them deprived",
                nrow(idaci), sum(idaci$dep3 > 0.5)))

# ---- 8. Geometry -----------------------------------------------------
# Simplified hard: this is redrawn on every slider move, so the payload
# matters more than the coastline does.

# The app used to carry sf objects and convert coordinates at run time,
# which meant the deployed app needed sf - and so, through leaflet, the
# whole compiled geospatial stack (sf, terra, sp, raster, s2, units).
# None of that is needed to DRAW a map that is already projected. So the
# projection and the simplification happen here, once, and the app
# receives lon/lat numbers and GeoJSON text.

design_sf <- fr$regions_sf %>%
  sf::st_transform(27700) %>% sf::st_simplify(dTolerance = 60) %>%
  sf::st_transform(4326)

# One GeoJSON FeatureCollection per catchment design, as a string.
design_geojson <- vapply(unique(design_sf$design), function(d) {
  g <- design_sf[design_sf$design == d, "grp"]
  f <- tempfile(fileext = ".geojson")
  sf::st_write(g, f, quiet = TRUE, delete_dsn = TRUE,
               layer_options = "COORDINATE_PRECISION=5")
  txt <- paste(readLines(f, warn = FALSE), collapse = "")
  unlink(f)
  txt
}, character(1))

# Outlines for the two maps built above, from the published boundary files
# rather than whole LSOAs: both boundaries run through LSOAs, so an outline
# dissolved from them put whole neighbourhoods on the wrong side. The model
# itself never used the LSOA version - its zones were split on both maps
# postcode by postcode. The pre-2024 outline is the council's file as
# published; the Whitehawk redraw is today's map with the part of
# Stringer / Varndean's catchment that was Longhill's before 2024 given back
# to Longhill, the same rule that moves the model's zones.
norm_catch <- function(g, field) {
  g %>% sf::st_transform(27700) %>% sf::st_make_valid() %>%
    mutate(grp = dplyr::recode(as.character(.data[[field]]),
      "Patcham HighSchool" = "Patcham", "StringerVarndean" = "DS_Varndean",
      "BrightonAldridge" = "BACA", "BlatchingtonHove" = "Hove_Blatch", "Portslade" = "PACA",
      "VarndeanStringer" = "DS_Varndean", "HoveBlatchington" = "Hove_Blatch")) %>%
    select(grp)
}
pre_sf <- norm_catch(bh_data("catchments_pre2024.geojson"), "AreaName")
now_sf <- norm_catch(bh_data("catchments_current.geojson"), "catchment")
stopifnot(setequal(pre_sf$grp, unique(design_sf$grp[design_sf$design == "Current catchments"])),
          setequal(now_sf$grp, pre_sf$grp))
lh_before <- sf::st_union(pre_sf[pre_sf$grp == "Longhill", ])
dsv_now <- sf::st_union(now_sf[now_sf$grp == "DS_Varndean", ])
given_back <- sf::st_intersection(dsv_now, lh_before)
wh_sf <- now_sf
sf::st_geometry(wh_sf)[wh_sf$grp == "DS_Varndean"] <- sf::st_difference(dsv_now, lh_before)
sf::st_geometry(wh_sf)[wh_sf$grp == "Longhill"] <-
  sf::st_union(sf::st_union(now_sf[now_sf$grp == "Longhill", ]), given_back)
outline_sf <- list(`Pre-2024 catchments` = pre_sf,
                   `Current catchments, Whitehawk back to Longhill` = wh_sf)
for (nm in names(outline_sf)) {
  g <- outline_sf[[nm]] %>%
    group_by(grp) %>% summarise(.groups = "drop") %>%
    sf::st_simplify(dTolerance = 60) %>%
    sf::st_transform(4326)
  f <- tempfile(fileext = ".geojson")
  sf::st_write(g, f, quiet = TRUE, delete_dsn = TRUE,
               layer_options = "COORDINATE_PRECISION=5")
  design_geojson[[nm]] <- paste(readLines(f, warn = FALSE), collapse = "")
  unlink(f)
}

# School dots, projected once. Both sites, so moving Longhill to Elm
# Grove moves its dot without any run-time transformation.
to_ll <- function(e, n) {
  sf::st_coordinates(sf::st_transform(
    sf::st_as_sf(data.frame(e = e, n = n), coords = c("e", "n"), crs = 27700),
    4326))
}
xy <- to_ll(schools$easting, schools$northing)
schools$lon <- xy[, 1]; schools$lat <- xy[, 2]
xy <- to_ll(schools$elm_easting, schools$elm_northing)
schools$elm_lon <- xy[, 1]; schools$elm_lat <- xy[, 2]
stopifnot(all(is.finite(schools$lon)), all(is.finite(schools$elm_lat)),
          all(schools$lon > -0.4 & schools$lon < 0.2),
          all(schools$lat > 50.7 & schools$lat < 51.0))

message(sprintf("  %d design outlines as GeoJSON (%.0f KB), %d school dots projected",
                nrow(design_sf), sum(nchar(design_geojson)) / 1024, nrow(schools)))

# ---- 9. Presets ------------------------------------------------------
# Starting points, so the first thing a user sees is a question rather
# than a blank form. Each is a list of overrides the app applies on top
# of the baseline.

# The shrink scenario sets a city total rather than hand-picked numbers,
# so it is reproduced by moving the total-places slider: the 2035 cohort
# plus a 5% margin, in whole classes of 30.
COHORT_2035 <- demand$cohort[demand$year == 2035]
SHRINK_TOTAL <- 30 * round(COHORT_2035 * 1.05 / 30)
stopifnot(length(SHRINK_TOTAL) == 1, is.finite(SHRINK_TOTAL))

presets <- list(
  `Today` = list(
    note = "The city as it stands: today's admission numbers, today's catchments, Longhill at Ovingdean, and the council's 2026/27 admission priorities.",
    design = "Current catchments", site = "now", year = 2026,
    pan = NULL, w = NULL),
  `Do nothing until 2035` = list(
    note = "Everything held as it is, with the cohort ten years smaller. This is the do-nothing case the rest of the document describes.",
    design = "Current catchments", site = "now", year = 2035,
    pan = NULL, w = NULL),
  `Longhill to Elm Grove, PAN 150` = list(
    note = "The relocation, at the reduced admission number, behind the catchments drawn for it.",
    design = "Flow regions, Longhill at Elm Grove", site = "elm", year = 2030,
    pan = c(`Longhill High School` = 150), w = NULL),
  `Redraw for balance` = list(
    note = "The power diagram, which spreads disadvantage most evenly of the designs tested, with everything else as it is.",
    design = "Power diagram", site = "now", year = 2030,
    pan = NULL, w = NULL),
  `Shrink the system to fit` = list(
    note = sprintf("Total places cut to %s, about 5%% above the 2035 cohort of %s, and shared out in proportion to today's admission numbers. Move the total or any school from there.",
                   fmt_n(SHRINK_TOTAL), fmt_n(COHORT_2035)),
    design = "Current catchments", site = "now", year = 2035,
    total = SHRINK_TOTAL, pan = NULL, w = NULL),
  `Make the catchment count` = list(
    note = "Everything as it is, but living in a catchment weighs far more heavily on the choice than families currently behave as though it does. This is the other way to fill a school, and it needs nothing from the school itself.",
    design = "Current catchments", site = "now", year = 2026,
    gamma = 1.6, pan = NULL, w = NULL),
  `Make Longhill wanted` = list(
    note = "Longhill's attractiveness lifted to Dorothy Stringer's, everything else unchanged. The question is what it takes, and who loses the children.",
    design = "Current catchments", site = "now", year = 2026,
    pan = NULL,
    w = c(`Longhill High School` =
            schools$W[schools$name == "Dorothy Stringer School"] /
            schools$W[schools$name == "Longhill High School"])),
  # The app starts from the council's 2026/27 priorities, so the scenario
  # worth one click is the other rule: the model section 7 publishes.
  `Everyone has the same chance` = list(
    note = "Today's city with no oversubscription priorities: every applicant to a full school has the same chance, which is the model section 7 of the strategic view publishes. Compare the Catchments and Fairness tabs with Today, which runs the council's 2026/27 priorities.",
    design = "Current catchments", site = "now", year = 2026,
    pan = NULL, w = NULL, rule = "published"),
  `The council's first proposal: 20% open` = list(
    note = "The open-admissions priority as the council first consulted on it, at 20% of places rather than the 5% it settled on after objections from the six community schools. Watch displacement in the two dual catchments.",
    design = "Current catchments", site = "now", year = 2026,
    pan = NULL, w = NULL, rule = "priorities", p6 = 20, fsm = TRUE, targeted = FALSE),
  `The 2027/28 rules: Targeted FSM` = list(
    note = "The 2027/28 arrangements: the FSM priority narrowed to Targeted FSM, which the council puts at about 56% of currently eligible children. Everything else as in 2026/27.",
    design = "Current catchments", site = "now", year = 2027,
    pan = NULL, w = NULL, rule = "priorities", p6 = 5, fsm = TRUE, targeted = TRUE),
  `CoMArt re-opened: three small eastern schools` = list(
    note = "CoMArt, closed in 2005, open again on its East Brighton site with 150 places, sharing Longhill's catchment, and Brighton Aldridge and Longhill both cut to 150. Watch segregation, journeys, and whether the central schools still fill.",
    design = "Current catchments", site = "now", year = 2026,
    pan = c(`Brighton Aldridge Community Academy` = 150, `Longhill High School` = 150,
            CoMArt = 150),
    w = NULL, closed = character(0)),
  `CoMArt re-opened, and Stringer to 300` = list(
    note = "The same three small eastern schools, with Dorothy Stringer's admission number cut from 330 to 300. Does Stringer still fill, and where do the children go?",
    design = "Current catchments", site = "now", year = 2026,
    pan = c(`Brighton Aldridge Community Academy` = 150, `Longhill High School` = 150,
            `Dorothy Stringer School` = 300, CoMArt = 150),
    w = NULL, closed = character(0)))

# ---- 10. The admission rules, and FSM take-up -----------------------
# The app can run the council's oversubscription priorities as a tiered
# ceiling (app/R/model.R). Two things it needs are published: the rules,
# and what they produced in September 2026, from the council's Secondary
# school admissions guide 2027-2028. Those offers are made on national
# offer day, before six months of appeals and movement, which is the same
# basis the model runs on.
#
# One thing is not published: how many children in each neighbourhood are
# eligible for FSM AND claim the priority. The IDACI score is the shape;
# a single take-up constant sets the level, chosen so the model's FSM-
# priority offers at the three schools that ration match the 192 the
# council actually made there. How those 192 split between the schools,
# and everything under priority 6, is then not fitted.

RATIONING <- c("Blatchington Mill School", "Dorothy Stringer School",
               "Varndean School")

OUTTURN_2026 <- local({
  # SEN, then priorities 1 to 8
  w <- rbind(`Blatchington Mill School` = c(7, 3, 0, 73, 45, 14, 17, 171, 0),
             `Dorothy Stringer School`  = c(11, 3, 2, 65, 35, 32, 17, 148, 17),
             `Varndean School`          = c(19, 8, 1, 70, 61, 5, 15, 121, 0))
  tibble(school = rep(rownames(w), each = 9), priority = rep(0:8, times = 3),
         offers = as.vector(t(w)))
})
stopifnot(all(tapply(OUTTURN_2026$offers, OUTTURN_2026$school, sum) ==
                schools$pan[match(sort(RATIONING), schools$name)]))

RULES <- list(
  community = COMMUNITY, rationing = RATIONING,
  fsm_cap_share = 0.30, p6_share = 0.05,
  # The council's own figure for the Targeted FSM narrowing, as quoted in
  # the open model's FSM section.
  targeted_share = 0.56,
  outturn_2026 = OUTTURN_2026,
  source = "Brighton & Hove City Council, Secondary school admissions guide 2027-2028")

source(file.path(APP_DIR, "R", "model.R"))
cal <- list(schools = schools, zones = zones, cost = cost, designs = DESIGNS,
            params = params, demand = demand, rules = RULES)
fsm_target <- sum(OUTTURN_2026$offers[OUTTURN_2026$priority %in% 4:5])
fsm_places <- function(k) {
  cal$zones$fsm <- pmin(k * cal$zones$idaci_score, 0.95)
  t <- run_sim(cal, year = 2026, rules = list(rule = "priorities"))$tiers
  sum((t$p45_in + t$p45_out)[t$name %in% RATIONING])
}
lo <- 0.05; hi <- 6
if (fsm_places(hi) < fsm_target)
  stop("FSM take-up cannot reach the published FSM offers at any level")
for (i in 1:40) {
  mid <- (lo + hi) / 2
  if (fsm_places(mid) < fsm_target) lo <- mid else hi <- mid
  if (hi - lo < 1e-4) break
}
zones$fsm <- pmin(hi * zones$idaci_score, 0.95)
RULES$fsm_takeup <- hi
RULES$fsm_city <- weighted.mean(zones$fsm, zones$Oi)
message(sprintf("  FSM take-up %.3f x IDACI: %.0f FSM-priority places at the three rationing schools (published %d); %.1f%% of the city's children",
                hi, fsm_places(hi), fsm_target, 100 * RULES$fsm_city))

# ---- 10b. Disadvantaged pupils, calibrated to the published shares ---
# Checked against the Department for Education's published share of
# disadvantaged pupils at each school, the neighbourhood measure of an
# intake's deprivation got the schools in the wrong order (strategic view,
# section 11). So each school's pull on disadvantaged children is fitted
# here: a neighbourhood's disadvantaged share is its IDACI score scaled to
# the city's published average, and lambda_j is adjusted until every
# school's modelled share, in 2026 under the council's priorities,
# reproduces its published one. The target is the mean of the three
# latest years, which is steadier than one. The published figure is the
# whole school (Years 7 to 11), used here for its intake.
source(file.path(APP_DIR, "R", "outcomes.R"))
pub_dis <- nat$bh %>%
  mutate(urn = as.character(URN)) %>%
  filter(!is.na(PTFSM6CLA1A)) %>%
  group_by(urn) %>% arrange(desc(year_numeric), .by_group = TRUE) %>%
  summarise(published = mean(head(PTFSM6CLA1A, 3)) / 100,
            years = paste(head(year_label, 3), collapse = ", "), .groups = "drop")
dis_target <- schools %>% filter(city) %>% select(name, urn) %>%
  inner_join(pub_dis, by = "urn")
stopifnot(nrow(dis_target) == sum(schools$city))
#
# The published figures lag the admissions policy. Every cohort in them
# (Years 7 to 11 in 2022-23 to 2024-25) was admitted before September 2025:
# under the pre-2024 map, with no free school meals priority and no
# priority 6. Fitted on today's map, the pulls would absorb the policy
# change itself - Longhill's standing in for the Whitehawk children it no
# longer admits, Varndean's holding down the ones it now does. So the pulls
# are fitted on the map and rules those cohorts were admitted under, with
# the 2026 cohort's size, and then held fixed. The fit on today's map is
# kept for the comparison.
cal$zones <- zones
DIS_N <- dis_target$name
dis_share <- function(inp_c, r) {
  dd <- dis_flows(inp_c, r$flows)
  (tapply(dd, r$flows$name, sum)[DIS_N] / tapply(r$flows$flow, r$flows$name, sum)[DIS_N])
}
fit_dis <- function(r, target = dis_target$published) {
  lam <- setNames(rep(1, length(DIS_N)), DIS_N)
  k <- 1
  intake <- tapply(r$flows$flow, r$flows$name, sum)[DIS_N]
  for (it in 1:500) {
    cal$params$dis <- list(k = k, lambda = lam)
    sh <- dis_share(cal, r)
    city_mod <- sum(sh * intake) / sum(intake)
    city_pub <- sum(target * intake) / sum(intake)
    gap <- max(abs(sh - target))
    if (gap < 1e-4 && abs(city_mod - city_pub) < 1e-4) break
    k <- k * city_pub / city_mod
    lam <- lam * (target / sh)^0.8
    lam <- lam / exp(mean(log(lam)))
  }
  stopifnot(gap < 0.005)
  list(k = k, lambda = lam, share = sh, intake = intake, gap = gap, rounds = it)
}
DIS_PRE_RULES <- list(rule = "priorities", p6_share = 0, fsm = FALSE)
r_dis_pre <- run_sim(cal, year = 2026, design = "Pre-2024 catchments", rules = DIS_PRE_RULES)
r_dis <- run_sim(cal, year = 2026, rules = list(rule = "priorities"))
fit_pre <- fit_dis(r_dis_pre)
fit_now <- fit_dis(r_dis)

# Better than either: the first Year 7 intakes admitted under today's map
# and rules, with no lag. The council's allocation factsheet for September
# 2026 gives, for each community school, the children eligible for free
# school meals offered a place under any priority, and all the offers
# made, on national offer day - the basis the model runs on. The four
# academies and faith schools do not publish it and their arrangements did
# not change in 2025, so they keep their published whole-school shares.
# Free school meal eligibility is taken as the disadvantaged share; the
# Pupil Premium measure also counts children eligible at any point in six
# years, so if anything this understates the community schools' intakes.
# 2025 gives only the priority 4 and 5 offers, kept for the record.
YEAR7_FSM <- tibble::tribble(
  ~name, ~offers_2026, ~fsm_2026, ~fsm_p13_2026, ~fsm_p45_2026, ~offers_2025, ~fsm_p45_2025,
  "Blatchington Mill School", 330, 74, 15, 59, 330, 47,
  "Dorothy Stringer School",  330, 84, 18, 66, 330, 49,
  "Hove Park School",         136, 48, 11, 37, 171, 31,
  "Longhill High School",      81, 40, 12, 28,  97, 14,
  "Patcham High School",      204, 55, 20, 35, 202, 24,
  "Varndean School",          300, 90, 24, 66, 300, 88)
stopifnot(with(YEAR7_FSM, all(fsm_p13_2026 + fsm_p45_2026 == fsm_2026)),
          all(YEAR7_FSM$name %in% DIS_N))
y7_target <- dis_target$published
y7_target[match(YEAR7_FSM$name, DIS_N)] <- YEAR7_FSM$fsm_2026 / YEAR7_FSM$offers_2026
fit_y7 <- fit_dis(r_dis, y7_target)
dis_k <- fit_y7$k
dis_lam <- fit_y7$lambda
# What the pre-change pulls give a Year 7 intake under today's map and
# rules, for the comparison with the offers.
cal$params$dis <- list(k = fit_pre$k, lambda = fit_pre$lambda)
y7_now <- dis_share(cal, r_dis)
cal$params$dis <- NULL
# The measure the calibration replaces, kept for the record: each school's
# intake scored by the neighbourhoods its children come from.
nb_dep <- coalesce(idaci$dep3[match(r_dis$flows$lsoa, idaci$lsoa)], 0)
nb_share <- (tapply(r_dis$flows$flow * nb_dep, r_dis$flows$name, sum) /
               tapply(r_dis$flows$flow, r_dis$flows$name, sum))[DIS_N]
params$dis <- list(
  k = dis_k, lambda = dis_lam,
  fit = tibble(name = DIS_N, published = dis_target$published,
               year7_fsm = YEAR7_FSM$fsm_2026[match(DIS_N, YEAR7_FSM$name)] /
                 YEAR7_FSM$offers_2026[match(DIS_N, YEAR7_FSM$name)],
               target = y7_target, modelled = unname(fit_y7$share),
               neighbourhood = unname(nb_share), intake = unname(fit_now$intake),
               years = dis_target$years, lambda = unname(dis_lam),
               year7_pre = unname(y7_now),
               lambda_pre = unname(fit_pre$lambda),
               lambda_today_map = unname(fit_now$lambda)),
  k_pre = fit_pre$k, k_today_map = fit_now$k,
  year7_fsm = YEAR7_FSM,
  fitted_on = "Today's map and the council's priorities, 2026: community schools to their September 2026 Year 7 free school meal offers, academies and faith schools to their published whole-school shares",
  source = "Brighton & Hove City Council, Year 7 allocation factsheet, September 2026 (free school meal offers); DfE performance tables, PTFSM6CLA1A, mean of the three latest years")
message(sprintf("  disadvantaged pupils, fitted to the 2026 Year 7 offers: k %.3f, lambda %s; largest gap %.2f points",
                dis_k, paste(sprintf("%s %.2f", short_sch(DIS_N), dis_lam), collapse = ", "),
                100 * fit_y7$gap))
message(sprintf("  ... fitted before the policy change: lambda %s",
                paste(sprintf("%s %.2f", short_sch(DIS_N), fit_pre$lambda), collapse = ", ")))
message(sprintf("  ... on today's map instead: lambda %s",
                paste(sprintf("%s %.2f", short_sch(DIS_N), fit_now$lambda), collapse = ", ")))
message(sprintf("  ... Year 7 today with the pre-change pulls: %s",
                paste(sprintf("%s %.0f%% (published %.0f%%)", short_sch(DIS_N), 100 * y7_now,
                              100 * dis_target$published), collapse = ", ")))

# Children offered a place outside Brighton & Hove somewhere the model does
# not go - West Sussex, London, and anything else the four East Sussex
# schools do not account for. The adjudicator's Table 11 gives all
# out-of-city offers by home catchment (mean of three rounds); what M5
# places at the East Sussex schools in 2026 is taken off it. The app shows
# the remainder beside the model's flows; it does not simulate it.
adj_out <- bh_data("adjudicator.rds")$outside %>%
  group_by(catchment) %>% summarise(children = mean(children), .groups = "drop")
adj_out <- with(adj_out, setNames(children, catchment))
ext_mod <- tapply(OUTSIDE$by_catchment$children, OUTSIDE$by_catchment$catchment, sum)
ext_mod <- setNames(dplyr::coalesce(as.numeric(ext_mod[names(adj_out)]), 0), names(adj_out))
# Named vector first: pmax takes its names from the first argument.
outflow_other <- pmax(adj_out - ext_mod, 0)
stopifnot(identical(names(outflow_other), names(adj_out)))
stopifnot(length(outflow_other) == 6, all(is.finite(outflow_other)))
message(sprintf("  outside the city, 2026: modelled at East Sussex schools %s; elsewhere (published less modelled) %s",
                paste(sprintf("%s %.1f", names(ext_mod), ext_mod), collapse = ", "),
                paste(sprintf("%s %.1f", names(outflow_other), outflow_other), collapse = ", ")))

saveRDS(list(
  schools = schools, zones = zones, cost = cost, designs = DESIGNS,
  rules = RULES, outflow_other = outflow_other,
  comart = c(CM_SCEN, list(w_from_short = short_sch(CM_SCEN$w_from),
                           check = CMC$check, departure = CMC$departure)),
  attain = attain,
  params = params, demand = demand, cohort = cohort, finance = finance,
  seed_intakes = seed_intakes, idaci = idaci,
  design_geojson = design_geojson, presets = presets,
  city = CITY, out_of_city = OUT_OF_CITY,
  elm = elm, built_at = Sys.time()),
  file.path(APP_DIR, "data", "sim_inputs.rds"))

message(sprintf("\nSaved app/data/sim_inputs.rds (%.1f MB)",
                file.size(file.path(APP_DIR, "data", "sim_inputs.rds")) / 1e6))

# ---- 11. What priority 6 does to the social mix of intakes -----------
# Moving the priority-6 slider in the app turned up something the policy
# does not intend: under the council's priorities, a larger share of
# places for children from single-school catchments makes the city's
# intakes MORE segregated on Gorard's index, not less. The strategic view
# reports it (section 8), so the numbers are made here, from the same
# model the app runs, and saved for the document rather than typed into it.
source(file.path(APP_DIR, "R", "outcomes.R"))
inp_s <- readRDS(file.path(APP_DIR, "data", "sim_inputs.rds"))
P6_GRID <- c(0, 5, 10, 15, 20, 25, 30, 40)
CATCH8 <- setdiff(inp_s$city, inp_s$schools$name[inp_s$schools$faith])
gorard_8 <- function(mix) { k <- mix$name %in% CATCH8; gorard_index(mix$n[k], mix$dep_n[k]) }
run_p6 <- function(p, year = 2026, fsm = TRUE)
  run_sim(inp_s, year = year, rules = list(rule = "priorities", p6_share = p / 100, fsm = fsm))
mix_of <- function(r) {
  mix <- outcomes(inp_s, r)$mix %>% filter(name %in% inp_s$city)
  mix %>% mutate(contrib = 0.5 * abs(dep_n / sum(dep_n) - n / sum(n)))
}

p6_sweep <- tidyr::expand_grid(year = c(2026, 2030), fsm = c(TRUE, FALSE), p6 = P6_GRID) %>%
  purrr::pmap_dfr(function(year, fsm, p6) {
    r <- run_p6(p6, year, fsm); m <- outcomes(inp_s, r)
    tibble(year, fsm, p6, gorard = m$gorard, gorard_8 = gorard_8(m$mix),
           p6_places = sum(r$flows$p6), displaced = m$catchment$displaced)
  })
p6_published <- purrr::map_dfr(c(2026, 2030), function(y) {
  m <- outcomes(inp_s, run_sim(inp_s, year = y))
  tibble(year = y, gorard = m$gorard, gorard_8 = gorard_8(m$mix))
})

P6_AT <- 15
m0 <- mix_of(run_p6(0)); m1 <- mix_of(run_p6(P6_AT))
p6_schools <- m0 %>% select(name, n_0 = n, dep_0 = dep_share, contrib_0 = contrib) %>%
  inner_join(m1 %>% select(name, n_at = n, dep_at = dep_share, contrib_at = contrib), by = "name") %>%
  left_join(inp_s$schools %>% select(name, short, faith), by = "name")

r1 <- run_p6(P6_AT)
# dep3 is now the disadvantaged fraction of each flow (dis_flows).
fl1 <- r1$flows %>% mutate(dep3 = dis_flows(inp_s, r1$flows) / pmax(flow, 1e-12))
p6_winners <- fl1 %>% group_by(catchment) %>%
  summarise(children = sum(flow), dep_all = sum(flow * dep3) / sum(flow),
            # dep_p6 before p6: inside summarise, a column once summed
            # is the sum in every later expression.
            dep_p6 = sum(p6 * dep3) / pmax(sum(p6), 1e-9), p6 = sum(p6), .groups = "drop") %>%
  filter(p6 > 0.5)
p6_to <- fl1 %>% filter(p6 > 0) %>% group_by(catchment, name) %>%
  summarise(dep_p6 = sum(p6 * dep3) / sum(p6), p6 = sum(p6), .groups = "drop")

saveRDS(list(sweep = p6_sweep, published = p6_published, at = P6_AT,
             schools = p6_schools, winners = p6_winners, to = p6_to,
             city_dep = sum(m0$dep_n) / sum(m0$n), built_at = Sys.time()),
        file.path(DATA, "priority6_sweep.rds"))
s26 <- p6_sweep %>% filter(year == 2026, fsm)
message(sprintf("  priority 6 and Gorard, 2026: %s",
                paste(sprintf("%d%% %.3f", s26$p6, s26$gorard), collapse = ", ")))
message("Saved data/priority6_sweep.rds")

# ---- 12. CoMArt re-opened -------------------------------------------
# The scenario the strategic view reports in section 9: CoMArt open again
# on its site as a small school sharing Longhill's catchment, with
# Brighton Aldridge and Longhill both at 150, and whether that lets the
# central schools shrink. Run on the app's model and saved for the
# document, like the priority-6 sweep above.
BACA_N <- "Brighton Aldridge Community Academy"; LH_N <- "Longhill High School"
DS_N <- "Dorothy Stringer School"; V_N <- "Varndean School"
SMALL <- setNames(c(150, 150), c(BACA_N, LH_N))
CM1 <- list(pan = 150, w = 1)
CM_CONFIGS <- list(
  today    = list(label = "Today", pans = NULL, comart = NULL),
  shrink   = list(label = "Brighton Aldridge and Longhill at 150", pans = SMALL, comart = NULL),
  comart   = list(label = "CoMArt open at 150, Brighton Aldridge and Longhill at 150", pans = SMALL, comart = CM1),
  ds300    = list(label = "... and Dorothy Stringer at 300", pans = c(SMALL, setNames(300, DS_N)), comart = CM1),
  ds270    = list(label = "... and Dorothy Stringer at 270", pans = c(SMALL, setNames(270, DS_N)), comart = CM1),
  comart180 = list(label = "CoMArt open at 180, Brighton Aldridge and Longhill at 150", pans = SMALL, comart = list(pan = 180, w = 1)))

dep_min <- function(r) {
  f <- r$flows %>% mutate(dep3 = dis_flows(inp_s, r$flows) / pmax(flow, 1e-12)) %>%
    filter(name %in% r$schools$name[r$schools$city])
  c(dep = sum(f$flow * f$dep3 * f$cij) / sum(f$flow * f$dep3),
    rest = sum(f$flow * (1 - f$dep3) * f$cij) / sum(f$flow * (1 - f$dep3)))
}
cm_metrics <- function(r) {
  m <- outcomes(inp_s, r); s <- r$schools
  wanted <- tapply(r$flows$wanted, r$flows$name, sum)
  k8 <- m$mix$name %in% s$name[s$city & !s$faith]
  at <- function(nm, col) { v <- s[[col]][s$name == nm]; if (length(v)) v else NA_real_ }
  dm <- dep_min(r)
  tibble(gorard = m$gorard, gorard_catch = gorard_index(m$mix$n[k8], m$mix$dep_n[k8]),
         mean_min = m$mean_min, p90_min = m$p90_min, over_40 = m$over_40,
         dep_min = dm[["dep"]], rest_min = dm[["rest"]], dep_gap = m$dep_gap,
         places = m$pan, fill = m$fill, below_pan = m$below_pan, empty = m$empty,
         displaced = m$catchment$displaced, left_es = m$catchment$left_es,
         intake_lh = at(LH_N, "intake"), intake_baca = at(BACA_N, "intake"),
         intake_comart = at(CM_SCEN$name, "intake"),
         intake_ds = at(DS_N, "intake"), pan_ds = at(DS_N, "pan"),
         wanted_ds = unname(wanted[DS_N]),
         intake_v = at(V_N, "intake"), wanted_v = unname(wanted[V_N]))
}
cm_run <- function(cf, year, rule, w = NULL) {
  cm <- cf$comart; if (!is.null(cm) && !is.null(w)) cm$w <- w
  run_sim(inp_s, year = year, pans = cf$pans, comart = cm,
          rules = if (rule == "priorities") list(rule = "priorities") else NULL)
}
cm_runs <- tidyr::expand_grid(id = names(CM_CONFIGS), year = c(2026, 2030),
                              rule = c("priorities", "published")) %>%
  purrr::pmap_dfr(function(id, year, rule)
    bind_cols(tibble(id, label = CM_CONFIGS[[id]]$label, year, rule),
              cm_metrics(cm_run(CM_CONFIGS[[id]], year, rule))))

# Each school, 2026, council priorities: the configurations side by side.
cm_schools <- purrr::map_dfr(names(CM_CONFIGS), function(id) {
  r <- cm_run(CM_CONFIGS[[id]], 2026, "priorities"); m <- outcomes(inp_s, r)
  r$schools %>% filter(city) %>%
    left_join(m$mix %>% select(name, dep_share), by = "name") %>%
    transmute(id, name, short, pan, intake, fill, dep_share, mean_min)
})

# How much rests on the attractiveness CoMArt is given.
w_cm0 <- inp_s$schools$W[inp_s$schools$name == CM_SCEN$name]
W_city <- inp_s$schools$W[inp_s$schools$city]
CM_W <- c(`As Brighton Aldridge (default)` = 1,
          `As Longhill` = inp_s$schools$W[inp_s$schools$name == LH_N] / w_cm0,
          `The city median` = stats::median(W_city) / w_cm0)
cm_sens <- tidyr::expand_grid(variant = names(CM_W), year = c(2026, 2030)) %>%
  purrr::pmap_dfr(function(variant, year)
    bind_cols(tibble(variant, w = CM_W[[variant]], year),
              cm_metrics(cm_run(CM_CONFIGS$comart, year, "priorities", w = CM_W[[variant]]))))

saveRDS(list(runs = cm_runs, schools = cm_schools, sens = cm_sens,
             configs = purrr::map(CM_CONFIGS, ~ .x[c("label", "pans", "comart")]),
             w_from = CM_SCEN$w_from, routing_check = CMC$check,
             built_at = Sys.time()),
        file.path(DATA, "comart_scenarios.rds"))
cm26 <- cm_runs %>% filter(year == 2026, rule == "priorities")
message(sprintf("  CoMArt, 2026, council priorities: %s",
                paste(sprintf("%s: Gorard %.3f, mean %.1f min, CoMArt %s", cm26$id, cm26$gorard,
                              cm26$mean_min, ifelse(is.na(cm26$intake_comart), "-",
                                                   sprintf("%.0f", cm26$intake_comart))),
                      collapse = "; ")))
message("Saved data/comart_scenarios.rds")

# ---- 13. The council's options, scored on its priorities -------------
# Section 11 of the strategic view weighs the council's priorities against
# each other: keeping Longhill open, access and segregation for deprived
# children, and - a parent's priority - children kept at a school in their
# catchment and not displaced. Every option here uses only levers the
# council holds: the six community schools' admission numbers, the
# catchment map, Longhill's site and the admission priorities. The
# academies and faith schools keep their numbers except where an option
# says otherwise. Each is run under the council's priorities in 2026,
# 2030 and 2035 on the app's model, and scored on every priority at once.
N <- function(s) switch(s,
  LH = "Longhill High School", DS = "Dorothy Stringer School", V = "Varndean School",
  BMS = "Blatchington Mill School", HP = "Hove Park School", PAT = "Patcham High School",
  BACA = "Brighton Aldridge Community Academy", PACA = "Portslade Aldridge Community Academy",
  CN = "Cardinal Newman Catholic School", KINGS = "King's School")
P <- function(...) { x <- c(...); setNames(unname(x), vapply(names(x), N, "")) }
pan_city <- setNames(inp_s$schools$pan[inp_s$schools$city], inp_s$schools$name[inp_s$schools$city])
FIXED_OTHER <- sum(pan_city[!names(pan_city) %in% COMMUNITY])
shrink_comm <- scale_pans(pan_city[COMMUNITY], SHRINK_TOTAL - FIXED_OTHER)

CO_OPTIONS <- list(
  today        = list(group = "Baseline", label = "Today"),
  lh150        = list(group = "Keep Longhill open", label = "Longhill cut to 150", pans = P(LH = 150)),
  lh120        = list(group = "Keep Longhill open", label = "Longhill cut to 120", pans = P(LH = 120)),
  lh_wanted    = list(group = "Keep Longhill open", label = "Longhill at 150, and 1.5 times as attractive",
                      pans = P(LH = 150), w = P(LH = 1.5)),
  central      = list(group = "Keep Longhill open",
                      label = "Longhill 150; Stringer 300, Varndean 270, Blatchington Mill 300",
                      pans = P(LH = 150, DS = 300, V = 270, BMS = 300)),
  central_hard = list(group = "Keep Longhill open",
                      label = "Longhill 150; Stringer 270, Varndean 240, Blatchington Mill 270, Hove Park 150",
                      pans = P(LH = 150, DS = 270, V = 240, BMS = 270, HP = 150)),
  # Whitehawk's zones back in Longhill's catchment, so children moved out
  # of the central schools have a nearer catchment school to go to.
  whitehawk    = list(group = "Keep Longhill open", label = "Whitehawk back in Longhill's catchment",
                      design = "Current catchments, Whitehawk back to Longhill"),
  whitehawk_lh150 = list(group = "Keep Longhill open",
                      label = "Whitehawk back in Longhill's catchment; Longhill 150",
                      design = "Current catchments, Whitehawk back to Longhill", pans = P(LH = 150)),
  whitehawk_central = list(group = "Keep Longhill open",
                      label = "Whitehawk back to Longhill; Longhill 150; Stringer 300, Varndean 270, Blatchington Mill 300",
                      design = "Current catchments, Whitehawk back to Longhill",
                      pans = P(LH = 150, DS = 300, V = 270, BMS = 300)),
  pre2024      = list(group = "Keep Longhill open", label = "The pre-2024 catchments",
                      design = "Pre-2024 catchments"),
  elm          = list(group = "Keep Longhill open", label = "Longhill at Elm Grove at 150, catchments redrawn",
                      site = "elm", design = "Flow regions, Longhill at Elm Grove", pans = P(LH = 150)),
  shrink       = list(group = "Fit the system",
                      label = sprintf("Community schools cut in proportion, to %s places in all by 2035", fmt_n(SHRINK_TOTAL)),
                      pans = shrink_comm),
  power        = list(group = "Access and segregation", label = "Power diagram catchments", design = "Power diagram"),
  power_lh150  = list(group = "Access and segregation", label = "Power diagram catchments, Longhill 150",
                      design = "Power diagram", pans = P(LH = 150)),
  p6_off       = list(group = "Access and segregation", label = "No priority 6",
                      rules = list(rule = "priorities", p6_share = 0)),
  p6_20        = list(group = "Access and segregation", label = "Priority 6 at 20%",
                      rules = list(rule = "priorities", p6_share = 0.20)),
  equal        = list(group = "Access and segregation", label = "No oversubscription priorities: an equal chance",
                      rule = "published"),
  comart       = list(group = "Access and segregation",
                      label = "CoMArt open at 150; Brighton Aldridge and Longhill at 150",
                      pans = P(BACA = 150, LH = 150), comart = list(pan = 150, w = 1)),
  package_a    = list(group = "Packages",
                      label = "Longhill 150; Stringer 300, Varndean 270, Blatchington Mill 300; power diagram; no priority 6",
                      design = "Power diagram", pans = P(LH = 150, DS = 300, V = 270, BMS = 300),
                      rules = list(rule = "priorities", p6_share = 0)),
  package_b    = list(group = "Packages",
                      label = "Package A, with Longhill 1.5 times as attractive",
                      design = "Power diagram", pans = P(LH = 150, DS = 300, V = 270, BMS = 300),
                      w = P(LH = 1.5), rules = list(rule = "priorities", p6_share = 0)),
  # The schools the council does not run. It cannot set their numbers,
  # but it can ask, and the evidence for asking is what these show.
  shrink_all   = list(group = "Fit the system",
                      label = sprintf("Every school cut in proportion, to %s places in all by 2035", fmt_n(SHRINK_TOTAL)),
                      pans = scale_pans(pan_city, SHRINK_TOTAL)),
  cn300        = list(group = "Asks of schools the council does not run",
                      label = "Cardinal Newman cut to 300", pans = P(CN = 300)),
  cn270_kings  = list(group = "Asks of schools the council does not run",
                      label = "Cardinal Newman cut to 270, King's to 150", pans = P(CN = 270, KINGS = 150)),
  academies    = list(group = "Asks of schools the council does not run",
                      label = "Brighton Aldridge cut to 150, Portslade Aldridge to 180",
                      pans = P(BACA = 150, PACA = 180)),
  package_c    = list(group = "Packages",
                      label = "Package A, with Cardinal Newman at 300 and Brighton Aldridge at 150",
                      design = "Power diagram",
                      pans = P(LH = 150, DS = 300, V = 270, BMS = 300, CN = 300, BACA = 150),
                      rules = list(rule = "priorities", p6_share = 0)),
  package_d    = list(group = "Packages",
                      label = "Whitehawk back to Longhill; Longhill 150; Stringer 300, Varndean 270, Blatchington Mill 300; no priority 6",
                      design = "Current catchments, Whitehawk back to Longhill",
                      pans = P(LH = 150, DS = 300, V = 270, BMS = 300),
                      rules = list(rule = "priorities", p6_share = 0)),
  package_e    = list(group = "Packages",
                      label = "Package D, with Cardinal Newman at 300 and Brighton Aldridge at 150",
                      design = "Current catchments, Whitehawk back to Longhill",
                      pans = P(LH = 150, DS = 300, V = 270, BMS = 300, CN = 300, BACA = 150),
                      rules = list(rule = "priorities", p6_share = 0)),
  # Led by access: a smaller Longhill at the top of Elm Grove, with the
  # catchments redrawn round it, and the asks of the schools the council
  # does not run that section 11 makes anyway.
  package_f    = list(group = "Packages",
                      label = "Longhill at Elm Grove at 150, catchments redrawn; Cardinal Newman 300, King's 150, Brighton Aldridge 150",
                      site = "elm", design = "Flow regions, Longhill at Elm Grove",
                      pans = P(LH = 150, CN = 300, KINGS = 150, BACA = 150)),
  close_lh     = list(group = "Counterfactual", label = "Longhill closed",
                      closed = c(N("LH"), CM_SCEN$name)))

co_run <- function(o, year)
  run_sim(inp_s, year = year, pans = o$pans, w_mult = o$w,
          site = o$site %||% "now", design = o$design %||% "Current catchments",
          rules = if (identical(o$rule, "published")) NULL else (o$rules %||% list(rule = "priorities")),
          comart = o$comart, closed = o$closed)

co_metrics <- function(r) {
  m <- outcomes(inp_s, r); s <- r$schools; fin <- m$by_school
  at <- function(tbl, nm, col) { v <- tbl[[col]][tbl$name == nm]; if (length(v)) v[1] else NA_real_ }
  cf <- fin %>% filter(name %in% COMMUNITY)
  lhc <- m$catchment$outside %>% filter(home == "Longhill")
  comm <- s %>% filter(name %in% COMMUNITY)
  tibble(lh_intake = at(s, N("LH"), "intake"), lh_pan = at(s, N("LH"), "pan"),
         lh_fill = at(s, N("LH"), "fill"), lh_gap = at(fin, N("LH"), "gap"),
         lh_gap_pct = at(fin, N("LH"), "gap_pct"), lh_years = at(fin, N("LH"), "years_left"),
         comm_in_deficit = sum(cf$in_deficit), comm_gap = sum(cf$gap),
         comm_empty = sum(pmax(0, comm$pan - comm$intake)), comm_full = sum(comm$fill >= 0.995),
         city_gap = m$city_gap, empty = m$empty, below_pan = m$below_pan,
         gorard = m$gorard, dep_gap = m$dep_gap, dep_min = dep_min(r)[["dep"]],
         mean_min = m$mean_min, p90_min = m$p90_min, over_40 = m$over_40,
         displaced = m$catchment$displaced, in_catchment = 1 - m$catchment$outside_share,
         left_city = m$catchment$left_city,
         lh_catch_at_home = if (nrow(lhc)) 1 - lhc$outside_share else NA_real_)
}

co_runs <- tidyr::expand_grid(id = names(CO_OPTIONS), year = c(2026, 2030, 2035)) %>%
  purrr::pmap_dfr(function(id, year)
    bind_cols(tibble(id, group = CO_OPTIONS[[id]]$group, label = CO_OPTIONS[[id]]$label, year),
              co_metrics(co_run(CO_OPTIONS[[id]], year))))

# What moving Longhill does for Whitehawk. The 2024 change took six zones
# around CoMArt's old site out of Longhill's catchment; a map drawn round
# Longhill at Elm Grove puts most of them back, as part of a redesign
# rather than a reversal. Their journeys, and their disadvantaged
# children's, under today's map, the Whitehawk redraw and the move.
ELM_DESIGN <- "Flow regions, Longhill at Elm Grove"
wh_zones_co <- zones$zone[wh_catch != zones$catchment]
elm_zone <- DESIGNS[[ELM_DESIGN]]$zone
now_zone <- DESIGNS[["Current catchments"]]$zone
elm_moves <- tibble(
  n_zones = sum(elm_zone[zones$zone] != now_zone[zones$zone]),
  children = sum(zones$Oi[elm_zone[zones$zone] != now_zone[zones$zone]]),
  whitehawk_zones = length(wh_zones_co),
  whitehawk_to_longhill = sum(elm_zone[wh_zones_co] == "Longhill"))
whitehawk_elm <- purrr::map_dfr(c("today", "whitehawk", "elm", "package_f"), function(id)
  purrr::map_dfr(c(2026, 2030), function(y) {
    r <- co_run(CO_OPTIONS[[id]], y)
    fl <- r$flows
    dd <- dis_flows(inp_s, fl)
    k <- fl$zone %in% wh_zones_co
    tibble(id = id, year = y, children = sum(fl$flow[k]),
           to_longhill = sum(fl$flow[k & fl$name == N("LH")]),
           minutes = weighted.mean(fl$cij[k], fl$flow[k]),
           minutes_disadvantaged = weighted.mean(fl$cij[k], dd[k]))
  }))
stopifnot(elm_moves$whitehawk_to_longhill >= 1)

saveRDS(list(runs = co_runs, options = CO_OPTIONS, community = COMMUNITY,
             dis_fit = inp_s$params$dis$fit, dis_source = inp_s$params$dis$source,
             shrink_total = SHRINK_TOTAL, elm_moves = elm_moves,
             whitehawk_elm = whitehawk_elm, built_at = Sys.time()),
        file.path(DATA, "council_options.rds"))
message(sprintf("  Elm Grove map: %d zones (%.0f children) change catchment; %d of %d Whitehawk zones back in Longhill's",
                elm_moves$n_zones, elm_moves$children, elm_moves$whitehawk_to_longhill,
                elm_moves$whitehawk_zones))
co30 <- co_runs %>% filter(year == 2030)
message(sprintf("  council options, 2030: %s",
                paste(sprintf("%s: Longhill %s, Gorard %.3f, displaced %.0f", co30$id,
                              ifelse(is.na(co30$lh_intake), "-", sprintf("%.0f", co30$lh_intake)),
                              co30$gorard, co30$displaced), collapse = "; ")))
message("Saved data/council_options.rds")

# ---- 14. What the Whitehawk redraw does, and why ----------------------
# Strategic view, section 11: returning Whitehawk to Longhill's catchment
# moves segregation, and the direction depends on how disadvantage is
# measured. This is the evidence for the explanation there: who lives in
# the Whitehawk zones, where their children and their disadvantaged
# children go under each map, and what that does to each school's intake.
WH_DESIGN <- "Current catchments, Whitehawk back to Longhill"
wh_zones <- zones$zone[wh_catch != zones$catchment]
zone_dis <- pmin(inp_s$params$dis$k * zones$idaci_score, 0.95)
wh_area <- tibble(
  # n_zones, not zones: inside tibble() a column called zones would hide
  # the zones table from every column after it.
  n_zones = length(wh_zones),
  children = sum(zones$Oi[zones$zone %in% wh_zones]),
  disadvantaged_share = weighted.mean(zone_dis[zones$zone %in% wh_zones], zones$Oi[zones$zone %in% wh_zones]),
  city_share = weighted.mean(zone_dis, zones$Oi),
  dsv_share = weighted.mean(zone_dis[zones$catchment == "DS_Varndean" & !zones$zone %in% wh_zones],
                            zones$Oi[zones$catchment == "DS_Varndean" & !zones$zone %in% wh_zones]),
  longhill_share = weighted.mean(zone_dis[zones$catchment == "Longhill"], zones$Oi[zones$catchment == "Longhill"]))

wh_sch <- list(); wh_dest <- list(); wh_g <- list()
for (y in c(2026, 2030)) for (mp in c("today", "whitehawk")) {
  r <- run_sim(inp_s, year = y, design = if (mp == "today") "Current catchments" else WH_DESIGN,
               rules = list(rule = "priorities"))
  fl <- r$flows %>% mutate(dis = dis_flows(inp_s, r$flows), wh = zone %in% wh_zones)
  city_n <- r$schools$name[r$schools$city]
  wh_sch[[length(wh_sch) + 1]] <- fl %>% filter(name %in% city_n) %>%
    group_by(name) %>% summarise(intake = sum(flow), dis = sum(dis), .groups = "drop") %>%
    mutate(share = dis / intake,
           contrib = 0.5 * abs(dis / sum(dis) - intake / sum(intake)),
           year = y, map = mp)
  wh_dest[[length(wh_dest) + 1]] <- fl %>% filter(wh) %>%
    group_by(name) %>%
    summarise(children = sum(flow), dis = sum(dis), minutes = weighted.mean(cij, flow), .groups = "drop") %>%
    mutate(year = y, map = mp)
  m <- outcomes(inp_s, r)
  wh_g[[length(wh_g) + 1]] <- tibble(year = y, map = mp, gorard = m$gorard, dep_gap = m$dep_gap,
                                     displaced = m$catchment$displaced)
}
# How much the direction rests on the fitted pull on disadvantaged
# children: as fitted, at half its strength (lambda to the power 0.5), and
# with none (every lambda 1, so disadvantaged children spread as their
# neighbourhood's children do). Only the fitted version reproduces the
# published shares.
#
# And how much it rests on the measure: the first version's neighbourhood
# score (no calibration), the pulls fitted on today's map (which absorb the
# policy change), and the pulls fitted on the map and rules the published
# cohorts were admitted under (the model's measure).
dis_variant <- function(v) {
  inp_a <- inp_s
  if (v == "neighbourhood") inp_a$params$dis <- NULL
  if (v == "today_map") {
    inp_a$params$dis$lambda <- setNames(inp_s$params$dis$fit$lambda_today_map, inp_s$params$dis$fit$name)
    inp_a$params$dis$k <- inp_s$params$dis$k_today_map
  }
  if (v == "pre") {
    inp_a$params$dis$lambda <- setNames(inp_s$params$dis$fit$lambda_pre, inp_s$params$dis$fit$name)
    inp_a$params$dis$k <- inp_s$params$dis$k_pre
  }
  inp_a
}
wh_measures <- purrr::map_dfr(c(`By neighbourhood` = "neighbourhood",
                                `Whole-school shares, fitted on today's map` = "today_map",
                                `Whole-school shares, fitted before the policy change` = "pre",
                                `Year 7 offers, 2026 (the model's measure)` = "year7"), function(v) {
  inp_a <- dis_variant(v)
  purrr::map_dfr(c(today = "Current catchments", whitehawk = WH_DESIGN), function(des) {
    purrr::map_dfr(c(2026, 2030), function(y) {
      m <- outcomes(inp_a, run_sim(inp_a, year = y, design = des, rules = list(rule = "priorities")))
      tibble(year = y, gorard = m$gorard,
             longhill_share = m$mix$dep_share[m$mix$name == "Longhill High School"],
             varndean_share = m$mix$dep_share[m$mix$name == "Varndean School"])
    })
  }, .id = "map")
}, .id = "measure")
wh_sens <- purrr::map_dfr(c(`As fitted` = 1, `Half the pull` = 0.5, `No pull` = 0), function(a) {
  inp_a <- inp_s
  inp_a$params$dis$lambda <- inp_s$params$dis$lambda^a
  purrr::map_dfr(c(today = "Current catchments", whitehawk = WH_DESIGN), function(des) {
    r <- run_sim(inp_a, year = 2026, design = des, rules = list(rule = "priorities"))
    m <- outcomes(inp_a, r)
    tibble(gorard = m$gorard,
           longhill_share = m$mix$dep_share[m$mix$name == "Longhill High School"],
           varndean_share = m$mix$dep_share[m$mix$name == "Varndean School"])
  }, .id = "map")
}, .id = "variant")

short_of <- inp_s$schools %>% select(name, short)
saveRDS(list(area = wh_area, sens = wh_sens, measures = wh_measures,
             schools = bind_rows(wh_sch) %>% left_join(short_of, by = "name"),
             dest = bind_rows(wh_dest) %>% left_join(short_of, by = "name"),
             gorard = bind_rows(wh_g), zones = wh_zones,
             dis_fit = inp_s$params$dis$fit, built_at = Sys.time()),
        file.path(DATA, "whitehawk_explained.rds"))
whg <- bind_rows(wh_g)
message(sprintf("  Whitehawk: %d zones, %.0f children, %.0f%% disadvantaged (city %.0f%%); Gorard %s",
                wh_area$n_zones, wh_area$children, 100 * wh_area$disadvantaged_share, 100 * wh_area$city_share,
                paste(sprintf("%d %s %.3f", whg$year, whg$map, whg$gorard), collapse = ", ")))
message("Saved data/whitehawk_explained.rds")
