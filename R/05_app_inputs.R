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
OUT_OF_CITY <- setdiff(oi$schools$name, CITY)

schools <- oi$schools %>%
  transmute(name, short = short_sch(name), urn = as.character(urn),
            easting, northing, faith,
            pan = pan2026,
            city = name %in% CITY) %>%
  left_join(oi$attract %>% select(name, W = W_wprefs), by = "name") %>%
  left_join(
    purrr::imap_dfr(oi$catchment_schools, ~ tibble(name = .x, group = .y)),
    by = "name")

stopifnot(!any(is.na(schools$W)), !any(is.na(schools$pan)))

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
  transmute(zone, lsoa, catchment, Oi)

cost <- list(
  now = oi$costs_now %>% filter(zone %in% zones$zone) %>%
    select(zone, name, cij, km),
  elm = oi$costs_elm %>% filter(zone %in% zones$zone) %>%
    select(zone, name, cij, km))

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

params <- list(beta = mt$beta, sigma = mt$sigma,
               gamma = mt$gamma_hat, delta = mt$delta_hat)
message(sprintf("  beta %.2f, gamma %.2f, delta %.2f, sigma %.0f",
                params$beta, params$gamma, params$delta, params$sigma))

# ---- 4b. What attractiveness means in Attainment 8 ------------------
# Section 5.4 finds that the published number families respond to is
# headline Attainment 8, and fits log(weighted preferences per place)
# against it. The app needs that fit the other way round: given a
# multiplier on attractiveness, how many Attainment 8 points is that?
#
# The response is logged, so the slope is a constant proportional effect
# per point and the inversion is just log(m) / b. Fitted here on the ten
# city schools, the same rows section 5.4 uses.

att_fit <- lm(log(W_wprefs) ~ att8,
              data = oi$attract %>% filter(name %in% CITY))
ATT_B <- unname(coef(att_fit)[["att8"]])

stopifnot(ATT_B > 0, summary(att_fit)$r.squared > 0.5)

attain <- list(
  slope = ATT_B,
  r2 = summary(att_fit)$r.squared,
  n = nobs(att_fit),
  se = unname(summary(att_fit)$coefficients["att8", "Std. Error"]),
  # A point of Attainment 8 is worth this much on the attractiveness
  # scale: about 7% more weighted preferences per place.
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

lsoa_sf <- bh_data("lsoa.geojson") %>%
  filter(lsoa21cd %in% zones$lsoa) %>%
  sf::st_transform(27700) %>%
  sf::st_simplify(dTolerance = 60) %>%
  sf::st_transform(4326) %>%
  select(lsoa = lsoa21cd)

design_sf <- fr$regions_sf %>%
  sf::st_transform(27700) %>% sf::st_simplify(dTolerance = 60) %>%
  sf::st_transform(4326)

message(sprintf("  %d neighbourhood polygons, %d design outlines",
                nrow(lsoa_sf), nrow(design_sf)))

# ---- 9. Presets ------------------------------------------------------
# Starting points, so the first thing a user sees is a question rather
# than a blank form. Each is a list of overrides the app applies on top
# of the baseline.

presets <- list(
  `Today` = list(
    note = "The city as it stands: today's admission numbers, today's catchments, Longhill at Ovingdean.",
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
    note = "Admission numbers cut roughly in proportion to the fall in the cohort, so the city is not carrying empty places.",
    design = "Current catchments", site = "now", year = 2035,
    pan = c(`Longhill High School` = 120,
            `Brighton Aldridge Community Academy` = 120,
            `Hove Park School` = 150,
            `Patcham High School` = 180,
            `Portslade Aldridge Community Academy` = 180),
    w = NULL),
  `Make Longhill wanted` = list(
    note = "Longhill's attractiveness lifted to Dorothy Stringer's, everything else unchanged. The question is what it takes, and who loses the children.",
    design = "Current catchments", site = "now", year = 2026,
    pan = NULL,
    w = c(`Longhill High School` =
            schools$W[schools$name == "Dorothy Stringer School"] /
            schools$W[schools$name == "Longhill High School"])))

saveRDS(list(
  schools = schools, zones = zones, cost = cost, designs = DESIGNS,
  attain = attain,
  params = params, demand = demand, finance = finance,
  seed_intakes = seed_intakes, idaci = idaci,
  lsoa_sf = lsoa_sf, design_sf = design_sf, presets = presets,
  city = CITY, out_of_city = OUT_OF_CITY,
  elm = elm, built_at = Sys.time()),
  file.path(APP_DIR, "data", "sim_inputs.rds"))

message(sprintf("\nSaved app/data/sim_inputs.rds (%.1f MB)",
                file.size(file.path(APP_DIR, "data", "sim_inputs.rds")) / 1e6))
