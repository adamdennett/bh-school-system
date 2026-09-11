# app/R/outcomes.R — scoring one configuration on every objective
# ======================================================================
# The point of the app is that these pull against each other. Evening
# out disadvantage moves children across the city and lengthens
# journeys; filling a school takes children from its neighbours and
# pushes THEM below their admission number. So every run is scored on
# all of them at once and the panel shows all of them at once.
#
#   Places      how full the city is, and how many schools are short
#   Money       income against the cost base, school by school
#   Fairness    Gorard's index over the modelled intakes
#   Travel      mean journey, and who makes the long ones
# ======================================================================

# The app does not source R/00_core.R - it has to run from app/ alone,
# so the handful of formatters it needs live here.
fmt_n <- function(x, d = 0)
  formatC(round(x, d), big.mark = ",", format = "f", digits = d)

gbp_app <- function(x, d = 1) {
  a <- abs(x); s <- ifelse(x < 0, "−", "")
  ifelse(a >= 1e6, sprintf("%s£%.*fm", s, d, a / 1e6),
  ifelse(a >= 1e3, sprintf("%s£%.0fk", s, a / 1e3),
                   sprintf("%s£%.0f", s, a)))
}

#' Gorard's segregation index over a set of intakes
#'
#' Half the sum of the absolute difference between each school's share
#' of the city's deprived children and its share of all of them. Zero is
#' a perfectly even spread.
gorard_index <- function(n, dep_n) {
  keep <- n > 0
  n <- n[keep]; dep_n <- dep_n[keep]
  if (sum(n) <= 0 || sum(dep_n) <= 0) return(NA_real_)
  0.5 * sum(abs(dep_n / sum(dep_n) - n / sum(n)))
}

#' Score a run on every objective
#'
#' @param inp the input bundle
#' @param r the result of run_sim()
#' @param shed the share of the funding a school can take out of its
#'   cost base when it loses pupils. Section 6.4's middle assumption.
outcomes <- function(inp, r, shed = 0.75) {

  city <- r$schools %>% dplyr::filter(city)

  # ---- Places --------------------------------------------------------
  places <- list(
    intake = sum(city$intake),
    pan = sum(city$pan),
    fill = sum(city$intake) / sum(city$pan),
    below_pan = sum(!city$at_pan),
    empty = sum(city$pan) - sum(city$intake),
    out_of_city = sum(r$schools$intake[!r$schools$city]))

  # ---- Travel --------------------------------------------------------
  # Weighted by children, and split by whether the child lives in one of
  # the neighbourhoods in the three most deprived deciles nationally.
  f <- r$flows %>%
    dplyr::left_join(inp$idaci %>% dplyr::select(lsoa, dep3), by = "lsoa") %>%
    dplyr::mutate(dep3 = dplyr::coalesce(dep3, 0))

  wq <- function(x, w, p) {
    o <- order(x); x <- x[o]; w <- w[o]
    x[which(cumsum(w) / sum(w) >= p)[1]]
  }
  travel <- list(
    mean_min = stats::weighted.mean(f$cij, f$flow),
    p90_min = wq(f$cij, f$flow, 0.9),
    over_40 = sum(f$flow[f$cij > 40]) / sum(f$flow),
    child_km_day = 2 * sum(f$flow * f$km),
    dep_gap = sum(f$flow * f$dep3 * f$cij) / sum(f$flow * f$dep3) -
              sum(f$flow * (1 - f$dep3) * f$cij) / sum(f$flow * (1 - f$dep3)))

  # ---- Fairness ------------------------------------------------------
  # Over the intakes the model produces, not over the catchments the
  # design draws. Section 8.3 is about why those differ.
  mix <- f %>%
    dplyr::mutate(dep_flow = flow * dep3) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(n = sum(flow), dep_n = sum(dep_flow), .groups = "drop") %>%
    dplyr::mutate(dep_share = dplyr::if_else(n > 0, dep_n / n, NA_real_))

  in_city <- mix$name %in% inp$city
  fairness <- list(
    gorard = gorard_index(mix$n[in_city], mix$dep_n[in_city]),
    intake_lo = suppressWarnings(min(mix$dep_share[in_city], na.rm = TRUE)),
    intake_hi = suppressWarnings(max(mix$dep_share[in_city], na.rm = TRUE)),
    mix = mix)

  # ---- Money ---------------------------------------------------------
  # A school's roll is five year groups. The app moves one of them, so
  # the steady-state roll is five times the intake - which is the roll
  # this configuration converges on if it is held for five years, not
  # the roll next September.
  fin <- inp$finance %>%
    dplyr::inner_join(city %>% dplyr::select(name, intake), by = "name") %>%
    dplyr::mutate(
      roll_ss = 5 * intake,
      d_roll = roll_ss - funded_roll,
      funding_change = d_roll * marginal_pp,
      # Losing pupils costs funding the school cannot fully shed; gaining
      # them brings funding it does not fully have to spend. The same
      # rate is applied in both directions, which is generous on the way
      # up and about right on the way down.
      gap = balance + funding_change * (1 - shed),
      gap_pct = gap / income,
      in_deficit = gap < 0,
      years_left = dplyr::case_when(
        reserve <= 0 ~ 0,
        gap >= 0 ~ Inf,
        TRUE ~ reserve / -gap))

  money <- list(
    by_school = fin,
    city_gap = sum(fin$gap),
    in_deficit = sum(fin$in_deficit),
    worst = fin$short[which.min(fin$gap_pct)],
    worst_pct = min(fin$gap_pct),
    # A school with no reserve and a deficit is the one a council has to
    # act on first, so it gets counted separately.
    critical = sum(fin$reserve <= 0 & fin$gap < 0))

  c(places, travel, fairness, money,
    list(year = r$year, design = r$design, site = r$site))
}

#' What would it take for one school to fill?
#'
#' The inverse question, by bisection on an attractiveness multiplier.
#' Returns Inf if the school cannot reach the target at any
#' attractiveness, which happens when its catchment simply has too few
#' children within reach.
#'
#' `w_mult` is a NAMED FORMAL rather than something passed through the
#' dots. It was in the dots, and the caller passed it, so run_sim saw it
#' twice and the whole thing errored - and had it not errored it would
#' have been worse, because the dots version reset every other school's
#' attractiveness to 1 and answered a question nobody asked. The other
#' schools stay where the user put them.
#'
#' The multiplier returned is on the school's PUBLISHED attractiveness,
#' not on whatever the slider currently reads.
solve_w_for_pan <- function(inp, school, target_fill = 1, hi = 60,
                            w_mult = NULL, ...) {
  # City schools only: run_sim refuses a multiplier for a school it does
  # not model, and Peacehaven is in inp$schools but not in the model.
  city <- inp$schools$name[inp$schools$city]
  base <- if (is.null(w_mult)) setNames(rep(1, length(city)), city) else w_mult
  stopifnot(school %in% names(base))

  fill_at <- function(m) {
    w <- base; w[school] <- m
    r <- run_sim(inp, w_mult = w, ...)
    r$schools$fill[r$schools$name == school]
  }
  lo_f <- fill_at(base[[school]])
  if (lo_f >= target_fill)
    return(list(multiplier = base[[school]], fill = lo_f,
                note = "already at or above"))
  hi_f <- fill_at(hi)
  if (hi_f < target_fill)
    return(list(multiplier = Inf, fill = hi_f,
                note = "cannot reach it at any attractiveness"))
  lo <- base[[school]]
  for (i in 1:28) {
    mid <- sqrt(lo * hi)           # geometric: the scale is multiplicative
    if (fill_at(mid) < target_fill) lo <- mid else hi <- mid
    if (hi / lo < 1.002) break
  }
  list(multiplier = hi, fill = fill_at(hi), note = "solved")
}
