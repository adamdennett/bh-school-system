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

  # ---- Who gets a place in their own catchment ------------------------
  # A catchment is a promise about where your child can go. This counts
  # how many children living in each catchment the model cannot place in
  # one of its schools, which is the policy question a boundary review
  # actually has to answer.
  #
  # The two faith schools have no catchment at all and admit across the
  # city, so children going to them are "outside" by construction rather
  # than by displacement. They are counted separately, because lumping
  # them in would make every catchment look far leakier than it is.
  dsg <- inp$designs[[r$design]]
  faith_names <- inp$schools$name[inp$schools$faith]

  flows <- r$flows
  if (!"p6" %in% names(flows)) flows$p6 <- 0
  if (!"orig" %in% names(flows)) flows$orig <- flows$zone

  f2 <- flows %>%
    dplyr::mutate(home = unname(dsg$zone[zone]),
                  dest_grp = unname(dsg$school[name]),
                  at_home = !is.na(dest_grp) & dest_grp == home,
                  where = dplyr::case_when(
                    name %in% faith_names ~ "A faith school",
                    at_home ~ "Their own catchment",
                    TRUE ~ "Another catchment"))

  catch_lab <- vapply(split(inp$schools$short[inp$schools$city],
                            dsg$school[inp$schools$name[inp$schools$city]]),
                      function(x) paste(sort(x), collapse = " / "), character(1))

  # TWO DIFFERENT PROCESSES, and conflating them was wrong. A child ends
  # up outside their catchment either because the model's own utility
  # sent them elsewhere while their catchment school still had room -
  # they chose to leave and got what they chose - or because the
  # capacity ceiling cut them from a catchment school that was full.
  # Only the second is a place the system failed to provide.
  #
  # The uncapped flow says what they wanted; the capped flow says what
  # they got. The shortfall between the two, at the home catchment, is
  # the displacement. Everything else outside is choice.
  # Per population, not per zone: a Varndean-only family's shortfall at
  # home must not be netted against a flexible family in the same
  # neighbourhood picking up the Stringer place they gave up.
  by_zone <- f2 %>%
    dplyr::group_by(orig, zone, home) %>%
    dplyr::summarise(living = sum(flow),
                     got_home = sum(flow[at_home]),
                     want_home = sum(wanted[at_home]),
                     to_faith = sum(flow[where == "A faith school"]),
                     via_p6 = sum(p6),
                     .groups = "drop") %>%
    dplyr::mutate(
      outside = pmax(0, living - got_home),
      # Capacity balancing can leave a zone with MORE home places than it
      # asked for, when its other choices were cut harder; the pmin keeps
      # displacement from going negative and the remainder is choice.
      displaced = pmin(outside, pmax(0, want_home - got_home)),
      # A child placed under priority 6 chose to leave and got the school
      # they chose, but through a rule that exists to let them. It is
      # counted apart from the choice the rules did not have to make room
      # for. Under the published model it is zero.
      through_p6 = pmin(outside - displaced, via_p6),
      chose = outside - displaced - through_p6)

  wide <- by_zone %>%
    dplyr::group_by(home) %>%
    dplyr::summarise(living = sum(living), `Their own catchment` = sum(got_home),
                     `Left by choice` = sum(chose),
                     `Through priority 6` = sum(through_p6),
                     `Displaced` = sum(displaced),
                     to_faith = sum(to_faith), .groups = "drop")

  # One row per catchment per destination bucket. This was a
  # tidyr::pivot_longer; stacking three named columns does not justify
  # shipping tidyr with the app.
  WHERE <- c("Their own catchment", "Left by choice", "Through priority 6",
             "Displaced")
  by_catch <- dplyr::bind_rows(lapply(WHERE, function(w)
    dplyr::mutate(wide[, setdiff(names(wide), WHERE), drop = FALSE],
                  where = w, n = wide[[w]]))) %>%
    dplyr::mutate(share = n / living,
                  label = dplyr::coalesce(unname(catch_lab[home]), home))

  outside <- by_catch %>%
    dplyr::filter(where != "Their own catchment") %>%
    dplyr::group_by(home, label, living) %>%
    dplyr::summarise(outside = sum(n), .groups = "drop") %>%
    dplyr::left_join(by_catch %>% dplyr::filter(where == "Displaced") %>%
                       dplyr::select(home, displaced = n), by = "home") %>%
    dplyr::mutate(outside_share = outside / living,
                  displaced_share = displaced / living)

  tot <- sum(by_zone$living)
  catchment <- list(
    by_catch = by_catch, outside = outside, by_zone = by_zone,
    outside_total = sum(outside$outside),
    outside_share = sum(outside$outside) / tot,
    displaced = sum(by_zone$displaced),
    displaced_share = sum(by_zone$displaced) / tot,
    chose_share = sum(by_zone$chose) / tot,
    p6 = sum(by_zone$through_p6),
    p6_share = sum(by_zone$through_p6) / tot,
    worst = outside$label[which.max(outside$outside_share)],
    worst_share = max(outside$outside_share),
    worst_displaced = outside$label[which.max(outside$displaced_share)],
    worst_displaced_share = max(outside$displaced_share),
    faith_share = sum(by_zone$to_faith) / tot)

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
    list(catchment = catchment, tiers = r$tiers, rule = r$rule),
    list(year = r$year, design = r$design, site = r$site))
}

#' Attainment 8 and attractiveness, in both directions
#'
#' Section 5.4 of the strategic view finds that headline Attainment 8 is
#' the published number families respond to, and fits
#' log(weighted preferences per place) against it. That fit is what lets
#' the app answer "how many more points?" instead of only "how many
#' times more attractive?".
#'
#' The relationship is an ASSOCIATION across ten schools, not a lever.
#' Attainment 8 is itself largely set by the intake, so a school cannot
#' simply decide to score higher - which is the point section 2 makes at
#' length, and the reason these numbers are framed as "what it would
#' take" rather than "what to do".

#' Attainment 8 points equivalent to a multiplier on attractiveness
att8_points <- function(inp, multiplier) log(multiplier) / inp$attain$slope

#' The multiplier equivalent to a change in Attainment 8 points
att8_multiplier <- function(inp, points) exp(inp$attain$slope * points)

#' The absence rate that goes with a target Attainment 8 score
#'
#' Inverts the Lever specification's elasticity on log(absence), holding
#' everything else about the school's intake still. Absence is not a
#' dial either - see the note in the app.
absence_for <- function(inp, absence_now, att8_now, att8_target)
  absence_now * (att8_target / att8_now)^(1 / inp$attain$absence$elasticity)

#' Where an absence rate sits nationally, as a percentile
absence_percentile <- function(inp, rate)
  100 * inp$attain$absence$national$ecdf(rate)

#' Where a score sits in the national distribution, as a percentile
att8_percentile <- function(inp, score) 100 * inp$attain$national$ecdf(score)

#' One line of context for a required score
att8_context <- function(inp, score) {
  city <- inp$attain$city
  pc <- att8_percentile(inp, score)
  top <- inp$schools$short[inp$schools$city][
    which.max(replace(inp$schools$att8[inp$schools$city], NA, -Inf))]
  where <- if (score > city$max)
    sprintf("above every school in the city — %s is the highest at %.1f",
            top, city$max)
  else if (score > city$median)
    sprintf("above the city median of %.1f", city$median)
  else sprintf("below the city median of %.1f", city$median)
  sprintf("%s, and in the top %.0f%% of the %s state schools in England",
          where, 100 - pc, fmt_n(inp$attain$national$n))
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
  # The capacity balancer converges to a tolerance, not to the last bit,
  # so a school that is full comes back at 0.99997 rather than 1. Testing
  # `fill < 1` therefore kept the bisection climbing past the answer, and
  # by an amount that depended on where it started: the same question
  # returned 4.2x from one starting slider and 5.1x from another. The
  # comparison needs the same tolerance the balancer has.
  hit <- function(m) fill_at(m) >= target_fill - 1e-3

  # Always start from 1, the school's published attractiveness. Starting
  # from the slider made the answer depend on the slider, which is the
  # one thing it must not do - the question is what the school needs, not
  # what it needs on top of where someone happened to leave the control.
  lo <- 1
  lo_f <- fill_at(lo)
  if (hit(lo))
    return(list(multiplier = lo, fill = lo_f, note = "already at or above"))
  if (!hit(hi))
    return(list(multiplier = Inf, fill = fill_at(hi),
                note = "cannot reach it at any attractiveness"))
  for (i in 1:40) {
    mid <- sqrt(lo * hi)           # geometric: the scale is multiplicative
    if (hit(mid)) hi <- mid else lo <- mid
    if (hi / lo < 1.001) break
  }
  list(multiplier = hi, fill = fill_at(hi), note = "solved")
}
