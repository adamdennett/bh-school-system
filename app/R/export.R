# app/R/export.R — one run, written out as a workbook
# ======================================================================
# Everything needed to reproduce and read a run: the scenario it started
# from, every setting as the scenario had it and as it was used, the
# fixed parameters, and what the run produced. Each setting is marked
# "changed" where the user moved it away from the scenario, so a
# workbook says on its face which numbers are the scenario's and which
# are someone's experiment.
# ======================================================================

#' The scenario's own values, before anyone touches a slider
scenario_defaults <- function(inp, preset) {
  s <- inp$presets[[preset]]
  city <- inp$schools[inp$schools$city, ]
  base <- stats::setNames(city$pan, city$name)
  if (!is.null(s$pan)) base[names(s$pan)] <- s$pan
  pans <- if (is.null(s$total)) base else scale_pans(base, s$total)
  w <- stats::setNames(rep(1, nrow(city)), city$name)
  if (!is.null(s$w)) {
    k <- intersect(names(s$w), names(w))
    w[k] <- round(s$w[k], 2)
  }
  list(s = s, pans = pans, w = w)
}

#' The workbook, as a named list of data frames
#'
#' @param settings list(design, site, year, gamma, exclusive, rule, p6,
#'   fsm, targeted) as used in the run
#' @param w_used,pans_used named per-school attractiveness multipliers and
#'   admission numbers as used in the run
scenario_export <- function(inp, r, m, preset, settings, w_used, pans_used,
                            generated = Sys.time()) {
  def <- scenario_defaults(inp, preset)
  s <- def$s
  val <- function(x) if (is.null(x) || length(x) == 0) NA else x
  chr <- function(x) if (is.logical(x)) ifelse(x, "yes", "no") else as.character(x)
  site_lab <- c(now = "Ovingdean (as now)", elm = "Elm Grove (relocated)")
  rule_lab <- c(published = "Everyone has the same chance (the published model)",
                priorities = "The council's priorities (2026/27 arrangements)")

  rows <- list(
    c("Catchment map", s$design, settings$design),
    c("Longhill's site", site_lab[[s$site]], site_lab[[settings$site]]),
    c("Entry year", s$year, settings$year),
    c("How much living in the catchment counts (x fitted)", s$gamma %||% 1, settings$gamma),
    c("Paired-catchment families who would take only one (x fitted)", s$exclusive %||% 1, settings$exclusive),
    c("When a school is full", rule_lab[[s$rule %||% "published"]], rule_lab[[settings$rule]]),
    c("Places for single-school catchments, priority 6 (%)", s$p6 %||% 5, settings$p6),
    c("Free school meals priority (4 and 5)", chr(s$fsm %||% TRUE), chr(settings$fsm)),
    c("Narrowed to Targeted FSM", chr(s$targeted %||% FALSE), chr(settings$targeted)),
    c("Total places in the city", sum(def$pans), sum(pans_used)))
  scen <- data.frame(
    Setting = vapply(rows, `[`, "", 1),
    `Scenario value` = vapply(rows, function(x) chr(x[2]), ""),
    `Used in this run` = vapply(rows, function(x) chr(x[3]), ""),
    check.names = FALSE, stringsAsFactors = FALSE)
  num_or <- function(x) suppressWarnings(as.numeric(x))
  same <- ifelse(!is.na(num_or(scen$`Scenario value`)) & !is.na(num_or(scen$`Used in this run`)),
                 abs(num_or(scen$`Scenario value`) - num_or(scen$`Used in this run`)) < 1e-9,
                 scen$`Scenario value` == scen$`Used in this run`)
  scen$Changed <- ifelse(same, "", "changed")

  readme <- data.frame(
    Item = c("Base scenario", "What the scenario is", "Settings changed from the scenario",
             "Schools changed from the scenario", "Generated", "Model", "Status"),
    Value = c(preset, val(s$note),
              sum(scen$Changed == "changed"),
              sum(abs(w_used[names(def$w)] - def$w) > 1e-9 |
                    abs(pans_used[names(def$pans)] - def$pans) > 1e-9),
              format(generated, "%Y-%m-%d %H:%M %Z"),
              "Brighton secondary schools policy simulator: M5, the calibrated spatial interaction model of the strategic view, section 7",
              "BETA. This simulator is currently in beta test mode - outputs have not been validated fully, so nothing should, at this point, be taken as reliable, however the simulator shows what could be possible to develop and outputs that are possible."),
    stringsAsFactors = FALSE)

  prm <- inp$params
  g <- prm$gamma
  fixed <- rbind(
    data.frame(Parameter = c("Distance decay (beta)", "Competing destinations (delta)",
                             "Competition distance exponent (sigma)"),
               Group = "", Fitted = c(prm$beta, prm$delta, prm$sigma),
               `Multiplier used` = NA_real_, `Value used` = c(prm$beta, prm$delta, prm$sigma),
               check.names = FALSE),
    data.frame(Parameter = "Catchment term (gamma)", Group = names(g), Fitted = unname(g),
               `Multiplier used` = settings$gamma, `Value used` = unname(g) * settings$gamma,
               check.names = FALSE),
    if (!is.null(prm$exclusive) && nrow(prm$exclusive))
      data.frame(Parameter = paste("Would take only", prm$exclusive$school),
                 Group = prm$exclusive$catchment, Fitted = prm$exclusive$share,
                 `Multiplier used` = settings$exclusive,
                 `Value used` = pmin(prm$exclusive$share * settings$exclusive, 0.49),
                 check.names = FALSE))

  wanted <- tapply(r$flows$wanted, r$flows$name, sum)
  city <- r$schools[r$schools$city, ]
  schools <- data.frame(
    School = city$name, Short = city$short, Faith = chr(city$faith),
    `Scenario admission number` = unname(def$pans[city$name]),
    `Admission number used` = unname(pans_used[city$name]),
    `Scenario attractiveness (x)` = unname(def$w[city$name]),
    `Attractiveness used (x)` = unname(w_used[city$name]),
    Changed = ifelse(abs(unname(pans_used[city$name]) - unname(def$pans[city$name])) > 1e-9 |
                       abs(unname(w_used[city$name]) - unname(def$w[city$name])) > 1e-9, "changed", ""),
    `Demand before the ceiling` = round(as.numeric(wanted[city$name]), 2),
    Intake = round(city$intake, 2),
    `Share of places filled` = round(city$fill, 4),
    `Places short of admission number` = round(pmax(0, city$pan - city$intake), 2),
    `Priority 6 places` = round(city$p6, 2),
    `FSM priority places` = round(city$fsm, 2),
    `Mean journey (min)` = round(city$mean_min, 2),
    `Mean journey (km)` = round(city$mean_km, 3),
    check.names = FALSE, stringsAsFactors = FALSE)

  bc <- m$catchment$by_catch
  catch <- data.frame(Catchment = bc$label, `Catchment id` = bc$home,
                      `Children living there` = round(bc$living, 2),
                      `Where they go` = bc$where, Children = round(bc$n, 2),
                      Share = round(bc$share, 4), check.names = FALSE)

  fl <- stats::aggregate(cbind(flow, wanted) ~ catchment + name, data = r$flows, FUN = sum)
  flows <- data.frame(`Home catchment (map in force today)` = fl$catchment, School = fl$name,
                      Children = round(fl$flow, 2), `Before the ceiling` = round(fl$wanted, 2),
                      check.names = FALSE)

  cm <- m$catchment
  out <- list(
    "Children placed in city schools" = m$intake,
    "Places" = m$pan,
    "Share of places filled" = m$fill,
    "Empty places" = m$empty,
    "Schools short of their admission number" = m$below_pan,
    "Mean journey (min)" = m$mean_min,
    "90th percentile journey (min)" = m$p90_min,
    "Share of journeys over 40 min" = m$over_40,
    "Child-km per school day, both ways" = m$child_km_day,
    "Deprived children's extra journey (min)" = m$dep_gap,
    "Segregation of intakes (Gorard index)" = m$gorard,
    "Lowest deprived share of an intake" = m$intake_lo,
    "Highest deprived share of an intake" = m$intake_hi,
    "City-wide annual balance, steady state (GBP)" = m$city_gap,
    "Schools in deficit" = m$in_deficit,
    "Schools with no reserve and a deficit" = m$critical,
    "Share of children outside their catchment" = cm$outside_share,
    "Share who left by choice" = cm$chose_share,
    "  of which to a faith school (share of cohort)" = cm$faith_choice_share,
    "  of which to another city school (share of cohort)" = cm$other_city_share,
    "  of which outside the city (share of cohort)" = cm$left_city_share,
    "Children outside the city" = cm$left_city,
    "Share placed through priority 6" = cm$p6_share,
    "Children displaced from their catchment" = cm$displaced,
    "Share displaced from their catchment" = cm$displaced_share)
  outc <- data.frame(Measure = names(out),
                     Value = vapply(out, function(x) as.numeric(val(x)), numeric(1)),
                     stringsAsFactors = FALSE)

  wb <- list(README = readme, Scenario = scen, Schools = schools,
             Catchments = catch, Flows = flows, Outcomes = outc,
             Money = as.data.frame(m$by_school), `Fixed parameters` = fixed)
  if (!is.null(r$tiers)) wb$Priorities <- as.data.frame(r$tiers)
  wb
}
