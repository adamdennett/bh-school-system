# app/R/model.R — the spatial interaction model, run live
# ======================================================================
# This is the same model as section 7 of the strategic view, at its
# fullest rung (M4): weighted preferences, a capacity ceiling, a
# catchment term and competing destinations. It is reimplemented here
# rather than imported because the app has to run it on every slider
# move, with attractiveness and admission numbers that no precomputed
# run contains.
#
# It is checked against the published M4 figures in app/R/check.R. If
# this file and the document ever disagree, that check fails.
# ======================================================================

#' Competing destinations, from whatever attractiveness is in force
#'
#' Fotheringham's term: a school with many attractive schools nearby
#' faces stiffer competition than its own distance decay implies. It has
#' to be recomputed rather than cached, because making one school more
#' attractive changes what its neighbours are up against - which is
#' exactly the kind of question this app exists to ask.
compete_now <- function(sch, W, sigma) {
  e <- sch$easting; n <- sch$northing; nm <- sch$name
  d <- sqrt(outer(e, e, "-")^2 + outer(n, n, "-")^2) / 1000
  d[d < 0.1] <- 0.1
  diag(d) <- NA_real_
  Cj <- colSums(matrix(W[nm], nrow(d), ncol(d)) * d^-sigma, na.rm = TRUE)
  setNames(Cj, nm)
}

#' Iterative proportional fitting with a one-sided ceiling
#'
#' Rows are scaled so every neighbourhood places all its children;
#' columns are scaled by min(capacity / demand, 1), so a school is cut
#' back to its admission number if it is over and left alone if it is
#' under. See section 7.5.2 of the document.
ipf_cap <- function(flow, orig, dest, o_target, cap,
                    max_iter = 200, tol = 1e-4) {
  for (i in seq_len(max_iter)) {
    o_now <- tapply(flow, orig, sum)
    f <- o_target[names(o_now)] / o_now
    f[!is.finite(f)] <- 1
    flow <- flow * f[orig]

    d_now <- tapply(flow, dest, sum)
    g <- pmin(cap[names(d_now)] / d_now, 1)
    g[!is.finite(g)] <- 1
    flow <- flow * g[dest]

    d_chk <- tapply(flow, dest, sum)
    o_chk <- tapply(flow, orig, sum)
    if (max(d_chk - cap[names(d_chk)]) <= tol &&
        max(abs(o_chk - o_target[names(o_chk)])) <= tol) break
  }
  attr(flow, "iterations") <- i
  flow
}

#' Run one configuration
#'
#' @param inp the bundle from R/05_app_inputs.R
#' @param w_mult named multipliers on baseline attractiveness (1 = as is)
#' @param pans named admission numbers (NULL = as published)
#' @param site "now" or "elm"
#' @param design a name from inp$designs
#' @param year an entry year; scales the cohort, not its geography
#' @param capped whether the admission numbers bind
run_sim <- function(inp, w_mult = NULL, pans = NULL, site = "now",
                    design = "Current catchments", year = 2026,
                    capped = TRUE, gamma = NULL) {

  # City schools only, which is what section 7 models. Peacehaven is in
  # the cost matrix and in the bundle, but the published model does not
  # let children leave the city and neither does this: including it as a
  # destination drains children from every Brighton school and the two
  # models stop agreeing. The limitation is real and is stated in the
  # app's notes rather than quietly fixed here.
  sch <- inp$schools[inp$schools$city, ]
  W <- setNames(sch$W, sch$name)
  if (!is.null(w_mult)) {
    stopifnot(all(names(w_mult) %in% names(W)))
    W[names(w_mult)] <- W[names(w_mult)] * w_mult
  }
  W <- pmax(W, 1e-6)

  cap <- setNames(as.numeric(sch$pan), sch$name)
  if (!is.null(pans)) {
    stopifnot(all(names(pans) %in% names(cap)))
    cap[names(pans)] <- pans
  }

  # The cohort shrinks; where it lives does not move. Section 3 projects
  # the city, not the neighbourhood.
  idx <- inp$demand$index[match(year, inp$demand$year)]
  stopifnot(!is.na(idx))
  z <- inp$zones
  z$Oi <- z$Oi * idx

  dsg <- inp$designs[[design]]
  stopifnot(!is.null(dsg))

  d <- inp$cost[[site]] %>%
    dplyr::filter(name %in% sch$name) %>%
    dplyr::inner_join(z, by = "zone") %>%
    dplyr::filter(Oi > 0, is.finite(cij), cij > 0)

  d$Wj <- W[d$name]
  d$Cj <- compete_now(sch, W, inp$params$sigma)[d$name]
  d$in_catch <- as.integer(
    !is.na(dsg$school[d$name]) &
      dsg$school[d$name] == dsg$zone[d$zone])

  # How much living in a catchment counts. The fitted value is a nudge,
  # not a rule - which is why changing the map moves so few children -
  # and the app lets it be turned up so that can be seen rather than
  # taken on trust.
  g <- if (is.null(gamma)) inp$params$gamma else gamma

  util <- d$Wj * d$cij^(-inp$params$beta) *
    exp(g * d$in_catch + inp$params$delta * log(d$Cj))

  # Production-constrained: every neighbourhood places its own children.
  A <- tapply(util, d$zone, sum)
  d$flow <- util * (z$Oi[match(d$zone, z$zone)] / A[d$zone])

  if (capped)
    d$flow <- as.numeric(ipf_cap(d$flow, d$zone, d$name,
                                 setNames(z$Oi, z$zone), cap))

  by_school <- d %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(intake = sum(flow),
                     mean_min = stats::weighted.mean(cij, flow),
                     mean_km = stats::weighted.mean(km, flow),
                     .groups = "drop") %>%
    dplyr::right_join(sch %>% dplyr::select(name, short, urn, faith, city),
                      by = "name") %>%
    dplyr::mutate(intake = dplyr::coalesce(intake, 0),
                  pan = unname(cap[name]),
                  fill = intake / pan,
                  at_pan = fill >= 0.995)

  list(flows = d, schools = by_school, W = W, cap = cap,
       year = year, site = site, design = design, index = idx, gamma = g,
       in_catch_share = sum(d$flow[d$in_catch == 1]) / sum(d$flow))
}
