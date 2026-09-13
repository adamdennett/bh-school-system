# app/R/model.R — the spatial interaction model, run live
# ======================================================================
# This is the same model as section 7 of the strategic view, at its
# fullest rung (M5): attractiveness balanced to first preferences, a
# catchment term fitted for each catchment, competing destinations, the
# families in the paired catchments who would take only one of the two
# schools, and a capacity ceiling. It is reimplemented here
# rather than imported because the app has to run it on every slider
# move, with attractiveness and admission numbers that no precomputed
# run contains.
#
# It is checked against the published M5 figures in app/tests/check.R. If
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

#' The council's oversubscription priorities, as a tiered ceiling
#'
#' ipf_cap() cuts every applicant to a full school back by the same
#' proportion, which is the average outcome of one lottery over everyone
#' who applied. The council draws its lottery WITHIN priorities, so at the
#' six community schools the ceiling is filled tier by tier instead:
#'
#'   4  FSM children living in the catchment    } together up to 30% of
#'   5  FSM children living elsewhere           } the admission number
#'   6  children from a single-school catchment applying outside it, up
#'      to a share of the admission number (5% from 2026/27)
#'   7  children living in the catchment
#'   8  everyone else
#'
#' and proportionally inside each tier, which is what a lottery averages
#' to. A child who misses a capped tier drops to the one below: an FSM
#' child in the catchment to 7, an FSM child elsewhere to 6 if they
#' qualify for it and 8 if not, a priority-6 child to 8. The academies and
#' faith schools keep the proportional ceiling; their own criteria are not
#' in any published data.
#'
#' Priorities 1-3 - looked-after children, SEN and exceptional need, and
#' siblings - cannot be told apart in zone-level flows. Most siblings live
#' in the catchment, so they sit inside tier 7.
#'
#' The private allocation engine (BH_Pupil_Destinations,
#' R/20_admissions_allocation.R) confirms, on the pupil records, that the
#' tie-break inside each priority is random rather than by distance, which
#' is why the proportional cut inside a tier is the right mean-field.
#'
#' @param ff,fn flows of FSM-eligible children and of everyone else
#' @param in_c,s6,com logical per flow row: lives in the destination's
#'   catchment; lives in a single-school catchment and is applying outside
#'   it; the destination is a community school
tier_accept <- function(ff, fn, name, cap, pan, in_c, s6, com,
                        p6_share, fsm_cap_share) {
  S <- names(cap)
  by <- factor(name, levels = S)
  sumby <- function(x) { v <- tapply(x, by, sum); v[is.na(v)] <- 0; v[S] }
  ratio <- function(a, d) ifelse(d > 1e-12, a / d, 1)
  C <- cap[S]
  Fc <- fsm_cap_share * pan[S]
  Rc <- p6_share * pan[S]

  d4  <- sumby(ff * in_c)
  d5s <- sumby(ff * (!in_c & s6))
  d5o <- sumby(ff * (!in_c & !s6))
  a4 <- pmin(d4, Fc, C); r4 <- ratio(a4, d4); C <- C - a4
  a5 <- pmin(d5s + d5o, pmax(Fc - a4, 0), C); r5 <- ratio(a5, d5s + d5o); C <- C - a5
  d6 <- sumby(fn * s6) + d5s * (1 - r5)
  a6 <- pmin(d6, Rc, C); r6 <- ratio(a6, d6); C <- C - a6
  d7 <- sumby(fn * in_c) + d4 * (1 - r4)
  a7 <- pmin(d7, C); r7 <- ratio(a7, d7); C <- C - a7
  d8 <- sumby(fn * (!in_c & !s6)) + d5o * (1 - r5) + d6 * (1 - r6)
  a8 <- pmin(d8, C); r8 <- ratio(a8, d8)

  k <- as.character(name)
  af <- ifelse(in_c, r4[k] + (1 - r4[k]) * r7[k],
        ifelse(s6, r5[k] + (1 - r5[k]) * (r6[k] + (1 - r6[k]) * r8[k]),
                   r5[k] + (1 - r5[k]) * r8[k]))
  an <- ifelse(in_c, r7[k], ifelse(s6, r6[k] + (1 - r6[k]) * r8[k], r8[k]))

  # Everywhere else, the proportional ceiling.
  g <- pmin(cap[S] / sumby(ff + fn), 1)
  g[!is.finite(g)] <- 1
  af[!com] <- g[k][!com]
  an[!com] <- g[k][!com]

  list(f = unname(af), n = unname(an),
       p6f = unname(ifelse(com & s6, (1 - r5[k]) * r6[k], 0)),
       p6n = unname(ifelse(com & s6, r6[k], 0)),
       tiers = data.frame(name = S, p45_in = unname(a4), p45_out = unname(a5),
                          p6 = unname(a6), p7 = unname(a7), p8 = unname(a8)))
}

#' Balance two populations against the tiered ceiling
#'
#' FSM children and everyone else share preferences but not priorities,
#' and a child refused under one tier goes on to their other choices as a
#' member of the group they belong to. So each population places all of
#' its own children, and the ceiling is applied to both together.
ipf_priorities <- function(flow, zone, name, o_target, cap, pan, fsm_share,
                           in_c, s6, com, p6_share, fsm_cap_share,
                           max_iter = 500, tol = 1e-4) {
  ff <- flow * fsm_share[zone]
  fn <- flow - ff
  of <- o_target * fsm_share[names(o_target)]
  on <- o_target - of
  rebal <- function(x, target) {
    now <- tapply(x, zone, sum)
    f <- target[names(now)] / now
    f[!is.finite(f)] <- 1
    x * f[zone]
  }
  for (i in seq_len(max_iter)) {
    ff <- rebal(ff, of)
    fn <- rebal(fn, on)
    acc <- tier_accept(ff, fn, name, cap, pan, in_c, s6, com,
                       p6_share, fsm_cap_share)
    p6 <- ff * acc$p6f + fn * acc$p6n
    ff <- ff * acc$f
    fn <- fn * acc$n
    tot <- ff + fn
    d_chk <- tapply(tot, name, sum)
    o_chk <- tapply(tot, zone, sum)
    if (max(d_chk - cap[names(d_chk)]) <= tol &&
        max(abs(o_chk - o_target[names(o_chk)])) <= tol) break
  }
  list(flow = unname(tot), fsm = unname(ff), p6 = unname(p6),
       tiers = acc$tiers, iterations = i)
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
#' @param gamma NULL for the fitted catchment terms, or a multiplier on all
#'   of them together
#' @param exclusive NULL for the fitted shares of paired-catchment families
#'   who would take only one of the two schools, or a multiplier on them
#' @param rules NULL for the published model's proportional ceiling, or a
#'   list(rule = "priorities", p6_share, fsm, targeted) for the council's
#'   oversubscription priorities
run_sim <- function(inp, w_mult = NULL, pans = NULL, site = "now",
                    design = "Current catchments", year = 2026,
                    capped = TRUE, gamma = NULL, rules = NULL,
                    exclusive = NULL) {

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

  # In the two paired catchments some families would take only one of
  # the two schools. They are carried as populations of their own whose
  # choice sets leave the other school out, so a child refused at
  # Varndean who would not take Stringer looks elsewhere instead of
  # falling back on it. A trait of where they live, so it follows the
  # zone's own catchment whatever map is in force.
  ex_mult <- if (is.null(exclusive)) 1 else exclusive
  ex <- inp$params$exclusive
  pops <- if (is.null(ex) || !nrow(ex))
    data.frame(catchment = character(0), pop = character(0),
               pop_share = numeric(0), excluded = character(0))
  else dplyr::bind_rows(lapply(split(ex, ex$catchment), function(e) {
    s <- pmin(e$share * ex_mult, 0.49)
    data.frame(catchment = e$catchment[1],
               pop = c("either", paste("only", e$school)),
               pop_share = c(1 - sum(s), s),
               excluded = c(NA_character_, rev(e$school)),
               stringsAsFactors = FALSE)
  }))
  d <- d %>%
    dplyr::left_join(pops, by = "catchment", relationship = "many-to-many") %>%
    dplyr::mutate(pop = dplyr::coalesce(pop, "either"),
                  pop_share = dplyr::coalesce(pop_share, 1)) %>%
    dplyr::filter(pop_share > 0, is.na(excluded) | name != excluded) %>%
    dplyr::mutate(orig = paste(zone, pop, sep = "#"), Oi_pop = Oi * pop_share)
  o_pop <- tapply(d$Oi_pop, d$orig, function(v) v[1])

  # How much living in a catchment counts, catchment by catchment. Fitted
  # in the open model's M5 to the first preferences each catchment's
  # children actually give each school, and far from uniform: a
  # moderate pull in some catchments, a strong one in Stringer/Varndean.
  # Like the paired-school trait it follows the zone's own catchment; the
  # slider scales all of them together.
  g_mult <- if (is.null(gamma)) 1 else gamma
  g_fit <- inp$params$gamma
  g <- if (is.null(names(g_fit))) rep(g_fit, nrow(d)) else unname(g_fit[d$catchment])
  g[is.na(g)] <- 0

  util <- d$Wj * d$cij^(-inp$params$beta) *
    exp(g_mult * g * d$in_catch + inp$params$delta * log(d$Cj))

  # Production-constrained: every population in every neighbourhood places
  # its own children.
  A <- tapply(util, d$orig, sum)
  d$flow <- util * d$Oi_pop / A[d$orig]

  # Keep what the model wanted before the ceiling bit. The difference
  # between this and the capped flow is the whole of the displacement, and
  # without it "outside their catchment" cannot be split into children
  # who chose to leave and children who were pushed out.
  d$wanted <- d$flow

  # Which ceiling. With no rules the published model's, so everything
  # checked against the document is untouched unless asked for.
  rl <- inp$rules
  rule <- utils::modifyList(
    list(rule = "published",
         p6_share = if (is.null(rl)) 0.05 else rl$p6_share,
         fsm = TRUE, targeted = FALSE),
    if (is.null(rules)) list() else rules)
  stopifnot(rule$rule %in% c("published", "priorities"))

  d$p6 <- 0
  d$fsm_flow <- 0
  tiers <- NULL

  if (capped && rule$rule == "published")
    d$flow <- as.numeric(ipf_cap(d$flow, d$orig, d$name, o_pop, cap))

  if (capped && rule$rule == "priorities") {
    stopifnot(!is.null(rl), "fsm" %in% names(z))
    share <- if (isTRUE(rule$fsm))
      z$fsm * (if (isTRUE(rule$targeted)) rl$targeted_share else 1)
    else 0 * z$Oi

    # Priority 6 belongs to any catchment with one school in it, under
    # whichever map is in force - the four today, all of them under a
    # one-region-per-school design.
    grp <- dsg$school[sch$name[!sch$faith]]
    n_sch <- table(grp[!is.na(grp)])
    single <- names(n_sch)[n_sch == 1]
    in_c <- d$in_catch == 1
    s6 <- unname(dsg$zone[d$zone]) %in% single & !in_c
    com <- d$name %in% rl$community

    share_z <- setNames(share, z$zone)
    share_o <- setNames(unname(share_z[sub("#.*$", "", names(o_pop))]), names(o_pop))
    res <- ipf_priorities(d$flow, d$orig, d$name, o_pop,
                          cap, cap, share_o,
                          in_c, s6, com, rule$p6_share, rl$fsm_cap_share)
    d$flow <- res$flow
    d$fsm_flow <- res$fsm
    d$p6 <- res$p6
    tiers <- res$tiers[res$tiers$name %in% rl$community, ]
  }

  by_school <- d %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(intake = sum(flow), p6 = sum(p6), fsm = sum(fsm_flow),
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
       rule = rule$rule, rules = rule, tiers = tiers,
       year = year, site = site, design = design, index = idx,
       gamma = g_mult, exclusive = ex_mult,
       in_catch_share = sum(d$flow[d$in_catch == 1]) / sum(d$flow))
}
