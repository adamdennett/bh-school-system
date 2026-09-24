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

#' Where refused children go
#'
#' A proportional ceiling cuts every applicant to a full school back by
#' the same share and then scales each neighbourhood's flows back up to
#' place all its children - which spreads a refused child across every
#' other school in proportion to how much they wanted it first. That is
#' not what happens. A child refused at Varndean who would take Stringer
#' gets Stringer, and a child refused at Cardinal Newman goes where
#' families like them put their second preference.
#'
#' So refused demand is re-offered in two steps, only ever to schools with
#' room:
#'
#'   1  PARTNER FIRST. In the two paired catchments, a family who would
#'      take either school and is refused at one goes to the other.
#'   2  SECOND PREFERENCES. Everything else is shared among schools with
#'      room in proportion to the second preferences the family's home
#'      catchment gives each school (the council's catchment preference
#'      table), modulated by how near this neighbourhood is to each
#'      school compared with its catchment as a whole.
#'
#' Re-offered demand can overfill a school with little room; the next
#' round cuts it back and re-offers the excess. If no school has room at
#' all, the refused children stay unplaced rather than being forced over
#' an admission number.
#'
#' The kernel and the partner map are traits of where a family lives, so
#' they follow the neighbourhood's own catchment under any map.
#' When a school has moved, `ref` is the flow each row would have with
#' every school where it stands today. The kernel records the second
#' preferences families gave with the schools there, so each
#' neighbourhood's weight is its pull at this site against its
#' catchment's pull at today's: a catchment the move brings a school
#' nearer to sends it more of its overflow. With no move it is the same
#' flow, and nothing changes.
overflow_setup <- function(flow, orig, name, home, pop, kern, partner, ref = NULL) {
  oid <- match(orig, unique(orig))
  shares <- function(f) { v <- f / rowsum(f, oid)[oid, 1]; v[!is.finite(v)] <- 0; v }
  u <- shares(flow)
  hs <- paste(home, name)
  ubar <- stats::ave(if (is.null(ref)) u else shares(ref), hs, FUN = mean)
  k <- unname(kern[hs]); k[is.na(k)] <- 0
  bw <- k * u / pmax(ubar, 1e-12)
  bw[!is.finite(bw)] <- 0
  p_name <- unname(partner[hs])
  pr <- pop == "either" & !is.na(p_name)
  p_idx <- rep(NA_integer_, length(flow))
  p_idx[pr] <- match(paste(orig[pr], p_name[pr]), paste(orig, name))
  pr <- pr & !is.na(p_idx)
  list(oid = oid, u = u, bw = bw, pr = pr, p_idx = p_idx)
}

overflow_room <- function(held_rows, name, cap) {
  held <- tapply(held_rows, name, sum)
  unname((held < cap[names(held)] - 1e-6)[name])
}

overflow_add <- function(refused, room, st) {
  add <- numeric(length(refused))
  to_p <- st$pr
  to_p[st$pr] <- room[st$p_idx[st$pr]]
  if (any(to_p)) {
    pa <- rowsum(refused[to_p], st$p_idx[to_p])
    idx <- as.integer(rownames(pa))
    add[idx] <- add[idx] + pa[, 1]
  }
  rest <- refused
  rest[to_p] <- 0
  R <- rowsum(rest, st$oid)[, 1]
  w <- st$bw * room
  ws <- rowsum(w, st$oid)[st$oid, 1]
  w2 <- st$u * room
  ws2 <- rowsum(w2, st$oid)[st$oid, 1]
  share <- ifelse(ws > 0, w / ws, ifelse(ws2 > 0, w2 / ws2, 0))
  add + unname(R[st$oid]) * share
}

cascade_cap <- function(flow, orig, name, home, pop, kern, partner, cap,
                        max_iter = 500, tol = 1e-6, ref = NULL) {
  st <- overflow_setup(flow, orig, name, home, pop, kern, partner, ref)
  D <- flow
  for (i in seq_len(max_iter)) {
    load <- tapply(D, name, sum)
    g <- pmin(cap[names(load)] / load, 1)
    g[!is.finite(g)] <- 1
    H <- D * unname(g[name])
    refused <- D - H
    if (sum(refused) <= tol) { D <- H; break }
    room <- overflow_room(H, name, cap)
    if (!any(room)) { D <- H; break }
    D <- H + overflow_add(refused, room, st)
  }
  attr(D, "iterations") <- i
  D
}

partner_map <- function(ex) {
  out <- character(0)
  if (is.null(ex) || !nrow(ex)) return(out)
  for (h in unique(ex$catchment)) {
    s <- ex$school[ex$catchment == h]
    if (length(s) == 2) out <- c(out, stats::setNames(rev(s), paste(h, s)))
  }
  out
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
#' The council's published admission arrangements break ties inside each
#' priority by random allocation rather than by distance, which is why the
#' proportional cut inside a tier is the right mean-field.
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
ipf_priorities <- function(flow, orig, name, cap, pan, fsm_share,
                           in_c, s6, com, p6_share, fsm_cap_share,
                           home, pop, kern, partner,
                           max_iter = 500, tol = 1e-6, ref = NULL) {
  st <- overflow_setup(flow, orig, name, home, pop, kern, partner, ref)
  ff <- flow * unname(fsm_share[orig])
  fn <- flow - ff
  for (i in seq_len(max_iter)) {
    acc <- tier_accept(ff, fn, name, cap, pan, in_c, s6, com,
                       p6_share, fsm_cap_share)
    Hf <- ff * acc$f
    Hn <- fn * acc$n
    rf <- ff - Hf
    rn <- fn - Hn
    if (sum(rf + rn) <= tol) { ff <- Hf; fn <- Hn; break }
    # A refused child is re-offered as a member of the group they belong
    # to, so FSM children and everyone else overflow separately, into the
    # same schools with room.
    room <- overflow_room(Hf + Hn, name, cap)
    if (!any(room)) { ff <- Hf; fn <- Hn; break }
    ff <- Hf + overflow_add(rf, room, st)
    fn <- Hn + overflow_add(rn, room, st)
  }
  # The tiers of the intake that results, not of the last round's queue.
  acc <- tier_accept(ff, fn, name, cap, pan, in_c, s6, com,
                     p6_share, fsm_cap_share)
  list(flow = unname(ff + fn), fsm = unname(ff),
       p6 = unname(ff * acc$p6f + fn * acc$p6n),
       tiers = acc$tiers, iterations = i)
}

#' Run one configuration
#'
#' @param inp the bundle from R/05_app_inputs.R
#' @param w_mult named multipliers on baseline attractiveness (1 = as is)
#' @param pans named admission numbers (NULL = as published)
#' @param site "now" or "elm"
#' @param hp_site "valley" or "nevill" for Hove Park's Year 7 campus;
#'   NULL picks it from the entry year (Valley before 2028)
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
                    exclusive = NULL, comart = NULL, closed = NULL,
                    hp_site = NULL) {

  # The ten city schools, and the four East Sussex schools M5 fits as
  # destinations: children do leave the city, most of them from Longhill's
  # catchment for Priory School in Lewes. Those four have an attractiveness
  # each and a decay on straight-line km, no catchment and no competition
  # term, and they are not rationed. Nothing about them is adjustable.
  # Which schools are open. Any of them can be closed, and a school that
  # does not exist today - CoMArt - is closed unless a scenario or the user
  # opens it. A closed school is taken out of the city, not given no
  # places: a school with no places would still draw families, refuse them
  # all and push them elsewhere, which is not what a closure does.
  # `comart = list(pan, w)` is the shorter way the analysis opens CoMArt.
  cm <- inp$comart
  hyp <- inp$schools$name[inp$schools$hypothetical %in% TRUE]
  if (is.null(closed))
    closed <- setdiff(hyp, if (!is.null(comart) && !is.null(cm)) cm$name)
  known <- inp$schools$name[inp$schools$city | inp$schools$name %in% hyp]
  stopifnot(all(closed %in% known))
  sch <- inp$schools[inp$schools$name %in% setdiff(known, closed), ]
  sch$city <- TRUE
  stopifnot(nrow(sch) > 0)
  cm_on <- !is.null(cm) && cm$name %in% sch$name
  if (cm_on && !is.null(comart)) {
    k <- sch$name == cm$name
    if (!is.null(comart$pan)) sch$pan[k] <- comart$pan
    if (!is.null(comart$w)) sch$W[k] <- sch$W[k] * comart$w
  }
  outside <- inp$params$outside
  ext_names <- if (is.null(outside)) character(0) else names(outside$W)
  ext_sch <- inp$schools[inp$schools$name %in% ext_names, ]

  # CoMArt, the East Brighton school closed in 2005, when a scenario opens
  # it again: a small community school on its old site, sharing Longhill's
  # catchment in whatever map is in force. `comart` is list(pan, w) - its
  # places, and a multiplier on the attractiveness it starts from. It is
  # routed on the same network as every other school and competes with
  # them like any other; what it lacks is preferences, so its starting
  # attractiveness is borrowed (R/05_app_inputs.R says from where).
  # (Whether CoMArt is open, and at what, is settled above.)
  W <- setNames(sch$W, sch$name)
  if (!is.null(w_mult)) {
    # The app passes every school's slider, closed ones included.
    stopifnot(all(names(w_mult) %in% known))
    w_mult <- w_mult[names(w_mult) %in% names(W)]
    W[names(w_mult)] <- W[names(w_mult)] * w_mult
  }
  W <- pmax(W, 1e-6)

  cap <- setNames(as.numeric(sch$pan), sch$name)
  if (!is.null(pans)) {
    stopifnot(all(names(pans) %in% known))
    pans <- pans[names(pans) %in% names(cap)]
    cap[names(pans)] <- pans
  }
  cap_all <- c(cap, setNames(rep(1e6, length(ext_names)), ext_names))

  # The cohort shrinks; where it lives does not move. Section 3 projects
  # the city, not the neighbourhood.
  idx <- inp$demand$index[match(year, inp$demand$year)]
  stopifnot(!is.na(idx))
  z <- inp$zones
  z$Oi <- z$Oi * idx

  dsg <- inp$designs[[design]]
  stopifnot(!is.null(dsg))
  # CoMArt joins Longhill's catchment, so that catchment has two schools.
  if (cm_on && is.na(dsg$school[cm$name]))
    dsg$school[cm$name] <- unname(dsg$school[cm$joins])

  # ---- Which Hove Park? ------------------------------------------------
  # Hove Park teaches Year 7 at the Valley Campus on Hangleton Way, 1.65 km
  # west of the Nevill Road address the school register gives. The council
  # gave statutory notice on 8 June 2026 to close the Valley Campus on
  # 31 August 2028, so entry years up to 2027 belong at Hangleton Way and
  # 2028 onwards at Nevill Road. That is the default; `hp_site` overrides
  # it, which is how the scenario holds one site across every year.
  hpv <- inp$hove_park_valley
  if (is.null(hp_site)) hp_site <- if (!is.null(hpv) && year < hpv$consolidates)
    "valley" else "nevill"
  stopifnot(hp_site %in% c("valley", "nevill"))
  cost_key <- if (hp_site == "valley")
    c(now = "valley", elm = "valley_elm")[[site]] else site
  if (is.null(inp$cost[[cost_key]])) {
    # An older input bundle has only the two Nevill tables. Fall back
    # rather than fail, but say so: every Hove Park result is then for
    # the wrong campus.
    warning("No '", cost_key, "' cost table in this input bundle; ",
            "Hove Park stays at Nevill Road.")
    cost_key <- site
    hp_site <- "nevill"
  }

  d <- inp$cost[[cost_key]] %>%
    dplyr::filter(name %in% c(sch$name, ext_names)) %>%
    dplyr::inner_join(z, by = "zone") %>%
    dplyr::filter(Oi > 0, is.finite(cij), cij > 0)

  d$Wj <- W[d$name]
  # Competition is measured from where the schools stand in this run: with
  # Longhill at Elm Grove, its distances to its rivals are from Elm Grove.
  sch_xy <- sch
  if (site == "elm" && all(c("elm_easting", "elm_northing") %in% names(sch))) {
    sch_xy$easting <- sch$elm_easting
    sch_xy$northing <- sch$elm_northing
  }
  # Hove Park's rivals are measured from the campus Year 7 attends too.
  if (hp_site == "valley" && !is.null(hpv)) {
    k <- sch_xy$name == hpv$school
    sch_xy$easting[k] <- hpv$easting
    sch_xy$northing[k] <- hpv$northing
  }
  d$Cj <- compete_now(sch_xy, W, inp$params$sigma)[d$name]
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
  is_ext <- d$name %in% ext_names
  if (any(is_ext))
    util[is_ext] <- unname(outside$W[d$name[is_ext]]) * d$km[is_ext]^(-outside$decay)

  # Production-constrained: every population in every neighbourhood places
  # its own children.
  A <- tapply(util, d$orig, sum)
  d$flow <- util * d$Oi_pop / A[d$orig]

  # The same demand with every school where it stands today: what the
  # second preferences behind the overflow were given against. Only a
  # moved school needs it; the map in force is kept, so it isolates the
  # move.
  u_ref <- NULL
  if (site != "now") {
    # The reference keeps Hove Park where this run has it, so the
    # comparison isolates Longhill's move and nothing else.
    ref <- inp$cost[[if (hp_site == "valley") "valley" else "now"]]
    cij_ref <- ref$cij[match(paste(d$zone, d$name), paste(ref$zone, ref$name))]
    cij_ref[is.na(cij_ref)] <- d$cij[is.na(cij_ref)]
    Cj_ref <- compete_now(sch, W, inp$params$sigma)[d$name]
    util_ref <- d$Wj * cij_ref^(-inp$params$beta) *
      exp(g_mult * g * d$in_catch + inp$params$delta * log(Cj_ref))
    if (any(is_ext)) util_ref[is_ext] <- util[is_ext]
    u_ref <- util_ref * d$Oi_pop / tapply(util_ref, d$orig, sum)[d$orig]
  }

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

  # Where refused children go: partner school first in the paired
  # catchments, then the home catchment's second preferences. Older input
  # bundles without the kernel fall back to the proportional ceiling.
  kern <- inp$params$overflow
  partner <- partner_map(ex)

  if (capped && rule$rule == "published")
    d$flow <- if (is.null(kern))
      as.numeric(ipf_cap(d$flow, d$orig, d$name, o_pop, cap_all))
    else as.numeric(cascade_cap(d$flow, d$orig, d$name, d$catchment, d$pop,
                                kern, partner, cap_all, ref = u_ref))

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
    com <- d$name %in% c(rl$community, if (cm_on) cm$name)

    share_z <- setNames(share, z$zone)
    share_o <- setNames(unname(share_z[sub("#.*$", "", names(o_pop))]), names(o_pop))
    stopifnot(!is.null(kern))
    res <- ipf_priorities(d$flow, d$orig, d$name, cap_all, cap_all, share_o,
                          in_c, s6, com, rule$p6_share, rl$fsm_cap_share,
                          d$catchment, d$pop, kern, partner, ref = u_ref)
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
    dplyr::right_join(rbind(sch, ext_sch) %>% dplyr::select(name, short, urn, faith, city),
                      by = "name") %>%
    dplyr::mutate(intake = dplyr::coalesce(intake, 0),
                  pan = unname(cap[name]),
                  fill = intake / pan,
                  at_pan = fill >= 0.995)

  list(flows = d, schools = by_school, W = W, cap = cap,
       design_map = dsg, comart = cm_on,
       rule = rule$rule, rules = rule, tiers = tiers,
       year = year, site = site, hp_site = hp_site, design = design, index = idx,
       gamma = g_mult, exclusive = ex_mult,
       in_catch_share = sum(d$flow[d$in_catch == 1]) / sum(d$flow))
}