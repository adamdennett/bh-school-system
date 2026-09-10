# R/03_flow_regions.R — catchments built from the modelled flows
# ======================================================================
# The council's catchments were drawn round schools. These are drawn
# round the flows: functional regions, in the sense used for travel-to-
# work areas, derived from where the model sends children rather than
# from where the buildings are.
#
# WHICH FLOWS, AND WHY IT MATTERS
#
# The obvious input is the full model (M4). It would be the wrong one.
# M4 contains a catchment term, so its flows already know the current
# boundaries, and regionalising them would partly rediscover the map
# this is meant to be an alternative to. The input is M2 instead:
# weighted preferences and a capacity ceiling, no catchment term at all.
# That is the closest published data comes to "where would children go
# if the rule did not exist but the places still ran out".
#
# THE METHOD, which is deliberately a standard one
#
#   1. Dominant flow. Every LSOA joins the school it sends most children
#      to. This is the Nystuen-Dacey construction and it is the oldest
#      functional-region method there is.
#   2. Contiguity repair. A region has to be one piece. Fragments not
#      touching their region's main body move to the adjacent region
#      they send most flow to.
#   3. Capacity balance. Regions are then traded LSOA by LSOA until each
#      holds roughly as many children as its schools have places, always
#      moving the boundary LSOA that costs the least flow.
#   4. Self-containment, the standard travel-to-work-area statistic:
#      what share of a region's children have their modelled first
#      choice inside it.
#
# FIVE DESIGNS ARE SCORED against each other:
#
#   Current catchments      what is in force
#   Power diagram           the other method already in this work, from
#                           the open bundle: lowest cost minus a price,
#                           iterated to capacity. A proximity design.
#   Flow regions, single    one region per school
#   Flow regions, paired    the council's pairings kept, because
#                           Stringer and Varndean are 470 metres apart
#                           and a boundary between them means little
#   Flow regions, Elm Grove the same, on a re-run of the model with
#                           Longhill relocated and its PAN reduced
#
# Faith schools are left out of the geography, as they are now. They
# admit across the city and giving them a catchment would be a change of
# policy rather than a change of map.
#
# Every design is then profiled by IDACI decile using the method in
# BH_Schools_Consultation/postcode_school_pop.qmd: postcodes inside a
# catchment polygon, households with dependent children counted by
# decile. Same method, so the comparison is with the consultation's own
# figures rather than with a different statistic about deprivation.
#
# Output: data/flow_regions.rds
# ======================================================================

source(here::here("R", "00_core.R"))

message("\n=== Catchments from the modelled flows ===")

oi  <- bh_data("open_inputs.rds")
mt  <- bh_data("model_terms.rds")
dep <- bh_data("deprivation_open.rds")

FLOW_MODEL <- "M2"
BETA       <- mt$beta

# ---- Children, at whole-LSOA level ----------------------------------
# 30 of the 179 LSOAs are split across two current catchments, which is
# an artefact of the boundaries being replaced. Aggregating to whole
# LSOAs removes it and matches the unit the new design is built from.

city_zones <- oi$zones %>% filter(area != "Expansion area")

lsoa_children <- city_zones %>%
  group_by(lsoa) %>%
  summarise(Oi = sum(Oi), .groups = "drop")

# ---- Deprivation at LSOA level, on the consultation's own definition -
# The postcode file carries an IDACI decile and a count of households
# with dependent children for every postcode in the city, and an LSOA
# code with them. Aggregating it here gives each neighbourhood a share
# of its households with children that sit in the three most deprived
# deciles nationally - the same "deprived" as the catchment profiles
# further down, and the same as the open model's segregation figures.
#
# It is used for two things the polygon profiles cannot do: the
# deprivation mix of each SCHOOL's modelled intake, which is what the
# over-subscription rule actually determines, and splitting journey
# times by whether the child making them is from a deprived
# neighbourhood.
# IDACI is published at LSOA level, so every postcode in a neighbourhood
# carries the same decile and dep3 comes out as 0 or 1 rather than as a
# share: 39 of the 165 neighbourhoods are in the three most deprived
# deciles in England. It is aggregated from the postcode file anyway,
# because that file is the consultation's own, and then checked against
# the open model's flag so the two definitions cannot drift apart.
idaci_lsoa <- readr::read_csv(file.path(DATA, "postcode_children.csv"),
                              show_col_types = FALSE) %>%
  filter(!is.na(idaci_decile), hh_with_ch > 0) %>%
  group_by(lsoa) %>%
  summarise(hh = sum(hh_with_ch),
            dep3 = sum(hh_with_ch[idaci_decile <= 3]) / sum(hh_with_ch),
            .groups = "drop")

local({
  chk <- inner_join(idaci_lsoa, dep$idaci %>% select(lsoa, deprived),
                    by = "lsoa")
  stopifnot(all(chk$dep3 %in% c(0, 1)),
            all((chk$dep3 > 0.5) == chk$deprived))
})

FAITH   <- oi$schools$name[oi$schools$faith]
CATCH_S <- setdiff(oi$schools$name[oi$schools$name %in% unique(mt$od_flows$name)],
                   FAITH)
pan     <- setNames(oi$schools$pan2026, oi$schools$name)

# ---- The two worlds --------------------------------------------------
# Everything below runs twice: once on the city as it is, and once with
# Longhill at the top of Elm Grove on a reduced admission number. The
# second is the configuration section 8 finds composes best, and the
# question it answers is whether the catchments that suit a relocated
# school are the same ones that suit it where it stands.

world <- function(model_id, costs, label, lh_pan = NULL) {
  fl <- mt$od_flows %>%
    filter(model == model_id) %>%
    inner_join(city_zones %>% select(zone, lsoa), by = "zone") %>%
    group_by(lsoa, name) %>%
    summarise(flow = sum(flow), .groups = "drop")

  ct <- costs %>%
    inner_join(city_zones %>% select(zone, lsoa, Oi), by = "zone") %>%
    group_by(lsoa, name) %>%
    # Both cost columns are carried through. cij is the routed
    # walk-and-bus time in minutes, which is what the model runs on; km
    # is the network distance, which is what a transport budget and a
    # carbon figure are counted in. Section 8 wants both.
    summarise(cij = weighted.mean(cij, Oi),
              km  = weighted.mean(km, Oi), .groups = "drop")

  p <- pan
  if (!is.null(lh_pan)) p["Longhill High School"] <- lh_pan

  list(label = label, flows = fl, cost = ct, pan = p,
       # Capacity has to be measured against the children a catchment
       # could actually be asked to place, and that is not all of them.
       # The two faith schools admit across the city with no catchment
       # and take about a fifth of the cohort, so an LSOA's demand on
       # the geography is the flow it sends to schools that HAVE one.
       demand = fl %>%
         filter(name %in% CATCH_S) %>%
         group_by(lsoa) %>%
         summarise(demand = sum(flow), .groups = "drop") %>%
         right_join(lsoa_children, by = "lsoa") %>%
         mutate(demand = coalesce(demand, 0)))
}

W_NOW <- world("M2", oi$costs_now, "Ovingdean, PAN 210")

# Two relocation scenarios. The modelled flows are IDENTICAL between
# them - Longhill draws 137 children at Elm Grove and the ceiling does
# not bind at either number - so the only thing that differs is the
# capacity target the catchment is balanced to, and therefore how much
# territory the school is given. That is the whole comparison: not what
# the school would attract, but how much of the city its boundary is
# asked to cover.
W_ELM    <- world("ELM150", oi$costs_elm, "Elm Grove, PAN 150", lh_pan = 150)
W_ELM210 <- world("ELM210", oi$costs_elm, "Elm Grove, PAN 210", lh_pan = 210)

# THE RELOCATION DESIGNS ARE SEEDED ON ACCESSIBILITY, NOT ON FLOW, and
# the reason is a limit of the flow method rather than a preference.
#
# Dominant flow gives each neighbourhood to the school it sends most
# children to. For a school that has just moved there is no such school:
# the modelled flows at the new site are shaped by attractiveness, and
# Longhill's is 0.32 against a city average of 1. Even uncapped it wins
# the dominant flow almost nowhere, so a flow seed hands the east to
# Stringer/Varndean and the greedy balancer, which can only trade
# LSOAs across an existing boundary, never reaches far enough east to
# take them back. Two of the twelve easternmost neighbourhoods ended up
# in Longhill's catchment; the other ten went to a school an hour away
# that they are not nearest to.
#
# A catchment is a statement about geography and capacity. It is not a
# popularity contest, and a school does not forfeit a catchment for
# being unpopular - that is what the over-subscription rule is for.
# So the relocation designs seed on "which catchment can this
# neighbourhood reach quickest", and the capacity balance then trades
# from there. The Ovingdean designs keep the flow seed, because there
# the flows describe a school that is actually where it is.
W_ELM$seed_mode    <- "accessibility"
W_ELM210$seed_mode <- "accessibility"

# Distance carries more weight in these two than in the others, because
# the relocation is what puts the eastern edge of the city at risk of
# being handed to a school an hour away. Neighbourhoods are given up in
# order of the journey they would then make, not of how much that
# journey changes, and one already sitting with its nearest catchment is
# never pushed more than MOVE_TOL minutes beyond it.
PROTECT_MIN <- 35
W_ELM$move_cost    <- W_ELM210$move_cost <- "absolute"
W_ELM$protect_min  <- W_ELM210$protect_min <- PROTECT_MIN

message(sprintf("  %d LSOAs, %s children; %.0f%% have a school with a catchment as their destination",
                nrow(lsoa_children),
                format(round(sum(lsoa_children$Oi)), big.mark = ","),
                100 * sum(W_NOW$demand$demand) / sum(W_NOW$demand$Oi)))

# ---- Geometry and adjacency -----------------------------------------

geom <- bh_data("lsoa.geojson") %>%
  filter(lsoa21cd %in% lsoa_children$lsoa) %>%
  st_transform(27700)
stopifnot(nrow(geom) == nrow(lsoa_children))

# st_touches gives shared-boundary adjacency. A couple of LSOAs meet
# only across water or at a point; st_is_within_distance picks those up
# so the graph is connected, which the repair step needs.
nb <- st_touches(geom)
iso <- which(lengths(nb) == 0)
if (length(iso)) {
  near <- st_is_within_distance(geom[iso, ], geom, dist = 250)
  for (k in seq_along(iso)) nb[[iso[k]]] <- setdiff(near[[k]], iso[k])
}
names(nb) <- geom$lsoa21cd
NB <- lapply(nb, function(i) geom$lsoa21cd[i])

message(sprintf("  adjacency: %.1f neighbours on average, %d isolated",
                mean(lengths(NB)), sum(lengths(NB) == 0)))

# In the relocation world Longhill is at the top of Elm Grove, so that
# is the LSOA its catchment has to contain. Without telling the design
# where the school actually is, it put the Elm Grove site inside BACA's
# region and left Longhill with nine LSOAs on the far side of the city.
elm_lsoa <- st_join(
  st_sf(geometry = st_sfc(st_point(c(COMART$lon, COMART$lat)), crs = 4326)) %>%
    st_transform(27700),
  geom %>% select(lsoa21cd), join = st_within)$lsoa21cd
stopifnot(length(elm_lsoa) == 1, !is.na(elm_lsoa))
W_ELM$moved_school    <- list(name = "Longhill High School", lsoa = elm_lsoa)
W_ELM210$moved_school <- W_ELM$moved_school
message("  relocated Longhill sits in ", elm_lsoa)

# An enclave is a region wholly surrounded by ONE other region. The
# test "all my neighbours belong to a single other region" is not
# enough on its own: Longhill and PACA sit at the two ends of the city
# with the sea on one side and the boundary on the other, so they have
# one land neighbour each and are not islands. LSOAs on the outer edge
# of the study area are therefore exempt.
outer_edge <- {
  hull <- st_union(geom) %>% st_boundary()
  geom$lsoa21cd[lengths(st_intersects(geom, hull)) > 0]
}
message(sprintf("  %d of %d LSOAs lie on the edge of the study area",
                length(outer_edge), nrow(geom)))

components <- function(members) {
  seen <- character(0); out <- list()
  for (s in members) {
    if (s %in% seen) next
    stack <- s; comp <- character(0)
    while (length(stack)) {
      v <- stack[1]; stack <- stack[-1]
      if (v %in% comp) next
      comp <- c(comp, v)
      stack <- c(stack, setdiff(intersect(NB[[v]], members), comp))
    }
    seen <- c(seen, comp); out[[length(out) + 1]] <- comp
  }
  out
}

# ---- The pipeline, run per world -------------------------------------
#   1. Dominant flow      every LSOA joins the school it sends most to
#   2. Contiguity repair  a region has to be one piece
#   3. Capacity balance   trade boundary LSOAs until each region holds
#                         about as many children as it has places

#' @param seed an existing assignment to start from, instead of the
#'   dominant-flow construction. Used for the power diagram, whose shape
#'   repair knocks it off the capacity balance it was built to hold, so
#'   it has to be re-balanced on the same rules as everything else.
regionalise <- function(w, groups, tol = 0.05, max_moves = 400, seed = NULL) {

  sch_of <- setNames(rep(names(groups), lengths(groups)), unlist(groups))

  flow_to <- function(ls, reg) {
    f <- w$flows$flow[w$flows$lsoa == ls & w$flows$name == reg]
    if (length(f)) sum(f) else 0
  }

  # Accessibility of a group from an LSOA: the journey time to the
  # nearest of its schools. Used wherever a repair has to choose between
  # regions, because a fragment or an island should join whichever
  # catchment is genuinely quickest to reach, not whichever it happened
  # to send the most modelled flow to.
  acc <- w$cost %>%
    mutate(grp = unname(sch_of[name])) %>%
    filter(!is.na(grp)) %>%
    group_by(lsoa, grp) %>%
    summarise(cij = min(cij), .groups = "drop")
  acc_key <- setNames(acc$cij, paste(acc$lsoa, acc$grp))
  acc_to <- function(ls, g) {
    v <- acc_key[paste(ls, g)]
    ifelse(is.na(v), Inf, v)
  }
  acc_to_v <- function(ls, g) unname(vapply(ls, acc_to, numeric(1), g = g))

  # The LSOA each school sits in. A catchment that does not contain its
  # own school is not a catchment, and without this the relocation
  # design put the Elm Grove site inside BACA's region.
  home_lsoa <- st_join(
    schools_sf() %>% filter(name %in% unlist(groups)) %>% st_transform(27700),
    geom %>% select(lsoa21cd), join = st_within) %>%
    st_drop_geometry() %>%
    transmute(name, lsoa = lsoa21cd, grp = unname(sch_of[name]))
  if (!is.null(w$moved_school))
    home_lsoa$lsoa[home_lsoa$name == w$moved_school$name] <- w$moved_school$lsoa
  stopifnot(!any(is.na(home_lsoa$lsoa)))
  PINNED <- setNames(home_lsoa$grp, home_lsoa$lsoa)

  # ---- 1. Dominant flow ----------------------------------------------
  assign <- if (!is.null(seed)) {
    seed %>% select(lsoa, region) %>% mutate(grp = unname(sch_of[region]))
  } else if (identical(w$seed_mode, "accessibility")) {
    acc %>%
      group_by(lsoa) %>%
      slice_min(cij, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(lsoa, region = purrr::map_chr(grp, ~ groups[[.x]][1]), grp)
  } else {
    w$flows %>%
      filter(name %in% CATCH_S) %>%
      group_by(lsoa) %>%
      slice_max(flow, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(lsoa, region = name) %>%
      mutate(grp = unname(sch_of[region]))
  }
  stopifnot(nrow(assign) == nrow(lsoa_children))
  # Every school starts in its own region and stays there.
  assign$grp[match(names(PINNED), assign$lsoa)] <- unname(PINNED)

  set_grp <- function(a, ls, g) {
    a$grp[a$lsoa %in% ls] <- g
    a$region[a$lsoa %in% ls] <- groups[[g]][1]
    a
  }

  # ---- 2. Repair -----------------------------------------------------
  # Two faults, and the second is the one that made the power diagram
  # look odd: a region can be in one piece and still enclose an island
  # of another region, because a ring is connected.
  #
  #   fragment  a component of a region that is not its main body
  #   enclave   a component every neighbour of which is one other region
  #
  # Both are repaired the same way: the offending component joins the
  # adjacent region that is most accessible from it, weighted by
  # children. An enclave has only one neighbouring region, so for those
  # the choice makes itself.
  repair <- function(a, passes = 30) {
    for (p in seq_len(passes)) {
      moved <- 0
      for (g in unique(a$grp)) {
        mem <- a$lsoa[a$grp == g]
        cc  <- components(mem)
        main <- if (length(cc) > 1) cc[[which.max(sapply(cc, length))]] else cc[[1]]
        for (k in cc) {
          is_main <- identical(k, main)
          adj <- setdiff(unique(unlist(NB[k])), k)
          nbr <- setdiff(unique(a$grp[match(adj, a$lsoa)]), c(NA, g))
          if (!length(nbr)) next
          enclosed <- length(nbr) == 1 && !any(k %in% outer_edge)
          # A component holding a school never moves, and the main body
          # only moves if it is a true enclave of one other region.
          if (any(k %in% names(PINNED))) next
          if (is_main && !enclosed) next
          if (!is_main || enclosed) {
            kids <- lsoa_children$Oi[match(k, lsoa_children$lsoa)]
            best <- nbr[which.min(sapply(nbr, function(r)
              weighted.mean(sapply(k, acc_to, g = r), kids)))]
            a <- set_grp(a, k, best)
            moved <- moved + length(k)
          }
        }
      }
      if (!moved) break
    }
    a
  }

  assign   <- repair(assign)
  dominant <- assign

  # ---- 3. Capacity balance -------------------------------------------
  places <- sapply(groups, function(g) sum(w$pan[g]))
  placed <- sum(w$flows$flow[w$flows$name %in% unlist(groups)])
  target <- places / sum(places) * placed

  size <- function(a) {
    s <- a %>% left_join(w$demand, by = "lsoa") %>%
      group_by(grp) %>% summarise(n = sum(demand), .groups = "drop")
    setNames(s$n, s$grp)[names(groups)]
  }

  # A move is legal only if both regions stay in one piece AND neither
  # ends up enclosing an island. Checking connectivity alone let the
  # balancer carve out enclaves it had no way to see.
  clean <- function(a, gs) {
    for (g in gs) {
      cc <- components(a$lsoa[a$grp == g])
      if (length(cc) != 1) return(FALSE)
      k <- cc[[1]]
      adj <- setdiff(unique(unlist(NB[k])), k)
      nbr <- setdiff(unique(a$grp[match(adj, a$lsoa)]), c(NA, g))
      if (length(nbr) == 1 && !any(k %in% outer_edge)) return(FALSE)
    }
    TRUE
  }

  m <- 0
  for (m in seq_len(max_moves)) {
    cur <- size(assign); rel <- (cur - target) / target
    if (max(abs(rel), na.rm = TRUE) <= tol) break
    donors    <- names(sort(rel[rel >  tol], decreasing = TRUE))
    receivers <- names(sort(rel[rel < -tol]))
    if (!length(donors) || !length(receivers)) break

    done <- FALSE
    for (from in donors) {
      for (to in receivers) {
        cand <- assign$lsoa[assign$grp == from & !assign$lsoa %in% names(PINNED)]
        cand <- cand[sapply(cand, function(l)
          any(assign$grp[match(NB[[l]], assign$lsoa)] == to, na.rm = TRUE))]
        if (!length(cand)) next

        # WHICH NEIGHBOURHOOD TO GIVE UP.
        #
        # Ranking by how much WORSE the receiving catchment is than the
        # donating one looks right and behaves badly at the edges of the
        # city. Rottingdean and Saltdean are 46 to 52 minutes from
        # Longhill at Elm Grove and 54 to 67 from Stringer/Varndean: a
        # difference of only 8 to 15 minutes, so on that ranking they
        # are cheap to give away, and they went first. Somewhere in
        # Hanover, five minutes from Elm Grove and twelve from Varndean,
        # scored worse and was kept.
        #
        # The ranking is therefore on the journey the child would
        # actually make after the move, not on the change in it. A
        # neighbourhood with a short alternative is given up before one
        # whose only alternative is an hour away.
        cost_l <- if (identical(w$move_cost, "absolute"))
          sapply(cand, function(l) acc_to(l, to))
        else
          sapply(cand, function(l) acc_to(l, to) - acc_to(l, from))

        # And a hard floor under it, for the remote end of the city
        # only. A neighbourhood whose nearest catchment is already
        # PROTECT_MIN minutes away is never moved further from it,
        # whatever that does for the capacity balance: Rottingdean and
        # Saltdean have no good option and should keep their least bad
        # one. Everywhere with a reasonable alternative stays fully
        # tradeable, which is the point - the inner neighbourhoods are
        # the ones that should absorb the balancing.
        #
        # A blanket tolerance was tried first and was worse than useless.
        # Six minutes blocked the inner moves as well, the balancer could
        # shed almost nothing, and Longhill finished 231% over.
        if (is.finite(w$protect_min %||% Inf)) {
          best_t <- vapply(cand, function(l) min(acc$cij[acc$lsoa == l]),
                           numeric(1))
          keep <- best_t < w$protect_min |
                  acc_to_v(cand, to) <= acc_to_v(cand, from) + 1e-9
          cand <- cand[keep]; cost_l <- cost_l[keep]
          if (!length(cand)) next
        }

        for (pick in cand[order(cost_l)]) {
          trial <- set_grp(assign, pick, to)
          if (clean(trial, c(from, to))) { assign <- trial; done <- TRUE; break }
        }
        if (done) break
      }
      if (done) break
    }
    if (!done) break
  }

  # ---- 4. Smooth the ragged edges ------------------------------------
  # A neighbourhood can pass the island test and still be a sliver: one
  # neighbour of seven in its own catchment and the rest in someone
  # else's. Two of these faced each other across Elm Grove - a BACA
  # tongue reaching down to St Luke's, and a Stringer/Varndean one
  # reaching up past it - each nearly surrounded by the other.
  #
  # Any neighbourhood with at most one neighbour of its own joins
  # whichever adjacent catchment it can reach quickest, provided that
  # keeps both catchments in one piece and island-free. This is the
  # standard tidying pass of a regionalisation and it costs a little
  # capacity balance, which is reported rather than hidden.
  own_nbrs <- function(a, l) {
    n <- a$grp[match(NB[[l]], a$lsoa)]
    sum(n == a$grp[a$lsoa == l], na.rm = TRUE)
  }
  # The pass is capacity-aware. Smoothing purely on accessibility tidied
  # the map and undid the balancing with it - the relocation design went
  # from 73% over to 116% - so a sliver only moves into a catchment that
  # has room, or out of one that is over its target.
  smoothed <- 0
  for (p in seq_len(10)) {
    cur <- size(assign)
    moved <- 0
    for (l in setdiff(assign$lsoa, names(PINNED))) {
      if (own_nbrs(assign, l) > 1) next
      here <- assign$grp[assign$lsoa == l]
      cand <- setdiff(unique(assign$grp[match(NB[[l]], assign$lsoa)]), c(NA, here))
      if (!length(cand)) next
      best <- cand[which.min(sapply(cand, acc_to, ls = l))]
      if (acc_to(l, best) >= acc_to(l, here)) next
      room_there <- cur[[best]] <= target[[best]] * (1 + tol)
      spare_here <- cur[[here]] >= target[[here]] * (1 - tol)
      if (!room_there && !spare_here) next
      trial <- set_grp(assign, l, best)
      if (clean(trial, c(here, best))) {
        d <- w$demand$demand[w$demand$lsoa == l]
        cur[[here]] <- cur[[here]] - d; cur[[best]] <- cur[[best]] + d
        assign <- trial; moved <- moved + 1; smoothed <- smoothed + 1
      }
    }
    if (!moved) break
  }

  # A final repair, because balancing and smoothing can still leave a
  # stray island, then a check that it did not undo the capacity work.
  assign <- repair(assign)

  cur <- size(assign)
  list(assign = assign %>% select(lsoa, region, grp), dominant = dominant,
       groups = groups, target = target, moves = m, world = w$label,
       pinned = PINNED, smoothed = smoothed,
       final = tibble(grp = names(target), demand = unname(cur[names(target)]),
                      target = unname(target), places = unname(places)) %>%
         mutate(gap = demand / target - 1))
}

# Repair a design that arrived from somewhere else - the power diagram,
# and the whole-LSOA approximation of the current map - on the same
# rules. The power diagram is built zone by zone against a price and has
# no contiguity logic of its own beyond its own repair step, and it
# arrived with five fragments and three islands.
repair_external <- function(a, groups, w) {
  sch_of <- setNames(rep(names(groups), lengths(groups)), unlist(groups))
  acc <- w$cost %>%
    mutate(grp = unname(sch_of[name])) %>%
    filter(!is.na(grp)) %>%
    group_by(lsoa, grp) %>% summarise(cij = min(cij), .groups = "drop")
  acc_key <- setNames(acc$cij, paste(acc$lsoa, acc$grp))

  home <- st_join(
    schools_sf() %>% filter(name %in% unlist(groups)) %>% st_transform(27700),
    geom %>% select(lsoa21cd), join = st_within) %>%
    st_drop_geometry() %>% transmute(lsoa = lsoa21cd, grp = unname(sch_of[name]))
  PINNED <- setNames(home$grp, home$lsoa)

  a <- a %>% mutate(grp = unname(sch_of[region]))
  for (p in 1:30) {
    moved <- 0
    for (g in unique(a$grp)) {
      cc <- components(a$lsoa[a$grp == g])
      main <- cc[[which.max(sapply(cc, length))]]
      for (k in cc) {
        if (any(k %in% names(PINNED))) next
        adj <- setdiff(unique(unlist(NB[k])), k)
        nbr <- setdiff(unique(a$grp[match(adj, a$lsoa)]), c(NA, g))
        if (!length(nbr)) next
        enclosed <- length(nbr) == 1 && !any(k %in% outer_edge)
        if (identical(k, main) && !enclosed) next
        kids <- lsoa_children$Oi[match(k, lsoa_children$lsoa)]
        best <- nbr[which.min(sapply(nbr, function(r)
          weighted.mean(sapply(k, function(l) {
            v <- acc_key[paste(l, r)]; ifelse(is.na(v), Inf, v) }), kids)))]
        a$grp[a$lsoa %in% k] <- best
        a$region[a$lsoa %in% k] <- groups[[best]][1]
        moved <- moved + length(k)
      }
    }
    if (!moved) break
  }
  a %>% select(lsoa, region, grp)
}

GROUPS_SINGLE <- setNames(as.list(CATCH_S), CATCH_S)

# The paired design keeps exactly the groupings the council uses, so it
# takes the council's own keys. Giving it prettier names made every LSOA
# in a paired catchment look as though it had changed hands, because the
# comparison was matching on the label rather than on the schools.
GROUPS_PAIRED <- list(
  DS_Varndean = c("Dorothy Stringer School", "Varndean School"),
  Hove_Blatch = c("Hove Park School", "Blatchington Mill School"),
  Patcham     = "Patcham High School",
  Longhill    = "Longhill High School",
  BACA        = "Brighton Aldridge Community Academy",
  PACA        = "Portslade Aldridge Community Academy")
GROUPS_NOW <- GROUPS_PAIRED

message("\n  Regionalising...")
R_SINGLE <- regionalise(W_NOW, GROUPS_SINGLE)
R_PAIRED <- regionalise(W_NOW, GROUPS_PAIRED)
R_ELM    <- regionalise(W_ELM, GROUPS_PAIRED)
R_ELM210 <- regionalise(W_ELM210, GROUPS_PAIRED)
for (r in list(R_SINGLE, R_PAIRED, R_ELM, R_ELM210))
  message(sprintf("    %-24s %3d moves, %2d slivers smoothed, worst gap %+.0f%%",
                  r$world, r$moves, r$smoothed, 100 * max(abs(r$final$gap))))

# ---- The catchments now, and the power-diagram design ----------------
# The power diagram in the open bundle is the other method already in
# this work: it assigns each zone to the catchment with the lowest cost
# minus a price, and iterates the prices until every catchment hits its
# capacity. It is a proximity-and-capacity design. The flow regions are
# a demand design. Comparing them is the point of putting both here.

now_assign <- city_zones %>%
  group_by(lsoa) %>% slice_max(Oi, n = 1, with_ties = FALSE) %>% ungroup() %>%
  transmute(lsoa, region = purrr::map_chr(catchment,
    ~ GROUPS_NOW[[.x]][1] %||% NA_character_)) %>%
  filter(!is.na(region))

cdo <- bh_data("catchment_design_open.rds")
pd_zone <- cdo$designs$now_210$assignment_repaired %||%
           cdo$designs$now_210$assignment
stopifnot(!is.null(pd_zone), all(c("zone", "catchment") %in% names(pd_zone)))

# Zones back to whole LSOAs: where a split LSOA's halves landed in
# different catchments, the half with more children wins.
pd_assign <- pd_zone %>%
  inner_join(city_zones %>% select(zone, lsoa, Oi), by = "zone") %>%
  group_by(lsoa, catchment) %>% summarise(Oi = sum(Oi), .groups = "drop_last") %>%
  slice_max(Oi, n = 1, with_ties = FALSE) %>% ungroup() %>%
  filter(catchment %in% names(GROUPS_NOW)) %>%
  transmute(lsoa, region = purrr::map_chr(catchment, ~ GROUPS_NOW[[.x]][1]))

message(sprintf("  power-diagram design covers %d of %d LSOAs",
                nrow(pd_assign), nrow(lsoa_children)))

# The power diagram is priced zone by zone and has no notion of an
# island, so it arrived with five detached fragments and three
# catchments wholly enclosed by another. Both are repaired on the same
# accessibility rule as the flow regions, so all five designs are held
# to the same standard of shape.
pd_assign <- repair_external(pd_assign, GROUPS_NOW, W_NOW)
# Repairing the shape knocks it off the capacity balance it was built to
# hold - the worst gap went from +8% to +26% - so it is re-balanced on
# the same rules as the flow designs, which now preserve shape while
# they trade.
R_PD <- regionalise(W_NOW, GROUPS_NOW, seed = pd_assign)
pd_assign <- R_PD$assign
message(sprintf("  power diagram re-balanced in %d moves, worst gap now %+.0f%%",
                R_PD$moves, 100 * max(abs(R_PD$final$gap))))

# ---- Every design has to be a usable map ----------------------------
# Contiguous, island-free, and containing its own schools. Asserted
# rather than hoped for: the relocation design failed all three and
# looked plausible enough on a thumbnail to survive a first reading.

audit <- function(a, groups, label, w) {
  sch_of <- setNames(rep(names(groups), lengths(groups)), unlist(groups))
  a <- a %>% mutate(grp = unname(sch_of[region]))
  home <- st_join(
    schools_sf() %>% filter(name %in% unlist(groups)) %>% st_transform(27700),
    geom %>% select(lsoa21cd), join = st_within) %>%
    st_drop_geometry() %>% transmute(lsoa = lsoa21cd, want = unname(sch_of[name]))
  if (!is.null(w$moved_school))
    home$lsoa[home$want == unname(sch_of[w$moved_school$name])] <-
      w$moved_school$lsoa
  frag <- 0; encl <- 0
  for (g in unique(a$grp)) {
    cc <- components(a$lsoa[a$grp == g])
    frag <- frag + length(cc) - 1
    for (k in cc) {
      adj <- setdiff(unique(unlist(NB[k])), k)
      nbr <- setdiff(unique(a$grp[match(adj, a$lsoa)]), c(NA, g))
      if (length(nbr) == 1 && !any(k %in% outer_edge)) encl <- encl + 1
    }
  }
  # Ragged edges: neighbourhoods with at most one neighbour of their own
  # catchment. Not islands - they pass the enclave test - but they read
  # as slivers on a map and they are what a reader notices. The current
  # map has a BACA tongue reaching down past St Luke's pool and two
  # Stringer/Varndean ones reaching up past Elm Grove, facing each other.
  ragged <- sum(vapply(a$lsoa, function(l) {
    n <- a$grp[match(NB[[l]], a$lsoa)]
    sum(n == a$grp[a$lsoa == l], na.rm = TRUE) <= 1
  }, logical(1)))

  got <- a$grp[match(home$lsoa, a$lsoa)]
  tibble(design = label, fragments = frag, enclaves = encl,
         ragged = ragged,
         schools_outside = sum(got != home$want, na.rm = TRUE))
}

# ---- Diagnostics -----------------------------------------------------
# Self-containment is the travel-to-work-area statistic: the share of a
# region's children whose modelled first choice is inside it.

score_design <- function(assign, groups, label, w, target = NULL) {
  sch_of <- setNames(rep(names(groups), lengths(groups)), unlist(groups))
  first_choice <- w$flows %>%
    group_by(lsoa) %>% slice_max(flow, n = 1, with_ties = FALSE) %>%
    ungroup() %>% select(lsoa, top_school = name)

  a <- assign %>%
    mutate(grp = unname(sch_of[region])) %>%
    left_join(lsoa_children, by = "lsoa") %>%
    left_join(first_choice, by = "lsoa") %>%
    mutate(top_grp = unname(sch_of[top_school]),
           contained = !is.na(top_grp) & top_grp == grp)

  dem  <- a %>% left_join(w$demand %>% select(lsoa, demand), by = "lsoa")
  kids <- tapply(dem$demand, dem$grp, sum)
  if (is.null(target)) {
    places <- sapply(groups, function(g) sum(w$pan[g]))
    target <- places / sum(places) *
      sum(w$flows$flow[w$flows$name %in% unlist(groups)])
  }
  gap <- kids[names(target)] / target - 1

  jt <- a %>%
    left_join(w$cost, by = "lsoa", relationship = "many-to-many") %>%
    filter(name %in% unlist(groups),
           unname(sch_of[name]) == grp) %>%
    group_by(lsoa) %>% slice_min(cij, n = 1, with_ties = FALSE) %>% ungroup()

  idc <- a %>% left_join(dep$idaci %>% select(lsoa, idaci_score), by = "lsoa")
  idaci_by <- tapply(idc$idaci_score * idc$Oi, idc$grp, sum, na.rm = TRUE) /
    tapply(idc$Oi, idc$grp, sum)

  tibble(design = label,
         regions = n_distinct(a$grp),
         self_containment = sum(a$Oi[a$contained]) / sum(a$Oi),
         mean_journey = weighted.mean(jt$cij, jt$Oi),
         worst_gap = max(abs(gap), na.rm = TRUE),
         idaci_lo = min(idaci_by), idaci_hi = max(idaci_by),
         idaci_range = diff(range(idaci_by)))
}

DESIGNS <- list(
  list(a = now_assign,       g = GROUPS_NOW,    w = W_NOW, t = NULL,
       lab = "Current catchments"),
  list(a = pd_assign,        g = GROUPS_NOW,    w = W_NOW, t = R_PD$target,
       lab = "Power diagram (proximity and capacity)"),
  list(a = R_SINGLE$assign,  g = GROUPS_SINGLE, w = W_NOW, t = R_SINGLE$target,
       lab = "Flow regions, one per school"),
  list(a = R_PAIRED$assign,  g = GROUPS_PAIRED, w = W_NOW, t = R_PAIRED$target,
       lab = "Flow regions, pairs kept"),
  list(a = R_ELM$assign,     g = GROUPS_PAIRED, w = W_ELM, t = R_ELM$target,
       lab = "Flow regions, Elm Grove, PAN 150"),
  list(a = R_ELM210$assign,  g = GROUPS_PAIRED, w = W_ELM210, t = R_ELM210$target,
       lab = "Flow regions, Elm Grove, PAN 210"))

# ---- Why the relocation design looks the way it does -----------------
# Its Stringer/Varndean region reaches from Varndean to Saltdean, which
# is an odd-looking catchment and worth explaining rather than tidying
# away. For the easternmost neighbourhoods the relocated Longhill is
# STILL the most accessible school - the coast road reaches Elm Grove
# before it reaches Varndean - but at an admission number of 150 it is
# already full, and BACA is exactly at capacity. So the east attaches to
# a school it cannot reach quickly, because the two schools it can reach
# have no room.
EAST_N <- 12
east_lsoa <- {
  ctr <- geom %>% st_point_on_surface()
  tibble(lsoa = ctr$lsoa21cd, easting = st_coordinates(ctr)[, 1]) %>%
    arrange(desc(easting)) %>% head(EAST_N) %>% pull(lsoa)
}

east_of <- function(r, w, label) {
  sch_of <- setNames(rep(names(w$pan)[0], 0), character(0))  # placeholder
  sch_of <- setNames(rep(names(GROUPS_PAIRED), lengths(GROUPS_PAIRED)),
                     unlist(GROUPS_PAIRED))
  accE <- w$cost %>%
    mutate(grp = unname(sch_of[name])) %>% filter(!is.na(grp)) %>%
    group_by(lsoa, grp) %>% summarise(cij = min(cij), .groups = "drop")
  accE %>%
    filter(lsoa %in% east_lsoa) %>%
    group_by(lsoa) %>%
    summarise(nearest = grp[which.min(cij)], t_near = min(cij),
              assigned = r$assign$grp[match(lsoa[1], r$assign$lsoa)],
              t_assigned = cij[grp == assigned][1], .groups = "drop") %>%
    left_join(lsoa_children, by = "lsoa") %>%
    mutate(design = label)
}

# ---- What admission number does the east actually need? -------------
# Seeded on accessibility, Longhill's natural area at Elm Grove holds
# far more demand than 150 or 210 places, so the balancer has to give
# most of it away - and what it gives away first is the far east, which
# is furthest from the new site. This sweeps the admission number and
# asks how much of the east survives balancing at each one.

ELM_SWEEP <- c(150, 180, 210, 240, 270, 300, 330, 360)
elm_sweep <- purrr::map_dfr(ELM_SWEEP, function(p) {
  w <- world("ELMU", oi$costs_elm, sprintf("Elm Grove, PAN %d", p), lh_pan = p)
  w$moved_school <- W_ELM$moved_school
  w$seed_mode <- "accessibility"
  w$move_cost <- "absolute"; w$protect_min <- PROTECT_MIN
  r <- regionalise(w, GROUPS_PAIRED)
  e <- east_of(r, w, sprintf("PAN %d", p))
  tibble(pan = p,
         east_in_longhill = sum(e$assigned == "Longhill"),
         east_minutes = weighted.mean(e$t_assigned, e$Oi),
         longhill_gap = r$final$gap[r$final$grp == "Longhill"],
         worst_gap = max(abs(r$final$gap)),
         assign = list(r$assign), regionalised = list(r), world = list(w))
})

message("\n=== Longhill at Elm Grove: what admission number holds the east? ===")
print(as.data.frame(elm_sweep %>%
  transmute(`PAN` = pan,
            `East in Longhill` = sprintf("%d of %d", east_in_longhill, EAST_N),
            `Mean journey for the east` = sprintf("%.0f min", east_minutes),
            `Longhill capacity gap` = sprintf("%+.0f%%", 100 * longhill_gap),
            `Worst gap in the design` = sprintf("%+.0f%%", 100 * worst_gap))),
  row.names = FALSE)

east_check <- bind_rows(
  east_of(R_ELM,    W_ELM,    "Elm Grove, PAN 150"),
  east_of(R_ELM210, W_ELM210, "Elm Grove, PAN 210"))

message(sprintf("\n=== The %d easternmost neighbourhoods under each relocation ===",
                EAST_N))
print(as.data.frame(east_check %>% group_by(design) %>%
  summarise(`to Longhill` = sum(assigned == "Longhill"),
            `to Stringer/Varndean` = sum(assigned == "DS_Varndean"),
            `nearest is Longhill` = sum(nearest == "Longhill"),
            `mean min, assigned` = round(weighted.mean(t_assigned, Oi)),
            `mean min, nearest` = round(weighted.mean(t_near, Oi)),
            .groups = "drop")), row.names = FALSE)

shape <- purrr::map_dfr(DESIGNS, ~ audit(.x$a, .x$g, .x$lab, .x$w))
message("\n=== Is each design a usable map? ===")
print(as.data.frame(shape), row.names = FALSE)
bad <- shape %>% filter(fragments > 0 | enclaves > 0 | schools_outside > 0)
if (nrow(bad))
  warning("designs with shape problems: ",
          paste(bad$design, collapse = "; "), call. = FALSE)

designs <- purrr::map_dfr(DESIGNS, ~ score_design(.x$a, .x$g, .x$lab, .x$w, .x$t)) %>%
  left_join(shape, by = "design")

message("\n=== The designs compared ===")
print(as.data.frame(designs %>%
  transmute(Design = design, Regions = regions,
            `Self-containment` = sprintf("%.0f%%", 100 * self_containment),
            `Mean journey` = sprintf("%.1f min", mean_journey),
            `Worst capacity gap` = sprintf("%+.0f%%", 100 * worst_gap),
            IDACI = sprintf("%.3f to %.3f", idaci_lo, idaci_hi))),
  row.names = FALSE)

# ---- Allocation under a distance-based over-subscription rule --------
# A catchment map is only half a policy; the other half is what happens
# when a school is over-subscribed. This runs the rule England uses, in
# the simplified form published data supports: an over-subscribed school
# admits in-catchment children first and, within each priority group,
# the nearest first; children who miss out cascade to their next
# preference. It is deferred acceptance - schools hold offers
# provisionally and can bump a held child when a higher-priority
# applicant arrives - which is how the coordinated scheme behaves. The
# faith schools admit on criteria this cannot see, so they get no
# catchment and rank purely on distance.

allocate <- function(assign, groups, w, tol = 1e-3, max_round = 60) {
  sch_of <- setNames(rep(names(groups), lengths(groups)), unlist(groups))
  home_g <- setNames(unname(sch_of[assign$region]), assign$lsoa)

  prefs <- w$cost %>%
    left_join(oi$attract %>% select(name, W = W_wprefs), by = "name") %>%
    filter(is.finite(cij), cij > 0, lsoa %in% assign$lsoa) %>%
    mutate(u = W * cij^(-BETA),
           grp = unname(sch_of[name]),
           in_catch = !is.na(grp) & grp == unname(home_g[lsoa])) %>%
    group_by(lsoa) %>% arrange(desc(u), .by_group = TRUE) %>%
    mutate(rank = row_number()) %>% ungroup()

  cap  <- w$pan[unique(prefs$name)]
  held <- tibble(lsoa = character(0), name = character(0), n = numeric(0))
  nxt  <- setNames(rep(1L, nrow(assign)), assign$lsoa)
  left <- setNames(w$demand$Oi[match(assign$lsoa, w$demand$lsoa)], assign$lsoa)
  n_pref <- max(prefs$rank)

  for (r in seq_len(max_round)) {
    apply_now <- tibble(lsoa = names(left)[left > tol],
                        n = unname(left[left > tol])) %>%
      mutate(rank = unname(nxt[lsoa])) %>%
      inner_join(prefs %>% select(lsoa, name, rank, in_catch, cij),
                 by = c("lsoa", "rank"))
    if (!nrow(apply_now)) break

    pool <- bind_rows(
      held %>% left_join(prefs %>% select(lsoa, name, in_catch, cij),
                         by = c("lsoa", "name")),
      apply_now %>% select(lsoa, name, n, in_catch, cij))

    keep <- pool %>%
      group_by(name) %>%
      arrange(desc(in_catch), cij, .by_group = TRUE) %>%
      mutate(cum = cumsum(n),
             room = pmax(0, pmin(n, unname(cap[name]) - (cum - n))),
             rejected = n - room) %>%
      ungroup()

    held <- keep %>% filter(room > tol) %>% transmute(lsoa, name, n = room)
    back <- keep %>% group_by(lsoa) %>%
      summarise(rej = sum(rejected), .groups = "drop")

    left[] <- 0
    left[back$lsoa] <- back$rej
    moved <- back$lsoa[back$rej > tol]
    nxt[moved] <- nxt[moved] + 1L
    left[moved[nxt[moved] > n_pref]] <- 0
    if (all(left <= tol)) break
  }

  res <- held %>%
    mutate(grp = unname(sch_of[name]), home = unname(home_g[lsoa]),
           cross = is.na(grp) | is.na(home) | grp != home) %>%
    left_join(prefs %>% select(lsoa, name, cij, km, rank),
              by = c("lsoa", "name"))

  # The detail is kept, not just the summary. Every journey statistic
  # and every school-level intake figure below is computed from it, and
  # computing them here instead would mean this function grew a new
  # return column each time section 8 asked a new question.
  tibble(placed = sum(res$n),
         cross_share = sum(res$n[res$cross]) / sum(res$n),
         faith_share = sum(res$n[res$name %in% FAITH]) / sum(res$n),
         local_share = sum(res$n[!res$cross & !res$name %in% FAITH]) / sum(res$n),
         first_pref = sum(res$n[res$rank == 1]) / sum(res$n),
         mean_journey = weighted.mean(res$cij, res$n), rounds = r,
         detail = list(res))
}

message("\n=== Allocation under an in-catchment-then-distance rule ===")
alloc <- purrr::map_dfr(DESIGNS, function(d)
  allocate(d$a, d$g, d$w) %>% mutate(design = d$lab))

print(as.data.frame(alloc %>%
  transmute(Design = design, Placed = round(placed),
            `1st pref` = sprintf("%.0f%%", 100 * first_pref),
            `Own catchment` = sprintf("%.0f%%", 100 * local_share),
            Crossed = sprintf("%.0f%%", 100 * cross_share),
            `of which faith` = sprintf("%.0f%%", 100 * faith_share),
            `Mean journey` = sprintf("%.1f min", mean_journey))),
  row.names = FALSE)

# ---- The regions as polygons ----------------------------------------

dissolve <- function(assign, groups, label) {
  sch_of <- setNames(rep(names(groups), lengths(groups)), unlist(groups))
  geom %>%
    inner_join(assign %>% mutate(grp = unname(sch_of[region])),
               by = c("lsoa21cd" = "lsoa")) %>%
    group_by(grp) %>% summarise(lsoas = n(), .groups = "drop") %>%
    # A dissolve leaves hairline slivers where neighbours were digitised
    # slightly apart; a zero-width buffer closes them.
    st_buffer(0) %>%
    mutate(design = label) %>%
    st_transform(4326)
}

# The CURRENT map is a published boundary file, not an LSOA aggregate,
# and it must be drawn as published. Dissolving LSOAs to approximate it
# produced a map that did not match the one section 4 draws: 30 of the
# 165 LSOAs straddle a current boundary, and giving each one whole to
# its larger half moves the line by up to an LSOA in either direction.
#
# The alternatives have to be built from whole LSOAs - that is the unit
# a redesign can work in - so they keep the dissolve. Only the design
# that already exists as geometry uses its own geometry.
current_sf <- bh_data("catchments_current.geojson") %>%
  st_transform(4326) %>%
  mutate(grp = as_model_from_boundary(catchment), lsoas = NA_integer_,
         design = "Current catchments") %>%
  select(grp, lsoas, design)
stopifnot(!any(is.na(current_sf$grp)),
          setequal(current_sf$grp, names(GROUPS_NOW)))

regions_sf <- bind_rows(
  current_sf,
  purrr::map_dfr(DESIGNS[-1], ~ dissolve(.x$a, .x$g, .x$lab)))

# ---- IDACI profiles, the consultation's own method -------------------
# BH_Schools_Consultation/postcode_school_pop.qmd profiles the current
# catchments by putting every postcode inside a catchment polygon and
# counting households with dependent children by IDACI decile. The same
# method is applied here to every design, so the comparison is with the
# consultation's figures rather than with a different statistic that
# happens to be about deprivation.

pcd <- readr::read_csv(file.path(DATA, "postcode_children.csv"),
                       show_col_types = FALSE) %>%
  filter(!is.na(idaci_decile), hh_with_ch > 0) %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  st_transform(4326)

message(sprintf("\n  %s postcodes with dependent children and an IDACI decile",
                format(nrow(pcd), big.mark = ",")))

profile_design <- function(label) {
  poly <- regions_sf %>% filter(design == label)
  j <- st_join(pcd, poly %>% select(grp), join = st_within) %>%
    st_drop_geometry() %>%
    filter(!is.na(grp))
  j %>%
    group_by(grp, idaci_decile) %>%
    summarise(hh = sum(hh_with_ch), .groups = "drop_last") %>%
    mutate(share = hh / sum(hh), design = label) %>%
    ungroup()
}

idaci_profiles <- purrr::map_dfr(unique(regions_sf$design), profile_design)

# "Deprived" is IDACI decile 1 to 3, which is the definition used
# throughout this body of work, so these numbers sit next to the
# segregation figures in the open model rather than beside them.
idaci_region <- idaci_profiles %>%
  group_by(design, grp) %>%
  summarise(dep3 = sum(share[idaci_decile <= 3]),
            hh = sum(hh), .groups = "drop")

# Gorard's segregation index, the measure the open model uses for the
# same question: half the sum of the absolute difference between each
# catchment's share of the city's deprived households and its share of
# all households. 0 is a perfectly even spread, 1 is complete
# separation. It answers "which design spreads disadvantage most
# evenly" in one number, which max-minus-min cannot: a design can have a
# narrow range and still put every deprived household in one place.
gorard <- function(p) {
  F_j <- p$hh * p$dep3
  0.5 * sum(abs(F_j / sum(F_j) - p$hh / sum(p$hh)))
}

idaci_summary <- idaci_region %>%
  group_by(design) %>%
  summarise(lo = min(dep3), hi = max(dep3), spread = max(dep3) - min(dep3),
            gorard = gorard(pick(everything())),
            .groups = "drop") %>%
  arrange(gorard)

message("\n=== Households with dependent children in the three most deprived deciles ===")
print(as.data.frame(idaci_summary %>%
  transmute(Design = design,
            `Least deprived region` = sprintf("%.0f%%", 100 * lo),
            `Most deprived region` = sprintf("%.0f%%", 100 * hi),
            Spread = sprintf("%.0f pp", 100 * spread),
            Gorard = sprintf("%.3f", gorard))), row.names = FALSE)

# The full decile table: what share of each region's households with
# dependent children sits in each decile.
idaci_table <- idaci_profiles %>%
  select(design, grp, idaci_decile, share) %>%
  tidyr::complete(tidyr::nesting(design, grp), idaci_decile = 1:10,
                  fill = list(share = 0))

# Three bands rather than ten deciles. Ten single-hue steps are closer
# together than a reader can tell apart - the colour validator fails the
# ramp outright - and ten columns of percentages is the table that
# prompted this rewrite. Three bands answer the question the section
# actually asks.
IDACI_BANDS <- c("Deciles 1-3 (most deprived)", "Deciles 4-7",
                 "Deciles 8-10 (least deprived)")
idaci_bands <- idaci_profiles %>%
  mutate(band = factor(cut(idaci_decile, c(0, 3, 7, 10), labels = IDACI_BANDS),
                       IDACI_BANDS)) %>%
  group_by(design, grp, band) %>%
  summarise(share = sum(share), hh = sum(hh), .groups = "drop") %>%
  # A catchment with nobody in a band has no row for it, which left
  # Patcham unlabelled in the two relocation designs rather than
  # labelled zero. Fill the gaps.
  tidyr::complete(tidyr::nesting(design, grp), band,
                  fill = list(share = 0, hh = 0))

# The segregation curve. Catchments are ordered from least to most
# deprived, then the cumulative share of all households with dependent
# children is plotted against the cumulative share of the DEPRIVED ones.
# A design that spread deprivation perfectly evenly would trace the
# diagonal; the largest vertical gap from it is the dissimilarity index,
# and the area between is what Gorard summarises. One line per design,
# which is what makes five designs comparable at a glance in a way the
# decile table never was.
idaci_curve <- idaci_region %>%
  group_by(design) %>%
  arrange(dep3, .by_group = TRUE) %>%
  mutate(dep_hh = hh * dep3,
         x = cumsum(hh) / sum(hh),
         y = cumsum(dep_hh) / sum(dep_hh),
         gap = x - y) %>%
  ungroup()

idaci_curve <- bind_rows(
  idaci_curve %>% distinct(design) %>% mutate(grp = NA_character_, x = 0, y = 0,
                                              gap = 0, dep3 = NA, hh = NA,
                                              dep_hh = NA),
  idaci_curve) %>%
  arrange(design, x)

message("\n  Segregation curve, largest gap from the diagonal (dissimilarity):")
print(as.data.frame(idaci_curve %>% group_by(design) %>%
  summarise(dissimilarity = round(max(gap), 3), .groups = "drop") %>%
  arrange(dissimilarity)), row.names = FALSE)

# ---- Journeys, and who makes the long ones ---------------------------
# The tables above measure a design by its geography: how far a child is
# from the school whose catchment they live in. That is not the journey
# anyone actually makes. The journey they make is to the school the
# over-subscription rule gives them, which is what allocate() returns,
# and it is the only basis on which "this design costs more travel" can
# honestly be said.
#
# Every statistic here is weighted by children, so a design is not
# rewarded for shortening the journey of a neighbourhood with four
# children in it.

DESIGN_LEVELS <- vapply(DESIGNS, function(d) d$lab, character(1))

# Weighted quantile, type 1 (the inverse-CDF definition). The base
# quantile() has no weights, and repeating each child would mean
# expanding fractional counts.
wq <- function(x, w, p) {
  o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w) / sum(w) >= p)[1]]
}

journeys <- purrr::map_dfr(seq_len(nrow(alloc)), function(i)
  alloc$detail[[i]] %>% mutate(design = alloc$design[i])) %>%
  left_join(idaci_lsoa %>% select(lsoa, dep3), by = "lsoa") %>%
  mutate(design = factor(design, DESIGN_LEVELS))

stopifnot(!any(is.na(journeys$dep3)), !any(is.na(journeys$km)))

# Two thresholds, both from section 5: the National Travel Survey
# average one-way school trip, and half the DfE's statutory maximum for
# a secondary-age child. Neither is a standard anyone has adopted for
# Brighton; they are there so a number of minutes means something.
NTS_MIN  <- 19
LONG_MIN <- 40

journey_stats <- journeys %>%
  group_by(design) %>%
  summarise(
    children  = sum(n),
    mean_min  = weighted.mean(cij, n),
    median_min = wq(cij, n, 0.5),
    p90_min   = wq(cij, n, 0.9),
    over_nts  = sum(n[cij > NTS_MIN]) / sum(n),
    over_long = sum(n[cij > LONG_MIN]) / sum(n),
    mean_km   = weighted.mean(km, n),
    # Both ways, every school day, all children: the quantity a
    # transport budget and a carbon figure are actually counted in.
    child_km_day = 2 * sum(n * km),
    child_hours_day = 2 * sum(n * cij) / 60,
    # Children living in the 39 neighbourhoods in the three most
    # deprived deciles nationally, against the children living in the
    # other 126. Both means are over children rather than over
    # neighbourhoods, so a small deprived area does not count as much as
    # a large one.
    min_deprived = sum(n * dep3 * cij) / sum(n * dep3),
    min_rest     = sum(n * (1 - dep3) * cij) / sum(n * (1 - dep3)),
    .groups = "drop") %>%
  mutate(dep_gap = min_deprived - min_rest)

message("\n=== Journeys to the school each child is actually offered ===")
print(as.data.frame(journey_stats %>%
  transmute(Design = design,
            Mean = sprintf("%.1f min", mean_min),
            Median = sprintf("%.0f", median_min),
            `90th pct` = sprintf("%.0f", p90_min),
            `Over 40 min` = sprintf("%.0f%%", 100 * over_long),
            `Mean km` = sprintf("%.2f", mean_km),
            `Child-km/day` = format(round(child_km_day), big.mark = ","),
            `Deprived - rest` = sprintf("%+.1f min", dep_gap))),
  row.names = FALSE)

# The distribution behind the mean. One curve per design: the share of
# children whose offered school is within t minutes. A mean hides which
# tail a design is trading; this does not.
journey_curve <- purrr::map_dfr(levels(journeys$design), function(d) {
  j <- journeys %>% filter(design == d)
  tibble(design = d, t = seq(0, 60, by = 1)) %>%
    mutate(share = vapply(t, function(k) sum(j$n[j$cij <= k]) / sum(j$n),
                          numeric(1)))
}) %>% mutate(design = factor(design, DESIGN_LEVELS))

# ---- Deprivation between SCHOOLS, not between catchments -------------
# The catchment profiles measure the neighbourhoods a boundary encloses.
# They are not the intake: the over-subscription rule sends a real
# minority across boundaries and the faith schools admit across the city
# entirely, so two designs enclosing identically mixed catchments can
# still fill their schools very differently.
#
# This is the same Gorard index applied to the modelled intake of each
# school. It is restricted to the schools that HAVE a catchment, so it
# is like for like with the figure computed on the polygons; the faith
# schools are added back in the "all schools" column, with the caveat
# that this model ranks them on distance alone and their real criteria
# are not in published data.

intake_mix <- journeys %>%
  # dep_n is computed BEFORE the group total, not inside the same
  # summarise. Written the other way round, dplyr evaluates the
  # arguments in order and sum(n * dep3) multiplies the group's new
  # scalar total by every dep3 in it, which put one school's intake at
  # 800% deprived and went straight past a first reading.
  mutate(dep_n = n * dep3) %>%
  group_by(design, name) %>%
  summarise(n = sum(n), dep_n = sum(dep_n), .groups = "drop") %>%
  mutate(dep_share = dep_n / n)

gorard_n <- function(n, dep_n)
  0.5 * sum(abs(dep_n / sum(dep_n) - n / sum(n)))

intake_seg <- intake_mix %>%
  group_by(design) %>%
  summarise(
    gorard_schools = gorard_n(n[name %in% unlist(GROUPS_NOW)],
                              dep_n[name %in% unlist(GROUPS_NOW)]),
    gorard_all     = gorard_n(n, dep_n),
    intake_lo = min(dep_share[name %in% unlist(GROUPS_NOW)]),
    intake_hi = max(dep_share[name %in% unlist(GROUPS_NOW)]),
    .groups = "drop")

message("\n=== Deprivation between schools, on the modelled intakes ===")
print(as.data.frame(intake_seg %>%
  transmute(Design = design,
            `Gorard, catchment schools` = sprintf("%.3f", gorard_schools),
            `Gorard, all schools` = sprintf("%.3f", gorard_all),
            `Least to most deprived intake` =
              sprintf("%.0f%% to %.0f%%", 100 * intake_lo, 100 * intake_hi))),
  row.names = FALSE)

# ---- What moves against the current map -----------------------------

# Only designs that use the SAME grouping of schools can be compared
# against the current map LSOA by LSOA. The one-per-school design splits
# the two pairs, so every LSOA in a paired catchment necessarily has a
# different region label and the comparison reported 100% changed, which
# is an artefact of the labels rather than a fact about the map.
sch_now <- setNames(rep(names(GROUPS_NOW), lengths(GROUPS_NOW)),
                    unlist(GROUPS_NOW))

changed <- purrr::map_dfr(
  Filter(function(d) identical(sort(names(d$g)), sort(names(GROUPS_NOW))),
         DESIGNS[-1]),
  function(d) {
    sch_new <- setNames(rep(names(d$g), lengths(d$g)), unlist(d$g))
    now_assign %>%
      transmute(lsoa, now = unname(sch_now[region])) %>%
      inner_join(d$a %>% transmute(lsoa, new = unname(sch_new[region])),
                 by = "lsoa") %>%
      left_join(lsoa_children, by = "lsoa") %>%
      mutate(moved = now != new, design = d$lab)
  })

message("\n=== Change against the current map ===")
print(as.data.frame(changed %>% group_by(design) %>%
  summarise(lsoas = sum(moved), children = round(sum(Oi[moved])),
            pct = sprintf("%.0f%%", 100 * sum(Oi[moved]) / sum(Oi)),
            .groups = "drop")), row.names = FALSE)

# ---- One scorecard --------------------------------------------------
# Six designs, three families of measure, one table. Every entry is a
# number computed above rather than a judgement, and every row carries
# the direction that counts as better, so the ranking is done by the
# code and not by whoever writes the paragraph underneath it.

moved_by <- changed %>% group_by(design) %>%
  summarise(moved = sum(Oi[moved]) / sum(Oi), .groups = "drop")

score_wide <- designs %>%
  select(design, regions, self_containment, catch_journey = mean_journey,
         worst_gap, fragments, enclaves, ragged, schools_outside) %>%
  left_join(alloc %>% select(design, first_pref, local_share, cross_share),
            by = "design") %>%
  left_join(journey_stats, by = "design") %>%
  left_join(intake_seg, by = "design") %>%
  left_join(idaci_summary %>% select(design, gorard_catch = gorard,
                                     idaci_lo = lo, idaci_hi = hi),
            by = "design") %>%
  left_join(idaci_curve %>% group_by(design) %>%
              summarise(curve_gap = max(gap), .groups = "drop"),
            by = "design") %>%
  left_join(moved_by, by = "design") %>%
  mutate(design = factor(design, DESIGN_LEVELS),
         shape_faults = fragments + enclaves + schools_outside,
         abs_gap = abs(worst_gap)) %>%
  arrange(design)

stopifnot(nrow(score_wide) == length(DESIGN_LEVELS),
          !any(is.na(score_wide$mean_min)),
          !any(is.na(score_wide$gorard_catch)),
          # The identity asserted in SCORE_SPEC below, so that if a
          # future change to the curve breaks it the render stops rather
          # than the document quietly losing a measure.
          all(abs(score_wide$curve_gap - score_wide$gorard_catch) < 1e-9))

# metric | family | direction | how to print it
SCORE_SPEC <- tibble::tribble(
  ~metric,            ~family,        ~label,                                   ~better, ~fmt,
  # The curve's widest gap from the diagonal is NOT a second measure. On
  # catchments ordered by deprivation it is arithmetically identical to
  # Gorard - every design returns the same number to four decimals - so
  # listing both would count one measure twice and read as two measures
  # agreeing. Only Gorard is scored.
  "gorard_catch",     "Deprivation",  "Segregation between catchments",          "low",  "%.3f",
  "gorard_schools",   "Deprivation",  "Segregation between school intakes",      "low",  "%.3f",
  "mean_min",         "Journeys",     "Mean journey to the school offered",      "low",  "%.1f min",
  "p90_min",          "Journeys",     "Longest tenth of journeys, from",         "low",  "%.0f min",
  "over_long",        "Journeys",     "Children over 40 minutes",                "low",  "pct0",
  "mean_km",          "Journeys",     "Mean distance",                           "low",  "%.2f km",
  "child_km_day",     "Journeys",     "Child-kilometres a day, both ways",       "low",  "n0",
  "dep_gap",          "Journeys",     "Deprived children's journeys, against the rest", "low", "%+.1f min",
  "self_containment", "Fit",          "Self-containment",                        "high", "pct0",
  "abs_gap",          "Fit",          "Worst capacity gap",                      "low",  "pct0",
  "first_pref",       "Fit",          "Got their first preference",              "high", "pct0",
  "local_share",      "Fit",          "Placed in their own catchment",           "high", "pct0",
  "shape_faults",     "Fit",          "Shape faults",                            "low",  "%.0f",
  "moved",            "Fit",          "Neighbourhood children reassigned",       "low",  "pct0")

score_long <- score_wide %>%
  select(design, all_of(SCORE_SPEC$metric)) %>%
  tidyr::pivot_longer(-design, names_to = "metric", values_to = "value") %>%
  left_join(SCORE_SPEC, by = "metric") %>%
  group_by(metric) %>%
  mutate(rank = if (better[1] == "low") rank(value, ties.method = "min")
                else rank(-value, ties.method = "min"),
         best = rank == min(rank, na.rm = TRUE),
         # Against the map in force, with a tolerance: several of these
         # differ in the fourth decimal and a bare > would report a
         # float's last bit as a policy difference.
         vs_now = value - value[design == "Current catchments"],
         beats_now = dplyr::case_when(
           design == "Current catchments" ~ NA,
           abs(vs_now) <= 1e-6 * pmax(1, abs(value)) ~ NA,
           better[1] == "low" ~ vs_now < 0,
           TRUE ~ vs_now > 0)) %>%
  ungroup() %>%
  mutate(family = factor(family, c("Deprivation", "Journeys", "Fit")),
         label = factor(label, SCORE_SPEC$label))

message("\n=== Measures on which each design beats the current map ===")
print(as.data.frame(score_long %>% filter(!is.na(beats_now)) %>%
  group_by(design) %>%
  summarise(better = sum(beats_now), worse = sum(!beats_now),
            `same or n/a` = sum(is.na(beats_now)), .groups = "drop")),
  row.names = FALSE)

saveRDS(list(
  designs = designs, alloc = alloc, regions_sf = regions_sf,
  changed = changed, idaci_profiles = idaci_profiles, shape = shape,
  elm_sweep = elm_sweep %>% select(-assign, -regionalised, -world),
  protect_min = PROTECT_MIN,
  east_check = east_check,
  idaci_summary = idaci_summary, idaci_region = idaci_region,
  idaci_table = idaci_table, idaci_bands = idaci_bands,
  idaci_curve = idaci_curve, idaci_band_levels = IDACI_BANDS,
  idaci_lsoa = idaci_lsoa,
  journey_stats = journey_stats, journey_curve = journey_curve,
  intake_mix = intake_mix, intake_seg = intake_seg,
  score_wide = score_wide, score_long = score_long, score_spec = SCORE_SPEC,
  nts_min = NTS_MIN, long_min = LONG_MIN,
  regions = list(single = R_SINGLE, paired = R_PAIRED, elm = R_ELM,
                 elm210 = R_ELM210),
  now_assign = now_assign, pd_assign = pd_assign,
  groups_single = GROUPS_SINGLE, groups_paired = GROUPS_PAIRED,
  groups_now = GROUPS_NOW, lsoa_children = lsoa_children,
  worlds = list(now = W_NOW[c("label", "pan")], elm = W_ELM[c("label", "pan")]),
  elm_pan = mt$elm_pan, faith = FAITH, flow_model = "M2",
  run_at = Sys.time()), file.path(DATA, "flow_regions.rds"))

message("\nSaved data/flow_regions.rds")
