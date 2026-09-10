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
    summarise(cij = weighted.mean(cij, Oi), .groups = "drop")

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

W_NOW <- world("M2",  oi$costs_now, "Ovingdean, PAN 210")
W_ELM <- world("ELM", oi$costs_elm, sprintf("Elm Grove, PAN %d", mt$elm_pan),
               lh_pan = mt$elm_pan)

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

regionalise <- function(w, groups, tol = 0.05, max_moves = 400) {

  flow_to <- function(ls, reg) {
    f <- w$flows$flow[w$flows$lsoa == ls & w$flows$name == reg]
    if (length(f)) sum(f) else 0
  }

  # 1. Dominant flow
  assign <- w$flows %>%
    filter(name %in% CATCH_S) %>%
    group_by(lsoa) %>%
    slice_max(flow, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(lsoa, region = name)
  stopifnot(nrow(assign) == nrow(lsoa_children))

  # 2. Contiguity repair. Everything outside a region's largest
  # connected component is a fragment, and each fragment moves whole to
  # whichever adjacent region it sends most flow to.
  for (pass in 1:20) {
    moved <- 0
    for (reg in unique(assign$region)) {
      mem <- assign$lsoa[assign$region == reg]
      cc  <- components(mem)
      if (length(cc) <= 1) next
      main <- cc[[which.max(sapply(cc, length))]]
      for (frag in cc[!sapply(cc, identical, main)]) {
        adj  <- setdiff(unique(unlist(NB[frag])), frag)
        cand <- setdiff(unique(assign$region[assign$lsoa %in% adj]), reg)
        if (!length(cand)) next
        score <- sapply(cand, function(r) sum(sapply(frag, flow_to, reg = r)))
        assign$region[assign$lsoa %in% frag] <- cand[which.max(score)]
        moved <- moved + length(frag)
      }
    }
    if (!moved) break
  }
  dominant <- assign

  # 3. Capacity balance. Each move takes one boundary LSOA from an
  # over-target region and gives it to an under-target neighbour. Two
  # things this has to get right, both learned the hard way:
  #   * try EVERY over/under pair, not just the worst of each. The most
  #     over-subscribed region is often not adjacent to the most under-
  #     subscribed one, and stopping there ended the search early with a
  #     region still 46% out.
  #   * try every candidate LSOA in cost order, not just the cheapest,
  #     because the cheapest may be the one holding the region together.
  places <- sapply(groups, function(g) sum(w$pan[g]))
  placed <- sum(w$flows$flow[w$flows$name %in% unlist(groups)])
  target <- places / sum(places) * placed

  sch_of <- setNames(rep(names(groups), lengths(groups)), unlist(groups))
  assign$grp <- unname(sch_of[assign$region])

  size <- function(a) {
    s <- a %>% left_join(w$demand, by = "lsoa") %>%
      group_by(grp) %>% summarise(n = sum(demand), .groups = "drop")
    setNames(s$n, s$grp)[names(groups)]
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
        cand <- assign$lsoa[assign$grp == from]
        cand <- cand[sapply(cand, function(l)
          any(assign$grp[match(NB[[l]], assign$lsoa)] == to, na.rm = TRUE))]
        if (!length(cand)) next
        cost_l <- sapply(cand, function(l)
          sum(sapply(groups[[from]], flow_to, ls = l)) -
          sum(sapply(groups[[to]],   flow_to, ls = l)))
        for (pick in cand[order(cost_l)]) {
          trial <- assign
          trial$grp[trial$lsoa == pick]    <- to
          trial$region[trial$lsoa == pick] <- groups[[to]][1]
          if (all(sapply(c(from, to), function(g)
            length(components(trial$lsoa[trial$grp == g])) == 1))) {
            assign <- trial; done <- TRUE; break
          }
        }
        if (done) break
      }
      if (done) break
    }
    if (!done) break
  }

  cur <- size(assign)
  list(assign = assign %>% select(lsoa, region), dominant = dominant,
       groups = groups, target = target, moves = m, world = w$label,
       final = tibble(grp = names(target), demand = unname(cur[names(target)]),
                      target = unname(target),
                      places = unname(places)) %>%
         mutate(gap = demand / target - 1))
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
for (r in list(R_SINGLE, R_PAIRED, R_ELM))
  message(sprintf("    %-28s %d moves, worst gap %+.0f%%",
                  r$world, r$moves, 100 * max(abs(r$final$gap))))

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
  list(a = pd_assign,        g = GROUPS_NOW,    w = W_NOW, t = NULL,
       lab = "Power diagram (proximity and capacity)"),
  list(a = R_SINGLE$assign,  g = GROUPS_SINGLE, w = W_NOW, t = R_SINGLE$target,
       lab = "Flow regions, one per school"),
  list(a = R_PAIRED$assign,  g = GROUPS_PAIRED, w = W_NOW, t = R_PAIRED$target,
       lab = "Flow regions, pairs kept"),
  list(a = R_ELM$assign,     g = GROUPS_PAIRED, w = W_ELM, t = R_ELM$target,
       lab = "Flow regions, Longhill at Elm Grove"))

designs <- purrr::map_dfr(DESIGNS, ~ score_design(.x$a, .x$g, .x$lab, .x$w, .x$t))

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
    left_join(prefs %>% select(lsoa, name, cij, rank), by = c("lsoa", "name"))

  tibble(placed = sum(res$n),
         cross_share = sum(res$n[res$cross]) / sum(res$n),
         faith_share = sum(res$n[res$name %in% FAITH]) / sum(res$n),
         local_share = sum(res$n[!res$cross & !res$name %in% FAITH]) / sum(res$n),
         first_pref = sum(res$n[res$rank == 1]) / sum(res$n),
         mean_journey = weighted.mean(res$cij, res$n), rounds = r)
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

saveRDS(list(
  designs = designs, alloc = alloc, regions_sf = regions_sf,
  changed = changed, idaci_profiles = idaci_profiles,
  idaci_summary = idaci_summary, idaci_region = idaci_region,
  idaci_table = idaci_table,
  regions = list(single = R_SINGLE, paired = R_PAIRED, elm = R_ELM),
  now_assign = now_assign, pd_assign = pd_assign,
  groups_single = GROUPS_SINGLE, groups_paired = GROUPS_PAIRED,
  groups_now = GROUPS_NOW, lsoa_children = lsoa_children,
  worlds = list(now = W_NOW[c("label", "pan")], elm = W_ELM[c("label", "pan")]),
  elm_pan = mt$elm_pan, faith = FAITH, flow_model = "M2",
  run_at = Sys.time()), file.path(DATA, "flow_regions.rds"))

message("\nSaved data/flow_regions.rds")
