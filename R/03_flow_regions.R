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
# Two designs are produced: one region per school, and one that keeps
# the two paired catchments as pairs, because Stringer and Varndean are
# 470 metres apart and a boundary between them is close to meaningless.
#
# Faith schools are left out of the geography, as they are now. They
# admit across the city and giving them a catchment would be a change of
# policy rather than a change of map.
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

# ---- Children and flows, at whole-LSOA level -------------------------
# 30 of the 179 LSOAs are split across two current catchments, which is
# an artefact of the boundaries being replaced. Aggregating to whole
# LSOAs removes it and matches the unit the new design is built from.

city_zones <- oi$zones %>% filter(area != "Expansion area")

lsoa_children <- city_zones %>%
  group_by(lsoa) %>%
  summarise(Oi = sum(Oi), .groups = "drop")

# Capacity has to be measured against the children a catchment could
# actually be asked to place, and that is not all of them. The two faith
# schools admit across the city with no catchment at all and take about
# a fifth of the cohort, so an LSOA's demand on the geography is the
# flow it sends to schools that HAVE a geography. Comparing every child
# in a region against a target that excludes faith places made each
# region look 30 to 95 per cent over capacity by construction.

flows <- mt$od_flows %>%
  filter(model == FLOW_MODEL) %>%
  inner_join(city_zones %>% select(zone, lsoa), by = "zone") %>%
  group_by(lsoa, name) %>%
  summarise(flow = sum(flow), .groups = "drop")

cost <- oi$costs_now %>%
  inner_join(city_zones %>% select(zone, lsoa, Oi), by = "zone") %>%
  group_by(lsoa, name) %>%
  summarise(cij = weighted.mean(cij, Oi), .groups = "drop")

FAITH   <- oi$schools$name[oi$schools$faith]
CATCH_S <- setdiff(oi$schools$name[oi$schools$name %in% unique(flows$name)], FAITH)
pan     <- setNames(oi$schools$pan2026, oi$schools$name)

lsoa_demand <- flows %>%
  filter(name %in% CATCH_S) %>%
  group_by(lsoa) %>%
  summarise(demand = sum(flow), .groups = "drop") %>%
  right_join(lsoa_children, by = "lsoa") %>%
  mutate(demand = coalesce(demand, 0))

message(sprintf("  of %s children, %s have a school with a catchment as their modelled destination (%.0f%%)",
                format(round(sum(lsoa_demand$Oi)), big.mark = ","),
                format(round(sum(lsoa_demand$demand)), big.mark = ","),
                100 * sum(lsoa_demand$demand) / sum(lsoa_demand$Oi)))

message(sprintf("  %d LSOAs, %s children, %d schools with a geography (%d faith excluded)",
                nrow(lsoa_children),
                format(round(sum(lsoa_children$Oi)), big.mark = ","),
                length(CATCH_S), length(FAITH)))

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

# ---- 1. Dominant flow ------------------------------------------------

dominant <- flows %>%
  filter(name %in% CATCH_S) %>%
  group_by(lsoa) %>%
  slice_max(flow, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(lsoa, region = name, dom_flow = flow)

stopifnot(nrow(dominant) == nrow(lsoa_children))
message(sprintf("  dominant flow assigns %d LSOAs to %d regions",
                nrow(dominant), n_distinct(dominant$region)))

# ---- 2. Contiguity repair -------------------------------------------
# Depth-first search over the adjacency graph, per region. Everything
# outside the largest connected component is a fragment; each fragment
# moves whole to whichever adjacent region it sends most flow to, and
# the pass repeats until nothing moves.

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

flow_to <- function(ls, reg) {
  f <- flows$flow[flows$lsoa == ls & flows$name == reg]
  if (length(f)) sum(f) else 0
}

repair <- function(assign) {
  for (pass in 1:20) {
    moved <- 0
    for (reg in unique(assign$region)) {
      mem <- assign$lsoa[assign$region == reg]
      cc  <- components(mem)
      if (length(cc) <= 1) next
      main <- cc[[which.max(sapply(cc, length))]]
      for (frag in cc[!sapply(cc, identical, main)]) {
        adj <- setdiff(unique(unlist(NB[frag])), frag)
        cand <- setdiff(unique(assign$region[assign$lsoa %in% adj]), reg)
        if (!length(cand)) next
        score <- sapply(cand, function(r) sum(sapply(frag, flow_to, reg = r)))
        assign$region[assign$lsoa %in% frag] <- cand[which.max(score)]
        moved <- moved + length(frag)
      }
    }
    if (!moved) break
  }
  assign
}

assign1 <- repair(dominant %>% select(lsoa, region))
message(sprintf("  after contiguity repair: %d regions",
                n_distinct(assign1$region)))

# ---- 3. Capacity balance --------------------------------------------
# Targets are proportional to places. Not every child is the geography's
# to place: the faith schools take about a fifth of the city across no
# catchment at all, so the target is scaled to the children the model
# actually sends to schools that do have one.

balance <- function(assign, groups, tol = 0.05, max_moves = 400) {
  places <- sapply(groups, function(g) sum(pan[g]))
  placed <- sum(flows$flow[flows$name %in% unlist(groups)])
  target <- places / sum(places) * placed

  sch_of <- setNames(rep(names(groups), lengths(groups)), unlist(groups))
  assign$grp <- unname(sch_of[assign$region])

  size <- function(a) {
    s <- a %>% left_join(lsoa_demand, by = "lsoa") %>%
      group_by(grp) %>% summarise(n = sum(demand), .groups = "drop")
    setNames(s$n, s$grp)[names(groups)]
  }

  # Each move takes one boundary LSOA from an over-target region and
  # gives it to an under-target neighbour. Two things this has to get
  # right, both learned the hard way:
  #
  #   * try EVERY over/under pair, not just the worst of each. The most
  #     over-subscribed region is often not adjacent to the most under-
  #     subscribed one, and stopping there ended the search after twenty
  #     moves with a region still 46% out.
  #   * try every candidate LSOA in cost order, not just the cheapest,
  #     because the cheapest may be the one holding the region together.
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
        # Cost of a move: the flow the LSOA sends to the donor's schools
        # less what it already sends to the receiver's.
        cost_l <- sapply(cand, function(l)
          sum(sapply(groups[[from]], flow_to, ls = l)) -
          sum(sapply(groups[[to]],   flow_to, ls = l)))
        for (pick in cand[order(cost_l)]) {
          trial <- assign
          trial$grp[trial$lsoa == pick] <- to
          trial$region[trial$lsoa == pick] <- groups[[to]][1]
          # Never break contiguity to fix capacity.
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
  list(assign = assign, target = target, moves = m,
       final = tibble(grp = names(target), demand = unname(cur[names(target)]),
                      target = unname(target),
                      places = unname(sapply(groups, function(g) sum(pan[g])))) %>%
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

message("\n  Balancing, one region per school...")
b_single <- balance(assign1, GROUPS_SINGLE)
message(sprintf("    %d moves", b_single$moves))

message("  Balancing, paired catchments kept...")
b_paired <- balance(assign1, GROUPS_PAIRED)
message(sprintf("    %d moves", b_paired$moves))

# ---- 4. Diagnostics --------------------------------------------------
# Self-containment is the travel-to-work-area statistic: the share of a
# region's children whose modelled first choice lies inside it. The
# current map is scored the same way so the two are comparable.

first_choice <- flows %>%
  group_by(lsoa) %>%
  slice_max(flow, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(lsoa, top_school = name)

score_design <- function(assign, groups, label, target = NULL) {
  sch_of <- setNames(rep(names(groups), lengths(groups)), unlist(groups))
  a <- assign %>%
    mutate(grp = unname(sch_of[region])) %>%
    left_join(lsoa_children, by = "lsoa") %>%
    left_join(first_choice, by = "lsoa") %>%
    mutate(top_grp = unname(sch_of[top_school]),
           contained = !is.na(top_grp) & top_grp == grp)

  # Demand on the geography, against the region's fair share of the
  # places in schools that have one.
  dem <- a %>% left_join(lsoa_demand %>% select(lsoa, demand), by = "lsoa")
  kids <- tapply(dem$demand, dem$grp, sum)
  if (is.null(target)) {
    places <- sapply(groups, function(g) sum(pan[g]))
    target <- places / sum(places) * sum(flows$flow[flows$name %in% unlist(groups)])
  }
  gap <- kids[names(target)] / target - 1

  jt <- a %>%
    left_join(cost, by = "lsoa", relationship = "many-to-many") %>%
    filter(name %in% unlist(groups)) %>%
    mutate(in_grp = unname(sch_of[name]) == grp) %>%
    filter(in_grp) %>%
    group_by(lsoa) %>% slice_min(cij, n = 1, with_ties = FALSE) %>% ungroup()

  idaci <- a %>%
    left_join(dep$idaci %>% select(lsoa, idaci_score), by = "lsoa")

  idaci_by <- tapply(idaci$idaci_score * idaci$Oi, idaci$grp, sum, na.rm = TRUE) /
    tapply(idaci$Oi, idaci$grp, sum)

  tibble(design = label,
         regions = n_distinct(a$grp),
         self_containment = sum(a$Oi[a$contained]) / sum(a$Oi),
         mean_journey = weighted.mean(jt$cij, jt$Oi),
         worst_gap = max(abs(gap), na.rm = TRUE),
         idaci_lo = min(idaci_by), idaci_hi = max(idaci_by),
         idaci_range = diff(range(idaci_by)))
}

GROUPS_NOW <- GROUPS_PAIRED
stopifnot(identical(sort(names(GROUPS_NOW)), sort(names(GROUPS_PAIRED))))

now_assign <- city_zones %>%
  group_by(lsoa) %>%
  slice_max(Oi, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(lsoa, region = purrr::map_chr(catchment, function(cc)
    GROUPS_NOW[[cc]][1] %||% NA_character_)) %>%
  filter(!is.na(region))

designs <- bind_rows(
  score_design(now_assign,      GROUPS_NOW,    "Current catchments"),
  score_design(b_single$assign, GROUPS_SINGLE, "Flow regions, one per school",
               b_single$target),
  score_design(b_paired$assign, GROUPS_PAIRED, "Flow regions, pairs kept",
               b_paired$target))

message("\n=== The designs compared ===")
print(as.data.frame(designs %>%
  transmute(Design = design, Regions = regions,
            `Self-containment` = sprintf("%.0f%%", 100 * self_containment),
            `Mean journey` = sprintf("%.1f min", mean_journey),
            `Worst capacity gap` = sprintf("%+.0f%%", 100 * worst_gap),
            `IDACI, least to most deprived region` =
              sprintf("%.3f to %.3f", idaci_lo, idaci_hi))), row.names = FALSE)

message("\n=== Capacity, region by region ===")
for (d in list(list(b_single, "one per school"), list(b_paired, "pairs kept"))) {
  message("  ", d[[2]], ":")
  print(as.data.frame(d[[1]]$final %>%
    arrange(desc(gap)) %>%
    transmute(Region = grp, Demand = round(demand), Target = round(target),
              Places = places, Gap = sprintf("%+.0f%%", 100 * gap))),
    row.names = FALSE)
}

# ---- 5. Allocation under a distance-based over-subscription rule ----
# A catchment map is only half a policy; the other half is what happens
# when a school is over-subscribed. This runs the rule England actually
# uses, in the simplified form published data supports:
#
#   * every child has a preference order over all ten schools, taken
#     from the model's own utility W_j c_ij^-beta, so it is the same
#     ordering the flows come from
#   * an over-subscribed school admits in-catchment children first, and
#     within each priority group the nearest first
#   * children who miss out cascade to their next preference
#
# It is deferred acceptance: schools hold offers provisionally and can
# bump a held child when a higher-priority applicant arrives, which is
# how the coordinated scheme behaves. The faith schools admit on faith
# criteria this cannot see, so they are given no catchment and rank
# purely on distance.
#
# The point of the exercise is the cross-catchment share. A rule that
# admitted everybody locally would be a zoning system, not a choice
# system; what a good design should produce is most children local and
# a real minority crossing.

util <- cost %>%
  left_join(oi$attract %>% select(name, W = W_wprefs), by = "name") %>%
  filter(is.finite(cij), cij > 0) %>%
  mutate(u = W * cij^(-BETA))

allocate <- function(assign, groups, tol = 1e-3, max_round = 60) {
  sch_of <- setNames(rep(names(groups), lengths(groups)), unlist(groups))
  home   <- setNames(assign$region, assign$lsoa)
  home_g <- unname(sch_of[home[assign$lsoa]]); names(home_g) <- assign$lsoa

  prefs <- util %>%
    filter(lsoa %in% assign$lsoa) %>%
    mutate(grp = unname(sch_of[name]),
           in_catch = !is.na(grp) & grp == unname(home_g[lsoa])) %>%
    group_by(lsoa) %>% arrange(desc(u), .by_group = TRUE) %>%
    mutate(rank = row_number()) %>% ungroup()

  cap  <- pan[unique(prefs$name)]
  # held: what each school is currently holding, by LSOA
  held <- prefs[0, c("lsoa", "name")] %>% mutate(n = numeric(0))
  # next preference each LSOA's unplaced children will try
  nxt  <- setNames(rep(1L, nrow(assign)), assign$lsoa)
  left <- setNames(lsoa_demand$Oi[match(assign$lsoa, lsoa_demand$lsoa)],
                   assign$lsoa)

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

    # In-catchment first, then nearest first, then admit up to the PAN.
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
    # An LSOA that was rejected moves down its list; one that has run out
    # of preferences stops trying.
    moved <- back$lsoa[back$rej > tol]
    nxt[moved] <- nxt[moved] + 1L
    exhausted <- moved[nxt[moved] > 10]
    left[exhausted] <- 0
    if (all(left <= tol)) break
  }

  res <- held %>%
    mutate(grp = unname(sch_of[name]),
           home = unname(home_g[lsoa]),
           cross = is.na(grp) | is.na(home) | grp != home) %>%
    left_join(prefs %>% select(lsoa, name, cij, rank), by = c("lsoa", "name"))

  tibble(placed = sum(res$n),
         cross_share = sum(res$n[res$cross]) / sum(res$n),
         faith_share = sum(res$n[res$name %in% FAITH]) / sum(res$n),
         local_share = sum(res$n[!res$cross & !res$name %in% FAITH]) / sum(res$n),
         first_pref = sum(res$n[res$rank == 1]) / sum(res$n),
         mean_journey = weighted.mean(res$cij, res$n),
         rounds = r)
}

message("\n=== Allocation under an in-catchment-then-distance rule ===")
alloc <- bind_rows(
  allocate(now_assign,      GROUPS_NOW)    %>% mutate(design = "Current catchments"),
  allocate(b_single$assign, GROUPS_SINGLE) %>% mutate(design = "Flow regions, one per school"),
  allocate(b_paired$assign, GROUPS_PAIRED) %>% mutate(design = "Flow regions, pairs kept"))

print(as.data.frame(alloc %>%
  transmute(Design = design,
            Placed = round(placed),
            `Got 1st preference` = sprintf("%.0f%%", 100 * first_pref),
            `To their own catchment` = sprintf("%.0f%%", 100 * local_share),
            `Crossed a boundary` = sprintf("%.0f%%", 100 * cross_share),
            `of which faith` = sprintf("%.0f%%", 100 * faith_share),
            `Mean journey` = sprintf("%.1f min", mean_journey))),
  row.names = FALSE)

# ---- 6. The regions as polygons -------------------------------------

dissolve <- function(assign, groups, label) {
  sch_of <- setNames(rep(names(groups), lengths(groups)), unlist(groups))
  geom %>%
    inner_join(assign %>% mutate(grp = unname(sch_of[region])),
               by = c("lsoa21cd" = "lsoa")) %>%
    group_by(grp) %>%
    summarise(lsoas = n(), .groups = "drop") %>%
    # A dissolve leaves hairline slivers where neighbours were digitised
    # slightly apart; a zero-width buffer closes them.
    st_buffer(0) %>%
    mutate(design = label) %>%
    st_transform(4326)
}

regions_sf <- bind_rows(
  dissolve(now_assign,      GROUPS_NOW,    "Current catchments"),
  dissolve(b_single$assign, GROUPS_SINGLE, "Flow regions, one per school"),
  dissolve(b_paired$assign, GROUPS_PAIRED, "Flow regions, pairs kept"))

# Which LSOAs change hands, against the current map.
sch_now <- setNames(rep(names(GROUPS_NOW), lengths(GROUPS_NOW)), unlist(GROUPS_NOW))
sch_new <- setNames(rep(names(GROUPS_PAIRED), lengths(GROUPS_PAIRED)),
                    unlist(GROUPS_PAIRED))
changed <- now_assign %>%
  transmute(lsoa, now = unname(sch_now[region])) %>%
  inner_join(b_paired$assign %>%
               transmute(lsoa, new = unname(sch_new[region])), by = "lsoa") %>%
  left_join(lsoa_children, by = "lsoa") %>%
  mutate(moved = now != new)

message(sprintf("\n  Against the current map, the paired design moves %d of %d LSOAs (%s children, %.0f%%)",
                sum(changed$moved), nrow(changed),
                format(round(sum(changed$Oi[changed$moved])), big.mark = ","),
                100 * sum(changed$Oi[changed$moved]) / sum(changed$Oi)))
print(as.data.frame(changed %>% filter(moved) %>%
  count(now, new, wt = Oi, name = "children") %>%
  arrange(desc(children)) %>%
  mutate(children = round(children)) %>% head(8)), row.names = FALSE)

saveRDS(list(
  single = b_single, paired = b_paired, alloc = alloc,
  regions_sf = regions_sf, changed = changed, lsoa_demand = lsoa_demand,
  groups_single = GROUPS_SINGLE, groups_paired = GROUPS_PAIRED,
  groups_now = GROUPS_NOW, now_assign = now_assign,
  designs = designs, dominant = dominant, repaired = assign1,
  flows = flows, cost = cost, lsoa_children = lsoa_children,
  flow_model = FLOW_MODEL, faith = FAITH,
  run_at = Sys.time()), file.path(DATA, "flow_regions.rds"))

message("\nSaved data/flow_regions.rds")
