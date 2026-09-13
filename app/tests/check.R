# app/tests/check.R — does the app's model agree with the document's?
# ======================================================================
# The app reimplements the spatial interaction model so it can run on
# arbitrary attractiveness and admission numbers. A reimplementation
# that quietly disagrees with the published one would make every number
# in the app wrong in a way nobody would notice, so it is checked
# against the figures section 7 publishes.
#
#   Rscript app/tests/check.R
#
# Exits non-zero on disagreement.
#
# It lives in tests/ rather than R/ because Shiny sources every file in
# an app's R/ directory at startup. Sitting there, this ran on each
# cold start, and a failure would have called quit() on the app rather
# than reported anything - so the symptom of a broken model would have
# been an app that simply would not open.
# ======================================================================

suppressPackageStartupMessages({ library(dplyr) })
ROOT <- here::here()
source(file.path(ROOT, "app", "R", "model.R"))
source(file.path(ROOT, "app", "R", "outcomes.R"))

inp <- readRDS(file.path(ROOT, "app", "data", "sim_inputs.rds"))
mt  <- readRDS(file.path(ROOT, "data", "model_terms.rds"))

fail <- character(0)
note <- function(...) message("  ", ...)

# ---- M4, the full model, at the published settings -------------------
r <- run_sim(inp, site = "now", design = "Current catchments",
             year = 2026, capped = TRUE)

pub <- mt$runs[["M4"]] %>% select(name, published = modelled)
cmp <- r$schools %>%
  filter(city | name %in% inp$out_of_city) %>%
  select(name, app = intake) %>%
  inner_join(pub, by = "name") %>%
  mutate(diff = app - published)

note(sprintf("M4: %d schools compared, largest difference %.2f children",
             nrow(cmp), max(abs(cmp$diff))))
print(as.data.frame(cmp %>% arrange(desc(abs(diff))) %>%
  transmute(name = substr(name, 1, 30), app = round(app, 1),
            published = round(published, 1), diff = round(diff, 2))),
  row.names = FALSE)

if (nrow(cmp) < 10)
  fail <- c(fail, sprintf("only %d schools matched the published run", nrow(cmp)))
# A tenth of a child is well inside the tolerance of the capacity
# balancer; anything larger means the two models differ.
if (max(abs(cmp$diff)) > 0.5)
  fail <- c(fail, sprintf("M4 differs from the published run by up to %.2f children",
                          max(abs(cmp$diff))))

# ---- The constraints the model claims to satisfy ---------------------
o <- r$flows %>% group_by(zone) %>%
  summarise(sent = sum(flow), .groups = "drop") %>%
  inner_join(inp$zones %>% select(zone, Oi), by = "zone")
over <- r$schools %>% filter(intake - pan > 1e-6)

note(sprintf("origins: largest shortfall %.3f children; schools over their PAN: %d",
             max(abs(o$sent - o$Oi)), nrow(over)))
if (max(abs(o$sent - o$Oi)) > 1) fail <- c(fail, "children are being lost or invented")
if (nrow(over)) fail <- c(fail, "a school is over its admission number")

# ---- The outcome measures are finite and in range --------------------
m <- outcomes(inp, r)
note(sprintf("outcomes: fill %.0f%%, below PAN %d, Gorard %.3f, mean journey %.1f min, gap %s",
             100 * m$fill, m$below_pan, m$gorard, m$mean_min,
             gbp_app(m$city_gap)))
if (!is.finite(m$gorard) || m$gorard < 0 || m$gorard > 1)
  fail <- c(fail, "Gorard index is out of range")
if (!is.finite(m$mean_min) || m$mean_min <= 0)
  fail <- c(fail, "mean journey is not a positive number")

# ---- Displacement adds up -------------------------------------------
# Every child living in a catchment ends up in exactly one of the three
# buckets, so the shares must sum to one for every catchment, and the
# children living in them must sum to the cohort.
cd <- m$catchment
sums <- cd$by_catch %>% group_by(home) %>% summarise(s = sum(share), .groups = "drop")
note(sprintf("catchments: outside %.0f%% = chose %.0f%% + displaced %.0f%%; shares sum to %.3f-%.3f",
             100 * cd$outside_share, 100 * cd$chose_share,
             100 * cd$displaced_share, min(sums$s), max(sums$s)))
if (max(abs(sums$s - 1)) > 1e-6)
  fail <- c(fail, "a catchment's three shares do not sum to one")
if (abs(sum(cd$outside$living) - m$intake) > 1)
  fail <- c(fail, "the children living in the catchments do not sum to the cohort")
if (abs(cd$chose_share + cd$p6_share + cd$displaced_share - cd$outside_share) > 1e-9)
  fail <- c(fail, "choosing and displacement do not add up to being outside")

# A catchment whose schools have room cannot displace anybody. Longhill's
# do, by a wide margin, so its displacement must be zero - and that is the
# distinction the chart exists to draw.
lh_disp <- cd$outside$displaced[grepl("Longhill", cd$outside$label)]
note(sprintf("Longhill catchment: %.0f%% outside, %.1f displaced",
             100 * cd$outside$outside_share[grepl("Longhill", cd$outside$label)],
             lh_disp))
if (length(lh_disp) != 1 || lh_disp > 1)
  fail <- c(fail, "children are being displaced from a catchment with spare places")

# Turning the catchment up keeps more children local overall, and turns
# what is left from choosing into displacement.
tight <- outcomes(inp, run_sim(inp, site = "now", design = "Current catchments",
                               year = 2026, gamma = 3))$catchment
note(sprintf("catchment strength 3: outside %.0f%%, of which displaced %.0f%%",
             100 * tight$outside_share, 100 * tight$displaced_share))
if (tight$outside_share >= cd$outside_share)
  fail <- c(fail, "a stronger catchment does not keep more children local")
if (tight$displaced_share <= cd$displaced_share)
  fail <- c(fail, "a stronger catchment does not push more children into displacement")

# ---- Each preset runs -------------------------------------------------
for (p in names(inp$presets)) {
  s <- inp$presets[[p]]
  ok <- tryCatch({
    rr <- run_sim(inp, w_mult = s$w, pans = s$pan, site = s$site,
                  design = s$design, year = s$year, gamma = s$gamma,
                  rules = if (is.null(s$rule)) NULL else
                    list(rule = s$rule, p6_share = (s$p6 %||% 5) / 100,
                         fsm = s$fsm %||% TRUE, targeted = s$targeted %||% FALSE))
    mm <- outcomes(inp, rr)
    is.finite(mm$gorard) && is.finite(mm$city_gap)
  }, error = function(e) { note("preset '", p, "' failed: ", conditionMessage(e)); FALSE })
  if (!ok) fail <- c(fail, sprintf("preset '%s' does not run", p))
}
note(sprintf("presets: %d checked", length(inp$presets)))

# ---- The inverse solver lands on the target --------------------------
lh <- "Longhill High School"
sol <- solve_w_for_pan(inp, lh, target_fill = 1,
                       site = "now", design = "Current catchments", year = 2026)
if (is.finite(sol$multiplier)) {
  chk <- run_sim(inp, w_mult = setNames(sol$multiplier, lh),
                 site = "now", design = "Current catchments", year = 2026)
  got <- chk$schools$fill[chk$schools$name == lh]
  note(sprintf("solver: Longhill needs %.1fx its attractiveness to fill; check gives %.3f",
               sol$multiplier, got))
  if (got < 0.995)
    fail <- c(fail, sprintf("the solver's answer only fills to %.3f", got))
} else {
  note("solver: Longhill cannot reach its admission number at any attractiveness")
}

# ---- The solver as the APP calls it ----------------------------------
# The app passes the whole slider vector, which is the path that broke:
# w_mult was reaching run_sim twice, once through the dots and once from
# inside the solver. The version without w_mult kept working, so the
# check above never saw it.
w_all <- setNames(rep(1, length(inp$city)), inp$city)
w_all[["Varndean School"]] <- 0.5      # a slider the user has moved
sol2 <- tryCatch(
  solve_w_for_pan(inp, lh, target_fill = 1, w_mult = w_all, pans = NULL,
                  site = "now", design = "Current catchments", year = 2026),
  error = function(e) { note("solver with sliders failed: ", conditionMessage(e)); NULL })

if (is.null(sol2)) {
  fail <- c(fail, "the solver errors when given the slider vector the app passes")
} else if (is.finite(sol2$multiplier)) {
  chk2 <- run_sim(inp, w_mult = replace(w_all, lh, sol2$multiplier),
                  site = "now", design = "Current catchments", year = 2026)
  got2 <- chk2$schools$fill[chk2$schools$name == lh]
  note(sprintf("solver with Varndean at 0.5x: Longhill needs %.1fx, check gives %.3f",
               sol2$multiplier, got2))
  if (got2 < 0.995)
    fail <- c(fail, "the solver's answer does not fill when the other sliders are held")
  # Weakening a rival should make the job easier, not harder.
  if (sol2$multiplier > sol$multiplier + 1e-6)
    fail <- c(fail, "the solver ignores the other schools' sliders")
}

# ---- The answer must not depend on the target's own slider -----------
# It did. The capacity balancer converges to a tolerance, so a full
# school reads 0.99997 and a test against exactly 1 kept the bisection
# climbing - by an amount that depended on where it started. The same
# question answered 4.2x from one slider position and 5.1x from another.
inv <- vapply(c(1, 2, 3.5), function(v) {
  w <- setNames(rep(1, length(inp$city)), inp$city); w[[lh]] <- v
  solve_w_for_pan(inp, lh, target_fill = 1, w_mult = w, site = "now",
                  design = "Current catchments", year = 2026)$multiplier
}, numeric(1))
note(sprintf("solver invariance to its own slider: %s",
             paste(sprintf("%.2f", inv), collapse = ", ")))
if (diff(range(inv)) > 0.02)
  fail <- c(fail, sprintf("the answer moves with the target's own slider: %s",
                          paste(round(inv, 2), collapse = ", ")))

# ---- Attainment 8 and absence invert consistently --------------------
pts <- att8_points(inp, 4)
note(sprintf("attainment: 4x is %+.1f points; a point is worth %.1f%%",
             pts, 100 * inp$attain$per_point))
if (abs(att8_multiplier(inp, pts) - 4) > 1e-6)
  fail <- c(fail, "the Attainment 8 conversion does not round-trip")

ab <- absence_for(inp, 14.7, 37.2, 37.2 + pts)
note(sprintf("absence: +%.1f points takes 14.7%% to %.1f%% (%.0fth percentile)",
             pts, ab, absence_percentile(inp, ab)))
if (!(ab > 0 && ab < 14.7))
  fail <- c(fail, "raising attainment does not lower the implied absence rate")

# ---- The council's priorities ----------------------------------------
# The tiered ceiling. FSM take-up is calibrated so the FSM-priority offers
# at the three rationing schools match the September 2026 total, so that
# total must come back. Everything else is an invariant, or a comparison
# with published offers that nothing was fitted to.
RATION <- inp$rules$rationing
pr <- function(...) utils::modifyList(
  list(rule = "priorities", p6_share = 0.05, fsm = TRUE, targeted = FALSE),
  list(...))
rp <- run_sim(inp, site = "now", design = "Current catchments", year = 2026,
              rules = pr())
tp <- rp$tiers
out <- inp$rules$outturn_2026

fsm_m <- sum((tp$p45_in + tp$p45_out)[tp$name %in% RATION])
fsm_p <- sum(out$offers[out$priority %in% 4:5])
note(sprintf("priorities: FSM places at the three rationing schools %.1f, published %d (calibrated)",
             fsm_m, fsm_p))
if (abs(fsm_m - fsm_p) > 1)
  fail <- c(fail, "FSM take-up no longer reproduces the published FSM offers")

cmp <- data.frame(
  school = substr(RATION, 1, 24),
  fsm_model = round((tp$p45_in + tp$p45_out)[match(RATION, tp$name)], 1),
  fsm_pub = vapply(RATION, function(s) sum(out$offers[out$school == s & out$priority %in% 4:5]), numeric(1)),
  p6_model = round(tp$p6[match(RATION, tp$name)], 1),
  p6_pub = vapply(RATION, function(s) sum(out$offers[out$school == s & out$priority == 6]), numeric(1)),
  row.names = NULL)
print(cmp, row.names = FALSE)

o2 <- rp$flows %>% group_by(zone) %>% summarise(s = sum(flow), .groups = "drop") %>%
  inner_join(inp$zones %>% select(zone, Oi), by = "zone")
if (max(abs(o2$s - o2$Oi)) > 1)
  fail <- c(fail, "priorities: children are being lost or invented")
if (any(rp$schools$intake - rp$schools$pan > 1e-3))
  fail <- c(fail, "priorities: a school is over its admission number")
if (any(tp$p6 > 0.05 * rp$cap[tp$name] + 1e-6))
  fail <- c(fail, "priority 6 exceeds its share of the admission number")
if (any(tp$p45_in + tp$p45_out > 0.30 * rp$cap[tp$name] + 1e-6))
  fail <- c(fail, "the FSM priority exceeds 30% of the admission number")
if (sum(rp$flows$p6[!rp$flows$name %in% inp$rules$community]) > 1e-9)
  fail <- c(fail, "priority 6 places children at a school it does not apply to")

cp <- outcomes(inp, rp)$catchment
if (abs(cp$chose_share + cp$p6_share + cp$displaced_share - cp$outside_share) > 1e-9)
  fail <- c(fail, "priorities: the catchment buckets do not add up")

s20 <- run_sim(inp, year = 2026, rules = pr(p6_share = 0.20))
c20 <- outcomes(inp, s20)$catchment
note(sprintf("priority 6 at 5%%: %.1f places, %.2f%% displaced; at 20%%: %.1f places, %.2f%% displaced",
             sum(tp$p6), 100 * cp$displaced_share,
             sum(s20$tiers$p6), 100 * c20$displaced_share))
if (sum(s20$tiers$p6) < sum(tp$p6) - 1e-6)
  fail <- c(fail, "a larger priority-6 share places fewer children under it")
if (c20$displaced_share < cp$displaced_share - 1e-6)
  fail <- c(fail, "a larger priority-6 share displaces fewer catchment children")

tg <- run_sim(inp, year = 2026, rules = pr(targeted = TRUE))
f_all <- sum(tp$p45_in + tp$p45_out); f_tg <- sum(tg$tiers$p45_in + tg$tiers$p45_out)
note(sprintf("Targeted FSM: %.1f FSM places at the community schools, against %.1f", f_tg, f_all))
if (f_tg >= f_all)
  fail <- c(fail, "narrowing FSM eligibility does not shrink the FSM priority")

off <- run_sim(inp, year = 2026, rules = pr(p6_share = 0, fsm = FALSE))
if (sum(off$flows$p6) > 1e-9 || sum(off$tiers$p45_in + off$tiers$p45_out) > 1e-9)
  fail <- c(fail, "switching priorities 4-6 off leaves places under them")

# Priority 6 belongs to single-school catchments under any map.
one <- run_sim(inp, year = 2026, design = "Flow regions, one per school", rules = pr())
note(sprintf("one region per school: %.1f priority-6 places at the community schools",
             sum(one$tiers$p6)))

if (length(fail)) {
  message("\nFAILED:")
  for (f in fail) message("  - ", f)
  quit(status = 1)
}
message("\nThe app's model agrees with the published one.")
