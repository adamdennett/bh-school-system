# app/R/check.R — does the app's model agree with the document's?
# ======================================================================
# The app reimplements the spatial interaction model so it can run on
# arbitrary attractiveness and admission numbers. A reimplementation
# that quietly disagrees with the published one would make every number
# in the app wrong in a way nobody would notice, so it is checked
# against the figures section 7 publishes.
#
#   Rscript app/R/check.R
#
# Exits non-zero on disagreement.
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
note(sprintf("catchments: %s displaced (%.0f%%), worst %s at %.0f%%; shares sum to %.3f-%.3f",
             fmt_n(cd$displaced), 100 * cd$displaced_share, cd$worst,
             100 * cd$worst_share, min(sums$s), max(sums$s)))
if (max(abs(sums$s - 1)) > 1e-6)
  fail <- c(fail, "a catchment's three destination shares do not sum to one")
if (abs(sum(cd$outside$living) - m$intake) > 1)
  fail <- c(fail, "the children living in the catchments do not sum to the cohort")

# Turning the catchment up must keep more children local, or the term is
# not doing what the app says it does.
tight <- outcomes(inp, run_sim(inp, site = "now", design = "Current catchments",
                               year = 2026, gamma = 3))
note(sprintf("catchment strength 3: displaced falls to %.0f%%",
             100 * tight$catchment$displaced_share))
if (tight$catchment$displaced_share >= cd$displaced_share)
  fail <- c(fail, "a stronger catchment does not keep more children local")

# ---- Each preset runs -------------------------------------------------
for (p in names(inp$presets)) {
  s <- inp$presets[[p]]
  ok <- tryCatch({
    rr <- run_sim(inp, w_mult = s$w, pans = s$pan, site = s$site,
                  design = s$design, year = s$year)
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

if (length(fail)) {
  message("\nFAILED:")
  for (f in fail) message("  - ", f)
  quit(status = 1)
}
message("\nThe app's model agrees with the published one.")
