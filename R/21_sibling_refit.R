# 21_sibling_refit.R - re-fit the free school meal layers with priority 3 in
# ---------------------------------------------------------------------------
# R/20_sibling_share.R gave the model a sibling tier above the free school
# meal priorities, and left it off, because two things were fitted without
# it (both in R/05_app_inputs.R, section 10):
#
#   fsm_takeup   k in  zones$fsm = min(k * IDACI, 0.95). The share of each
#                neighbourhood the TIERS treat as eligible. Bisected so that
#                tiers 4 and 5 at the three rationing schools total the 192
#                priority 4 and 5 offers the council made in 2026.
#   dis$lambda   per-school pull in the separate disadvantage layer that
#                REPORTS each school's disadvantaged share (outcomes.R,
#                dis_flows). Fitted on the final flows to the September
#                2026 Year 7 free school meal offers. It does not feed the
#                tiers.
#
# Insert a tier above 4 and 5 that seats eligible siblings first, and k is
# answering a question that no longer exists: at the old k, tiers 4 and 5
# fall well short of 192, because a share of the eligible children now
# arrive at 3. So k is re-bisected with the tier in. Lambda then follows,
# because it is fitted to whatever flows k produces.
#
# Both fits are re-run here with the sibling tier OFF as well. That run
# must reproduce the k and lambda already in the bundle - which is the
# check that this script follows R/05's procedure rather than a near
# relative of it.
#
# Nothing in app/data/sim_inputs.rds is overwritten. The result is written
# to data/sibling_refit.rds, and with_siblings() applies it to a bundle.
#
# Run from project root.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(tidyr)})

OUT <- here::here("data", "sibling_refit.rds")


#' Apply the sibling-tier calibration to an input bundle
#'
#' Sets everything that has to move together: the sibling shares, the
#' re-fitted take-up and the city share it implies, the re-fitted
#' disadvantage layer, and the rule. Using one without the others is the
#' double-count this script exists to remove.
with_siblings <- function(inp, fit = readRDS(OUT)) {
  inp$sibling_share     <- fit$sibling_share
  inp$zones$fsm         <- pmin(fit$on$k_fsm * inp$zones$idaci_score, 0.95)
  inp$rules$fsm_takeup  <- fit$on$k_fsm
  inp$rules$fsm_city    <- fit$on$fsm_city
  inp$params$dis$k      <- fit$on$dis_k
  inp$params$dis$lambda <- fit$on$dis_lambda
  inp$rules$siblings_calibrated <- TRUE
  inp
}


refit <- function(inp, sibs, counts = TRUE) {
  source(here::here("app", "R", "model.R"))
  source(here::here("app", "R", "outcomes.R"))
  rl <- inp$rules
  rationing <- rl$rationing
  target <- sum(rl$outturn_2026$offers[rl$outturn_2026$priority %in% 4:5])
  # `counts` is the counting rule for eligible siblings against the free
  # school meal ceiling. The 2026 offers being fitted were made under the
  # corrected rule, so that is the calibration; the other is kept to show
  # what fitting under the wrong rule does.
  RULES <- list(rule = "priorities", siblings = sibs, sibling_fsm_counts = counts)

  # ---- k: tiers 4 and 5 at the rationing schools to the council's 192
  run_k <- function(k) {
    inp$zones$fsm <- pmin(k * inp$zones$idaci_score, 0.95)
    run_sim(inp, year = 2026, rules = RULES)
  }
  p45 <- function(r) {
    t <- r$tiers
    sum((t$p45_in + t$p45_out)[t$name %in% rationing])
  }
  # Under the corrected counting the response is not monotone: more
  # eligibility puts more eligible siblings above the quota, which then
  # shrinks it, so tiers 4 and 5 rise, peak and fall, and cross 192
  # twice. R/05's bisection assumes a single crossing. Here a grid finds
  # the FIRST crossing - the lowest take-up consistent with the offers -
  # and the bisection works inside that bracket only. With the tier off
  # the response is monotone and this lands on R/05's answer.
  grid <- seq(0.25, 6, by = 0.25)
  above <- NA
  for (g in grid) if (p45(run_k(g)) >= target) { above <- g; break }
  if (is.na(above))
    stop("take-up cannot reach the published FSM offers at any level")
  lo <- max(0.05, above - 0.25); hi <- above
  for (i in 1:40) {
    mid <- (lo + hi) / 2
    if (p45(run_k(mid)) < target) lo <- mid else hi <- mid
    if (hi - lo < 1e-4) break
  }
  k_fsm <- hi
  inp$zones$fsm <- pmin(k_fsm * inp$zones$idaci_score, 0.95)
  r <- run_sim(inp, year = 2026, rules = RULES)
  fsm_city <- weighted.mean(inp$zones$fsm, inp$zones$Oi)

  # ---- lambda: the reporting layer, on the flows that k produces.
  # Same loop as fit_dis() in R/05, against the same targets.
  fit_tab <- inp$params$dis$fit
  DIS_N <- fit_tab$name
  y7_target <- setNames(fit_tab$target, DIS_N)
  intake <- tapply(r$flows$flow, r$flows$name, sum)[DIS_N]
  share <- function(dk, lam) {
    inp$params$dis <- list(k = dk, lambda = lam)
    dd <- dis_flows(inp, r$flows)
    tapply(dd, r$flows$name, sum)[DIS_N] / intake
  }
  lam <- setNames(rep(1, length(DIS_N)), DIS_N); dk <- 1
  for (it in 1:500) {
    sh <- share(dk, lam)
    city_mod <- sum(sh * intake) / sum(intake)
    city_pub <- sum(y7_target * intake) / sum(intake)
    gap <- max(abs(sh - y7_target))
    if (gap < 1e-4 && abs(city_mod - city_pub) < 1e-4) break
    dk <- dk * city_pub / city_mod
    lam <- lam * (y7_target / sh)^0.8
    lam <- lam / exp(mean(log(lam)))
  }
  stopifnot(gap < 0.005)

  # ---- what was NOT fitted, against the council's own figures
  y7 <- inp$params$dis$year7_fsm
  t <- r$tiers |> filter(name %in% rationing)
  by_school <- r$schools |> filter(name %in% rationing) |>
    select(name, intake, fsm) |>
    inner_join(t |> transmute(name, p45 = p45_in + p45_out,
                              p3 = p3_sibling, p3_fsm), by = "name") |>
    inner_join(y7 |> select(name, offers_2026, fsm_2026, fsm_p13_2026,
                            fsm_p45_2026), by = "name") |>
    inner_join(rl$outturn_2026 |> filter(priority %in% 4:5) |>
                 group_by(name = school) |>
                 summarise(p45_council = sum(offers), .groups = "drop"),
               by = "name")

  list(k_fsm = k_fsm, fsm_city = fsm_city, dis_k = dk,
       dis_lambda = lam, dis_rounds = it, dis_gap = gap,
       p45_total = p45(r), target = target, by_school = by_school)
}


#' What adopting the calibration would move, in the simulator's own Today
#'
#' Two kinds of number, kept apart because they answer different
#' questions. The headline outcomes the analysis argues from - intakes,
#' segregation, the travel gap, journeys - should barely move, because the
#' sibling tier reorders who gets in rather than how many. The free school
#' meal share the tiers work with moves a lot, and is checked against the
#' council's September 2026 Year 7 figures, which neither fit used.
adoption_effect <- function(inp, fit_out, year = 2026) {
  source(here::here("app", "R", "model.R"))
  source(here::here("app", "R", "outcomes.R"))
  inp2 <- with_siblings(inp, fit_out)
  a <- run_sim(inp,  year = year, rules = list(rule = "priorities"))
  b <- run_sim(inp2, year = year, rules = list(rule = "priorities", siblings = TRUE))
  oa <- outcomes(inp, a); ob <- outcomes(inp2, b)
  y7 <- inp$params$dis$year7_fsm
  fsm <- a$schools |> select(name, intake_now = intake, fsm_now = fsm) |>
    inner_join(b$schools |> select(name, intake_new = intake, fsm_new = fsm),
               by = "name") |>
    inner_join(y7 |> transmute(name, council = 100 * fsm_2026 / offers_2026),
               by = "name") |>
    mutate(now = 100 * fsm_now / intake_now, new = 100 * fsm_new / intake_new)
  heads <- c("gorard", "dep_gap", "mean_min")
  list(fsm = fsm,
       headline = tibble(measure = heads,
                         now = vapply(heads, function(k) oa[[k]], numeric(1)),
                         adopted = vapply(heads, function(k) ob[[k]], numeric(1))),
       intake_shift = max(abs(fsm$intake_new - fsm$intake_now)),
       fsm_mae_now = mean(abs(fsm$now - fsm$council)),
       fsm_mae_new = mean(abs(fsm$new - fsm$council)),
       council_city_y7 = 100 * sum(y7$fsm_2026) / sum(y7$offers_2026))
}


build_sibling_refit <- function() {
  source(here::here("R", "00_core.R"))
  inp <- readRDS(here::here("app", "data", "sim_inputs.rds"))
  sb  <- readRDS(here::here("data", "sibling_share.rds"))
  inp$sibling_share <- sb$share

  message("Re-fitting with the sibling tier OFF (must reproduce the bundle) ...")
  off <- refit(inp, FALSE)
  message("Re-fitting with the sibling tier ON, 2025/26 counting ...")
  on <- refit(inp, TRUE, counts = TRUE)
  message("Re-fitting with the sibling tier ON, 2024/25 counting (for comparison) ...")
  on_2425 <- refit(inp, TRUE, counts = FALSE)

  # The check: the off fit is R/05's own fit, recomputed.
  repro <- c(k_fsm = off$k_fsm - inp$rules$fsm_takeup,
             dis_k = off$dis_k - inp$params$dis$k,
             lambda = max(abs(off$dis_lambda - inp$params$dis$lambda[names(off$dis_lambda)])))
  message(sprintf("\nreproduction of the bundle, siblings off: k %+.4f, dis k %+.4f, max lambda %.4f",
                  repro[["k_fsm"]], repro[["dis_k"]], repro[["lambda"]]))
  stopifnot(abs(repro[["k_fsm"]]) < 2e-4, abs(repro[["dis_k"]]) < 1e-3,
            repro[["lambda"]] < 1e-2)

  message(sprintf("\ntake-up k: %.4f off -> %.4f on  (city eligible share %.1f%% -> %.1f%%)",
                  off$k_fsm, on$k_fsm, 100 * off$fsm_city, 100 * on$fsm_city))
  message(sprintf("disadvantage k: %.4f off -> %.4f on", off$dis_k, on$dis_k))
  lam <- tibble(school = names(on$dis_lambda),
                off = unname(off$dis_lambda), on = unname(on$dis_lambda)) |>
    mutate(change_pct = round(100 * (on / off - 1), 1))
  print(as.data.frame(lam |> mutate(across(c(off, on), ~round(.x, 3)))), row.names = FALSE)

  show <- function(f, tag) {
    message("\n", tag)
    print(as.data.frame(f$by_school |>
      transmute(school = short_sch(name),
                p45_model = round(p45, 1), p45_council = p45_council,
                p3_fsm_model = round(p3_fsm, 1), p13_fsm_council = fsm_p13_2026,
                fsm_model = round(fsm, 1), fsm_council = fsm_2026)),
      row.names = FALSE)
  }
  show(off, "Siblings OFF - not fitted except the 192 total:")
  show(on_2425, "Siblings ON, re-fitted under the 2024/25 counting:")
  show(on,  "Siblings ON, re-fitted under the 2025/26 counting - the calibration:")

  # One number per fit for how well the unfitted split lands.
  err <- function(f) with(f$by_school, c(
    p45_mae = mean(abs(p45 - p45_council)),
    p13_mae = mean(abs(p3_fsm - fsm_p13_2026)),
    fsm_mae = mean(abs(fsm - fsm_2026))))
  scores <- rbind(off = err(off), on_2024_25 = err(on_2425), on_2025_26 = err(on))
  message("\nMean absolute error at the three rationing schools, children:")
  print(round(scores, 1))

  out <- list(off = off, on = on, on_2425 = on_2425, scores = scores,
              lambda = lam, reproduction = repro,
              sibling_share = sb$share, built_at = Sys.time())
  saveRDS(out, OUT)   # with_siblings() below reads this

  ad <- adoption_effect(inp, out)
  message(sprintf(paste0("\nAdopting it, 2026: intakes move by at most %.1f; tier FSM share ",
                         "against the council's Year 7 figures, mean error %.1f -> %.1f points"),
                  ad$intake_shift, ad$fsm_mae_now, ad$fsm_mae_new))
  print(as.data.frame(ad$headline |> mutate(across(where(is.numeric), ~round(.x, 3)))),
        row.names = FALSE)
  out$adoption <- ad
  saveRDS(out, OUT)
  message("\nWrote ", OUT)
  invisible(out)
}

if (sys.nframe() == 0) build_sibling_refit()
