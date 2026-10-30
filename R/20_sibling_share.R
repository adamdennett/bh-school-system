# 20_sibling_share.R - priority 3, the sibling link
# ---------------------------------------------------------------------------
# The council's published oversubscription criteria are, in order:
#
#   1 looked after      2 exceptional need      3 SIBLING LINK
#   4 in catchment and eligible for free school meals, to the city average
#   5 other children eligible for free school meals, to the city average
#   6 out of catchment but within BACA/PACA/Patcham/Longhill, to 5%
#   7 in catchment      8 other
#
# The model has tiers 4 to 8. Tier 3 is missing, and it is not small: at
# the three schools where priorities actually bind it is a fifth of the
# intake.
#
# WHY IT MATTERS, and it is not the obvious reason. Sibling sits ABOVE
# both free school meal priorities. A child who is both a sibling and
# eligible for free school meals is admitted at 3, so they never consume
# the quota at 4 or 5 - but they still count in the school's free school
# meal share. Capping the quota is not the same as capping the intake.
# That is how Varndean came out at about 40% free school meals in 2024/25
# against a stated ceiling of 30%: the siblings being admitted were
# disproportionately eligible, and were not counted towards it. The
# counting was corrected for the 2025/26 round.
#
# The model has the mirror image of the same confusion. With no tier 3,
# every eligible child is routed through the capped tiers, so the cap
# binds harder in the model than it does in life.
#
# This script derives the share of each school's demand that carries a
# sibling link, from the council's own allocation outturn by priority.
# R/05_app_inputs.R calls sibling_share() to build the tier into the
# bundle (since October 2026). Run on its own, it writes
# data/sibling_share.rds with the stacking demonstration for the guide.
#
# Run from project root.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(tidyr)})

OUT <- here::here("data", "sibling_share.rds")


#' Share of admitted children holding a sibling link, per school
#'
#' Taken from the offers-by-priority table the council publishes for the
#' schools that were oversubscribed. Those are the only schools where it
#' can be observed and the only schools where it changes anything: where
#' a school admits everyone who names it, the priority a child came in
#' under is a label, not a gate.
#'
#' For schools with no published row the city mean is used. That is a
#' guess, and it is inert in the baseline because those schools are under
#' their admission numbers - but it stops a scenario that fills one of
#' them behaving as though it had no siblings at all.
sibling_share <- function(inp) {
  o <- inp$rules$outturn_2026
  stopifnot(!is.null(o), all(c("school", "priority", "offers") %in% names(o)))

  # The sibling and quota lines are computed before `offers` is collapsed:
  # name the total anything else and summarise() would have them reading
  # the scalar it had just created rather than the column.
  obs <- o |>
    group_by(school) |>
    summarise(siblings      = sum(offers[priority == 3]),
              fsm_criterion = sum(offers[priority %in% c(4, 5)]),
              total         = sum(offers),
              .groups = "drop") |>
    rename(offers = total) |>
    mutate(share = siblings / offers)

  city <- sum(obs$siblings) / sum(obs$offers)

  all_schools <- inp$schools$name[inp$schools$community %in% TRUE]
  out <- tibble(school = all_schools) |>
    left_join(obs |> select(school, share), by = "school") |>
    mutate(observed = !is.na(share),
           share = ifelse(is.na(share), city, share))

  list(share = setNames(out$share, out$school),
       table = out,
       observed = obs,
       city = city,
       source = paste("Brighton & Hove City Council, Year 7 allocation",
                      "outturn by admission priority, September 2026"),
       built_at = Sys.time())
}


#' What the tier does, for @sec-siblings
#'
#' Two things: whether it reproduces the council's sibling counts (it
#' does, by construction, because the share came from them), and what it
#' does to a school's free school meal share once the quota binds (which
#' is not by construction, and is the reason the tier exists).
sibling_effect <- function(inp, share, cap = inp$rules$fsm_city) {
  source(here::here("app", "R", "model.R"))
  inp$sibling_share <- share
  rationing <- names(share)[share > 0 &
                            names(share) %in% inp$rules$outturn_2026$school]

  # The stacking demonstration asks for the 2024/25 counting explicitly -
  # eligible siblings NOT counted against the ceiling - because that is
  # the mechanism it exists to show. run_sim() now defaults to the
  # corrected rule, and left to the default this table would quietly stop
  # demonstrating anything. R/21_sibling_refit.R covers the rule in force.
  run <- function(fsm_cap, sibs) {
    i2 <- inp; i2$rules$fsm_cap_share <- fsm_cap
    run_sim(i2, year = 2026,
            rules = list(rule = "priorities", p6_share = inp$rules$p6_share,
                         fsm = TRUE, targeted = FALSE, siblings = sibs,
                         sibling_fsm_counts = FALSE))
  }

  on_default <- run(inp$rules$fsm_cap_share, TRUE)
  counts <- on_default$tiers |>
    filter(name %in% rationing) |>
    select(school = name, modelled = p3_sibling) |>
    inner_join(inp$rules$outturn_2026 |>
                 filter(priority == 3) |>
                 select(school, observed = offers), by = "school")

  a <- run(cap, FALSE)$schools; b <- run(cap, TRUE)$schools
  stack <- a |> filter(name %in% rationing) |>
    transmute(school = name, fsm_off = 100 * fsm / intake) |>
    inner_join(b |> filter(name %in% rationing) |>
                 transmute(school = name, fsm_on = 100 * fsm / intake),
               by = "school") |>
    mutate(cap = cap)

  list(counts = counts, stack = stack, cap = cap)
}


build_sibling_share <- function() {
  inp <- readRDS(here::here("app", "data", "sim_inputs.rds"))
  s <- sibling_share(inp)

  message("Sibling link as a share of admitted children:")
  print(as.data.frame(s$observed |>
    mutate(share = round(100 * share, 1)) |>
    select(school, offers, siblings, share_pct = share, fsm_criterion)),
    row.names = FALSE)
  message("\nCity mean used where unobserved: ", round(100 * s$city, 1), "%")
  message(sum(!s$table$observed), " of ", nrow(s$table),
          " community schools take the city mean")

  eff <- sibling_effect(inp, s$share)
  message("\nSibling admissions, council against model:")
  print(as.data.frame(eff$counts |> mutate(modelled = round(modelled, 1))),
        row.names = FALSE)
  message("\nFSM share with the quota at the city average (",
          round(100 * eff$cap, 1), "%):")
  print(as.data.frame(eff$stack |>
          mutate(across(c(fsm_off, fsm_on), ~round(.x, 1)),
                 diff = round(fsm_on - fsm_off, 1)) |>
          select(school, fsm_off, fsm_on, diff)), row.names = FALSE)

  s$effect <- eff
  saveRDS(s, OUT)
  message("\nWrote ", OUT)
  invisible(s)
}

if (sys.nframe() == 0) build_sibling_share()
