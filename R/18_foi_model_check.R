# 18_foi_model_check.R - does the model put children where the FOI maps do?
# ---------------------------------------------------------------------------
# The spatial interaction model is calibrated against the council's
# published offer totals: how many children each school takes. It is NOT
# calibrated against where those children live. The FOI allocation maps
# (R/17_foi_allocations.R) are the first independent observation of that
# origin geography, so comparing the two tests the part of the model that
# was never fitted - the distance decay, the catchment term and the
# competition term, which between them decide which neighbourhood a child
# comes from rather than how many arrive.
#
# Comparison is on SHARES, not counts. The extraction recovers about 87% of
# published offers because overlapping symbols merge, and that shortfall is
# worse in dense neighbourhoods - exactly where the flows are largest. A
# count comparison would measure the extraction's bias. Comparing each
# school's origin distribution removes it, because the bias is common to
# the whole school.
#
# Writes data/foi_model_check.rds. Run from project root.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(sf)
})

OUT  <- here::here("data", "foi_model_check.rds")
LSOA <- here::here("data", "lsoa.geojson")


#' Baseline flows for the year the FOI maps cover
model_flows <- function(inp, year) {
  source(here::here("app", "R", "model.R"), local = FALSE)
  sim <- run_sim(inp, year = year)
  sim$flows |>
    group_by(lsoa, school = name) |>
    summarise(modelled = sum(flow), .groups = "drop")
}


#' Observed origins from the FOI maps
foi_flows <- function(foi) {
  foi$by_lsoa |>
    group_by(lsoa = lsoa21cd, school) |>
    summarise(observed = sum(children), .groups = "drop")
}


#' Share of each school's intake coming from each LSOA, both sides
as_shares <- function(j) {
  j |>
    group_by(school) |>
    mutate(obs_share = observed / sum(observed),
           mod_share = modelled / sum(modelled)) |>
    ungroup()
}


#' How alike are two origin distributions?
#'
#' The index of dissimilarity is the share of children who would have to
#' move neighbourhood for the two distributions to coincide: 0 is identical,
#' 1 is no overlap. It is the natural measure here because both sides are
#' distributions over the same zones, and it does not care about the
#' extraction's overall undercount.
school_fit <- function(s) {
  s |>
    group_by(school) |>
    summarise(
      lsoas      = n(),
      observed   = sum(observed),
      modelled   = round(sum(modelled)),
      r          = cor(obs_share, mod_share),
      rho        = cor(obs_share, mod_share, method = "spearman"),
      dissim     = 0.5 * sum(abs(obs_share - mod_share)),
      .groups    = "drop") |>
    arrange(dissim)
}


#' The same comparison at catchment level
#'
#' An LSOA holds a handful of children, so most cells are 0, 1 or 2 and the
#' share comparison is mostly measuring noise - and the extraction's
#' undercount is worst in exactly the dense LSOAs that carry the largest
#' flows, which pushes the two distributions apart for a reason that has
#' nothing to do with the model. Six catchments are coarse enough that
#' neither problem bites, and they are the unit policy is actually argued
#' in.
catchment_fit <- function(foi, flows_raw, zones) {
  lk <- zones |> select(lsoa, catchment) |> distinct(lsoa, .keep_all = TRUE)
  obs <- foi$by_lsoa |>
    left_join(lk, by = c("lsoa21cd" = "lsoa")) |>
    filter(!is.na(catchment)) |>
    group_by(school, catchment) |>
    summarise(observed = sum(children), .groups = "drop")
  mod <- flows_raw |>
    group_by(school = name, catchment) |>
    summarise(modelled = sum(flow), .groups = "drop")
  full_join(obs, mod, by = c("school", "catchment")) |>
    filter(school %in% unique(obs$school)) |>
    mutate(across(c(observed, modelled), ~tidyr::replace_na(.x, 0))) |>
    group_by(school) |>
    mutate(obs_share = observed / sum(observed),
           mod_share = modelled / sum(modelled)) |>
    ungroup()
}


#' How far the catchment-memory term can go towards closing the Longhill gap
#'
#' §"Catchment memory" in app/R/model.R already carries a share of the six
#' neighbourhoods moved out of Longhill's catchment in 2024 as though they
#' were still in it. It is set to 0.5. Sweeping it says whether the Longhill
#' origin gap is that parameter being too low, or something the parameter
#' cannot reach.
memory_sweep <- function(inp, foi, zones, phis = c(0, 0.3, 0.5, 0.7, 0.85, 1)) {
  lk <- zones |> select(lsoa, catchment) |> distinct(lsoa, .keep_all = TRUE)
  obs <- foi$by_lsoa |>
    filter(school == "Longhill High School") |>
    left_join(lk, by = c("lsoa21cd" = "lsoa")) |>
    summarise(intake = sum(children),
              own_pct = 100 * sum(children[catchment == "Longhill"], na.rm = TRUE) / intake,
              from_dsv = sum(children[catchment == "DS_Varndean"], na.rm = TRUE))

  rows <- lapply(phis, function(p) {
    s <- run_sim(inp, year = foi$entry_year, memory = p)
    lh <- s$flows |> filter(name == "Longhill High School") |>
      left_join(lk, by = "lsoa", suffix = c("", "_z"))
    tibble(phi = p,
           intake = sum(lh$flow),
           own_pct = 100 * sum(lh$flow[lh$catchment_z == "Longhill"], na.rm = TRUE) / sum(lh$flow),
           from_dsv = sum(lh$flow[lh$catchment_z == "DS_Varndean"], na.rm = TRUE))
  })
  bind_rows(rows) |>
    bind_rows(tibble(phi = NA_real_, intake = obs$intake,
                     own_pct = obs$own_pct, from_dsv = obs$from_dsv))
}


#' Mean straight-line distance from home to the allocated school
#'
#' A second, blunter check that does not go through LSOAs at all: the maps
#' give real home locations, so the average journey they imply can be
#' compared with the model's. The model's own cij is a bus-and-walk time,
#' so the comparison is on kilometres, which both sides have.
distance_check <- function(foi, inp, flows_raw) {
  sch <- inp$schools |>
    select(school = name, s_e = easting, s_n = northing)
  obs <- foi$points |>
    st_drop_geometry() |>
    inner_join(sch, by = "school") |>
    mutate(km = sqrt((easting - s_e)^2 + (northing - s_n)^2) / 1000) |>
    group_by(school) |>
    summarise(obs_km = weighted.mean(km, est_children),
              children = sum(est_children), .groups = "drop")
  mod <- flows_raw |>
    group_by(school = name) |>
    summarise(mod_km = weighted.mean(km, flow), .groups = "drop")
  obs |> inner_join(mod, by = "school") |>
    mutate(diff_km = obs_km - mod_km) |>
    arrange(desc(abs(diff_km)))
}


build_foi_model_check <- function() {
  inp <- readRDS(here::here("app", "data", "sim_inputs.rds"))
  foi <- readRDS(here::here("data", "foi_allocations.rds"))

  source(here::here("app", "R", "model.R"))
  sim <- run_sim(inp, year = foi$entry_year)
  flows_raw <- sim$flows

  mod <- flows_raw |>
    group_by(lsoa, school = name) |>
    summarise(modelled = sum(flow), .groups = "drop")
  obs <- foi_flows(foi)

  # Every LSOA x school cell either side has, so a neighbourhood the model
  # sends children to and the maps show none is counted as a miss rather
  # than dropped.
  j <- full_join(obs, mod, by = c("lsoa", "school")) |>
    filter(school %in% unique(obs$school)) |>
    mutate(across(c(observed, modelled), ~tidyr::replace_na(.x, 0)))

  s   <- as_shares(j)
  fit <- school_fit(s)
  dis <- distance_check(foi, inp, flows_raw)

  overall <- with(s, c(cells = nrow(s),
                       r = cor(obs_share, mod_share),
                       rho = cor(obs_share, mod_share, method = "spearman")))

  message("Origin geography, model against the FOI maps, ", foi$entry_year, ":")
  print(as.data.frame(fit |> mutate(across(c(r, rho, dissim), ~round(.x, 3)))),
        row.names = FALSE)
  message("\nAcross all ", nrow(s), " LSOA x school cells: r = ",
          round(overall[["r"]], 3), ", rho = ", round(overall[["rho"]], 3))

  cf <- catchment_fit(foi, flows_raw, inp$zones)
  cfit <- cf |>
    group_by(school) |>
    summarise(r = cor(obs_share, mod_share),
              dissim = 0.5 * sum(abs(obs_share - mod_share)),
              .groups = "drop") |>
    arrange(dissim)
  message("\nSame thing by catchment, six zones not 165:")
  print(as.data.frame(cfit |> mutate(across(where(is.numeric), ~round(.x, 3)))),
        row.names = FALSE)

  # The single most interpretable number: how much of each school's intake
  # comes from its own catchment, observed against modelled.
  # The schools table calls the catchment `group`; it is the same labelling
  # the zones use.
  #
  # Own-catchment share, model against the figure the council PRINTS on
  # each map. This is the comparison that counts: the maps carry an exact
  # out-of-catchment total, so the observed share is arithmetic rather
  # than estimated from dots. The dot-derived column sits beside it to
  # show how far the extraction can be trusted on this quantity - which
  # turns out not to be far, school by school.
  own_pub <- cf |>
    inner_join(inp$schools |> select(school = name, home = group), by = "school") |>
    filter(catchment == home) |>
    transmute(school, model_pct = round(100 * mod_share),
              dots_pct = round(100 * obs_share)) |>
    inner_join(foi$own_published |> select(school, offers, out_of_catchment,
                                           published_pct = own_pct),
               by = "school") |>
    mutate(gap = model_pct - published_pct,
           dots_error = dots_pct - published_pct) |>
    arrange(desc(abs(gap))) |>
    select(school, offers, out_of_catchment, published_pct, model_pct, gap,
           dots_pct, dots_error)
  message("\nOwn-catchment share, model against the council's printed figure:")
  print(as.data.frame(own_pub), row.names = FALSE)

  own <- cf |>
    inner_join(inp$schools |> select(school = name, home = group),
               by = "school") |>
    filter(catchment == home) |>
    transmute(school, obs_pct = round(100 * obs_share),
              mod_pct = round(100 * mod_share),
              gap = obs_pct - mod_pct) |>
    # Carried alongside because it cuts against the finding: the dots the
    # extraction loses are the overlapping ones, which sit in the dense
    # core of a school's own catchment. A school with a low recovery rate
    # may therefore have a truer own-catchment share than shown, and its
    # gap is the least trustworthy. Longhill, where the extraction
    # recovers everything, is the one to believe.
    left_join(foi$validation |> select(school = name, recovered),
              by = "school") |>
    arrange(desc(abs(gap)))
  message("\nShare of intake from the school's own catchment:")
  print(as.data.frame(own), row.names = FALSE)

  message("\nMean straight-line journey, km:")
  print(as.data.frame(dis |> mutate(across(where(is.numeric), ~round(.x, 2)))),
        row.names = FALSE)

  sweep <- memory_sweep(inp, foi, inp$zones)
  message("\nCatchment-memory sweep at Longhill (last row is observed):")
  print(as.data.frame(sweep |> mutate(across(where(is.numeric), ~round(.x, 1)))),
        row.names = FALSE)

  # City LSOAs in the boundary file that the model gives no origin
  # population: they are Lewes, so the model is right to exclude them -
  # but children do cross the boundary into Longhill and it cannot
  # represent them.
  lsoa_all <- st_read(LSOA, quiet = TRUE)
  outside <- setdiff(lsoa_all$lsoa21cd, unique(inp$zones$lsoa))
  cross <- foi$by_lsoa |> filter(lsoa21cd %in% outside) |>
    group_by(lsoa21cd, school) |>
    summarise(children = sum(children), .groups = "drop")
  message("\nChildren from LSOAs with no modelled origin population: ",
          sum(cross$children))
  print(as.data.frame(cross), row.names = FALSE)

  out <- list(cells = s, by_school = fit, by_catchment = cf, own_published = own_pub,
              catchment_fit = cfit, own_catchment = own, distance = dis,
              memory_sweep = sweep, cross_boundary = cross,
              overall = overall, entry_year = foi$entry_year,
              built_at = Sys.time())
  saveRDS(out, OUT)
  message("\nWrote ", OUT)
  invisible(out)
}

if (sys.nframe() == 0) build_foi_model_check()
