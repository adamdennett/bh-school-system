# app/R/state.R — a scenario as a file or a link, and back again
# ======================================================================
# The controls exactly as they stand, not the run they produce. The Excel
# workbook cannot be read back: it holds products, like the city and
# catchment multipliers multiplied together, and the slider positions
# cannot be recovered from them.
#
# A saved scenario stores settings, not results, so if the model is
# re-fitted the same settings give different numbers. Hence the model
# stamp, and the warning when it differs.
#
# Everything read here is user input. It is parsed as data and each value
# is checked against what its control can hold; nothing in it is run.
# ======================================================================

STATE_FORMAT <- "bh-school-simulator-scenario"
STATE_VERSION <- 1L
STATE_MAX_BYTES <- 100000

state_to_json <- function(state)
  as.character(jsonlite::toJSON(state, auto_unbox = TRUE, digits = NA, null = "null"))

#' Read a scenario, returning list(state, notes)
#'
#' Fails, with a message fit to show the user, on anything that is not a
#' scenario. Anything merely out of date or out of range is put right and
#' said so in `notes`, because most of a scenario is better than none.
#'
#' @param ctx what the app can hold: presets, designs, sites, hp_sites,
#'   bases, rules, years, gammas, schools (urn, name, pan, hypothetical),
#'   model
state_parse <- function(txt, ctx) {
  if (length(txt) != 1 || !is.character(txt) || nchar(txt, "bytes") > STATE_MAX_BYTES)
    stop("That file is too large to be a saved scenario.", call. = FALSE)
  x <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                error = function(e) NULL)
  if (!is.list(x) || !identical(x$format, STATE_FORMAT))
    stop("That is not a scenario saved from this simulator.", call. = FALSE)
  if (!is.numeric(x$version) || x$version > STATE_VERSION)
    stop("That scenario was saved by a newer version of the simulator.", call. = FALSE)

  notes <- character(0)
  say <- function(...) notes <<- c(notes, paste0(...))
  pick <- function(v, choices, default, what) {
    if (is.character(v) && length(v) == 1 && v %in% choices) return(v)
    say(what, " in the file is not available now, so it has been set to \"", default, "\".")
    default
  }
  num <- function(v, lo, hi, default, what) {
    if (!is.numeric(v) || length(v) != 1 || !is.finite(v)) {
      say(what, " was missing from the file, so it has been set to ", default, ".")
      return(default)
    }
    if (v < lo || v > hi) say(what, " was out of range and has been brought back to it.")
    min(max(v, lo), hi)
  }
  flag <- function(v, default) if (is.logical(v) && length(v) == 1 && !is.na(v)) v else default
  text <- function(v, n) if (is.character(v) && length(v) == 1) substr(v, 1, n) else ""

  preset <- pick(x$preset, ctx$presets, ctx$presets[1], "The base scenario")
  gamma_each <- vapply(ctx$gammas, function(g)
    num(x$gamma_each[[g]], 0, 2, 1, paste("The multiplier for", g)), numeric(1))

  schools <- lapply(seq_len(nrow(ctx$schools)), function(i) {
    u <- as.character(ctx$schools$urn[i])
    s <- x$schools[[u]]
    nm <- ctx$schools$name[i]
    if (is.null(s)) say(nm, " was not in the file, so it has its current settings.")
    list(w = if (is.null(s)) 1 else num(s$w, 0.25, 4, 1, paste(nm, "attractiveness")),
         pan = if (is.null(s)) ctx$schools$pan[i] else
           num(s$pan, 30, 400, ctx$schools$pan[i], paste(nm, "places")),
         closed = flag(s$closed, isTRUE(ctx$schools$hypothetical[i])))
  })
  names(schools) <- as.character(ctx$schools$urn)

  if (!identical(x$model, ctx$model))
    say("This scenario was saved from a different build of the model. Its settings are restored, but the figures may differ from when it was saved.")

  list(state = list(
    format = STATE_FORMAT, version = STATE_VERSION, model = ctx$model,
    preset = preset, title = text(x$title, 200), notes = text(x$notes, 5000),
    design = pick(x$design, ctx$designs, ctx$designs[1], "The catchment map"),
    site = pick(x$site, ctx$sites, ctx$sites[1], "Longhill's site"),
    hp_site = pick(x$hp_site, ctx$hp_sites, ctx$hp_sites[1], "Hove Park's Year 7 site"),
    w_basis = pick(x$w_basis, ctx$bases, ctx$bases[1], "What families choose on"),
    year = num(x$year, ctx$years[1], ctx$years[2], ctx$years[1], "The entry year"),
    gamma = num(x$gamma, 0, 2, 1, "The catchment multiplier"),
    gamma_each = as.list(gamma_each),
    exclusive = num(x$exclusive, 0, 2, 1, "The paired-catchment multiplier"),
    rule = pick(x$rule, ctx$rules, ctx$rules[1], "The admission rule"),
    p6 = num(x$p6, 0, 40, 5, "The priority 6 share"),
    fsm = flag(x$fsm, TRUE), targeted = flag(x$targeted, FALSE),
    siblings = flag(x$siblings, TRUE), sib_counts = flag(x$sib_counts, TRUE),
    schools = schools), notes = notes)
}

# ---- As a link ----------------------------------------------------------
# The whole scenario in the address, so nothing is stored anywhere and the
# link can go wherever a link can go. Compressed, then base64 made safe for
# a URL. The ceiling is on what is accepted, so a link cannot be used to
# hand the app something enormous.
STATE_MAX_LINK <- 8000

state_to_link <- function(state) {
  z <- memCompress(charToRaw(enc2utf8(state_to_json(state))), "gzip")
  chartr("+/", "-_", gsub("=", "", base64enc::base64encode(z), fixed = TRUE))
}

state_from_link <- function(s) {
  if (length(s) != 1 || nchar(s) > STATE_MAX_LINK)
    stop("That link is too long to be a scenario.", call. = FALSE)
  b <- chartr("-_", "+/", s)
  b <- paste0(b, strrep("=", (4 - nchar(b) %% 4) %% 4))
  tryCatch({
    txt <- rawToChar(memDecompress(base64enc::base64decode(b), "gzip"))
    Encoding(txt) <- "UTF-8"
    txt
  }, error = function(e) stop("That link does not hold a scenario. It may have been cut short.",
                              call. = FALSE))
}
