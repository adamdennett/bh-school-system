# app/app.R — Brighton secondary schools: a policy simulator
# ======================================================================
# Adjust attractiveness, admission numbers, the site of one school, the
# catchment map and the year; the spatial interaction model re-runs and
# every objective is rescored.
#
# The point is the tension between the objectives. Filling one school
# empties its neighbours. Evening out disadvantage lengthens journeys.
# Cutting admission numbers fixes the finances and puts children further
# away. So the outcome strip shows all of them at once and never lets
# you optimise one out of sight of the others.
#
# The model is the one in section 7 of the strategic view, at its
# fullest rung, and app/tests/check.R asserts that it still agrees with the
# published figures.
#
#   shiny::runApp("app")
# ======================================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# Base R gained %||% in 4.4.0 and shinyapps.io may be running something
# older, so it is defined here rather than assumed.
`%||%` <- function(x, y) if (is.null(x)) y else x

# Deployed, the app IS the working directory; run locally from the
# repository root, it is in app/. Look for the files rather than for the
# script, which is the part that differs between the two.
APP <- if (file.exists(file.path(".", "data", "sim_inputs.rds"))) "." else "app"
stopifnot(file.exists(file.path(APP, "data", "sim_inputs.rds")))
source(file.path(APP, "R", "model.R"))
source(file.path(APP, "R", "outcomes.R"))

inp <- readRDS(file.path(APP, "data", "sim_inputs.rds"))
CITY <- inp$schools %>% filter(city) %>% arrange(short)

# Total places, moved in classes of 30. The slider's grid is laid out from
# whatever the total currently is, so any sum the ten admission numbers
# can reach is a point on it and a hand edit never snaps to a neighbour.
TOTAL_NOW <- sum(CITY$pan)
total_grid <- function(total) list(
  min = total - 30 * floor(max(0, total - 0.5 * TOTAL_NOW) / 30),
  max = total + 30 * floor(max(0, 1.3 * TOTAL_NOW - total) / 30))

OK <- "#1baf7a"; BAD <- "#d03b3b"; WARN <- "#eda100"; INK <- "#1f3b57"

# ---- Map colours: demand against places ---------------------------------
# Green for every school at its admission number was misleading: it made a
# school that just fills - the goldilocks case - look the same as one
# turning a hundred children away, and neither extreme is what a city
# wants. So the dots are coloured on one continuous, diverging scale:
#
#   below 1   intake / PAN            a school short of its number (red)
#   1         full, demand = places   the sweet spot (neutral grey)
#   above 1   demand / PAN            more children want it than it has
#                                     places (blue)
#
# Demand is the model's uncapped flow: what families ask for before the
# ceiling cuts it back. Intake cannot show over-subscription, because the
# ceiling holds it at the admission number.
#
# Two hues and a grey midpoint, symmetric about full, so neither side of
# it reads as 'better'. The stops are sent to the map with the dots and
# the legend is drawn from them, so the two cannot drift apart.
PRESSURE_STOPS <- data.frame(at  = c(0.4, 0.7, 1.0, 1.3, 1.6),
                             col = c("#b2182b", "#ef8a62", "#bdb8b0", "#67a9cf", "#2166ac"),
                             stringsAsFactors = FALSE)

pressure_col <- function(r) {
  at <- PRESSURE_STOPS$at
  m <- t(grDevices::col2rgb(PRESSURE_STOPS$col))
  r <- pmin(pmax(r, min(at)), max(at))
  vapply(r, function(v) {
    k <- min(findInterval(v, at, rightmost.closed = TRUE), length(at) - 1)
    f <- (v - at[k]) / (at[k + 1] - at[k])
    x <- m[k, ] + f * (m[k + 1, ] - m[k, ])
    grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255)
  }, character(1))
}

MAP_LEGEND <- list(
  title = "Demand against places",
  left = "Short of its number", right = "More want it than it can take",
  stops = lapply(seq_len(nrow(PRESSURE_STOPS)), function(i)
    list(at = PRESSURE_STOPS$at[i], col = PRESSURE_STOPS$col[i])),
  ticks = list(list(at = 0.4, lab = "40%"), list(at = 0.7, lab = "70%"),
               list(at = 1.0, lab = "Full"), list(at = 1.3, lab = "1.3×"),
               list(at = 1.6, lab = "1.6×+")))



# ---- UI --------------------------------------------------------------

school_row <- function(s) {
  div(class = "sch-row",
      div(class = "sch-name", s$short),
      div(class = "sch-w",
          sliderInput(paste0("w_", s$urn), NULL, min = 0.25, max = 4,
                      value = 1, step = 0.05, width = "100%", ticks = FALSE)),
      # A numeric box, not a dropdown from a fixed ladder. A ladder that
      # did not happen to contain King's 165 or Patcham's 225 silently
      # selected its first entry instead, so the app opened with two
      # schools on admission numbers nobody had chosen.
      div(class = "sch-pan",
          numericInput(paste0("pan_", s$urn), NULL, value = s$pan,
                       min = 30, max = 400, step = 15, width = "100%")))
}

ui <- page_sidebar(
  # Not a fillable page. A fillable main area shrinks every plotOutput to
  # whatever space is left, so a tab holding two charts and a table drew
  # them below R's minimum margins ("figure margins too large") and the
  # cohort banner at 65 pixels. Declared heights are kept; the page scrolls.
  fillable = FALSE,
  # Beta watermark, in the title bar so it is on screen whatever tab is open.
  title = div(class = "app-title",
    span("Brighton secondary schools — policy simulator"),
    span(class = "beta-mark",
         span(class = "beta-tag", "BETA"),
         span(tags$b("This simulator is currently in beta test mode"),
              " - outputs have not been validated fully, so nothing should, at this point, be taken as reliable, however the simulator shows what could be possible to develop and outputs that are possible"))),
  window_title = "Brighton secondary schools — policy simulator (beta)",
  theme = bs_theme(version = 5, bootswatch = "cosmo", base_font_size = "0.92rem"),
  tags$head(tags$style(HTML("
    .sch-row{display:flex;align-items:center;gap:6px;margin-bottom:-14px}
    .sch-name{width:104px;font-size:11.5px;line-height:1.15}
    .sch-w{flex:1}.sch-pan{width:84px}
    .sch-row .form-group{margin-bottom:0}
    .irs--shiny .irs-bar{background:#2a78d6;border-top-color:#2a78d6;border-bottom-color:#2a78d6}
    .irs--shiny .irs-single{background:#2a78d6}
    .kpi{border:1px solid #e3e3e3;border-radius:7px;padding:7px 10px;background:#fff}
    .kpi .v{font-size:19px;font-weight:600;line-height:1.1}
    .kpi .l{font-size:10.5px;color:#666;text-transform:uppercase;letter-spacing:.4px}
    .kpi .d{font-size:10.5px;color:#888}
    .note{font-size:12px;color:#555}
    .app-title{display:flex;align-items:center;gap:14px;flex-wrap:wrap;width:100%}
    .beta-mark{display:flex;align-items:center;gap:8px;flex:1;min-width:280px;background:#fff3cd;color:#664d03;border:1px solid #e0b84a;border-radius:6px;padding:4px 10px;font-size:12px;font-weight:400;line-height:1.3;white-space:normal}
    .beta-tag{background:#b35c00;color:#fff;font-weight:700;letter-spacing:1px;border-radius:4px;padding:1px 7px;font-size:11px;flex:none}
    .map-legend{background:rgba(255,255,255,.93);padding:6px 10px 4px;border-radius:5px;box-shadow:0 1px 4px rgba(0,0,0,.25);font-size:10.5px;color:#333;width:240px;line-height:1.25}
    .map-legend .ml-title{font-weight:600;margin-bottom:1px}
    .map-legend .ml-sides{display:flex;justify-content:space-between;color:#666;font-size:9.5px}
    .map-legend .ml-bar{height:10px;border-radius:3px;margin:2px 0 1px;border:1px solid #aaa}
    .map-legend .ml-ticks{position:relative;height:14px}
  ")),
    # Leaflet itself, served from www/, rather than the R package that
    # wraps it - the same build that package was shipping, so the map
    # behaves exactly as it did. See
    # www/map.js for what that saves.
    tags$link(rel = "stylesheet",
              href = "leaflet/leaflet.css"),
    tags$script(src = "leaflet/leaflet.js"),
    tags$script(src = "map.js")),

  sidebar = sidebar(
    width = 372,
    div(strong("Scenarios")),
    div(class = "note", style = "margin:2px 0 6px",
        "A selection of ready-made scenarios. Each one sets the controls below ",
        "to match it. They are starting points: everything can then be adjusted ",
        "by hand to see the likely impact of any policy choice."),
    selectInput("preset", NULL, choices = names(inp$presets)),
    div(style = "font-size:12px;font-weight:600;margin-top:-6px", "Scenario explanation"),
    div(class = "note", textOutput("preset_note")),
    hr(),
    selectInput("design", "Catchment map", choices = names(inp$designs)),
    selectInput("site", "Longhill's site",
                choices = c("Ovingdean (as now)" = "now",
                            "Elm Grove (relocated)" = "elm")),
    checkboxInput("comart", "Re-open CoMArt (closed 2005)", value = FALSE),
    conditionalPanel(
      "input.comart",
      div(class = "sch-row", style = "margin:-4px 0 -6px;font-size:11px;color:#666",
          div(class = "sch-name", ""),
          div(class = "sch-w", "Attractiveness (×)"),
          div(class = "sch-pan", "Places (PAN)")),
      div(class = "sch-row",
          div(class = "sch-name", "CoMArt"),
          div(class = "sch-w",
              sliderInput("comart_w", NULL, min = 0.25, max = 4, value = 1,
                          step = 0.05, width = "100%", ticks = FALSE)),
          div(class = "sch-pan",
              numericInput("comart_pan", NULL, value = inp$comart$pan,
                           min = 30, max = 400, step = 15, width = "100%"))),
      div(class = "note", style = "margin-top:10px", textOutput("comart_note"))),
    sliderInput("year", "Entry year", min = min(inp$demand$year),
                max = max(inp$demand$year), value = 2026, step = 1, sep = "",
                ticks = FALSE),
    sliderInput("gamma", "How much living in the catchment counts",
                min = 0, max = 2, value = 1, step = 0.05, post = "×",
                ticks = FALSE),
    div(class = "note", textOutput("gamma_note")),
    sliderInput("exclusive", "Paired-catchment families who would take only one of the two",
                min = 0, max = 2, value = 1, step = 0.1, post = "×",
                ticks = FALSE),
    div(class = "note", textOutput("exclusive_note")),
    hr(),
    radioButtons("rule", "When a school is full",
                 choices = c("Everyone has the same chance (the published model)" = "published",
                             "The council's priorities (2026/27 arrangements)" = "priorities"),
                 selected = "priorities"),
    conditionalPanel(
      "input.rule == 'priorities'",
      sliderInput("p6", "Places for single-school catchments (priority 6)",
                  min = 0, max = 40, value = 5, step = 1, post = "%",
                  ticks = FALSE),
      checkboxInput("fsm", "Free school meals priority (4 and 5)", value = TRUE),
      checkboxInput("targeted", "Narrowed to Targeted FSM (2027/28)", value = FALSE),
      div(class = "note", textOutput("rule_note"))),
    hr(),
    div(strong("Per school")),
    div(class = "note", style = "margin-top:2px",
        "Each school has two controls. ",
        tags$b("Drag its slider"), " to make the school more or less attractive to families: ",
        "1× is how attractive it is now, 2× twice as attractive, 0.5× half as attractive. ",
        tags$b("Type in the box on the right"), ", or use its arrows, to change that school's ",
        "admission number (PAN), the number of Year 7 places it offers. ",
        "The total-places slider below changes every school's number at once."),
    div(class = "sch-row", style = "margin:8px 0 -6px;font-size:11px;color:#666",
        div(class = "sch-name", "School"),
        div(class = "sch-w", "Attractiveness (× now)"),
        div(class = "sch-pan", "Places (PAN)")),
    div(style = "margin-top:8px", lapply(seq_len(nrow(CITY)),
                                         function(i) school_row(CITY[i, ]))),
    div(style = "margin-top:16px",
        sliderInput("total_pan", "Total places in the city",
                    min = total_grid(TOTAL_NOW)$min, max = total_grid(TOTAL_NOW)$max,
                    value = TOTAL_NOW, step = 30, sep = ",", ticks = FALSE,
                    width = "100%")),
    div(class = "note", style = "margin-top:-6px",
        "Moves a class of 30 at a time, and shares the change across every school in proportion to its admission number. Edit a school directly and the total follows. A city needs some places to spare: children arrive during the year, families move house and change schools, and preference only works if there is room to be offered something. Planning usually allows a margin of a few per cent over the cohort, often put at around 5%. Every place beyond that is one the city pays for and does not fill."),
    div(style = "margin-top:10px", uiOutput("pan_total")),
    div(style = "margin-top:12px",
        actionButton("reset", "Reset to the scenario", class = "btn-sm btn-outline-secondary")),
    div(style = "margin-top:10px",
        downloadButton("download_xlsx", "Download this run (Excel)",
                       class = "btn-sm btn-outline-primary")),
    div(class = "note", style = "margin-top:4px",
        "A workbook of this run: the base scenario, every setting as the scenario had it and as used here (marked where changed), each school's numbers, where each catchment's children go, the headline outcomes, the money and the fixed parameters."),
    hr(),
    selectInput("solve_for", "How attractive would a school have to be to fill?",
                choices = c("—", CITY$short)),
    div(class = "note", textOutput("solve_note"))),

  # Before the numbers, so nobody reads them as counts.
  div(style = "background:#eef4fb;border:1px solid #c9dbef;border-radius:6px;padding:6px 12px;font-size:12.5px;color:#1f3b57",
      tags$b("Every number here is modelled"),
      " - an estimate from a model of the city's schools, not a record of what happened or a forecast to plan on. ",
      "For the detail of the inputs and the models behind the simulator, read the full documentation: ",
      tags$a(href = "https://adamdennett.github.io/bh-school-system/", target = "_blank",
             rel = "noopener", "The Brighton Secondary School System: a strategic view"), "."),

  layout_columns(
    fill = FALSE, col_widths = c(2, 2, 2, 2, 2, 2),
    uiOutput("kpi_fill"), uiOutput("kpi_short"), uiOutput("kpi_money"),
    uiOutput("kpi_seg"), uiOutput("kpi_travel"), uiOutput("kpi_gap")),

  card(full_screen = FALSE,
       card_body(padding = 6, plotOutput("p_cohort", height = 190))),

  navset_card_tab(
    nav_panel("Map",
              layout_columns(
                col_widths = c(7, 5),
                div(div(id = "map", style = "height:430px"),
                    div(class = "note", style = "padding-top:6px",
                        "Dot area is the modelled intake. Colour is demand against places, on the scale in the map's legend: red is a school short of its admission number, grey one that is full with demand close to its places, and blue one that more children want than it can take. Hover a school for the numbers. The shading is the catchment map in force."),
                    div(class = "note", style = "padding-top:4px",
                        textOutput("design_note")),
                    div(style = "padding-top:10px",
                        plotOutput("p_catch_map", height = 250))),
                div(plotOutput("p_att_live", height = 292),
                    plotOutput("p_abs_live", height = 292)))),
    nav_panel("Places", plotOutput("p_places", height = 430),
              tableOutput("t_places")),
    nav_panel("Catchments",
              plotOutput("p_catch", height = 430),
              tableOutput("t_catch"),
              plotOutput("p_priority", height = 360),
              div(class = "note", htmlOutput("priority_note")),
              div(class = "note", htmlOutput("catch_note"))),
    nav_panel("Money", plotOutput("p_money", height = 430),
              tableOutput("t_money")),
    nav_panel("Fairness", plotOutput("p_fair", height = 430),
              div(class = "note",
                  "Gorard's index over the modelled intakes: half the sum of the absolute difference between each school's share of the city's deprived children and its share of all of them. Zero would be a perfectly even spread."),
              div(class = "note", style = "margin-top:10px;max-width:820px",
                  htmlOutput("fair_note"))),
    nav_panel("Attainment",
              plotOutput("p_att", height = 480),
              tableOutput("t_att"),
              div(class = "note", htmlOutput("att_note"))),
    nav_panel("Travel", plotOutput("p_travel", height = 430),
              div(class = "note",
                  "These journeys are longer than the ones in section 8 of the document, and for a reason worth knowing. The model gives every child a probability of attending every school, so a small fraction of each neighbourhood is counted as travelling to a school right across the city. Section 8 instead assigns each child to one school under the admission rules, which is a shorter journey by construction. Use these figures to compare one configuration with another, not against the section 8 numbers.")),
    nav_panel("What this is not",
              div(class = "note", style = "padding:14px 4px;max-width:760px",
                  htmlOutput("caveats")))))

# ---- Server ----------------------------------------------------------

server <- function(input, output, session) {

  # ---- Total places and the ten admission numbers ---------------------
  # Two ways in, one set of numbers. Moving the total rescales every school
  # from a BASE, in proportion. Editing a school directly makes the numbers
  # on screen the new base and moves the total to match, so the next move
  # of the slider starts from the edit. Every update the server makes comes
  # back to it as an input event, so the numbers being pushed are held in
  # `pending` and events that are only the echo of them are ignored -
  # otherwise the half-applied set would be taken for a hand edit.
  pans_rv <- reactiveValues(base = setNames(CITY$pan, CITY$name), pending = NULL)

  set_total <- function(total) {
    g <- total_grid(total)
    updateSliderInput(session, "total_pan", min = g$min, max = g$max, value = total)
  }
  push_pans <- function(p) {
    cur <- isolate(pan_now())
    changed <- names(p)[!is.finite(cur[names(p)]) | abs(cur[names(p)] - p) > 0.5]
    pans_rv$pending <- if (length(changed)) p else NULL
    for (nm in changed)
      updateNumericInput(session, paste0("pan_", CITY$urn[CITY$name == nm]),
                         value = unname(p[nm]))
  }
  set_pans <- function(base, final = NULL) {
    if (is.null(final)) final <- base
    pans_rv$base <- base
    push_pans(final)
    set_total(sum(final))
  }

  observeEvent(input$total_pan, {
    tot <- input$total_pan
    cur <- if (!is.null(pans_rv$pending)) sum(pans_rv$pending) else sum(pan_now())
    if (!is.finite(cur) || abs(tot - cur) < 0.5) return()
    push_pans(scale_pans(pans_rv$base, tot))
  }, ignoreInit = TRUE)

  observe({
    p <- pan_now()
    req(all(is.finite(p)))
    pend <- isolate(pans_rv$pending)
    if (!is.null(pend)) {
      if (all(abs(p[names(pend)] - pend) < 0.5)) pans_rv$pending <- NULL
      return()
    }
    tot <- isolate(input$total_pan)
    if (is.null(tot) || abs(sum(p) - tot) >= 0.5) {
      pans_rv$base <- p
      set_total(sum(p))
    }
  })

  apply_preset <- function(p) {
    s <- inp$presets[[p]]
    updateSelectInput(session, "design", selected = s$design)
    updateSelectInput(session, "site", selected = s$site)
    updateSliderInput(session, "year", value = s$year)
    # A preset that does not name a catchment strength means the fitted
    # one, not "leave whatever the last preset set".
    updateSliderInput(session, "gamma", value = s$gamma %||% 1)
    updateSliderInput(session, "exclusive", value = s$exclusive %||% 1)
    # Likewise a preset that says nothing about the admission rules means
    # the published model, not whatever the last preset left.
    updateRadioButtons(session, "rule", selected = s$rule %||% "priorities")
    updateSliderInput(session, "p6", value = s$p6 %||% 5)
    updateCheckboxInput(session, "fsm", value = s$fsm %||% TRUE)
    updateCheckboxInput(session, "targeted", value = s$targeted %||% FALSE)
    updateCheckboxInput(session, "comart", value = !is.null(s$comart))
    updateNumericInput(session, "comart_pan", value = s$comart$pan %||% inp$comart$pan)
    updateSliderInput(session, "comart_w", value = s$comart$w %||% 1)
    for (i in seq_len(nrow(CITY))) {
      nm <- CITY$name[i]; urn <- CITY$urn[i]
      updateSliderInput(session, paste0("w_", urn),
                        value = if (!is.null(s$w) && nm %in% names(s$w))
                          round(unname(s$w[nm]), 2) else 1)
    }
    # Admission numbers: the published ones, any the scenario names, and
    # then - where a scenario sets a city total - shared out to that total,
    # exactly as moving the slider would.
    base <- setNames(CITY$pan, CITY$name)
    if (!is.null(s$pan)) base[names(s$pan)] <- s$pan
    set_pans(base, if (is.null(s$total)) NULL else scale_pans(base, s$total))
  }
  observeEvent(input$preset, apply_preset(input$preset))
  observeEvent(input$reset, apply_preset(input$preset))

  output$preset_note <- renderText(inp$presets[[input$preset]]$note)

  CATCH_NAME <- c(PACA = "Portslade Aldridge", Hove_Blatch = "Hove Park / Blatchington",
                  Patcham = "Patcham", DS_Varndean = "Stringer / Varndean",
                  BACA = "Brighton Aldridge", Longhill = "Longhill")

  output$gamma_note <- renderText({
    req(input$gamma)
    s <- sim()
    g <- inp$params$gamma
    paste0(
      sprintf("%.0f%% of children are modelled as going to a school in their own catchment. ",
              100 * s$in_catch_share),
      sprintf("The catchment term is fitted catchment by catchment on the first preferences families actually give, from %.1f in %s to %.1f in %s. ",
              min(g), CATCH_NAME[names(which.min(g))], max(g), CATCH_NAME[names(which.max(g))]),
      if (abs(input$gamma - 1) < 0.03) "At ×1 the app uses those fitted values."
      else sprintf("At ×%.2f every catchment counts %s than families behave as though it does.",
                   input$gamma, if (input$gamma > 1) "more" else "less"))
  })

  output$exclusive_note <- renderText({
    req(input$exclusive)
    ex <- inp$params$exclusive
    sh <- function(h, s) 100 * ex$share[ex$catchment == h & grepl(s, ex$school)] * input$exclusive
    sprintf("Families who would not take the other school if refused at their choice: %.0f%% of Stringer / Varndean families would take only Varndean and %.0f%% only Stringer; %.0f%% of Hove Park / Blatchington families only Blatchington Mill and %.0f%% only Hove Park. The council's table bounds these rather than pinning them, so they can be scaled.",
            sh("DS_Varndean", "Varndean"), sh("DS_Varndean", "Stringer"),
            sh("Hove_Blatch", "Blatchington"), sh("Hove_Blatch", "Hove Park"))
  })

  w_now <- reactive({
    v <- vapply(CITY$urn, function(u) input[[paste0("w_", u)]] %||% 1, numeric(1))
    setNames(as.numeric(v), CITY$name)
  })
  pan_now <- reactive({
    v <- vapply(CITY$urn, function(u) as.numeric(input[[paste0("pan_", u)]] %||% NA),
                numeric(1))
    setNames(v, CITY$name)
  })

  # The admission rules in force, in the shape run_sim() takes.
  rule_args <- reactive(list(
    rule = input$rule %||% "priorities",
    p6_share = (input$p6 %||% 5) / 100,
    fsm = isTRUE(input$fsm %||% TRUE),
    targeted = isTRUE(input$targeted %||% FALSE)))

  # CoMArt, when a scenario or the user opens it again.
  comart_arg <- reactive(
    if (isTRUE(input$comart))
      list(pan = input$comart_pan %||% inp$comart$pan, w = input$comart_w %||% 1)
    else NULL)
  comart_places <- reactive(
    if (isTRUE(input$comart)) as.numeric(input$comart_pan %||% inp$comart$pan) else 0)
  output$comart_note <- renderText(sprintf(paste(
    "On its old site in East Brighton, sharing Longhill's catchment in whatever map is in force,",
    "and run as a community school under the council's priorities - so Longhill's catchment",
    "has two schools and its children lose priority 6. There are no preferences for a school",
    "that does not exist, so 1× starts it as attractive as %s. Routed on the same network as",
    "every other school. Its places count in the total below; it has no finance or attainment figures."),
    inp$comart$w_from_short))

  sim <- reactive({
    req(input$design, input$site, input$year)
    w <- w_now(); p <- pan_now()
    req(all(is.finite(w)), all(is.finite(p)))
    run_sim(inp, w_mult = w, pans = p, site = input$site,
            design = input$design, year = input$year, gamma = input$gamma,
            exclusive = input$exclusive, rules = rule_args(),
            comart = comart_arg())
  })
  met <- reactive(outcomes(inp, sim()))

  # ---- Download this run -------------------------------------------------
  output$download_xlsx <- downloadHandler(
    filename = function()
      sprintf("bh-school-simulator_%s_%s.xlsx",
              gsub("[^A-Za-z0-9]+", "-", input$preset %||% "run"),
              format(Sys.time(), "%Y%m%d-%H%M")),
    content = function(file) {
      settings <- list(design = input$design, site = input$site, year = input$year,
                       gamma = input$gamma, exclusive = input$exclusive,
                       rule = input$rule %||% "priorities", p6 = input$p6 %||% 5,
                       fsm = isTRUE(input$fsm %||% TRUE),
                       targeted = isTRUE(input$targeted %||% FALSE),
                       comart = comart_arg())
      writexl::write_xlsx(
        scenario_export(inp, sim(), met(), input$preset, settings, w_now(), pan_now()),
        file)
    })

  # ---- Headline strip ------------------------------------------------
  kpi <- function(value, label, detail = NULL, colour = INK)
    div(class = "kpi",
        div(class = "v", style = paste0("color:", colour), value),
        div(class = "l", label),
        if (!is.null(detail)) div(class = "d", detail))

  output$kpi_fill <- renderUI({ m <- met()
    kpi(sprintf("%.0f%%", 100 * m$fill), "Places filled",
        sprintf("%s of %s", fmt_n(m$intake), fmt_n(m$pan)),
        if (m$fill >= 0.95) OK else if (m$fill >= 0.85) WARN else BAD) })

  output$kpi_short <- renderUI({ m <- met()
    kpi(m$below_pan, "Schools short of their number",
        sprintf("%s empty places", fmt_n(m$empty)),
        if (m$below_pan == 0) OK else if (m$below_pan <= 3) WARN else BAD) })

  output$kpi_money <- renderUI({ m <- met()
    kpi(gbp_app(m$city_gap), "City-wide annual balance",
        sprintf("%d school%s in deficit", m$in_deficit,
                if (m$in_deficit == 1) "" else "s"),
        if (m$city_gap >= 0) OK else BAD) })

  output$kpi_seg <- renderUI({ m <- met()
    kpi(sprintf("%.3f", m$gorard), "Segregation of intakes",
        sprintf("%.0f%% to %.0f%% deprived", 100 * m$intake_lo, 100 * m$intake_hi),
        if (m$gorard <= 0.15) OK else if (m$gorard <= 0.25) WARN else BAD) })

  output$kpi_travel <- renderUI({ m <- met()
    kpi(sprintf("%.1f min", m$mean_min), "Mean journey",
        sprintf("%.0f%% over 40 min", 100 * m$over_40),
        if (m$mean_min <= 30) OK else if (m$mean_min <= 36) WARN else BAD) })

  output$kpi_gap <- renderUI({ m <- met()
    kpi(sprintf("%+.1f min", m$dep_gap), "Deprived children travel",
        "further than everyone else",
        if (m$dep_gap <= 0) OK else if (m$dep_gap <= 5) WARN else BAD) })

  # How much work the chosen map is doing. Without this the design
  # control looks inert, when what is actually true is that it moves
  # very few children at the strength families behave as though
  # catchments have.
  output$design_note <- renderText({
    base <- inp$designs[["Current catchments"]]$zone
    z <- inp$designs[[input$design]]$zone
    regrouped <- sum(z[names(base)] != base, na.rm = TRUE)
    now <- run_sim(inp, w_mult = w_now(), pans = pan_now(), site = input$site,
                   design = "Current catchments", year = input$year,
                   gamma = input$gamma, exclusive = input$exclusive,
                   rules = rule_args(), comart = comart_arg())
    moved <- sum(pmax(0, sim()$schools$intake - now$schools$intake))
    if (regrouped == 0)
      sprintf("This is the map in force. %.0f%% of children are modelled as attending a school in their own catchment.",
              100 * sim()$in_catch_share)
    else
      sprintf("This map regroups %d of the %d neighbourhood-catchment zones against the one in force, and at this catchment strength it moves %s children between schools.",
              regrouped, length(base), fmt_n(moved))
  })

  # ---- What the sliders add up to --------------------------------------
  # The admission numbers above are set school by school, and nobody
  # adding ten of them in their head notices that the total has drifted
  # to a fifth more places than there are children. This says so, in the
  # year the rest of the app is set to.
  output$pan_total <- renderUI({
    req(input$year)
    places <- sum(pan_now()) + comart_places()
    kids <- inp$demand$cohort[match(input$year, inp$demand$year)]
    pct <- 100 * places / kids
    spare <- places - kids
    col <- if (pct <= 105) OK else if (pct <= 115) WARN else BAD
    fill <- max(2, min(100, 100 / max(pct, 1) * 100))

    div(class = "kpi",
        div(class = "v", style = paste0("color:", col),
            sprintf("%.0f%%", pct)),
        div(class = "l", "Places, against the children"),
        div(style = "height:9px;border-radius:4px;background:#e8e8e8;margin:5px 0 4px;position:relative;overflow:hidden",
            div(style = paste0("position:absolute;left:0;top:0;bottom:0;width:",
                               sprintf("%.1f", fill), "%;background:", col))),
        div(class = "d",
            sprintf("%s places for %s children in %d — %s %s",
                    fmt_n(places), fmt_n(kids), input$year,
                    fmt_n(abs(spare)),
                    if (spare >= 0) "spare" else "short")))
  })

  # ---- The children, and the places set against them -------------------
  # Figure 11 of the report, with the admission numbers the sliders are
  # currently set to drawn across it. The gap between the line and the
  # bar is the surplus, and it is the thing the whole app is about.
  output$p_cohort <- renderPlot({
    ob <- inp$cohort$observed
    places <- sum(pan_now()) + comart_places()
    yr <- input$year
    kids <- inp$demand$cohort[match(yr, inp$demand$year)]

    # The forward line is the app's OWN demand series, not the
    # reception-cohort projection behind figure 11 of the report. They
    # are both cohort projections and they differ by up to 165 children
    # in 2033; drawing one as the line and putting the marker on the
    # other would have shown a dot floating off its own trend. The model
    # scales demand by this series, so this is the one that has to be
    # drawn.
    fw <- inp$demand %>%
      transmute(year, kids = cohort, seg = if_else(extrapolated,
                                                   "extrapolated", "projected"))
    solid <- fw %>% filter(seg == "projected")
    dash <- fw %>% filter(year >= max(solid$year))

    ggplot() +
      geom_line(data = dash, aes(year, kids), colour = "#b2182b",
                linewidth = 1, linetype = "12") +
      geom_line(data = solid, aes(year, kids), colour = "#b2182b",
                linewidth = 1, linetype = "22") +
      geom_line(data = ob, aes(year, y7), colour = "grey25", linewidth = 1) +
      geom_hline(yintercept = places, colour = "#2a78d6", linewidth = 1) +
      annotate("segment", x = yr, xend = yr, y = kids, yend = places,
               colour = "#2a78d6", linewidth = 0.5, linetype = "31") +
      annotate("point", x = yr, y = kids, colour = "#b2182b", size = 2.6) +
      annotate("text", x = min(ob$year), y = places,
               label = sprintf("%s places, as you have set them", fmt_n(places)),
               colour = "#2a78d6", hjust = 0, vjust = -0.6, size = 3.2,
               fontface = "bold") +
      annotate("text", x = yr, y = kids,
               label = sprintf("%s children in %d", fmt_n(kids), yr),
               colour = "#b2182b", hjust = if (yr > 2030) 1.08 else -0.08,
               vjust = 1.4, size = 3.2) +
      scale_y_continuous(labels = scales::label_comma(),
                         limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
      scale_x_continuous(breaks = seq(2010, 2035, 5)) +
      labs(x = NULL, y = NULL,
           title = "Year 7 children, and the places set against them",
           subtitle = sprintf("Grey is what happened. Red is projected from children already in school, and extrapolated after %d.",
                              max(inp$demand$year[!inp$demand$extrapolated]))) +
      theme_minimal(11) +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", size = 12),
            plot.subtitle = element_text(colour = "grey35", size = 9),
            plot.margin = margin(2, 8, 2, 2))
  })

  # ---- Map ------------------------------------------------------------
  # There is no renderLeaflet here. The map is Leaflet proper, set up in
  # www/map.js; this sends it the two things it needs. See that file for
  # why. Redrawing is a message rather than a re-render, so the view the
  # user has panned to survives every slider move, exactly as
  # leafletProxy() used to arrange.
  observe({
    s <- sim()
    sch <- s$schools %>% filter(city) %>%
      inner_join(inp$schools %>% select(name, lon, lat, elm_lon, elm_lat),
                 by = "name")
    elm <- identical(input$site, "elm")

    # Demand before the ceiling, per school, and where it sits on the scale.
    wanted <- tapply(s$flows$wanted, s$flows$name, sum)
    sch$wanted <- dplyr::coalesce(as.numeric(wanted[sch$name]), 0)
    full <- sch$fill >= 0.995
    sch$pressure <- ifelse(full, pmax(1, sch$wanted / sch$pan), sch$fill)
    sch$col <- pressure_col(sch$pressure)
    sch$lab <- sprintf(
      "<b>%s</b><br>Intake %s of %s places (%.0f%%)<br>%s<br>Mean journey %.0f min",
      sch$short, fmt_n(sch$intake), fmt_n(sch$pan), 100 * sch$fill,
      ifelse(!full,
             sprintf("%s places short of its admission number", fmt_n(sch$pan - sch$intake)),
             ifelse(sch$wanted > 1.005 * sch$pan,
                    sprintf("Demand %.2f× its places: about %s more children want it than it can take",
                            sch$wanted / sch$pan, fmt_n(sch$wanted - sch$pan)),
                    "Full, with demand close to its places")),
      sch$mean_min)

    dname <- switch(input$design,
                    "Current catchments" = "Current catchments",
                    "Power diagram" = "Power diagram (proximity and capacity)",
                    "Flow regions, pairs kept" = "Flow regions, pairs kept",
                    "Flow regions, one per school" = "Flow regions, one per school",
                    "Flow regions, Longhill at Elm Grove" = "Flow regions, Elm Grove, PAN 150")
    gj <- unname(inp$design_geojson[dname])
    if (is.na(gj)) gj <- NULL

    session$sendCustomMessage("map_draw", list(
      geojson = gj,
      legend = MAP_LEGEND,
      dots = unname(lapply(seq_len(nrow(sch)), function(i) list(
        lon = if (elm) sch$elm_lon[i] else sch$lon[i],
        lat = if (elm) sch$elm_lat[i] else sch$lat[i],
        r   = max(5, sqrt(sch$intake[i]) * 1.5),
        col = sch$col[i],
        lab = sch$lab[i])))))
  })

  # ---- Places ---------------------------------------------------------
  output$p_places <- renderPlot({
    s <- sim()$schools %>% filter(city) %>%
      mutate(short = stats::reorder(short, fill))
    ggplot(s, aes(fill, short)) +
      geom_vline(xintercept = 1, colour = "grey40", linetype = "31") +
      geom_segment(aes(x = 0, xend = fill, yend = short), colour = "grey78",
                   linewidth = 1.1) +
      geom_point(aes(colour = fill >= 0.995), size = 3.6) +
      geom_text(aes(label = sprintf("%s / %s", fmt_n(intake), fmt_n(pan))),
                hjust = 0, nudge_x = 0.035, size = 3, colour = "grey25") +
      scale_colour_manual(values = c(`TRUE` = OK, `FALSE` = BAD), guide = "none") +
      scale_x_continuous(labels = scales::label_percent(),
                         limits = c(0, max(1.25, max(s$fill) + 0.18))) +
      labs(x = "Intake against admission number", y = NULL,
           title = "How full each school is") +
      theme_minimal(12) +
      theme(panel.grid.major.y = element_blank(),
            plot.title = element_text(face = "bold"))
  })

  output$t_places <- renderTable({
    sim()$schools %>% filter(city) %>% arrange(fill) %>%
      transmute(School = short, `Admission number` = fmt_n(pan),
                Intake = fmt_n(intake), Fill = sprintf("%.0f%%", 100 * fill),
                `Mean journey` = sprintf("%.0f min", mean_min))
  }, striped = TRUE, width = "100%")

  # ---- Money -----------------------------------------------------------
  output$p_money <- renderPlot({
    f <- met()$by_school %>%
      mutate(short = stats::reorder(short, gap_pct))
    ggplot(f, aes(gap_pct, short)) +
      geom_vline(xintercept = 0, colour = "grey40") +
      geom_segment(aes(x = 0, xend = gap_pct, yend = short),
                   colour = "grey78", linewidth = 1.1) +
      geom_point(aes(colour = gap >= 0), size = 3.6) +
      geom_text(aes(label = gbp_app(gap),
                    hjust = ifelse(gap_pct < 0, 1.18, -0.18)),
                size = 3, colour = "grey25") +
      scale_colour_manual(values = c(`TRUE` = OK, `FALSE` = BAD), guide = "none") +
      scale_x_continuous(labels = scales::label_percent(),
                         expand = expansion(mult = 0.17)) +
      labs(x = "Annual surplus or deficit, as a share of income", y = NULL,
           title = "Where each school's budget lands",
           subtitle = "Roll held at five times this intake; a school sheds three quarters of the cost when funding goes.") +
      theme_minimal(12) +
      theme(panel.grid.major.y = element_blank(),
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(colour = "grey35", size = 10))
  })

  output$t_money <- renderTable({
    met()$by_school %>% arrange(gap_pct) %>%
      transmute(School = short,
                `Roll now` = fmt_n(funded_roll),
                `Roll at this intake` = fmt_n(roll_ss),
                `Funding change` = gbp_app(funding_change),
                `Annual balance` = gbp_app(gap),
                Reserve = gbp_app(reserve),
                # A school a hundred pounds in deficit is not "670 years
                # of reserve"; past a couple of decades the number is
                # noise and says so.
                `Years of reserve` = dplyr::case_when(
                  is.infinite(years_left) ~ "in surplus",
                  years_left == 0 ~ "overdrawn",
                  years_left > 20 ~ "20+",
                  TRUE ~ sprintf("%.1f", years_left)))
  }, striped = TRUE, width = "100%")

  # ---- Who gets a place in their own catchment -------------------------
  # Blue and orange rather than green and red: the map already uses green
  # and red for whether a school fills, and these bars are a different
  # question. Grey for the faith schools, which are outside every
  # catchment by design rather than by displacement.
  # Blue for a place at home, grey for a child who chose to go elsewhere
  # and got it, orange for one the ceiling pushed out. Only the orange is
  # a place the system failed to provide, and the colours say so.
  # Purple for a child placed under priority 6: they chose to leave, but
  # through a rule that exists to let them, so it is not the same grey.
  # Choice now splits three ways: two greys for the modelled choices, a
  # sand for children offered a place outside the city, which comes from
  # published counts rather than the model.
  CATCH_COL <- c(`Their own catchment`          = "#2a78d6",
                 `Left for a faith school`      = "#7d8793",
                 `Left for another city school` = "#b9c1c9",
                 `Left the city: East Sussex schools` = "#c4a064",
                 `Left the city: elsewhere (estimate)` = "#e8dcc2",
                 `Through priority 6`           = "#7b61c9",
                 `Displaced`                    = "#eb6834")

  catch_plot <- function(b, title, subtitle, base = 12, label_all = TRUE) {
    b <- b %>% mutate(where = factor(where, names(CATCH_COL)))
    ord <- b %>% filter(where == "Displaced") %>% arrange(share, label)
    b <- b %>% mutate(label = factor(label, ord$label))
    lab <- b %>% filter(where != "Their own catchment") %>%
      group_by(label) %>% summarise(share = sum(share), .groups = "drop")
    rat <- ord %>% mutate(label = factor(label, ord$label)) %>% filter(share > 0.005)

    ggplot(b, aes(share, label, fill = where)) +
      geom_col(width = 0.74, colour = "white", linewidth = 0.6,
               position = position_stack(reverse = TRUE)) +
      geom_text(data = lab, aes(x = 1.02, y = label,
                                label = sprintf("%.0f%%", 100 * share)),
                inherit.aes = FALSE, hjust = 0, size = base * 0.26,
                colour = "grey35") +
      geom_text(data = rat, aes(x = 1.02, y = label,
                                label = sprintf("(%.0f%% displaced)", 100 * share)),
                inherit.aes = FALSE, hjust = 0, size = base * 0.24,
                colour = "#b8501f", fontface = "bold", nudge_x = 0.10) +
      scale_fill_manual(values = CATCH_COL, name = NULL) +
      scale_x_continuous(labels = scales::label_percent(),
                         limits = c(0, 1.34), breaks = seq(0, 1, 0.25),
                         expand = expansion(mult = 0)) +
      labs(x = "Children living in the catchment", y = NULL,
           title = title, subtitle = subtitle) +
      theme_minimal(base) +
      theme(legend.position = "top",
            panel.grid.major.y = element_blank(),
            plot.title = element_text(face = "bold", size = base),
            plot.subtitle = element_text(colour = "grey35", size = base * 0.8),
            legend.text = element_text(size = base * 0.82),
            axis.title.x = element_text(size = base * 0.8, colour = "grey35"))
  }

  output$p_catch <- renderPlot({
    catch_plot(met()$catchment$by_catch,
               "Who has to leave their catchment, and why",
               paste(strwrap(paste(
                 "Dark grey chose a faith school, light grey another city school; sand went to",
                 "Priory, Peacehaven, Seahaven or Seaford Head, pale sand elsewhere outside the city (an estimate);",
                 "purple was placed under priority 6; orange was pushed out of a full",
                 "catchment school. Only the orange is a place the system could not provide."),
                 width = 84), collapse = "\n"))
  })

  output$p_catch_map <- renderPlot({
    catch_plot(met()$catchment$by_catch,
               "Who leaves their catchment, and why",
               "Greys and sand chose to go; orange was pushed out of a full school.",
               base = 9.5)
  })

  output$t_catch <- renderTable({
    m <- met()$catchment
    b <- m$by_catch
    # This was a pivot_wider. The long frame holds exactly one row per
    # catchment per bucket, so a match() is the whole of the pivot, and
    # tidyr need not be deployed for it.
    at <- function(lab, w) b$n[match(paste(lab, w), paste(b$label, b$where))]
    m$outside %>%
      select(label, living, outside_share, displaced_share) %>%
      arrange(desc(displaced_share), desc(outside_share)) %>%
      transmute(Catchment = label,
                `Children living there` = fmt_n(living),
                `Place at home` = fmt_n(at(label, "Their own catchment")),
                `To a faith school` = fmt_n(at(label, "Left for a faith school")),
                `To another city school` = fmt_n(at(label, "Left for another city school")),
                `East Sussex schools` = fmt_n(at(label, "Left the city: East Sussex schools")),
                `Elsewhere outside (est.)` = fmt_n(at(label, "Left the city: elsewhere (estimate)")),
                `Through priority 6` = fmt_n(at(label, "Through priority 6")),
                `Displaced` = fmt_n(at(label, "Displaced")),
                `Outside` = sprintf("%.0f%%", 100 * outside_share),
                `of which displaced` = sprintf("%.0f%%", 100 * displaced_share))
  }, striped = TRUE, width = "100%")

  output$catch_note <- renderUI({
    m <- met()$catchment
    HTML(sprintf(paste0(
      "<p><b>Two different things put a child outside their catchment, and ",
      "they are not the same problem.</b> %.0f%% of the cohort ends up at a ",
      "school outside the catchment they live in. Almost all of that — ",
      "%.0f%% of the cohort — is children who preferred an out-of-catchment ",
      "school and got it, their own catchment school having had room: ",
      "%.0f%% of the cohort at the two faith schools, which have no catchment ",
      "at all, %.0f%% at another city school, about %s children at the four East ",
      "Sussex schools the model includes - Priory in Lewes above all - and about ",
      "%s more offered a place elsewhere outside the city. That last figure is ",
      "not modelled: it is the adjudicator's published count for each catchment, ",
      "less what the model places in East Sussex, scaled with the cohort. ",
      "Only %.0f%% were displaced: they would have taken a place at ",
      "home and the capacity ceiling did not have one.</p>",
      "<p><b>Displacement only happens where the schools fill.</b> %s loses ",
      "%.0f%% of its children and displaces none of them, because its schools ",
      "have room to spare — that is a school nobody is choosing, not a ",
      "system failing to provide. %s is the catchment where children are ",
      "actually turned away, at %.0f%%.</p>",
      "<p>The two move in opposite directions. Turning up how much the ",
      "catchment counts cuts the leaving, but it converts what is left into ",
      "displacement: more children want a place at home, and the full schools ",
      "still cannot take them. The number that measures a system failing ",
      "its families is the orange one, not the total.</p>"),
      100 * m$outside_share, 100 * m$chose_share,
      100 * m$faith_choice_share, 100 * m$other_city_share, fmt_n(m$left_es),
      fmt_n(m$left_other),
      100 * m$displaced_share, m$worst, 100 * m$worst_share,
      m$worst_displaced, 100 * m$worst_displaced_share))
  })

  # ---- The council's priorities ---------------------------------------
  output$rule_note <- renderText({
    r <- sim()
    if (is.null(r$tiers)) return("")
    t <- r$tiers
    held <- (input$p6 %||% 5) / 100 * sum(r$cap[t$name])
    sprintf("Priority 6 fills %s of the %s places it holds at the six community schools; %s children are placed under the FSM priority.",
            fmt_n(sum(t$p6)), fmt_n(held), fmt_n(sum(t$p45_in + t$p45_out)))
  })

  PRIO_COL <- c(`FSM (4-5)` = "#1baf7a",
                `Single-school catchments (6)` = "#7b61c9",
                `Catchment, with siblings, SEN, looked-after (1-3, 7)` = "#2a78d6",
                `Other (8)` = "#9aa5b1")

  output$p_priority <- renderPlot({
    r <- sim()
    if (is.null(r$tiers))
      return(ggplot() +
        annotate("text", 0, 0, size = 4.2, colour = "grey40",
                 label = "Set 'When a school is full' to the council's priorities\nto see how each community school's places are filled.") +
        theme_void())
    sh <- setNames(inp$schools$short, inp$schools$name)
    b <- names(PRIO_COL)
    mod <- r$tiers
    long <- dplyr::bind_rows(
      tibble(school = mod$name, src = "model", bucket = b[1], n = mod$p45_in + mod$p45_out),
      tibble(school = mod$name, src = "model", bucket = b[2], n = mod$p6),
      tibble(school = mod$name, src = "model", bucket = b[3], n = mod$p7),
      tibble(school = mod$name, src = "model", bucket = b[4], n = mod$p8),
      inp$rules$outturn_2026 %>%
        mutate(bucket = dplyr::case_when(priority %in% 4:5 ~ b[1],
                                         priority == 6 ~ b[2],
                                         priority == 8 ~ b[4],
                                         TRUE ~ b[3])) %>%
        group_by(school, bucket) %>%
        summarise(n = sum(offers), .groups = "drop") %>%
        mutate(src = "September 2026 offers"))
    long <- long %>%
      mutate(row = paste0(sh[school], ifelse(src == "model", "  (model)", "  (Sept 2026 offers)")),
             bucket = factor(bucket, b))
    lev <- unlist(lapply(sort(sh[mod$name]), function(s)
      c(paste0(s, "  (model)"), paste0(s, "  (Sept 2026 offers)"))))
    long <- long %>% filter(row %in% lev) %>% mutate(row = factor(row, rev(lev)))

    ggplot(long, aes(n, row, fill = bucket)) +
      geom_col(width = 0.72, colour = "white", linewidth = 0.5,
               position = position_stack(reverse = TRUE)) +
      scale_fill_manual(values = PRIO_COL, name = NULL, drop = FALSE) +
      labs(x = "Places", y = NULL,
           title = "How each community school's places are filled",
           subtitle = paste(strwrap(paste(
             "Under the rules as set. The published September 2026 offers sit under the three",
             "schools that rationed, for comparison rather than as a target: they were made to",
             "a different cohort, and the model cannot separate priorities 1-3 from the catchment."),
             width = 100), collapse = "\n")) +
      theme_minimal(11) +
      theme(legend.position = "top", legend.text = element_text(size = 9),
            panel.grid.major.y = element_blank(),
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(colour = "grey35", size = 9))
  })

  output$priority_note <- renderUI({
    rl <- inp$rules
    HTML(sprintf(paste0(
      "<p><b>How the priorities run.</b> At the six community schools a full ",
      "school is filled tier by tier - FSM children up to 30%% of places, then ",
      "children from single-school catchments up to the priority-6 share, then ",
      "the catchment, then everyone else - and by lottery within each tier. ",
      "The two academies and the two faith schools keep the published model's ",
      "single lottery; their own criteria are not in published data.</p>",
      "<p><b>What is fitted, and what is not.</b> Who is eligible for FSM and ",
      "claims it is not published by neighbourhood. The IDACI score gives the ",
      "shape, and one constant (%.2f times it, %.0f%%%% of the city's children) ",
      "sets the level so that the model's FSM offers at Blatchington Mill, ",
      "Stringer and Varndean together match the %d the council made in ",
      "September 2026. How those places split between the three, and ",
      "everything under priority 6, is not fitted.</p>",
      "<p><b>What is left out.</b> Siblings, SEN and looked-after children ",
      "(priorities 1-3) sit inside the catchment tier, which is where most of ",
      "them live. Families are held to the same preferences whatever the ",
      "rules, when in practice a family with a real chance at a school is more ",
      "likely to name it.</p>"),
      rl$fsm_takeup, 100 * rl$fsm_city,
      sum(rl$outturn_2026$offers[rl$outturn_2026$priority %in% 4:5])))
  })

  # ---- Fairness --------------------------------------------------------
  output$p_fair <- renderPlot({
    m <- met()
    d <- m$mix %>% filter(name %in% inp$city, n > 0) %>%
      inner_join(inp$schools %>% select(name, short), by = "name") %>%
      mutate(short = stats::reorder(short, dep_share))
    city_share <- sum(d$dep_n) / sum(d$n)
    ggplot(d, aes(dep_share, short)) +
      geom_vline(xintercept = city_share, colour = "grey40", linetype = "31") +
      geom_segment(aes(x = city_share, xend = dep_share, yend = short),
                   colour = "grey78", linewidth = 1.1) +
      geom_point(size = 3.6, colour = "#0d366b") +
      geom_text(aes(label = sprintf("%.0f%%", 100 * dep_share)),
                hjust = 0, nudge_x = 0.015, size = 3, colour = "grey25") +
      annotate("text", x = city_share, y = 0.55,
               label = sprintf("city average %.0f%%", 100 * city_share),
               size = 3, colour = "grey40", hjust = 0.5, vjust = 1) +
      scale_x_continuous(labels = scales::label_percent(),
                         expand = expansion(mult = c(0.04, 0.14))) +
      labs(x = "Share of the intake from the most deprived neighbourhoods",
           y = NULL,
           title = sprintf("Gorard index %.3f", m$gorard),
           subtitle = "Each school's modelled intake, against the city as a whole.") +
      theme_minimal(12) +
      theme(panel.grid.major.y = element_blank(),
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(colour = "grey35", size = 10))
  })

  # ---- Why priority 6 can raise segregation ------------------------------
  # Found by moving the slider: 0% to 15% under the council's priorities
  # takes Gorard from about 0.152 to 0.162 in 2026. The live line compares
  # the run on screen with the same run and priority 6 switched off, so
  # the note reports what THIS configuration does rather than a fixed
  # number.
  output$fair_note <- renderUI({
    m <- met()
    live <- ""
    if (identical(input$rule, "priorities") && (input$p6 %||% 5) > 0) {
      r0 <- run_sim(inp, w_mult = w_now(), pans = pan_now(), site = input$site,
                    design = input$design, year = input$year, gamma = input$gamma,
                    exclusive = input$exclusive,
                    rules = utils::modifyList(rule_args(), list(p6_share = 0)),
                    comart = comart_arg())
      g0 <- outcomes(inp, r0)$gorard
      live <- sprintf(paste0(
        "<p><b>In this run:</b> Gorard %.3f with priority 6 at %d%% of places, ",
        "against %.3f with it switched off (%+.1f%%), and %s children placed under it.</p>"),
        m$gorard, as.integer(input$p6), g0, 100 * (m$gorard / g0 - 1),
        fmt_n(sum(sim()$flows$p6)))
    }
    HTML(paste0(live,
      "<p><b>Priority 6 can make the city's intakes slightly more segregated, not less.</b> ",
      "Under the council's priorities in 2026, raising it from 0% to 15% of places moves ",
      "the Gorard index from about 0.152 to 0.162; past about 17% there is no more demand ",
      "for the places, and it stops moving. Two things in the model drive it.</p>",
      "<p><b>The children who use it still go to the popular school nearest them.</b> ",
      "Priority 6 lets a child cross a catchment boundary, but families still choose on ",
      "distance, and the popular schools do not all sit among the same kind of ",
      "neighbourhood. Brighton Aldridge's and ",
      "Longhill's catchments send their priority-6 children mostly to Varndean, whose ",
      "intake is already more deprived than the city's; the catchment children they ",
      "displace move next door to Stringer, which barely changes. Portslade's go west, ",
      "to Blatchington Mill and Hove Park, whose intakes are already the least deprived. ",
      "A school above the city average moves further above it, and one below further below.</p>",
      "<p><b>The families who use it are the better-off ones in their catchment.</b> ",
      "Priority-6 places won from Brighton Aldridge's catchment go to neighbourhoods about ",
      "two-thirds deprived, where the catchment as a whole is nearer four in five; from ",
      "Portslade's, fewer than one in five against nearly one in three. A lottery does not ",
      "change that: it is who applies out of catchment that sets it, and in the model that ",
      "follows how strongly each neighbourhood is drawn to each school.</p>",
      "<p>The effect is small beside what a catchment redesign does, and deprivation here ",
      "is a neighbourhood measure, so a better-off family in a deprived neighbourhood ",
      "counts as deprived. Priority 6 may still widen access for the children who use it. ",
      "What it does not do, on this measure, is even out the intakes.</p>"))
  })

  # ---- Travel ----------------------------------------------------------
  output$p_travel <- renderPlot({
    # City schools only, like the journey figures: the routed times to the
    # East Sussex schools go through Brighton by bus.
    f <- sim()$flows %>% filter(name %in% inp$city)
    brk <- seq(0, 90, 5)
    h <- f %>% mutate(b = cut(cij, brk, labels = brk[-length(brk)] + 2.5)) %>%
      group_by(b) %>% summarise(n = sum(flow), .groups = "drop") %>%
      mutate(b = as.numeric(as.character(b)))
    m <- met()
    ggplot(h, aes(b, n)) +
      geom_col(fill = "#2a78d6", width = 4.4) +
      geom_vline(xintercept = m$mean_min, colour = BAD, linewidth = 0.8) +
      annotate("text", x = m$mean_min, y = max(h$n), hjust = -0.08, vjust = 1,
               label = sprintf("mean %.1f min", m$mean_min),
               colour = BAD, size = 3.4) +
      labs(x = "Journey to the school the model sends them to", y = "Children",
           title = "How far the city travels",
           subtitle = sprintf("%.0f%% over 40 minutes; %s child-kilometres a day, both ways.",
                              100 * m$over_40, fmt_n(m$child_km_day))) +
      theme_minimal(12) +
      theme(plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(colour = "grey35", size = 10))
  })

  # ---- The two live bars beside the map --------------------------------
  # Moving a school's attractiveness slider says, implicitly, that its
  # published numbers changed. These say which numbers, and put them
  # against the national distribution so the size of the claim is
  # visible rather than buried in a multiplier.
  live <- reactive({
    w <- w_now()
    CITY %>%
      mutate(mult = unname(w[name]),
             att8_new = att8 + att8_points(inp, mult),
             abs_new = absence_for(inp, absence, att8, att8_new),
             moved = abs(mult - 1) > 0.01)
  })

  # A rug of national deciles, with the median and the tails named. All
  # nine are drawn so the spacing shows how bunched the middle is.
  decile_layer <- function(q) {
    d <- as.numeric(q[2:10])
    list(geom_vline(xintercept = d, colour = "grey88", linewidth = 0.4),
         geom_vline(xintercept = as.numeric(q[c("10%", "50%", "90%")]),
                    colour = "grey62", linewidth = 0.5, linetype = "31"))
  }

  live_plot <- function(d, xnow, xnew, q, title, xlab, lims, digits = 1) {
    # Two lines at most, and wrapped: the column is 430px wide and a
    # one-line axis title was being cut off mid-word.
    xlab <- paste(strwrap(xlab, width = 58), collapse = "\n")
    # The row order and the axis are FIXED, on the school's own current
    # value and on England's full range. Both used to be computed from
    # the data, so moving a slider rescaled the frame and reordered the
    # rows underneath the dot that had just moved - which is why the
    # charts looked inert when they were in fact responding. A dot now
    # moves against a backdrop that stays still.
    d <- d %>% mutate(short = factor(short, d$short[order(d[[xnow]])]))
    ggplot(d, aes(y = short)) +
      decile_layer(q) +
      geom_segment(aes(x = .data[[xnow]], xend = .data[[xnew]], yend = short),
                   colour = "grey70", linewidth = 0.9) +
      geom_point(aes(x = .data[[xnow]]), size = 2, colour = "grey45") +
      geom_point(aes(x = .data[[xnew]], colour = moved), size = 2.6) +
      geom_text(data = d %>% filter(moved),
                aes(x = .data[[xnew]],
                    label = sprintf("%.*f", digits, .data[[xnew]])),
                colour = "#2a78d6", size = 2.9, vjust = -1, fontface = "bold") +
      scale_colour_manual(values = c(`TRUE` = "#2a78d6", `FALSE` = "grey45"),
                          guide = "none") +
      scale_x_continuous(limits = lims, oob = scales::squish) +
      labs(x = xlab, y = NULL, title = title) +
      theme_minimal(10) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", size = 11),
            plot.subtitle = element_text(colour = "grey35", size = 8.5),
            axis.title.x = element_text(size = 8.5, colour = "grey35"),
            axis.text.y = element_text(size = 8),
            plot.margin = margin(4, 8, 2, 2))
  }

  output$p_att_live <- renderPlot({
    q <- inp$attain$att8_deciles
    live_plot(live(), "att8", "att8_new", q,
              "Attainment 8 the slider implies",
              sprintf("Score. Ticks are England's deciles, dashed the 10th (%.0f), median (%.0f) and 90th (%.0f)",
                      q[["10%"]], q[["50%"]], q[["90%"]]),
              lims = inp$attain$att8_lims, digits = 1)
  })

  output$p_abs_live <- renderPlot({
    q <- inp$attain$absence$national$q
    live_plot(live(), "absence", "abs_new", q,
              "Absence rate that would go with it",
              sprintf("Per cent of sessions missed, lower better. Dashed: England's 10th (%.1f), median (%.1f), 90th (%.1f)",
                      q[["10%"]], q[["50%"]], q[["90%"]]),
              lims = inp$attain$abs_lims, digits = 1)
  })

  # ---- Attainment ------------------------------------------------------
  # What each school's attractiveness is worth in the one published
  # number families appear to respond to, and what it would have to
  # reach to fill the places it is offering.
  att_tab <- reactive({
    w <- w_now(); p <- pan_now()
    bind_rows(lapply(seq_len(nrow(CITY)), function(i) {
      nm <- CITY$name[i]
      s <- solve_w_for_pan(inp, nm, target_fill = 1, w_mult = w, pans = p,
                           rules = rule_args(), gamma = input$gamma,
                           exclusive = input$exclusive,
                           site = input$site, design = input$design,
                           year = input$year, comart = comart_arg())
      data.frame(
        name = nm, short = CITY$short[i], att8 = CITY$att8[i],
        set_at = CITY$att8[i] + att8_points(inp, w[[nm]]),
        needed = if (is.finite(s$multiplier))
          CITY$att8[i] + att8_points(inp, s$multiplier) else NA_real_,
        pan = p[[nm]], stringsAsFactors = FALSE)
    })) %>% mutate(gap = needed - att8)
  })

  output$p_att <- renderPlot({
    a <- att_tab() %>% mutate(short = stats::reorder(short, att8))
    q <- inp$attain$national$q
    # The three reference lines are named in the subtitle rather than
    # labelled in the panel: a numeric y on a discrete axis is what
    # "Discrete value supplied to a continuous scale" was complaining
    # about, and the plot did not draw at all.
    refs <- c(q[["50%"]], q[["90%"]], inp$attain$city$max)
    ggplot(a, aes(y = short)) +
      geom_vline(xintercept = refs, colour = "grey72", linetype = "31") +
      geom_segment(aes(x = att8, xend = needed, yend = short),
                   colour = "grey78", linewidth = 1.1, na.rm = TRUE) +
      geom_point(aes(x = att8), size = 3.4, colour = "#1f3b57") +
      geom_point(aes(x = needed), size = 3.4, colour = BAD, na.rm = TRUE) +
      geom_text(aes(x = needed, label = sprintf("+%.0f", gap)), hjust = -0.35,
                size = 3, colour = BAD, na.rm = TRUE) +
      scale_x_continuous(expand = expansion(mult = c(0.05, 0.12))) +
      labs(x = "Attainment 8 score", y = NULL,
           title = "What each school would have to score to fill its places",
           subtitle = strwrap(sprintf(paste(
             "Dark dot is this year's score, red is what it would take to fill at the admission number you have set.",
             "Dashed lines: England median %.0f, England top tenth %.0f, best in the city %.0f."),
             q[["50%"]], q[["90%"]], inp$attain$city$max), width = 118) %>%
             paste(collapse = "\n")) +
      theme_minimal(12) +
      theme(panel.grid.major.y = element_blank(),
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(colour = "grey35", size = 10))
  })

  output$t_att <- renderTable({
    att_tab() %>% arrange(desc(gap)) %>%
      transmute(School = short,
                `Admission number` = fmt_n(pan),
                `Attainment 8 now` = sprintf("%.1f", att8),
                `At this slider` = sprintf("%.1f", set_at),
                `Needed to fill` = ifelse(is.na(needed), "unreachable",
                                          sprintf("%.1f", needed)),
                `Points short` = ifelse(is.na(gap), "—",
                                        sprintf("%+.1f", gap)),
                `Where that would rank` = ifelse(
                  is.na(needed), "—",
                  sprintf("top %.0f%% in England",
                          pmax(1, round(100 - att8_percentile(inp, needed))))))
  }, striped = TRUE, width = "100%")

  output$att_note <- renderUI(HTML(sprintf(
    paste0("<p>Attainment 8 explains %.0f%% of the variation in the model's ",
           "attractiveness (M5) across the city's %d schools, on a log scale, and a point ",
           "is worth about %.0f%% more attractiveness. That fit is what converts ",
           "the attractiveness slider into points here.</p>",
           "<p><b>It is an association, not a lever.</b> Attainment 8 is ",
           "largely set by the intake a school receives — section 2 of the ",
           "document spends some time on how little of it a school controls. ",
           "Read these as <i>what it would take</i>, not as <i>what to do</i>. ",
           "A school that raised its score by fifteen points would almost ",
           "certainly have done so by changing who walks through the door, ",
           "which is the thing the rest of this app is about.</p>",
           "<p>England figures are %s state schools in %s: median %.1f, ",
           "top tenth above %.1f, highest %.1f.</p>"),
    100 * inp$attain$r2, inp$attain$n, 100 * inp$attain$per_point,
    fmt_n(inp$attain$national$n), inp$attain$national$year,
    inp$attain$national$q[["50%"]], inp$attain$national$q[["90%"]],
    inp$attain$national$q[["100%"]])))

  # ---- The inverse question --------------------------------------------
  output$solve_note <- renderText({
    req(input$solve_for)
    if (input$solve_for == "—")
      return("Pick a school and the app solves for the attractiveness that fills it, holding everything else where you have set it.")
    nm <- CITY$name[match(input$solve_for, CITY$short)]
    p <- pan_now()
    # The other schools stay where the user has set them: the question
    # is what THIS school needs given everything else on the screen.
    s <- solve_w_for_pan(inp, nm, target_fill = 1, w_mult = w_now(), pans = p,
                           rules = rule_args(), gamma = input$gamma,
                           exclusive = input$exclusive,
                         site = input$site, design = input$design,
                         year = input$year, comart = comart_arg())
    if (is.infinite(s$multiplier))
      sprintf("%s cannot fill %s places at any attractiveness: at 60 times its own it reaches %.0f%%. There are not enough children within reach.",
              input$solve_for, fmt_n(p[[nm]]), 100 * s$fill)
    else if (s$fill >= 0.999 && s$multiplier <= w_now()[[nm]] * 1.001)
      sprintf("%s already fills %s places where the sliders are now.",
              input$solve_for, fmt_n(p[[nm]]))
    else {
      pts <- att8_points(inp, s$multiplier)
      now <- inp$schools$att8[inp$schools$name == nm]
      ab0 <- inp$schools$absence[inp$schools$name == nm]
      ab1 <- absence_for(inp, ab0, now, now + pts)
      sprintf(paste("%s would need to be %.1f× as attractive to fill %s places.",
                    "On the one published number families appear to respond to, that is %+.1f Attainment 8 points — from %.1f to %.1f, which is %s.",
                    "Holding its intake still, the attainment model puts the absence rate that goes with that score at %.1f%%, against %.1f%% now — the %s percentile in England."),
              input$solve_for, s$multiplier, fmt_n(p[[nm]]), pts, now, now + pts,
              att8_context(inp, now + pts), ab1, ab0,
              scales::ordinal(pmax(1, round(absence_percentile(inp, ab1)))))
    }
  })

  output$caveats <- renderUI(HTML(paste0(
    "<p><b>This simulator is in beta.</b> Its outputs have not been fully ",
    "validated, so none of them should yet be taken as reliable. It shows ",
    "what a tool like this could do and the kinds of answers it could give, ",
    "not settled numbers.</p>",
    "<p><b>This is a model, calibrated at catchment level and no finer.</b> ",
    "The flows come from the full model in section 7 of the strategic view ",
    "(M5). Its catchment terms, school attractiveness and paired-catchment ",
    "exclusivity are fitted to the council's catchment-level preference table ",
    "from its evidence to the Schools Adjudicator; distance decay is set, not ",
    "fitted. It has never seen an individual application, and every number ",
    "here moves with its assumptions.</p>",
    "<p><b>Attractiveness is a single number per school.</b> Moving the slider ",
    "says 'suppose families wanted this school this much more'. It does not ",
    "say how that would be achieved, how long it would take, or whether it is ",
    "possible. The scale is the model's attractiveness (M5), balanced so the ",
    "model's demand for each school matches its share of first preferences ",
    "once distance and the catchment are accounted for; 1× is the school as it is.</p>",
    "<p><b>Some children do leave the city, and most of them are modelled.</b> ",
    "Four East Sussex schools are destinations: Priory School in Lewes, ",
    "Peacehaven, Seahaven and Seaford Head. Each has an attractiveness of its ",
    "own, fitted with one distance decay to the council's published answer to ",
    "a Freedom of Information request, which gives the 2024 offers by home ",
    "catchment and school - Longhill's catchment sent 38 children to Priory, ",
    "and fewer than five each to Peacehaven and Seahaven. They are chosen on ",
    "straight-line distance, because the bus network has no Woodingdean to ",
    "Lewes service and the routed journey goes through Brighton. They respond ",
    "to Longhill's site and attractiveness, but they are not rationed and ",
    "cannot be adjusted, and one year of suppressed counts is a thin basis. ",
    "Children offered places in West Sussex or London, a handful a year from ",
    "Hove, Portslade and Stringer / Varndean, are not modelled: the Catchments ",
    "tab shows them as an estimate, the Schools Adjudicator's published count ",
    "less what the model places in East Sussex. Children who go to independent ",
    "schools or move away are in neither, so every city school's intake is ",
    "still slightly high.</p>",
    "<p><b>Where a refused child goes is an average, not an allocation.</b> ",
    "When a school is full, the children it turns away are re-offered a place ",
    "only at schools with room: in the two paired catchments, the other school ",
    "of the pair first, and after that in proportion to where families in ",
    "their catchment put their second preferences. That stands in for the ",
    "rest of each family's list, which no published data contains, and it ",
    "averages over the random tie-break rather than drawing one. A real round ",
    "can place a few children differently, and third and later preferences ",
    "are not used at all.</p>",
    "<p><b>The two faith schools are the least reliable rows.</b> Cardinal ",
    "Newman and King's admit on criteria no published dataset contains, so the ",
    "model admits to them on distance alone.</p>",
    "<p><b>The money is a steady state, not a budget.</b> A roll of five times ",
    "the intake is where a school ends up if this configuration holds for five ",
    "years. Reserves, balances and funding rates are today's, in today's ",
    "prices, with no pay award, energy shock or capital receipt in them.</p>",
    "<p><b>The absence figures are an inversion, not a plan.</b> They come ",
    "from the attainment model in section 2.3: absence enters as an ",
    "elasticity, so holding a school's intake still, the rate that goes with ",
    "a target score can be read off backwards. Absence is no more a dial than ",
    "attainment is - a school with 15% absence and one with 6% differ mostly ",
    "in who attends them.</p>",
    "<p><b>Journeys here are expected journeys, not assigned ones.</b> ",
    "Every child has a probability of attending every school, so a sliver ",
    "of each neighbourhood counts as travelling right across the city. That ",
    "makes the mean longer than the figure in section 8, which assigns each ",
    "child to one school. Compare configurations with each other here; do not ",
    "read the level against the document.</p>",
    "<p><b>The catchment term describes families, not the rules.</b> It is ",
    "fitted catchment by catchment to what families ask for, and it is strong: ",
    "four in five first preferences from the Stringer / Varndean catchment go ",
    "to one of its two schools. It follows the neighbourhood when the map is ",
    "redrawn, because it describes how those families choose; whether families ",
    "would follow a new boundary as closely as the old one is not something ",
    "the data can say. Living in a catchment raises the odds of choosing that ",
    "school; it does not guarantee ",
    "a place. The app starts from the council's 2026/27 oversubscription ",
    "priorities; the Catchments tab says how they are run, and what they ",
    "cannot see. Switch 'When a school is full' to 'Everyone has the same ",
    "chance' for the model section 7 of the document publishes, which gives ",
    "every applicant to a full school the same chance. Priority-6 places above the council's 5% ",
    "are outside anything observed, so the slider's upper range is for ",
    "exploring, not forecasting.</p>",
    "<p><b>Total places and the map colours are simple rules.</b> Moving the ",
    "total shares places out in proportion to today's admission numbers; real ",
    "changes would be negotiated school by school, and a class of 30 is not ",
    "always how a school can grow or shrink. The map colours compare demand ",
    "the model generates before places are rationed with each school's ",
    "admission number, so a blue school is one the model says is ",
    "over-subscribed, not a count of real applications.</p>")))
}

shinyApp(ui, server)
