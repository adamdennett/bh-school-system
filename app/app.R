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
# fullest rung, and app/R/check.R asserts that it still agrees with the
# published figures.
#
#   shiny::runApp("app")
# ======================================================================

library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(ggplot2)

APP <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), "."),
                     mustWork = FALSE)
if (!dir.exists(file.path(APP, "R"))) APP <- "app"
source(file.path(APP, "R", "model.R"))
source(file.path(APP, "R", "outcomes.R"))

inp <- readRDS(file.path(APP, "data", "sim_inputs.rds"))
CITY <- inp$schools %>% filter(city) %>% arrange(short)

OK <- "#1baf7a"; BAD <- "#d03b3b"; WARN <- "#eda100"; INK <- "#1f3b57"



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
  title = "Brighton secondary schools — policy simulator",
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
  "))),

  sidebar = sidebar(
    width = 372,
    selectInput("preset", "Start from", choices = names(inp$presets)),
    div(class = "note", textOutput("preset_note")),
    hr(),
    selectInput("design", "Catchment map", choices = names(inp$designs)),
    selectInput("site", "Longhill's site",
                choices = c("Ovingdean (as now)" = "now",
                            "Elm Grove (relocated)" = "elm")),
    sliderInput("year", "Entry year", min = min(inp$demand$year),
                max = max(inp$demand$year), value = 2026, step = 1, sep = "",
                ticks = FALSE),
    hr(),
    div(strong("Per school"), span(class = "note", " — attractiveness ×, and admission number")),
    div(style = "margin-top:8px", lapply(seq_len(nrow(CITY)),
                                         function(i) school_row(CITY[i, ]))),
    div(style = "margin-top:18px",
        actionButton("reset", "Reset to the preset", class = "btn-sm btn-outline-secondary")),
    hr(),
    selectInput("solve_for", "How attractive would a school have to be to fill?",
                choices = c("—", CITY$short)),
    div(class = "note", textOutput("solve_note"))),

  layout_columns(
    fill = FALSE, col_widths = c(2, 2, 2, 2, 2, 2),
    uiOutput("kpi_fill"), uiOutput("kpi_short"), uiOutput("kpi_money"),
    uiOutput("kpi_seg"), uiOutput("kpi_travel"), uiOutput("kpi_gap")),

  navset_card_tab(
    nav_panel("Map", leafletOutput("map", height = 560),
              div(class = "note", style = "padding-top:6px",
                  "Dot area is the modelled intake. Green means the school fills its admission number, amber within a tenth of it, red short. The shading is the catchment map in force.")),
    nav_panel("Places", plotOutput("p_places", height = 430),
              tableOutput("t_places")),
    nav_panel("Money", plotOutput("p_money", height = 430),
              tableOutput("t_money")),
    nav_panel("Fairness", plotOutput("p_fair", height = 430),
              div(class = "note",
                  "Gorard's index over the modelled intakes: half the sum of the absolute difference between each school's share of the city's deprived children and its share of all of them. Zero would be a perfectly even spread.")),
    nav_panel("Travel", plotOutput("p_travel", height = 430),
              div(class = "note",
                  "These journeys are longer than the ones in section 8 of the document, and for a reason worth knowing. The model gives every child a probability of attending every school, so a small fraction of each neighbourhood is counted as travelling to a school right across the city. Section 8 instead assigns each child to one school under the admission rules, which is a shorter journey by construction. Use these figures to compare one configuration with another, not against the section 8 numbers.")),
    nav_panel("What this is not",
              div(class = "note", style = "padding:14px 4px;max-width:760px",
                  htmlOutput("caveats")))))

# ---- Server ----------------------------------------------------------

server <- function(input, output, session) {

  apply_preset <- function(p) {
    s <- inp$presets[[p]]
    updateSelectInput(session, "design", selected = s$design)
    updateSelectInput(session, "site", selected = s$site)
    updateSliderInput(session, "year", value = s$year)
    for (i in seq_len(nrow(CITY))) {
      nm <- CITY$name[i]; urn <- CITY$urn[i]
      updateSliderInput(session, paste0("w_", urn),
                        value = if (!is.null(s$w) && nm %in% names(s$w))
                          round(unname(s$w[nm]), 2) else 1)
      updateNumericInput(session, paste0("pan_", urn),
                         value = if (!is.null(s$pan) && nm %in% names(s$pan))
                           unname(s$pan[nm]) else CITY$pan[i])
    }
  }
  observeEvent(input$preset, apply_preset(input$preset))
  observeEvent(input$reset, apply_preset(input$preset))

  output$preset_note <- renderText(inp$presets[[input$preset]]$note)

  w_now <- reactive({
    v <- vapply(CITY$urn, function(u) input[[paste0("w_", u)]] %||% 1, numeric(1))
    setNames(as.numeric(v), CITY$name)
  })
  pan_now <- reactive({
    v <- vapply(CITY$urn, function(u) as.numeric(input[[paste0("pan_", u)]] %||% NA),
                numeric(1))
    setNames(v, CITY$name)
  })

  sim <- reactive({
    req(input$design, input$site, input$year)
    w <- w_now(); p <- pan_now()
    req(all(is.finite(w)), all(is.finite(p)))
    run_sim(inp, w_mult = w, pans = p, site = input$site,
            design = input$design, year = input$year)
  })
  met <- reactive(outcomes(inp, sim()))

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

  # ---- Map ------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles(urlTemplate = paste0(
                 "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/",
                 "World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"),
               attribution = paste0(
                 'Tiles &copy; <a href="https://www.esri.com/">Esri</a> &mdash; ',
                 'Esri, HERE, Garmin, &copy; <a href="https://www.openstreetmap.org/copyright">',
                 'OpenStreetMap</a> contributors'),
               options = tileOptions(maxNativeZoom = 16, maxZoom = 20)) %>%
      addTiles(urlTemplate = paste0(
                 "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/",
                 "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"),
               attribution = "",
               options = tileOptions(maxNativeZoom = 16, maxZoom = 20)) %>%
      setView(-0.14, 50.845, 12)
  })

  observe({
    s <- sim(); m <- met()
    sch <- s$schools %>%
      inner_join(inp$schools %>%
                   select(name, easting, northing, elm_easting, elm_northing),
                 by = "name")
    e <- if (input$site == "elm") sch$elm_easting else sch$easting
    n <- if (input$site == "elm") sch$elm_northing else sch$northing
    ll <- sf::st_as_sf(data.frame(e = e, n = n), coords = c("e", "n"),
                       crs = 27700) %>% sf::st_transform(4326) %>%
      sf::st_coordinates()

    sch$col <- ifelse(sch$fill >= 0.995, OK,
                      ifelse(sch$fill >= 0.9, WARN, BAD))
    sch$lab <- sprintf(
      "<b>%s</b><br>Intake %s of %s places (%.0f%%)<br>Mean journey %.0f min",
      sch$short, fmt_n(sch$intake), fmt_n(sch$pan), 100 * sch$fill, sch$mean_min)

    poly <- inp$design_sf
    dname <- switch(input$design,
                    "Current catchments" = "Current catchments",
                    "Power diagram" = "Power diagram (proximity and capacity)",
                    "Flow regions, pairs kept" = "Flow regions, pairs kept",
                    "Flow regions, one per school" = "Flow regions, one per school",
                    "Flow regions, Longhill at Elm Grove" = "Flow regions, Elm Grove, PAN 150")
    poly <- poly[poly$design == dname, ]

    leafletProxy("map") %>%
      clearShapes() %>% clearMarkers() %>%
      addPolygons(data = poly, fill = TRUE, fillColor = "#8aa0b4",
                  fillOpacity = 0.10, color = "#5a6b7c", weight = 1.2,
                  label = ~grp) %>%
      # Plain vectors, not formulas: a formula with no data= argument
      # sends leaflet looking for metaData on NULL and the whole map
      # silently fails to draw.
      addCircleMarkers(lng = ll[, 1], lat = ll[, 2],
                       radius = pmax(5, sqrt(sch$intake) * 1.5),
                       color = "#333333", weight = 1,
                       fillColor = sch$col, fillOpacity = 0.85,
                       label = lapply(sch$lab, HTML))
  })

  # ---- Places ---------------------------------------------------------
  output$p_places <- renderPlot({
    s <- sim()$schools %>% filter(city) %>%
      mutate(short = forcats::fct_reorder(short, fill))
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
      mutate(short = forcats::fct_reorder(short, gap_pct))
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

  # ---- Fairness --------------------------------------------------------
  output$p_fair <- renderPlot({
    m <- met()
    d <- m$mix %>% filter(name %in% inp$city, n > 0) %>%
      inner_join(inp$schools %>% select(name, short), by = "name") %>%
      mutate(short = forcats::fct_reorder(short, dep_share))
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

  # ---- Travel ----------------------------------------------------------
  output$p_travel <- renderPlot({
    f <- sim()$flows
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

  # ---- The inverse question --------------------------------------------
  output$solve_note <- renderText({
    req(input$solve_for)
    if (input$solve_for == "—")
      return("Pick a school and the app solves for the attractiveness that fills it, holding everything else where you have set it.")
    nm <- CITY$name[match(input$solve_for, CITY$short)]
    p <- pan_now()
    s <- solve_w_for_pan(inp, nm, target_fill = 1, w_mult = NULL, pans = p,
                         site = input$site, design = input$design,
                         year = input$year)
    if (is.infinite(s$multiplier))
      sprintf("%s cannot fill %s places at any attractiveness: at 60 times its own it reaches %.0f%%. There are not enough children within reach.",
              input$solve_for, fmt_n(p[[nm]]), 100 * s$fill)
    else if (s$multiplier <= 1.001)
      sprintf("%s already fills at its current attractiveness.", input$solve_for)
    else
      sprintf("%s would need %.1f times its current attractiveness to fill %s places — about %s on the weighted-preference scale, against Dorothy Stringer's %.2f.",
              input$solve_for, s$multiplier, fmt_n(p[[nm]]),
              sprintf("%.2f", s$multiplier * inp$schools$W[inp$schools$name == nm]),
              inp$schools$W[inp$schools$name == "Dorothy Stringer School"])
  })

  output$caveats <- renderUI(HTML(paste0(
    "<p><b>This is a model, and an uncalibrated one.</b> The flows come from ",
    "the spatial interaction model in section 7 of the strategic view, fitted ",
    "on published preference and offer counts. It has never seen a real ",
    "application. Every number here moves with its assumptions.</p>",
    "<p><b>Attractiveness is a single number per school.</b> Moving the slider ",
    "says 'suppose families wanted this school this much more'. It does not ",
    "say how that would be achieved, how long it would take, or whether it is ",
    "possible. The scale is preferences per place, weighted across three ranks.</p>",
    "<p><b>Children cannot leave the city.</b> The model places every Brighton ",
    "child in a Brighton school, as the published one does. In reality some go ",
    "to Peacehaven, some go private, and some move away — and a school that ",
    "becomes unattractive enough loses children to all three.</p>",
    "<p><b>The two faith schools are the least reliable rows.</b> Cardinal ",
    "Newman and King's admit on criteria no published dataset contains, so the ",
    "model admits to them on distance alone.</p>",
    "<p><b>The money is a steady state, not a budget.</b> A roll of five times ",
    "the intake is where a school ends up if this configuration holds for five ",
    "years. Reserves, balances and funding rates are today's, in today's ",
    "prices, with no pay award, energy shock or capital receipt in them.</p>",
    "<p><b>Journeys here are expected journeys, not assigned ones.</b> ",
    "Every child has a probability of attending every school, so a sliver ",
    "of each neighbourhood counts as travelling right across the city. That ",
    "makes the mean longer than the figure in section 8, which assigns each ",
    "child to one school. Compare configurations with each other here; do not ",
    "read the level against the document.</p>",
    "<p><b>The catchment term is a nudge, not a rule.</b> Living in a ",
    "catchment raises the odds of choosing that school; it does not guarantee ",
    "a place, and the admission rules are not modelled here. Section 8.6 of ",
    "the document does that separately.</p>")))
}

shinyApp(ui, server)
