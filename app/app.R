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
library(tibble)

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
    sliderInput("gamma", "How much living in the catchment counts",
                min = 0, max = 3, value = inp$params$gamma, step = 0.1,
                ticks = FALSE),
    div(class = "note", textOutput("gamma_note")),
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
    nav_panel("Map",
              layout_columns(
                col_widths = c(7, 5),
                div(leafletOutput("map", height = 560),
                    div(class = "note", style = "padding-top:6px",
                        "Dot area is the modelled intake. Green means the school fills its admission number, amber within a tenth of it, red short. The shading is the catchment map in force."),
                    div(class = "note", style = "padding-top:4px",
                        textOutput("design_note"))),
                div(plotOutput("p_att_live", height = 292),
                    plotOutput("p_abs_live", height = 292)))),
    nav_panel("Places", plotOutput("p_places", height = 430),
              tableOutput("t_places")),
    nav_panel("Money", plotOutput("p_money", height = 430),
              tableOutput("t_money")),
    nav_panel("Fairness", plotOutput("p_fair", height = 430),
              div(class = "note",
                  "Gorard's index over the modelled intakes: half the sum of the absolute difference between each school's share of the city's deprived children and its share of all of them. Zero would be a perfectly even spread.")),
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

  apply_preset <- function(p) {
    s <- inp$presets[[p]]
    updateSelectInput(session, "design", selected = s$design)
    updateSelectInput(session, "site", selected = s$site)
    updateSliderInput(session, "year", value = s$year)
    # A preset that does not name a catchment strength means the fitted
    # one, not "leave whatever the last preset set".
    updateSliderInput(session, "gamma",
                      value = s$gamma %||% inp$params$gamma)
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

  output$gamma_note <- renderText({
    req(input$gamma)
    s <- sim()
    fitted <- inp$params$gamma
    base <- sprintf("%.0f%% of children are modelled as going to a school in their own catchment. ",
                    100 * s$in_catch_share)
    paste0(base, if (abs(input$gamma - fitted) < 0.05)
      sprintf("%.1f is the value fitted on the real preferences — a nudge, which is why changing the map moves so few children. Turn it up to see what a binding catchment would do.", fitted)
      else sprintf("The fitted value is %.1f; this is %s.", fitted,
                   if (input$gamma > fitted) "a stronger catchment than families actually behave as though they face"
                   else "a weaker one"))
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

  sim <- reactive({
    req(input$design, input$site, input$year)
    w <- w_now(); p <- pan_now()
    req(all(is.finite(w)), all(is.finite(p)))
    run_sim(inp, w_mult = w, pans = p, site = input$site,
            design = input$design, year = input$year, gamma = input$gamma)
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
                   gamma = input$gamma)
    moved <- sum(pmax(0, sim()$schools$intake - now$schools$intake))
    if (regrouped == 0)
      sprintf("This is the map in force. %.0f%% of children are modelled as attending a school in their own catchment.",
              100 * sim()$in_catch_share)
    else
      sprintf("This map regroups %d of the %d neighbourhood-catchment zones against the one in force, and at this catchment strength it moves %s children between schools.",
              regrouped, length(base), fmt_n(moved))
  })

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
    purrr::map_dfr(seq_len(nrow(CITY)), function(i) {
      nm <- CITY$name[i]
      s <- solve_w_for_pan(inp, nm, target_fill = 1, w_mult = w, pans = p,
                           site = input$site, design = input$design,
                           year = input$year)
      tibble::tibble(
        name = nm, short = CITY$short[i], att8 = CITY$att8[i],
        set_at = CITY$att8[i] + att8_points(inp, w[[nm]]),
        needed = if (is.finite(s$multiplier))
          CITY$att8[i] + att8_points(inp, s$multiplier) else NA_real_,
        pan = p[[nm]])
    }) %>% mutate(gap = needed - att8)
  })

  output$p_att <- renderPlot({
    a <- att_tab() %>% mutate(short = forcats::fct_reorder(short, att8))
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
    paste0("<p>Attainment 8 explains %.0f%% of the variation in weighted ",
           "preferences per place across the city's %d schools, and a point ",
           "is worth about %.0f%% more preferences. That fit is what converts ",
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
                         site = input$site, design = input$design,
                         year = input$year)
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
    "<p><b>The catchment term is a nudge, not a rule.</b> Living in a ",
    "catchment raises the odds of choosing that school; it does not guarantee ",
    "a place, and the admission rules are not modelled here. Section 8.6 of ",
    "the document does that separately.</p>")))
}

shinyApp(ui, server)
