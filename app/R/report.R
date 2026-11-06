# app/R/report.R — one run, written out as a page to print
# ======================================================================
# A single self-contained HTML file: nothing to install, nothing fetched
# from anywhere, and the browser does the printing. Its tables come from
# the workbook scenario_export() builds and its charts from the builders
# behind the tabs, so nothing here works out a number of its own.
#
# What the user changed comes first, because that is what a reader of a
# printed scenario needs and cannot otherwise find. The warnings stay on
# it, because a printed page travels without the app around it.
# ======================================================================

#' The catchment map as a still picture
#'
#' @param sch one row per open school: mlon, mlat, short, intake, pressure
#' @param geojson the outlines as a GeoJSON string, or NULL
#' @param stops the map's colour stops: at, col
static_map <- function(sch, geojson, stops) {
  shapes <- NULL
  if (!is.null(geojson)) {
    feats <- jsonlite::fromJSON(geojson, simplifyVector = FALSE)$features
    rings <- list()
    for (i in seq_along(feats)) {
      g <- feats[[i]]$geometry
      polys <- if (g$type == "Polygon") list(g$coordinates) else g$coordinates
      for (p in seq_along(polys)) {
        ring <- polys[[p]][[1]]   # outer ring: a catchment outline has no holes to show
        rings[[length(rings) + 1]] <- data.frame(
          lon = vapply(ring, function(v) as.numeric(v[[1]]), numeric(1)),
          lat = vapply(ring, function(v) as.numeric(v[[2]]), numeric(1)),
          id = paste(i, p, sep = "_"))
      }
    }
    shapes <- do.call(rbind, rings)
  }
  sch$scale <- pmin(pmax(sch$pressure, min(stops$at)), max(stops$at))
  # Two pairs of schools sit almost on top of each other. The eastern one
  # of a close pair takes its label below the dot, so both can be read.
  sch$label_at <- vapply(seq_len(nrow(sch)), function(i) {
    dx <- sch$mlon[i] - sch$mlon[-i]
    dy <- abs(sch$mlat[i] - sch$mlat[-i])
    if (any(dx > 0 & dx < 0.01 & dy < 0.006)) 2.4 else -1.5
  }, numeric(1))
  ggplot2::ggplot() +
    (if (!is.null(shapes))
      ggplot2::geom_polygon(data = shapes, ggplot2::aes(lon, lat, group = id),
                            fill = "#eef1f5", colour = "#6b7b8c", linewidth = 0.4)) +
    ggplot2::geom_point(data = sch, ggplot2::aes(mlon, mlat, size = intake, colour = scale)) +
    ggplot2::geom_text(data = sch, ggplot2::aes(mlon, mlat, label = short, vjust = label_at),
                       size = 3, colour = "grey15") +
    ggplot2::scale_size_area(max_size = 12, guide = "none") +
    ggplot2::scale_colour_gradientn(
      colours = stops$col, limits = range(stops$at),
      values = (stops$at - min(stops$at)) / diff(range(stops$at)),
      breaks = c(0.4, 0.7, 1, 1.3, 1.6),
      labels = c("40%", "70%", "Full", "1.3×", "1.6×+"),
      name = "Demand against places") +
    ggplot2::coord_quickmap() +
    ggplot2::labs(title = "Where the schools sit, and how each one fills") +
    ggplot2::theme_void(12) +
    ggplot2::theme(legend.position = "bottom", legend.key.width = grid::unit(1.6, "cm"),
                   plot.title = ggplot2::element_text(face = "bold"))
}

report_table <- function(d, class = NULL) {
  d <- as.data.frame(d, check.names = FALSE, stringsAsFactors = FALSE)
  htmltools::tags$table(
    class = class,
    htmltools::tags$thead(htmltools::tags$tr(lapply(names(d), htmltools::tags$th))),
    htmltools::tags$tbody(lapply(seq_len(nrow(d)), function(i)
      htmltools::tags$tr(lapply(d[i, ], function(v)
        htmltools::tags$td(if (is.na(v)) "" else as.character(v)))))))
}

report_figure <- function(f) {
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png))
  # Drawn at 10 inches and shown at page width, so the charts keep the
  # proportions they have on screen, where their labels were laid out.
  shiny::plotPNG(function() print(f$plot), filename = png, res = 144,
                 width = 10 * 144, height = f$height * 144)
  htmltools::tags$figure(
    htmltools::tags$img(src = base64enc::dataURI(file = png, mime = "image/png"), alt = ""),
    if (!is.null(f$caption)) htmltools::tags$figcaption(f$caption))
}

REPORT_CSS <- "
  :root{--ink:#1f3b57;--line:#d9dee4}
  *{box-sizing:border-box}
  body{font:14px/1.45 -apple-system,'Segoe UI',Roboto,Arial,sans-serif;color:#222;background:#fff;max-width:900px;margin:0 auto;padding:24px 16px}
  h1{font-size:25px;line-height:1.2;color:var(--ink);margin:0 0 4px}
  h2{font-size:18px;color:var(--ink);margin:26px 0 8px;border-bottom:1px solid var(--line);padding-bottom:3px}
  h3{font-size:14.5px;color:var(--ink);margin:16px 0 6px}
  p{margin:6px 0}
  .meta{color:#555;font-size:13px}
  .warn{background:#fff3cd;color:#664d03;border:1px solid #e0b84a;border-radius:6px;padding:7px 11px;margin:12px 0;font-size:13px}
  .notes{white-space:pre-wrap;border-left:3px solid #2a78d6;padding:4px 0 4px 12px;margin:12px 0}
  .kpis{display:grid;grid-template-columns:repeat(3,1fr);gap:8px}
  .kpi{border:1px solid var(--line);border-radius:6px;padding:7px 10px}
  .kpi .v{font-size:20px;font-weight:600;line-height:1.15}
  .kpi .l{font-size:11px;color:#555;text-transform:uppercase;letter-spacing:.3px}
  .kpi .d{font-size:11.5px;color:#666}
  .wide{overflow-x:auto}
  table{border-collapse:collapse;width:100%;font-size:12px;margin:6px 0 12px}
  th,td{border-bottom:1px solid var(--line);padding:3px 6px;text-align:left;vertical-align:top}
  th{background:#f3f5f8}
  figure{margin:12px 0}
  img{display:block;width:100%;height:auto}
  figcaption{font-size:12px;color:#555}
  .caveats p{font-size:12.5px}
  .print{position:sticky;top:8px;float:right;background:#2a78d6;color:#fff;border:0;border-radius:5px;padding:7px 14px;font:inherit;cursor:pointer}
  @page{size:A4;margin:14mm}
  @media (max-width:600px){.kpis{grid-template-columns:repeat(2,1fr)}}
  @media print{
    body{max-width:none;padding:0;font-size:11.5pt}
    .print{display:none}
    *{-webkit-print-color-adjust:exact;print-color-adjust:exact}
    h2,h3{break-after:avoid}
    figure,tr,.kpi,.warn{break-inside:avoid}
    thead{display:table-header-group}
    .wide{overflow:visible}
    table{font-size:9pt}
    .appendix{break-before:page}
  }
"

#' Write the report
#'
#' @param wb the workbook from scenario_export()
#' @param kpis list of list(value, label, detail, colour)
#' @param figures list of list(plot, height in inches, caption)
#' @param tables named list of display tables, as shown on the tabs
#' @param title,notes the user's own words, or empty
#' @param caveats HTML from the "What this is not" tab
#' @param on_figure called with the number of figures after each one
scenario_report <- function(wb, kpis, figures, tables, title = "", notes = "",
                            caveats = "", file, on_figure = function(n) NULL) {
  tg <- htmltools::tags
  has <- function(x) length(x) == 1 && !is.na(x) && nzchar(trimws(x))
  item <- function(k) wb$README$Value[match(k, wb$README$Item)]
  preset <- item("Base scenario")
  heading <- if (has(title)) trimws(title) else paste("Scenario:", preset)
  status <- item("Status")

  changed_set <- wb$Scenario[wb$Scenario$Changed == "changed",
                             c("Setting", "Scenario value", "Used in this run")]
  sc <- wb$Schools[wb$Schools$Changed == "changed", ]
  changed_sch <- data.frame(
    School = sc$Short,
    `Places: scenario` = sc$`Scenario admission number`,
    `Places: this run` = sc$`Admission number used`,
    `Attractiveness: scenario` = sprintf("%.2f×", sc$`Scenario attractiveness (x)`),
    `Attractiveness: this run` = sprintf("%.2f×", sc$`Attractiveness slider (x)`),
    check.names = FALSE, stringsAsFactors = FALSE)
  nothing <- nrow(changed_set) == 0 && nrow(changed_sch) == 0

  drawn <- lapply(figures, function(f) {
    out <- report_figure(f)
    on_figure(length(figures))
    out
  })

  body <- tg$body(
    tg$button(class = "print", onclick = "window.print()", "Print or save as PDF"),
    tg$h1(heading),
    tg$p(class = "meta", "Brighton secondary schools policy simulator"),
    tg$p(class = "meta", sprintf("Base scenario: %s. Generated %s.", preset, item("Generated"))),
    if (has(item("What the scenario is"))) tg$p(class = "meta", item("What the scenario is")),
    tg$div(class = "warn",
           tg$b("Every number here is modelled"),
           " - an estimate from a model of the city's schools, not a record of what happened or a forecast to plan on. ",
           status),
    if (has(notes)) tg$div(class = "notes", trimws(notes)),

    tg$h2("What was changed from the scenario"),
    if (nothing) tg$p(sprintf("Nothing: this is the \"%s\" scenario as it stands.", preset)),
    if (nrow(changed_set)) report_table(changed_set),
    if (nrow(changed_sch)) report_table(changed_sch),

    tg$h2("Headline figures"),
    tg$div(class = "kpis", lapply(kpis, function(k)
      tg$div(class = "kpi",
             tg$div(class = "v", style = paste0("color:", k$colour), k$value),
             tg$div(class = "l", k$label),
             tg$div(class = "d", k$detail)))),

    tg$h2("Charts"),
    drawn,

    tg$h2("Tables"),
    lapply(names(tables), function(n)
      list(tg$h3(n), tg$div(class = "wide", report_table(tables[[n]])))),

    tg$h2("What this is not"),
    tg$div(class = "caveats", caveats),

    tg$div(class = "appendix",
           tg$h2("Appendix: every setting in this run"),
           report_table(wb$Scenario),
           tg$h3("School by school"),
           tg$div(class = "wide", report_table(wb$Schools[, c(
             "Short", "Admission number used", "Attractiveness slider (x)",
             "Attractiveness used (x)", "Demand before the ceiling", "Intake",
             "Priority 6 places", "FSM priority places", "Mean journey (min)")]))))

  # Written out by hand: htmltools lifts a tags$head() out of a page when it
  # renders one, which leaves the file with no styles.
  page <- paste0(
    "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", htmltools::htmlEscape(heading), "</title>\n",
    "<style>", REPORT_CSS, "</style>\n</head>\n",
    as.character(body), "\n</html>\n")
  con <- file(file, open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(page), con, useBytes = TRUE)
  invisible(file)
}
