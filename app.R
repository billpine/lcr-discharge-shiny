# Little Colorado River Discharge Data - USGS Water Data API version
# Rim-readable revision, November 2026.
# Requires: shiny, dataRetrieval, ggplot2, plotly, dplyr, lubridate, purrr, scales
# USGS parameter 00060 = discharge (cfs); daily statistic 00003 = mean.
#
# What this app does: shows observed USGS discharge. It does NOT forecast.
#
# Page layout, top to bottom:
#  1. Gage Pool headline: flow now, trend, and where it sits against the crew
#     levels (no crossing above 600 cfs; alert above 3,000 cfs).
#  2. Upstream watch: one line naming any gage that is rising.
#  3. Gage table in flow order (Winslow, Moenkopi, Cameron, Gage Pool):
#     now, trend, 24-h peak, yesterday's average, last reading, reporting.
#  4. Plots: continuous (with crew levels marked) and daily mean.
#
# Background (unchanged from the previous revision):
#  - Auto-refresh every 15 min, clock-aligned, one USGS pull shared by all
#    viewers via bindCache(); "Refresh now" forces a new pull.
#  - USGS errors shown on the page via purrr::safely().
#  - Okabe-Ito colors plus line types; Gage Pool solid black.
#  - API key read from API_USGS_PAT in .Renviron (never in this file).

library(shiny)
library(dataRetrieval)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(purrr)

# ---- Settings -------------------------------------------------------------
refresh_minutes <- 15     # auto-refresh interval
days_back       <- 10     # plotting window
stale_hours     <- 3      # "Gage reporting: Delayed" if last reading older
trend_hours     <- 3      # trend = now vs. this many hours ago
trend_pct       <- 0.10   # change must exceed 10% ...
trend_min_cfs   <- 20     # ... and 20 cfs to count as Rising/Falling
no_cross_cfs    <- 600    # crew level at Gage Pool: extreme caution crossing at camps
alert_cfs       <- 3000   # crew level at Gage Pool: alert

# ---- Gages (flow order), colors, line types -------------------------------
# Moenkopi Wash joins above the Cameron gage, so Cameron is the only gaged
# input to the Gage Pool.
gages <- data.frame(
  monitoring_location_id = paste0(
    "USGS-", c("09400350", "09401260", "09402000", "09402300")
  ),
  Site = c("Winslow", "Moenkopi", "Cameron", "LCR Gage Pool"),
  stringsAsFactors = FALSE
)

# Okabe-Ito. Gage Pool is black and solid so it reads as the reference line.
site_colors <- c("Winslow" = "#E69F00", "Moenkopi" = "#009E73",
                 "Cameron" = "#56B4E9", "LCR Gage Pool" = "#000000")
site_lines  <- c("Winslow" = "dashed", "Moenkopi" = "longdash",
                 "Cameron" = "dotdash", "LCR Gage Pool" = "solid")
site_shapes <- c("Winslow" = 16, "Moenkopi" = 15,
                 "Cameron" = 17, "LCR Gage Pool" = 18)

lcr_scales <- list(
  scale_color_manual(values = site_colors, name = "Gage site"),
  scale_linetype_manual(values = site_lines, name = "Gage site"),
  theme_minimal(base_size = 13)
)

# Crew levels drawn on the continuous plot. Labels sit at the left edge
# (x_min) because ggplotly does not place text at x = -Inf.
crew_lines <- function(x_min) list(
  geom_hline(yintercept = c(no_cross_cfs, alert_cfs),
             linetype = "dotted", colour = "grey40"),
  annotate("text", x = x_min, y = c(no_cross_cfs, alert_cfs),
           label = c(sprintf("Gage Pool %s: no crossing",
                             scales::comma(no_cross_cfs)),
                     sprintf("Gage Pool %s: crew alert",
                             scales::comma(alert_cfs))),
           hjust = 0, vjust = -0.4, size = 3, colour = "grey30")
)

# ggplotly labels legend entries "(Winslow,Winslow)" when color and linetype
# map to the same variable. This strips them back to "Winslow".
clean_legend <- function(p) {
  p$x$data <- purrr::map(p$x$data, function(tr) {
    if (!is.null(tr$name)) tr$name <- sub("^\\(([^,]+),.*\\)$", "\\1", tr$name)
    tr
  })
  p
}

site_factor <- function(x) factor(x, levels = gages$Site)
fmt_cfs <- function(x) ifelse(is.na(x), "-", scales::comma(round(x)))

# ---- Empty templates ------------------------------------------------------
empty_daily <- function() {
  data.frame(Date = as.Date(character()), Discharge = numeric(),
             Site = site_factor(character()))
}
empty_continuous <- function() {
  data.frame(dateTime = as.POSIXct(character(), tz = "US/Arizona"),
             Discharge = numeric(), Site = site_factor(character()))
}

# ---- USGS fetchers --------------------------------------------------------
fetch_daily <- function(start_date, end_date) {
  x <- dataRetrieval::read_waterdata_daily(
    monitoring_location_id = gages$monitoring_location_id,
    parameter_code = "00060",
    statistic_id = "00003",
    time = c(as.character(start_date), as.character(end_date)),
    skipGeometry = TRUE
  )
  if (nrow(x) == 0L) return(empty_daily())

  x %>%
    dplyr::filter(.data$parameter_code == "00060",
                  .data$statistic_id == "00003") %>%
    dplyr::left_join(gages, by = "monitoring_location_id") %>%
    dplyr::transmute(
      Date = as.Date(.data$time),
      Discharge = suppressWarnings(as.numeric(as.character(.data$value))),
      Site = site_factor(.data$Site)
    ) %>%
    dplyr::filter(!is.na(.data$Site), !is.na(.data$Discharge)) %>%
    dplyr::arrange(.data$Site, .data$Date)
}

fetch_continuous <- function(start_date, end_time) {
  start_time <- as.POSIXct(paste(start_date, "00:00:00"), tz = "US/Arizona")
  x <- dataRetrieval::read_waterdata_continuous(
    monitoring_location_id = gages$monitoring_location_id,
    parameter_code = "00060",
    time = c(start_time, end_time),
    skipGeometry = TRUE
  )
  if (nrow(x) == 0L) return(empty_continuous())

  x %>%
    dplyr::filter(.data$parameter_code == "00060") %>%
    dplyr::left_join(gages, by = "monitoring_location_id") %>%
    dplyr::transmute(
      dateTime = lubridate::with_tz(as.POSIXct(.data$time, tz = "UTC"),
                                    tzone = "US/Arizona"),
      Discharge = suppressWarnings(as.numeric(as.character(.data$value))),
      Site = site_factor(.data$Site)
    ) %>%
    dplyr::filter(!is.na(.data$Site), !is.na(.data$Discharge),
                  !is.na(.data$dateTime)) %>%
    dplyr::arrange(.data$Site, .data$dateTime)
}

safe_daily      <- purrr::safely(fetch_daily,      otherwise = empty_daily())
safe_continuous <- purrr::safely(fetch_continuous, otherwise = empty_continuous())

# ---- Per-gage summary -----------------------------------------------------
trend_label <- function(now, before) {
  thresh <- pmax(trend_min_cfs, trend_pct * before)
  dplyr::case_when(
    is.na(now) | is.na(before) ~ "-",
    now - before > thresh      ~ "Rising",
    before - now > thresh      ~ "Falling",
    TRUE                       ~ "Steady"
  )
}

# Reading closest to trend_hours before the latest one; NA if the nearest
# reading is more than an hour off (gap in the record).
value_hours_before <- function(t, q, hours) {
  target <- as.numeric(max(t)) - hours * 3600
  gap    <- abs(as.numeric(t) - target)
  if (min(gap) > 3600) NA_real_ else q[which.min(gap)]
}

site_summary <- function(continuous, pulled) {
  if (nrow(continuous) == 0L) {
    return(data.frame(Site = site_factor(character()),
                      last_time = as.POSIXct(character(), tz = "US/Arizona"),
                      now_cfs = numeric(), before_cfs = numeric(),
                      peak24_cfs = numeric(), hours_old = numeric(),
                      trend = character()))
  }
  continuous %>%
    group_by(Site) %>%
    summarise(
      last_time  = max(dateTime),
      now_cfs    = Discharge[which.max(dateTime)],
      before_cfs = value_hours_before(dateTime, Discharge, trend_hours),
      peak24_cfs = max(Discharge[as.numeric(dateTime) >
                                   as.numeric(max(dateTime)) - 24 * 3600]),
      .groups = "drop"
    ) %>%
    mutate(hours_old = as.numeric(difftime(pulled, last_time, units = "hours")),
           trend     = trend_label(now_cfs, before_cfs))
}

# ---- UI -------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .provisional { background:#F0E442; color:#000; padding:8px 12px;
                   margin-bottom:12px; font-weight:bold; }
    .usgs-error  { color:#D55E00; }
    .headline    { border:2px solid #000; padding:12px 16px; margin-bottom:10px; }
    .headline-main { font-size:1.6em; font-weight:bold; }
    .headline-level { font-size:1.15em; font-weight:bold; margin-top:4px; }
    .headline-sub  { color:#444; margin-top:4px; }
    .upstream    { font-size:1.15em; margin-bottom:16px; }
  "))),
  titlePanel("Little Colorado River Discharge"),
  tags$div(class = "provisional",
           "PROVISIONAL USGS DATA, subject to revision. Observed flows only; ",
           "this page does not forecast when or whether a flood will arrive."),
  sidebarLayout(
    sidebarPanel(
      uiOutput("status"),
      actionButton("refresh", "Refresh now"),
      tags$hr(),
      tags$p(tags$strong("Crew levels at the Gage Pool:"),
             sprintf("caution when crossing at camps above %s cfs; alert above %s cfs.",
                     scales::comma(no_cross_cfs), scales::comma(alert_cfs))),
      tags$p(sprintf(paste0("Trend compares the latest reading with %d hours ",
                            "earlier. Rising or Falling means a change of ",
                            "more than %d%% and %d cfs."),
                     trend_hours, round(100 * trend_pct), trend_min_cfs)),
      tags$p(sprintf(paste0("Gage reporting: Yes = reading within %d h; ",
                            "Delayed = older; No = nothing in %d days. ",
                            "A gage that is not reporting can hide a flood."),
                     stale_hours, days_back)),
      tags$p("Flood path: Winslow, then Moenkopi Wash joins, then Cameron, ",
             "then the Gage Pool."),
      tags$p("Times are Arizona time (MST). Data: USGS Water Data APIs.")
    ),
    mainPanel(
      uiOutput("gpHeadline"),
      uiOutput("upstreamWatch"),
      h3("Gages, upstream to downstream"),
      tableOutput("gageTable"),
      h3(sprintf("Continuous discharge, last %d days", days_back)),
      plotlyOutput("hourlyDischargePlot"),
      h3(sprintf("Daily mean discharge, last %d days", days_back)),
      plotlyOutput("dischargePlot")
    )
  )
)

# ---- Server ---------------------------------------------------------------
server <- function(input, output, session) {

  # Clock-aligned refresh bucket; fires just after the next boundary.
  bucket <- reactive({
    step <- refresh_minutes * 60
    now  <- as.numeric(Sys.time())
    invalidateLater(ceiling((step - now %% step) * 1000) + 1000)
    floor(now / step)
  })

  # One pull per bucket, shared by all viewers. "Refresh now" forces a pull.
  usgs <- reactive({
    end_time   <- Sys.time()
    end_date   <- as.Date(end_time, tz = "US/Arizona")
    start_date <- end_date - days_back
    list(
      daily      = safe_daily(start_date, end_date),
      continuous = safe_continuous(start_date, end_time),
      pulled     = end_time
    )
  }) %>%
    bindCache(bucket(), input$refresh)

  daily_data      <- reactive(usgs()$daily$result)
  continuous_data <- reactive(usgs()$continuous$result)
  summ            <- reactive(site_summary(continuous_data(), usgs()$pulled))

  output$status <- renderUI({
    d <- usgs()
    errs <- list(Daily = d$daily$error, Continuous = d$continuous$error) %>%
      purrr::compact() %>%
      purrr::imap_chr(~ paste0(.y, " request failed: ", conditionMessage(.x)))
    tagList(
      tags$p(tags$strong("Last USGS pull: "),
             format(d$pulled, "%b %d %H:%M %Z", tz = "US/Arizona")),
      tags$p(sprintf("Auto-refreshes every %d min.", refresh_minutes)),
      if (length(errs) > 0L) {
        tags$p(class = "usgs-error", tags$strong("WARNING: "),
               paste(errs, collapse = "; "))
      }
    )
  })

  # ---- Gage Pool headline ----
  output$gpHeadline <- renderUI({
    gp <- dplyr::filter(summ(), Site == "LCR Gage Pool")

    if (nrow(gp) == 0L) {
      return(tags$div(class = "headline",
        tags$div(class = "headline-main", "Gage Pool: NOT REPORTING"),
        tags$div(class = "headline-sub",
                 "No recent Gage Pool readings. Check upstream gages below.")))
    }

    trend_txt <- if (gp$trend == "-") "trend unknown" else tolower(gp$trend)
    level_txt <- dplyr::case_when(
      gp$now_cfs >= alert_cfs ~
        sprintf("ABOVE %s cfs CREW ALERT LEVEL.", scales::comma(alert_cfs)),
      gp$now_cfs >  no_cross_cfs ~
        sprintf("Above %s cfs: no crossing at camps.",
                scales::comma(no_cross_cfs)),
      TRUE ~
        sprintf("Below the %s cfs no-crossing level.",
                scales::comma(no_cross_cfs))
    )
    delayed <- if (gp$hours_old > stale_hours) " READING IS DELAYED." else ""

    tags$div(class = "headline",
      tags$div(class = "headline-main",
               sprintf("Gage Pool: %s cfs, %s", fmt_cfs(gp$now_cfs), trend_txt)),
      tags$div(class = "headline-level", level_txt),
      tags$div(class = "headline-sub",
               sprintf("Reading at %s (%.1f h ago). Peak in last 24 h: %s cfs.%s",
                       format(gp$last_time, "%a %b %d %H:%M"), gp$hours_old,
                       fmt_cfs(gp$peak24_cfs), delayed)))
  })

  # ---- One-line upstream watch ----
  output$upstreamWatch <- renderUI({
    up <- summ() %>%
      dplyr::filter(Site != "LCR Gage Pool") %>%
      dplyr::arrange(Site)
    rising <- up %>%
      dplyr::filter(trend == "Rising") %>%
      dplyr::mutate(txt = sprintf("%s (%s cfs)", Site, fmt_cfs(now_cfs))) %>%
      dplyr::pull(txt)
    quiet <- setdiff(gages$Site[gages$Site != "LCR Gage Pool"],
                     as.character(up$Site[up$hours_old <= stale_hours]))

    tags$div(class = "upstream",
      tags$strong("Upstream: "),
      if (length(rising) > 0L) {
        paste0("RISING at ", paste(rising, collapse = ", "), ".")
      } else {
        "nothing rising."
      },
      if (length(quiet) > 0L) {
        paste0(" Not reporting or delayed: ", paste(quiet, collapse = ", "), ".")
      })
  })

  # ---- Gage table ----
  output$gageTable <- renderTable({
    s     <- summ()
    daily <- daily_data()

    validate(need(nrow(s) > 0L || nrow(daily) > 0L,
                  "No USGS discharge data were returned. See status at left."))

    yesterday <- daily %>%
      group_by(Site) %>%
      slice_max(Date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(Site, daily_cfs = Discharge)

    data.frame(Site = site_factor(gages$Site)) %>%
      left_join(s,         by = "Site") %>%
      left_join(yesterday, by = "Site") %>%
      transmute(
        `Gage`                      = as.character(Site),
        `Now (cfs)`                 = fmt_cfs(now_cfs),
        `Trend`                     = dplyr::coalesce(trend, "-"),
        `Peak, last 24 h (cfs)`     = fmt_cfs(peak24_cfs),
        `Yesterday's average (cfs)` = fmt_cfs(daily_cfs),
        `Last reading`              = ifelse(is.na(last_time), "-",
                                             format(last_time, "%a %H:%M",
                                                    tz = "US/Arizona")),
        `Gage reporting`            = dplyr::case_when(
          is.na(hours_old)        ~ "No",
          hours_old > stale_hours ~ "Delayed",
          TRUE                    ~ "Yes")
      )
  }, align = "lrlrrll")

  # ---- Plots ----
  output$hourlyDischargePlot <- renderPlotly({
    continuous <- continuous_data()
    validate(need(nrow(continuous) > 0L,
                  "No continuous discharge data available for this period."))

    p <- ggplot(continuous,
                aes(x = dateTime, y = Discharge,
                    color = Site, linetype = Site)) +
      crew_lines(min(continuous$dateTime)) +
      geom_line(linewidth = 1) +
      labs(x = "Date and time (Arizona MST)", y = "Discharge (cfs)") +
      scale_x_datetime(date_labels = "%b %d %H:%M", timezone = "US/Arizona") +
      lcr_scales
    ggplotly(p) %>% clean_legend()
  })

  output$dischargePlot <- renderPlotly({
    daily <- daily_data()
    validate(need(nrow(daily) > 0L,
                  "No daily mean discharge data available for this period."))

    p <- ggplot(daily,
                aes(x = Date, y = Discharge,
                    color = Site, linetype = Site, shape = Site)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      labs(x = "Date", y = "Discharge (cfs)") +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
      scale_shape_manual(values = site_shapes, name = "Gage site") +
      lcr_scales
    ggplotly(p) %>% clean_legend()
  })
}

shinyApp(ui = ui, server = server)
