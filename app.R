# Little Colorado River Discharge Data - USGS Water Data API version
# Hosting-ready revision, November 2026. Original layout retained.
# Requires: shiny, dataRetrieval, ggplot2, plotly, dplyr, lubridate, purrr
# USGS parameter 00060 = discharge (cfs); daily statistic 00003 = mean.
#
# Changes from the October version:
#  1. Date window is recomputed on every pull. Before, date_window() had no
#     reactive dependencies, so it was computed once per session and
#     "Refresh" re-requested the same frozen window.
#  2. Auto-refresh every 15 min, aligned to the clock (:00, :15, :30, :45).
#  3. Results cached across viewers with bindCache(): 10 colleagues watching
#     still means one USGS request per 15 min, not ten.
#  4. Status line on the page: time of last pull and any USGS errors
#     (hosted, nobody sees the RStudio console).
#  5. Table leads with the latest instantaneous reading, its age, and a
#     text status flag (OK / STALE / NO DATA) so gage outages are obvious.
#  6. Okabe-Ito colors plus distinct line types; Gage Pool in solid black.
#  7. Provisional-data banner.

library(shiny)
library(dataRetrieval)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(purrr)

# ---- Settings -------------------------------------------------------------
refresh_minutes <- 15   # auto-refresh interval
days_back       <- 10   # plotting window
stale_hours     <- 3    # flag a gage if its latest reading is older than this

# ---- Gages, colors, line types ------------------------------------------
gages <- data.frame(
  monitoring_location_id = paste0(
    "USGS-", c("09400350", "09402000", "09401260", "09402300")
  ),
  Site = c("Winslow", "Cameron", "Moenkopi", "LCR Gage Pool"),
  stringsAsFactors = FALSE
)

# Okabe-Ito. Gage Pool is black and solid so it reads as the reference line.
site_colors <- setNames(c("#E69F00", "#56B4E9", "#009E73", "#000000"),
                        gages$Site)
site_lines  <- setNames(c("dashed", "dotdash", "longdash", "solid"),
                        gages$Site)

lcr_scales <- list(
  scale_color_manual(values = site_colors, name = "Gage site"),
  scale_linetype_manual(values = site_lines, name = "Gage site"),
  theme_minimal(base_size = 13)
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

# safely() returns list(result, error) instead of throwing, so one failed
# request does not take down the other, and the error can be shown on page.
safe_daily      <- purrr::safely(fetch_daily,      otherwise = empty_daily())
safe_continuous <- purrr::safely(fetch_continuous, otherwise = empty_continuous())

# ---- UI -------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .provisional { background:#F0E442; color:#000; padding:8px 12px;
                   margin-bottom:12px; font-weight:bold; }
    .usgs-error  { color:#D55E00; }
  "))),
  titlePanel("Little Colorado River Discharge Data"),
  tags$div(class = "provisional",
           "PROVISIONAL USGS DATA, subject to revision. For situational ",
           "awareness only; this page is not a validated flood forecast."),
  sidebarLayout(
    sidebarPanel(
      uiOutput("status"),
      actionButton("refresh", "Refresh now"),
      tags$hr(),
      tags$p("Data: USGS Water Data APIs (parameter 00060)."),
      tags$p("Times shown in Arizona time (MST, no daylight saving)."),
      tags$p(sprintf("STALE = latest reading older than %d hours.",
                     stale_hours))
    ),
    mainPanel(
      h3("Current Conditions"),
      tableOutput("latestValues"),
      h3("Continuous (Instantaneous) Discharge"),
      plotlyOutput("hourlyDischargePlot"),
      h3("Daily Mean Discharge"),
      plotlyOutput("dischargePlot")
    )
  )
)

# ---- Server ---------------------------------------------------------------
server <- function(input, output, session) {

  # Clock-aligned refresh bucket. invalidateLater fires just after the next
  # boundary, so plots redraw once per interval rather than every minute.
  bucket <- reactive({
    step <- refresh_minutes * 60
    now  <- as.numeric(Sys.time())
    invalidateLater(ceiling((step - now %% step) * 1000) + 1000)
    floor(now / step)
  })

  # One pull per bucket, shared by every viewer of the app (app-level cache).
  # Clicking "Refresh now" changes the key and forces a fresh pull.
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

  output$latestValues <- renderTable({
    daily      <- daily_data()
    continuous <- continuous_data()
    pulled     <- usgs()$pulled

    validate(need(nrow(daily) > 0L || nrow(continuous) > 0L,
                  "No USGS discharge data were returned. See status at left."))

    latest_cont <- continuous %>%
      group_by(Site) %>%
      slice_max(dateTime, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(Site,
                Latest_cfs   = Discharge,
                Reading_time = format(dateTime, "%b %d %H:%M",
                                      tz = "US/Arizona"),
                Hours_old    = as.numeric(difftime(pulled, dateTime,
                                                   units = "hours")))

    max_cont <- continuous %>%
      group_by(Site) %>%
      summarise(Max_10day_cfs = max(Discharge), .groups = "drop")

    latest_daily <- daily %>%
      group_by(Site) %>%
      slice_max(Date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(Site,
                Daily_mean_cfs  = Discharge,
                Daily_mean_date = format(Date, "%b %d"))

    data.frame(Site = site_factor(gages$Site)) %>%
      left_join(latest_cont,  by = "Site") %>%
      left_join(max_cont,     by = "Site") %>%
      left_join(latest_daily, by = "Site") %>%
      mutate(Status = case_when(is.na(Hours_old)        ~ "NO DATA",
                                Hours_old > stale_hours ~ "STALE",
                                TRUE                    ~ "OK"),
             Hours_old = round(Hours_old, 1)) %>%
      select(Site, Status, Latest_cfs, Reading_time, Hours_old,
             Max_10day_cfs, Daily_mean_cfs, Daily_mean_date)
  }, digits = 1, na = "-")

  output$hourlyDischargePlot <- renderPlotly({
    continuous <- continuous_data()
    validate(need(nrow(continuous) > 0L,
                  "No continuous discharge data available for this period."))

    p <- ggplot(continuous,
                aes(x = dateTime, y = Discharge,
                    color = Site, linetype = Site)) +
      geom_line(linewidth = 1) +
      labs(title = sprintf("Continuous discharge, last %d days", days_back),
           x = "Date and time (Arizona MST)", y = "Discharge (cfs)") +
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
      labs(title = sprintf("Daily mean discharge, last %d days", days_back),
           x = "Date", y = "Discharge (cfs)") +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
      scale_shape_manual(values = setNames(c(16, 17, 15, 18), gages$Site),
                         name = "Gage site") +
      lcr_scales
    ggplotly(p) %>% clean_legend()
  })
}

shinyApp(ui = ui, server = server)
