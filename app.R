# Little Colorado River Discharge Data - USGS Water Data API version
# Updated October 2026. Original LCR Shiny layout retained.
# Requires: shiny, dataRetrieval, ggplot2, plotly, dplyr, lubridate
# USGS parameter 00060 = discharge (cfs); daily statistic 00003 = mean.

library(shiny)
library(dataRetrieval)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)

# USGS monitoring locations used by the original app.
gages <- data.frame(
  monitoring_location_id = paste0(
    "USGS-", c("09400350", "09402000", "09401260", "09402300")
  ),
  Site = c("Winslow", "Cameron", "Moenkopi", "LCR Gage Pool"),
  stringsAsFactors = FALSE
)

# These typed empty data frames let Shiny show a useful message when no records
# are available for the chosen period, rather than crashing in ggplot.
empty_daily <- function() {
  data.frame(Date = as.Date(character()), Discharge = numeric(),
             Site = character())
}
empty_continuous <- function() {
  data.frame(dateTime = as.POSIXct(character(), tz = "US/Arizona"),
             Discharge = numeric(), Site = character())
}

# Download mean DAILY discharge. The new API uses long-format columns:
# monitoring_location_id, time, value (not X_00060_00003).
fetch_daily <- function(start_date, end_date) {
  x <- dataRetrieval::read_waterdata_daily(
    monitoring_location_id = gages$monitoring_location_id,
    parameter_code = "00060",
    statistic_id = "00003",           # daily mean
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
      Site = .data$Site
    ) %>%
    dplyr::filter(!is.na(.data$Site), !is.na(.data$Discharge)) %>%
    dplyr::arrange(.data$Site, .data$Date)
}

# Download CONTINUOUS (instantaneous) discharge. Depending on the station,
# measurements may be every 15 minutes or at another recording interval.
# Returned timestamps are UTC; display in Arizona local time (no DST).
fetch_continuous <- function(start_date, end_time) {
  # Explicit timestamps capture today's observations through the moment of
  # refresh, instead of accidentally stopping at the start of today's date.
  start_time <- as.POSIXct(paste(start_date, "00:00:00"),
                           tz = "US/Arizona")
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
      Site = .data$Site
    ) %>%
    dplyr::filter(!is.na(.data$Site), !is.na(.data$Discharge),
                  !is.na(.data$dateTime)) %>%
    dplyr::arrange(.data$Site, .data$dateTime)
}

ui <- fluidPage(
  titlePanel("Little Colorado River Discharge Data"),
  sidebarLayout(
    sidebarPanel(
      actionButton("refresh", "Refresh Data"),
      tags$p("Data: USGS Water Data APIs (parameter 00060)."),
      tags$p("Continuous readings are displayed in Arizona time (MST).")
    ),
    mainPanel(
      h3("Latest Daily Mean Discharge and Maximum Observed Discharge"),
      tableOutput("latestValues"),
      h3("Daily Mean Discharge"),
      plotlyOutput("dischargePlot"),
      h3("Continuous (Instantaneous) Discharge"),
      plotlyOutput("hourlyDischargePlot")
    )
  )
)

server <- function(input, output, session) {

  # Date window follows Arizona local calendar dates rather than the server's
  # clock setting. Refresh on page load and again when the button is clicked.
  date_window <- reactive({
    end_time <- Sys.time()
    end_date <- as.Date(end_time, tz = "US/Arizona")
    list(start_date = end_date - 10,
         end_date = end_date,
         end_time = end_time)
  })

  daily_discharge_data <- eventReactive(input$refresh, {
    window <- date_window()
    tryCatch(
      fetch_daily(window$start_date, window$end_date),
      error = function(e) {
        warning("USGS daily data request failed: ", conditionMessage(e))
        empty_daily()
      }
    )
  }, ignoreNULL = FALSE)

  continuous_discharge_data <- eventReactive(input$refresh, {
    window <- date_window()
    tryCatch(
      fetch_continuous(window$start_date, window$end_time),
      error = function(e) {
        warning("USGS continuous data request failed: ", conditionMessage(e))
        empty_continuous()
      }
    )
  }, ignoreNULL = FALSE)

  output$latestValues <- renderTable({
    daily <- daily_discharge_data()
    continuous <- continuous_discharge_data()

    validate(need(nrow(daily) > 0L || nrow(continuous) > 0L,
                  "No USGS discharge data were returned. Try Refresh Data; check RStudio's console for API errors."))

    latest_daily <- if (nrow(daily) > 0L) {
      daily %>%
        dplyr::group_by(.data$Site) %>%
        dplyr::slice_max(.data$Date, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(Site, Date = as.character(.data$Date),
                         Daily_Mean_cfs = .data$Discharge)
    } else {
      data.frame(Site = character(), Date = character(),
                 Daily_Mean_cfs = numeric())
    }

    max_continuous <- if (nrow(continuous) > 0L) {
      continuous %>%
        dplyr::group_by(.data$Site) %>%
        dplyr::summarise(Max_Observed_cfs = max(.data$Discharge, na.rm = TRUE),
                         .groups = "drop")
    } else {
      data.frame(Site = character(), Max_Observed_cfs = numeric())
    }

    gages %>%
      dplyr::select(.data$Site) %>%
      dplyr::left_join(latest_daily, by = "Site") %>%
      dplyr::left_join(max_continuous, by = "Site") %>%
      dplyr::arrange(.data$Site)
  }, digits = 1, na = "—")

  output$dischargePlot <- renderPlotly({
    daily <- daily_discharge_data()
    validate(need(nrow(daily) > 0L,
                  "No daily mean discharge data available for this period."))

    p <- ggplot(daily, aes(x = Date, y = Discharge, color = Site)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      labs(title = "Daily Mean Discharge — Recent 10 Days",
           x = "Date", y = "Discharge (cfs)", color = "Gage Site") +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
      theme_minimal()
    ggplotly(p)
  })

  output$hourlyDischargePlot <- renderPlotly({
    continuous <- continuous_discharge_data()
    validate(need(nrow(continuous) > 0L,
                  "No continuous discharge data available for this period."))

    p <- ggplot(continuous,
                aes(x = dateTime, y = Discharge, color = Site)) +
      geom_line(linewidth = 1.1) +
      labs(title = "Continuous Discharge — Recent 10 Days",
           x = "Date and Time (Arizona MST)",
           y = "Discharge (cfs)", color = "Gage Site") +
      scale_x_datetime(date_labels = "%b %d %H:%M", timezone = "US/Arizona") +
      theme_minimal()
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
