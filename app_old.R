# Load necessary libraries
library(shiny)
library(dataRetrieval)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)

# Define the UI
ui <- fluidPage(
  titlePanel("Little Colorado River Discharge Data"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("refresh", "Refresh Data")
    ),
    
    mainPanel(
      h3("Latest Daily Discharge Values and Maximum Hourly Discharge"),
      tableOutput("latestValues"),
      
      h3("Daily Discharge Data Plot"),
      plotlyOutput("dischargePlot"),
      
      h3("Hourly Discharge Data Plot"),
      plotlyOutput("hourlyDischargePlot")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  
  # Function to retrieve data for a specific gage for the last 10 days
  get_discharge_data <- function(site_no, site_name, frequency = "daily") {
    end_date <- Sys.Date()
    start_date <- end_date - 10
    
    if (frequency == "daily") {
      # Retrieve daily data
      data <- readNWISdv(siteNumbers = site_no, parameterCd = "00060", 
                         startDate = as.character(start_date), endDate = as.character(end_date))
      data <- data %>%
        rename(Discharge = X_00060_00003) %>%
        mutate(Site = site_name, Frequency = "Daily") %>%
        select(Date, Discharge, Site, Frequency)
    } else if (frequency == "hourly") {
      # Retrieve hourly data
      data <- readNWISuv(siteNumbers = site_no, parameterCd = "00060", 
                         startDate = as.character(start_date), endDate = as.character(end_date))
      data <- data %>%
        rename(Discharge = X_00060_00000) %>%
        mutate(Date = as.Date(dateTime),
               Site = site_name,
               Frequency = "Hourly") %>%
        select(Date, dateTime, Discharge, Site, Frequency)
    }
    return(data)
  }
  
  # Reactive to fetch and store daily discharge data on button click
  daily_discharge_data <- eventReactive(input$refresh, {
    bind_rows(
      get_discharge_data("09400350", "Winslow", "daily"),
      get_discharge_data("09402000", "Cameron", "daily"),
      get_discharge_data("09401260", "Moenkopi", "daily"),
      get_discharge_data("09402300", "LCR Gage Pool", "daily")
    )
  })
  
  # Reactive to fetch and store hourly discharge data on button click
  hourly_discharge_data <- eventReactive(input$refresh, {
    data <- bind_rows(
      get_discharge_data("09400350", "Winslow", "hourly"),
      get_discharge_data("09402000", "Cameron", "hourly"),
      get_discharge_data("09401260", "Moenkopi", "hourly"),
      get_discharge_data("09402300", "LCR Gage Pool", "hourly")
    )
    
    # Convert UTC to US/Arizona (MST)
    data <- data %>%
      mutate(dateTime = with_tz(dateTime, tzone = "US/Arizona"))
    
    return(data)
  })
  
  # Table for the latest daily discharge values and maximum hourly discharge
  output$latestValues <- renderTable({
    req(daily_discharge_data(), hourly_discharge_data())
    
    max_hourly_discharge <- hourly_discharge_data() %>%
      group_by(Site) %>%
      summarise(Max_Hourly_Discharge = max(Discharge, na.rm = TRUE))
    
    latest_data <- daily_discharge_data() %>%
      group_by(Site) %>%
      filter(Date == max(Date, na.rm = TRUE)) %>%
      ungroup() %>%
      select(Site, Date, Discharge) %>%
      mutate(Date = format(Date, "%Y-%m-%d")) %>%
      left_join(max_hourly_discharge, by = "Site") %>% 
      arrange(Site)
    
    latest_data
  })
  
  # Plot for daily discharge data
  output$dischargePlot <- renderPlotly({
    req(daily_discharge_data())
    
    y_max_daily <- max(daily_discharge_data()$Discharge, na.rm = TRUE)
    
    p_daily <- ggplot(daily_discharge_data(), aes(x = Date, y = Discharge, color = Site)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(title = "Daily River Discharge for All Gage Sites - Last 10 Days",
           x = "Date",
           y = "Discharge (cfs)",
           color = "Gage Site") +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
      scale_y_continuous(breaks = seq(0, y_max_daily, by = 50)) +
      theme_minimal()
    
    ggplotly(p_daily)
  })
  
  # Plot for hourly discharge data
  output$hourlyDischargePlot <- renderPlotly({
    req(hourly_discharge_data())
    
    y_max_hourly <- max(hourly_discharge_data()$Discharge, na.rm = TRUE)
    
    p_hourly <- ggplot(hourly_discharge_data(), aes(x = dateTime, y = Discharge, color = Site)) +
      geom_line(size = 1.2) +
      labs(title = "Hourly River Discharge for All Gage Sites - Last 10 Days",
           x = "DateTime (Local MST - US/Arizona)",
           y = "Discharge (cfs)",
           color = "Gage Site") +
      scale_y_continuous(breaks = seq(0, y_max_hourly, by = 100)) +
      theme_minimal()
    
    ggplotly(p_hourly)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)