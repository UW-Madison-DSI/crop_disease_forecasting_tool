library(shiny)
library(leaflet)
library(shinydashboard)
library(scales)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(dplyr)
library(flexdashboard)
library(lubridate)
library(tigris)  # For county data
library(sf)      # For handling spatial data
library(gridExtra)
library(plotly) 

source("functions/stations.R")
source("functions/logic.R")
source("functions/auxiliar_functions.R")

station_choices <- c("All" = "all", setNames(names(stations), 
                      sapply(stations, function(station) station$name)))

# Load county data for Wisconsin
county_boundaries <- counties(state = "WI", cb = TRUE, class = "sf")

# Define UI
ui <- dashboardPage(
  title = "Tar Spot Forecasting App (Beta)",
  
  dashboardHeader(
    title = "Tar Spot Forecasting App (Beta)",
    titleWidth = 450
  ),
  
  dashboardSidebar(
    width = 450,
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Public+Sans:wght@400;600;700&display=swap")
    ),
    tags$style(HTML("
      .box {
        padding: 10px;
      }
      .plot-container {
        margin: 0 auto;
      }
      .leaflet-container {
        height: 500px !important;
      }
    ")),
    sidebarMenu(
      #h2(strong(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Settings")), style = "font-size:18px;"),
      sliderInput("risk_threshold", "Action Threshold (%)", 
                  min = 20, max = 50, value = 35, step = 1),
      menuItem(textOutput("current_date"), icon = icon("calendar-day")),
      selectInput("custom_station_code", "Please Select a Station", 
                  choices = station_choices),
      checkboxInput("fungicide_applied", "No Fungicide in the last 14 days?", value = FALSE),  
      checkboxInput("crop_growth_stage", "Growth stage within V10-R3?", value = FALSE), 
      
      tags$p("Note: Weather plots may have a short delay.", style = "color: gray; font-style: italic; font-size: 12px;")
      # Note and today's date as menu items
      #menuItem(text = "Note: Weather plots may have a short delay.", icon = icon("info-circle")),
    )
  ),
  
  dashboardBody(
    fluidRow(
      conditionalPanel(
        condition = "input.custom_station_code != 'all' && input.fungicide_applied && input.crop_growth_stage",
        div(
          textOutput("risk_label"),
          style = "
          font-size: 1.5em; 
          color: green; 
          text-align: left; 
          font-weight: bold; 
          margin-bottom: 10px; 
          margin-left: 20px;
          padding: 10px;
          border: 2px solid green;
          border-radius: 5px;
          background-color: #f9f9f9;
          box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.1);"
        )
      ),
      box(
        leafletOutput("mymap", height = "600px"),
        width = 12
      )
    ),
    fluidRow(
      conditionalPanel(
        condition = "input.custom_station_code != 'all' && input.fungicide_applied && input.crop_growth_stage",
        box(
          h2(strong("Tar Spot Risk Trend"), style = "font-size:18px;"),
          plotOutput("risk_trend"),
          textOutput("risk_class_text"),
          width = 12
        )
      )
    ),
    fluidRow(
      box(
        textOutput("station_info"),
        tableOutput("weather_data"),
        width = 12
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to get the selected station data or all stations
  selected_station_data <- reactive({
    station_code <- input$custom_station_code
    if (station_code == "all") {
      return(stations)
    } else {
      return(list(station_code = stations[[station_code]]))
    }
  })
  
  # Fetch station weather data and risk probability
  weather_data <- reactive({
    station_code <- input$custom_station_code
    if (station_code != "all") {
      station <- stations[[station_code]]
      risk_threshold <- input$risk_threshold / 100
      current <- Sys.time()
      today_ct <- with_tz(current, tzone = "America/Chicago")
      mo <- 6
      out <- from_ct_to_gmt(today_ct, mo)
      start_time <- out$start_time_gmt
      end_time <- out$end_time_gmt
      result <- call_tarspot_for_station(station_code, station$name, risk_threshold, start_time)
      airtemp <- api_call_wisconet_data_daily(station_code, start_time, end_time)
      return(list(tarspot = result, airtemp = airtemp))
    } else {
      return(NULL)
    }
  })
  
  # Render the leaflet map with an initial layer control
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -89.75, lat = 44.76, zoom = 6) %>%
      addPolygons(
        data = county_boundaries,
        color = "darkgreen",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.2,
        fillColor = "lightpink",
        group = "County Boundaries",
        popup = ~NAME
      ) %>%
      addLayersControl(
        overlayGroups = c("County Boundaries"),
        options = layersControlOptions(collapsed = TRUE)
      ) 
  })
  
  output$current_date <- renderText({
    paste("Today’s Date:", Sys.Date())
  })
  
  # Update map based on selected station
  observe({
    station_data <- selected_station_data()
    leafletProxy("mymap") %>% clearMarkers()
    for (station_code in names(station_data)) {
      station <- station_data[[station_code]]
      leafletProxy("mymap") %>%
        addMarkers(lng = station$longitude, lat = station$latitude,
                   popup = paste0("<strong>", station$name, "</strong><br>",
                                  station$location, "<br>",
                                  "Region: ", station$region, "<br>",
                                  "State: ", station$state))
    }
  })
  
  output$risk_label <- renderText({
    crop_fung <- input$fungicide_applied
    crop_gs <- input$crop_growth_stage
    if (!is.null(weather_data()) && crop_fung && crop_gs) {
      # Extract the most recent risk data
      most_recent_data <- weather_data()$tarspot %>%
        slice_max(order_by = date_day, n = 1)  # Get the row with the latest date
      
      # Extract Risk and Risk_Class separately
      most_recent_risk <- most_recent_data %>% pull(Risk)%>%round(2)%>%
        as.character() %>%.[1]
      most_recent_risk_class <- most_recent_data %>% pull(Risk_Class)%>%
        as.character() %>%.[1]
      # Combine Risk Class and formatted Risk into a single message
      paste("Risk ", most_recent_risk_class, most_recent_risk, '%')

    } else {
      NULL
    }
  })
  
  output$station_info <- renderText({
    station_code <- input$custom_station_code
    if (station_code == "all") {
      return("You have selected all stations. 
             Please select one to see the risk of Tar Spot. 
             If you applied a fungicide in the last 14 days to your crop, we cannot estimate a probability of Tar Spot.")
    } else {
      station <- stations[[station_code]]
      paste("You have selected", station$name, "in", station$state)
    }
  })
  
  output$risk_trend <- renderPlot({
    weatheroutputs <- weather_data()
    data <- weatheroutputs$tarspot
    variables_at_rh <- weatheroutputs$airtemp
    station_code <- input$custom_station_code
    threshold<-input$risk_threshold
    station <- stations[[station_code]]
    
    tarspot_plot <- NULL
    if (!is.null(data)) {
      df <- data %>%
        mutate(Date = ymd(date_day)) %>%
        select(Date, Risk, Risk_Class)
      tarspot_plot <- plot_trend(df, station) +
        geom_hline(yintercept = threshold, linetype = "dashed", color = "red")
    } else {
      tarspot_plot <- ggplot() +
        ggtitle("No Tar Spot Data Available") +
        theme_void()
    }
    
    weather_plot <- NULL
    if (!is.null(variables_at_rh)) {
      weather_plot <- plot_weather_data(variables_at_rh, station = station)
    } else {
      weather_plot <- ggplot() +
        ggtitle("No Weather Data Available") +
        theme_void()
    }
    
    grid.arrange(tarspot_plot#, weather_plot, ncol = 2
                 )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
