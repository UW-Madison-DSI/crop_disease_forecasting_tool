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
#install.packages("shinyBS")
#library(shinyBS)

source("functions/stations.R")
source("functions/logic.R")
source("functions/auxiliar_functions.R")



station_choices <- c("All" = "all", setNames(names(stations), 
                      sapply(stations, function(station) station$name)))
logo_src = "logos/uw-logo-horizontal-color-web-digital.svg"

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
    
    div(
      class = "logo-container",
      tags$img(
        src = logo_src,  # Path to the logo file
        style = "height: 100px; width: auto; display: block; margin: 0 auto;"
      )
    ),
    
    tags$div(
      `data-toggle` = "tooltip", 
      title = "Adjust this value to set the threshold for triggering actions.",
      sliderInput("risk_threshold", "Action Threshold (%)", 
                  min = 20, max = 50, value = 35, step = 1)
    ),
    
    # SelectInput with tooltip
    tags$div(
      `data-toggle` = "tooltip", 
      title = "Choose a station to view its risk data.",
      selectInput("custom_station_code", "Please Select a Station", choices = station_choices)
    ),
    
    # DateInput with tooltip
    tags$div(
      `data-toggle` = "tooltip", 
      title = "Pick a date to forecast risk.",
      dateInput("forecast_date", "Select Forecast Date", 
                value = Sys.Date(), 
                min = as.Date("2024-08-01"), 
                max = Sys.Date())
    ),
    
    # CheckboxInput with tooltip
    tags$div(
      `data-toggle` = "tooltip", 
      title = "Check if no fungicide has been applied recently.",
      checkboxInput("fungicide_applied", "No Fungicide in the last 14 days?", value = FALSE)
    ),
    
    tags$div(
      `data-toggle` = "tooltip", 
      title = "Select if the crop is in the V10-R3 growth stage.",
      checkboxInput("crop_growth_stage", "Growth stage within V10-R3?", value = FALSE)
    ),
    tags$p(
      "Note: Weather plots may have a short delay.", 
      style = "color: gray; font-style: italic; font-size: 12px; margin-top: 5px;"
    ),
    # Collapsible Instructions Panel
    tags$div(
      style = "margin-top: 20px;",
      tags$button(
        type = "button",
        class = "btn btn-info",
        `data-toggle` = "collapse",
        `data-target` = "#collapseInstructions",
        "How to Use The Tar Spot Forecasting App"
      ),
      tags$div(
        id = "collapseInstructions",
        class = "collapse",
        style = "border: 1px solid #ccc; padding: 10px; margin-top: 10px; border-radius: 5px;",
        tags$h4("Instructions", style = "margin-top: 0;"),
        tags$p("1. Use the Action Threshold slider to set the desired risk level."),
        tags$p("2. Select a station from the dropdown menu."),
        tags$p("3. Pick a forecast date to view the risk data."),
        tags$p("4. Check if no fungicide has been applied in the last 14 days."),
        tags$p("5. Ensure the crop is within the V10-R3 growth stage."),
        tags$p("6. View the map and risk trend for insights.")
      )
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
      current <- input$forecast_date  # Access the selected date
      
      today_ct <- with_tz(current, tzone = "America/Chicago")
      mo <- 6
      out <- from_ct_to_gmt(today_ct, mo)
      start_time <- out$start_time_gmt
      end_time <- out$end_time_gmt
      result <- call_tarspot_for_station(station_code, station$name, risk_threshold, today_ct)
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
      setView(lng = -89.75, lat = 44.76, zoom = 7) %>%
      addPolygons(
        data = county_boundaries,
        color = "gray",
        weight = 1,
        opacity = 1,
        fillOpacity = 0,
        fillColor = "lightpink",
        group = "County Boundaries",
        popup = ~NAME
      ) %>%
      addLayersControl(
        overlayGroups = c("County Boundaries"),
        options = layersControlOptions(collapsed = TRUE)
      ) 
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
      paste("Tar Spot Risk is ", most_recent_risk_class, most_recent_risk, '%')

    } else {
      NULL
    }
  })
  
  #output$current_date <- renderText({
  #  current <- input$forecast_date
  #  paste("")
  #})
  
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
        geom_hline(yintercept = threshold, linetype = "dashed", color = "black")+
        geom_hline(yintercept = 20, linetype = "dashed", color = "gray")
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
    
    grid.arrange(tarspot_plot
                 , weather_plot, ncol = 2
                 )
  })
  
}

shinyApp(ui = ui, server = server)
