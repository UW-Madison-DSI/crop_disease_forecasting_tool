library(shiny)
library(leaflet)
library(shinydashboard)
library(scales)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(dplyr)
library(flexdashboard)

source("functions/stations.R")
source("functions/logic.R")

station_choices <- c("All" = "all", setNames(names(stations), sapply(stations, function(station) station$name)))

# Define UI
ui <- dashboardPage(
  title = "Tarspot Forecasting App (Beta)",
  
  dashboardHeader(
    titleWidth = 450
  ) |> tagAppendChild(
    div(
      "Tarspot Forecasting App (Beta)",
      style = "
      display: block;
      font-size: 1.5em;
      margin-block-start: 0.5em;
      font-weight: bold;
      color: white;
      margin-right: 50%",
      align = "right"
    ),
    .cssSelector = "nav"
  ),
  
  dashboardSidebar(
    width = 350,
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Public+Sans:wght@400;600;700&display=swap")
    ),
    
    # Custom CSS for controlling appearance
    tags$style(HTML("
      body, h1, h2, h3, h4, h5, h6 {
        font-family: 'Public Sans', sans-serif;
      }
      .main-header .logo {
        font-family: 'Public Sans', sans-serif;
        font-weight: 700;
      }
      .skin-blue .main-header .navbar {
        font-family: 'Public Sans', sans-serif;
        font-weight: 600;
      }
      .sidebar-menu h2 {
        font-family: 'Public Sans', sans-serif;
        font-weight: 600;
        font-size: 18px;
      }
      .box h2 {
        font-family: 'Public Sans', sans-serif;
        font-weight: 700;
        font-size: 18px;
      }
    ")),
    
    sidebarMenu(
      h2(strong(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Settings")), style = "font-size:18px;"),
      selectInput("custom_station_code", "Please Select an Station", 
                  choices = station_choices),
      checkboxInput("fungicide_applied", "No Fungicide in the last 14 days?", value = FALSE),  # Changed to checkbox
      checkboxInput("crop_growth_stage", "Growth stage within V10-R3?", value = FALSE),  # Changed to checkbox
      
      sliderInput("risk_threshold", "Set Risk Threshold (%)", 
                  min = 20, max = 50, value = 35, step = 1)
    )
  ),
  
  dashboardBody(
    fluidRow(
      # Add a box for the Risk Gauge
      conditionalPanel(
        condition = "input.custom_station_code != 'all' && input.fungicide_applied && input.crop_growth_stage",  # Refined condition
        box(
          h2(strong("Tarspot Risk"), style = "font-size:18px;"),
          gaugeOutput("gauge"),  # Risk gauge
          textOutput("risk_class_text"),  # Risk class text output
          width = 12  # Full width for visibility
        )
      )
    ),
    fluidRow(
      box(
        leafletOutput("mymap", height = "600px"),
        width = 12
      )
    ),
    fluidRow(
      box(
        textOutput("station_info"),
        tableOutput("weather_data"),  # Output to show weather data
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
      return(stations)  # Return all stations if "All" is selected
    } else {
      return(list(station_code = stations[[station_code]]))  # Return the selected station as a named list
    }
  })
  
  # Fetch station weather data and risk probability when a station is selected
  weather_data <- reactive({
    station_code <- input$custom_station_code
    if (station_code != "all") {
      station <- stations[[station_code]]
      station_name <- station$name  # Get station name
      risk_threshold <- input$risk_threshold / 100  # Convert risk threshold to a percentage
      
      # Call the API or function to get the data
      result <- call_tarspot_for_station(station_code, station_name, risk_threshold)  # Fetch data
      #print(result)
      return(result)
    } else {
      return(NULL)
    }
  })
  
  # Render the leaflet map
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = -89.758205, lat = 44.769571, zoom = 7)  # Default map view over Wisconsin
  })
  
  # Update the map based on the selected station(s)
  observe({
    station_data <- selected_station_data()
    
    leafletProxy("mymap") %>% clearMarkers()  # Clear previous markers
    
    # Loop through each station and add a marker
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
  
  # Display station info based on the selection
  output$station_info <- renderText({
    station_code <- input$custom_station_code
    if (station_code == "all") {
      return("You have selected all stations. 
             Please select one to see the risk of tarspot. 
             If you have applied a fungicide in the last 14 days to your crop, 
             we can not estimate a probability of tarspot.")
    } else {
      station <- stations[[station_code]]
      paste("You have selected", station$name, "in", station$state)
    }
  })
  
  # Display the fetched weather data in a table
  output$weather_data <- renderTable({
    data <- weather_data()
    
    if (!is.null(data)) {
      # Use dplyr to select specific columns
      selected_data <- data %>%
        dplyr::select(Station, AirTemp_C_30dma, Max_RH_pct_30dma, Tot_Nhrs_RHab90_14dma)  # Replace with actual column names
      
      return(selected_data)
    }
  })
  
  # Render the gauge based on the risk value from weather_data
  output$gauge <- renderGauge({
    weather <- weather_data()
    if (!is.null(weather)) {
      risk_value <- weather$Risk  # Use the actual risk probability from the API
      risk_class <- weather$Risk_Class
      
      gauge(risk_value, 
            min = 0, 
            max = 100, 
            sectors = gaugeSectors(
              success = c(0, 20),  # Green (below 20)
              warning = c(20, 35),  # Yellow (20 to 35)
              danger = c(35, 100)  # Red (above 35)
            )
      )
    } else {
      gauge(0, min = 0, max = 100)  # Default to 0 if no data available
    }
  })
  
  output$risk_class_text <- renderText({
    weather <- weather_data()
    if (!is.null(weather)) {
      paste("Risk Class:", weather$Risk_Class)
    } else {
      "No data available"
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
