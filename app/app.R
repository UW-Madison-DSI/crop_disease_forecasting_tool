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
# Render the line plot showing the trend of Risk over Date
library(gridExtra)

source("functions/stations.R")
source("functions/logic.R")
source("functions/auxiliar_functions.R")

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
    margin-right: auto;  # Center or align as needed
    text-align: center;",  # Ensure the text aligns properly
      align = "center"  # Change alignment if needed
    ),
    .cssSelector = "nav"
  ),
  
  dashboardSidebar(
    width = 450,
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Public+Sans:wght@400;600;700&display=swap")
    ),
    
    # Custom CSS for controlling appearance
    tags$style(HTML("
      .box {
        padding: 10px;
      }
      .plot-container {
        margin: 0 auto;
      }
      .leaflet-container {
        height: 500px !important;  # Ensure map height fits the layout
      }
    ")),
    
    sidebarMenu(
      h2(strong(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Settings")), style = "font-size:18px;"),
      selectInput("custom_station_code", "Please Select an Station", 
                  choices = station_choices),
      #selectInput("custom_disease", "Please Select A Disease", 
      #            choices = c("Tarspot","Gray Leaf Spot")),
      checkboxInput("fungicide_applied", "No Fungicide in the last 14 days?", value = FALSE),  
      checkboxInput("crop_growth_stage", "Growth stage within V10-R3?", value = FALSE), 
      
      sliderInput("risk_threshold", "Set Risk Threshold (%)", 
                  min = 20, max = 50, value = 35, step = 1),
      tags$p("Note: Weather plots may have a short delay.", style = "color: gray; font-style: italic; font-size: 12px;")
    )
  ),
  
  dashboardBody(
    fluidRow(
      div(
        textOutput("risk_label"),
        style = "font-size: 1.5em; color: yellow; text-align: center; font-weight: bold; margin-bottom: 10px;"
      ),
      box(
        leafletOutput("mymap", height = "600px"),
        width = 12
      )
    ),
    fluidRow(
      # Show the Risk trend plot conditionally
      conditionalPanel(
        condition = "input.custom_station_code != 'all' && input.fungicide_applied && input.crop_growth_stage",
        box(
          h2(strong("Tarspot Risk in the last 7 days"), style = "font-size:18px;"),
          plotOutput("risk_trend"),  # Show the Risk trend plot here
          textOutput("risk_class_text"),
          width = 12  # Full width for visibility
        )
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
      current <- Sys.time()
      today_ct <- with_tz(current, tzone = "America/Chicago")
      
      mo <- 3 # historical data in terms of num of months
      out <- from_ct_to_gmt(today_ct, mo)
      
      # Convert both dates to Unix timestamps in GMT
      start_time <- out$start_time_gmt
      end_time <- out$end_time_gmt
      
      # Call the API or function to get the data
      result <- call_tarspot_for_station(station_code, station_name, risk_threshold, start_time)  # Fetch data
      
      airtemp <- api_call_wisconet_data_daily(station_code, start_time, end_time)
      print("===========here ")
      print(result)
      
      return(list(tarspot = result, airtemp = airtemp))
    } else {
      return(NULL)
    }
  })
  
  # Render the leaflet map
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = -89.75, lat = 44.76, zoom = 6)  # Default map view over Wisconsin
  })
  
  # Update the map based on the selected station(s)
  observe({
    station_data <- selected_station_data()
    selected_station <- input$selected_station  # 
    
    leafletProxy("mymap") %>% clearMarkers()  #
    
    # Center on selected station
    if (!is.null(selected_station)) {
      station <- station_data[[selected_station]]
      leafletProxy("mymap") %>%
        setView(lng = station$longitude, lat = station$latitude, zoom = 3)
    }
    
     
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
  output$risk_label <- renderText({
    # Ensure weather_data is not NULL
    crop_fung <- input$fungicide_applied
    crop_gs <- input$crop_growth_stage
    if (!is.null(weather_data()) && crop_fung && crop_gs) {
      # Extract the most recent Risk_Class as a single value
      most_recent_risk <- weather_data()$tarspot %>%
        slice_max(order_by = date_day, n = 1) %>%  # Get the row with the latest date
        pull(Risk_Class) %>%
        as.character() %>%
        .[1]  # Ensure only one value is taken
      
      # Display the most recent risk level
      paste("Current Risk Level:", most_recent_risk)
    } else {
      NULL  # Hide the output if data is missing
    }
  })
  
  
  output$station_info <- renderText({
    station_code <- input$custom_station_code
    if (station_code == "all") {
      return("You have selected all stations. 
             Please select one to see the risk of tarspot. 
             If you applied a fungicide in the last 14 days to your crop, 
             we can not estimate a probability of tarspot.")
    } else {
      station <- stations[[station_code]]
      paste("You have selected", station$name, "in", station$state)
    }
  })
  
  output$risk_trend <- renderPlot({
    # Get the weather data
    weatheroutputs <- weather_data() 
    
    # Select the columns of interest from tarspot and airtemp
    data <- weatheroutputs$tarspot
    
    variables_at_rh <- weatheroutputs$airtemp
    station_code <- input$custom_station_code
    station <- stations[[station_code]]
    
    # Plot 1: Tarspot Risk Trend Plot
    tarspot_plot <- NULL
    if (!is.null(data)) {
      df <- data %>%
        mutate(Date = ymd(date_day)) %>%    # Convert date_day to Date
        dplyr::select(Date, Risk, Risk_Class)
      
      tarspot_plot <- plot_trend(df, station) +
        geom_hline(yintercept = 35, linetype = "dashed", color = "red")  # Horizontal line at Risk = 35
    } else {
      tarspot_plot <- ggplot() + 
        ggtitle("No Tarspot Data Available") +
        theme_void()
    }
    
    # Plot 2: Weather Data (Temperature and Humidity)
    weather_plot <- NULL
    if (!is.null(variables_at_rh)) {
      # Example call to the function
      weather_plot <- plot_weather_data(variables_at_rh, station = station)
        
      # Display the plot
      print(weather_plot)
      
    } else {
      weather_plot <- ggplot() + 
        ggtitle("No Weather Data Available") +
        theme_void()
    }
    
    # Arrange the two plots side by side
    grid.arrange(tarspot_plot, weather_plot, ncol = 2)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
