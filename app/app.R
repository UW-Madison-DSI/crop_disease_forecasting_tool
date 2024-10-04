# Install necessary packages if not installed
# install.packages("shiny")
# install.packages("leaflet")

library(shiny)
library(leaflet)
library(shinydashboard)
library(scales)
library(shinyWidgets)

# Load functions and station data
source("functions/stations.R")

# Vector where station names are displayed with "All" option
station_choices <- c("All" = "all", setNames(names(stations), sapply(stations, function(station) station$name)))

# Define UI
ui <- dashboardPage(
  title = "Tarspot Forecasting App",
  
  dashboardHeader(
    titleWidth = 450
  ) |> tagAppendChild(
    div(
      "Tarspot Forecasting App",
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
    width = 460,
    
    # Custom CSS for controlling appearance
    tags$style(HTML(".js-irs-0 .irs-single,
                    .js-irs-0 .irs-bar-edge,
                    .js-irs-0 .irs-bar {background: #006939},
                    .skin-blue .main-header .logo {
                              background-color: #006939;
                    }
                    .skin-blue .main-header .navbar .sidebar-toggle:hover {
                              background-color: #006939;
                    }
                    .logo {background-color: #006939 !important;}
                    .navbar {background-color: #006939 !important;}")),
    
    sidebarMenu(
      h2(strong("Crop Characteristics"), style = "font-size:18px;"),
      selectInput("fungicide_applied", "Did you apply fungicide in the last 14 days?", 
                  choices = c("Yes", "No")),
      selectInput("crop_growth_stage", "What is the growth stage of your crop?", 
                  choices = c("V10-V15", "R1", "R2", "R3")),
      selectInput("custom_station_code", "Please Select a Station", 
                  choices = station_choices)  # Added "All" option
    )
  ),
  
  dashboardBody(
    fluidRow(
      box(
        leafletOutput("mymap", height = "600px"),
        textOutput("station_info"),
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
      return("You have selected all stations.")
    } else {
      station <- stations[[station_code]]
      paste("You have selected", station$name, "in", station$state)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
