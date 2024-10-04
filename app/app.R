# Install necessary packages if not installed
# install.packages("shiny")
# install.packages("leaflet")

library(shiny)
library(leaflet)

# Define your stations list
stations <- list(
  ALTN = list(
    name = "Arlington",
    latitude = 43.2966,
    longitude = -89.3843,
    location = "Arlington Agricultural Research Station",
    region = "South Central",
    state = "WI"
  ),
  BBCK = list(
    name = "Babcock",
    latitude = 44.2538,
    longitude = -90.269,
    location = "Amundson Cranberry Inc",
    region = "Central",
    state = "WI"
  )
)

# Convert the station names into a vector for selection
station_names <- names(stations)

# Define the UI
ui <- fluidPage(
  titlePanel("Station Map Viewer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_station", "Choose a Station:",
                  choices = station_names)
    ),
    mainPanel(
      leafletOutput("mymap")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Create a reactive expression to get the selected station data
  selected_station_data <- reactive({
    station <- stations[[input$selected_station]]
    return(station)
  })
  
  # Render the leaflet map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -89.5, lat = 43.5, zoom = 7)  # Default map view over WI
  })
  
  # Update the map based on the selected station
  observe({
    station <- selected_station_data()
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addMarkers(lng = station$longitude, lat = station$latitude,
                 popup = paste0("<strong>", station$name, "</strong><br>",
                                station$location, "<br>",
                                "Region: ", station$region, "<br>",
                                "State: ", station$state))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
