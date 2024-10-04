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

# Create a named vector where station names are displayed, but station codes are the values
station_choices <- setNames(names(stations), sapply(stations, function(station) station$name))

# Define the UI
ui <- fluidPage(
  titlePanel("Tarspot Forecasting App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("custom_station_code", "Select the station", choices = station_choices),  # Now displaying names, but selecting by code
      selectInput("fungicide_applied", "Did you apply fungicide in the last 14 days?", 
                  choices = c("No", "Yes")),  
      selectInput("crop_growth_stage", "What is the growth stage of your crop?", 
                  choices = c("V10-V15", "R1", "R2", "R3"))
    ),
    mainPanel(
      leafletOutput("mymap"),
      textOutput("station_info")  # Display station info
    )
  )
)


api_call_wisconet <- function(station) {
  base_url <- 'https://wisconet.wisc.edu'  # Define the base URL
  
  endpoint <- paste0('/api/v1/stations/', station$station_id, '/measures')
  
  params <- list(
    end_time = 1728011869,  # Sample timestamp; replace with actual logic
    start_time = 1726715869,  # Sample timestamp
    fields = 'daily_air_temp_f_max,daily_air_temp_f_min,daily_relative_humidity_pct_max'
  )
  
  # Make the API request
  response <- GET(url = paste0(base_url, endpoint), query = params)
  
  # Check if the request was successful
  if (response$status_code == 200) {
    data1 <- fromJSON(content(response, as = "text"), flatten = TRUE)
    data <- data1$data
    
    # Extract the data and create a DataFrame
    extracted_data <- lapply(data, function(entry) {
      air_temp_max <- NA
      air_temp_min <- NA
      rh_max <- NA
      
      # Extract the measurements
      for (measure in entry$measures) {
        if (measure[1] == 4) {  # Air Temp Max
          air_temp_max <- measure[2]
        }
        if (measure[1] == 6) {  # Air Temp Min
          air_temp_min <- measure[2]
        }
        if (measure[1] == 20) {  # Relative Humidity Max
          rh_max <- measure[2]
        }
      }
      
      return(data.frame(
        collection_time = as.POSIXct(entry$collection_time, origin = "1970-01-01"),
        air_temp_avg = mean(c(air_temp_max, air_temp_min), na.rm = TRUE),
        rh_max = rh_max
      ))
    })
    
    # Convert to a single data frame
    df <- bind_rows(extracted_data)
    return(df)  # Return the data frame to the caller
    
  } else {
    print(paste("Error: ", response$status_code))
    return(NULL)  # Return NULL in case of an error
  }
}



# Define the server logic
server <- function(input, output, session) {
  
  # Reactive expression to get the selected station data by matching the station code
  selected_station_data <- reactive({
    station_code <- input$custom_station_code
    station <- stations[[station_code]]  # Use the selected station code directly
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
        
    if (!is.null(station)) {
      leafletProxy("mymap") %>%
        clearMarkers() %>%
        addMarkers(lng = station$longitude, lat = station$latitude,
                   popup = paste0("<strong>", station$name, "</strong><br>",
                                  station$location, "<br>",
                                  "Region: ", station$region, "<br>",
                                  "State: ", station$state))
    }
  })
  
  # Display station info if a valid station is found
  output$station_info <- renderText({
    station <- selected_station_data()
    if (!is.null(station)) {
      paste("You have selected", station$name, "in", station$state)
    } else {
      "Station not found."
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
