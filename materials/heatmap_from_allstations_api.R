library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
#install.packages("leaflet.extras")
library(leaflet.extras)
library(httr)

############ this is a mini test to include the heat map
## still need to check how to "store" to not call any time suring the day
# api call, tarspot
# Improved date input validation
validate_date <- function(date) {
  # Ensure the date is not in the future
  if (as.Date(date) > Sys.Date()) {
    stop("Date cannot be in the future")
  }
  paste0("The input date is ", date)
  return(as.character(date))
}

# Modify fetch_forecasting_data to use the validation
fetch_forecasting_data <- function(date) {
  tryCatch({
    api_url <- paste0("https://connect.doit.wisc.edu/forecasting_crop_disease/predict_wisconet_stations_risk?forecasting_date=", date)
    response <- POST(
      url = api_url,
      add_headers("Content-Type" = "application/json")
    )
    
    # Check for valid response
    if (status_code(response) != 200) {
      stop(paste("API Error:", status_code(response), "Message:", content(response, as = "text")))
    }
    
    response_content <- content(response, as = "parsed", type = "application/json")
    
    if (is.null(response_content$stations_risk) || length(response_content$stations_risk) == 0) {
      stop("No stations_risk data in API response")
    }
    
    json_string <- response_content$stations_risk[[1]]
    stations_data <- fromJSON(json_string)
    
    stations_df <- bind_rows(lapply(stations_data, bind_rows)) %>%
      mutate(
        across(c(latitude, longitude, tarspot_risk), as.numeric),
        tarspot_risk = 100 * tarspot_risk,  # Scale risk
        popup_content = sprintf(
          "<strong>Station:</strong> %s<br><strong>Tar Spot Risk:</strong> %.1f%%<br><strong>Date:</strong> %s",
          station_name,
          tarspot_risk,
          date
        )
      )
    return(stations_df)
  }, error = function(e) {
    message(paste0("Error fetching data: ", e$message))
    return(NULL)
  })
}




#fetch_forecasting_data('2024-11-01')



# UI 
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),  # Add a theme for better aesthetics
  tags$head(
    tags$style(HTML("
      .well { background-color: #f4f4f4; }
      .shiny-output-error { color: red; }
    "))
  ),
  titlePanel(
    div(
      HTML("Wisconsin Tar Spot Risk <small>Crop Disease Forecasting</small>"),
      style = "color: #333; margin-bottom: 20px;"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      dateInput(
        "date", 
        "Select Date:", 
        value = Sys.Date(),
        min = '2024-01-01',  # Limit to last 30 days
        max = Sys.Date()
      ),
      actionButton(
        "update", 
        "Update Map", 
        icon = icon("refresh"),
        class = "btn-primary"
      ),
      hr(),  # Horizontal line for visual separation
      h4("Map Layers"),
      checkboxInput("show_stations", "Show Stations", value = TRUE),
      checkboxInput("show_heatmap", "Show Heat Map", value = TRUE),
      hr(),
      h4("Visualization Settings"),
      sliderInput(
        "radius", 
        "Heat Map Radius:", 
        min = 5, 
        max = 50, 
        value = 15,
        step = 1
      ),
      sliderInput(
        "blur", 
        "Heat Map Blur:", 
        min = 1, 
        max = 30, 
        value = 20,
        step = 1
      ),
      sliderInput(
        "opacity", 
        "Heat Map Opacity:", 
        min = 0, 
        max = 1, 
        value = 0.8,
        step = 0.1
      )
    ),
    mainPanel(
      leafletOutput("risk_map", height = 600),
      div(
        textOutput("map_info"),
        style = "margin-top: 10px; color: #666;"
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  # Reactive value for stations data
  stations_data <- reactiveVal(
    data.frame(
      longitude = numeric(), 
      latitude = numeric(), 
      tarspot_risk = numeric()
    )
  )
  
  # Information text
  output$map_info <- renderText({
    data <- stations_data()
    # Check if data is NULL or has no rows
    if (is.null(data) || nrow(data) == 0) {
      return("No data available")
    }
    # Calculate the average risk
    avg_risk <- mean(data$tarspot_risk, na.rm = TRUE)
    sprintf(
      "Total Stations: %d | Average Tar Spot Risk: %.2f%%",
      nrow(data),
      avg_risk
    )
  })
  
  
  
  # Default data fetch when app loads
  observe({
    req(input$date)
    data <- fetch_forecasting_data(as.character(Sys.Date()))
    print(data)
    if (!is.null(data)) {
      stations_data(data)
    }
  })
  
  # Update stations data when "Update Map" button is clicked
  observeEvent(input$update, {
    req(input$date)
    data <- fetch_forecasting_data(as.character(input$date))
    #data$tarspot_risk <- data$tarspot_risk*100
    if (!is.null(data)) {
      stations_data(data)
    }
  })
  
  # Color palette for risk levels
  color_palette <- colorNumeric(
    palette = "viridis",  # Changed from magma to viridis for better readability
    domain = c(0, 100)
  )
  
  # Render Leaflet map
  output$risk_map <- renderLeaflet({
    data <- stations_data()
    # If no data, return a default map view
    if (is.null(data) || nrow(data) == 0) {
      return(
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = -89.4, lat = 43.1, zoom = 7)
      )
    }
    
    map <- leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(
        lng = mean(data$longitude, na.rm = TRUE),
        lat = mean(data$latitude, na.rm = TRUE),
        zoom = 7
      )
    
    # Conditional layers
    if (input$show_heatmap) {
      map <- map %>%
        addHeatmap(
          lng = ~longitude,
          lat = ~latitude,
          intensity = ~tarspot_risk,
          blur = input$blur %||% 20,
          max = 1,
          radius = input$radius %||% 15,
          minOpacity = input$opacity %||% 0.8
        )
    }
    
    if (input$show_stations) {
      map <- map %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          popup = ~popup_content,
          radius = 6,
          color = "black",
          fillColor = ~color_palette(tarspot_risk),
          fillOpacity = 0.8,
          weight = 1.5,
          label = ~station_name,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        )
    }
    
    # Add legend
    if (!is.null(data$tarspot_risk) && nrow(data) > 0) {
      map <- map %>%
        addLegend(
          position = "bottomright",
          title = "Tar Spot Risk (%)",
          pal = color_palette,
          values = ~tarspot_risk,
          opacity = 0.8,
          labFormat = labelFormat(suffix = "%")
        )
    }
    
    map
  })
}

# Optional: Add error handling wrapper
shinyApp(ui = ui, server = server)