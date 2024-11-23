library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)


############ this is a mini test to include the heat map
## still need to check how to "store" to not call any time suring the day
# api call, tarspot

fetch_forecasting_data <- function(date) {
  api_url <- paste0("https://connect.doit.wisc.edu/forecasting_crop_disease/predict_wisconet_stations_risk?date=", date)
  
  response <- POST(
    url = api_url,
    add_headers("Content-Type" = "application/json")
  )
  
  if (status_code(response) == 200) {
    response_content <- content(response, as = "parsed", type = "application/json")
    json_string <- response_content$stations_risk[[1]]
    stations_data <- fromJSON(json_string)
    
    stations_df <- bind_rows(lapply(stations_data, bind_rows)) %>%
      mutate(across(c(latitude, longitude, tarspot_risk), as.numeric)) %>%
      mutate(
        tarspot_risk = 100 * tarspot_risk,
        popup_content = sprintf(
          "<strong>Station:</strong> %s<br><strong>Tar Spot Risk:</strong> %.1f%%",
          station_name,
          tarspot_risk
        )
      )
    
    return(stations_df)
  } else {
    stop(paste("Error: ", status_code(response)))
  }
}

# UI 
ui <- fluidPage(
  titlePanel("Heat Map about Tar Spot Risk"),
  sidebarLayout(
    sidebarPanel(
      dateInput("date", "Date:", value = "2024-07-11"),
      actionButton("update", "Update Map"),
      checkboxInput("show_stations", "show Stations", value = TRUE),
      checkboxInput("show_heatmap", "Show heat map", value = TRUE),
      sliderInput("radius", "Ratio:", 
                  min = 5, max = 50, value = 15),
      sliderInput("blur", "Desenfoque:", 
                  min = 1, max = 30, value = 20),
      sliderInput("opacity", "Opacity:", 
                  min = 0, max = 1, value = 0.8)
    ),
    mainPanel(
      leafletOutput("risk_map", height = 600)
    )
  )
)

# Servidor
server <- function(input, output, session) {
  stations_data <- reactiveVal(data.frame())
  
  observeEvent(input$update, {
    req(input$date)
    tryCatch({
      data <- fetch_forecasting_data(as.character(input$date))
      stations_data(data)
      print("ok")
    }, error = function(e) {
      showNotification("Error uploading data. Check Date.", type = "error")
    })
  })
  
  # Crear función de paleta de colores
  color_palette <- colorNumeric(
    palette = "magma",
    domain = c(100, 0)
  )
  
  output$risk_map <- renderLeaflet({
    data <- stations_data()
    req(nrow(data) > 0)
    
    map <- leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(
        lng = mean(data$longitude, na.rm = TRUE),
        lat = mean(data$latitude, na.rm = TRUE),
        zoom = 7
      )
    
    if (input$show_heatmap) {
      map <- map %>%
        addHeatmap(
          lng = ~longitude,
          lat = ~latitude,
          intensity = ~tarspot_risk,
          blur = input$blur,
          max = 100,
          radius = input$radius,
          minOpacity = input$opacity
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
          fillColor = ~color_palette(tarspot_risk), # Color based on `tarspot_risk`
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
    
    # ADD legend
    map %>%
      addLegend(
        position = "bottomright",
        title = "Tar Spot Risk (%)",
        pal = color_palette,
        values = ~tarspot_risk,
        opacity = 0.8,
        labFormat = labelFormat(suffix = "%")
      )
  })
  
}

# Shiny test
shinyApp(ui = ui, server = server)
