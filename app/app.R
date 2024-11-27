library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
#install.packages("leaflet.extras")
library(leaflet.extras)
library(httr)
library(tigris)  # For geographic boundary data
library(sf)      # For spatial data manipulation
options(tigris_use_cache = TRUE)

#library(sf)

source("functions/logic.R")
source("functions/auxiliar_functions.R")
source("functions/api_calls_logic.R")


logo_src = "logos/uw-logo-horizontal-color-web-digital.svg"
county_boundaries <- counties(state = "WI", cb = TRUE, class = "sf")


tool_title <- "Agricultural Forecasting and Advisory System"

# UI 
ui <- navbarPage(
  title = tool_title,
  theme = shinythemes::shinytheme("flatly"),  # Add a theme for better aesthetics
  id = "navbar", 
  # Add custom CSS for UW-Madison branding
  #tags$head(
  #  tags$style(HTML("
  #    /* Navbar styles */
  #    .navbar {
  #      background-color: #c5050c; /* UW-Madison red */
  #      border-color: #990000; /* Darker red for borders */
  #    }
  #    .navbar .navbar-brand {
  #      color: #ffffff !important; /* White for title */
  #      font-weight: bold;
  #    }
  #    .navbar .navbar-nav > li > a {
  #      color: #ffffff !important; /* White for links */
  #    }
  #    .navbar .navbar-nav > li > a:hover {
  #      color: #f2f2f2 !important; /* Light gray on hover */
  #    }
  #    .navbar .navbar-brand:hover {
  #      color: #f2f2f2 !important; /* Light gray on hover */
  #    }
      
  #    /* Background and text colors */
  #    body {
  #      background-color: #ffffff; /* White background */
  #      color: #333333; /* Dark text for readability */
  #    }
      
  #    /* Sidebar panel styling */
  #    .well {
  #      background-color: #f5f5f5; /* Light gray for sidebar panels */
  #      border-color: #c5050c; /* Red border */
  #    }
      
  #    /* Panel headings for consistency */
  #    .panel-heading {
  #      background-color: #c5050c !important;
  #      color: #ffffff !important;
  #    }
      
  #    /* Buttons */
  #    .btn-primary {
  #      background-color: #c5050c;
  #      border-color: #990000;
  #    }
  #    .btn-primary:hover {
  #      background-color: #a00000;
  #      border-color: #800000;
  #    }
  #  "))
  #),
  # Tab 1: Weather Map
  tabPanel(
    title = "Disease Forecasting",
    sidebarLayout(
      sidebarPanel(
        div(
          class = "logo-container",
          tags$img(
            src = logo_src,
            style = "max-width: 100%; height: auto; display: block; margin: 0 auto;"
          )
          
        ),
        hr(),  # Horizontal line for visual separation
        dateInput(
          "forecast_date",
          "Select Forecasting Date:",
          value = Sys.Date(),
          min = '2024-01-01',
          max = Sys.Date()
        ),
        selectInput(
          "disease_name",
          "Select Disease:",
          choices = c(
            "Tar Spot" = 'tarspot',
            "Gray Leaf Spot" = 'gls',
            "Frogeye Leaf Spot" = 'frogeye_leaf_spot'
          )
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
        ),
        div(
          textOutput("station_count"),  # To display the number of stations
          style = "margin-top: 10px; color: #666; font-size: 14px;"
        )
      )
    )
  ),
  
  # Tab 2: Growth Projection
  #tabPanel(
  #  title = "Growth Projection",
  #  fluidPage(
  #    h3("Growth Projection"),
  #    p("This section will display growth projection data.")
  #  )
  #),
  
  # Tab 4: Weather Charts
  tabPanel(
    title = "Weather Charts",
    fluidPage(
      h3("Weather Charts"),
      p("This section will display weather-related charts.")
    )
  ),
  
  # Tab 5: Downloads
  tabPanel(
    title = "Downloads",
    fluidPage(
      h3("Downloads"),
      p("This section will provide downloadable content.")
    )
  ),
  
  # Tab 6: About
  tabPanel(
    title = "About",
    fluidPage(
      h3("About the Agricultural Forecasting and Advisory System"),
      p("This application provides weather-based forecasting and risk assessments for various crop diseases, helping farmers and agricultural researchers make data-driven decisions."),
      h4("Features:"),
      tags$ul(
        tags$li("Interactive weather map with disease risk visualization"),
        tags$li("Dynamic data for different forecasting dates and diseases"),
        tags$li("Downloadable Report")
      ),
      h4("How It Works:"),
      p("The application uses data from trusted weather and agricultural sources to forecast the risk of crop diseases."),
      tags$ul(
        tags$li("Select a disease and forecasting date to view the risk map."),
        tags$li("The map highlights disease risk levels across different weather stations."),
        tags$li("Users can click on stations to get more details and center the map on specific locations.")
      ),
      h4("Credits:"),
      p("This application was developed by a multidisciplinary team of data scientists and agricultural researchers."),
      tags$ul(
        tags$li("Weather data provided by: Wisconet Stations"),
        tags$li("Crop disease data provided by: Plant Pathology at UW Madison"),
        tags$li("This is an innitiative from: the Open Source Program Office at UW Madison")
      ),
      h4("Contact Us:"),
      p("For inquiries or feedback, please reach out to us:"),
      tags$ul(
        tags$li(tags$a(href = "mailto:contact@forecasting-system.com", "Email: ospo@datascienceinstitute.wisc.edu")),
        tags$li(tags$a(href = "mailto:contact@forecasting-system.com", "Email: damon.smith@wisc.edu")),
        tags$li(tags$a(href = "mailto:contact@forecasting-system.com", "Email: maria.oros@wisc.edu")),
        tags$li(tags$a(href = "https://github.com/UW-Madison-DSI/corn_disease_forecast_api.git", "Github Repo: https://github.com/UW-Madison-DSI/corn_disease_forecast_api.git"))
      ),
      h4("Acknowledgments:"),
      p("This project is supported by OSPO and relies on contributions from multiple research groups.")
    )
  )
  
)


server <- function(input, output, session) {
  # Fetch fresh data directly based on user inputs
  stations_data <- reactive({
    req(input$forecast_date)
    req(input$disease_name)
    fetch_forecasting_data(as.character(input$forecast_date), input$disease_name)
  })
  
  # Update stations data when "Update Map" button is clicked
  observeEvent(input$update, {
    req(input$forecast_date)
    req(input$disease_name)
    stations_data()
  })
  
  # Create the color palette with a dynamic domain
  color_palette <- colorNumeric(
    palette = "viridis",
    domain = c(0, 100)
  )
  
  # Render Leaflet map
  output$risk_map <- renderLeaflet({
    data <- stations_data()
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
        lng = -89.75, lat = 44.76, zoom = 7
      )
    
    # Conditional layers
    if (input$show_heatmap) {
      map <- map %>%
        addHeatmap(
          lng = ~longitude,
          lat = ~latitude,
          intensity = ~risk,
          blur = input$blur %||% 20,
          max = 1,
          radius = input$radius %||% 15,
          minOpacity = input$opacity %||% 0.8
        )
    }
    
    if (input$show_stations) {
      map <- map %>%
        addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
        addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
        addProviderTiles("USGS.USTopo", group = "Topographic") %>%  # USGS Topographic
        addProviderTiles("Esri.WorldImagery", group = "Esri Imagery") %>%  # Esri Imagery
        #addProviderTiles(providers$CartoDB.Positron) %>%
        setView(
          lng = -89.75, lat = 44.76, zoom = 7
        ) %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          popup = ~popup_content,
          radius = 6,
          color = "black",
          fillColor = ~color_palette(risk),
          fillOpacity = 0.8,
          weight = 1.5,
          label = ~station_name,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          ),
          layerId = ~station_id
        ) %>%
        addLegend(
          "bottomright",                # Position of the legend
          pal = color_palette,          # The color palette function
          values = data$risk,           # The range of risk values
          title = "Risk (%)",           # Legend title
          labFormat = labelFormat(suffix = "%"),  # Add % suffix to labels
          opacity = 1                   # Opacity of the legend
        )%>%
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
          baseGroups = c("CartoDB Positron","OpenStreetMap", "Topographic",  #"Terrain",
                         "Esri Imagery"),
          overlayGroups = c("County Boundaries"),
          options = layersControlOptions(collapsed = TRUE)
        )%>%
        # Hide "County Boundaries" by default
        hideGroup("County Boundaries")
    }
    return(map)
  })
  
  # Observe click event to center the map on the selected station
  observeEvent(input$risk_map_marker_click, {
    click <- input$risk_map_marker_click
    print(click)
    if (!is.null(click$id)) {
      station_id <- click$id  # Extract the station ID from the click event
      showNotification(paste("Selected Station ID:", station_id), type = "message")
      # Use station_id for further processing
      print(paste("Station ID:", station_id))
    }
    if (!is.null(click)) {
      leafletProxy("risk_map") %>%
        setView(lng = click$lng, lat = click$lat, zoom = 10)  # Adjust zoom level as needed
    }
  })
  
  output$station_count <- renderText({
    data <- stations_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return("No stations available.")
    }
    
    # Calculate mean risk, excluding NA values
    if (input$disease_name=='tarspot'){
      avg_risk <- mean(data$tarspot_risk, na.rm = TRUE)
    }
    if (input$disease_name=='gls'){
      avg_risk <- mean(data$gls_risk, na.rm = TRUE)
    }
    if (input$disease_name=='frogeye_leaf_spot'){
      avg_risk <- mean(data$frogeye_risk, na.rm = TRUE)
    }
    paste(
      "Number of stations: ", nrow(data),
      " | Mean Risk for the selected forecasting date: ", sprintf("%.2f%%", 100*avg_risk),
      "\n ",
      "\n Please check our section About to have a broader suggestion on whether to use this risk forecasting based on the crop practices."
    )
  })
  
}


shinyApp(ui = ui, server = server)