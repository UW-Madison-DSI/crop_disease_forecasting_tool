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


source("functions/auxiliar_functions.R")
source("functions/api_calls_logic.R")
source("functions/instructions.R")
source("functions/logic.R")


logo_src = "logos/uw-logo-horizontal-color-web-digital.svg"
county_boundaries <- counties(state = "WI", cb = TRUE, class = "sf")

tool_title <- "Agricultural Forecasting and Advisory System"

risk_class_function <- function(risk, disease_name, threshold) {
  if (disease_name == "gls") {
    return(ifelse(risk <= threshold * 0.5, "Low",
                  ifelse(risk <= threshold, "Moderate", "High")))
  } else {
    return("No Class") # Default case for unsupported diseases
  }
}

custom_disease_name <- function(disease){
  if (disease=='tarspot'){
    return(
      "Tar Spot"
    )
  }else if (disease=='gls'){
    return(
      "Gray Leaf Spot"
    )
  }else if (disease=='frogeye_leaf_spot'){
    return(
      "Frogeye Leaf Spot"
    )
  }
}

# UI 
ui <- navbarPage(
  title = tool_title,
  theme = shinythemes::shinytheme("flatly"),  # Add a theme for better aesthetics
  id = "navbar", 
  
  tags$head(
    tags$style(HTML("
    .logo-container img {
      max-height: 80px;
      max-width: 100%;
      margin: 10px auto;
      display: block;
    }
  "))
  ),
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
            style = "max-width: 100%; max-height: 80px; display: block; margin: 10px auto;" # Limit height
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
        h4("Crop Management"),
        checkboxInput("no_fungicide", "No fungicide applied in the last 14 days?", value = TRUE),
        checkboxInput("growth_stage", "Growth stage in the recommended range?", value = TRUE),
        conditionalPanel(
          condition = "input.disease_name == 'tarspot'",
          sliderInput(
            "risk_threshold",
            "Risk Threshold (Tar Spot):",
            min = 20,
            max = 50,
            value = 35,
            step = 1
          )
        ),
        conditionalPanel(
          condition = "input.disease_name == 'gls'",
          sliderInput(
            "risk_threshold",
            "Risk Threshold (Gray Leaf Spot):",
            min = 10,
            max = 40,
            value = 25,
            step = 1
          )
        ),
        conditionalPanel(
          condition = "input.disease_name == 'frogeye_leaf_spot'",
          sliderInput(
            "risk_threshold",
            "Risk Threshold (Frogeye Leaf Spot):",
            min = 5,
            max = 30,
            value = 15,
            step = 1
          )
        ),
        
        hr(),  # Horizontal line for visual separation
        h4("Map Layers"),
        checkboxInput("show_stations", "Show Stations", value = TRUE),
        checkboxInput("show_heatmap", "Show Heat Map", value = TRUE),
        #hr(),
        #h4("Visualization Settings"),
        #sliderInput(
        #  "radius", 
        #  "Heat Map Radius:", 
        #  min = 5, 
        #  max = 50, 
        #  value = 15,
        #  step = 1
        #),
        #sliderInput(
        #  "blur", 
        #  "Heat Map Blur:", 
        #  min = 1, 
        #  max = 30, 
        #  value = 20,
        #  step = 1
        #),
        #sliderInput(
        #  "opacity", 
        #  "Heat Map Opacity:", 
        #  min = 0, 
        #  max = 1, 
        #  value = 0.8,
        #  step = 0.1
        #)
      ),
      mainPanel(
        leafletOutput("risk_map", height = 700),
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
  
  # Tab 4: Weather Charts
  tabPanel(
    title = "Weather Charts",
    fluidPage(
      h3("Weather Charts"),
      textOutput("weather_charts"),
      p("This section will display weather-related charts for the choosed Wisconet Station.")
    )
  ),
  
  # Tab 5: Downloads
  tabPanel(
    title = "Downloads",
    fluidPage(
      h3("Downloads"),
      textOutput("download_report"),
      p("This section will provide downloadable content as a summary of the risk trend for the specified disease, wisconet station and forecasting date."),
      div(
        downloadButton("download_report", "Download Report", 
                       class = "btn-primary", 
                       style = "margin: 1px;"),
        style = "text-align: center; margin-bottom: 10px;"
      )
    )
  ),
  
  # Tab 6: About
  tabPanel(
    title = "About",
    about_page
  )
  
)


server <- function(input, output, session) {
  # Fetch fresh data directly based on user inputs
  stations_data <- reactive({
    req(input$forecast_date)
    req(input$disease_name)
    fetch_forecasting_data(as.character(input$forecast_date), input$disease_name)
  })
  
  output$slider_value <- renderText({
    paste("Selected Risk Threshold Value:", input$risk_threshold)
  }) 
  
  # Update stations data when "Update Map" button is clicked
  observeEvent(input$update, {
    req(input$forecast_date)
    req(input$disease_name)
    stations_data()
  })
  
  ################################################################## This is the section 1 risk_map
  output$risk_map <- renderLeaflet({
    data <- stations_data()
    
    risk_max <- min(max(data$risk)+1,100)
    color_palette <- colorNumeric(
      palette = "viridis",
      domain = c(0, risk_max)
    )
    if (is.null(data) || nrow(data) == 0) {
      return(
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = -89.75, lat = 44.76, zoom = 7.2)
      )
    }
    
    map <- leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(
        lng = -89.75, lat = 44.76, zoom = 7.2
      )
    
    # Conditional layers
    if (input$show_heatmap) {
      map <- map %>%
        addHeatmap(
          lng = ~longitude,
          lat = ~latitude,
          intensity = ~risk,
          blur = 20,
          max = 1,
          radius = 15,
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
          lng = -89.75, lat = 44.76, zoom = 7.2
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
          title = paste0("Predicted Risk (%) of \n", 
                         custom_disease_name(input$disease_name)),           # Legend title
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
  
  shared_data <- reactiveValues(
    w_station_id = NULL
  )
  
  # Observe click event to center the map on the selected station
  observeEvent(input$risk_map_marker_click, {
    click <- input$risk_map_marker_click
    print(click)
    if (!is.null(click$id)) {
      station_id <- click$id  # Extract the station ID from the click event
      shared_data$w_station_id <- click$id 
      showNotification(paste("Selected Station ID:", station_id), type = "message")
      # Use station_id for further processing
      print(paste("Station ID:", station_id))
    }
    if (!is.null(click)) {
      leafletProxy("risk_map") %>%
        setView(lng = click$lng, lat = click$lat, zoom = 11)  # Adjust zoom level as needed
    }
  })
  

  output$station_count <- renderText({
    data <- stations_data()
    print(input$disease_name)
    print(input$risk_threshold)
    if (is.null(data) || nrow(data) == 0) {
      return("No stations available.")
    }
    # Calculate mean risk, excluding NA values
    if (input$disease_name=='tarspot'){
      avg_risk <- mean(data$tarspot_risk, na.rm = TRUE)
      data$risk_class <- risk_class_function(data$tarspot_risk, input$disease_name, .35)
    }
    if (input$disease_name=='gls'){
      avg_risk <- mean(data$gls_risk, na.rm = TRUE)
      data$risk_class <- risk_class_function(data$gls_risk, input$disease_name, .5)
    }
    if (input$disease_name=='frogeye_leaf_spot'){
      avg_risk <- mean(data$frogeye_risk, na.rm = TRUE)
      data$risk_class <- risk_class_function(data$frogeye_risk, input$disease_name, .6)
    }
    
    risk_counts <- table(data$risk_class)
    
    # Construct a descriptive text
    risk_summary <- paste(names(risk_counts), risk_counts, sep = ": ", collapse = " | ")
    
    paste(
      "Number of stations: ", nrow(data), "\n",
      "Mean Risk for the selected forecasting date: ", sprintf("%.1f%%", 100 * avg_risk)#,
      #"\n Stations per Risk Class: ", risk_summary
    )
  })
  
  ################################################################## This is the section 2 or tab panel Weather Charts
  output$weather_charts <- renderText({
    if(!is.null(shared_data$w_station_id)){
      paste(
        "Disease Selected:", input$disease_name, "\n",
        
        "Forecast Date:", input$forecast_date, "\n",
        
        "Selected Station ID:", ifelse(is.null(shared_data$w_station_id), "None", shared_data$w_station_id)
      )
    }else{
      print("No station")
    }
  })
  
  ################################################################## This is the section 3 Download
  # Create LaTeX header file - fix escape sequences
  cat('\\usepackage{fancyhdr}
    \\usepackage[margin=1in]{geometry}
    \\usepackage{graphicx}
    \\usepackage{color}
    
    \\fancypagestyle{watermark}{
      \\fancyfootoffset{15pt}
      \\renewcommand{\\headrulewidth}{0pt}
      \\fancyhf{}
      \\cfoot{\\textcolor{gray!30}{\\scalebox{4}{TarSpot Forecast}}}
    }
    \\pagestyle{watermark}
    \\AtBeginDocument{\\thispagestyle{watermark}}
    ', file = "header.tex")
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("UWMadison_TarSpotForecast_Report_", shared_data$w_station_id, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Create temporary directory
      temp_dir <- tempdir()
      
      # Copy required files
      copy_report_files(temp_dir)
      
      # Prepare Tar Spot data
      #tarspot_7d <- prepare_tarspot_data(weather_data())
      
      # Get station information
      #station_address <- get_station_address(shared_data$w_station_id, stations)
      
      # Prepare report parameters
      report_params <- list(
        #station_address = station_address,
        forecast_date = input$forecast_date,
        threshold = input$risk_threshold,
        fungicide = input$fungicide_applied,
        growth_stage = input$crop_growth_stage,
        tarspot = NULL
      )
      
      # Render the report
      tryCatch({
        rmarkdown::render(
          file.path(temp_dir, "report_template.Rmd"),
          output_file = file,
          params = list(
            #selected_station = station_code,
            #station_address = station_address,
            forecast_date = input$forecast_date,
            threshold = input$risk_threshold,
            fungicide = input$fungicide_applied,
            growth_stage = input$crop_growth_stage,
            tarspot = NULL
          )
        )
      }, error = function(e) {
        stop(paste("Failed to render report:", e$message))
      })
    }
  )
}


shinyApp(ui = ui, server = server)