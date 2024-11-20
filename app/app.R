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


source("functions/stations.R")
source("functions/logic.R")
source("functions/auxiliar_functions.R")


############# Settings
tool_title <- "Agricultural Forecasting and Advisory System"
station_choices <- c("All" = "all", setNames(names(stations), 
                      sapply(stations, function(station) station$name)))
logo_src = "logos/uw-logo-horizontal-color-web-digital.svg"
condition_text <- "input.custom_station_code != 'all' && input.fungicide_applied && input.crop_growth_stage && input.run_model"

# Load county data for Wisconsin
county_boundaries <- counties(state = "WI", cb = TRUE, class = "sf")


widhts <- 450

############# Define UI
ui <- dashboardPage(
  
  
  title = tool_title,
  
  dashboardHeader(
    title = tool_title,
    titleWidth = widhts
  ),
  
  dashboardSidebar(
    width = widhts,
    div(
      class = "logo-container",
      tags$img(
        src = logo_src,
        style = "height: 100px; width: auto; display: block; margin: 0 auto;"
      )
    ),
    
    tags$div(
      `data-toggle` = "tooltip", 
      title = "The action threshold defaults to a research-based appropriate level. You are encouraged to leave the threshold at the default.",
      sliderInput("risk_threshold", "Action Threshold (%)", 
                  min = 20, max = 50, value = 35, step = 1)
    ),
    
    # SelectInput with tooltip
    tags$div(
      `data-toggle` = "tooltip", 
      title = "Choose a weather station to view disease risk at this location.",
      selectInput("custom_station_code", "Please Select a Weather Station", choices = station_choices)
    ),
    
    # DateInput with tooltip
    tags$div(
      `data-toggle` = "tooltip", 
      title = "Pick a date for which you would like a disease risk forecast.",
      dateInput("forecast_date", "Select Forecast Date", 
                value = Sys.Date(), 
                min = as.Date("2024-06-01"), 
                max = Sys.Date())
    ),
    
    # CheckboxInput with tooltip
    tags$div(
      `data-toggle` = "tooltip", 
      title = "Check if no fungicide has been applied recently; Forecasts will only be made if no fungicide has been used in the past two weeks.",
      checkboxInput("fungicide_applied", "No Fungicide in the last 14 days?", value = FALSE)
    ),
    
    tags$div(
      `data-toggle` = "tooltip", 
      title = "Check Check if no fungicide has been applied recently; Forecasts will only be made if the crop you are scouting is between V10 and R3 growth stages.",
      checkboxInput("crop_growth_stage", "Growth stage within V10-R3?", value = FALSE)
    ),
    
    tags$div(
      actionButton(
        inputId = "run_model",
        label = "Run Forecast",
        style = "
              background-color: #FFD700; /* Yellow color */
              color: black; 
              font-type: bolt;
              font-size: 16px; 
              margin-top: 30px; 
              padding: 10px; 
              border-radius: 5px; 
              border: none; 
              cursor: pointer; 
              text-align: center;
              margin-left: auto; 
              margin-right: auto;"
      )
    ),
    
    tags$p(
          "Need help getting started? Click below for step-by-step instructions tailored to this app.",
          style = "
            color: gray; 
            font-weight: bold; /* Corrected to font-weight */ 
            font-size: 12px; 
            margin-top: 35px; 
            width: 300px; /* Adjust the width as needed */
            margin-left: auto; 
            margin-right: auto;
          "
    ),
    
    
    # Collapsible Instructions Panel
    tags$div(
      style = "margin-top: 20px;",
      tags$div(
        id = "triangleToggle",
        style = "
          width: 0; 
          height: 0; 
          border-left: 15px solid transparent; 
          border-right: 15px solid transparent; 
          border-top: 15px solid #007bff; 
          cursor: pointer; 
          margin: 0 auto;
        ",
        `data-toggle` = "collapse",
        `data-target` = "#collapseInstructions"
      ),
      tags$div(
        id = "collapseInstructions",
        class = "collapse",
        style = "border: 1px solid #ccc; padding: 10px; margin-top: 10px; border-radius: 3px;",
        tags$h4("User Guide", style = "margin-top: 0;"),
        tags$p("1. Use the Action Threshold slider to set the risk threshold. Leave the slider at the research-based default 
               threshold unless you have informed reason to believe it should be adjusted."),
        tags$p("2. Select a station from the dropdown menu."),
        tags$p("3. Pick a forecast date to view the risk data."),
        tags$p("4. Check if no fungicide has been applied in the last 14 days."),
        tags$p("5. Ensure the crop is within the V10-R3 growth stage."),
        tags$p("6. Push Run the Model to see the map and risk trend for insights."),
        tags$p("7. You can also download a PDF report of the forecast obtained for your location of interest 
                by pushing the “Download Report” button 
               that will appear after the forecast is obtained.")
      )
    )
  ),
  
  dashboardBody(
    tags$style(HTML("
          .btn-primary {
            font-weight: bold; /* Makes all buttons with the 'btn-primary' class bold */
          }
          p {
            font-weight: bold; /* Makes all paragraphs bold */
          }
          h2 {
            font-weight: bold; /* Makes all h2 headings bold */
          }
        ")
    ),
    
    fluidRow(
      conditionalPanel(
        condition = condition_text,
        div(
          downloadButton("download_report", "Download Report", 
                         class = "btn-primary", 
                         style = "margin: 10px;"),
          style = "text-align: center;"
        )
      )
    ),
    fluidRow(
      conditionalPanel(
        condition = condition_text,
        div(
          textOutput("risk_label"),
          style = "
          font-size: 1.5em; 
          color: black; 
          text-align: left; 
          font-weight: bold; 
          margin-bottom: 10px; 
          margin-left: 20px;
          padding: 10px;
          border: 2px solid dark;
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
        condition = condition_text,
        box(
          h2(strong("Tar Spot Risk Trend"), style = "font-size:18px; font-weight: bold;"),
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

################################################ Define server logic
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
      out <- from_ct_to_gmt(today_ct, 6) # 6 mo
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
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("USGS.USTopo", group = "Topographic") %>%  # USGS Topographic
      addProviderTiles("Esri.WorldImagery", group = "Esri Imagery") %>%  # Esri Imagery
      addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
      #addProviderTiles("Esri.WorldTerrain", group = "Terrain") %>%
      #addTiles() %>%
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
        baseGroups = c("OpenStreetMap", "Topographic", "CartoDB Positron", #"Terrain",
                       "Esri Imagery"),
        overlayGroups = c("County Boundaries"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addTiles(group = "OpenStreetMap") 
  })
  
  # Update map based on selected station
  observe({
    station_code <- input$custom_station_code
    station_data <- selected_station_data()
    if (station_code == "all"){
      for (station_code in names(station_data)) {
        #seems like i could compute the risk forecasting here for all stations
        station <- station_data[[station_code]]
        lon_value <- station$longitude
        lat_value <- station$latitude
        leafletProxy("mymap") %>%
          addMarkers(lng = station$longitude, lat = station$latitude,
                     popup = paste0("<strong> Station: ", station$name, "</strong><br>",
                                    "Location: ", station$location, "<br>",
                                    "Region: ", station$region, "<br>",
                                    "State: ", station$state))
      }
    }else{
      station <- stations[[station_code]]
      lon_value <- station$longitude
      lat_value <- station$latitude
      
      leafletProxy("mymap") %>%
        clearMarkers() %>%  # Clear existing markers
        setView(lng = lon_value, lat = lat_value, zoom = 12) %>%  # Different zoom level
        addMarkers(
          lng = lon_value,
          lat = lat_value,
          popup = paste0(
            "<strong>", station$name, "</strong><br>",
            station$location, "<br>",
            "Region: ", station$region, "<br>",
            "State: ", station$state
          )
        )
    }
  })
  
  risk_level <- reactive({
    crop_fung <- input$fungicide_applied
    crop_gs <- input$crop_growth_stage
    
    if (!is.null(weather_data()) && crop_fung && crop_gs) {
      # Extract the most recent risk data
      most_recent_data <- weather_data()$tarspot %>%
        slice_max(order_by = date_day, n = 1)  # Get the row with the latest date
      
      # Extract Risk and Risk_Class separately
      most_recent_risk <- most_recent_data %>%
        pull(Risk) %>%
        round(2) %>%
        as.character() %>%
        .[1]
      most_recent_risk_class <- most_recent_data %>%
        pull(Risk_Class) %>%
        as.character() %>%
        .[1]
      
      # Combine Risk Class and formatted Risk into a single message
      paste("Tar Spot Risk is", most_recent_risk_class, most_recent_risk, '%')
    } else {
      "No data available"
    }
  })

  
  output$station_info <- renderText({
    station_code <- input$custom_station_code
    if (station_code == "all") {
      return("You have selected all stations. 
             Please select one to see the risk of Tar Spot. 
             If you applied a fungicide in the last 14 days to your crop, we cannot estimate a probability of Tar Spot.")
    } else {
      env <- Sys.getenv("test")
      
      station <- stations[[station_code]]
      paste("You have selected ", env, station$name, "Station. Clic on the pin to see specifications.")
    }
  })
  
  output$risk_label <- renderText({
    risk_level()
  })
  
  output$risk_trend <- renderPlot({
    weatheroutputs <- weather_data()
    data <- weatheroutputs$tarspot
    variables_at_rh <- weatheroutputs$airtemp
    station_code <- input$custom_station_code
    threshold <- input$risk_threshold
    station <- stations[[station_code]]
    
    # Initialize plots as NULL
    tarspot_plot <- NULL
    weather_plot <- NULL
    
    # Create tarspot plot
    if (!is.null(data) && nrow(data) > 0) {
      tarspot_df <- data %>%
        mutate(Date = ymd(date_day)) %>%
        select(Date, Risk, Risk_Class)
      tarspot_plot <- plot_trend(tarspot_df, station) +
        geom_hline(yintercept = threshold, linetype = "dashed", color = "black") +
        geom_hline(yintercept = 20, linetype = "dashed", color = "gray")
    } else {
      tarspot_plot <- ggplot() +
        ggtitle("No Tar Spot Data Available") +
        theme_void()
    }
    
    # Create weather plot
    if (!is.null(variables_at_rh) && !identical(variables_at_rh, "Error: 400")) {
      weather_plot <- plot_weather_data(variables_at_rh, station = station)
    } else {
      weather_plot <- ggplot() +
        ggtitle("No Weather Data is Available") +
        theme_void()
    }
    
    # Arrange plots only if both are valid
    if (!is.null(tarspot_plot) && !is.null(weather_plot) && !identical(weather_plot, "Error: 400")) {
      tryCatch({
        grid.arrange(tarspot_plot, weather_plot, ncol = 2)
      }, error = function(e) {
        message("An error occurred while arranging the plots: ", e$message)
      })
    } else if (!is.null(tarspot_plot)) {
      # Display only tarspot plot
      grid.arrange(tarspot_plot, ncol = 1)
    } else if (!is.null(weather_plot) && !identical(weather_plot, "Error: 400")) {
      # Display only weather plot
      grid.arrange(weather_plot, ncol = 1)
    } else {
      message("No valid plots to display.")
    }
  })
  
  # Create LaTeX header file - fix escape sequences
  cat('\\usepackage{fancyhdr}
      \\usepackage[margin=1in]{geometry}
      \\usepackage{graphicx}
      \\fancypagestyle{watermark}{
        \\fancyfootoffset{0pt}
        \\renewcommand{\\headrulewidth}{0pt}
        \\fancyhf{}
        \\cfoot{\\textcolor{gray!30}{\\scalebox{4}{TarSpot Forecast}}}
      }
      \\pagestyle{watermark}
      \\AtBeginDocument{\\thispagestyle{watermark}}
      ', file = "header.tex", sep = "")
  
  # Define download handler
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("TarSpotForecast_Report_", input$custom_station_code, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Create temporary directory for report generation
      temp_dir <- tempdir()
      
      # Copy required files to temp directory
      files_to_copy <- list(
        rmd = c(from = "report_template.Rmd", to = file.path(temp_dir, "report_template.Rmd")),
        header = c(from = "header.tex", to = file.path(temp_dir, "header.tex")),
        logo1 = c(from = "logos/OPENSOURDA_color-flush.png", to = file.path(temp_dir, "OPENSOURDA_color-flush.png")),
        logo2 = c(from = "logos/PLANPATHCO_color-flush.png", to = file.path(temp_dir, "PLANPATHCO_color-flush.png"))
      )
      
      # Copy all files and check for errors
      for (item in files_to_copy) {
        if (!file.copy(item["from"], item["to"], overwrite = TRUE)) {
          stop(paste("Failed to copy file:", item["from"]))
        }
      }
      
      # Prepare Tar Spot data with error handling
      if (!is.null(weather_data())) {
        tarspot_7d <- weather_data()$tarspot %>%
          mutate(
            Risk = round(Risk, 2),
            date_day = as.Date(date_day, format = "%Y-%m-%d") + 1
          )
      } else {
        stop("Weather data is not available")
      }
      
      # Get station information
      station_code <- input$custom_station_code
      if (!station_code %in% names(stations)) {
        stop("Invalid station code")
      }
      station_info <- stations[[station_code]]
      
      # Construct station address
      station_address <- paste(
        station_info$location,
        station_info$region,
        station_info$state,
        sep = ", "
      )
      
      # Render the report with error handling
      tryCatch({
        rmarkdown::render(
          file.path(temp_dir, "report_template.Rmd"),
          output_file = file,
          params = list(
            #selected_station = station_code,
            station_address = station_address,
            forecast_date = input$forecast_date,
            threshold = input$risk_threshold,
            fungicide = input$fungicide_applied,
            growth_stage = input$crop_growth_stage,
            tarspot = tarspot_7d
          )
        )
      }, error = function(e) {
        stop(paste("Failed to render report:", e$message))
      })
    }
  )
  
  
  
}

shinyApp(ui = ui, server = server)
