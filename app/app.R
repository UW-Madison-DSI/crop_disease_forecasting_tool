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
source("functions/instructions.R")
source("functions/heatmap.R")


tool_title <- "Agricultural Forecasting and Advisory System"

####################################################################### Settings
station_choices <- c("All" = "all", setNames(names(stations), 
                      sapply(stations, function(station) station$name)))

logo_src = "logos/uw-logo-vertical-color-reverse-web-digital.svg"
condition_text <- "input.custom_station_code != 'all' && input.fungicide_applied && input.crop_growth_stage && input.run_model"

widhts <- 450


################################################################ Define UI
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
    
    switchInput(
      inputId = "toggle_switch",
      label = "Wisconet Data",
      value = TRUE,
      onLabel = "ON",
      offLabel = "OFF",
      size = "small"
    ),
    
    # Control panel FROM functions/instructions.R
    risk_buttom,
    tags$div(
      `data-toggle` = "tooltip", 
      title = "Pick a date for which you would like a disease risk forecast.",
      dateInput("forecast_date", "Select Forecast Date For the Station", 
                value = Sys.Date(), 
                min = '2024-05-01', #as.Date(station$earliest_api_date)-35, 
                max = Sys.Date())
    ),
    conditionalPanel(
      condition = "input.forecast_date", # Show only when the toggle switch is ON
      tags$div(
        `data-toggle` = "tooltip", 
        title = "Choose a weather station to view disease risk at this location.",
        selectInput("custom_station_code", "Please Select a Weather Station", choices = station_choices)
      )
    ),
    
    # Instructions panel and section FROM functions/instructions.R
    
    #tags$div(
    #  `data-toggle` = "tooltip", 
    #  title = "Choose a weather station to view disease risk at this location.",
    #  selectInput("custom_station_code", "Please Select a Weather Station", choices = station_choices)
    #),
    fungicide_applied_buttom,
    crop_growth_stage_buttom,
    run_model_buttom,
    instructions_section,
    instructions_panel,
    instructions_block,
    contact_info
  ),
  
  dashboardBody(
    tags$style(HTML("
          .btn-primary {
            font-weight: sans; /* Makes all buttons with the 'btn-primary' class bold */
          }
          p {
            font-weight: sans; /* Makes all paragraphs bold */
          }
          h2 {
            font-weight: sans; /* Makes all h2 headings bold */
          }
        ")
    ),
    
    fluidRow(
      conditionalPanel(
        condition = condition_text,
        div(
          downloadButton("download_report", "Download Report", 
                         class = "btn-primary", 
                         style = "margin: 1px;"),
          style = "text-align: center; margin-bottom: 10px;"
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
                  font-weight: sans; 
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
          h2(strong("Tar Spot Risk Trend"), style = "font-size:18px; font-weight: sans;"),
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
      #earliest_date <- as.Date(station$earliest_api_date)
      
      # Check if a date is already selected; otherwise, default to Sys.Date()
      #current_forecast_date <- isolate(input$forecast_date)  # Preserve user selection

      risk_threshold <- input$risk_threshold / 100
      current <- input$forecast_date  # Access the selected date
      
      today_ct <- with_tz(current, tzone = "America/Chicago")
      out <- from_ct_to_gmt(today_ct, 1.5) # 6 mo
      end_time <- out$end_time_gmt
      result <- call_tarspot_for_station(station_code, 
                                         station$name, 
                                         risk_threshold, 
                                         today_ct)
      airtemp <- api_call_wisconet_data_daily(station_code, #start_time, 
                                              end_time)
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
  
  output$user_mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() #%>%  # Add OpenStreetMap tiles
  })
  
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
  
  # Update map based on selected station
  observe({
    station_code <- input$custom_station_code
    #this is the condition on the map interactivity
    if (TRUE) {
      if (input$toggle_switch==TRUE){
        station_data <- selected_station_data()
        if (station_code == "all") {
          for (station_code in names(station_data)) {
            station <- station_data[[station_code]]
            leafletProxy("mymap") %>%
              setView(lng = -89.75, lat = 44.76, zoom = 7) %>%
              addMarkers(
                lng = station$longitude, lat = station$latitude,
                popup = paste0(
                  "<strong> Station: ", station$name, "</strong><br>",
                  "Location: ", station$location, "<br>",
                  "Region: ", station$region, "<br>",
                  "County: ", station$county, "<br>",
                  "State: ", station$state, "<br>",
                  "Station available since : ", station$earliest_api_date
                )
              )
          }
        } else {
          station <- stations[[station_code]]
  
          lon_value <- station$longitude
          lat_value <- station$latitude
          showNotification(paste("Station", station$name), type = "default")
          
          leafletProxy("mymap") %>%
            clearMarkers() %>%
            setView(lng = lon_value, lat = lat_value, zoom = 15) %>%
            addMarkers(
              lng = lon_value,
              lat = lat_value,
              popup = paste0(
                "<strong> Station: ", station$name, "</strong><br>",
                "Location: ", station$location, "<br>",
                "Region: ", station$region, "<br>",
                "County: ", station$county, "<br>",
                "State: ", station$state, "<br>",
                "Station available since : ", station$earliest_api_date
              )
            )
        }
      }else if (input$toggle_switch==FALSE){
        showNotification("This is a heatmap", type = "default")
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
    } else {
      # Toggle is OFF: Enable map click for user to select location
      showNotification("Click the map to choose a location within WI", type = "default")
      
      # Observe click events on the map
      observeEvent(input$mymap_click, {
        click <- input$mymap_click
        if (!is.null(click)) {
          # Extract clicked coordinates
          clicked_lat <- click$lat
          clicked_lng <- click$lng
          
          # Check if the click is within bounds
          if (clicked_lat >= lat_min && clicked_lat <= lat_max &&
              clicked_lng >= lng_min && clicked_lng <= lng_max) {
            
            # Valid click: Update map with the clicked location
            leafletProxy("mymap") %>%
              clearMarkers() %>%
              addMarkers(
                lng = clicked_lng,
                lat = clicked_lat,
                popup = paste0(
                  "<strong>Selected Location</strong><br>",
                  "Latitude: ", round(clicked_lat, 4), "<br>",
                  "Longitude: ", round(clicked_lng, 4)
                )
              )
            
            # Update UI to display the clicked coordinates
            output$selected_location <- renderText({
              paste("Selected Location - Latitude:", round(clicked_lat, 4), 
                    ", Longitude:", round(clicked_lng, 4))
            })
          } else {
            # Invalid click: Show a notification
            showNotification("Click is outside the bounds of Wisconsin.", type = "error")
          }
        }
      })
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
      paste("The Risk of Tar Spot is ", most_recent_risk_class,': ', most_recent_risk, '%')
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
      station <- stations[[station_code]]
      paste("You have selected ", station$name, "Station.")
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
        mutate(Date = ymd(date_day)+1) %>%
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
      weather_plot<-NULL
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
  
  ################# Define download handler and generates PDF report based on the Latex header
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
      paste0("UWMadison_TarSpotForecast_Report_", input$custom_station_code, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Create temporary directory
      temp_dir <- tempdir()
      
      # Copy required files
      copy_report_files(temp_dir)
      
      # Prepare Tar Spot data
      tarspot_7d <- prepare_tarspot_data(weather_data())
      
      # Get station information
      station_address <- get_station_address(input$custom_station_code, stations)
      
      # Prepare report parameters
      report_params <- list(
        station_address = station_address,
        forecast_date = input$forecast_date,
        threshold = input$risk_threshold,
        fungicide = input$fungicide_applied,
        growth_stage = input$crop_growth_stage,
        tarspot = tarspot_7d
      )
      
      # Render the report
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

################################################ The magic
shinyApp(ui = ui, server = server)
