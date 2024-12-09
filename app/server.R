library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
library(leaflet.extras)
library(tigris)
library(sf)
options(tigris_use_cache = TRUE)
library(DT)
library(gridExtra)


source("functions/1_wisconet_calls.R")
source("functions/2_external_source.R")
source("functions/pdf_template.R")
source("functions/weather_plots.R")

source("functions/auxiliar_functions.R")
#source("functions/api_calls_logic.R")
#source("functions/weather_plots.R")
#source("functions/punctual_estimates.R")

########################################################## SETTINGS: WI boundary
county_boundaries <- counties(state = "WI", cb = TRUE, class = "sf") %>%
  st_transform(crs = 4326)

wi_boundary <- states(cb = TRUE) %>%
  filter(NAME == "Wisconsin") %>%
  st_transform(4326)

wisconsin_bbox <- list(
  lat_min = 42.4919,
  lat_max = 47.3025,
  lng_min = -92.8894,
  lng_max = -86.2495
)


data_transform<-function(data, input){
  if (input$disease_name == 'tarspot') {
    data$risk <- data$tarspot_risk
    data$risk_class <- risk_class_function(data$tarspot_risk, 
                                           input$disease_name, 
                                           input$risk_threshold)
    
  } else if (input$disease_name == 'gls') {
    data$risk <- data$gls_risk
    data$risk_class <- risk_class_function(data$gls_risk, 
                                           input$disease_name, 
                                           input$risk_threshold)
    
  } else if (input$disease_name == 'frogeye_leaf_spot') {
    data$risk <- data$frogeye_risk
    data$risk_class <- risk_class_function(data$frogeye_risk, 
                                           input$disease_name, 
                                           input$risk_threshold)
  }
  data_f <- data %>%
    select(forecasting_date, risk, risk_class) %>%
    rename(
      #Station = station_name,
      `Forecasting Date` = forecasting_date,
      Risk = risk,
      `Risk Class` = risk_class
    )
  return(data_f)
}



########################################################## SETTINGS: WI boundary
server <- function(input, output, session) {
  # Store the clicked coordinates
  coordinates <- reactiveValues(lat = NULL, lng = NULL)
  
  # Fetch fresh data directly based on user inputs
  stations_data <- reactive({
    req(input$forecast_date)
    req(input$disease_name)
    fetch_forecasting_data(as.character(input$forecast_date), input$disease_name)
  })
  
  output$slider_value <- renderText({
    paste("Selected Risk Threshold Value:", input$risk_threshold)
  }) 
  
  ############################################################################## IBM data, AOI: Wisconsin
  
  # Add a marker on user click
  observeEvent(input$risk_map_click, {
    click <- input$risk_map_click
    
    if (!is.null(click) && (input$ibm_data == TRUE)) {
      # Gather the coordinates
      coordinates$lat <- click$lat
      coordinates$lng <- click$lng
      
      # Check if the click is inside Wisconsin
      inside_wisconsin <- click$lat >= wisconsin_bbox$lat_min &&
        click$lat <= wisconsin_bbox$lat_max &&
        click$lng >= wisconsin_bbox$lng_min &&
        click$lng <= wisconsin_bbox$lng_max
      
      if (!inside_wisconsin) {
        showNotification(
          "You clicked outside Wisconsin. Please click within the state boundary.",
          type = "warning"
        )
      } else {
        # Add a marker at the clicked location
        leafletProxy("risk_map") %>%
          clearMarkers() %>%
          clearShapes() %>% # Clear existing shapes to avoid overlaps
          addMarkers(
            lng = click$lng,
            lat = click$lat,
            popup = paste("Latitude:", round(click$lat, 4), "<br>Longitude:", round(click$lng, 4))
          ) %>%
          addPolygons(
            data = wi_boundary,
            color = "blue",
            fillColor = "lightblue",
            weight = 2,
            fillOpacity = 0,
            popup = ~NAME
          ) %>%
          setView(lng = click$lng, lat = click$lat, zoom = 12)
      }
    }
  })
  
  shared_data <- reactiveValues(
    w_station_id = NULL,
    run_model = NULL,
    lat_location = NULL,
    lng_location = NULL,
    ibm_data = NULL
  )
  
  ################################################ Punctual estimates, observe clicks, run model IBM
  observeEvent(input$run_model, {
    # Ensure click has occurred and ibm_data is TRUE
    req(!is.null(input$risk_map_click), input$ibm_data)
    shared_data$run_model<-TRUE
    click <- input$risk_map_click
    punctual_estimate <- ibm_query(input$forecast_date, click$lat, click$lng)
    
    shared_data$ibm_data <- punctual_estimate
    punctual_estimate <- punctual_estimate %>% filter(forecasting_date == as.Date(input$forecast_date))
    
    # Risk calculation
    risk <- NULL
    risk_class <- NULL
    
    if (input$disease_name == 'gls') {
      risk <- punctual_estimate$gls_risk
      risk_class <- punctual_estimate$gls_risk_class
    } else if (input$disease_name == 'tarspot') {
      risk <- punctual_estimate$tarspot_risk
      risk_class <- punctual_estimate$tarspot_risk_class
    } else if (input$disease_name == 'frogeye_leaf_spot') {
      risk <- punctual_estimate$fe_risk
      risk_class <- punctual_estimate$fe_risk_class
    }
    shared_data$lng_location <- click$lng
    shared_data$lat_location <- click$lat
    
    showNotification(paste0(risk_class, "Risk of ", custom_disease_name(input$disease_name),':', round(risk * 100,2), "%"), 
                     type = "message")
    # Display the coordinates and risk information
    output$click_coordinates <- renderText({
      paste(
        "Clicked Coordinates: Latitude =", round(click$lat, 4),
        ", Longitude =", round(click$lng, 4), "|", risk_class,
        " Risk of ", custom_disease_name(input$disease_name),':', round(risk * 100,2), "%"
      )
    })
  })
  
  ################################################################## This is the section 1 risk_map
  output$risk_map <- renderLeaflet({
    if(input$ibm_data==FALSE){
      county_boundaries <- st_transform(county_boundaries, crs = 4326)
      
      data <- stations_data()
      
      lowerb <- if_else(input$disease_name == 'tarspot', 20, 40)
      
      higherb<- input$risk_threshold
      
      # Create a custom palette function
      custom_palette <- function(x) {
        if (x <= lowerb) {
          return("#88CCEE")
        } else if (x > lowerb && x <= higherb) {
          return("#DDCC77")
        } else {
          return("#CC6677")
        }
      }
      
      # Apply the custom palette to create a vector of colors
      data$fill_color <- sapply(data$risk, custom_palette)
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
            blur = 10,
            max = 1,
            radius = 10,
            minOpacity = 0.8,
            gradient = unique(data$fill_color) # Use pre-computed colors
          )
      }
      
      #if (input$show_stations) {
      map <- map %>%
        addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
        addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
        addProviderTiles("USGS.USTopo", group = "Topographic") %>%
        addProviderTiles("Esri.WorldImagery", group = "Esri Imagery") %>%
        setView(lng = -89.75, lat = 44.76, zoom = 7.2) %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          popup = ~popup_content,
          radius = 6,
          color = "black",
          fillColor = ~fill_color,  # Use precomputed fill colors
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
          "bottomright",
          colors = c("#88CCEE", "#DDCC77", "#CC6677"),
          labels = c(paste0("Low (≤ ", lowerb, '%)'), 
                     paste0("Moderate (", lowerb, " - ", higherb,'%)'), 
                     paste0("High (> ", higherb,'%)')),
          title = paste0("Predicted Risk (%)"),
          opacity = 1
        ) %>%
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
          baseGroups = c("CartoDB Positron", "OpenStreetMap", "Topographic", "Esri Imagery"),
          overlayGroups = c("County Boundaries"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        hideGroup("County Boundaries")
    }else{
      map<-leaflet() %>%
        addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
        addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
        addProviderTiles("USGS.USTopo", group = "Topographic") %>% 
        addProviderTiles("Esri.WorldImagery", group = "Esri Imagery") %>%
        setView(lng = -89.75, lat = 44.76, zoom = 7.2) %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "CartoDB Positron","Topographic",  #"Terrain",
                         "Esri Imagery"),
          options = layersControlOptions(collapsed = TRUE)
        )
    }
    return(map)
  })
  
  
  
  # Observe click event to center the map on the selected station
  observeEvent(input$risk_map_marker_click, {
    click <- input$risk_map_marker_click
    
    if (!is.null(click)) {
      print(click)  # Debugging information
      
      # Update shared data with the clicked station ID
      if (!is.null(click$id)) {
        shared_data$w_station_id <- click$id
        
        # Fetch risk class for the clicked station
        rclass <- api_call_this_station_specifications(input, click$id)
        
        if (!is.null(rclass)) {
          # Determine notification type based on risk class
          rclass_msg <- switch(rclass,
                               "High" = "error",
                               "Low" = "message",
                               "high" = "error",
                               "low" = "message",
                               "default")
          
          # Display notification
          #showNotification(
          #  paste("Risk of ",custom_disease_name(input$disease_name)," is ", rclass), 
          #  type = rclass_msg
          #)
        } else {
          warning("Risk class is NULL for station ID: ", click$id)
        }
      } else {
        warning("Click event does not contain an ID.")
      }
      
      # Update the map view to the clicked location
      leafletProxy("risk_map") %>%
        setView(lng = click$lng, lat = click$lat, zoom = 16) %>%
        addProviderTiles("Esri.WorldImagery", group = "Esri Imagery")
    } else {
      warning("No click event detected.")
    }
  })
  
  observeEvent(input$crop_growth_stage,{  # Include inputs to trigger observation
    if (!input$no_fungicide) {
      showNotification(
        paste(
          custom_disease_name(input$disease_name),
          "risk can only be computed if no fungicide was applied in the last 14 days."
        ),
        type = "error"
      )
    } 
    if(!input$crop_growth_stage){
      showNotification(
        paste(
          custom_disease_name(input$disease_name),
          "risk can only be computed if the growth stage is as recommended."
        ),
        type = "error"
      )
    }
  })
  
  output$station_count <- renderText({
    if(input$ibm_data==FALSE){
      data <- stations_data()
      paste("Number of Active Wisconet stations: ", nrow(data),".")
    }else{
      paste("Data from IBM")
    }
  })
  
  disease_risk_data_ibm <- reactive({
    tryCatch({
      # Ensure lat_location and lon_location are not NULL
      if (!is.null(shared_data$lat_location) && !is.null(shared_data$lon_location)) {
        # Query IBM data
        ibm_data <- ibm_query(input$forecast_date, shared_data$lat_location, shared_data$lon_location)
        
        # Check if the result is NULL and return accordingly
        if (is.null(ibm_data)) {
          print("================= NO IBM data nrows")
          return(NULL)
          
        } else {
          paste0("================= IBM data nrows, ", nrow(ibm_data))
          return(ibm_data)
        }
      } else {
        return(NULL) # Return NULL if locations are missing
      }
    }, error = function(e) {
      # Handle errors and log the error message
      paste0("================= NO IBM data nrows ", e$message)
      message("Error occurred while querying IBM data: ", e$message)
      return(NULL) # Return NULL on error
    })
  })
  ############################################################################## This is the section 2 or tab panel Station summary
  disease_risk_data <- reactive({
    if (!is.null(shared_data$w_station_id)) {
      # Define the date range
      given_date <- as.Date(input$forecast_date)
      date_range <- as.list(seq(given_date - 7, given_date, by = "day"))
      brows <- call_forecasting_for_range_of_days(date_range, shared_data$w_station_id, input$disease_name)
      return(brows)
    } else {
      return(NULL)
    }
  })
  
  output$station_specifications <- renderText({
    if (is.null(shared_data$w_station_id)) {
      "This section will display weather-related charts for the chosen Wisconet Station or location. 
      Please select a location from the map by doing click on the station or punctual location."
    } else {
      data <- disease_risk_data()
      # Check if data is not empty
      if (nrow(data) > 0) {
        station <- data$station_name[1]
        earliest_api_date <- data$earliest_api_date[1]
        
        location <- if_else(
          data$location[1] == "Not set", 
          "", paste(" located at", data$location[1], ", ",data$region[1], " Region, "))
        
        date_obj <- as.Date(earliest_api_date, format = "%Y-%m-%d")
        # Format for user-friendly reading
        user_friendly_date <- format(date_obj, "%B %d, %Y")
        paste(
          station, "Station,", location," is active since:", user_friendly_date, "."
        )
      } else {
        "Please select a station by clicking on it in the map from the Disease Forecasting section."
      }
    }
  })
  
  output$risk_trend <- renderPlot({
    data <- disease_risk_data()
    if (!is.null(shared_data$w_station_id)){
      data_prepared <- data_table_for_station_7d(data, input)
      # Plot risk trend
      plot_trend_7days(data_prepared, custom_disease_name(input$disease_name), input$risk_threshold)
    } else {
      # Display an empty plot with a message
      plot.new()
      text(0.5, 0.5, "", cex = 1.5, col = "blue")
    }
  })
  
  output$air_temperature_plot <- renderPlot({
    # Call the API functions to get the data
    data_airtemp <- api_call_weather_data(shared_data$w_station_id, input$forecast_date, "AIRTEMP", "MIN60", 30)
    data_rh <- api_call_weather_data(shared_data$w_station_id, input$forecast_date, "RELATIVE_HUMIDITY", "MIN60", 14)

    if (!is.null(data_airtemp$daily_aggregations) && !is.null(data_rh$daily_aggregations)) {
      # Create the individual plots
      p1 <- plot_air_temp(data_airtemp$daily_aggregations)
      p2 <- plot_rh_dp(data_rh$daily_aggregations)
      p3 <- plot_rh_nh_dp(data_rh$daily_aggregations)
      # Arrange the plots vertically or side by side
      grid.arrange(p1, p2, p3, ncol = 1) # `ncol = 1` for vertical layout, `ncol = 2` for side by side
    } else {
      # Display an empty plot with a message
      plot.new()
      text(0.5, 0.5, " ", cex = 1.5, col = "gray")
    }
  })
  
  ############################################################################## This is the section 3 Download a csv with the wisconet Stations data
  output$download_stations <- downloadHandler(
    # Dynamically generate the filename
    filename = function() {
      paste0("Report_", input$disease_name, "_forecasting_", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      # Fetch the data from your reactive function
      data <- stations_data()  
      data_f <- NULL
      
      # Process data based on the selected disease
      if (input$disease_name == 'tarspot') { 
        data$tarspot_risk <- data$tarspot_risk * 100
        data$risk_class <- if_else(data$tarspot_risk > input$risk_threshold, "High", 
                                   if_else(data$tarspot_risk < 20, "Low", "Moderate"))
        data_f <- data %>% 
          select(forecasting_date, station_id, station_name, region, state, earliest_api_date, 
                 tarspot_risk, risk_class)
      } else if (input$disease_name == 'gls') { 
        data$gls_risk <- data$gls_risk * 100
        data$risk_class <- if_else(data$gls_risk > input$risk_threshold, "High", 
                                   if_else(data$gls_risk < 40, "Low", "Moderate"))
        data_f <- data %>% 
          select(forecasting_date, station_id, station_name, region, state, earliest_api_date, 
                 gls_risk, risk_class)
      } else if (input$disease_name == 'frogeye_leaf_spot') { 
        data$frogeye_risk <- data$frogeye_risk * 100
        data$risk_class <- if_else(data$frogeye_risk > input$risk_threshold, "High", 
                                   if_else(data$frogeye_risk < 40, "Low", "Moderate"))
        data_f <- data %>% 
          select(forecasting_date, station_id, station_name, region, state, earliest_api_date, 
                 frogeye_risk, risk_class)
      }
      
      # Validate if data is available for download
      if (!is.null(data_f) && nrow(data_f) > 0) {
        write.csv(data_f, file, row.names = FALSE)
      } else {
        stop("No data available for download.")
      }
    }
  )
  
  output$download_report <- downloadHandler(
    filename = function() {
      req(shared_data$w_station_id, cancelOutput = TRUE)
      paste0("Report_risktrend_uwmadison_",#input$station_name[1],'_', 
             input$disease_name, Sys.Date(), ".pdf")
    },
    content = function(file) {
      data <- disease_risk_data()
      location_name <- paste0(data$station_name[1], " Station")
      
      if (is.null(data) || nrow(data) == 0) {
        showNotification("No data available to generate the report.", type = "warning")
        stop("No data available to generate the report.")
      }
      
      data_f <- data_transform(data, input)
      
      report_template<-template_pdf(file)
      
      # Prepare report parameters
      report_params <- list(
        location = location_name,
        disease = custom_disease_name(input$disease_name),
        forecast_date = input$forecast_date,
        threshold = input$risk_threshold,
        fungicide = input$no_fungicide,
        growth_stage = input$crop_growth_stage,
        risk_table = data_f
      )
      
      # Render the report
      tryCatch({
        rmarkdown::render(
          input = report_template,
          output_file = file,
          params = report_params,
          envir = new.env(parent = globalenv()) # To avoid any potential environment issues
        )
      }, error = function(e) {
        showNotification(
          paste("Report generation failed:", e$message), 
          type = "error", 
          duration = 10
        )
        stop(e)
      })
    }
  )
}
