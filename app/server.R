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
library(reshape2)

source("functions/1_wisconet_calls.R")
source("functions/2_external_source.R")
source("functions/3_weather_plots.R")
source("functions/4_pdf_template.R")

source("functions/5_auxiliar_functions.R")
source("functions/7_data_transformations.R")
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




########################################################## SETTINGS: WI boundary
server <- function(input, output, session) {

  # Store the clicked coordinates
  shared_data <- reactiveValues(
    w_station_id = NULL,
    run_model = NULL,
    lat_location = NULL,
    lng_location = NULL,
    ibm_data = NULL
  )
  
  # Fetch fresh data directly based on user inputs
  stations_data <- reactive({
    req(input$forecast_date)
    req(input$disease_name)
    req(input$risk_threshold)
    paste0("Response ", nrow(fetch_forecasting_data(as.character(input$forecast_date), 
                                 input$disease_name)))
    return(fetch_forecasting_data(as.character(input$forecast_date), 
                                  input$disease_name)) #1_wisconet_calls.R
    
  })
  
  ############################################################################## IBM data, AOI: Wisconsin
  
  # Add a marker on user click
  observeEvent(input$risk_map_click, {
    click <- input$risk_map_click
    
    if (!is.null(click) && (input$ibm_data == TRUE)) {
      # Gather the coordinates
      shared_data$lat_location <- click$lat
      shared_data$lng_location <- click$lng
      
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
  
  ################################################ Punctual estimates, observe clicks, run model IBM
  observeEvent(input$run_model, {
    # Ensure click has occurred and ibm_data is TRUE
    req(!is.null(input$risk_map_click), input$ibm_data)
    shared_data$run_model<-TRUE
    click <- input$risk_map_click
    if(!is.null(click$lng)){
      shared_data$lng_location <- click$lng
      shared_data$lat_location <- click$lat
      
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
    
      showNotification(paste0(risk_class, " Risk of ", 
                custom_disease_name(input$disease_name),': ', round(risk * 100,2), "%"), 
                type = "message")
      
      # Display the coordinates and risk information
      output$click_coordinates <- renderText({
        paste(
          "Clicked Coordinates: Latitude =", round(click$lat, 4),
          ", Longitude =", round(click$lng, 4), "|", risk_class,
          " Risk of ", custom_disease_name(input$disease_name),':', round(risk * 100,2), "%"
        )
      })
    }else{
      showNotification("Please click on the map for a location within the State of Wisconsin, USA to run the model.", 
                       type = "message")
    }
  })
  
  ################################################################## This is the section 1 risk_map
  output$risk_map <- renderLeaflet({
    if(input$ibm_data==FALSE){
      county_boundaries <- st_transform(county_boundaries, crs = 4326)
      
      data <- stations_data() #ok
      
      data <- data_transform_risk_labels(data, input) #7_data_transformations.R
      print("data  data_transform_risk_labels ----")
      print(data %>% select(earliest_api_date, `Forecasting Date`, station_name,
                            Risk,`Risk Class`))
      
      lowerb<- if_else(input$disease_name=='tarspot',20,40)
      upperb<-input$risk_threshold
      # Create a custom palette function
      custom_palette <- function(x) {
        if (x %in% c("Low",'low')) {
          return("#88CCEE")
        } else if (x  %in% c("Moderate",'moderate')) {
          return("#DDCC77")
        } else if (x  %in% c("High",'high')){
          return("#CC6677")
        } else if (x  %in% c("No Class",'No class')){
          return("gray")
        }
      }
      
      # Apply the custom palette to create a vector of colors
      data$fill_color <- sapply(data$`Risk Class`, custom_palette)
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
            intensity = ~Risk,
            blur = ~Risk,
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
                     paste0("Moderate (", lowerb, " - ", upperb,'%)'), 
                     paste0("High (> ", upperb,'%)')),
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
    print("Click on map to pin a marker on the station ---------")
    print(click)
    shared_data$w_station_id<-click$id
    
    if (!is.null(click)) {
      
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

  
  ############################################################################## This is the section 2 or tab panel Station summary
  disease_risk_data <- reactive({
    if (!is.null(shared_data$w_station_id)) {
      # Define the date range
      given_date <- as.Date(input$forecast_date)
      date_range <- as.list(seq(given_date - 7, given_date, by = "day"))
      brows <- call_forecasting_for_range_of_days(date_range, shared_data$w_station_id, input$disease_name)
      return(brows)
    } else if(!is.null(shared_data$lat_location)){
      data_to_plot_ibm<-shared_data$ibm_data
      print("Data to plot on IBM")
      print(data_to_plot_ibm)
      return(NULL) #shoudl return data_to_plot_ibm
    }else {
      return(NULL)
    }
  })
  
  output$station_specifications <- renderText({
    if (!is.null(shared_data$ibm_data)) {
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
    if (is.null(shared_data$lat_location))  {
      tryCatch({
        data <- disease_risk_data()
        data_prepared <- data_table_for_station_7d(data, input)
        # Plot risk trend
        print("Here in the 7 days risk for a given station ")
        print(data_prepared)
        plot_trend_7days(data_prepared, custom_disease_name(input$disease_name), input$risk_threshold)
      
      }, error = function(e) {
        # Handle error
        message("Error: ", e$message)
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.5, col = "red")
      })
    }else if (!is.null(shared_data$lat_location)) {
      tryCatch({
        data_prepared <- shared_data$ibm_data
        data_prepared$forecasting_date <- as.Date(data_prepared$forecasting_date, format = '%Y-%m-%d')
        
        # Select and rename the relevant columns
        data_selected <- data_prepared %>%
          filter(!is.na(tarspot_risk)) %>%
          select(forecasting_date, tarspot_risk, gls_risk, fe_risk,
                 whitemold_irr_30in_risk,
                 whitemold_irr_15in_risk,
                 whitemold_nirr_risk) %>%
          rename(
            `Tar Spot` = tarspot_risk,
            `Gray Leaf Spot` = gls_risk,
            `Frog Eye Leaf Spot` = fe_risk,
            `Whitemold Irr (30in)` = whitemold_irr_30in_risk,
            `Whitemold Irr (15in)` = whitemold_irr_15in_risk,
            `Whitemold No Irr` = whitemold_nirr_risk
          )
        
        # Reshape the data into long format
        data_long <- data_selected %>%
          pivot_longer(cols = c("Tar Spot", "Gray Leaf Spot", "Frog Eye Leaf Spot",
                        "Whitemold Irr (30in)","Whitemold Irr (15in)","Whitemold No Irr"), 
                       names_to = "risk_type", 
                       values_to = "risk_value")
        data_long$risk_value <- data_long$risk_value*100
        
        # Plot the trend of the specified risk variables over time
        ggplot(data_long, aes(x = forecasting_date, y = risk_value, color = risk_type)) +
          geom_line() +
          geom_point() +
          labs(title = "Risk Trend in the last week for the given location",
               x = "Forecasting Date",
               y = "Risk (%)") +
          scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
          )
        
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, e$message, cex = 1.5, col = "blue")  # Use e$message to capture the error message
      })
    }else{
      plot.new()
      text(0.5, 0.5, "Option 3, check", cex = 1.5, col = "blue")
    }
  })
  
  
  output$air_temperature_plot <- renderPlot({
    print(shared_data$w_station_id)
    
    # Check if lat_location is NULL and fetch weather data
    if (is.null(shared_data$lat_location)) {
      data_airtemp <- api_call_weather_data(shared_data$w_station_id, input$forecast_date, "AIRTEMP", "MIN60", 30)
      data_rh <- api_call_weather_data(shared_data$w_station_id, input$forecast_date, "RELATIVE_HUMIDITY", "MIN60", 14)
      
      if (!is.null(data_airtemp$daily_aggregations) && !is.null(data_rh$daily_aggregations)) {
        p1 <- plot_air_temp(data_airtemp$daily_aggregations)
        p2 <- plot_rh_dp(data_rh$daily_aggregations)
        p3 <- plot_rh_nh_dp(data_rh$daily_aggregations)
        grid.arrange(p1, p2, p3, ncol = 1)
      }
    } else if (!is.null(shared_data$lat_location)) {
      tryCatch({
        data_ibm <- shared_data$ibm_data
        data_ibm$date <- as.Date(data_ibm$date, format = '%Y-%m-%d')
        #data_long_rh <- data_ibm ####### TBD: relativeHumidity_min, relativeHumidity_mean, relativeHumidity_max
        
        # Pivot data longer for temperature types
        data_long_temp <- data_ibm %>%
          select(date, temperature_max, temperature_mean, temperature_min,
                 temperature_max_30ma,temperature_mean_30ma,
                 temperature_min_21ma) %>%
          pivot_longer(cols = c('temperature_max', 'temperature_mean', 'temperature_min',
                                'temperature_max_30ma','temperature_mean_30ma','temperature_min_21ma'), 
                       names_to = "temperature_type", 
                       values_to = "temperature_value")
        
        ggplot(data_long_temp, aes(x = date, y = temperature_value, color = temperature_type)) +
          geom_line(size = 2) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
          labs(
            title = "Air Temperature (°C) Trends in the Last 30 Days",
            x = "Date",
            y = "Air Temperature (°C)",
            color = "Variable"
          ) +
          scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
          )
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, e$message, cex = 1.5, col = "blue")
      })
    } else {
      plot.new()
      text(0.5, 0.5, "Check this case", cex = 1.5, col = "gray")
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
      data_f <- NULL
      if(input$ibm_data==FALSE){
        data_f <- stations_data()
        #data_f <- data_transform_risk_labels(data, input)

      }else if(input$ibm_data==TRUE){
        if (!is.null(shared_data$ibm_data)) {
          # Query IBM data
          data_f <- shared_data$ibm_data
          data_f$lat <- shared_data$lat_location
          data_f$lng <- shared_data$lng_location
        }
      }
      
      # Validate if data is available for download
      if (!is.null(data_f) && nrow(data_f) > 0) {
        write.csv(data_f, file, row.names = FALSE)
      } else {
        stop("No data available for download.")
      }
    }
  )
  
  ##################################### PDF report only for station choice
  output$download_report <- downloadHandler(
    filename = function() {
      req(shared_data$w_station_id, cancelOutput = TRUE)
      paste0("Report_risktrend_uwmadison_",
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
