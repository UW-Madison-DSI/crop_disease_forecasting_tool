options(repos = c(CRAN = "https://cran.rstudio.com/"))
#install.packages("memoise")

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
library(httr2)
library(readr)
library(scales)
library(later)

source("functions/1_wisconet_calls.R")
source("functions/2_external_source.R")

source("functions/3_weather_plots.R") 
source("functions/4_pdf_template.R")
source("functions/6_weather_station.R")
source("functions/7_data_transformations.R")

risk_class_vector <- c("1.Low", "2.Moderate", "3.High",'Inactive')
popup_content_str <- paste0(
  "<strong>Station:</strong> %s<br>",
  "<strong>Location:</strong> %s<br>",
  "<strong>Region:</strong> %s<br>",
  "<strong>Forecasting Date:</strong> %s<br>",
  "<strong><mark>Crop Disease Forecasting in Corn</mark></strong><br>",
  "<strong>Tarspot Risk:</strong> %s<br>",
  "<strong>Frogeye Leaf Spot Risk:</strong> %s<br>",
  "<strong>Gray Leaf Spot Risk:</strong> %s<br>",
  "<strong><mark>Crop Disease Forecasting in Soybean</mark></strong><br>",
  "<strong>Whitemold Non-Irrigated Risk:</strong> %s<br>",
  "<strong>Whitemold Non-Irrigated Risk:</strong> %s<br>",
  "<strong>Whitemold Irrigation (30in) Risk:</strong> %s<br>",
  "<strong>Whitemold Irrigation (15in) Risk:</strong> %s<br>",
  "<strong>Whitemold Irrigation (30in) Risk:</strong> %s<br>",
  "<strong>Whitemold Irrigation (15in) Risk:</strong> %s"
)

historical_data <- get(load("data/historical_data.RData"))


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


######################################################################## SERVER
server <- function(input, output, session) {
  
  # Initialize shared_data with reactive values
  shared_data <- reactiveValues(
    w_station_id = NULL,
    run_model = NULL,
    lat_location = NULL,
    forecasting_date = Sys.Date(),
    lng_location = NULL,
    ibm_data = NULL,
    disease_name = 'tarspot',
    stations_data = NULL, #historical_data%>%filter(forecasting_date=='2025-03-03'),
    this_station_data = NULL,#historical_data%>%filter((forecasting_date=='2025-03-03')
                        #                        & (station_id=='HNCK')),
    start_time = Sys.time(),
    is_loading = FALSE
  )
  
  forecast_data <- reactive({
    # This will re-run only when input$forecasting_date changes.
    req(input$forecasting_date)  # Ensure the input is available

    # Convert input date to Date objects for safe comparison
    cutoff_date <- as.Date("2022-02-21")
    
    if (input$forecasting_date < "2022-02-21") {
      result<-historical_data%>% filter(forecasting_date == input$forecasting_date)%>%
               mutate(`Forecasting Date` = forecasting_date)
    } else {
      result<-fetch_forecasting_data(input$forecasting_date)
      print("<<<<<<------------->>>>>>")
      print(result)

    }

    return(result)
  })
  
  observeEvent(forecast_data(), {
    # Make sure forecast_data() returns data and is not NULL
    data1 <- forecast_data()
    if (!is.null(data1) && nrow(data1) > 0) {
      # Data is ready: remove the loading notification
      removeNotification(id = "loading-notification")
      # Optionally, update shared_data$is_loading to indicate completion
      shared_data$is_loading <- FALSE
    }
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
    #shared_data$run_model <- TRUE
    click <- input$risk_map_click
    getRiskColor <- function(risk) {
      if (risk == "Low") return("green")
      if (risk == "Moderate") return("orange")
      if (risk == "High") return("red")
      return("black")
    }
    
    getRiskIcon <- function(risk_class) {
      icon_html <- switch(
        risk_class,
        "Inactive" = icon("snowflake",  lib = "font-awesome", style = "color:gray; margin-right:4px;"),
        "1.Low"    = icon("leaf",       lib = "font-awesome", style = "color:green; margin-right:4px;"),
        "2.Medium" = icon("circle",     lib = "font-awesome", style = "color:yellow; margin-right:4px;"),
        "3.High"   = icon("circle",     lib = "font-awesome", style = "color:red; margin-right:4px;"),
        # default / unknown:
        icon("circle-question", lib = "font-awesome", style = "color:black; margin-right:4px;")
      )
      as.character(icon_html)  # convert to character for concatenation
    }
    
    
    if (!is.null(click$lng)) {
      shared_data$lng_location <- click$lng
      shared_data$lat_location <- click$lat
      
      punctual_estimate <- ibm_query(input$forecasting_date, click$lat, click$lng)
      shared_data$ibm_data <- punctual_estimate
      
      punctual_estimate <- punctual_estimate %>% filter(forecasting_date == as.Date(input$forecasting_date))
      
      # Display the summary of risks for all diseases
      all_risks_text <- sprintf(
        paste0(
          "Tarspot (Corn) Risk: %s%s | ",
          "Gray Leaf Spot (Corn) Risk: %s%s | ",
          "FrogEye (Soybean) Risk: %s%s | ",
          "Whitemold Irrigated 30in Soybean: %s%s | ",
          "Whitemold Irrigated 15in Soybean: %s%s | ",
          "Whitemold Dry Risk Soybean: %s%s."
        ),
        getRiskIcon(punctual_estimate$tarspot_risk_class), punctual_estimate$tarspot_risk_class,
        getRiskIcon(punctual_estimate$gls_risk_class), punctual_estimate$gls_risk_class,
        getRiskIcon(punctual_estimate$fe_risk_class), punctual_estimate$fe_risk_class,
        getRiskIcon(punctual_estimate$whitemold_irr_30in_class), punctual_estimate$whitemold_irr_30in_class,
        getRiskIcon(punctual_estimate$whitemold_irr_15in_class), punctual_estimate$whitemold_irr_15in_class,
        getRiskIcon(punctual_estimate$whitemold_nirr_risk_class), punctual_estimate$whitemold_nirr_risk_class
      )
      
      # Display the clicked coordinates and risk information
      output$click_coordinates <- renderText({
        paste(
          "Clicked Coordinates: Latitude =", round(click$lat, 4),
          ", Longitude =", round(click$lng, 4), "|",
          " Forecasting Date =", input$forecasting_date, " | \n",
          " Summary of All Diseases Risk:", all_risks_text
        )
      })
    } else {
      showNotification("Please click on the map for a location within the State of Wisconsin, USA, to run the model.", type = "message")
    }
  })
  
  relabeling_class <- function(data, disease, threshold){
    if(disease=='tarspot' && threshold!=.35){
      data$tarspot_risk_class <- if_else(data$tarspot_risk>threshold, "3.High", data$tarspot_risk_class)
    }
    return(data)
  }
  ################################################################## This is the section 1 risk_map
  # Create a reactive value to track if data is loaded
  output$risk_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -89.75, lat = 44.76, zoom = 7.2) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "CartoDB.Positron", "Topographic", "Esri Imagery"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  observeEvent(input$disease_name, {
    shared_data$disease_name <- input$disease_name
  })
  
  observeEvent(input$run_model_wisc, {
    showNotification(
      paste("Loading data for", input$forecasting_date, "..."),
      id='loading',
      type = "message",
      duration = 5
    )
  })
  
  # Make the map responsive to both data changes AND disease selection changes
  observeEvent(list(forecast_data(), input$disease_name), {
    tryCatch({
      
      data1 <- forecast_data() %>%
        mutate(`Forecasting Date` = forecasting_date) %>%
        group_by(station_id) %>%
        filter(forecasting_date == max(forecasting_date)) %>%
        ungroup()
      removeNotification("loading")
      # Debug print
      cat("Data retrieved, rows:", nrow(data1), "\n")
      
      if (input$ibm_data == FALSE) {
        # Check if county_boundaries exists and has data
        has_county_boundaries <- exists("county_boundaries") &&
          !is.null(county_boundaries) &&
          nrow(county_boundaries) > 0
        
        if (has_county_boundaries) {
          cat("County boundaries found, transforming...\n")
          county_boundaries <- st_transform(county_boundaries, crs = 4326)
        } else {
          cat("WARNING: County boundaries not available\n")
        }
        
        shared_data$stations_data <- data1
        time_part2 <- Sys.time() - shared_data$start_time
        cat(paste(" -> Time to display the map: ", time_part2, " seconds\n"))
        
        if (nrow(data1) > 0) {
          
          # Define color palette
          pal <- colorFactor(
            palette = c("1.Low" = "#009E73", "2.Moderate" = "#F0E442", "3.High" = "#D55E00", "Inactive" = "gray"),
            domain = risk_class_vector
          )
          
          # Get base map first
          map_proxy <- leafletProxy("risk_map") %>%
            clearShapes() %>%
            clearMarkers() %>%
            clearControls() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -89.75, lat = 44.76, zoom = 7.2)
          
          # Add base layers (break into separate operations)
          map_proxy <- map_proxy %>%
            addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
            addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
            addProviderTiles("USGS.USTopo", group = "Topographic") %>%
            addProviderTiles("Esri.WorldImagery", group = "Esri Imagery")
          
          # Add county boundaries if available
          if (has_county_boundaries) {
            map_proxy <- map_proxy %>%
              addPolygons(
                data = county_boundaries,
                color = "gray",
                weight = 1,
                opacity = 1,
                fillOpacity = 0,
                fillColor = "lightpink",
                group = "County Boundaries",
                popup = ~NAME
              )
          }
          
          # Add layer controls
          map_proxy <- map_proxy %>%
            addLayersControl(
              baseGroups = c("OpenStreetMap", "CartoDB Positron", "Topographic", "Esri Imagery"),
              overlayGroups = if (has_county_boundaries) c("County Boundaries") else c(),
              options = layersControlOptions(collapsed = TRUE)
            )
          
          if (has_county_boundaries) {
            map_proxy <- map_proxy %>% hideGroup("County Boundaries")
          }
          
          # For tarspot risk
          if (input$disease_name %in% c("tarspot")) {
            filtered_data <- data1 %>% filter(tarspot_risk_class %in% risk_class_vector)
            filtered_data$tarspot_risk_class <- factor(filtered_data$tarspot_risk_class,
                                                       levels = risk_class_vector)
            filtered_data$ts_color <- pal(filtered_data$tarspot_risk_class)

            map_proxy %>%
              addCircleMarkers(
                data = filtered_data,
                lng = ~longitude,
                lat = ~latitude,
                popup = ~popup_content,
                color = ~ts_color,
                fillColor = ~ts_color,
                fillOpacity = 0.8,
                radius = 14,
                weight = 1.5,
                label = ~station_name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px", direction = "auto"
                ),
                layerId = ~station_id
              ) %>%
              addLegend(
                position = "bottomright",
                title = "Tar Spot Risk",
                pal = pal,
                values = risk_class_vector,
                opacity = 1
              )
          }
          
          # For frogeye leaf spot risk
          if (input$disease_name %in% c("fe")) {
            if (!"fe_risk_class" %in% names(data1)) {
              cat("WARNING: fe_risk_class column not found in data\n")
              return()
            }
            
            filtered_data <- data1 %>% filter(fe_risk_class %in% risk_class_vector)
            if (nrow(filtered_data) == 0) {
              cat("WARNING: No valid fe_risk_class data found after filtering\n")
              return()
            }
            
            filtered_data$fe_risk_class <- factor(filtered_data$fe_risk_class,
                                                  levels = risk_class_vector)
            filtered_data$fe_color <- pal(filtered_data$fe_risk_class)
            
            map_proxy %>%
              addCircleMarkers(
                data = filtered_data,
                lng = ~longitude,
                lat = ~latitude,
                popup = ~popup_content,
                color = ~fe_color,
                fillColor = ~fe_color,
                fillOpacity = 0.8,
                radius = 14,
                weight = 1.5,
                label = ~station_name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px", direction = "auto"
                ),
                layerId = ~station_name
              ) %>%
              addLegend(
                position = "bottomright",
                title = "Frog Eye Risk",
                pal = pal,
                values = risk_class_vector,
                opacity = 1
              )
          }
          
          # For whitemold non-irrigated risk (whitemold_nirr_risk)
          if (input$disease_name %in% c("whitemold_nirr")) {
            # Updated check: using the column "whitemold_nirr_risk_class" consistently
            
            filtered_data <- data1 %>% filter(whitemold_nirr_risk_class %in% risk_class_vector)
            if (nrow(filtered_data) == 0) {
              cat("WARNING: No valid whitemold_nirr data found after filtering\n")
              return()
            }
            
            filtered_data$whitemold_nirr_risk_class <- factor(filtered_data$whitemold_nirr_risk_class,
                                                         levels = risk_class_vector)
            filtered_data$whitemold_nirr_color <- pal(filtered_data$whitemold_nirr_risk_class)
            
            map_proxy %>%
              addCircleMarkers(
                data = filtered_data,
                lng = ~longitude,
                lat = ~latitude,
                popup = ~popup_content,
                color = ~whitemold_nirr_color,
                fillColor = ~whitemold_nirr_color,
                fillOpacity = 0.8,
                radius = 14,
                weight = 1.5,
                label = ~station_name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px", direction = "auto"
                ),
                layerId = ~station_id
              ) %>%
              addLegend(
                position = "bottomright",
                title = "Whitemold Dry Risk",
                pal = pal,
                values = risk_class_vector,
                opacity = 1
              )
          }
          
          # For whitemold irrigated risk
          if (input$disease_name %in% c("whitemold_irr_30in","whitemold_irr_15in")) {
            if (input$disease_name %in% c("whitemold_irr_30in")){
              filtered_data <- data1 %>% filter(whitemold_irr_30in_class %in% risk_class_vector)
              if (nrow(filtered_data) == 0) {
                cat("WARNING: No valid whitemold_irr_class data found after filtering\n")
                return()
              }
              
              filtered_data$whitemold_irr_30in_class <- factor(filtered_data$whitemold_irr_30in_class,
                                                          levels = risk_class_vector)
              filtered_data$whitemold_irr_color <- pal(filtered_data$whitemold_irr_30in_class)
            }else{
              filtered_data <- data1 %>% filter(whitemold_irr_15in_class %in% risk_class_vector)
              if (nrow(filtered_data) == 0) {
                cat("WARNING: No valid whitemold_irr_class data found after filtering\n")
                return()
              }
              
              filtered_data$whitemold_irr_15in_class <- factor(filtered_data$whitemold_irr_15in_class,
                                                               levels = risk_class_vector)
              filtered_data$whitemold_irr_color <- pal(filtered_data$whitemold_irr_15in_class)
            }
            map_proxy %>%
              addCircleMarkers(
                data = filtered_data,
                lng = ~longitude,
                lat = ~latitude,
                popup = ~popup_content,
                color = ~whitemold_irr_color,
                fillColor = ~whitemold_irr_color,
                fillOpacity = 0.8,
                radius = 14,
                weight = 1.5,
                label = ~station_name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px", direction = "auto"
                ),
                layerId = ~station_id
              ) %>%
              addLegend(
                position = "bottomright",
                title = "Whitemold Irrigation Risk",
                pal = pal,
                values = risk_class_vector,
                opacity = 1
              )
          }
          
          # For gray leaf spot risk
          if (input$disease_name %in% c("gls")) {
            if (!"gls_risk_class" %in% names(data1)) {
              cat("WARNING: gls_risk_class column not found in data\n")
              return()
            }
            
            filtered_data <- data1 %>% filter(gls_risk_class %in% risk_class_vector)
            if (nrow(filtered_data) == 0) {
              cat("WARNING: No valid gls_risk_class data found after filtering\n")
              return()
            }
            
            filtered_data$gls_risk_class <- factor(filtered_data$gls_risk_class,
                                                   levels = risk_class_vector)
            filtered_data$gls_risk_color <- pal(filtered_data$gls_risk_class)
            
            map_proxy %>%
              addCircleMarkers(
                data = filtered_data,
                lng = ~longitude,
                lat = ~latitude,
                popup = ~popup_content,
                color = ~gls_risk_color,
                fillColor = ~gls_risk_color,
                fillOpacity = 0.8,
                radius = 14,
                weight = 1.5,
                label = ~station_name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px", direction = "auto"
                ),
                layerId = ~station_id
              ) %>%
              addLegend(
                position = "bottomright",
                title = "Gray Leaf Spot",
                pal = pal,
                values = risk_class_vector,
                opacity = 1
              )
          }
        } else {
          # If no data available, return a default map
          leafletProxy("risk_map") %>%
            clearShapes() %>%
            clearMarkers() %>%
            clearControls() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -89.75, lat = 44.76, zoom = 7.2) %>%
            addLayersControl(
              baseGroups = c("OpenStreetMap", "CartoDB.Positron", "Topographic", "Esri Imagery"),
              options = layersControlOptions(collapsed = TRUE)
            )
        }
        #removeNotification("loading_data_notification")
      } else {
        #removeNotification("loading_data_notification")
        # When input$ibm_data is TRUE, return a default map
        map_proxy <- leafletProxy("risk_map") %>%
          addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
          addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
          addProviderTiles("USGS.USTopo", group = "Topographic") %>%
          addProviderTiles("Esri.WorldImagery", group = "Esri Imagery") %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = -89.75, lat = 44.76, zoom = 7.2)
      }
    }, error = function(e) {
      # Log any errors that occur
      cat("ERROR in map rendering:", conditionMessage(e), "\n")
      
      # Provide a clean fallback map when errors occur
      leafletProxy("risk_map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -89.75, lat = 44.76, zoom = 7.2) %>%
        addControl(
          html = paste("<div style='padding: 6px; background-color: #fff; border-radius: 4px;'>",
                       e,"Error loading map data.<br>Please try again or select different parameters.",
                       "</div>"),
          position = "topright"
        )
    })
  })
  
  # Observe click event to center the map on the selected station
  observeEvent(input$risk_map_marker_click, {
    click <- input$risk_map_marker_click
    shared_data$w_station_id<-click$id
    shared_data$w_station_key<-click$id
    print(click)
    
    this_station <- shared_data$stations_data 
    this_station <- this_station%>% filter(station_id == click$id)
    
    shared_data$this_station_data <- this_station
    if (!is.null(click)) {
      # Update the map view to the clicked location
      leafletProxy("risk_map") %>%
        setView(lng = click$lng, lat = click$lat, zoom = 16) %>%
        addProviderTiles("Esri.WorldImagery", group = "Esri Imagery")
    } else {
      warning("No click event detected.")
    }
  })
  
  observeEvent(c(input$crop_growth_stage, input$no_fungicide),{
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
  
  output$station_specifications <- renderText({
    tryCatch({
      if (!is.null(shared_data$ibm_data)) {
        paste("Given location: Lat ", shared_data$lat_location, ", Lon ", shared_data$lng_location)
      } else if (!is.null(shared_data$w_station_id)){
        data <- shared_data$this_station_data

        # Check if data is not empty
        if (nrow(data) > 0) {
          station <- data$station_name[1]
          #earliest_api_date <- data$earliest_api_date[1]
          
          location <- if_else(
            data$location[1] == "Not set",  
            "", 
            paste("situated in", data$location[1], ",", data$region[1], "Region,", data$state[1])
          )
          
          #date_obj <- as.Date(earliest_api_date, format = "%Y-%m-%d")  
          # Format for user-friendly reading
          user_friendly_date <- format(date_obj, "%B %d, %Y")
          
          paste(
            station, "Station,", location, "."
          )
          
        } else if ((is.null(shared_data$w_station_id)) && (is.null(shared_data$ibm_data))) {
          "Please select a station by clicking on it in the map from the Disease Forecasting section."
        }
      }
    }, error = function(e) {
      # Handle error
      message(e, "Please select a station by clicking on it in the map from the Disease Forecasting section.")
    })
  })
  
  output$risk_trend <- renderPlot({
    ## Preparación de la data y definición de la ubicación
    data_prepared <- historical_data %>% filter(station_id == 'ALTN')
    location <- "HNCK Station"
    if (!is.null(shared_data$w_station_id)) {
      print(shared_data$w_station_id)
      data_prepared <- forecast_data() %>% filter(station_id == shared_data$w_station_id)
      #historical_data %>% filter(station_name == shared_data$w_station_id)
      location <- paste0(shared_data$w_station_id, " Station")
      title_txt <- paste0("Risk Trend at ", shared_data$w_station_id)
      data_selected <- data_prepared %>% 
        mutate(forecasting_date = as.Date(date, format = "%Y-%m-%d")) %>% 
        filter(!is.na(tarspot_risk) & (station_id == shared_data$w_station_id)) %>% 
        select(forecasting_date,station_name, tarspot_risk,tarspot_risk_class, 
               gls_risk, gls_risk_class, 
               fe_risk, fe_risk_class, 
               whitemold_irr_30in_risk, whitemold_irr_30in_class, 
               whitemold_irr_15in_risk, whitemold_irr_15in_class, 
               whitemold_nirr_risk, whitemold_nirr_risk_class) %>% 
        rename(
          `Tar Spot` = tarspot_risk,
          `Gray Leaf Spot` = gls_risk,
          `Frog Eye Leaf Spot` = fe_risk,
          `Whitemold Irr (30in)` = whitemold_irr_30in_risk,
          `Whitemold Irr (15in)` = whitemold_irr_15in_risk,
          `Whitemold No Irr` = whitemold_nirr_risk
        )
    }
    
    if (!is.null(shared_data$ibm_data)) {
      data_prepared <- shared_data$ibm_data
      data_prepared$forecasting_date <- as.Date(data_prepared$forecasting_date, format = '%Y-%m-%d')
      location <- paste0("Lat ", shared_data$latitude, " Lon ", shared_data$longitude)
      title_txt <- paste0("Risk Trend at Lat ", shared_data$latitude, " Lon ", shared_data$longitude)
      
      data_selected <- data_prepared %>% 
        mutate(forecasting_date = date)%>%#as.Date(date, format = "%Y-%m-%d") + 1) %>% 
        filter(!is.na(tarspot_risk)) %>% 
        mutate(across(ends_with("_risk"), as.numeric)) %>%
        select(forecasting_date, tarspot_risk, tarspot_risk_class, 
               gls_risk, gls_risk_class, 
               fe_risk, fe_risk_class, 
               whitemold_irr_30in_risk, whitemold_irr_30in_class, 
               whitemold_irr_15in_risk, whitemold_irr_15in_class, 
               whitemold_nirr_risk, whitemold_nirr_risk_class) %>% 
        rename(
          `Tar Spot` = tarspot_risk,
          `Gray Leaf Spot` = gls_risk,
          `Frog Eye Leaf Spot` = fe_risk,
          `Whitemold Irr (30in)` = whitemold_irr_30in_risk,
          `Whitemold Irr (15in)` = whitemold_irr_15in_risk,
          `Whitemold No Irr` = whitemold_nirr_risk
        )
      print("-----------------------")
      print(data_selected)
    }
    
    if (is.null(data_prepared) || nrow(data_prepared) == 0) {
      plot.new()
      #title("Please choose an station from the map first")
      text(0.5, 0.5, "Please choose an station from the map to display the risk and weather trends for such location", cex = 1.5)
    } else {
      selected_diseases <- input$disease
      
      #data_long <- data_selected %>% 
      #  pivot_longer(
      #    cols = c("Tar Spot", "Gray Leaf Spot", "Frog Eye Leaf Spot", "Whitemold Irr (30in)", "Whitemold Irr (15in)", "Whitemold No Irr"),
      #    names_to = "Disease",
      #    values_to = "risk_value"
      #  )
      data_long <- data_selected %>% 
        pivot_longer(
          cols = c("Tar Spot", "Gray Leaf Spot", "Frog Eye Leaf Spot", 
                   "Whitemold Irr (30in)", "Whitemold Irr (15in)", "Whitemold No Irr"),
          names_to = "Disease",
          values_to = "risk_value"
        ) %>%
        pivot_longer(
          cols = c(tarspot_risk_class, 
                   gls_risk_class, 
                   fe_risk_class, 
                   whitemold_irr_30in_class, 
                   whitemold_irr_15in_class, 
                   whitemold_nirr_risk_class),
          names_to = "Risk_Class_Type",
          values_to = "risk_class"
        )
      
      data_long$risk_value <- as.numeric(data_long$risk_value) * 100
      
      df_subset <- data_long %>% filter((Disease %in% selected_diseases))%>%
        # This step is needed to match the risk class with the appropriate disease.
        filter((Disease == "Tar Spot" & Risk_Class_Type == "tarspot_risk_class") |
                 (Disease == "Gray Leaf Spot" & Risk_Class_Type == "gls_risk_class") |
                 (Disease == "Frog Eye Leaf Spot" & Risk_Class_Type == "fe_risk_class") |
               (Disease == "Whitemold Irr (15in)" & Risk_Class_Type == "whitemold_irr_15in_class") |
               (Disease == "Whitemold Irr (30in)" & Risk_Class_Type == "whitemold_irr_30in_class") |
                 (Disease == "Whitemold No Irr" & Risk_Class_Type == "whitemold_nirr_risk_class"))
      
      if (selected_diseases == 'Tar Spot') {
        print(df_subset)
        ggplot(df_subset, aes(x = forecasting_date, y = risk_value, color = Disease)) +
          geom_line(size = 1.5) +
          geom_point() +
          geom_text(aes(label = risk_class), vjust = -1, size = 4) +
          geom_hline(yintercept = 35, linetype = "dashed", color = "green") +
          geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
          annotate("text", x = min(df_subset$forecasting_date), y = 17.5, label = "1.Low", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 42.5, label = "2.Moderate", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 57.5, label = "3.High", vjust = -0.5, hjust = 0, size = 4) +
          labs(
            title = title_txt,
            x = "Forecasting Date",
            y = "Risk (%)",
            color = "Disease"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "bottom"
          )
      } else if (selected_diseases == 'Frog Eye Leaf Spot') {
        ggplot(df_subset, aes(x = forecasting_date, y = risk_value, color = Disease)) +
          geom_line(size = 1.5) +
          geom_point(size = 4) +
          geom_text(aes(label = risk_class), vjust = -1, size = 4) +
          geom_hline(yintercept = 50, linetype = "dashed", color = "green") +
          geom_hline(yintercept = 60, linetype = "dashed", color = "gray50") +
          annotate("text", x = min(df_subset$forecasting_date), y = 37.5, label = "1.Low", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 52.5, label = "2.Moderate", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 67.5, label = "3.High", vjust = -0.5, hjust = 0, size = 4) +
          labs(
            title = title_txt,
            x = "Forecasting Date",
            y = "Risk (%)",
            color = "Disease"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "bottom"
          )
      } else if (selected_diseases == 'Gray Leaf Spot') {
        ggplot(df_subset, aes(x = forecasting_date, y = risk_value, color = Disease)) +
          geom_line(size = 1.5) +
          geom_point(size = 4) +
          geom_text(aes(label = risk_class), vjust = -1, size = 4) +
          geom_hline(yintercept = 39, linetype = "dashed", color = "green") +
          geom_hline(yintercept = 60, linetype = "dashed", color = "gray50") +
          annotate("text", x = min(df_subset$forecasting_date), y = 17.5, label = "1.Low", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 42.5, label = "2.Moderate", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 67.5, label = "3.High", vjust = -0.5, hjust = 0, size = 4) +
          labs(
            title = paste("Risk Trend at ", shared_data$w_station_id),
            x = "Forecasting Date",
            y = "Risk (%)",
            color = "Disease"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "bottom"
          )
      } else if (selected_diseases == 'Whitemold No Irr') {
        ggplot(df_subset, aes(x = forecasting_date, y = risk_value, color = Disease)) +
          geom_line(size = 1.5) +
          geom_point(size = 4) +
          geom_text(aes(label = risk_class), vjust = -1, size = 4) +
          geom_hline(yintercept = 20, linetype = "dashed", color = "green") +
          geom_hline(yintercept = 35, linetype = "dashed", color = "gray50") +
          annotate("text", x = min(df_subset$forecasting_date), y = 17.5, label = "1.Low", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 32.5, label = "2.Moderate", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 47.5, label = "3.High", vjust = -0.5, hjust = 0, size = 4) +
          labs(
            title = paste("Whitemold Dry at", shared_data$w_station_id),
            x = "Forecasting Date",
            y = "Risk (%)",
            color = "Disease"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "bottom"
          )
      }else {
        ggplot(df_subset, aes(x = forecasting_date, y = risk_value, color = Disease)) +
          geom_line(size = 1.5) +
          geom_point(size = 4) +
          geom_text(aes(label = risk_class), vjust = -1, size = 4) +
          geom_hline(yintercept = 5, linetype = "dashed", color = "green") +
          geom_hline(yintercept = 10, linetype = "dashed", color = "gray50") +
          annotate("text", x = min(df_subset$forecasting_date), y = 3.5, label = "1.Low", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 7.5, label = "2.Moderate", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 20.5, label = "3.High", vjust = -0.5, hjust = 0, size = 4) +
          labs(
            title = paste("Whitemold Not Irrigation at ", shared_data$w_station_id),
            x = "Forecasting Date",
            y = "Risk (%)",
            color = "Disease"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "bottom"
          )
      }
    }
  }, width = 800, height = 600)
  
  
  output$weather_trend <- renderPlot({
    # Prepare the data based on shared_data.
    data_prepared <- historical_data %>% filter(station_id == 'ALTN')
    air_temp_data <- NULL
    rh_data <- NULL
    flag = TRUE
    if (!is.null(shared_data$w_station_id)) {
      print(shared_data$w_station_id)
      result <- plot_airtemp_30day(shared_data$w_station_id, input$forecasting_date)
      # View the data
      # head(result$data)
      # Print or render the plot
      print(result$plot)
    } else if (!is.null(shared_data$ibm_data)) {
      data_prepared <- shared_data$ibm_data
      data_prepared$forecasting_date <- as.Date(data_prepared$forecasting_date, format = '%Y-%m-%d')
      location <- paste0("Lat ", shared_data$latitude, " Lon ", shared_data$longitude)
      air_temp_data <- data_prepared %>%
        pivot_longer(
          cols = contains("temperature_"),
          names_to = "variable",
          values_to = "value"
        )
      # Prepare relative humidity data.
      rh_data <- data_prepared %>%
        pivot_longer(
          cols = contains("relativeHumidity_"),
          names_to = "variable",
          values_to = "value"
        )
    
    
      if(!is.null(air_temp_data) && !is.null(rh_data)){
        # Prepare air temperature data.
        p1 <- ggplot(air_temp_data, aes(x = forecasting_date, y = value, color = variable)) +
          geom_line() +
          geom_point() +
          labs(
            title = paste("Air Temperature (°C) Trends at", location),
            x = "Forecasting Date",
            y = "Air Temperature (°C)"
          ) +
          theme_minimal()
        
        p2 <- ggplot(rh_data, aes(x = forecasting_date, y = value, color = variable)) +
          geom_line() +
          geom_point() +
          labs(
            title = paste("Relative Humidity Trends at", location),
            x = "Forecasting Date",
            y = "Relative Humidity (%)"
          ) +
          theme_minimal()
        
        grid.arrange(p1, p2, ncol = 1)
      }else{
        if(flag == TRUE){
          plot.new()
          #title("Please choose an station from the map first")
          text(0.5, 0.5, "Please choose an station from the map to display the risk and weather trends for such location", cex = 1.5)
        }
      }
    }
  }, width = 800, height = 600)
  

  ############################################################################## This is the section 3 Download a csv with the wisconet Stations data
  output$download_stations <- downloadHandler(
    # Dynamically generate the filename
    filename = function() {
      paste0("Report_ag_forecasting_", input$forecasting_date, ".csv")
    },
    
    content = function(file) {
      # Fetch the data from your reactive function
      data_f <- NULL
      if(input$ibm_data==FALSE){
        data_f <- forecast_data()
             
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
  
  ############################################################################## PDF report only for station choice
  output$download_report <- downloadHandler(
    filename = function() {
      req(shared_data$w_station_id, cancelOutput = TRUE)
      paste0("Report_risktrend_uwmadison_",
             output$w_station_id,'_', Sys.Date(), ".pdf")
    },
    content = function(file) {
      #data <- historical_data %>% filter(as.Date(forecasting_date) == as.Date(input$forecasting_date) 
      #                                   & (station_id==shared_data$w_station_id))
      if (!is.null(shared_data$w_station_id)) {
        data <- forecast_data() %>% filter(station_id == shared_data$w_station_id)
        location_name <- paste0(data$station_name[1], " Station")
      }else{
        data <- shared_data$ibm_data
        data$forecasting_date <- as.Date(data$forecasting_date, format = '%Y-%m-%d')
        
      }
      
      if (is.null(data) || nrow(data) == 0) {
        showNotification("No data available to generate the report.", type = "warning")
        stop("No data available to generate the report.")
      }
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("historical_data_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          # Write CSV file without row names
          write.csv(historical_data, file, row.names = FALSE)
        }
      )
      data_f <- historical_data %>% select(forecasting_date,
                                tarspot_risk,tarspot_risk_class,
                                gls_risk,gls_risk_class,
                                fe_risk,fe_risk_class,
                                whitemold_irr_30in_risk,whitemold_irr_15in_risk,
                                whitemold_nirr_risk
                                )
      
      report_template<-template_pdf(file)
      
      # Prepare report parameters
      report_params <- list(
        location = location_name,
        #disease = custom_disease_name(input$disease_name),
        forecasting_date = input$forecasting_date,
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
