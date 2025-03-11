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

source("functions/7_data_transformations.R")

risk_class_vector <- c("1.Low", "2.Moderate", "3.High",'Inactive')
popup_content_str <- "<strong>Station:</strong> %s<br><strong>Location:</strong> %s <br><strong>Region:</strong> %s<br><strong>Forecasting Date:</strong> %s<br><strong>Risk Models</strong><br><strong>Tarspot:</strong> %.2f%%<br><strong>Frogeye Leaf Spot:</strong> %.2f%%<br><strong>Gray Leaf Spot:</strong> %.2f%%<br><strong>Whitemold Irrigation (30in):</strong> %.2f%%<br><strong>Whitemold Irrigation (15in):</strong> %.2f%%"

historical_data <- get(load("data/historical_data.RData"))%>%
  mutate(
    forecasting_date = as.Date(date)+1,
    popup_content = sprintf(
      popup_content_str,
      station_name,
      location,
      region,
      forecasting_date,
      tarspot_risk * 100,
      fe_risk * 100,
      gls_risk * 100,
      whitemold_nirr_risk * 100,
      whitemold_irr_30in_risk * 100,
      whitemold_irr_15in_risk * 100
    )
  )


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
    stations_data = historical_data%>%filter(forecasting_date=='2025-03-03'),
    this_station_data = historical_data%>%filter((forecasting_date=='2025-03-03')
                                                & (station_id=='HNCK')),
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
      result<-fetch_forecasting_data(input$forecasting_date)%>%
        mutate(`Forecasting Date` = forecasting_date) %>%
        group_by(station_id) %>%
        filter(forecasting_date == max(forecasting_date)) %>%
        ungroup()
      print(colnames(result))
      print(unique(result$tarspot_risk_class))
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
    
    if (!is.null(click$lng)) {
      shared_data$lng_location <- click$lng
      shared_data$lat_location <- click$lat
      
      punctual_estimate <- ibm_query(input$forecasting_date, click$lat, click$lng)
      shared_data$ibm_data <- punctual_estimate
      
      punctual_estimate <- punctual_estimate %>% filter(forecasting_date == as.Date(input$forecasting_date))
      
      # Display the summary of risks for all diseases
      all_risks_text <- paste(
        "Tarspot (Corn) Risk Class: ", punctual_estimate$tarspot_risk_class, "| ",
        "Gray Leaf Spot (Corn) Risk Class: ", punctual_estimate$gls_risk_class, "| ",
        "FrogEye (Soybean) Risk Class: ", punctual_estimate$fe_risk_class, "| ",
        "Whitemold Irrigated (30in) Risk: ", round(punctual_estimate$whitemold_irr_30in_risk * 100, 2), "% | ",
        "Whitemold Irrigated (15in) Risk: ", round(punctual_estimate$whitemold_irr_15in_risk * 100, 2), "% | ",
        "Whitemold Dry Risk: ", round(punctual_estimate$whitemold_nirr_risk * 100, 2), "%"
      )
      
      # Display the clicked coordinates and risk information
      output$click_coordinates <- renderText({
        paste(
          "Clicked Coordinates: Latitude =", round(click$lat, 4),
          ", Longitude =", round(click$lng, 4), "|", 
          " Summary of All Diseases Risk:", all_risks_text
        )
      })
    } else {
      showNotification("Please click on the map for a location within the State of Wisconsin, USA to run the model.", type = "message")
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
  
  
  # Make the map responsive to both data changes AND disease selection changes
  observeEvent(list(forecast_data(), input$disease_name), {
    showNotification("Loading data")
    tryCatch({
      data1 <- forecast_data()
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
        
        # You had a risk_variables list here; it isn't used so it can be removed or used as needed
        
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
            
            # Debug prints
            print("--------")
            print(unique(filtered_data$tarspot_risk_class))
            print(unique(filtered_data$ts_color))
            
            map_proxy %>%
              addCircleMarkers(
                data = filtered_data,
                lng = ~longitude,
                lat = ~latitude,
                popup = ~popup_content,
                color = ~ts_color,
                fillColor = ~ts_color,
                fillOpacity = 0.8,
                radius = 6,
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
                title = "Predicted Risk (%)",
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
                radius = 6,
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
                title = "Predicted Risk (%)",
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
                radius = 6,
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
                title = "Predicted Risk (%)",
                pal = pal,
                values = risk_class_vector,
                opacity = 1
              )
          }
          
          # For whitemold irrigated risk
          if (input$disease_name %in% c("whitemold_irr_30in", "whitemold_irr_15in")) {
            
            filtered_data <- data1 %>% filter(whitemold_irr_class %in% risk_class_vector)
            if (nrow(filtered_data) == 0) {
              cat("WARNING: No valid whitemold_irr_class data found after filtering\n")
              return()
            }
            
            filtered_data$whitemold_irr_class <- factor(filtered_data$whitemold_irr_class,
                                                        levels = risk_class_vector)
            filtered_data$whitemold_irr_color <- pal(filtered_data$whitemold_irr_class)
            
            map_proxy %>%
              addCircleMarkers(
                data = filtered_data,
                lng = ~longitude,
                lat = ~latitude,
                popup = ~popup_content,
                color = ~whitemold_irr_color,
                fillColor = ~whitemold_irr_color,
                fillOpacity = 0.8,
                radius = 6,
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
                title = "Predicted Risk (%)",
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
                radius = 6,
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
                title = "Predicted Risk (%)",
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
            setView(lng = -89.75, lat = 44.76, zoom = 7.2)
        }
      } else {
        # When input$ibm_data is TRUE, return a default map
        leafletProxy("risk_map") %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
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
                       "Error loading map data.<br>Please try again or select different parameters.",
                       "</div>"),
          position = "topright"
        )
    })
  })
  
  # Observe click event to center the map on the selected station
  observeEvent(input$risk_map_marker_click, {
    click <- input$risk_map_marker_click
    shared_data$w_station_id<-click$id
    print(click)
    
    this_station <- shared_data$stations_data 
    this_station <- this_station%>% filter(station_name == click$id)
    
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
    data_prepared <- historical_data %>% filter(station_name == 'ALTN')
    location <- "HNCK Station"
    if (!is.null(shared_data$w_station_id)) {
      data_prepared <- historical_data %>% filter(station_name == shared_data$w_station_id)
      location <- paste0(shared_data$w_station_id, " Station")
    }
    
    if (!is.null(shared_data$ibm_data)) {
      data_prepared <- shared_data$ibm_data
      data_prepared$forecasting_date <- as.Date(data_prepared$forecasting_date, format = '%Y-%m-%d')
      location <- paste0("Lat ", shared_data$latitude, " Lon ", shared_data$longitude)
    }
    
    if (is.null(data_prepared) || nrow(data_prepared) == 0) {
      plot.new()
      #title("Please choose an station from the map first")
      text(0.5, 0.5, "Please choose an station from the map to display the risk and weather trends for such location", cex = 1.5)
    } else {
      selected_diseases <- input$disease
      
      data_selected <- data_prepared %>% 
        mutate(forecasting_date = as.Date(date, format = "%Y-%m-%d") + 1) %>% 
        filter(!is.na(tarspot_risk)) %>% 
        select(forecasting_date, tarspot_risk, gls_risk, fe_risk, 
               whitemold_irr_30in_risk, whitemold_irr_15in_risk, whitemold_nirr_risk) %>% 
        rename(
          `Tar Spot` = tarspot_risk,
          `Gray Leaf Spot` = gls_risk,
          `Frog Eye Leaf Spot` = fe_risk,
          `Whitemold Irr (30in)` = whitemold_irr_30in_risk,
          `Whitemold Irr (15in)` = whitemold_irr_15in_risk,
          `Whitemold No Irr` = whitemold_nirr_risk
        )
      
      data_long <- data_selected %>% 
        pivot_longer(
          cols = c("Tar Spot", "Gray Leaf Spot", "Frog Eye Leaf Spot", "Whitemold Irr (30in)", "Whitemold Irr (15in)", "Whitemold No Irr"),
          names_to = "Disease",
          values_to = "risk_value"
        )
      
      data_long$risk_value <- as.numeric(data_long$risk_value) * 100
      
      df_subset <- data_long %>% filter(Disease %in% selected_diseases)
      
      if (selected_diseases == 'Tar Spot') {
        ggplot(df_subset, aes(x = forecasting_date, y = risk_value, color = Disease)) +
          geom_line() +
          geom_point() +
          geom_hline(yintercept = 35, linetype = "dashed", color = "green") +
          geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
          annotate("text", x = min(df_subset$forecasting_date), y = 17.5, label = "1.Low", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 42.5, label = "2.Moderate", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 57.5, label = "3.High", vjust = -0.5, hjust = 0, size = 4) +
          labs(
            title = paste("Risk Trend at", shared_data$w_station_id, "Station"),
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
          geom_line() +
          geom_point() +
          geom_hline(yintercept = 39, linetype = "dashed", color = "green") +
          geom_hline(yintercept = 60, linetype = "dashed", color = "gray50") +
          annotate("text", x = min(df_subset$forecasting_date), y = 17.5, label = "1.Low", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 42.5, label = "2.Moderate", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 67.5, label = "3.High", vjust = -0.5, hjust = 0, size = 4) +
          labs(
            title = paste("Risk Trend at", shared_data$w_station_id, "Station"),
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
      } else {
        ggplot(df_subset, aes(x = forecasting_date, y = risk_value, color = Disease)) +
          geom_line() +
          geom_point() +
          labs(
            title = paste("Risk Trend at", shared_data$w_station_id, "Station"),
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
    data_prepared <- historical_data %>% filter(station_name == 'ALTN')
    if (!is.null(shared_data$w_station_id)) {
      data_prepared <- historical_data %>%
        filter(station_name == shared_data$w_station_id)
      location <- paste0(shared_data$w_station_id, " Station")
    } else if (!is.null(shared_data$ibm_data)) {
      data_prepared <- shared_data$ibm_data
      data_prepared$forecasting_date <- as.Date(data_prepared$forecasting_date, format = '%Y-%m-%d')
      location <- paste0("Lat ", shared_data$latitude, " Lon ", shared_data$longitude)
    }
    
    if(!is.null(data_prepared)){
      # Prepare air temperature data.
      air_temp_data <- data_prepared %>%
        pivot_longer(
          cols = c(contains("air_temp"), -ends_with("_f")),
          names_to = "variable",
          values_to = "value"
        )
      
      p1 <- ggplot(air_temp_data, aes(x = forecasting_date, y = value, color = variable)) +
        geom_line() +
        geom_point() +
        labs(
          title = paste("Air Temperature (°C) Trends at", location),
          x = "Forecasting Date",
          y = "Air Temperature (°C)"
        ) +
        theme_minimal()
      
      # Prepare relative humidity data.
      rh_data <- data_prepared %>%
        pivot_longer(
          cols = contains("rh_max"),
          names_to = "variable",
          values_to = "value"
        )
      
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
      plot.new()
      #title("Please choose an station from the map first")
      text(0.5, 0.5, "Please choose an station from the map to display the risk and weather trends for such location", cex = 1.5)
    }
  }, width = 800, height = 600)
  

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
        data_f <- shared_data$stations_data 
        data_f <- data_f %>%
          select(date, forecasting_date, location, station_name, 
                 city, county, earliest_api_date, latitude, longitude, region, state, 
                 station_timezone, tarspot_risk, tarspot_risk_class, gls_risk, 
                 gls_risk_class, fe_risk, fe_risk_class, whitemold_nirr_risk,
                 whitemold_irr_30in_risk, whitemold_irr_15in_risk) %>%
          mutate(across(ends_with("_risk"), ~ . * 100))      
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
      data <- historical_data %>% filter(as.Date(forecasting_date) == as.Date(input$forecasting_date) 
                                         & (station_id==shared_data$w_station_id))

      location_name <- paste0(data$station_name[1], " Station")
      
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
