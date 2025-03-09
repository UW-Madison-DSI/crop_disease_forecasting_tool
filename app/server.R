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
      #whitemold_nirr_risk * 100,
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
    cutoff_date <- as.Date("2025-02-21")
    
    if (input$forecasting_date < "2025-02-21") {
      result<-historical_data%>% filter(forecasting_date == input$forecasting_date)%>%
               mutate(`Forecasting Date` = forecasting_date)
    } else {
      result<-fetch_forecasting_data(input$forecasting_date)%>%
      #api_data%>%
        mutate(`Forecasting Date` = forecasting_date) %>%
        group_by(station_id) %>%
        filter(forecasting_date == max(forecasting_date)) %>%
        ungroup()
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
        "Whitemold Irrigated (15in) Risk: ", round(punctual_estimate$whitemold_irr_15in_risk * 100, 2), "% | "
        #"Whitemold Dry Risk: ", round(punctual_estimate$whitemold_nirr_risk * 100, 2), "%"
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
      data$tarspot_risk_class <- if_else(data$tarspot_risk>threshold, "High", data$tarspot_risk_class)
    }
    return(data)
  }
  ################################################################## This is the section 1 risk_map
  # Create a reactive value to track if data is loaded
  output$risk_map <- renderLeaflet({
    # Base map setup
    default_map <- leaflet() %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("USGS.USTopo", group = "Topographic") %>% 
      addProviderTiles("Esri.WorldImagery", group = "Esri Imagery") %>%
      setView(lng = -89.75, lat = 44.76, zoom = 7.2) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "CartoDB Positron", "Topographic", "Esri Imagery"),
        options = layersControlOptions(collapsed = TRUE)
      )
    
    
    
    if (input$ibm_data == FALSE) {
      data1 <- forecast_data()
      
      # Transform boundaries and record timing
      county_boundaries <- st_transform(county_boundaries, crs = 4326)
      shared_data$stations_data <- data1
      time_part2 <- Sys.time() - shared_data$start_time
      cat(paste(" -> Time to display the map: ", time_part2, " seconds\n"))
      
      risk_variables <- list(
        'whitemold_irr_30in' = 'whitemold_irr_30in_risk',
        'whitemold_irr_15in' = 'whitemold_irr_15in_risk',
        'whitemold_nirr'     = 'whitemold_nirr_risk'
      )
      
      if (nrow(data1) > 0) {

        pal <- colorFactor(
          palette = c("Low" = "#D55E00", "Moderate" = "#F0E442", "High" = "#009E73"),
          domain = c("Low", "Moderate", "High")
        )
        
        # For tarspot risk
        if (input$disease_name %in% c('tarspot')) {
          data1$ts_color <- pal(data1$tarspot_risk_class)
          map <- leaflet(data1) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -89.75, lat = 44.76, zoom = 7.2) %>%
            addCircleMarkers(
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
              values = c("Low", "Moderate", "High"),
              opacity = 1
            )
        }
        
        # For frogeye leaf spot risk
        if (input$disease_name %in% c('fe')) {
          data1 <- data1 %>% filter(fe_risk_class %in% c("Low", "Moderate", "High"))
          data1$fe_risk_class <- factor(data1$fe_risk_class, levels = c("Low", "Moderate", "High"))
          data1$fe_color <- pal(data1$fe_risk_class)
          
          map <- leaflet(data1) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -89.75, lat = 44.76, zoom = 7.2) %>%
            addCircleMarkers(
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
              values = c("Low", "Moderate", "High"),
              opacity = 1
            )
        }
        
        # For gray leaf spot risk
        if (input$disease_name %in% c('gls')) {
          data1 <- data1 %>% filter(gls_risk_class %in% c("Low", "Moderate", "High"))
          data1$gls_risk_class <- factor(data1$gls_risk_class, levels = c("Low", "Moderate", "High"))
          data1$color <- pal(data1$gls_risk_class)
          
          map <- leaflet(data1) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -89.75, lat = 44.76, zoom = 7.2) %>%
            addCircleMarkers(
              lng = ~longitude,
              lat = ~latitude,
              popup = ~popup_content,
              color = ~color,
              fillColor = ~color,
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
              values = c("Low", "Moderate", "High"),
              opacity = 1
            )
        }
        
        # For other disease names with risk variables
        if (input$disease_name %in% names(risk_variables)) {
          risk_variable <- risk_variables[[input$disease_name]]
          map <- leaflet(data1) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -89.75, lat = 44.76, zoom = 7.2) %>%
            addCircleMarkers(
              lng = ~longitude,
              lat = ~latitude,
              popup = ~popup_content, 
              color = "black",
              fillColor = ~colorNumeric(palette = "YlGnBu", 
                                        domain = data1[[risk_variable]])(data1[[risk_variable]]),
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
              pal = colorNumeric(palette = "YlGnBu", domain = data1[[risk_variable]]),
              values = data1[[risk_variable]],
              title = "Risk (%)",  
              opacity = 1 
            )
        }
        
        # Add county boundaries and additional base layers
        map <- map %>%
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
          addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
          addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
          addProviderTiles("USGS.USTopo", group = "Topographic") %>% 
          addProviderTiles("Esri.WorldImagery", group = "Esri Imagery") %>%
          addLayersControl(
            baseGroups = c("OpenStreetMap", "CartoDB Positron", "Topographic", "Esri Imagery"),
            overlayGroups = c("County Boundaries"),
            options = layersControlOptions(collapsed = TRUE)
          ) %>%
          hideGroup("County Boundaries")
        
        map  # Return the constructed map
      } else {
        # If no data available, return a default map
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = -89.75, lat = 44.76, zoom = 7.2)
      }
    } else {
      # When input$ibm_data is TRUE, return a default map
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -89.75, lat = 44.76, zoom = 7.2)
    }
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
  
  observeEvent(c(input$crop_growth_stage, input$no_fungicide),{  # Include inputs to trigger observation
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
      message("Please select a station by clicking on it in the map from the Disease Forecasting section.")
    })
  })
  
  output$risk_trend <- renderPlot({
    ## Preparación de la data y definición de la ubicación
    data_prepared <- NULL
    location <- NULL
    if (!is.null(shared_data$w_station_id))  {
      data_prepared <- historical_data %>% filter(station_name == shared_data$w_station_id)
      location <- paste0(shared_data$w_station_id, " Station")
    } else if (!is.null(shared_data$ibm_data)) {
      data_prepared <- shared_data$ibm_data
      data_prepared$forecasting_date <- as.Date(data_prepared$forecasting_date, format = '%Y-%m-%d')
      location <- paste0("Lat ", shared_data$latitude, " Lon ", shared_data$longitude)
    }
    
    # Si no hay datos (por ejemplo, mientras se espera la respuesta del API)
    if (is.null(data_prepared) || nrow(data_prepared) == 0) {
      # Se muestra un mapa o gráfico por defecto
      plot.new()
      title("Mapa por defecto")
      text(0.5, 0.5, "Cargando datos o seleccione una estación...", cex = 1.5)
    } else {
      # Si la data está disponible, se prepara y se genera el gráfico
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
      
      # Reestructurar la data a formato long
      data_long <- data_selected %>% 
        pivot_longer(
          cols = c("Tar Spot", "Gray Leaf Spot", "Frog Eye Leaf Spot", "Whitemold Irr (30in)", "Whitemold Irr (15in)", "Whitemold No Irr"),
          names_to = "Disease",
          values_to = "risk_value"
        )
      
      data_long$risk_value <- as.numeric(data_long$risk_value) * 100
      
      df_subset <- data_long %>% filter(Disease %in% selected_diseases)
      
      # Ejemplo para Tar Spot
      if (selected_diseases == 'Tar Spot') {
        ggplot(df_subset, aes(x = forecasting_date, y = risk_value, color = Disease)) +
          geom_line() +
          geom_point() +
          geom_hline(yintercept = 35, linetype = "dashed", color = "green") +
          geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
          annotate("text", x = min(df_subset$forecasting_date), y = 17.5, label = "Low", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 42.5, label = "Moderate", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 57.5, label = "High", vjust = -0.5, hjust = 0, size = 4) +
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
          annotate("text", x = min(df_subset$forecasting_date), y = 17.5, label = "Low", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 42.5, label = "Moderate", vjust = -0.5, hjust = 0, size = 4) +
          annotate("text", x = min(df_subset$forecasting_date), y = 67.5, label = "High", vjust = -0.5, hjust = 0, size = 4) +
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
    if (!is.null(shared_data$w_station_id)) {
      data_prepared <- historical_data %>%
        filter(station_name == shared_data$w_station_id)
      location <- paste0(shared_data$w_station_id, " Station")
    } else if (!is.null(shared_data$ibm_data)) {
      data_prepared <- shared_data$ibm_data
      data_prepared$forecasting_date <- as.Date(data_prepared$forecasting_date, format = '%Y-%m-%d')
      location <- paste0("Lat ", shared_data$latitude, " Lon ", shared_data$longitude)
    }
    
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
    
    # Combine the two plots in a vertical layout.
    # You can also use patchwork (e.g., p1 / p2) if preferred.
    library(gridExtra)
    grid.arrange(p1, p2, ncol = 1)
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
                 gls_risk_class, fe_risk, fe_risk_class, 
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
                                whitemold_irr_30in_risk,whitemold_irr_15in_risk
                                #whitemold_nirr_risk
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
