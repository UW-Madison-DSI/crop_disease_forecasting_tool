options(repos = c(CRAN = "https://cran.rstudio.com/"))
library(shinyWidgets)
library(shiny)
library(shinythemes)
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

# Source functions (make sure these files exist and are error-free)
tryCatch({
  source("functions/1_wisconet_calls1.R")
  source("functions/2_external_source.R")
  source("functions/3_weather_plots.R") 
  #source("functions/4_pdf_template.R")
  source("functions/7_data_transformations.R")
}, error = function(e) {
  cat("Warning: Could not source one or more function files:", e$message, "\n")
})

# GLOBAL VARIABLES AND SETTINGS
risk_class_vector <- c("1.Low", "2.Moderate", "3.High", 'Inactive')

popup_content_str <- paste0(
  "<strong>Station:</strong> <mark>%s</mark><br>",
  "<strong>Region:</strong> %s<br>",
  "<strong><mark>Corn Crop Disease Forecasting</mark></strong><br>",
  "<strong>Tarspot Risk:</strong> %.2f%%<br>",
  "<strong>Frogeye Leaf Spot Risk:</strong> %.2f%%<br>",
  "<strong>Gray Leaf Spot Risk:</strong> %.2f%%<br>",
  "<strong><mark>Soybean Crop Disease Forecasting</mark></strong><br>",
  "<strong>Whitemold Non-Irrigated Risk:</strong> %.2f%%<br>",
  "<strong>Whitemold Irrigation (30in) Risk:</strong> %.2f%%<br>",
  "<strong>Whitemold Irrigation (15in) Risk:</strong> %.2f%%"
)

# FUNCTIONS
getStationIcon <- function(risk_class) {
  icon_color <- switch(
    as.character(risk_class),
    "1.Low" = "green",
    "2.Moderate" = "orange",
    "3.High" = "red",
    "Inactive" = "gray",
    "black" # default
  )
  
  leaflet::makeIcon(
    iconUrl = sprintf("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-%s.png", icon_color),
    iconWidth = 25, 
    iconHeight = 41,
    iconAnchorX = 12, 
    iconAnchorY = 41,
    shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
    shadowWidth = 41, 
    shadowHeight = 41,
    shadowAnchorX = 12, 
    shadowAnchorY = 41
  )
}

token <- Sys.getenv('MAPBOX')

create_custom_icon <- function(color, symbol = "●") {
  makeIcon(
    iconUrl = paste0("data:image/svg+xml;charset=UTF-8,",
                     URLencode(paste0('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24">
                                      <circle cx="12" cy="12" r="10" fill="', color, '" stroke="white" stroke-width="2"/>
                                      <text x="12" y="16" text-anchor="middle" fill="white" font-size="12" font-weight="bold">', symbol, '</text>
                                      </svg>'))),
    iconWidth = 24, iconHeight = 24,
    iconAnchorX = 12, iconAnchorY = 12
  )
}

# LOAD DATA WITH ERROR HANDLING
historical_data <- tryCatch({
  data <- get(load("data/historical_data.RData"))
  data %>%
    mutate(
      forecasting_date = as.Date(date) + 1,
      popup_content = sprintf(
        popup_content_str,
        station_name,
        region,
        tarspot_risk * 100,
        fe_risk * 100,
        gls_risk * 100,
        whitemold_nirr_risk * 100,
        whitemold_irr_30in_risk * 100,
        whitemold_irr_15in_risk * 100
      )
    )
}, error = function(e) {
  cat("Error loading historical data:", e$message, "\n")
  # Return empty data frame with required columns
  data.frame(
    station_name = character(0),
    forecasting_date = as.Date(character(0)),
    stringsAsFactors = FALSE
  )
})

reverse_mapbox <- function(lat, lon, token, types = c("place", "region", "country")) {
  if (is.null(token) || token == "") {
    return("Location unavailable")
  }
  
  tryCatch({
    url <- sprintf(
      "https://api.mapbox.com/geocoding/v5/mapbox.places/%s,%s.json",
      lon, lat
    )
    resp <- GET(url, query = list(
      access_token = token,
      types = paste(types, collapse = ","),
      limit = 1
    ))
    
    if (status_code(resp) != 200) {
      return("Location unavailable")
    }
    
    dat <- content(resp, as = "text", encoding = "UTF-8")
    js <- fromJSON(dat, simplifyVector = FALSE)
    
    if (length(js$features) && nzchar(js$features[[1]]$place_name)) {
      return(js$features[[1]]$place_name)
    } else {
      return("Location unavailable")
    }
  }, error = function(e) {
    cat("Reverse geocoding error:", e$message, "\n")
    return("Location unavailable")
  })
}

# GEOGRAPHIC BOUNDARIES WITH ERROR HANDLING
county_boundaries <- tryCatch({
  counties(state = "WI", cb = TRUE, class = "sf") %>% 
    st_transform(crs = 4326)
}, error = function(e) {
  cat("Error loading county boundaries:", e$message, "\n")
  NULL
})

wi_boundary <- tryCatch({
  states(cb = TRUE) %>%
    filter(NAME == "Wisconsin") %>%
    st_transform(4326)
}, error = function(e) {
  cat("Error loading Wisconsin boundary:", e$message, "\n")
  NULL
})

wisconsin_bbox <- list(
  lat_min = 42.4919,
  lat_max = 47.3025,
  lng_min = -92.8894,
  lng_max = -86.2495
)



######################################################################## SERVER
server <- function(input, output, session) {
  
  log_file <- "usage_log.csv"
  ip_addr <- session$request$REMOTE_ADDR
  
  # if it doesn't exist yet, create it with column names
  if (!file.exists(log_file)) {
    write.table(
      data.frame(
        time  = character(),
        event = character(),
        value = character()
      ),
      file      = log_file,
      sep       = ",",
      row.names = FALSE,
      col.names = TRUE
    )
  }
  
  observeEvent(input$disease_name, {
    shared_data$disease_name <- input$disease_name
    
    entry <- data.frame(
      time  = as.character(Sys.time()),
      event = "disease_selected",
      value = input$disease_name,
      stringsAsFactors = FALSE
    )
    write.table(
      entry,
      file      = log_file,
      sep       = ",",
      row.names = FALSE,
      col.names = FALSE,
      append    = TRUE
    )
  })
  
  write.table(
    data.frame(
      time  = as.character(Sys.time()),
      event = "session_start",
      value = session$token,
      stringsAsFactors = FALSE
    ),
    file      = log_file,
    sep       = ",",
    row.names = FALSE,
    col.names = FALSE,
    append    = TRUE
  )
  
  write.table(
    data.frame(
      time  = as.character(Sys.time()),
      event = "user",
      value = ip_addr,
      stringsAsFactors = FALSE
    ),
    file      = log_file,
    sep       = ",",
    row.names = FALSE,
    col.names = FALSE,
    append    = TRUE
  )
  
  session$onSessionEnded(function() {
    write.table(
      data.frame(
        time  = as.character(Sys.time()),
        event = "session_end",
        value = session$token,
        stringsAsFactors = FALSE
      ),
      file      = log_file,
      sep       = ",",
      row.names = FALSE,
      col.names = FALSE,
      append    = TRUE
    )
  })
  
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
                                                 & (station_id=='Marshfield')),
    start_time = Sys.time(),
    is_loading = FALSE
  )
  
  forecast_data <- reactive({
    # This will re-run only when input$forecasting_date changes.
    req(input$forecasting_date)  # Ensure the input is available
    
    # show a permanent notification
    showNotification(
      "Loading data…",
      id       = "loading-notice",
      duration = NULL,         # stay until removed
      type     = "default"
    )
    
    on.exit({
      # as soon as reactive finishes (success or error), remove it
      removeNotification("loading-notice")
    }, add = TRUE)
    
    # Convert input date to Date objects for safe comparison
    cutoff_date <- as.Date("2025-05-23")
    
    if (input$forecasting_date < "2022-02-21") {
      result<-historical_data%>% filter(forecasting_date == input$forecasting_date)%>%
        mutate(`Forecasting Date` = forecasting_date)
    } else {
      result <- fetch_forecasting_data(input$forecasting_date, 7)
      
      if (is.null(result)){
        result<-historical_data%>% filter(forecasting_date == input$forecasting_date)%>%
          mutate(`Forecasting Date` = forecasting_date)
      }
      #result<-fetch_forecasting_data_uncached(input$forecasting_date)
      print(result)
    }
    
    print(result)
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
        
        popup_txt <- paste0(
          #"<strong>Address:</strong> ", addr, "<br/>",
          "<strong>Latitude:</strong> ", round(click$lat, 4), "<br/>",
          "<strong>Longitude:</strong> ", round(click$lng, 4)
        )
        
        # Add a marker at the clicked location
        leafletProxy("risk_map") %>%
          clearMarkers() %>%
          clearShapes() %>% # Clear existing shapes to avoid overlaps
          addMarkers(
            lng = click$lng,
            lat = click$lat,
            popup = popup_txt
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
    
    entry <- data.frame(
      time  = as.character(Sys.time()),
      event = "ibm_source",
      value = input$run_model,
      stringsAsFactors = FALSE
    )
    write.table(
      entry,
      file      = log_file,
      sep       = ",",
      row.names = FALSE,
      col.names = FALSE,
      append    = TRUE
    )
    
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
          "Tarspot (🌽 ) Risk: %s%s | ",
          "Gray Leaf Spot (🌽 ) Risk: %s%s | ",
          "FrogEye (🫘 Soybean) Risk: %s%s | ",
          "Whitemold Irrigated 30in (🫘 Soybean): %s%s | ",
          "Whitemold Irrigated 15in (🫘 Soybean): %s%s | ",
          "Whitemold Dry Risk (🫘 Soybean): %s%s."
        ),
        getRiskIcon(punctual_estimate$tarspot_risk_class), punctual_estimate$tarspot_risk_class,
        getRiskIcon(punctual_estimate$gls_risk_class), punctual_estimate$gls_risk_class,
        getRiskIcon(punctual_estimate$fe_risk_class), punctual_estimate$fe_risk_class,
        getRiskIcon(punctual_estimate$whitemold_irr_30in_class), punctual_estimate$whitemold_irr_30in_class,
        getRiskIcon(punctual_estimate$whitemold_irr_15in_class), punctual_estimate$whitemold_irr_15in_class,
        getRiskIcon(punctual_estimate$whitemold_nirr_risk_class), punctual_estimate$whitemold_nirr_risk_class
      )
      addr <- tryCatch(
        {
          reverse_mapbox(click$lat, click$lng, token)
        },
        error = function(err) {
          # Log the error if you like
          message("Reverse geocode failed: ", err$message)
          # Fallback address
          "Unknown location"
        }
      )
      
      # Display the clicked coordinates and risk information
      output$click_coordinates <- renderText({
        paste(
          "Clicked Coordinates: Latitude =", round(click$lat, 4),
          ", Longitude =", round(click$lng, 4), "(",addr, ") |",
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
  #output$risk_map <- renderLeaflet({
  #  leaflet() %>%
  #    addProviderTiles("CartoDB.Positron") %>%
  #    setView(lng = -89.75, lat = 44.76, zoom = 7.2) %>%
  #    addLayersControl(
  #      baseGroups = c("OpenStreetMap", "CartoDB.Positron", "Topographic", "Esri Imagery"),
  #      options = layersControlOptions(collapsed = TRUE)
  #    )
  #})
  output$risk_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -89.75, lat = 44.76, zoom = 7.2) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap","CartoDB.Positron","Topographic","Esri Imagery"),
        options    = layersControlOptions(collapsed = TRUE)
      ) %>%
      addEasyButton(
        easyButton(
          icon    = "fa-globe",       # the Font-Awesome "globe" icon
          title   = "Reset zoom",
          onClick = JS("
          function(btn, map){
            map.setView([44.76, -89.75], 7.2);
          }
        ")
        )
      )
  })
  
  
  
  observe({
    # If forecast_data() is NULL => show the notification, else remove it
    if (is.null(forecast_data())) {
      showNotification(
        ui = "ok...",
        id = "loading_data_notification",   # A unique ID
        type = "message",
        duration = NULL                    # Stay visible until removed
      )
    } else {
      removeNotification("loading_data_notification")
    }
  })
  
  # Make the map responsive to both data changes AND disease selection changes
  observeEvent(list(forecast_data(), input$disease_name,input$ibm_data), {
    tryCatch({
      # somewhere in your server, e.g. inside an observeEvent or onLoad
      showNotification(
        "Please choose a weather station by clicking a circle on the map.",
        type        = "message",             # uses Bootstrap “info” by default…
        duration    = 5,                  # stay until dismissed
        closeButton = TRUE
      )
      
      
      data1 <- forecast_data()
      #mutate(`Forecasting Date` = forecasting_date) %>%
      print(data1)
      data1<-data1%>%group_by(station_id) %>%
        filter(forecasting_date == max(forecasting_date)) %>%
        ungroup()
      
      print(data1)
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
            
            # Keep your original working circle markers
            map_proxy %>% 
              addCircleMarkers(
                data = filtered_data,
                lng = ~longitude, 
                lat = ~latitude,
                popup = ~popup_content,  # This works as before
                stroke = TRUE,
                color = ~ts_color,
                fillColor = ~ts_color,
                fillOpacity = 0.8,
                radius = 12,
                weight = 1.5,
                label = ~station_name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto"
                ),
                layerId = ~station_name
              ) %>%
              addLegend(
                position = "bottomright",
                title = "Tar Spot Risk",
                pal = pal,
                values = risk_class_vector,
                opacity = 1
              ) %>%
              # Add small icon markers on top (no popup to avoid conflict)
              addMarkers(
                data = filtered_data,
                lng = ~longitude, 
                lat = ~latitude,
                icon = makeIcon(
                  iconUrl = paste0("data:image/svg+xml;charset=UTF-8,",
                                   URLencode('<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16">
                                 <text x="8" y="12" text-anchor="middle" fill="white" font-size="12" stroke="black" stroke-width="0.5">🗼</text>
                                 </svg>')),
                  iconWidth = 16, iconHeight = 16,
                  iconAnchorX = 8, iconAnchorY = 8
                ),
                layerId = ~paste0(station_name, "_icon"),
                options = markerOptions(interactive = FALSE)  # Makes icon non-interactive
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
                popup = ~popup_content,  # This works as before
                stroke = TRUE,
                color = ~fe_color,
                fillColor = ~fe_color,
                fillOpacity = 0.8,
                radius = 12,
                weight = 1.5,
                label = ~station_name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto"
                ),
                layerId = ~station_name
              ) %>%
              addLegend(
                position = "bottomright",
                title = "Frog Eye Leaf Spot Risk",
                pal = pal,
                values = risk_class_vector,
                opacity = 1
              )%>%
              # Add small icon markers on top (no popup to avoid conflict)
              addMarkers(
                data = filtered_data,
                lng = ~longitude, 
                lat = ~latitude,
                icon = makeIcon(
                  iconUrl = paste0("data:image/svg+xml;charset=UTF-8,",
                                   URLencode('<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16">
                                 <text x="8" y="12" text-anchor="middle" fill="white" font-size="12" stroke="black" stroke-width="0.5">🗼</text>
                                 </svg>')),
                  iconWidth = 16, iconHeight = 16,
                  iconAnchorX = 8, iconAnchorY = 8
                ),
                layerId = ~paste0(station_name, "_icon"),
                options = markerOptions(interactive = FALSE)  # Makes icon non-interactive
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
                popup = ~popup_content,  # This works as before
                stroke = TRUE,
                color = ~whitemold_nirr_color,
                fillColor = ~whitemold_nirr_color,
                fillOpacity = 0.8,
                radius = 12,
                weight = 1.5,
                label = ~station_name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto"
                ),
                layerId = ~station_name
              ) %>%
              addLegend(
                position = "bottomright",
                title = "Whitemold no irrigation Risk",
                pal = pal,
                values = risk_class_vector,
                opacity = 1
              )%>%
              # Add small icon markers on top (no popup to avoid conflict)
              addMarkers(
                data = filtered_data,
                lng = ~longitude, 
                lat = ~latitude,
                icon = makeIcon(
                  iconUrl = paste0("data:image/svg+xml;charset=UTF-8,",
                                   URLencode('<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16">
                                 <text x="8" y="12" text-anchor="middle" fill="white" font-size="12" stroke="black" stroke-width="0.5">🗼</text>
                                 </svg>')),
                  iconWidth = 16, iconHeight = 16,
                  iconAnchorX = 8, iconAnchorY = 8
                ),
                layerId = ~paste0(station_name, "_icon"),
                options = markerOptions(interactive = FALSE)  # Makes icon non-interactive
              )
          }
          
          # For whitemold irrigated risk
          if (input$disease_name %in% c("whitemold_irr_30in")) {
            
            filtered_data <- data1 %>% filter(whitemold_irr_30in_class %in% risk_class_vector)
            if (nrow(filtered_data) == 0) {
              cat("WARNING: No valid whitemold_irr_class data found after filtering\n")
              return()
            }
            
            filtered_data$whitemold_irr_30in_class <- factor(filtered_data$whitemold_irr_30in_class,
                                                             levels = risk_class_vector)
            filtered_data$whitemold_irr_30in_class_color <- pal(filtered_data$whitemold_irr_30in_class)
            
            map_proxy %>%
              addCircleMarkers(
                data = filtered_data,
                lng = ~longitude, 
                lat = ~latitude,
                popup = ~popup_content,  # This works as before
                stroke = TRUE,
                color = ~whitemold_irr_30in_class_color,
                fillColor = ~whitemold_irr_30in_class_color,
                fillOpacity = 0.8,
                radius = 12,
                weight = 1.5,
                label = ~station_name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto"
                ),
                layerId = ~station_name
              )%>%
              addLegend(
                position = "bottomright",
                title = "Whitemold irrigation 30in Risk",
                pal = pal,
                values = risk_class_vector,
                opacity = 1
              )%>%
              # Add small icon markers on top (no popup to avoid conflict)
              addMarkers(
                data = filtered_data,
                lng = ~longitude, 
                lat = ~latitude,
                icon = makeIcon(
                  iconUrl = paste0("data:image/svg+xml;charset=UTF-8,",
                                   URLencode('<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16">
                                 <text x="8" y="12" text-anchor="middle" fill="white" font-size="12" stroke="black" stroke-width="0.5">🗼</text>
                                 </svg>')),
                  iconWidth = 16, iconHeight = 16,
                  iconAnchorX = 8, iconAnchorY = 8
                ),
                layerId = ~paste0(station_name, "_icon"),
                options = markerOptions(interactive = FALSE)  # Makes icon non-interactive
              )
          }
          
          # For whitemold irrigated risk
          if (input$disease_name %in% c("whitemold_irr_15in")) {
            
            filtered_data <- data1 %>% filter(whitemold_irr_15in_class %in% risk_class_vector)
            if (nrow(filtered_data) == 0) {
              cat("WARNING: No valid whitemold_irr_class data found after filtering\n")
              return()
            }
            
            filtered_data$whitemold_irr_15in_class <- factor(filtered_data$whitemold_irr_15in_class,
                                                             levels = risk_class_vector)
            filtered_data$whitemold_irr_15in_class_color <- pal(filtered_data$whitemold_irr_15in_class)
            
            map_proxy %>%
              addCircleMarkers(
                data = filtered_data,
                lng = ~longitude, 
                lat = ~latitude,
                popup = ~popup_content,  # This works as before
                stroke = TRUE,
                color = ~whitemold_irr_15in_class_color,
                fillColor = ~whitemold_irr_15in_class_color,
                fillOpacity = 0.8,
                radius = 12,
                weight = 1.5,
                label = ~station_name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto"
                ),
                layerId = ~station_name
              )%>%
              addLegend(
                position = "bottomright",
                title = "Whitemold irrigation 15in Risk",
                pal = pal,
                values = risk_class_vector,
                opacity = 1
              )%>%
              # Add small icon markers on top (no popup to avoid conflict)
              addMarkers(
                data = filtered_data,
                lng = ~longitude, 
                lat = ~latitude,
                icon = makeIcon(
                  iconUrl = paste0("data:image/svg+xml;charset=UTF-8,",
                                   URLencode('<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16">
                                 <text x="8" y="12" text-anchor="middle" fill="white" font-size="12" stroke="black" stroke-width="0.5">🗼</text>
                                 </svg>')),
                  iconWidth = 16, iconHeight = 16,
                  iconAnchorX = 8, iconAnchorY = 8
                ),
                layerId = ~paste0(station_name, "_icon"),
                options = markerOptions(interactive = FALSE)  # Makes icon non-interactive
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
                popup = ~popup_content,  # This works as before
                stroke = TRUE,
                color = ~gls_risk_color,
                fillColor = ~gls_risk_color,
                fillOpacity = 0.8,
                radius = 12,
                weight = 1.5,
                label = ~station_name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto"
                ),
                layerId = ~station_name
              )%>%
              addLegend(
                position = "bottomright",
                title = "Gray Leaf Spot Risk",
                pal = pal,
                values = risk_class_vector,
                opacity = 1
              )%>%
              # Add small icon markers on top (no popup to avoid conflict)
              addMarkers(
                data = filtered_data,
                lng = ~longitude, 
                lat = ~latitude,
                icon = makeIcon(
                  iconUrl = paste0("data:image/svg+xml;charset=UTF-8,",
                                   URLencode('<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16">
                                 <text x="8" y="12" text-anchor="middle" fill="white" font-size="12" stroke="black" stroke-width="0.5">🗼</text>
                                 </svg>')),
                  iconWidth = 16, iconHeight = 16,
                  iconAnchorX = 8, iconAnchorY = 8
                ),
                layerId = ~paste0(station_name, "_icon"),
                options = markerOptions(interactive = FALSE)  # Makes icon non-interactive
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
  
  observeEvent(input$ibm_data, {
    proxy <- leafletProxy("risk_map")
    
    if (input$ibm_data) {
      proxy %>%
        clearMarkers() %>%      # clears *all* marker layers
        clearControls() %>%
        clearGroup("polygons") %>% 
        # remove all station circles
        clearGroup("station_circles") %>%
        # remove any selected‐station highlight
        clearGroup("selected_station") %>%
        # show the draggable pin
        showGroup("center_pin")
      
      showNotification(
        "You can drag the marker to pick a custom location, then click Run Model.",
        type     = "message",
        duration = 5,
        id       = "pin_reminder"
      )
    } else {
      proxy %>%
        # hide the pin
        hideGroup("center_pin") %>%
        # (optionally) re‐show station circles if you want
        showGroup("station_circles")
      
      removeNotification("pin_reminder")
    }
  })
  
  
  
  # Observe click event to center the map on the selected station
  observeEvent(input$risk_map_marker_click, {
    click <- input$risk_map_marker_click
    req(click)
    shared_data$w_station_id<-click$id
    shared_data$w_station_key<-click$id
    
    this_station <- shared_data$stations_data%>% filter(station_id == click$id)
    shared_data$this_station_data <- this_station
    
    entry <- data.frame(
      time  = as.character(Sys.time()),
      event = "station_clicked",
      value = click$id,
      stringsAsFactors = FALSE
    )
    write.table(
      entry,
      file      = log_file,
      sep       = ",",
      row.names = FALSE,
      col.names = FALSE,
      append    = TRUE
    )
    
    # Zoom to station
    leafletProxy("risk_map") %>% 
      setView(lng = click$lng, lat = click$lat, zoom = 16) %>% 
      
      # remove any old highlight
      clearGroup("selected_station") %>%  
      addCircles(
        lng       = click$lng,
        lat       = click$lat,
        radius    = 500,             # in meters; adjust to taste
        color     = "black",
        weight    = 4,
        fill      = FALSE,
        group     = "selected_station"
      )
  })
  
  #observeEvent(input$risk_map_marker_click, {
  #  click <- input$risk_map_marker_click
  #  shared_data$w_station_id<-click$id
  #  shared_data$w_station_key<-click$id
  #  print(click)
  
  #  this_station <- shared_data$stations_data%>% filter(station_id == click$id)
  #  shared_data$this_station_data <- this_station
  #  if (!is.null(click)) {
  #    # Update the map view to the clicked location
  #    leafletProxy("risk_map") %>%
  #      setView(lng = click$lng, lat = click$lat, zoom = 16) %>%
  #     addProviderTiles("Esri.WorldImagery", group = "Esri Imagery")
  #  } else {
  #    warning("No click event detected.")
  #  }
  #})
  
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
        addr <- tryCatch(
          {
            reverse_mapbox(shared_data$lat_location, shared_data$lng_location, token)
          },
          error = function(err) {
            # Log the error if you like
            message("Reverse geocode failed: ", err$message)
            # Fallback address
            "Unknown location"
          }
        )
        paste("Custom Location 📍", addr)
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
    # Validate and prepare data
    req(shared_data)
    
    # Prepare data based on different selection methods
    if (is.null(shared_data$w_station_id) & (is.null(shared_data$ibm_data))){
      # Set default location and data
      
      data_prepared <- forecast_data() %>% filter(station_id == 'LNCT') 
      location <- data_prepared$station_name[1]
      title_txt <- paste0("Risk Trend at ", location, ' Station')
      county <- data_prepared$county[1]
      city <- data_prepared$city[1]
      subtitle_txt <- paste0("Located at ", county, ', ',city, ' City') 
    }
    
    if (!is.null(shared_data$ibm_data)) {
      # Data selected by map click
      req(shared_data$ibm_data)
      title_txt <- "Risk Trend at the given Location"
      subtitle_txt <- ""
      data_prepared <- shared_data$ibm_data %>%
        filter(!is.na(tarspot_risk) &
                 !is.na(whitemold_nirr_risk) &
                 !is.na(gls_risk) &
                 !is.na(whitemold_irr_15in_risk) &
                 !is.na(whitemold_irr_30in_risk) &
                 !is.na(fe_risk)) %>%
        arrange(desc(as.Date(date, format = "%Y-%m-%d"))) %>%  # Sort by date descending
        head(7)  # Get the top 7 rows
      
      location <- paste0("Lat ", shared_data$lat_location, " Lon ", shared_data$lng_location)
      
    }else if (!is.null(shared_data$w_station_id)) {
      # Data selected by station ID
      data_prepared <- forecast_data() %>% 
        filter(station_name == shared_data$w_station_id)
      #thestation <- data_prepared$station_name[1]
      county <- data_prepared$county[1]
      city <- data_prepared$city[1]
      subtitle_txt <- paste0("Located at ", county, ', ',city, ' City') 
      location <- data_prepared$station_name[1]
      title_txt <- paste0("Risk Trend at ", shared_data$w_station_id, ' Station')
      
    }
    
    # Prepare long-format data with separate risk and class columns
    data_long <- data_prepared %>% 
      mutate(forecasting_date = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(across(ends_with("_risk"), as.numeric)) %>%
      select(
        forecasting_date, 
        tarspot_risk, tarspot_risk_class,
        gls_risk, gls_risk_class,
        fe_risk, fe_risk_class,
        whitemold_irr_30in_risk, whitemold_irr_30in_class,
        whitemold_irr_15in_risk, whitemold_irr_15in_class,
        whitemold_nirr_risk, whitemold_nirr_risk_class
      ) %>%
      pivot_longer(
        cols = c(
          tarspot_risk, gls_risk, fe_risk, 
          whitemold_irr_30in_risk, whitemold_irr_15in_risk, 
          whitemold_nirr_risk
        ),
        names_to = "risk_type",
        values_to = "risk_value"
      ) %>%
      mutate(
        disease = case_when(
          risk_type == "tarspot_risk" ~ "tarspot",
          risk_type == "gls_risk" ~ "gls",
          risk_type == "fe_risk" ~ "fe",
          risk_type == "whitemold_irr_30in_risk" ~ "whitemold_irr_30in",
          risk_type == "whitemold_irr_15in_risk" ~ "whitemold_irr_15in",
          risk_type == "whitemold_nirr_risk" ~ "whitemold_nirr"
        ),
        risk_class = case_when(
          risk_type == "tarspot_risk" ~ tarspot_risk_class,
          risk_type == "gls_risk" ~ gls_risk_class,
          risk_type == "fe_risk" ~ fe_risk_class,
          risk_type == "whitemold_irr_30in_risk" ~ whitemold_irr_30in_class,
          risk_type == "whitemold_irr_15in_risk" ~ whitemold_irr_15in_class,
          risk_type == "whitemold_nirr_risk" ~ whitemold_nirr_risk_class
        )
      ) %>%
      select(-risk_type, -contains("_risk_class"))
    
    # Filter for selected diseases
    req(input$disease)
    df_subset <- data_long %>% 
      filter(disease %in% input$disease) %>%
      mutate(risk_value = risk_value * 100)
    
    # Define disease-specific plot settings
    disease_settings <- list(
      "Tar Spot" = list(
        low_threshold = 35,
        moderate_threshold = 50,
        low_label = "1.Low",
        moderate_label = "2.Moderate",
        high_label = "3.High"
      ),
      "Frog Eye Leaf Spot" = list(
        low_threshold = 50,
        moderate_threshold = 60,
        low_label = "1.Low",
        moderate_label = "2.Moderate", 
        high_label = "3.High"
      ),
      # Add more disease-specific settings as needed
      default = list(
        low_threshold = 20,
        moderate_threshold = 35,
        low_label = "1.Low",
        moderate_label = "2.Moderate",
        high_label = "3.High"
      )
    )
    
    # Create plot
    ggplot(df_subset, aes(x = forecasting_date, y = risk_value, color = disease)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 4) +
      geom_text(aes(label = risk_class), angle = 90, vjust = -1, size = 4)+
      # Use disease-specific or default thresholds
      {
        disease_key <- df_subset$disease[1]
        settings    <- disease_settings[[disease_key]] %||% disease_settings$default
        
        list(
          geom_hline(yintercept = settings$low_threshold, 
                     linetype = "dashed", color = "green"),
          geom_hline(yintercept = settings$moderate_threshold, 
                     linetype = "dashed", color = "gray50"),
          annotate("text", x = min(df_subset$forecasting_date), 
                   y = settings$low_threshold/2, 
                   label = settings$low_label, 
                   vjust = -0.5, hjust = 0, size = 4),
          annotate("text", x = min(df_subset$forecasting_date), 
                   y = (settings$low_threshold + settings$moderate_threshold)/2, 
                   label = settings$moderate_label, 
                   vjust = -0.5, hjust = 0, size = 4),
          annotate("text", x = min(df_subset$forecasting_date), 
                   y = settings$moderate_threshold + 10, 
                   label = settings$high_label, 
                   vjust = -0.5, hjust = 0, size = 4)
        )
      } +
      labs(
        title = title_txt,
        subtitle = subtitle_txt,
        x = "Forecasting Date",
        y = "Risk (%)",
        color = "Disease"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "bottom"
      )
  }, width = 800, height = 600)
  
  
  output$weather_trend <- renderPlot({
    # Prepare the data based on shared_data.
    data_prepared <- historical_data %>% filter(station_name == 'ALTN')
    air_plot_data <- NULL
    rh_data <- NULL
    # show a permanent notification
    showNotification(
      "Loading data…",
      id       = "loading-notice-weather",
      duration = NULL,         # stay until removed
      type     = "message"
    )
    
    on.exit({
      # as soon as reactive finishes (success or error), remove it
      removeNotification("loading-notice-weather")
    }, add = TRUE)
    
    if (!is.null(shared_data$ibm_data)) {
      print("shared ibm data")
      data_prepared <- shared_data$ibm_data
      
      data_prepared$date <- as.Date(data_prepared$forecasting_date, format = '%Y-%m-%d')+1
      #location <- paste0("Custom coordinates in map (", addr, ")")
      air_temp_data <- data_prepared %>%
        pivot_longer(
          cols = contains("temperature_"),
          names_to = "variable",
          values_to = "value"
        )
      air_plot_data <- air_temp_data %>%
        mutate(
          display_value = if (input$temp_unit == "F") {
            # C → F
            value * 9/5 + 32
          } else {
            value
          }
        )
      # Prepare relative humidity data.
      rh_data <- data_prepared %>%
        pivot_longer(
          cols = contains("relativeHumidity_"),
          names_to = "variable",
          values_to = "value"
        )
    }else if (!is.null(shared_data$w_station_id)) {
      station_id_from_sname <- historical_data %>%
        filter(station_name == shared_data$w_station_id) %>%
        pull(station_id) %>%
        first()
      
      data_prepared <- fetch_bulk_measures(station_id_from_sname, 
                                           input$forecasting_date, days = 100)  
      data_prepared <- data_prepared %>%rename(variable = standard_name)
      data_prepared <- data_prepared %>% mutate(
        variable          = as.character(variable),
        value             = as.numeric(value),
        date  = as.Date(data_prepared$collection_time, format = '%Y-%m-%d')
      )
      print("-------------")
      print(data_prepared)
      # pivot air‐temperature (excluding _f columns)
      air_temp_data <- data_prepared %>% 
        filter(measure_type=='Air Temp' & final_units=='fahrenheit') %>% 
        select(variable, value, date)
      
      air_plot_data <- air_temp_data %>%
        mutate(
          display_value = if (input$temp_unit == "C") {
            # F → C
            (value -32)* 5/9
          } else {
            value
          }
        )
      
      # pivot rh_max
      rh_data <- data_prepared %>%
        filter(measure_type=='Relative Humidity' & final_units=='pct') %>% 
        select(variable, value, date)
      
    }
    
    
    if(!is.null(air_plot_data) && !is.null(rh_data)){
      
      air_plot_data <- air_plot_data %>%
        mutate(
          measurement = case_when(
            variable == "air_temp_avg_c_30d_ma"   ~ "Mean Air Temp (30d)",
            variable == "daily_air_temp_f_avg"   ~ "Mean Air Temp",
            variable == "daily_air_temp_f_max"   ~ "Max Air Temp",
            variable == "daily_air_temp_f_min"   ~ "Min Air Temp",
            variable == "air_temp_max_c_30d_ma"   ~ "Max Air Temp (30d)",
            variable == "air_temp_min_c_30d_ma"   ~ "Min Air Temp (30d)",
            variable == "air_temp_avg_c"   ~ "Mean Air Temp",
            variable == "temperature_max"   ~ "Max Air Temp",
            variable == "temperature_min"   ~ "Min Air Temp",
            variable == "temperature_mean"   ~ "Mean Air Temp",
            variable == "temperature_max_30ma"   ~ "Max Air Temp (30d)",
            variable == "temperature_min_21ma"   ~ "Min Air Temp (21d)",
            variable == "temperature_mean_30ma"   ~ "Mean Air Temp (30d)",
            variable == "air_temp_min_c_21d_ma"   ~ "Min Air Temp (21d)",
            variable == "air_temp_min_c"   ~ "Min Air Temp",
            variable == "air_temp_max_c"   ~ "Max Air Temp",
            TRUE                           ~ variable   # fallback: keep original
          )
        )
      
      rh_data <- rh_data %>% 
        mutate(
          measurement = case_when(variable=='rh_max'~ "Max Relative Humidity %",
                                  variable=='daily_relative_humidity_pct_max'~ "Max Relative Humidity %",
                                  variable=='daily_relative_humidity_pct_min'~ "Min Relative Humidity %",
                                  variable=='rh_max_30d_ma'~ "Max Relative Humidity % (30d)",
                                  variable=='relativeHumidity_max'~ "Max Relative Humidity %",
                                  variable=='relativeHumidity_mean'~ "Mean Relative Humidity %",
                                  variable=='relativeHumidity_min'~ "Min Relative Humidity %",
                                  variable=='relativeHumidity_max_30ma'~ "Max Relative Humidity % (30d)",
                                  variable=='rh_max_30d_ma'~ "Max Relative Humidity % (30d)",
                                  TRUE ~ variable)   # fallback: keep original)
        )
      y_label <- paste0("Air Temperature (°", input$temp_unit, ")")
      
      # Prepare air temperature data.
      p1 <- ggplot(air_plot_data, aes(x = date, y = display_value, color = measurement)) +
        geom_line() +
        geom_point() +
        labs(
          title = paste("Air Temperature (°", input$temp_unit, ")"),
          x = "Forecasting Date",
          y = y_label
        ) +
        theme_minimal()
      
      p2 <- ggplot(rh_data, aes(x = date, y = value, color = measurement)) +
        geom_line() +
        geom_point() +
        labs(
          title = paste("Relative Humidity Trends"),
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
                 city, county, region, state, 
                 tarspot_risk_class,  
                 gls_risk_class, fe_risk_class, whitemold_nirr_risk_class,
                 whitemold_irr_30in_class, whitemold_irr_15in_class)      
      }else if(input$ibm_data==TRUE){
        if (!is.null(shared_data$ibm_data)) {
          # Query IBM data
          data_f1 <- shared_data$ibm_data
          data_f1$lat <- shared_data$lat_location
          data_f1$lng <- shared_data$lng_location
          data_f <- data_f1 %>% select(date, lat, lon, 
                                       tarspot_risk_class, gls_risk_class, fe_risk_class, 
                                       whitemold_nirr_risk_class, whitemold_irr_30in_class, 
                                       whitemold_irr_15in_class)  
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
  #output$download_report <- downloadHandler(
  #  filename = function() {
  #    req(shared_data$w_station_id, cancelOutput = TRUE)
  #    paste0("Report_risktrend_uwmadison_",
  #           output$w_station_id,'_', Sys.Date(), ".pdf")
  #  },
  #  content = function(file) {
  #    data <- historical_data %>% filter(as.Date(forecasting_date) == as.Date(input$forecasting_date) 
  #                                      & (station_name==shared_data$w_station_id))
  
  #    location_name <- paste0(data$station_name[1], " Station")
  #    
  #    if (is.null(data) || nrow(data) == 0) {
  #      showNotification("No data available to generate the report.", type = "warning")
  #      stop("No data available to generate the report.")
  #    }
  #    output$downloadData <- downloadHandler(
  #      filename = function() {
  #        paste("historical_data_", Sys.Date(), ".csv", sep = "")
  #      },
  #      content = function(file) {
  #        # Write CSV file without row names
  #        write.csv(historical_data, file, row.names = FALSE)
  #      }
  #    )
  #    data_f <- historical_data %>% select(forecasting_date,
  #                              tarspot_risk,tarspot_risk_class,
  #                              gls_risk,gls_risk_class,
  #                              fe_risk,fe_risk_class,
  #                              whitemold_irr_30in_risk,whitemold_irr_15in_risk,
  #                              whitemold_nirr_risk
  #                              )
  #    
  #    report_template<-template_pdf(file)
  
  #    # Prepare report parameters
  #    report_params <- list(
  #      location = location_name,
  #      #disease = custom_disease_name(input$disease_name),
  #      forecasting_date = input$forecasting_date,
  #      fungicide = input$no_fungicide,
  #      growth_stage = input$crop_growth_stage,
  #      risk_table = data_f
  #    )
  
  #    # Render the report
  #    tryCatch({
  #      rmarkdown::render(
  #        input = report_template,
  #        output_file = file,
  #        params = report_params,
  #        envir = new.env(parent = globalenv()) # To avoid any potential environment issues
  #      )
  #    }, error = function(e) {
  #      showNotification(
  #        paste("Report generation failed:", e$message), 
  #        type = "error", 
  #        duration = 10
  #      )
  #      stop(e)
  #    })
  #  }
  #)
}
