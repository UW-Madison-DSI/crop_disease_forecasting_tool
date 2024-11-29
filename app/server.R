library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
library(leaflet.extras)
library(httr)
library(tigris)
library(sf)
options(tigris_use_cache = TRUE)
library(DT)

source("functions/auxiliar_functions.R")
source("functions/api_calls_logic.R")



county_boundaries <- counties(state = "WI", cb = TRUE, class = "sf") %>%
  st_transform(crs = 4326)



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
  
  ################################################################## This is the section 1 risk_map
  output$risk_map <- renderLeaflet({
    county_boundaries <- st_transform(county_boundaries, crs = 4326)
    
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
          intensity = ~risk/10,
          blur = 10,
          max = 1,
          radius = 10,
          minOpacity = 0.8
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
        "bottomright",
        pal = color_palette,
        values = data$risk,
        title = paste0("Predicted Risk (%)"),
        labFormat = labelFormat(suffix = "%"),
        opacity = 1
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
      hideGroup("County Boundaries")
    
    return(map)
  })
  
  shared_data <- reactiveValues(
    w_station_id = NULL
  )
  
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
          showNotification(
            paste("Risk Class: ", rclass), 
            type = rclass_msg
          )
        } else {
          warning("Risk class is NULL for station ID: ", click$id)
        }
      } else {
        warning("Click event does not contain an ID.")
      }
      
      # Update the map view to the clicked location
      leafletProxy("risk_map") %>%
        setView(lng = click$lng, lat = click$lat, zoom = 16) %>%
        addProviderTiles("USGS.USTopo", group = "CartoDB Positron")
    } else {
      warning("No click event detected.")
    }
  })
  
  
  observeEvent({
    input$crop_growth_stage  # Include inputs to trigger observation
    input$no_fungicide
  }, {
    if (!input$no_fungicide) {
      showNotification(
        paste(
          custom_disease_name(input$disease_name),
          "risk can only be computed if no fungicide was applied in the last 14 days."
        ),
        type = "error"
      )
    } else if(!input$crop_growth_stage){
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
    data <- stations_data()

    if (is.null(data) || nrow(data) == 0) {
      return("No stations available.")
    }
    # Calculate mean risk, excluding NA values
    if (input$disease_name=='tarspot'){
      avg_risk <- mean(data$tarspot_risk, na.rm = TRUE)
      data$risk_class <- risk_class_function(data$tarspot_risk, input$disease_name, input$risk_threshold)
    }
    if (input$disease_name=='gls'){
      avg_risk <- mean(data$gls_risk, na.rm = TRUE)
      data$risk_class <- risk_class_function(data$gls_risk, input$disease_name, input$risk_threshold)
    }
    if (input$disease_name=='frogeye_leaf_spot'){
      avg_risk <- mean(data$frogeye_risk, na.rm = TRUE)
      data$risk_class <- risk_class_function(data$frogeye_risk, input$disease_name, input$risk_threshold)
    }
    
    risk_counts <- table(data$risk_class)
    
    # Construct a descriptive text
    risk_summary <- paste(names(risk_counts), risk_counts, sep = ": ", collapse = " | ")
    
    paste(
      "Number of stations: ", nrow(data), "| ",
      "Mean Risk for the selected forecasting date: ", sprintf("%.1f%%", 100 * avg_risk)#,
      #"\n Stations per Risk Class: ", risk_summary
    )
  })
  
  ################################################################## This is the section 2 or tab panel Station summary
  disease_risk_data <- reactive({
    if (!is.null(shared_data$w_station_id)) {
      # Define the date range
      given_date <- as.Date(input$forecast_date)
      date_range <- as.list(seq(given_date - 7, given_date, by = "day"))
      print(date_range)
      brows<-call_forecasting_for_range_of_days(date_range, shared_data$w_station_id, input$disease_name)
      return(brows)
    } else {
      return(NULL)
    }
  })
  
  # Render the data table in the UI
  output$station_trend <- renderDT({
    if (is.null(shared_data$w_station_id)){
      datatable(data.frame(Message = "No data available"))
    } else {
      data <- disease_risk_data() # Reactive data from the API
      if(nrows(data)==0){
        if (input$disease_name == 'tarspot') {
          data$risk <- data$tarspot_risk*100
          data$risk_class <- risk_class_function(data$tarspot_risk, input$disease_name, input$risk_threshold)
          data_f <- data %>%
            select(forecasting_date, risk, risk_class) %>% 
            rename(
              `Forecasting Date` = forecasting_date,
              Risk = risk,
              `Risk Class`=risk_class)
        } else if (input$disease_name == 'gls') {
          data$risk <- data$gls_risk*100
          data$risk_class <- risk_class_function(data$gls_risk, input$disease_name, input$risk_threshold)
          data_f <- data %>%
            select(forecasting_date, risk, risk_class) %>% 
            rename(
              `Forecasting Date` = forecasting_date,
              Risk = risk,
              `Risk Class`=risk_class)
        } else if (input$disease_name == 'frogeye_leaf_spot') {
          data$risk <- data$frogeye_risk*100
          data$risk_class <- risk_class_function(data$frogeye_risk, input$disease_name, input$risk_threshold)
          data_f <- data %>%
            select(forecasting_date, risk, risk_class) %>% 
            rename(
              `Forecasting Date` = forecasting_date,
              Risk = risk,
              `Risk Class`=risk_class)
        }
        datatable(data_f)
      }else{
        paste("no data")
      }
    }
  })
  
  output$station_specifications <- renderText({
    if (is.null(shared_data$w_station_id)) {
      "This section will display weather-related charts for the chosen Wisconet Station. 
      Please select an station from the map by doing click on it."
    } else {
      data <- disease_risk_data()
      print(data)
      # Check if data is not empty
      if (nrow(data) > 0) {
        station <- data$station_name[1]
        earliest_api_date <- data$earliest_api_date[1]
        print(station)
        
        
        location <- if_else(
          data$location[1] == "Not set", 
          "", 
          paste(" located at", data$location[1], ", ",data$region[1], " Region, ")
        )
        print(location)
        print(earliest_api_date)
        # Analyze Risk_Class
        if(input$disease_name=='tarspot'){
          data$Risk_Class <- risk_class_function(data$tarspot_risk, input$disease_name, input$risk_threshold)
        }else if(input$disease_name=='gls'){
          data$Risk_Class <- risk_class_function(data$gls_risk, input$disease_name, input$risk_threshold)
        }else{
          data$Risk_Class <- risk_class_function(data$frogeye_risk, input$disease_name, input$risk_threshold)
        }

        high_days_text <- sum(data$Risk_Class == "High") 
        low_days_text <- sum(data$Risk_Class == "Low")        #moderate_days_text <- paste(sum(data$Risk_Class == "Moderate"), "days have moderate probability of ", custom_disease_name(input$disease_name))

        
        if (all(data$Risk_Class =="High"))  {
          all_days_text <- paste("\n Reported high probability of ", custom_disease_name(input$disease_name))
        } else if (all(data$Risk_Class =="Moderate"))  {
          all_days_text <- paste("\n Reported moderate probability of ", custom_disease_name(input$disease_name))
        } else if (all(data$Risk_Class =="Low"))  {
          all_days_text <- paste("\n Reported low probability of ", custom_disease_name(input$disease_name))
        } else if(low_days_text>0){
          all_days_text <- paste(low_days_text, "days have low probability of ", custom_disease_name(input$disease_name))
        } else if(high_days_text>0){
          all_days_text <- paste(high_days_text, "days have high probability of ", custom_disease_name(input$disease_name))
        }
        
        #date_obj <- as.Date(earliest_api_date, format = "%Y-%m-%d")
        # Format for user-friendly reading
        #user_friendly_date <- format(date_obj, "%B %d, %Y")
        paste(
          station, "Station,", location," is active since: ", earliest_api_date, "."
          #all_days_text, ' on the last 8 days from the selected forecasting date.'
        )
      } else {
        "Please select a station by clicking on it in the map from the Disease Forecasting section."
      }
    }
  })
  
  
  # Render Plot
  output$risk_trend <- renderPlot({
    data <- disease_risk_data() # Reactive data from the API
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
  
  ################################################################## This is the section 3 Download
  output$download_stations <- downloadHandler(
    filename = function() {
      paste0("Wisconet_stations_",input$disease_name,"_forecasting_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- stations_data()
      if (input$disease_name=='tarspot'){
        data$tarspot_risk <- data$tarspot_risk*100
        data$risk_class <- risk_class_function(data$tarspot_risk, input$disease_name, input$risk_threshold)
        data_f <- data %>% select(forecasting_date, station_id,station_name,region,state,earliest_api_date,
                                  tarspot_risk, risk_class)
      }
      if (input$disease_name=='gls'){
        data$gls_risk <- data$gls_risk*100
        data$risk_class <- risk_class_function(data$gls_risk, input$disease_name, input$risk_threshold)
        data_f <- data %>% select(forecasting_date, station_id,station_name,region,state,earliest_api_date,
                                  gls_risk, risk_class)
      }
      if (input$disease_name=='frogeye_leaf_spot'){
        data$frogeye_risk <- data$frogeye_risk*100
        data$risk_class <- risk_class_function(data$frogeye_risk, input$disease_name, input$risk_threshold)
        data_f <- data %>% select(forecasting_date, station_id,station_name,region,state,earliest_api_date,
                                  frogeye_risk, risk_class)
      }
      
      if (!is.null(data) && nrow(data) > 0) {
        write.csv(data_f, file, row.names = FALSE)
      } else {
        stop("No data available for download.")
      }
    }
  )
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("UWMadison_", input$disease_name, "_Forecast_Report_", 
             shared_data$w_station_id %||% "NoStation", "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Check if a station is selected
      req(shared_data$w_station_id, cancelOutput = TRUE)
      
      # Create temporary directory
      temp_dir <- tempdir()
      logos_dir <- file.path(temp_dir, "logos")
      dir.create(logos_dir, showWarnings = FALSE)
      
      # Write the LaTeX header file to temp_dir
      header_path <- file.path(temp_dir, "header.tex")
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
    ', file = header_path)
      
      # Specify the path to your report template in the working directory
      original_template <- "report_template.Rmd"  # Update with the correct location of your Rmd file
      
      # Copy the report template to the temporary directory
      if (!file.exists(original_template)) {
        showNotification("Original report template not found in working directory.", type = "error")
        stop("Original report template not found in working directory.")
      }
      
      # Copy the report template to temp_dir
      report_template <- file.path(temp_dir, "report_template.Rmd")
      file.copy(original_template, report_template, overwrite = TRUE)
      
      # List of images
      images <- c("OPENSOURDA_color-flush.png", "PLANPATHCO_color-flush.png", "DATASCIE_color-flush.png")
      
      # Copy images to logos_dir within temp_dir
      for (img in images) {
        img_path <- file.path(getwd(), "logos", img) # Assume images are in the 'logos' directory within the working directory
        if (!file.exists(img_path)) {
          showNotification(paste("Image not found:", img), type = "error")
          stop(paste("Image not found:", img))
        }
        file.copy(img_path, file.path(logos_dir, img), overwrite = TRUE)
      }
      
      # Fetch and process disease data
      data <- disease_risk_data()
      
      if (is.null(data) || nrow(data) == 0) {
        showNotification("No data available to generate the report.", type = "warning")
        stop("No data available to generate the report.")
      }
      
      # Prepare data based on the selected disease
      if (input$disease_name == 'tarspot') {
        data$risk <- data$tarspot_risk
        data$risk_class <- risk_class_function(data$tarspot_risk, input$disease_name, input$risk_threshold)
        data_f <- data %>%
          select(station_name, forecasting_date, risk, risk_class) %>%
          rename(
            Station = station_name,
            `Forecasting Date` = forecasting_date,
            Risk = risk,
            `Risk Class` = risk_class
          )
      } else if (input$disease_name == 'gls') {
        data$risk <- data$gls_risk
        data$risk_class <- risk_class_function(data$gls_risk, input$disease_name, input$risk_threshold)
        data_f <- data %>%
          select(station_name, forecasting_date, risk, risk_class) %>%
          rename(
            Station = station_name,
            `Forecasting Date` = forecasting_date,
            Risk = risk,
            `Risk Class` = risk_class
          )
      } else if (input$disease_name == 'frogeye_leaf_spot') {
        data$risk <- data$frogeye_risk
        data$risk_class <- risk_class_function(data$frogeye_risk, input$disease_name, input$risk_threshold)
        data_f <- data %>%
          select(station_name, forecasting_date, risk, risk_class) %>%
          rename(
            Station = station_name,
            `Forecasting Date` = forecasting_date,
            Risk = risk,
            `Risk Class` = risk_class
          )
      }
      
      # Prepare report parameters
      report_params <- list(
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
