library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
#install.packages("leaflet.extras")
library(leaflet.extras)
library(httr)


plot_trend <- function(df, station){
  ggplot(df, aes(x = Date, y = Risk)) +
    geom_line(color = "#0C7BDC") +
    geom_point(aes(color = Risk_Class), size = 4) +  # Map color to Risk_Class
    geom_text(aes(label = Risk_Class),
              vjust = -0.5,
              color = "black",
              size = 5) +
    labs(title = paste(station$name, "Station,", station$region, "Region,", station$state),
         x = "Date",
         y = "Probability of Tar Spot (%)") +
    scale_y_continuous(labels = percent_format(scale = 1),
                       breaks = seq(0, 100, by = 20)) +
    
    # Set colors for Risk_Class categories
    scale_color_manual(values = c("High" = "black", "Medium" = "#FFC20A", "Low" = "darkgreen")) +
    
    # Control x-axis date formatting and frequency
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate date labels for readability
    )+guides(color = "none")  # Remove the color legend
}

# Define a function to create the weather plot
plot_weather_data <- function(data, station) {
  # Create the plot
  weather_plot <- data %>%
    ggplot(aes(x = collection_time)) +
    
    # Min temperature and its 30-day moving average
    geom_line(aes(y = air_temp_min_c, color = "Min Temp (°C)")) +
    geom_line(aes(y = air_temp_min_c_30d_ma, color = "Min Temp (°C) (30d MA)"), linetype = "dashed") +
    
    # Avg temperature and its 30-day moving average
    geom_line(aes(y = air_temp_avg_c, color = "Avg Temp (°C)")) +
    geom_line(aes(y = air_temp_avg_c_30d_ma, color = "Avg Temp (°C) (30d MA)"), linetype = "dashed") +
    
    # Max temperature and its 30-day moving average
    geom_line(aes(y = air_temp_max_c, color = "Max Temp (°C)")) +
    geom_line(aes(y = air_temp_max_c_30d_ma, color = "Max Temp (°C) (30d MA)"), linetype = "dashed") +
    
    # Add RH to the plot using the secondary y-axis
    #geom_line(aes(y = rh_max, color = "Max RH (%)"), linetype = "solid") +
    
    # Primary y-axis for temperature
    #scale_y_continuous(
    #  name = "Temperature (C)",  # Label for the primary y-axis
    #  sec.axis = sec_axis(~ ., name = "Relative Humidity (%)")  # Secondary y-axis for RH
    #) +
    
    # Title, labels, and theme
  labs(title = paste("Air Temperature (°C) for", station),
       x = "Date",y='Air Temperature (°C)') +
    
    # Minimal theme
    theme_minimal() +
    
    # Move the legend below the plot
    theme(
      legend.position = "bottom",         # Position the legend below the plot
      legend.direction = "horizontal",    # Arrange the legend items horizontally
      legend.title = element_blank(),     # Remove the legend title
      legend.text = element_text(size = 10)  # Customize legend text size
    ) +
    
    # Color manual assignment
    scale_color_manual(values = c(
      "Min Temp (°C)" = "#5DA5DA",            # Soft blue
      "Min Temp (°C) (30d MA)" = "#ADD8E6",   # Light blue
      "Avg Temp (°C)" = "#60BD68",            # Soft green
      "Avg Temp (°C) (30d MA)" = "#B2E2B2",   # Light green
      "Max Temp (°C)" = "#FAA43A",            # Light orange
      "Max Temp (°C) (30d MA)" = "#FDDC9B",   # Light peach
      "Max RH (%)" = "#B276B2",              # Soft purple
      "RH (30d MA)" = "#CFCFCF",             # Light gray for subtler contrast
      "Dew Point (°C)" = "#FFC107"            # Muted yellow
    ))
  
  
  # Return the plot
  return(weather_plot)
}

render_combined_plot <- function(tarspot_plot, weather_plot) {
  if (!is.null(tarspot_plot) && !is.null(weather_plot)) {
    grid.arrange(tarspot_plot, weather_plot, ncol = 2)
  } else if (!is.null(tarspot_plot)) {
    grid.arrange(tarspot_plot, ncol = 1)
  } else if (!is.null(weather_plot)) {
    grid.arrange(weather_plot, ncol = 1)
  } else {
    ggplot() + ggtitle("No Data Available") + theme_void()
  }
}

############ this is a mini test to include the heat map
## still need to check how to "store" to not call any time suring the day
# api call, tarspot
call_tarspot_for_station <- function(station_id, risk_threshold, current) {
  tryCatch({
    today_ct <- with_tz(current, tzone = "America/Chicago")
    out <- from_ct_to_gmt(today_ct, 1.5)
    start_time <- out$start_time_gmt
    end_time <- out$end_time_gmt
    
    # Fetch data
    rh_data <- fetch_rh_above_90_daily(station_id, end_time)
    at_data <- api_call_wisconet_data_daily(station_id, end_time)
    
    # Merge and compute risk
    merged_ds <- merge(
      x = rh_data %>% mutate(date_day = as.Date(adjusted_date)),
      y = at_data %>% mutate(date_day = as.Date(collection_time) - 1),
      by = "date_day"
    ) %>%
      rowwise() %>%
      mutate(
        risk_output = list(get_risk_probability(
          station_id = station_id,
          risk_threshold = risk_threshold,
          mat_30dma = air_temp_avg_c_30d_ma,
          max_rh_30dma = rh_max_30d_ma,
          th_rh90_14ma = rh_above_90_daily_14d_ma,
          url_ts = url_ts
        )),
        Risk = risk_output$Risk,
        Risk_Class = risk_output$Risk_Class
      ) %>%
      select(-risk_output)
    
    return(merged_ds)
  }, error = function(e) {
    stop(paste("Failed to fetch data:", e$message))
  })
}


# Improved date input validation
validate_date <- function(date) {
  # Ensure the date is not in the future
  if (as.Date(date) > Sys.Date()) {
    stop("Date cannot be in the future")
  }
  paste0("The input date is ", date)
  return(as.character(date))
}

# Modify fetch_forecasting_data to use the validation
disease_config <- list(
  tarspot = list(name = "Tar Spot", risk_col = "tarspot_risk"),
  gls = list(name = "Gray Leaf Spot", risk_col = "gls_risk"),
  frogeye_leaf_spot = list(name = "Frogeye Leaf Spot", risk_col = "frogeye_risk")
)

# Transform function for tarspot_risk
transform_tarspot_risk <- function(value) {
  if (is.na(value)) return(NA)
  value <- as.character(value)
  value <- trimws(value)
  value <- gsub("[^0-9.]", "", value)
  return(as.numeric(value))
}

# Main processing function
process_stations_data <- function(stations_data, risk_col) {
  tryCatch({
    # Combine and process data
    stations_df <- bind_rows(lapply(stations_data, function(x) {
      if (is.list(x) && risk_col %in% names(x)) {
        df <- as.data.frame(x, stringsAsFactors = FALSE)
        df[[risk_col]] <- sapply(df[[risk_col]], transform_tarspot_risk)
        return(df)
      } else {
        warning(paste("Missing or invalid column:", risk_col, "in station data"))
        return(NULL)
      }
    }))
    
    
    # Check if risk_col exists in stations_df before mutate
    if (!(risk_col %in% names(stations_df))) {
      stop(paste("Column", risk_col, "is missing in the processed data"))
    }
    
    # Create risk and popup_content columns
    stations_df <- stations_df %>%
      mutate(
        risk = .data[[risk_col]],  # Safely assign the risk column
        popup_content = ifelse(
          is.na(station_name) | is.na(risk),
          "Incomplete info",
          sprintf(
            "<strong>Station:</strong> %s <br><strong>Risk:</strong> %.1f%%",
            station_name,
            risk
          )
        )
      ) %>%
      filter(!is.na(risk))  # Remove rows with NA risk
    
    return(stations_df)
  }, error = function(e) {
    warning(paste("Error processing the station info:", e$message))
    return(data.frame(
      station_name = character(),
      longitude = numeric(),
      latitude = numeric(),
      risk = numeric(),
      popup_content = character()
    ))
  })
}


fetch_forecasting_data <- function(date, disease_name) {
  tryCatch({
    api_url <- sprintf(
      "https://connect.doit.wisc.edu/forecasting_crop_disease/predict_wisconet_stations_risk?forecasting_date=%s&disease_name=%s",
      date, disease_name
    )
    #api_url <- paste0("https://connect.doit.wisc.edu/forecasting_crop_disease/predict_wisconet_stations_risk?forecasting_date=", date)
    response <- POST(
      url = api_url,
      add_headers("Content-Type" = "application/json")
    )
    
    # Check for valid response
    if (status_code(response) != 200) {
      stop(paste("API Error:", status_code(response), "Message:", content(response, as = "text")))
    }
    
    response_content <- content(response, as = "parsed", type = "application/json")
    
    if (is.null(response_content$stations_risk) || length(response_content$stations_risk) == 0) {
      stop("No stations_risk data in API response")
    }
    
    json_string <- response_content$stations_risk[[1]]
    stations_data <- fromJSON(json_string)
    
    stations_df <- bind_rows(lapply(stations_data, bind_rows))
    print(stations_df)
    
    if (disease_name=='tarspot'){
      stations_df<- stations_df %>% mutate(
        across(c(latitude, longitude, tarspot_risk), as.numeric),
        risk = 100 * tarspot_risk,  # Scale risk
        popup_content = sprintf(
          "<strong>Station:</strong> %s<br><strong>Tar Spot Risk:</strong> %.1f%%<br><strong>Forecast Date:</strong> %s",
          station_name,
          risk,
          date
        )
      )
      return(stations_df)
    }
    
    if (disease_name=='gls'){
      stations_df<- stations_df %>% mutate(
        across(c(latitude, longitude, gls_risk), as.numeric),
        risk = 100 * gls_risk,  # Scale risk
        popup_content = sprintf(
          "<strong>Station:</strong> %s<br><strong>Gray Leaf Spot Risk:</strong> %.1f%%<br><strong>Forecast Date:</strong> %s",
          station_name,
          risk,
          date
        )
      )
      return(stations_df)
    }
    
    if (disease_name=='frogeye_leaf_spot'){
      stations_df<- stations_df %>% mutate(
        across(c(latitude, longitude, frogeye_risk), as.numeric),
        risk = 100 * frogeye_risk,  # Scale risk
        popup_content = sprintf(
          "<strong>Station:</strong> %s<br><strong>Frogeye Leaf Spot Risk:</strong> %.1f%%<br><strong>Forecast Date:</strong> %s",
          station_name,
          risk,
          date
        )
      )
      return(stations_df)
    }
    
  }, error = function(e) {
    message(paste0("Error fetching data: ", e$message))
    return(NULL)
  })
}



tool_title <- "Agricultural Forecasting and Advisory System"

# UI 
ui <- navbarPage(
  title = tool_title,
  theme = shinythemes::shinytheme("flatly"),  # Add a theme for better aesthetics
  id = "navbar", 
  # Tab 1: Weather Map
  tabPanel(
    title = "Disease Forecasting",
    sidebarLayout(
      sidebarPanel(
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
        h4("Map Layers"),
        checkboxInput("show_stations", "Show Stations", value = TRUE),
        checkboxInput("show_heatmap", "Show Heat Map", value = TRUE),
        hr(),
        h4("Visualization Settings"),
        sliderInput(
          "radius", 
          "Heat Map Radius:", 
          min = 5, 
          max = 50, 
          value = 15,
          step = 1
        ),
        sliderInput(
          "blur", 
          "Heat Map Blur:", 
          min = 1, 
          max = 30, 
          value = 20,
          step = 1
        ),
        sliderInput(
          "opacity", 
          "Heat Map Opacity:", 
          min = 0, 
          max = 1, 
          value = 0.8,
          step = 0.1
        )
      ),
      mainPanel(
        leafletOutput("risk_map", height = 600),
        div(
          textOutput("map_info"),
          style = "margin-top: 10px; color: #666;"
        ),
        div(
          textOutput("station_count"),  # To display the number of stations
          style = "margin-top: 10px; color: #666; font-size: 14px;"
        ),
        div(
          style = "margin-top: 20px; color: black; font-size: 14px;",
          tags$span(icon("envelope"), " Contact: "),
          tags$a(
            href = "mailto:maria.oros@wisc.edu",
            "maria.oros@wisc.edu",
            style = "color: black; text-decoration: none;"
          ),
          br(),
          tags$span(icon("github"), " GitHub: "),
          tags$a(
            href = "https://github.com/UW-Madison-DSI/corn_disease_forecast_api.git",
            "https://github.com/UW-Madison-DSI/corn_disease_forecast_api.git",
            style = "color: black; text-decoration: none;"
          )
        )
      )
    )
  ),
  
  # Tab 2: Growth Projection
  #tabPanel(
  #  title = "Growth Projection",
  #  fluidPage(
  #    h3("Growth Projection"),
  #    p("This section will display growth projection data.")
  #  )
  #),
  
  # Tab 4: Weather Charts
  tabPanel(
    title = "Weather Charts",
    fluidPage(
      h3("Weather Charts"),
      p("This section will display weather-related charts.")
    )
  ),
  
  # Tab 5: Downloads
  tabPanel(
    title = "Downloads",
    fluidPage(
      h3("Downloads"),
      p("This section will provide downloadable content.")
    )
  ),
  
  # Tab 6: About
  tabPanel(
    title = "About",
    fluidPage(
      h3("About the Agricultural Forecasting and Advisory System"),
      p("This application provides weather-based forecasting and risk assessments for various crop diseases, helping farmers and agricultural researchers make data-driven decisions."),
      h4("Features:"),
      tags$ul(
        tags$li("Interactive weather map with disease risk visualization"),
        tags$li("Dynamic data for different forecasting dates and diseases"),
        tags$li("Downloadable Report")
      ),
      h4("How It Works:"),
      p("The application uses data from trusted weather and agricultural sources to forecast the risk of crop diseases."),
      tags$ul(
        tags$li("Select a disease and forecasting date to view the risk map."),
        tags$li("The map highlights disease risk levels across different weather stations."),
        tags$li("Users can click on stations to get more details and center the map on specific locations.")
      ),
      h4("Credits:"),
      p("This application was developed by a multidisciplinary team of data scientists and agricultural researchers."),
      tags$ul(
        tags$li("Weather data provided by: Wisconet Stations"),
        tags$li("Crop disease data provided by: Plant Pathology at UW Madison"),
        tags$li("This is an innitiative from: the Open Source Program Office at UW Madison")
      ),
      h4("Contact Us:"),
      p("For inquiries or feedback, please reach out to us:"),
      tags$ul(
        tags$li(tags$a(href = "mailto:contact@forecasting-system.com", "Email: ospo@datascienceinstitute.wisc.edu")),
        tags$li(tags$a(href = "mailto:contact@forecasting-system.com", "Email: damon.smith@wisc.edu")),
        tags$li(tags$a(href = "mailto:contact@forecasting-system.com", "Email: maria.oros@wisc.edu")),
        tags$li(tags$a(href = "https://github.com/UW-Madison-DSI/corn_disease_forecast_api.git", "Github Repo: https://github.com/UW-Madison-DSI/corn_disease_forecast_api.git"))
      ),
      h4("Acknowledgments:"),
      p("This project is supported by OSPO and relies on contributions from multiple research groups.")
    )
  )
  
)


server <- function(input, output, session) {
  # Fetch fresh data directly based on user inputs
  stations_data <- reactive({
    req(input$forecast_date)
    req(input$disease_name)
    fetch_forecasting_data(as.character(input$forecast_date), input$disease_name)
  })
  
  # Update stations data when "Update Map" button is clicked
  observeEvent(input$update, {
    req(input$forecast_date)
    req(input$disease_name)
    stations_data()
  })
  
  # Create the color palette with a dynamic domain
  color_palette <- colorNumeric(
    palette = "viridis",
    domain = c(0, 100)
  )
  
  # Render Leaflet map
  output$risk_map <- renderLeaflet({
    data <- stations_data()
    if (is.null(data) || nrow(data) == 0) {
      return(
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = -89.4, lat = 43.1, zoom = 7)
      )
    }
    
    map <- leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(
        lng = -89.75, lat = 44.76, zoom = 7
      )
    
    # Conditional layers
    if (input$show_heatmap) {
      map <- map %>%
        addHeatmap(
          lng = ~longitude,
          lat = ~latitude,
          intensity = ~risk,
          blur = input$blur %||% 20,
          max = 1,
          radius = input$radius %||% 15,
          minOpacity = input$opacity %||% 0.8
        )
    }
    
    if (input$show_stations) {
      map <- map %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(
          lng = -89.75, lat = 44.76, zoom = 7
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
          title = "Risk (%)",           # Legend title
          labFormat = labelFormat(suffix = "%"),  # Add % suffix to labels
          opacity = 1                   # Opacity of the legend
        )
    }
    return(map)
  })
  
  # Observe click event to center the map on the selected station
  observeEvent(input$risk_map_marker_click, {
    click <- input$risk_map_marker_click
    print(click)
    if (!is.null(click$id)) {
      station_id <- click$id  # Extract the station ID from the click event
      showNotification(paste("Selected Station ID:", station_id), type = "message")
      # Use station_id for further processing
      print(paste("Station ID:", station_id))
    }
    if (!is.null(click)) {
      leafletProxy("risk_map") %>%
        setView(lng = click$lng, lat = click$lat, zoom = 10)  # Adjust zoom level as needed
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
    }
    if (input$disease_name=='gls'){
      avg_risk <- mean(data$gls_risk, na.rm = TRUE)
    }
    if (input$disease_name=='frogeye_leaf_spot'){
      avg_risk <- mean(data$frogeye_risk, na.rm = TRUE)
    }
    paste(
      "Number of stations: ", nrow(data),
      " | Mean Risk for the selected forecasting date: ", sprintf("%.2f%%", 100*avg_risk)
    )
  })
  
}



# Optional: Add error handling wrapper
shinyApp(ui = ui, server = server)