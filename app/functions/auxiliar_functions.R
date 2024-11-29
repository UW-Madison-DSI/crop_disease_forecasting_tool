library(lubridate)
library(httr)
library(jsonlite)
library(zoo)
library(ggplot2)
library(dplyr)
library(scales)



##################################################### AOI: Wisconsin
# Define bounds for Wisconsin
lat_min <- 42.49192
lat_max <- 47.08086
lng_min <- -92.88811
lng_max <- -86.80541


################################################################ Functions for transformations
# Function to convert Fahrenheit to Celsius
fahrenheit_to_celsius <- function(temp_f) {
  (temp_f - 32) * 5/9
}

# Function to convert current time to GMT and subtract a number of months
from_ct_to_gmt <- function(current_time, mo){
  # Subtract months from the current time in Central Time
  past_time_ct <- current_time - months(mo)
  
  # Convert both dates to Unix timestamps in GMT
  start_time <- as.integer(as.POSIXct(past_time_ct, tz = "GMT"))
  end_time <- as.integer(as.POSIXct(current_time, tz = "GMT"))
  
  return(list(
    start_time_gmt = start_time,
    end_time_gmt = end_time
  ))
}

################################################################ Risk labels
risk_class_function <- function(risk, disease_name, threshold) {
  #Risk class from Damon et al
  if (disease_name == "tarspot") {
    return(ifelse(risk < .2, "Low",
                  ifelse(risk > .35, "High", "Moderate")))
  } else if (disease_name == "gls") {
    return(ifelse(risk < .4, "Low",
                  ifelse(risk > .6, "High", "Moderate")))
  } else if (disease_name == "frogeye_leaf_spot") {
    return(ifelse(risk < .4, "Low",
                  ifelse(risk > .5, "High", "Moderate")))
  }
}


custom_disease_name <- function(disease){
  #Function to map the acronim of disease to the custom name
  if (disease=='tarspot'){
    return(
      "Tar Spot"
    )
  }else if (disease=='gls'){
    return(
      "Gray Leaf Spot"
    )
  }else if (disease=='frogeye_leaf_spot'){
    return(
      "Frogeye Leaf Spot"
    )
  }
}

################################################################ Function to plot the weather data
api_call_wisconet_plot <- function(df) {
  ggplot(df, aes(x = collection_time)) +
    geom_line(aes(y = air_temp_avg_c, color = "Daily Average")) +
    geom_line(aes(y = air_temp_avg_c_30d_ma, color = "30-day Moving Average")) +
    labs(title = "Average Temperature (Celsius)",
         x = "Date",
         y = "Temperature (°C)",
         color = "Legend") +
    theme_minimal() +
    theme(legend.position = "bottom")
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


#################################################################### This station

api_call_this_station_specifications <-function(input, station_id){
  url_single_station <- paste0(
    "https://connect.doit.wisc.edu/forecasting_crop_disease/predict_wisconet_stations_risk?",
    "forecasting_date=", input$forecast_date,
    "&station_id=", station_id,
    "&disease_name=", input$disease_name
  )
  response <- POST(url_single_station, add_headers(Accept = "application/json"))
  if (status_code(response) == 200) {
    # Parse the main JSON content
    content_data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
    
    
    # Parse the nested JSON in `stations_risk`
    stations_risk_data <- fromJSON(content_data$stations_risk)
    print("content data2")
    print(stations_risk_data)
    # Extract the risk based on the disease name
    if (input$disease_name == 'tarspot') {
      risk <- stations_risk_data$`1`$tarspot_risk
      rclass <- ifelse(risk>input$risk_threshold/100, "High", ifelse(risk > .2, "Moderate", "Low"))
    } else if (input$disease_name == 'gls') {
      risk <- stations_risk_data$`1`$gls_risk
      rclass <- ifelse(risk>input$risk_threshold/100, "High", ifelse(risk > .4, "Moderate", "Low"))
    } else if (input$disease_name == 'frogeye_leaf_spot') {
      risk <- stations_risk_data$`1`$frogeye_risk
      rclass <- ifelse(risk>input$risk_threshold/100, "High", ifelse(risk > .4, "Moderate", "Low"))
    }
    return(rclass)
  }else{
    return(NULL)
  }
}

#################################################################### Preparation of risk trend
#################### 7 days
call_forecasting_for_range_of_days <- function(date_range, station_id, disease_name){
  # Initialize an empty list to store results
  datalist <- vector("list", length = 7)  # Ensure it's pre-sized if you know the max length
  i <- 0
  
  # Loop through each date and call the API
  for (date in date_range) {
    input_date <- format(date, "%Y-%m-%d")
    
    # Construct the API URL
    api_url <- paste0(
      "https://connect.doit.wisc.edu/forecasting_crop_disease/predict_wisconet_stations_risk?",
      "forecasting_date=", input_date,
      "&station_id=", station_id,
      "&disease_name=", disease_name
    )
    print(api_url)  # Debugging
    
    # Make the API call
    response <- POST(
      url = api_url,
      add_headers("Content-Type" = "application/json")
    )
    print('----------------------------------------')
    print(response)
    
    if (status_code(response) == 200) {
      response_content <- content(response, as = "parsed", type = "application/json")
      
      if (!is.null(response_content$stations_risk)) {
        # Safely access the data if it exists
        stations_data <- tryCatch({
          fromJSON(response_content$stations_risk[[1]])
        }, error = function(e) {
          message("Error parsing stations_risk: ", e)
          NULL
        })
        
        if (!is.null(stations_data)) {
          i <- i + 1
          stations_df <- bind_rows(lapply(stations_data, bind_rows))
          datalist[[i]] <- stations_df
          print(stations_df)
        } else {
          print("No valid stations_data")
          print(response_content)
        }
      } else {
        print("stations_risk is NULL")
        print(response_content)
      }
    } else {
      warning(paste("API call failed for date:", date))
    }
  }
  
  # Remove NULL entries in the datalist
  datalist <- datalist[!sapply(datalist, is.null)]
  print(bind_rows(datalist))
  # Combine all results into a single data frame and return
  return(bind_rows(datalist))
  
}

#################### Table risk trend
data_table_for_station_7d<-function(data, input){
  if (!is.null(data)) {
    # Select the appropriate risk column based on the disease
    if (input$disease_name == 'tarspot') {
      data$Risk_Class <- risk_class_function(data$tarspot_risk, input$disease_name, input$treshold_risk)
      
      plot_data <- data %>% 
        select(station_name,earliest_api_date,location,
               forecasting_date, tarspot_risk, Risk_Class) %>% 
        rename(Risk = tarspot_risk)
      
    } else if (input$disease_name == 'gls') {
      data$Risk_Class <- risk_class_function(data$gls_risk, input$disease_name, input$treshold_risk)
      
      plot_data <- data %>% 
        select(station_name,earliest_api_date,location,
               forecasting_date, gls_risk, Risk_Class) %>% 
        rename(Risk = gls_risk)
      
    } else if (input$disease_name == 'frogeye_leaf_spot') {
      data$Risk_Class <- risk_class_function(data$frogeye_risk, input$disease_name, input$treshold_risk)
      
      plot_data <- data %>% 
        select(station_name,earliest_api_date,location,
               forecasting_date, frogeye_risk, Risk_Class) %>% 
        rename(Risk = frogeye_risk)
    }
    return(plot_data)
  }
}

#################### Risk trend plot
plot_trend_7days <- function(df, disease, threshold){
  df$date <- as.Date(df$forecasting_date, format = '%Y-%m-%d')

  station <- df$station_name[[1]]
  if(disease=='Tar Spot'){
    low_line<-.2
    up_line<-.35
  }else{
    low_line<-.4
    up_line<-.6
  }
  if(threshold>0){
    ggplot(df, aes(x = date, y = Risk)) +
      geom_line(color = "#0C7BDC") +
      geom_point(aes(color = Risk_Class), size = 4) +  # Map color to Risk_Class
      geom_text(aes(label = Risk_Class),
                vjust = -0.5,
                color = "black",
                size = 5) +
      geom_hline(yintercept = low_line, linetype = "dashed", color = "gray") +
      geom_hline(yintercept = up_line, linetype = "dashed", color = "black") +
      labs(
        title = paste(disease, "Risk trend for ", station, " Station"),
        x = "Date",
        y = "Risk (%)"
      ) +
      scale_y_continuous(
        labels = percent_format(scale = 100),  # Display as percentages
        breaks = seq(0, 1, by = .20),       # Tick marks every 20%
        limits = c(0, 1)                   # Limit y-axis range to 0–100
      ) +
      # Set colors for Risk_Class categories
      scale_color_manual(values = c("High" = "pink", "Moderated" = "#FFC20A", "Low" = "darkgreen")) +
      # Control x-axis date formatting and frequency
      scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate date labels for readability
      ) +
      guides(color = "none")  # Remove the color legend
  }else{
    ggplot(df, aes(x = date, y = Risk)) +
      geom_line(color = "#0C7BDC") +
      geom_point(aes(color = Risk_Class), size = 4) +  # Map color to Risk_Class
      geom_text(aes(label = Risk_Class),
                vjust = -0.5,
                color = "black",
                size = 5) +
      labs(
        title = paste(disease, "Risk trend for ", station, " Station"),
        x = "Date",
        y = "Risk (%)"
      ) +
      scale_y_continuous(
        labels = percent_format(scale = 100),  # Display as percentages
        breaks = seq(0, 1, by = .20),       # Tick marks every 20%
        limits = c(0, 1)                   # Limit y-axis range to 0–100
      ) +
      # Set colors for Risk_Class categories
      scale_color_manual(values = c("High" = "black", "Medium" = "#FFC20A", "Low" = "darkgreen")) +
      # Control x-axis date formatting and frequency
      scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate date labels for readability
      ) +
      guides(color = "none")
  }
}


############################################################ Functions for the PDF
# Function to ensure location is within bounds
ensure_within_bounds <- function(lat, lon, bounds) {
  lat <- max(min(lat, bounds$max_lat), bounds$min_lat)
  lon <- max(min(lon, bounds$max_lon), bounds$min_lon)
  return(c(lat, lon))
}

# Function to copy required files to a temporary directory
copy_report_files <- function(temp_dir) {
  files_to_copy <- list(
    rmd = c(from = "report_template.Rmd", to = file.path(temp_dir, "report_template.Rmd")),
    header = c(from = "header.tex", to = file.path(temp_dir, "header.tex")),
    logo1 = c(from = "logos/OPENSOURDA_color-flush.png", to = file.path(temp_dir, "OPENSOURDA_color-flush.png")),
    logo2 = c(from = "logos/PLANPATHCO_color-flush.png", to = file.path(temp_dir, "PLANPATHCO_color-flush.png")),
    logo3 = c(from = "logos/DATASCIE_color-flush.png", to = file.path(temp_dir, "DATASCIE_color-flush.png") )
  )
  
  for (item in files_to_copy) {
    if (!file.copy(item["from"], item["to"], overwrite = TRUE)) {
      stop(paste("Failed to copy file:", item["from"]))
    }
  }
}

# Function to prepare Tar Spot data
prepare_tarspot_data <- function(weather_data) {
  if (!is.null(weather_data)) {
    weather_data$tarspot %>%
      mutate(
        Risk = round(Risk, 2),
        date_day = as.Date(date_day, format = "%Y-%m-%d") + 1
      )
  } else {
    stop("Weather data is not available")
  }
}

# Function to get station address
get_station_address <- function(station_code, stations) {
  if (!station_code %in% names(stations)) {
    stop("Invalid station code")
  }
  
  station_info <- stations[[station_code]]
  paste(
    station_info$location,
    station_info$region,
    station_info$state,
    sep = ", "
  )
}

