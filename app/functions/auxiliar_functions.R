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


################################################################ My functions
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

################################################################ Call Tarspot 
## Using the logic of the model
logistic <- function(logit) {
  exp(logit) / (1 + exp(logit))
}

# Function to classify risk based on probability and thresholds
classify_risk <- function(probability, medium_threshold, high_threshold) {
  cat("\n Risk Class: ",medium_threshold,high_threshold, probability) 
  if (probability<=0.0){
    return ("NoRisk")
  }else if (high_threshold >= 1 & 1<=medium_threshold) {
    return("NoRiskClass")
  }else if (probability > 0 & probability<medium_threshold) {
    return("Low")
  } else if (probability >= medium_threshold & probability<=high_threshold) {
    return("Medium")
  } else if (probability > high_threshold){
    return("High")
  }
}

calculate_tarspot_risk <- function(meanAT, maxRH, rh90_night_tot) {
  # Logistic regression formulas for the two models, no irrigation total needed
  logit_LR4 <- 32.06987 - (0.89471 * meanAT) - (0.14373 * maxRH) #paper page5
  logit_LR6 <- 20.35950 - (0.91093 * meanAT) - (0.29240 * rh90_night_tot) #paper page5
  probabilities <- sapply(c(logit_LR4, logit_LR6), logistic)
  ensemble_prob <- mean(probabilities)
  
  # Calculate risk using the general disease risk function
  return(ensemble_prob)
}


roll_mean <- function(vec, width) {
  zoo::rollapply(vec, width, \(x) mean(x, na.rm = T), fill = NA, partial = T)
}



################################################################ Function to plot the data
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

#################### Risk trend
plot_trend1 <- function(df, station){
  ggplot(df, aes(x = Date, y = Risk)) +
    geom_line(color = "#0C7BDC") +
    geom_point(color = "#FFC20A", size = 4) +
    geom_text(aes(label = Risk_Class),
              vjust = -0.5,
              color = "black",
              size = 5) +
    labs(title = paste(station$name, "Station,", station$region, "Region,", station$state),
         x = "Date",
         y = "Probability of Tar Spot (%)") +
    scale_y_continuous(labels = percent_format(scale = 1),
                       breaks = seq(0, 100, by = 25)) +
    
    # Control x-axis date formatting and frequency
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate date labels for readability
    )
}


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

############################################################ Functions for thhe PDF
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

