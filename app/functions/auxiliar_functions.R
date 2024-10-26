# Load necessary packages
library(lubridate)
library(httr)
library(jsonlite)
library(zoo)
library(ggplot2)
library(dplyr)  # Load dplyr for the pipe operator
library(httr)
library(scales)

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


plot_trend <- function(df, station){
  ggplot(df, aes(x = Date, y = Risk)) +
    geom_line(color = "#0C7BDC") +
    geom_point(color = "#FFC20A") +
    geom_text(aes(label = Risk_Class),
              vjust = -0.5,
              color = "black") +
    labs(title = paste(station$name, "Station,", station$region, "Region,", station$state),
         x = "Date",
         y = "Probability of Tarspot (%)") +
    scale_y_continuous(labels = percent_format(scale = 1)) +  # Formats y-axis as percentages
    theme_minimal()
}