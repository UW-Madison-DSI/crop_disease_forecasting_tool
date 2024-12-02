# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)
library(zoo)
library(lubridate)
library(tidyr)
library(tidyr)
library(ggplot2)

########################################################################################
# Define the API endpoint and station ID
base_url <- "https://connect.doit.wisc.edu/pywisconet_wrapper/bulk_measures/"


# Temperature conversion function
fahrenheit_to_celsius <- function(fahrenheit) {
  (fahrenheit - 32) * 5 / 9
}

api_call_weather_data <- function(station_id, 
                                  start_date,
                                  measurement, 
                                  frequency,
                                  moving_average_days) {
  
  # Define start and end dates in UTC
  start_date <- as.POSIXct(start_date, tz = "UTC")
  date_utc_minus_31 <- start_date - days(31)
  
  # Print debug information
  print(paste("End Date (UTC):", start_date))
  print(paste("Start Date (UTC):", date_utc_minus_31))
  print(paste("Station ID:", station_id))
  
  # Construct the full URL
  url <- paste0(base_url, station_id)
  # Define query parameters
  query_params <- list(
    start_date = format(date_utc_minus_31, "%Y-%m-%d"),
    end_date = format(start_date, "%Y-%m-%d"),
    measurements = measurement,
    frequency = "MIN60"
  )
  
  # Optional headers (add Authorization if needed)
  headers <- c(
    "accept" = "application/json"
  )
  
  # Print full URL and parameters for debugging
  print(paste("Full URL:", url))
  print("Query Parameters:")
  print(query_params)
  
  # Make the GET request
  response <- GET(url, query = query_params, add_headers(.headers = headers))
  
  # Check the response status
  if (status_code(response) == 200){
    # Parse JSON response
    data = fromJSON(rawToChar(response$content))
    data$collection_time_ct <- with_tz(data$collection_time, tzone = "America/Chicago")
    if(measurement %in% c("AIRTEMP", "DEW_POINT")){
      data$value <- fahrenheit_to_celsius(data$value)
      daily_aggregations <- data %>%
        group_by(collection_time_ct) %>%
        summarize(
          air_temp_avg_c = mean(value, na.rm = TRUE),
          air_temp_max_c = max(value, na.rm = TRUE),
          air_temp_min_c = min(value, na.rm = TRUE)
        )
      daily_aggregations <- daily_aggregations %>%
        mutate(
          air_temp_avg_value_30d_ma = rollmean(air_temp_avg_c, k = moving_average_days, fill = NA, align = "right"),
          air_temp_max_value_30d_ma = rollmean(air_temp_max_c, k = moving_average_days, fill = NA, align = "right"),
          air_temp_min_value_30d_ma = rollmean(air_temp_min_c, k = moving_average_days, fill = NA, align = "right")
        )
    }else if(measurement %in% c("RELATIVE_HUMIDITY")){
      data <- data %>%
        mutate(
          time_period = case_when(
            hour(collection_time_ct) >= 20 | hour(collection_time_ct) < 6 ~ "8PM-6AM",
            hour(collection_time_ct) >= 12 & hour(collection_time_ct) < 18 ~ "12PM-6PM",
            TRUE ~ "Other"
          )
        )
      daily_aggregations <- data %>%
        filter(time_period %in% c("8PM-6AM", "12PM-6PM")) %>%  # Filter for relevant periods
        group_by(collection_time_ct) %>%
        summarize(
          count_90_8PM_6AM = sum(value >= 90 & time_period %in% c("8PM-6AM", "12PM-6PM"), na.rm = TRUE)
        )
      daily_aggregations <- daily_aggregations %>%
        mutate(
          count_90_8PM_6AM_30d_ma = rollmean(count_90_8PM_6AM, k = moving_average_days, fill = NA, align = "right")
        )
    }
    
    print("-------------------Response Data:-----------------------")
    print(daily_aggregations, n=32)
    
  } else {
    # Handle errors
    print(paste("Error:", status_code(response)))
    #print(content(response, as = "text"))
    daily_aggregations=NULL
  }
  return(daily_aggregations)
}


plot_air_temp <- function(data) {
  # Pivot air temperature variables to long format
  air_temp_data <- data %>%
    select(collection_time_ct, air_temp_max_c, air_temp_min_c, air_temp_avg_c,
           air_temp_avg_value_30d_ma, air_temp_max_value_30d_ma, air_temp_min_value_30d_ma) %>%
    rename(Date = collection_time_ct) %>%
    pivot_longer(
      cols = starts_with("air_temp"),
      names_to = "Variable",
      values_to = "Value"
    )
  
  # Create the ggplot
  ggplot(air_temp_data, aes(x = Date, y = Value, color = Variable)) +
    geom_line() +
    labs(
      title = "Air Temperature Trends in the last 30 days",
      x = "Date",
      y = "Temperature (°C)",
      color = "Variable"
    ) +
    theme_minimal()+
    theme(legend.position = "bottom")
}


########################################################################################
plot_rh_dp <- function(data) {
  # Pivot relative humidity and dew point variables to long format
  rh_dp_data <- data %>%
    select(collection_time, rh_max, dp_min_30d_c_ma) %>%
    pivot_longer(cols = c(rh_max, dp_min_30d_c_ma), names_to = "Variable", values_to = "Value")
  
  # Create the ggplot
  ggplot(rh_dp_data, aes(x = collection_time, y = Value, color = Variable)) +
    geom_line() +
    labs(
      title = "Relative Humidity and Dew Point Trends",
      x = "Date",
      y = "Value",
      color = "Variable"
    ) +
    theme_minimal()
}

plot_dew_point <- function(data) {
  # Ensure the required columns are present
  if (!all(c("collection_time", "min_dp_c", "dp_min_30d_c_ma") %in% colnames(data))) {
    stop("The data does not contain required columns: 'collection_time', 'min_dp_c', or 'dp_min_30d_c_ma'")
  }
  
  # Create the ggplot
  ggplot(data, aes(x = collection_time)) +
    geom_line(aes(y = min_dp_c, color = "Dew Point (°C)")) + 
    geom_line(aes(y = dp_min_30d_c_ma, color = "30-Day Moving Avg (°C)"), linetype = "dashed") +
    labs(
      title = "Dew Point Trends",
      x = "Date",
      y = "Dew Point (°C)",
      color = "Legend"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}
