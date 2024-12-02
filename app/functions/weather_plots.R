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
# This is the endpoint to the pywisconet, a wrapper of Wisconet data https://github.com/UW-Madison-DSI/pywisconet.git
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
  date_utc_minus_31 <- start_date - days(38)

  
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
  
  # Make the GET request
  response <- GET(url, query = query_params, add_headers(.headers = headers))
  
  # Check the response status
  if (status_code(response) == 200){
    # Parse JSON response
    data = fromJSON(rawToChar(response$content))
    data$hour <- hour(ymd_hms(data$collection_time))
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
      # Step 1: Add time_period column
      
      data <- data %>%
        mutate(
          time_period = case_when(
            hour >= 20 | hour <= 6 ~ "nigth_hours",
            TRUE ~ "day_hours"
          )
        )
      
      print("====== ----------- Data RH ----------- =======")
      print(data %>% select(collection_time, collection_time_ct, hour,
                            value,final_units,
                            time_period, measure_type))
      
      # Step 2: Aggregate daily counts of `value >= 90` for relevant periods
      daily_aggregations <- data %>%
        group_by(collection_time_ct) %>% # Group by date
        summarize(
          count_90_8PM_6AM = sum(value >= 90 & time_period %in% c("nigth_hours"), na.rm = TRUE),
          count_90_day = sum(value >= 90 & time_period %in% c("day_hours"), na.rm = TRUE),
          max_rh = max(value, na.rm = TRUE),
          max_rh_8PM_6AM = max(value[time_period %in% c("nigth_hours")], na.rm = TRUE),
          max_rh_day = max(value[time_period %in% c("day_hours")], na.rm = TRUE)
        )
      
      print(daily_aggregations)
      
      # Step 3: Compute 30-day moving average
      daily_aggregations <- daily_aggregations %>%
        mutate(
          count_90_8PM_6AM_14d_ma = rollmean(count_90_8PM_6AM, 
                                             k = 14, fill = NA, 
                                             align = "right")
        )
    }
    
    print("-------------------Response Data:-----------------------")
    print(daily_aggregations, n=50)
    
  } else {
    # Handle errors
    print(paste("Error:", status_code(response)))
    #print(content(response, as = "text"))
    daily_aggregations=NULL
    data=NULL
  }
  return(list(daily_aggregations=daily_aggregations,
         data=data))
}

########################################################################################

plot_air_temp <- function(data) {
  # Pivot air temperature variables to long format
  air_temp_data <- data %>%
    select(
      collection_time_ct,
      air_temp_max_c, air_temp_min_c, air_temp_avg_c,
      air_temp_avg_value_30d_ma, air_temp_max_value_30d_ma, air_temp_min_value_30d_ma
    ) %>%
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
    theme_minimal() +
    theme(legend.position = "bottom")
}


plot_rh_dp <- function(data) {
  # Select and prepare data for plotting
  rh_dp_data <- data %>%
    select(collection_time_ct, max_rh_8PM_6AM, max_rh, max_rh_day) %>%
    rename(Date = collection_time_ct) %>% # Rename for clarity
    pivot_longer(cols = c(max_rh_8PM_6AM, max_rh_day, max_rh), names_to = "Variable", values_to = "Value") # Pivot to long format
  
  print("+++++++++++++++++++++++++++++++++++")
  print(rh_dp_data)
  
  # Create the ggplot
  ggplot(rh_dp_data, aes(x = Date, y = Value, color = Variable)) +
    geom_line() +
    labs(
      title = "Relative Humidity (%)",
      x = "Date",
      y = "Relative Humidity (%)",
      color = "Variable"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}


