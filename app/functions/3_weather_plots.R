# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)
library(zoo)
library(lubridate)
library(tidyr)
library(ggplot2)

# This is the endpoint to the pywisconet, a wrapper of Wisconet data https://github.com/UW-Madison-DSI/pywisconet.git
########################################################################################

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
  start_date <- as.POSIXct(start_date, tz = "US/Central")
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
  print("---------------+++++++++++++++-----------------")
  # Make the GET request
  response <- GET(url, query = query_params, add_headers(.headers = headers))
  print(response)
  # Check the response status
  if (status_code(response) == 200){
    # Parse JSON response
    data = fromJSON(rawToChar(response$content))
    data$collection_time_ct <- format(with_tz(data$collection_time, tzone = "US/Central"), "%Y-%m-%d")
    #changed by hour_ct
    #data$hour <- hour(with_tz(ymd_hms(data$collection_time), tzone = "US/Central"))
    
    if(measurement %in% c("AIRTEMP")){
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
    }else if(measurement %in% c("DEW_POINT")){
      data$value <- fahrenheit_to_celsius(data$value)
      daily_aggregations <- data %>%
        group_by(collection_time_ct) %>%
        summarize(
          dew_point_avg_c = mean(value, na.rm = TRUE),
          dew_point_max_c = max(value, na.rm = TRUE),
          dew_point_min_c = min(value, na.rm = TRUE)
        )
      
      daily_aggregations <- daily_aggregations %>%
        mutate(
          dew_point_avg_value_30d_ma = rollmean(dew_point_avg_c, k = moving_average_days, fill = NA, align = "right"),
          dew_point_max_value_30d_ma = rollmean(dew_point_max_c, k = moving_average_days, fill = NA, align = "right"),
          dew_point_min_value_30d_ma = rollmean(dew_point_min_c, k = moving_average_days, fill = NA, align = "right")
        )
    }else if(measurement %in% c("RELATIVE_HUMIDITY")){
      # Step 1: Add aggregations
      print("====== ----------- Data RH ----------- =======")
      print(data %>% select(collection_time, collection_time_ct, hour_ct,
                            value,final_units,measure_type))
      
      # Step 2: Aggregate daily counts of `value >= 90` for relevant periods
      daily_aggregations <- data %>%
        group_by(collection_time_ct) %>% # Group by date
        summarize(
          count_90_8PM_6AM = sum(value >= 90 & hour_ct %in% c(0:6, 8:12), na.rm = TRUE),
          count_90_day = sum(value >= 90 & hour_ct %in% c(6:8), na.rm = TRUE),
          max_rh = max(value, na.rm = TRUE),
          max_rh_8PM_6AM = max(value[hour_ct %in% c(0:6, 8:12)], na.rm = TRUE),
          max_rh_day = max(value[hour_ct %in% c(6:8)], na.rm = TRUE)
        )
      print("Data RH aggregations --------------")
      print(daily_aggregations)
      # Step 3: Compute 30-day moving average
      daily_aggregations <- daily_aggregations %>%
        mutate(
          count_90_8PM_6AM_14d_ma = rollmean(count_90_8PM_6AM, k = 14, fill = NA, align = "right"),
          max_rh_30d_ma = rollmean(max_rh, k = 30, fill = NA, align = "right")
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
color_mapping <- c(
  "Max Temp (°C)" = "#E69F00",  # Orange
  "Min Temp (°C)" = "#56B4E9",  # Sky Blue
  "Avg Temp (°C)" = "#009E73",  # Green
  'count_90_8PM_6AM'="#E69F00",
  'max_rh_8PM_6AM' = "#E69F00",
  'max_rh' = "#56B4E9",
  'max_rh_30d_ma' = "#E69F00",
  "Max Temp (°C) 30D MA" = "#E69F00",  # Orange
  "Min Temp (°C) 30D MA" = "#56B4E9",  # Sky Blue
  "Avg Temp (°C) 30D MA" = "#009E73",   # Green
  'count_90_8PM_6AM_14d_ma'= "#E69F00" 
)

# Define linetypes
linetype_mapping <- c(
  "Max Temp (°C)" = "solid",
  "Min Temp (°C)" = "solid",
  "Avg Temp (°C)" = "solid",
  'count_90_8PM_6AM' = "solid",
  'max_rh_8PM_6AM' = "solid",
  'max_rh' = "solid",
  'max_rh_30d_ma' = "dotted",
  'count_90_8PM_6AM_14d_ma' = "dotted", 
  "Max Temp (°C) 30D MA" = "dotted",
  "Min Temp (°C) 30D MA" = "dotted",
  "Avg Temp (°C) 30D MA" = "dotted",
  'count_90_8PM_6AM_14d_ma'= "dotted"
)

plot_air_temp <- function(data) {
  # Pivot air temperature variables to long format
  air_temp_data <- data %>%
    select(
      collection_time_ct, 
      air_temp_max_c, air_temp_min_c, air_temp_avg_c,
      air_temp_max_value_30d_ma, air_temp_min_value_30d_ma, air_temp_avg_value_30d_ma
    ) %>%
    mutate(
      Date = as.Date(collection_time_ct, format="%Y-%m-%d")
    ) %>%
    rename(
      "Max Temp (°C)" = air_temp_max_c,
      "Min Temp (°C)" = air_temp_min_c,
      "Avg Temp (°C)" = air_temp_avg_c,
      "Max Temp (°C) 30D MA" = air_temp_max_value_30d_ma,
      "Min Temp (°C) 30D MA" = air_temp_min_value_30d_ma,
      "Avg Temp (°C) 30D MA" = air_temp_avg_value_30d_ma
    ) %>%  # Ensure proper conversion to Date
    select(-collection_time_ct) %>%
    pivot_longer(
      cols = c(
        "Max Temp (°C)", "Min Temp (°C)", "Avg Temp (°C)",
        "Max Temp (°C) 30D MA", "Min Temp (°C) 30D MA", "Avg Temp (°C) 30D MA"
      ),
      names_to = "Variable",
      values_to = "Value"
    )
  
  # Create the ggplot
  ggplot(air_temp_data, aes(x = Date, y = Value, color = Variable, linetype = Variable)) +
    geom_line(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
    labs(
      title = "Air Temperature (°C) Trends in the Last 30 Days",
      x = "Date",
      y = "Air Temperature (°C)",
      color = "Variable",
      linetype = "Variable"
    ) +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = linetype_mapping) +
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
}


plot_rh_dp <- function(data) {
  # Select and prepare data for plotting
  rh_dp_data <- data %>%
    select(collection_time_ct, max_rh_8PM_6AM, max_rh, max_rh_30d_ma) %>%
    mutate(
      Date = as.Date(collection_time_ct, format="%Y-%m-%d")  # Convert to Date
    ) %>%
    select(-collection_time_ct) %>%  # Drop original column
    pivot_longer(
      cols = c(max_rh_8PM_6AM, max_rh_30d_ma, max_rh),
      names_to = "Variable",
      values_to = "Value"
    )
  
  # Create the ggplot
  ggplot(rh_dp_data, aes(x = Date, y = Value, color = Variable)) +
    geom_line(size = 1.5) +
    labs(
      title = "Maximum Relative Humidity (%)",
      x = "Date",
      y = "Relative Humidity (%)",
      color = "Variable"
    ) +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = linetype_mapping) +
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)  
    )
}

plot_rh_nh_dp <- function(data) {
  # Select and prepare data for plotting
  rh_dp_data <- data %>%
    select(collection_time_ct, count_90_8PM_6AM_14d_ma, count_90_8PM_6AM) %>%
    mutate(
      Date = as.Date(collection_time_ct, format="%Y-%m-%d")  # Convert to Date
    ) %>%
    select(-collection_time_ct) %>%  # Drop original column
    pivot_longer(
      cols = c(count_90_8PM_6AM_14d_ma, count_90_8PM_6AM),
      names_to = "Variable",
      values_to = "Value"
    )
  
  # Create the ggplot
  ggplot(rh_dp_data, aes(x = Date, y = Value, color = Variable)) +
    geom_line(size = 1.5) +
    labs(
      title = "Total Night hours the Relative Humidity (%) was above 90%",
      x = "Date",
      y = "Total Night hours",
      color = "Variable"
    ) +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = linetype_mapping) +
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
}

