library(ggplot2)
library(httr)
library(tidyr)

api_call_wisconet_data_daily <- function(station, date_str) {
  endpoint <- paste0('/api/v1/stations/', station, '/measures')
  
  date_obj <- as.Date(date_str, format = "%Y-%m-%d")
  
  # Subtract and add 35 days
  date_minus_35 <- date_obj - 35

  # Convert to Unix timestamp
  unix_minus_35 <- as.numeric(as.POSIXct(date_minus_35, tz = "UTC"))
  
  params <- list(
    end_time = unix_timestamp,
    start_time = unix_minus_35,
    fields = 'daily_air_temp_f_max,daily_air_temp_f_min,daily_relative_humidity_pct_max,daily_dew_point_f_min,daily_wind_speed_mph_avg'
  )
  
  # Make API request
  response <- GET(url = paste0(base_url, endpoint), query = params)
  print(response)
  
  if (response$status_code == 200) {
    data1 <- fromJSON(content(response, as = "text"), flatten = TRUE)
    data <- data1$data
    if (nrow(data) == 0) return(NULL)  # Handle no data
    
    # Process collection times
    ctime <- as.POSIXct(data$collection_time, origin = "1970-01-01", tz = "UTC")
    collection_time_chicago <- with_tz(ctime, tzone = "America/Chicago")
    
    # Prepare results
    result_df <- data.frame(
      o_collection_time = ctime,
      collection_time = collection_time_chicago,
      air_temp_max_f = NA,
      air_temp_min_f = NA,
      rh_max = NA,
      min_dp = NA
    )
    
    # Populate measures
    for (i in seq_along(data$measures)) {
      measures <- data$measures[[i]]
      for (j in seq_len(nrow(measures))) {
        if (measures[j, 1] == 4) result_df$air_temp_max_f[i] <- measures[j, 2]
        if (measures[j, 1] == 6) result_df$air_temp_min_f[i] <- measures[j, 2]
        if (measures[j, 1] == 20) result_df$rh_max[i] <- measures[j, 2]
        if (measures[j, 1] == 12) result_df$min_dp_f[i] <- measures[j, 2]
      }
    }
    
    # Convert temperatures from Fahrenheit to Celsius
    result_df <- result_df %>%
      mutate(
        min_dp_c = fahrenheit_to_celsius(min_dp_f),
        air_temp_max_c = fahrenheit_to_celsius(air_temp_max_f),
        air_temp_min_c = fahrenheit_to_celsius(air_temp_min_f),
        air_temp_avg_c = fahrenheit_to_celsius((air_temp_max_f + air_temp_min_f) / 2) # Avoid rowMeans for clarity
      )
    
    # Calculate 30-day and 21-day moving averages
    result_df <- result_df %>%
      mutate(
        air_temp_max_c_30d_ma = rollmean(air_temp_max_c, k = 30, fill = NA, align = "right"),
        air_temp_min_c_21d_ma = rollmean(air_temp_min_c, k = 21, fill = NA, align = "right"),
        air_temp_avg_c_30d_ma = rollmean(air_temp_avg_c, k = 30, fill = NA, align = "right"),
        rh_max_30d_ma = rollmean(rh_max, k = 30, fill = NA, align = "right"),
        dp_min_30d_c_ma = rollmean(min_dp_c, k = 30, fill = NA, align = "right")
      )
    return(result_df)
  }else{
    return(NULL)
  }
}

plot_air_temp <- function(data) {
  # Pivot air temperature variables to long format
  air_temp_data <- data %>%
    select(collection_time, air_temp_max_c, air_temp_min_c, air_temp_avg_c) %>%
    pivot_longer(cols = starts_with("air_temp"), names_to = "Variable", values_to = "Value")
  
  # Create the ggplot
  ggplot(air_temp_data, aes(x = collection_time, y = Value, color = Variable)) +
    geom_line() +
    labs(
      title = "Air Temperature Trends",
      x = "Date",
      y = "Temperature (°C)",
      color = "Variable"
    ) +
    theme_minimal()
}

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
