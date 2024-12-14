library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(lubridate)
library(zoo)
library(tidyr)

# Logistic function to convert logits to probabilities
logistic_f <- function(logit) {
  return(exp(logit) / (1 + exp(logit)))
}

# Tarspot risk calculation
calculate_tarspot_risk_function <- function(meanAT, maxRH, rh90_night_tot) {
  logit_LR4 <- 32.06987 - (0.89471 * meanAT) - (0.14373 * maxRH)
  logit_LR6 <- 20.35950 - (0.91093 * meanAT) - (0.29240 * rh90_night_tot)
  probabilities <- c(logistic_f(logit_LR4), logistic_f(logit_LR6))
  ensemble_prob <- mean(probabilities, na.rm = TRUE)  # Handle NAs gracefully
  
  # Handle NA in ensemble_prob
  if (is.na(ensemble_prob)) {
    return(c(tarspot_risk = NA, tarspot_risk_class = "No class"))
  }
  
  # Determine risk class based on the ensemble probability
  if (ensemble_prob < 0.2) {
    risk_class <- "Low"
  } else if (ensemble_prob > 0.35) {
    risk_class <- "High"
  } else {
    risk_class <- "Moderate"
  }
  
  return(list(tarspot_risk = ensemble_prob, tarspot_risk_class = risk_class))
}

# Gray leaf spot risk calculation
calculate_gray_leaf_spot_risk_function <- function(minAT21, minDP30) {
  logit_gls <- -2.9467 - (0.03729 * minAT21) + (0.6534 * minDP30)
  prob <- logistic_f(logit_gls)
  
  if (is.na(prob)) {
    return(c(gls_risk = NA, gls_risk_class = "No class"))
  }
  
  # Determine risk class based on probability
  if (prob < 0.2) {
    risk_class <- "Low"
  } else if (prob > 0.6) {
    risk_class <- "High"
  } else {
    risk_class <- "Moderate"
  }
  
  return(list(gls_risk = prob, gls_risk_class = risk_class))
}

# Non-irrigated sporec risk calculation
calculate_non_irrigated_risk <- function(maxAT30MA, maxWS30MA) {
  logit_nirr <- (-0.47 * maxAT30MA) - (1.01 * maxWS30MA) + 16.65
  ensemble_prob <- logistic_f(logit_nirr)
  if (is.na(ensemble_prob)) {
    return(c(sporec_nirr_risk = NA, sporec_nirr_risk_class = NA))
  }
  return(list(sporec_nirr_risk = ensemble_prob, sporec_nirr_risk_class = "NoClass"))
}

# Irrigated sporec risk calculation
calculate_irrigated_risk <- function(maxAT30MA, maxRH30MA) {
  logit_irr_30 <- (-2.38 * 1) + (0.65 * maxAT30MA) + (0.38 * maxRH30MA) - 52.65
  prob_logit_irr_30 <- logistic_f(logit_irr_30)
  
  logit_irr_15 <- (-2.38 * 0) + (0.65 * maxAT30MA) + (0.38 * maxRH30MA) - 52.65
  prob_logit_irr_15 <- logistic_f(logit_irr_15)
  if (is.na(prob_logit_irr_15)) {
    return(c(prob_logit_irr_15 = NA, sporec_irr_15in_risk = NA))
  }
  return(list(sporec_irr_30in_risk = prob_logit_irr_30, sporec_irr_15in_risk = prob_logit_irr_15))
}

# Frogeye leaf spot risk calculation
calculate_frogeye_leaf_spot_function <- function(maxAT30, rh80tot30) {
  logit_fe <- -5.92485 - (0.1220 * maxAT30) + (0.1732 * rh80tot30)
  prob_logit_fe <- logistic_f(logit_fe)
  if (is.na(prob_logit_fe)) {
    return(c(prob_logit_fe = NA, prob_logit_fe_class = "No class"))
  }
  if (prob_logit_fe < 0.5) {
    risk_class <- "Low"
  } else if (prob_logit_fe > 0.6) {
    risk_class <- "High"
  } else {
    risk_class <- "Moderate"
  }
  
  return(list(fe_risk = prob_logit_fe, fe_risk_class = risk_class))
}

# Function to fetch and process data for any station_id and date
get_station_data <- function(station_id, start_date) {
  # Define base URL and convert start_date to POSIXct
  base_url <- "https://connect.doit.wisc.edu/pywisconet_wrapper/bulk_measures/"
  start_date <- as.POSIXct(start_date, tz = "US/Central")
  date_utc_minus_31 <- start_date - days(38)  # 31 days before the start date
  
  # Construct the URL
  url <- paste0(base_url, station_id)
  
  # Define query parameters for daily data
  query_params <- list(
    start_date = format(date_utc_minus_31, "%Y-%m-%d"),
    end_date = format(start_date, "%Y-%m-%d"),
    measurements = 'ALL',
    frequency = "DAILY"
  )
  
  # Optional headers (add Authorization if needed)
  headers <- c(
    "accept" = "application/json"
  )
  
  # Fetch the daily data
  response_daily <- GET(url, query = query_params, add_headers(.headers = headers))
  data_daily <- fromJSON(rawToChar(response_daily$content))
  
  # Process the daily data
  result_df_daily <- data_daily %>%
    select(collection_time_ct, standard_name, value) %>%
    pivot_wider(
      id_cols = c(collection_time_ct), 
      names_from = standard_name,               
      values_from = value                            
    ) %>%
    ungroup()
  
  # Convert to Celsius
  fahrenheit_to_celsius <- function(fahrenheit) {
    return((fahrenheit - 32) * 5 / 9)
  }
  
  daily_aggregations <- result_df_daily %>%
    select(collection_time_ct, daily_air_temp_f_max, daily_air_temp_f_avg, daily_air_temp_f_min,
           daily_dew_point_f_min, daily_relative_humidity_pct_max, daily_wind_speed_mph_max) %>%
    mutate(
      date = as.Date(collection_time_ct),
      air_temp_avg_value_30d_ma = rollmean(fahrenheit_to_celsius(daily_air_temp_f_avg), k = 30, fill = NA, align = "right"),
      air_temp_max_value_30d_ma = rollmean(fahrenheit_to_celsius(daily_air_temp_f_max), k = 30, fill = NA, align = "right"),
      air_temp_min_value_30d_ma = rollmean(fahrenheit_to_celsius(daily_air_temp_f_min), k = 21, fill = NA, align = "right"),
      daily_relative_humidity_pct_max_30d_ma = rollmean(daily_relative_humidity_pct_max, k = 30, fill = NA, align = "right"),
      daily_dew_point_f_min_30d_ma = rollmean(fahrenheit_to_celsius(daily_dew_point_f_min), k = 30, fill = NA, align = "right"),
      daily_wind_speed_mph_max_30d_ma = rollmean(daily_wind_speed_mph_max, k = 30, fill = NA, align = "right")
    )
  
  # Define query parameters for hourly data
  query_params_60min <- list(
    start_date = format(date_utc_minus_31, "%Y-%m-%d"),
    end_date = format(start_date, "%Y-%m-%d"),
    measurements = 'RELATIVE_HUMIDITY',
    frequency = "MIN60"
  )
  
  # Fetch the hourly data
  response_hrly <- GET(url, query = query_params_60min, add_headers(.headers = headers))
  data_response_hrly <- fromJSON(rawToChar(response_hrly$content))
  
  # Process the hourly data
  result_df_hrly <- data_response_hrly %>%
    select(collection_time_ct, standard_name, value) %>%
    pivot_wider(
      id_cols = c(collection_time_ct),
      names_from = standard_name,
      values_from = value
    ) %>%
    ungroup()
  
  result_df_hrly$collection_time_ct <- as.POSIXct(result_df_hrly$collection_time_ct, tz = "UTC")
  
  # Create the variables for hour and date
  result_df_hrly <- result_df_hrly %>%
    mutate(
      hour = hour(collection_time_ct),
      date = as.Date(collection_time_ct)
    )
  
  # Aggregating hourly data
  aggregated_data_hr <- result_df_hrly %>%
    group_by(date) %>%
    summarise(
      night_hours_above_90 = sum((hour >= 0 & hour < 6 | hour >= 20) & (`60min_relative_humidity_pct_avg` >= 90), na.rm = TRUE),
      total_hours_above_80 = sum(`60min_relative_humidity_pct_avg` >= 80, na.rm = TRUE)
    )
  
  # Aggregating the hourly data with 14-day and 30-day rolling averages
  aggregated_data_hr <- aggregated_data_hr %>%
    # Ensure the columns are numeric (explicitly cast to numeric)
    mutate(
      total_hours_above_80 = as.numeric(total_hours_above_80),
      night_hours_above_90 = as.numeric(night_hours_above_90)    
    ) %>%
    # Apply the rollmean function after ensuring the columns are numeric
    mutate(
      night_hours_above_90_14d_ma = rollapply(night_hours_above_90, width = 14, FUN = mean, fill = NA, align = "right"),
      total_hours_above_80_30d_ma = rollapply(total_hours_above_80, width = 30, FUN = mean, fill = NA, align = "right")
    )
  
  # Merge the daily and hourly aggregations
  result <- daily_aggregations %>%
    left_join(aggregated_data_hr, by = "date")
  
  # Assuming you have the following risk calculation functions defined:
  # - calculate_tarspot_risk_function
  # - calculate_gray_leaf_spot_risk_function
  # - calculate_non_irrigated_risk
  # - calculate_irrigated_risk
  # - calculate_frogeye_leaf_spot_function
  
  print("OK --- result ")
  
  # Using purrr::pmap_dfr to apply multiple risk functions in one step
  risk_df <- result %>% 
    filter(!is.na(air_temp_avg_value_30d_ma)) %>%  # Filter out rows with NA in air_temp_avg_value_30d_ma
    mutate(
      # Applying tarspot risk calculation
      tarspot_risk = purrr::pmap_dfr(
        list(air_temp_avg_value_30d_ma, daily_relative_humidity_pct_max_30d_ma, night_hours_above_90_14d_ma), 
        calculate_tarspot_risk_function
      ),
      # Applying gray leaf spot risk calculation
      gls_risk = purrr::pmap_dfr(
        list(air_temp_min_value_30d_ma, daily_dew_point_f_min_30d_ma), 
        calculate_gray_leaf_spot_risk_function
      ),
      # Applying non-irrigated sporec risk calculation
      sporec_nirr_risk = purrr::pmap_dfr(
        list(air_temp_max_value_30d_ma, daily_wind_speed_mph_max), 
        calculate_non_irrigated_risk
      ),
      # Applying irrigated sporec risk calculation
      sporec_irr_risk = purrr::pmap_dfr(
        list(air_temp_max_value_30d_ma, daily_relative_humidity_pct_max_30d_ma), 
        calculate_irrigated_risk
      ),
      # Applying frogeye leaf spot risk calculation
      fe_risk = purrr::pmap_dfr(
        list(air_temp_max_value_30d_ma, total_hours_above_80_30d_ma), 
        calculate_frogeye_leaf_spot_function
      )
    )
  
  # View the resulting data
  print(colnames(risk_df))
  return(risk_df)
}

# Example usage
station_id <- 'ALTN'
start_date <- '2024-12-12'

result <- get_station_data(station_id, start_date)
head(result)
colnames(result)
print(result%>%select(date, tarspot_risk, gls_risk,
                      sporec_nirr_risk, sporec_irr_risk, fe_risk), n=50)