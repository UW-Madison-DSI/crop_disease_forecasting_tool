library(httr)      # For API requests
library(jsonlite)  # For parsing JSON
library(dplyr)     # For data manipulation
library(purrr)
library(lubridate)
library(zoo)
library(tidyr)
library(dplyr)
library(jsonlite)

#source("R/logit_functions.R")
logistic_f <- function(logit) {
  probability<-exp(logit) / (1 + exp(logit))
  return(probability)
}

fahrenheit_to_celsius <- function(fahrenheit) {
  celsius <- (fahrenheit - 32) * 5 / 9
  return(celsius)
}

calculate_tarspot_risk_function <- function(meanAT, maxRH, rh90_night_tot) {
  logit_LR4 <- 32.06987 - (0.89471 * meanAT) - (0.14373 * maxRH)
  logit_LR6 <- 20.35950 - (0.91093 * meanAT) - (0.29240 * rh90_night_tot)
  logit_values <- c(logit_LR4, logit_LR6)
  probability <- sapply(logit_values, logistic_f)
  ensemble_prob <- mean(probability)
  
  class <- if (ensemble_prob < 0.2) {
    "low"
  } else if (ensemble_prob > 0.35) {
    "high"
  } else {
    "moderate"
  }
  
  return(list(tarspot_risk = ensemble_prob, tarspot_risk_class = class))
}

calculate_frogeye_leaf_spot_function <- function(maxAT30, rh80tot30) {
  # Logistic regression formula, no rrigation needed
  logit_fe <- -5.92485 -(0.1220 * maxAT30) + (0.1732 * rh80tot30)
  prob_logit_fe <- logistic_f(logit_fe)
  
  class <- if (prob_logit_fe < 0.5) {
    "low"
  } else if (prob_logit_fe > 0.6) {
    "high"
  } else {
    "moderate"
  }
  
  # Calculate risk using the general disease risk function
  return(list(
    fe_risk = prob_logit_fe,
    fe_risk_class = class
  ))
}

calculate_non_irrigated_risk <- function(maxAT30MA, maxWS30MA) {
  # Logistic regression formula for non-irrigated model
  logit_nirr <- (-0.47 * maxAT30MA) - (1.01 * maxWS30MA) + 16.65
  ensemble_prob <- logistic_f(logit_nirr)
  
  return(list(sporec_nirr_risk = ensemble_prob, sporec_nirr_risk_class = "NoClass"))
}

# Irrigated Sporecaster Risk
calculate_irrigated_risk <- function(maxAT30MA, maxRH30MA) {
  # Logistic regression formula for irrigated model
  logit_irr_30 <- (-2.38 *1) + (0.65 * maxAT30MA) + (0.38 * maxRH30MA) - 52.65
  prob_logit_irr_30 <- logistic_f(logit_irr_30)
  
  logit_irr_15 <- (-2.38 * 0) + (0.65 * maxAT30MA) + (0.38 * maxRH30MA) - 52.65
  prob_logit_irr_15 <- logistic_f(logit_irr_15)
  
  return(list(sporec_irr_30in_risk = prob_logit_irr_30, 
              sporec_irr_15in_risk = prob_logit_irr_15))
}

calculate_sporecaster_risk <- function(maxAT30MA, maxWS30MA, maxRH30MA){
  # un used yet
  sporec_irr_risk = calculate_irrigated_risk(maxAT30MA, maxRH30MA)
  sporec_no_irr_risk = calculate_non_irrigated_risk(maxAT30MA, maxWS30MA)
  return(list(sporec_irr_30in_risk = sporec_irr_risk$sporec_irr_30in_risk, 
              sporec_irr_15in_risk = sporec_irr_risk$sporec_irr_15in_risk,
              sporec_no_irr_risk = sporec_no_irr_risk$sporec_nirr_risk))
}

############################################################# wisconet stations
library(httr)      # For API requests
library(jsonlite)  # For parsing JSON
library(dplyr)     # For data manipulation
library(purrr)
library(lubridate)
library(zoo)
library(tidyr)
library(dplyr)
library(jsonlite)

current_wisconet_stations <- function(input_date) {
  if (is.null(input_date)) {
    input_date <- Sys.Date()
  } else {
    input_date <- as.Date(input_date)
  }
  
  # This is the endpoint from pywisconet: https://github.com/UW-Madison-DSI/pywisconet.git
  url <- paste0("https://connect.doit.wisc.edu/pywisconet_wrapper/all_stations/31?start_date=",input_date)
  
  # Perform the GET request
  response <- GET(url)
  
  # Check the status of the response
  if (status_code(response) == 200) {
    # Parse the content if the request is successful
    #content_data <- content(response, as = "text", encoding = "UTF-8")
    data <- fromJSON(rawToChar(content(response)))
    print("Request successful! Here is the data:")
    data <- data %>% filter(!station_id %in% c('WNTEST1', 'MITEST1')) %>% select(
      station_id, station_name, latitude, longitude, 
      city, county, location, region, state,
      station_timezone, earliest_api_date, days_active)
    return(data)
  } else {
    print(paste("Request failed with status code:", status_code(response)))
    return(NULL)
  }
}
allstations <- current_wisconet_stations(NULL)
#########################
api_call_weather_data <- function(station_id, 
                                  start_date){
  
  # Define start and end dates in UTC
  start_date <- as.POSIXct(start_date, tz = "UTC")
  date_utc_minus_31 <- start_date - days(30)
  
  
  # Construct the full URL
  url <- paste0(base_url, station_id)
  # Define query parameters
  query_params <- list(
    start_date = format(date_utc_minus_31, "%Y-%m-%d"),
    end_date = format(start_date, "%Y-%m-%d"),
    measurements = "ALL",
    frequency = "MIN60"
  )
  
  headers <- c(
    "accept" = "application/json"
  )
  
  # Make the GET request
  response <- GET(url, query = query_params, add_headers(.headers = headers))
  
  # Check the response status
  if (status_code(response) == 200){
    # Parse JSON response
    data = fromJSON(rawToChar(response$content))
    data <- data %>%
      filter(standard_name!='60min_wind_speed_mph_max') %>%
      mutate(date_ct = as.POSIXct(collection_time_ct, format = "%Y-%m-%d"),
             RH_90_night_condition = ifelse(measure_type == "Relative Humidity" & value >= 90 & hour_ct %in% c(0:6, 8:12), 1, 0),
             RH_80_allday_condition = ifelse(measure_type == "Relative Humidity" & value >= 80, 1, 0),
             value_c = ifelse(measure_type %in% c("Air Temp", "Dew Point"), fahrenheit_to_celsius(value), value)
             )
    
    print("Vertical data ------------")
    print(data)
    #fahrenheit_to_celsius(data$value_c) if 
    daily_stats1 <- data %>%
      group_by(date_ct, measure_type) %>%
      summarise(
        daily_min = min(value_c, na.rm = TRUE),
        daily_max = max(value_c, na.rm = TRUE),
        daily_avg = mean(value_c, na.rm = TRUE),
        RH_90_sum = sum(RH_90_night_condition, na.rm = TRUE),  # Sum of rows RH >= 90 for hours 0:6, 8:12
        RH_80_sum = sum(RH_80_allday_condition, na.rm = TRUE)   # Sum of rows RH >= 80
      ) %>%
      pivot_wider(
        names_from = measure_type,
        values_from = c(daily_min, daily_max, daily_avg, RH_80_sum, RH_90_sum)
        #values_fill = 0  # Fill missing values
      ) %>%
      ungroup()
    print(daily_stats1)
    
    daily_stats <- daily_stats1 %>%
      arrange(date_ct) %>%  # Ensure the data is sorted by date
      mutate(
        `mean_AT_c_30d_ma` = rollmean((`daily_avg_Air Temp`), 30, fill = NA, align = "right"),
        `max_RH_30d_ma` = rollmean(`daily_max_Relative Humidity`, 30, fill = NA, align = "right"),
        `min_DP_c_30d_ma` = rollmean((`daily_min_Dew Point`), 30, fill = NA, align = "right"),
        `max_AT_c_30d_ma` = rollmean((`daily_max_Air Temp`), 30, fill = NA, align = "right"),
        `Tot_RH_h_80_30d_ma` = rollmean(`RH_80_sum_Relative Humidity`, 30, fill = NA, align = "right"),
        `max_WS_30d_ma` = rollmean(`daily_max_Wind Speed`, 30, fill = NA, align = "right"),
        `Tot_RH_nh_14d_ma` = rollmean(`RH_90_sum_Relative Humidity`, 14, fill = NA, align = "right"),
        `min_AT_c_21d_ma` = rollmean((`daily_min_Air Temp`), 21, fill = NA, align = "right")
      )
    
    # View the result
    print(colnames(daily_stats))
    return(list(last_date_data = daily_stats%>%filter(!is.na(mean_AT_c_30d_ma)),
                weather_data30d = daily_stats))
  }else{
    return(NULL)
  }
}



res_data <- api_call_weather_data("ALTN", '2024-11-01')
res_data
######################################## Main logic
library(dplyr)
library(tidyr)

calculate_gray_leaf_spot_risk_function <- function(minAT21, 
                                                   minDP30) {
  prob <- logistic_f(-2.9467-(0.03729 * minAT21) + (0.6534 * minDP30))
  
  class <- if (prob < 0.2) {
    "low"
  } else if (prob > 0.6) {
    "high"
  } else {
    "moderate"
  }
  
  return(list(gls_risk = prob, gls_risk_class = class))
}

disease_forecasting_given_station_date <- function(input_station_id, input_date) {
  # Fetch weather data
  res_data <- api_call_weather_data(input_station_id, input_date)$last_date_data
  print(colnames(res_data))
  # Ensure required columns exist
  
  # Calculate risks
  res_data <- res_data %>%
    mutate(
      tarspot_risk = pmap_dbl(
        list(mean_AT_c_30d_ma, max_RH_30d_ma, Tot_RH_nh_14d_ma),
        ~ calculate_tarspot_risk_function(..1, ..2, ..3)$tarspot_risk
      ),
      gls_risk = pmap_dbl(
        list(min_AT_c_21d_ma, min_DP_c_30d_ma),
        ~ calculate_gray_leaf_spot_risk_function(..1, ..2)$gls_risk
      ),
      fe_ls_risk = pmap_dbl(
        list(max_AT_c_30d_ma, Tot_RH_h_80_30d_ma),
        ~ calculate_frogeye_leaf_spot_function(..1, ..2)$fe_risk
      )
  )
  
  # Sporecaster risk calculations (optimize repeated calls)
  sporecaster_results <- pmap(
    list(res_data$max_AT_c_30d_ma, res_data$max_WS_30d_ma, res_data$Tot_RH_h_80_30d_ma),
    calculate_sporecaster_risk
  )
  
  res_data <- res_data %>%
    mutate(
      sporec_irr_30in_risk = map_dbl(sporecaster_results, "sporec_irr_30in_risk"),
      sporec_irr_15in_risk = map_dbl(sporecaster_results, "sporec_irr_15in_risk"),
      sporec_no_irr_risk = map_dbl(sporecaster_results, "sporec_no_irr_risk")
    )

  return(res_data)
}

res_data <- disease_forecasting_given_station_date("ALTN", '2024-11-01')
res_data
compute_forecast <- function(station_id, input_date) {
  tryCatch({
    # Call the forecasting function for a specific station
    res_data <- disease_forecasting_given_station_date(station_id, input_date)
    print(res_data)
    # Select relevant columns
    selected_data <- res_data %>%
      select(
        date_ct, mean_AT_c_30d_ma, max_RH_30d_ma, Tot_RH_nh_14d_ma,
        min_AT_c_21d_ma, min_DP_c_30d_ma, tarspot_risk, gls_risk,fe_ls_risk,
        sporec_irr_30in_risk, sporec_irr_15in_risk, sporec_no_irr_risk
      )
    
    return(selected_data)
  }, error = function(e) {
    message(paste("Error processing station:", station_id, " - ", e$message))
    return(data.frame())
  })
}

res_data <- compute_forecast("ALTN", '2024-11-01')
res_data
###########################

retrieve_tarspot_all_stations <- function(input_date,
                                          input_station_id) {
  
  allstations <- current_wisconet_stations(input_date = input_date)
  
  if (!is.null(input_station_id)) {
    stations <- allstations %>% filter(station_id == input_station_id)
    print(stations)
  } else {
    stations <- allstations
  }
  print("------------------------- stations")
  print(stations)
  N<-nrow(stations)
  
  if (N == 0) {
    print("The selected station was not available to forecasting, please choose another one")
    return(NULL)
  } else {
    return(NULL)
  }  
}

retrieve_tarspot_all_stations('2024-11-01', "ALTN")

############################################## api output
convert_to_api_output <- function(dataframe, disease_name) {
  if (disease_name=="tarspot") {
    dataframe <- dataframe %>%
      select(
        station_id,location,
        latitude, longitude, region, state,
        earliest_api_date,
        forecasting_date,
        station_name,
        air_temp_avg_c_30d_ma,
        rh_max_30d_ma,
        rh_above_90_daily_14d_ma,
        tarspot_risk,
        tarspot_risk_class
      )
  } else if (disease_name=="gls") {
    dataframe <- dataframe %>%
      select(
        station_id,location,
        latitude, longitude, region, state,
        earliest_api_date,
        forecasting_date,
        station_name,
        air_temp_min_c_21d_ma,
        dp_min_30d_c_ma,
        gls_risk,
        gls_risk_class
      )
  } else if (disease_name=="sporecaster-irr"){
    dataframe <- dataframe %>%
      select(
        station_id,location,
        latitude, longitude, region, state,
        earliest_api_date,
        forecasting_date,
        station_name,
        air_temp_max_c_30d_ma, 
        rh_max_30d_ma,
        sporec_irr_30in_risk, 
        sporec_irr_15in_risk
      )
  } else if (disease_name=='frogeye_leaf_spot'){
    dataframe <- dataframe %>%
      select(station_id,location,
             latitude, longitude, region, state,
             earliest_api_date,
             forecasting_date,
             station_name,
             air_temp_avg_c_30d_ma, 
             rh_above_80_daily_30d_ma,
             frogeye_risk, 
             frogeye_risk_class)
  }else {
    stop("Error: Missing disease name input.")
  }
  
  # Convert to JSON
  dataframe %>%
    as_tibble() %>%
    split(1:nrow(.)) %>%
    toJSON(auto_unbox = TRUE, pretty = TRUE)
}