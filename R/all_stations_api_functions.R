library(httr)      # For API requests
library(jsonlite)  # For parsing JSON
library(dplyr)     # For data manipulation
library(purrr)
library(lubridate)
library(zoo)
library(tidyr)

base_url <- 'https://wisconet.wisc.edu'



logistic_f <- function(logit) {
  probability<-exp(logit) / (1 + exp(logit))
  return(probability)
}

fahrenheit_to_celsius <- function(fahrenheit) {
  celsius <- (fahrenheit - 32) * 5 / 9
  return(celsius)
}

calculate_tarspot_risk <- function(meanAT, maxRH, rh90_night_tot) {
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

calculate_gray_leaf_spot_risk <- function(minAT21, 
                                          minDP30) {
  logit_GLS <- -2.9467-(0.03729 * minAT21) + (0.6534 * minDP30)
  ensemble_prob <- logistic_f(logit_GLS)
  
  class <- if (ensemble_prob < 0.2) {
    "low"
  } else if (ensemble_prob > 0.6) {
    "high"
  } else {
    "moderate"
  }
  
  return(list(gls_risk = ensemble_prob, gls_risk_class = class))
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
  logit_irr_30 <- (-2.38) + (0.65 * maxAT30MA) + (0.38 * maxRH30MA) - 52.65
  prob_logit_irr_30 <- logistic_f(logit_irr_30)
  
  logit_irr_15 <- (0.65 * maxAT30MA) + (0.38 * maxRH30MA) - 52.65
  prob_logit_irr_15 <- logistic_f(logit_irr_15)
  
  return(list(sporec_irr_30in_risk = prob_logit_irr_30, 
              sporec_irr_15in_risk = prob_logit_irr_15))
}

################################################################################ Wisconet Stations
current_wisconet_stations <- function(input_date) {
  # Validate the input_date
  if (is.null(input_date)) {
    input_date <- Sys.Date()
  } else {
    input_date <- as.Date(input_date)
  }
  
  tryCatch({
    # Make the GET request
    stations_url <- 'https://wisconet.wisc.edu/api/v1/stations/'
    response <- GET(stations_url, add_headers(Accept = "application/json"))
    
    # Check if the request was successful
    if (status_code(response) == 200) {
      # Parse the JSON response
      data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      low_date <- as.Date(input_date- 31, format = "%Y-%m-%d")
      
      filtered_data <- data %>% mutate(earliest_api_date = as.Date(earliest_api_date, format = "%m/%d/%Y"),
                                       forecasting_date = input_date) %>% filter((earliest_api_date <= low_date)
                                                  & (!station_id %in% c('WNTEST1', 'MITEST1')))
      
      return(filtered_data)
    } else {
      # Return an error if the response status code is not 200
      return(list(error = paste("HTTP request failed with status code:", status_code(response))))
    }
  }, error = function(e) {
    # Handle any errors that occur during the API request or processing
    return(list(error = conditionMessage(e)))
  })
}

############################################# Daily measurements
current_wisconet_stations1 <- function(input_date) {
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

##################### Daily measurements
api_call_wisconet_data_daily <- function(station, end_time) {
  endpoint <- paste0('/api/v1/stations/', station, '/measures')
  
  
  end_date <- as.POSIXct(end_time, tz = "UTC")
  start_date <- end_date %m-% days(35)
  
  # Convert to epoch times
  params <- list(
    end_time = as.numeric(end_date),
    start_time = as.numeric(start_date),
    fields = 'daily_air_temp_f_max,daily_air_temp_f_min,daily_relative_humidity_pct_max,daily_dew_point_f_min'
  )
  
  # Make API request
  response <- GET(url = paste0(base_url, endpoint), query = params)
  
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
    
    # Convert to Celsius and calculate averages
    result_df$min_dp_c <- fahrenheit_to_celsius(result_df$min_dp_f)
    result_df$air_temp_max_c <- fahrenheit_to_celsius(result_df$air_temp_max_f)
    result_df$air_temp_min_c <- fahrenheit_to_celsius(result_df$air_temp_min_f)
    result_df$air_temp_avg_c <- fahrenheit_to_celsius(rowMeans(result_df[c("air_temp_max_f", "air_temp_min_f")], na.rm = TRUE))
    
    # Calculate 30-day moving averages
    result_df <- result_df %>%
      mutate(
        air_temp_max_c_30d_ma = rollmean(air_temp_max_c, k = 30, fill = NA, align = "right"),
        air_temp_min_c_21d_ma = rollmean(air_temp_min_c, k = 21, fill = NA, align = "right"),
        air_temp_avg_c_30d_ma = rollmean(air_temp_avg_c, k = 30, fill = NA, align = "right"),
        rh_max_30d_ma = rollmean(rh_max, k = 30, fill = NA, align = "right"),
        dp_min_30d_c_ma = rollmean(min_dp_c, k = 30, fill = NA, align = "right")
      )
    
    # Return the closest row
    current_time <- Sys.time()
    result_df <- result_df %>%
      arrange(desc(collection_time)) %>%
      slice_head(n = 1) %>%
      select(o_collection_time, 
             air_temp_max_c_30d_ma,
             air_temp_min_c_21d_ma, 
             air_temp_avg_c_30d_ma, 
             rh_max_30d_ma, 
             dp_min_30d_c_ma)
    
    return(result_df)
  } else {
    warning(paste("Error fetching data for station:", station, "with status code:", response$status_code))
    return(NULL)
  }
}

################################
api_call_wisconet_data_rh <- function(station,# start_time, 
                                      end_time) {
  tryCatch({
    endpoint <- paste0('/api/v1/stations/', station, '/measures')
    
    end_date <- as.POSIXct(end_time, tz = "UTC")
    start_date <- end_date %m-% days(18)
    
    # Convert to epoch times
    params <- list(
      end_time = as.numeric(end_date),
      start_time = as.numeric(start_date),
      fields = '60min_relative_humidity_pct_avg'
    )
    
    response <- GET(url = paste0(base_url, endpoint), query = params)
    scode <- response$status_code
  }, error = function(e) {
    stop(paste("Failed to render report:", e$message))
  }) 
  
  if (scode == 200) {
    data1 <- fromJSON(content(response, as = "text"), flatten = TRUE)
    data <- data1$data
    
    ctime <- as.POSIXct(data$collection_time, origin = "1970-01-01")
    collection_time_chicago <- with_tz(ctime, tzone = "America/Chicago")
    
    # Create the result data frame
    result_df <- data.frame(
      o_collection_time = ctime,
      collection_time = collection_time_chicago,
      rh_avg = NA,  # Placeholder for relative humidity values
      stringsAsFactors = FALSE
    )
    
    # Process measures to get '60min_relative_humidity_pct_avg'
    for (i in seq_along(data$measures)) {
      measures <- data$measures[[i]]
      for (j in seq_len(length(measures))) {
        result_df$rh_avg[i] <- measures[[j]][1]
      }
    }
    
    # Add a new column that counts the night hours where RH >= 90
    result_df <- result_df %>%
      mutate(
        hour = hour(collection_time),  # Extract hour
        collection_time_ct = with_tz(collection_time, tzone = "America/Chicago"),
        # Adjust date: only hours between 00:00 and 06:00 should belong to the previous day
        # Adjust date: If the hour is between 00:00 and 06:00, assign to the previous day
        adjusted_date = if_else(hour >= 0 & hour <= 6, 
                                floor_date(collection_time_ct - days(1), unit = "day"),  # Subtract 1 day for early morning hours
                                floor_date(collection_time_ct, unit = "day")),
        
        rh_night_above_90 = if_else(rh_avg >= 90 & (hour >= 20 | hour <= 6), 1, 0)
      ) %>%
      arrange(desc(adjusted_date))
    
    # Group by date and sum the counts of night hours where RH >= 90 for each day
    daily_rh_above_90 <- result_df %>%
      group_by(adjusted_date) %>%
      summarise(hours_rh_above_90 = sum(rh_night_above_90, na.rm = TRUE)) %>%
      ungroup() 
    daily_rh_above_90$rh_above_90_daily_14d_ma <- rollmean(daily_rh_above_90$hours_rh_above_90,
                                                           k = 14, fill = NA,
                                                           align = "right")
    
    # Calculate 14-day rolling mean for RH >= 90 hours
    daily_rh_above_90 <- daily_rh_above_90%>%
      arrange(desc(adjusted_date)) %>% slice_head(n = 1)
    
    return(daily_rh_above_90 %>% select(adjusted_date, rh_above_90_daily_14d_ma))
  } else {
    print(paste("Error: ", response$status_code))
    return(NULL)
  }
}

library(dplyr)
library(jsonlite)

# formating dfor api
convert_to_api_output <- function(dataframe, disease_name) {
  if (disease_name=="tarspot") {
    dataframe <- dataframe %>%
      select(
        station_id,
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
        station_id,
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
        station_id,
        latitude, longitude, region, state,
        earliest_api_date,
        forecasting_date,
        station_name,
        air_temp_max_c_30d_ma, 
        rh_max_30d_ma,
        sporec_irr_30in_risk, 
        sporec_irr_15in_risk
      )
  } else {
    stop("Error: Columns for either 'tarspot' or 'gls' risks are missing.")
  }
  
  # Convert to JSON
  dataframe %>%
    as_tibble() %>%
    split(1:nrow(.)) %>%
    toJSON(auto_unbox = TRUE, pretty = TRUE)
}


retrieve_tarspot_all_stations <- function(input_date,
                                          input_station_id, 
                                          disease_name = 'tarspot') {
  
  allstations <- current_wisconet_stations(input_date = input_date)
  print(nrow(allstations))
  
  if (!is.null(input_station_id)) {
    stations <- allstations %>% filter(station_id == input_station_id)
    print(stations)
  } else {
    stations <- allstations
  }
  
  N<-nrow(stations)
  
  if (N == 0) {
    print("The selected station was not available to forecasting, please choose another one")
    return(NULL)
  } else {
    # Fetch and process daily data eg max, mi, avg air temp, dp, others
    daily_enriched <- stations %>%
      mutate(daily_data = map(station_id, ~ api_call_wisconet_data_daily(.x, input_date))) %>%
      filter(!map_lgl(daily_data, is.null)) %>%
      unnest_wider(daily_data)
    
    if (disease_name == 'tarspot') {
      # Fetch and process RH data for tarspot
      rh_enriched <- stations %>%
        mutate(rh_nh_data = map(station_id, ~ api_call_wisconet_data_rh(.x, input_date))) %>%
        filter(!map_lgl(rh_nh_data, is.null)) %>%
        unnest_wider(rh_nh_data) %>% select(station_id, rh_above_90_daily_14d_ma)
      
      # Join the datasets
      enriched_stations <- daily_enriched %>% left_join(rh_enriched, 
                                                        by = "station_id")
      
      enriched_stations1 <- enriched_stations %>%
        filter(
          !is.na(air_temp_avg_c_30d_ma) & 
            !is.na(rh_max_30d_ma) & 
            !is.na(rh_above_90_daily_14d_ma)
        ) %>%
        mutate(
          tarspot_results = pmap(
            list(air_temp_avg_c_30d_ma, rh_max_30d_ma, rh_above_90_daily_14d_ma),
            ~ calculate_tarspot_risk(..1, ..2, ..3)
          )
        ) %>%
        mutate(
          tarspot_risk = map_dbl(tarspot_results, "tarspot_risk"),
          tarspot_risk_class = map_chr(tarspot_results, "tarspot_risk_class")
        ) %>%
        select(-tarspot_results)  # Remove intermediate list column
    }
    
    if (disease_name == 'gls') {
      enriched_stations1 <- daily_enriched %>%
        filter(
          !is.na(air_temp_min_c_21d_ma) & 
            !is.na(dp_min_30d_c_ma)
        ) %>%
        mutate(
          gls_results = pmap(
            list(air_temp_min_c_21d_ma, dp_min_30d_c_ma),
            ~ calculate_gray_leaf_spot_risk(..1, ..2)
          )
        ) %>%
        mutate(
          gls_risk = map_dbl(gls_results, "gls_risk"),
          gls_risk_class = map_chr(gls_results, "gls_risk_class")
        ) %>%
        select(-gls_results)
    }
    
    #sporecaster calculate_irrigated_risk <- function(maxAT30MA, maxRH30MA)
    if (disease_name == 'sporecaster-irr') {
      enriched_stations1 <- daily_enriched %>%
        filter(
          !is.na(air_temp_max_c_30d_ma) & 
            !is.na(rh_max_30d_ma)
        ) %>%
        mutate(
          sporecast_irr_results = pmap(
            list(air_temp_max_c_30d_ma, rh_max_30d_ma),
            ~ calculate_irrigated_risk(..1, ..2)
          )
        ) %>%
        mutate(
          sporec_irr_30in_risk = map_dbl(sporecast_irr_results, "sporec_irr_30in_risk"),
          sporec_irr_15in_risk = map_dbl(sporecast_irr_results, "sporec_irr_15in_risk")
        ) %>%
        select(-sporecast_irr_results)
    }
    
    api_output <- convert_to_api_output(enriched_stations1, disease_name)
    
    # Output
    return(list(stations_risk = api_output,
                n_stations = N,
                status = 200,
                disease_name = disease_name))
  }  
}