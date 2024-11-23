library(httr)      # For API requests
library(jsonlite)  # For parsing JSON
library(dplyr)     # For data manipulation
library(purrr)
library(lubridate)


base_url <- 'https://wisconet.wisc.edu'




logistic_f <- function(logit) {
  probability<-exp(logit) / (1 + exp(logit))
  return(probability)
}


calculate_tarspot_risk <- function(meanAT, maxRH, rh90_night_tot) {
  logit_LR4 <- 32.06987 - (0.89471 * meanAT) - (0.14373 * maxRH)
  logit_LR6 <- 20.35950 - (0.91093 * meanAT) - (0.29240 * rh90_night_tot)
  logit_values <- c(logit_LR4, logit_LR6)
  probability <- sapply(logit_values, logistic_f)
  ensemble_prob <- mean(probability)
  
  class <- if (ensemble_prob < 0.2) {
    "low"
  } else if (ensemble_prob < 0.35) {
    "moderate"
  } else {
    "high"
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

################################### wisconet stations
current_wisconet_stations <- function(input_date) {
  # Validate the input_date
  if (is.null(input_date)) {
    input_date <- Sys.Date()
  } else {
    input_date <- as.Date(input_date)
  }
  
  tryCatch({
    # API URL
    
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

      # Filter stations where earliest_api_date < input_date - 30 days
      print("Number of stations ")
      print(nrow(filtered_data))
      
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

##################### Daily measurements
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
      # Convert earliest_api_date to Date format
      print("Low date")
      print(low_date)
      print(input_date)
      
      # Filter stations where earliest_api_date < input_date - 30 days
      print("Number of stations ")
      print(nrow(filtered_data))
      
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
        air_temp_min_c_21d_ma = rollmean(air_temp_min_c, k = 21, fill = NA, align = "right"),
        air_temp_avg_c_30d_ma = rollmean(air_temp_avg_c, k = 30, fill = NA, align = "right"),
        rh_max_30d_ma = rollmean(rh_max, k = 30, fill = NA, align = "right"),
        dp_min_30d_c_ma = rollmean(min_dp_c, k = 30, fill = NA, align = "right")
      )
    
    print('---------------------------------------- Here in RH---------')
    print(result_df)
    print('---------------------------------------- Here in RH---------')
    # Return the closest row
    current_time <- Sys.time()
    result_df <- result_df %>%
      arrange(desc(collection_time)) %>%
      #arrange(abs(difftime(collection_time, current_time, units = "secs"))) %>%
      slice_head(n = 1) %>%
      select(o_collection_time, 
             air_temp_min_c_21d_ma, 
             air_temp_avg_c_30d_ma, 
             rh_max_30d_ma, 
             dp_min_30d_c_ma)
    
    print('---------------------------------------- 222Here in RH---------')
    print(result_df)
    print('---------------------------------------- 222Here in RH---------')
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
    print("response from wisconet 60min relative")
    
    print(response)
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
    
    print("+++++ +++++ +++++ +++++ --- +++++ +++++ +++++ +++++")
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
    
    
    print(result_df)
    
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
      
      #arrange(abs(difftime(collection_time, current_time, units = "secs"))) %>%
      #%>%
      #select(collection_time, air_temp_avg_c_30d_ma, rh_max_30d_ma)
    print(daily_rh_above_90)
    print("----------------------- +++++ --- +++++ ---------------------------------")

    return(daily_rh_above_90 %>% select(adjusted_date, rh_above_90_daily_14d_ma))
  } else {
    print(paste("Error: ", response$status_code))
    return(NULL)
  }
}

library(dplyr)
library(jsonlite)

# formating dfor api
convert_to_api_output <- function(dataframe) {
  if ("tarspot_risk" %in% colnames(dataframe) && "tarspot_risk_class" %in% colnames(dataframe)) {
    dataframe <- dataframe %>%
      select(
        station_id,
        forecasting_date.x,
        station_name.x,
        air_temp_avg_c_30d_ma,
        rh_max_30d_ma,
        rh_above_90_daily_14d_ma,
        tarspot_risk,
        tarspot_risk_class
      )
  } else if ("gls_risk" %in% colnames(dataframe) && "gls_risk_class" %in% colnames(dataframe)) {
    dataframe <- dataframe %>%
      select(
        station_id,
        forecasting_date.x,
        station_name.x,
        air_temp_min_c_21d_ma,
        dp_min_30d_c_ma,
        gls_risk,
        gls_risk_class
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


#allstations <- current_wisconet_stations(input_date = '2024-08-01')
#stations <- allstations %>% filter(station_id == "ANGO")

retrieve_tarspot_all_stations <- function(input_date,
                                          input_station_id, 
                                          disease_name = 'tarspot') {
  # Example usage
  allstations <- current_wisconet_stations(input_date = input_date)
  print(nrow(allstations))
  if (!is.null(input_station_id)) {
    stations <- allstations %>% filter(station_id == input_station_id)
    print(stations)
  } else {
    stations <- allstations
  }
  
  # Corrected condition: use nrow instead of nrows
  if (nrow(stations) == 0) {
    print("The selected station was not available to forecasting, please choose another one")
    return(NULL)
  } else {
    # Fetch and process daily data
    daily_enriched <- stations %>%
      mutate(daily_data = map(station_id, ~ api_call_wisconet_data_daily(.x, input_date))) %>%
      filter(!map_lgl(daily_data, is.null)) %>%
      unnest_wider(daily_data)
    
    print(daily_enriched)
    
    # Fetch and process RH data
    rh_enriched <- stations %>%
      mutate(rh_nh_data = map(station_id, ~ api_call_wisconet_data_rh(.x, input_date))) %>%
      filter(!map_lgl(rh_nh_data, is.null)) %>%
      unnest_wider(rh_nh_data)
    
    # Join the results
    enriched_stations <- daily_enriched %>% left_join(rh_enriched, by = "station_id")
    
    print("------------------ COLNAMES ------------------------")
    print(colnames(enriched_stations))
    
    # Enrich the dataset with tarspot or GLS risk based on `disease_name`
    if (disease_name == 'tarspot') {
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
      write.csv(enriched_stations1, "materials/enriched_stations.csv", row.names = FALSE)
      
      print("Filtered and enriched stations data:")
      print(enriched_stations1)
      print(colnames(enriched_stations))
      print(enriched_stations1 %>% select(station_id, forecasting_date.x, station_name.x, 
                                          air_temp_avg_c_30d_ma, 
                                          rh_max_30d_ma, rh_above_90_daily_14d_ma,
                                          tarspot_risk, tarspot_risk_class))
      api_output <- convert_to_api_output(enriched_stations1)
      
      # print result JSON
      return(list(stations_risk = api_output,
                  status = 200,
                  disease_name = disease_name)) 
    }
    
    if (disease_name == 'gls') {
      enriched_stations1 <- enriched_stations %>%
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
      
      print("Filtered and enriched stations data:")
      print(enriched_stations1)
      print(colnames(enriched_stations))
      
      print(enriched_stations1 %>% select(station_id, forecasting_date.x, station_name.x, 
                                          air_temp_min_c_21d_ma, dp_min_30d_c_ma,
                                          gls_risk, gls_risk_class))
      
      api_output <- convert_to_api_output(enriched_stations1)
      
      # output
      return(list(stations_risk = api_output,
                  status = 200,
                  disease_name = disease_name))
    }
    
    #sporecaster
  }  
}



retrieve_tarspot_all_stations('2024-11-01', 'ANGO', 'gls')
# Example
#api_call_wisconet_data_daily('ANGO', '2024-11-01')
#api_call_wisconet_data_rh('ANGO', '2024-11-01')