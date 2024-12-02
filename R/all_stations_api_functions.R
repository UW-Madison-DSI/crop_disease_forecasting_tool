library(httr)      # For API requests
library(jsonlite)  # For parsing JSON
library(dplyr)     # For data manipulation
library(purrr)
library(lubridate)
library(zoo)
library(tidyr)
library(dplyr)
library(jsonlite)


source("R/logit_functions.R")


base_url <- 'https://wisconet.wisc.edu'


############################################################# wisconet stations
current_wisconet_stations <- function(input_date) {
  # Validate the input_date
  if (is.null(input_date)) {
    input_date <- Sys.Date()
  } else {
    input_date <- as.Date(input_date)
  }
  
  tryCatch({
    # Make the GET request
    response <- GET('https://wisconet.wisc.edu/api/v1/stations/', add_headers(Accept = "application/json"))
    
    # Check if the request was successful
    if (status_code(response) == 200) {
      # Parse the JSON response
      data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      low_date <- as.Date(input_date- 31, format = "%Y-%m-%d")
      filtered_data <- data %>% mutate(earliest_api_date = as.Date(earliest_api_date, format = "%Y-%m-%d"),
                                       forecasting_date = input_date) %>% 
        filter((earliest_api_date <= low_date) & (!station_id %in% c('WNTEST1', 'MITEST1')))
      
      
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



############################################################# Daily measurements from Wisconet
api_call_wisconet_data_daily <- function(station, end) {
  endpoint <- paste0('/api/v1/stations/', station, '/measures')
  
  end_date <- as.POSIXct(end, tz = "UTC")
  start_date <- end_date %m-% days(35)
  
  params <- list(
    end_time = as.numeric(end_date),
    start_time = as.numeric(start_date),
    fields = 'daily_air_temp_f_max,daily_air_temp_f_min,daily_relative_humidity_pct_max,daily_dew_point_f_min,daily_wind_speed_mph_avg'
  )
  
  # Make API request
  response <- GET(url = paste0(base_url, endpoint), query = params)
  data1 <- data = fromJSON(rawToChar(response$content))
  print(response)
  
  if (response$status_code == 200) {
    data1 <- data = fromJSON(rawToChar(response$content))
    #fromJSON(content(response, as = "text"), flatten = TRUE)
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

    
    # Return the closest row
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
                                      end) {
  endpoint <- paste0('/api/v1/stations/', station, '/measures')
  
  end_date <- as.POSIXct(end, tz = "UTC")
  start_date <- end_date %m-% days(35)
  
  
  tryCatch({
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
        
        rh_night_above_90 = if_else(rh_avg >= 90 & (hour >= 20 | hour <= 6), 1, 0),
        rh_night_above_80 = if_else(rh_avg >= 80, 1, 0)
      ) %>%
      arrange(desc(adjusted_date))
    
    # Group by date and sum the counts of night hours where RH >= 90 for each day
    daily_rh_above_90 <- result_df %>%
      group_by(adjusted_date) %>%
      summarise(hours_rh_above_90 = sum(rh_night_above_90, na.rm = TRUE),
                hours_rh_above_80 = sum(rh_night_above_80, na.rm = TRUE),) %>%
      ungroup() 
    daily_rh_above_90$rh_above_90_daily_14d_ma <- rollmean(daily_rh_above_90$hours_rh_above_90,
                                                           k = 14, fill = NA,
                                                           align = "right")
    daily_rh_above_90$rh_above_80_daily_30d_ma <- rollmean(daily_rh_above_90$hours_rh_above_80,
                                                           k = 30, fill = NA,
                                                           align = "right")
    # Calculate 14-day rolling mean for RH >= 90 hours
    daily_rh_above_90 <- daily_rh_above_90%>%
      arrange(desc(adjusted_date)) %>% slice_head(n = 1)
    
    return(daily_rh_above_90 %>% select(adjusted_date, rh_above_90_daily_14d_ma, rh_above_80_daily_30d_ma))
  } else{
    print(paste("Error:", scode))
    print(content(response, as = "text", encoding = "UTF-8"))  # Log detailed error
    return(NULL)
  }
}

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

######################################## Main logic
retrieve_tarspot_all_stations <- function(input_date,
                                          input_station_id, 
                                          disease_name = 'tarspot') {
  
  allstations <- current_wisconet_stations(input_date = input_date)
  
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
    
    if (disease_name %in% c('tarspot', 'frogeye_leaf_spot')) {
      # Fetch and process RH data for tarspot
      rh_enriched <- stations %>%
        mutate(rh_nh_data = map(station_id, ~ api_call_wisconet_data_rh(.x, input_date))) %>%
        filter(!map_lgl(rh_nh_data, is.null)) %>%
        unnest_wider(rh_nh_data) %>% select(station_id, rh_above_90_daily_14d_ma, rh_above_80_daily_30d_ma)
      
      # Join the datasets
      enriched_stations <- daily_enriched %>% left_join(rh_enriched, 
                                                        by = "station_id")
      
      if (disease_name=='tarspot'){
        enriched_stations1 <- enriched_stations %>%
          filter(
            !is.na(air_temp_avg_c_30d_ma) & 
              !is.na(rh_max_30d_ma) & 
              !is.na(rh_above_90_daily_14d_ma)
          ) %>%
          mutate(
            tarspot_results = pmap(
              list(air_temp_avg_c_30d_ma, rh_max_30d_ma, rh_above_90_daily_14d_ma),
              ~ calculate_tarspot_risk_function(..1, ..2, ..3)
            )
          ) %>%
          mutate(
            tarspot_risk = map_dbl(tarspot_results, "tarspot_risk"),
            tarspot_risk_class = map_chr(tarspot_results, "tarspot_risk_class")
          ) %>%
          select(-tarspot_results)
      }else{
        enriched_stations1 <- enriched_stations %>%
          filter(
            !is.na(air_temp_avg_c_30d_ma) & 
              !is.na(rh_above_80_daily_30d_ma)
          ) %>%
          mutate(
            frogeye_leaf_spot_results = pmap(
              list(air_temp_avg_c_30d_ma, rh_above_80_daily_30d_ma),
              ~ calculate_frogeye_leaf_spot_function(..1, ..2)
            )
          ) %>%
          mutate(
            frogeye_risk = map_dbl(frogeye_leaf_spot_results, "fe_risk"),
            frogeye_risk_class = map_chr(frogeye_leaf_spot_results, "fe_risk_class")
          ) %>%
          select(-frogeye_leaf_spot_results)
        }
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
            ~ calculate_gray_leaf_spot_risk_function(..1, ..2)
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