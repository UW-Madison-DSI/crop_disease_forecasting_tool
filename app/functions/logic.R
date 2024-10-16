# Load necessary packages
library(lubridate)
library(httr)
library(jsonlite)
library(zoo)
library(ggplot2)
library(dplyr)  # Load dplyr for the pipe operator
library(httr)



################################################################ Dates
# Function to convert Fahrenheit to Celsius
fahrenheit_to_celsius <- function(temp_f) {
  (temp_f - 32) * 5/9
}

# Function to convert current time to GMT and subtract a number of months
from_ct_to_gmt <- function(current_time, mo){
  # Subtract months from the current time in Central Time
  past_time_ct <- current_time - months(mo)
  
  cat('current: CT', current_time, mo, 'months ago CT', past_time_ct, '\n')
  
  # Convert both dates to Unix timestamps in GMT
  start_time <- as.integer(as.POSIXct(past_time_ct, tz = "GMT"))
  end_time <- as.integer(as.POSIXct(current_time, tz = "GMT"))
  
  cat('Start time (GMT):', start_time, 'End time (GMT):', end_time, '\n')
  
  return(list(
    start_time_gmt = start_time,
    end_time_gmt = end_time
  ))
}

################################################################ Preparation

base_url <- 'https://wisconet.wisc.edu'
url_ts <- "https://connect.doit.wisc.edu/forecasting_crop_disease"


current <- Sys.time()
today_ct <- with_tz(current, tzone = "America/Chicago")

# Example: 2 months ago
mo <- 6
out <- from_ct_to_gmt(today_ct, mo)
# Convert both dates to Unix timestamps in GMT
start_time <- out$start_time_gmt
end_time <- out$end_time_gmt

cat(start_time, end_time)

################################################################ Function to get weather data from the API
api_call_wisconet_data <- function(station) {
  endpoint <- paste0('/api/v1/stations/', station, '/measures')
  
  params <- list(
    end_time = end_time,
    start_time = start_time,
    fields = 'daily_air_temp_f_max,daily_air_temp_f_min,daily_relative_humidity_pct_max'
  )
  
  response <- GET(url = paste0(base_url, endpoint), query = params)
  
  if (response$status_code == 200) {
    data1 <- fromJSON(content(response, as = "text"), flatten = TRUE)
    data <- data1$data
    
    # Create the result data frame
    result_df <- data.frame(
      collection_time = as.POSIXct(data$collection_time, origin = "1970-01-01"),
      air_temp_max_f = NA,
      air_temp_min_f = NA,
      rh_max = NA,
      stringsAsFactors = FALSE
    )
    
    # Process measures
    for (i in seq_along(data$measures)) {
      measures <- data$measures[[i]]
      for (j in seq_len(nrow(measures))) {
        if (measures[j, 1] == 4) result_df$air_temp_max_f[i] <- measures[j, 2]
        if (measures[j, 1] == 6) result_df$air_temp_min_f[i] <- measures[j, 2]
        if (measures[j, 1] == 20) result_df$rh_max[i] <- measures[j, 2]
      }
    }
    
    # Convert Fahrenheit to Celsius
    result_df$air_temp_max_c <- fahrenheit_to_celsius(result_df$air_temp_max_f)
    result_df$air_temp_min_c <- fahrenheit_to_celsius(result_df$air_temp_min_f)
    
    # Calculate average temperature in both Fahrenheit and Celsius
    result_df$air_temp_avg_f <- rowMeans(result_df[c("air_temp_max_f", "air_temp_min_f")], na.rm = TRUE)
    result_df$air_temp_avg_c <- fahrenheit_to_celsius(result_df$air_temp_avg_f)
    
    result_df <- result_df %>% arrange(collection_time)
    
    # Calculate 30-day moving averages
    result_df$air_temp_min_c_30d_ma <- rollmean(result_df$air_temp_min_c, k = 30, fill = NA, align = "right")
    result_df$air_temp_max_c_30d_ma <- rollmean(result_df$air_temp_max_c, k = 30, fill = NA, align = "right")
    result_df$air_temp_avg_c_30d_ma <- rollmean(result_df$air_temp_avg_c, k = 30, fill = NA, align = "right")
    result_df$rh_max_30d_ma <- rollmean(result_df$rh_max, k = 30, fill = NA, align = "right")
    
    # Rearrange columns for better readability
    result_df <- result_df[c("collection_time", "air_temp_avg_c", "air_temp_min_c_30d_ma",
                             "air_temp_max_c_30d_ma", "air_temp_avg_c_30d_ma",
                             "rh_max_30d_ma", "rh_max")]
    
    current_time <- Sys.time()
    result_df1 <- result_df %>%
      arrange(abs(difftime(collection_time, current_time, units = "secs"))) %>%  # Sort by proximity to current date
      slice(1)
    
    
    return(result_df1)
  } else {
    print(paste("Error: ", response$status_code))
    return(NULL)
  }
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

# Function to fetch and plot data
fetch_at <- function(station) {
  
  # Fetch the data using the API function
  data_df <- api_call_wisconet_data(station)
  
  # Plot the data if it is not NULL
  if (!is.null(data_df)) {
    return(data_df)
  } else {
    cat("No data returned for the specified station.\n")
    return(NULL)
  }
}

################################################################ Function to get 60-minute relative humidity data
api_call_wisconet_data_rh <- function(station) {
  
  endpoint <- paste0('/api/v1/stations/', station, '/measures')
  
  params <- list(
    end_time = end_time,
    start_time = start_time,
    fields = '60min_relative_humidity_pct_avg'
  )
  
  response <- GET(url = paste0(base_url, endpoint), query = params)
  
  if (response$status_code == 200) {
    data1 <- fromJSON(content(response, as = "text"), flatten = TRUE)
    data <- data1$data
    
    # Create the result data frame
    result_df <- data.frame(
      collection_time = as.POSIXct(data$collection_time, origin = "1970-01-01"),
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
    
    # Filter rows where RH > 90% and within night hours (20:00-06:00)
    result_df <- result_df %>%
      mutate(hour = hour(collection_time)) %>%
      filter(rh_avg >= 90 & (hour >= 20 | hour <= 6))
    
    # Group by day and count the number of hours where RH > 90 for each day
    daily_rh_above_90 <- result_df %>%
      mutate(date = as.Date(collection_time)) %>%
      group_by(date) %>%
      summarise(hours_rh_above_90 = n())

    return(list(
      data = result_df,
      daily_rh_above_90 = daily_rh_above_90
    ))
  } else {
    print(paste("Error: ", response$status_code))
    return(NULL)
  }
}

################################################################ Function to fetch and print the number of Night hours with RH > 90% per day
fetch_rh_above_90_daily <- function(station) {
  # Fetch the data using the API function
  rh_data <- api_call_wisconet_data_rh(station)
  
  data <- rh_data$daily_rh_above_90
  data$rh_above_90_daily_14d_ma <- rollmean(data$hours_rh_above_90, 
                                            k = 14, fill = NA, align = "right")
  print(tail(data))
  current_time <- Sys.time()
  rh_above_90_daily1 <- data %>%
    arrange(abs(difftime(date, current_time, units = "secs"))) %>%  # Sort by proximity to current date
    slice(1)
  
  # Return the data if it's not NULL
  if (!is.null(rh_data)) {
    return(rh_above_90_daily1)
  } else {
    cat("No data returned for the specified station.\n")
    return(NULL)
  }
}

################################################################ Call Tarspot API
################################################################################
################### API CALL EXAMPLE - single point ############################
################################################################################
get_risk_probability <- function(station_id, station_name, 
                                 risk_threshold,
                                 mat_30dma, max_rh_30dma,
                                 th_rh90_14ma, url_ts) {
  
  base_url <- "https://connect.doit.wisc.edu/forecasting_crop_disease/predict_tarspot_risk"
  
  body <- list(
    growth_stage = 'R1',
    fungicide_applied = 'no',
    risk_threshold = risk_threshold * 100,  # Ensure it's in percentage format
    mean_air_temp_30d_ma = mat_30dma,
    max_rh_30d_ma = max_rh_30dma,  
    tot_nhrs_rh90_14d_ma = th_rh90_14ma
  )
  
  #response <- POST(url_tspot, body = toJSON(body), encode = "json")
  response <- POST(url = base_url, query = params)
  cat('mean_air_temp_30d_ma', mat_30dma)
  cat('max_rh_30d_ma', max_rh_30dma)
  cat('tot_nhrs_rh90_14d_ma',th_rh90_14ma)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the content as JSON
    result <- content(response, as = "parsed", type = "application/json")
    
    # Retrieve only the probability value
    probability <- result$probability[[1]]
    probability_class <- result$risk_class[[1]]
    
    return(data.frame(
      Station = station_name,
      AirTemp_C_30dma=mat_30dma,
      Max_RH_pct_30dma=max_rh_30dma,
      Tot_Nhrs_RHab90_14dma=th_rh90_14ma,
      Risk = probability,
      Risk_Class=probability_class
    ))
  } else {
    # Print error if the request fails
    cat("Error: API request failed with status code", status_code(response), "\n")
    print(content(response, as = "text"))
    return(data.frame(
      Station = station_id,
      AirTemp_C_30dma=NA,
      Max_RH_pct_30dma=NA,
      Tot_Nhrs_RHab90_14dma=NA,
      Risk = NA,
      Risk_Class = NA
    ))
  }
}

###################################### Prpeare the relevant data for Tarspot
call_tarspot_for_station <- function(station_id, station_name, risk_threshold){
  rh_above_90_daily <- fetch_rh_above_90_daily(station_id)
  th_rh90_14ma <- rh_above_90_daily$rh_above_90_daily_14d_ma[1] 
  
  at<-fetch_at(station_id)
  
  mat_30dma <- at$air_temp_avg_c_30d_ma[1]  
  max_rh_30dma <- at$rh_max_30d_ma[1]
  
  #cat('==== Station id ====',station_id) 
  #cat('==== Vars ====', mat_30dma, max_rh_30dma,th_rh90_14ma, url_ts)
  result <- get_risk_probability(station_id, station_name, risk_threshold, 
                                 mat_30dma, max_rh_30dma,th_rh90_14ma, 
                                 url_ts)
  
  cat('====Risk====',result$Risk, result$Risk_Class)
  return(result)
}

