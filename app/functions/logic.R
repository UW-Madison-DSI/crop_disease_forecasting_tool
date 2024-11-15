# Load necessary packages
library(lubridate)
library(httr)
library(jsonlite)
library(zoo)
library(ggplot2)
library(dplyr)  # Load dplyr for the pipe operator
library(scales)


source("functions/auxiliar_functions.R")

################################################################ Preparation

base_url <- 'https://wisconet.wisc.edu'
url_ts <- "https://connect.doit.wisc.edu/forecasting_crop_disease"


################################################################ Function to get weather data from Wisconet API
api_call_wisconet_data_daily <- function(station, start_time, end_time) {
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
    
    ctime <- as.POSIXct(data$collection_time, origin = "1970-01-01")
    collection_time_chicago <- with_tz(ctime, tzone = "America/Chicago")
    
    # Create the result data frame
    result_df <- data.frame(
      o_collection_time = ctime,
      collection_time = collection_time_chicago,
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
    result_df <- result_df[c("collection_time", 
                             "air_temp_min_c",
                             "air_temp_avg_c",
                             "air_temp_max_c",
                             "air_temp_min_c_30d_ma",
                             "air_temp_max_c_30d_ma", 
                             "air_temp_avg_c_30d_ma",
                             "rh_max_30d_ma", 
                             "rh_max")]
    
    current_time <- Sys.time()
    result_df1 <- result_df %>%
      arrange(abs(difftime(collection_time, current_time, units = "secs")))  # Sort by proximity to current date
    
    return(result_df1)
  } else {
    print(paste("Error: ", response$status_code))
    return(NULL)
  }
}



# Function to fetch and plot data
fetch_at <- function(station, start_time, end_time) {
  
  # Fetch the data using the API function
  data_df <- api_call_wisconet_data_daily(station, start_time, end_time)
  
  # Plot the data if it is not NULL
  if (!is.null(data_df)) {
    return(data_df)
  } else {
    cat("No data returned for the specified station.\n")
    return(NULL)
  }
}

################################################################ Function to get 60-minute relative humidity data
api_call_wisconet_data_rh <- function(station, start_time, end_time) {
  
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
      )

    
    # Group by date and sum the counts of night hours where RH >= 90 for each day
    daily_rh_above_90 <- result_df %>%
      group_by(adjusted_date) %>%
      summarise(hours_rh_above_90 = sum(rh_night_above_90, na.rm = TRUE)) %>%
      ungroup()
    
    print("----------------------- +++++ --- +++++ ---------------------------------")
    # Calculate 14-day rolling mean for RH >= 90 hours
    daily_rh_above_90$rh_above_90_daily_14d_ma <- rollmean(daily_rh_above_90$hours_rh_above_90,
                                                           k = 14, fill = NA, align = "right")
    
    print(daily_rh_above_90)
    
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
fetch_rh_above_90_daily <- function(station, start_time, end_time) {
  # Fetch the data using the API function
  rh_data <- api_call_wisconet_data_rh(station, start_time, end_time)
  
  data <- rh_data$daily_rh_above_90
  data$rh_above_90_daily_14d_ma <- rollmean(data$hours_rh_above_90, 
                                            k = 14, fill = NA, align = "right")
  rh_above_90_daily1 <- data %>% arrange(adjusted_date) # Sort by proximity to current date
  
  # Return the data if it's not NULL
  if (!is.null(rh_data)) {
    return(rh_above_90_daily1)
  } else {
    cat("No data returned for the specified station.\n")
    return(NULL)
  }
}


################################################################################
################################################################################
################################################################ Call Tarspot 
## Using the logic of the model
logistic <- function(logit) {
  exp(logit) / (1 + exp(logit))
}

calculate_tarspot_risk <- function(meanAT, maxRH, rh90_night_tot) {
  # Logistic regression formulas for the two models, no irrigation total needed
  logit_LR4 <- 32.06987 - (0.89471 * meanAT) - (0.14373 * maxRH) #paper page5
  logit_LR6 <- 20.35950 - (0.91093 * meanAT) - (0.29240 * rh90_night_tot) #paper page5
  probabilities <- sapply(c(logit_LR4, logit_LR6), logistic)
  ensemble_prob <- mean(probabilities)
  
  # Calculate risk using the general disease risk function
  return(ensemble_prob)
}


classify_risk <- function(probability, medium_threshold, high_threshold) {
  cat("----------- Probability ", probability, medium_threshold, high_threshold)
  if (probability<=0){
    return ("NoRisk")
  }else if (probability >= 0 & probability<=medium_threshold) {
    return("Low")
  } else if (probability > medium_threshold & probability<=high_threshold) {
    return("Medium")
  } else if (probability > high_threshold){
    return("High")
  }
}

get_risk_probability <- function(station_id, station_name, 
                                        risk_threshold,
                                        mat_30dma, max_rh_30dma,
                                        th_rh90_14ma, url_ts) {
  
  probability<-calculate_tarspot_risk(mat_30dma, max_rh_30dma,
                                 th_rh90_14ma)
  print("--------------------------------------------------------------")
  cat(risk_threshold, probability)
  print("--------------------------------------------------------------")
  # Check if the request was successful
  if (probability) {
    # Retrieve relevant values

    probability_class <- classify_risk(probability,.2, risk_threshold)
    #if_else(probab>risk_threshold,"High", "Low")
    
    dframe<-data.frame(
      Station = station_name,
      AirTemp_C_30dma=mat_30dma,
      Max_RH_pct_30dma=max_rh_30dma,
      Tot_Nhrs_RHab90_14dma=th_rh90_14ma,
      Risk = 100*probability,
      Risk_Class = probability_class
    )
    return(dframe)
  } else {
    # Print error if the request fails
    cat("Error: API request failed with status code", status_code(response), "\n")
    print(content(response, as = "text"))
    dframe<-data.frame(
      Station = station_id,
      AirTemp_C_30dma=NA,
      Max_RH_pct_30dma=NA,
      Tot_Nhrs_RHab90_14dma=NA,
      Risk = NA,
      Risk_Class = NA
    )
    return(dframe)
  }
}


################### Still comparing if calling by internal logic or by the API, probably will be deprecated one
#get_risk_probability_by_api <- function(station_id, station_name, 
#                                 risk_threshold,
#                                 mat_30dma, max_rh_30dma,
#                                 th_rh90_14ma, url_ts) {
  
#  base_url <- "https://connect.doit.wisc.edu/forecasting_crop_disease/predict_tarspot_risk"
  
#  params <- list(
#    growth_stage = 'yes',
#    fungicide_applied = 'no',
#    risk_threshold = risk_threshold * 100,  # Ensure it's in percentage format
#    mean_air_temp_30d_ma = mat_30dma,
#    max_rh_30d_ma = max_rh_30dma,  
#    tot_nhrs_rh90_14d_ma = th_rh90_14ma
#  )
  
#  response <- POST(url = base_url, query = params)

  # Check if the request was successful
#  if (status_code(response) == 200) {
    # Parse the content as JSON
#    result <- content(response, as = "parsed", type = "application/json")
    
    # Retrieve relevant values
#    probability <- result$probability[[1]]
#    probability_class <- result$risk_class[[1]]
    
#    return(data.frame(
#      Station = station_name,
#      AirTemp_C_30dma=mat_30dma,
#      Max_RH_pct_30dma=max_rh_30dma,
#      Tot_Nhrs_RHab90_14dma=th_rh90_14ma,
#      Risk = probability,
#      Risk_Class = probability_class
#    ))
#  } else {
    # Print error if the request fails
#    cat("Error: API request failed with status code", status_code(response), "\n")
#    print(content(response, as = "text"))
#    return(data.frame(
#      Station = station_id,
#      AirTemp_C_30dma=NA,
#      Max_RH_pct_30dma=NA,
#      Tot_Nhrs_RHab90_14dma=NA,
#      Risk = NA,
#      Risk_Class = NA
#    ))
#  }
#}


###################################### Prpeare the relevant data for Tarspot
call_tarspot_for_station <- function(station_id, station_name, risk_threshold, current){
  today_ct <- with_tz(current, tzone = "America/Chicago")
  
  mo <- 6 # historical data in terms of num of months
  out <- from_ct_to_gmt(today_ct, mo)
  
  # Convert both dates to Unix timestamps in GMT
  start_time <- out$start_time_gmt
  end_time <- out$end_time_gmt
  
  
  
  rh_above_90_daily <- fetch_rh_above_90_daily(station_id,start_time, end_time)
  rh_above_90_daily1 <- rh_above_90_daily %>% mutate(date_day = as.Date(adjusted_date),
                        date_day1 = floor_date(as.Date(adjusted_date), unit='days')) %>%
    arrange(desc(date_day))
  
  at0 <- api_call_wisconet_data_daily(station_id, start_time, end_time)
  #fetch_at(station_id,start_time, end_time)
  at0 <- at0 %>% mutate(date_day1 = floor_date(collection_time, unit='days'),
                        date_day = as.Date(collection_time)-1) 

  # Single variables
  th_rh90_14ma <- rh_above_90_daily1$rh_above_90_daily_14d_ma[1]
  at <- at0 %>% slice(1)
  mat_30dma <- at$air_temp_avg_c_30d_ma[1]  
  max_rh_30dma <- at$rh_max_30d_ma[1]
  
  print("----------->>> here in rh above")
  print(head(rh_above_90_daily1,10))
  
  print("----------------------------->> Daily variables, Input API")
  print(at0)
  
  print("----------------------------->> Merge of the datasets")
  cat(colnames(rh_above_90_daily1))
  print("--------")
  cat(colnames(at0))

  merged_ds <- merge(x = rh_above_90_daily1,
                     y = at0, 
                     by.x = "date_day", 
                     by.y = "date_day") %>% 
    mutate(date_day = date_day + 1) %>%
    arrange(desc(date_day)) %>% 
    select(c('date_day',
             'rh_above_90_daily_14d_ma', 
             'rh_max',
             'rh_max_30d_ma',
             'air_temp_avg_c_30d_ma', 
             'air_temp_avg_c')) %>% slice(1:7)
  
  merged_ds <- merged_ds %>%
    rowwise() %>%
    mutate(risk_output = list(get_risk_probability(station_id = station_id,
                                                   station_name = station_name,
                                                   risk_threshold = risk_threshold,
                                                   mat_30dma = air_temp_avg_c_30d_ma,
                                                   max_rh_30dma = rh_max_30d_ma,
                                                   th_rh90_14ma = rh_above_90_daily_14d_ma,
                                                   url_ts = url_ts)),
           Risk = risk_output$Risk,
           Risk_Class = risk_output$Risk_Class,
           Station = risk_output$station_name) %>%
    select(-risk_output)
  
  return(merged_ds)
}

