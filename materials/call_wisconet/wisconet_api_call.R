# Load necessary packages
library(lubridate)
library(httr)
library(jsonlite)
library(zoo)
library(ggplot2)
library(dplyr)  # Load dplyr for the pipe operator
library(httr)

# Function to get weather data from the API
api_call_wisconet_data <- function(station, start_time, end_time) {
  base_url <- 'https://wisconet.wisc.edu'
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
    fahrenheit_to_celsius <- function(temp_f) {
      (temp_f - 32) * 5/9
    }
    
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
    
    api_call_wisconet_plot(result_df)
    
    plot1 <- ggplot(result_df, aes(x = collection_time)) +
      geom_line(aes(y = rh_max, color = "Max Relative Humidity (%)")) +
      geom_line(aes(y = rh_max_30d_ma, color = "30-day MA of Max RH"), linetype = "dashed") +
      labs(title = "Max Relative Humidity (%) - Arlington Station",
           x = "Date",
           y = "Max Relative Humidity (%)",
           color = "Legend") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    print(plot1)
    
    # Rearrange columns for better readability
    result_df <- result_df[c("collection_time", "air_temp_avg_c", "air_temp_min_c_30d_ma",
                             "air_temp_max_c_30d_ma", "air_temp_avg_c_30d_ma",
                             "rh_max_30d_ma", "rh_max")]
    print(result_df)
    
    
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

# Function to plot the data
api_call_wisconet_plot <- function(df) {
  plot <- ggplot(df, aes(x = collection_time)) +
    geom_line(aes(y = air_temp_avg_c, color = "Daily Average")) +
    geom_line(aes(y = air_temp_max_c_30d_ma, color = "30-day Moving Average Max A Temp"), linetype = "dashed") +
    geom_line(aes(y = air_temp_avg_c_30d_ma, color = "30-day Moving Average Mean A Temp"), linetype = "dashed") +
    geom_line(aes(y = air_temp_min_c_30d_ma, color = "30-day Moving Average Min A Temp"), linetype = "dashed") +
    geom_line(aes(y = air_temp_max_c, color = "Daily Max Temp"), alpha = 0.6) +
    geom_line(aes(y = air_temp_min_c, color = "Daily Min Temp"), alpha = 0.6) +
    labs(title = "Air Temperature (°C) - Arlington Station",
         x = "Date",
         y = "Temperature (°C)",
         color = "Legend") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(plot)  # Explicitly print the plot
}


# Function to fetch and plot data
fetch_at <- function(station) {
  # Get today's date
  today <- Sys.time()
  three_months_ago <- today - months(12)
  
  # Convert both dates to Unix timestamps in GMT
  start_time <- as.integer(as.POSIXct(three_months_ago, tz = "GMT"))
  end_time <- as.integer(as.POSIXct(today, tz = "GMT"))
  
  print(start_time)
  
  # Fetch the data using the API function
  data_df <- api_call_wisconet_data(station, start_time, end_time)
  
  # Plot the data if it is not NULL
  if (!is.null(data_df)) {
    return(data_df)
  } else {
    cat("No data returned for the specified station.\n")
    return(NULL)
  }
}

# Load necessary packages
# Function to get 60-minute relative humidity data
api_call_wisconet_data_rh <- function(station, start_time, end_time) {
  base_url <- 'https://wisconet.wisc.edu'
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
    #print(data$measures)
    # Process measures to get '60min_relative_humidity_pct_avg'
    for (i in seq_along(data$measures)) {
      measures <- data$measures[[i]]
      for (j in seq_len(length(measures))) {
        print(measures[[j]])
        result_df$rh_avg[i] <- measures[[j]][1]
      }
    }
    print(result_df)
    # Filter rows where RH > 90%
    result_df <- result_df %>%
      mutate(hour = hour(collection_time)) %>%
      filter(rh_avg >= 90 & (hour >= 20 | hour <= 6))
    
    rh_greater_than_90 <- result_df
    
    # Group by day and count the number of hours where RH > 90 for each day
    daily_rh_above_90 <- rh_greater_than_90 %>%
      mutate(date = as.Date(collection_time)) %>%
      group_by(date) %>%
      summarise(hours_rh_above_90 = n())
    
    # Print the result
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

# Function to fetch and print the number of hours with RH > 90% per day
fetch_rh_above_90_daily <- function(station) {
  # Get today's date
  today <- Sys.time()
  three_months_ago <- today - months(12)
  
  # Convert both dates to Unix timestamps in GMT
  start_time <- as.integer(as.POSIXct(three_months_ago, tz = "GMT"))
  end_time <- as.integer(as.POSIXct(today, tz = "GMT"))
  
  # Fetch the data using the API function
  rh_data <- api_call_wisconet_data_rh(station, start_time, end_time)
  
  data<-rh_data$daily_rh_above_90
  print(colnames(data))
  data$rh_above_90_daily_14d_ma <- rollmean(data$hours_rh_above_90, k = 14, fill = NA, align = "right")
  
  plot2 <- ggplot(data, aes(x = date)) +
    geom_line(aes(y = hours_rh_above_90, color = "Tot Night hrs RH>=90(%)")) +
    geom_line(aes(y = rh_above_90_daily_14d_ma, color = "14-day MA of Tot Night hrs RH>=90(%)"), linetype = "dashed") +
    labs(title = "Tot Night hrs RH(%)>= 90 - Arlington Station",
         x = "Date",
         y = "Tot Night hrs RH>=90 (%)",
         color = "Legend") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(plot2)
  ggsave("materials/call_wisconet/RH_above_90_plot.png", plot = plot2, width = 8, height = 6, dpi = 300)
  
  
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



get_risk_probability <- function(station_id, mat_30dma, max_rh_30dma,th_rh90_14ma, url) {
  cat('here....',mat_30dma, max_rh_30dma,th_rh90_14ma)
  url_ts <- paste0(url, "/predict_tarspot_risk")
  body <- list(
    growth_stage = 'R1',
    fungicide_applied = 'no',
    risk_threshold = 35, 
    mean_air_temp_30d_ma = mat_30dma,
    max_rh_30d_ma = max_rh_30dma,  
    tot_hrs_rh90_14d_ma = th_rh90_14ma
  )

  # Make the POST request
  response <- POST(url_ts, body = body, encode = "json")
  
  print(response)
  if (status_code(response) == 200) {
    response_content <- content(response, as = "parsed", type = "application/json")
    
    probability <- response_content$probability[[1]]
    risk_class <- response_content$risk_class[[1]]
    
    return(data.frame(
      station_id = station_id,
      mean_air_temp_30d_ma=mat_30dma,
      max_rh_30d_ma=max_rh_30dma,
      tot_hrs_rh90_14d_ma=th_rh90_14ma,
      risk_probability = probability,
      risk_class=risk_class
    ))
  } else {
    return(data.frame(
      station_id = station_id,
      mean_air_temp_30d_ma=NA,
      max_rh_30d_ma=NA,
      tot_hrs_rh90_14d_ma=NA,
      risk_probability = NA,
      risk_class = NA
    ))
  }
}

call_tarspot_for_station <- function(station_id){
  rh_above_90_daily <- fetch_rh_above_90_daily(station_id)
  th_rh90_14ma <- rh_above_90_daily$rh_above_90_daily_14d_ma[1] 
  
  at<-fetch_at(station_id)
  
  mat_30dma <- at$air_temp_avg_c_30d_ma[1]  
  max_rh_30dma <- at$rh_max_30d_ma[1]
  
  url_ts <- "https://connect.doit.wisc.edu/forecasting_crop_disease"
  cat(station_id, mat_30dma, max_rh_30dma,th_rh90_14ma, url_ts)
  result <- get_risk_probability(station_id, mat_30dma, max_rh_30dma,th_rh90_14ma, url_ts)
  
  print(result)
}

station_id <- 'ALTN'
call_tarspot_for_station(station_id)