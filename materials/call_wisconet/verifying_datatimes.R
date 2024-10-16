# Load necessary packages
library(lubridate)
library(httr)
library(jsonlite)
library(zoo)
library(ggplot2)
library(dplyr)
library(tidyr)  # For complete()

base_url <- 'https://wisconet.wisc.edu'
url_ts <- "https://connect.doit.wisc.edu/forecasting_crop_disease"

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

# Function to convert Fahrenheit to Celsius
fahrenheit_to_celsius <- function(temp_f) {
  (temp_f - 32) * 5/9
}

# API call function to fetch RH data
api_call_wisconet_data_rh <- function(station_id, start_time, end_time) {
  endpoint <- paste0('/api/v1/stations/', station_id, '/measures')
  
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
      o_collection_time = data$collection_time,
      collection_time = as.POSIXct(data$collection_time, tz = 'America/Chicago', origin = "1970-01-01"),
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
        adjusted_date = floor_date(collection_time_ct, unit = "day"),
        rh_night_above_90 = if_else(rh_avg >= 90 & (hour >= 20 | hour <= 6), 1, 0)
      )
    
    # Group by date and sum the counts of night hours where RH >= 90 for each day
    daily_rh_above_90 <- result_df %>%
      group_by(adjusted_date) %>%
      summarise(hours_rh_above_90 = sum(rh_night_above_90, na.rm = TRUE)) %>%
      ungroup()
    
    # Ensure all days are included, even if no hours where RH >= 90, using 0
    all_dates <- seq(min(result_df$adjusted_date), max(result_df$adjusted_date), by = "day")
    
    daily_rh_above_90 <- daily_rh_above_90 %>%
      complete(adjusted_date = all_dates, fill = list(hours_rh_above_90 = 0))
    
    # Calculate 14-day rolling mean for RH >= 90 hours
    daily_rh_above_90$rh_above_90_daily_14d_ma <- rollmean(daily_rh_above_90$hours_rh_above_90,
                                                           k = 14, fill = NA, align = "right")
    
    return(list(
      data = result_df,
      data_rh90 = daily_rh_above_90
    ))
  } else {
    print(paste("Error: ", response$status_code))
    return(NULL)
  }
}

# Parameters for the station ID and time range
station_id <- 'ALTN'
current <- Sys.time()
today_ct <- with_tz(current, tzone = "America/Chicago")

# Example: 2 months ago
mo <- 2
out <- from_ct_to_gmt(today_ct, mo)

cat('Start time:', out$start_time_gmt, 'End time:', out$end_time_gmt, '\n')

# Call the API function
result <- api_call_wisconet_data_rh(station_id, out$start_time_gmt, out$end_time_gmt)

# If there's a result, process and view the data
if (!is.null(result)) {
  table <- result$data_rh90
  
  # Print the last 20 rows and first 15 rows
  tail(table, 20)
  head(table, 15)
}

table1 <- result$data
tail(table1, 25)
head(table1, 25)

######
library(ggplot2)

# Create the line plot for Relative Humidity
ggplot(table, aes(x = adjusted_date)) +
  geom_line(aes(y = hours_rh_above_90, color = "RHumidity (%)")) +
  geom_line(aes(y = rh_above_90_daily_14d_ma, color = "14d MA RHumidity (%)")) +# Corrected placement of parentheses
  labs(title = "Relative Humidity (%) - Arlington Station",
       x = "Time-hrs",
       y = "Relative Humidity (%)",
       color = "Legend") +
  theme_minimal() +
  theme(legend.position = "bottom")
