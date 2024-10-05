# Load necessary package for date manipulation
library(lubridate)
library(httr)
library(jsonlite)
library(zoo)

# Get today's date
today <- Sys.time()

# Subtract 3 months from today
three_months_ago <- today - months(3)

# Convert both dates to Unix timestamps in GMT
start_time <- as.integer(as.POSIXct(three_months_ago, tz = "GMT"))
end_time <- as.integer(as.POSIXct(today, tz = "GMT"))

api_call_wisconet <- function(station, start_time, end_time) {
  
  
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
    
    
    # Rearrange columns for better readability
    result_df <- result_df[c("collection_time","air_temp_avg_c", "air_temp_min_c_30d_ma","air_temp_max_c_30d_ma",
                             "air_temp_avg_c_30d_ma", "rh_max_30d_ma",
                             "rh_max")]
    
    print(head(result_df))
    
    return(result_df)
  } else {
    print(paste("Error: ", response$status_code))
    return(NULL)
  }
}

#1728011869,
#start_time = 1726715869

# Print the results
cat("Start time (3 months ago):", (start_time), "\n") 
cat("End time (today):", (end_time), "\n") 
#1728089192
#1720140392
# Test the function
a <- api_call_wisconet('ALTN', start_time, end_time)
print(class(a))  # Should print "data.frame"
print(head(a))   # Display the first few rows of the data frame


library(ggplot2)

ggplot(a, aes(x = collection_time)) +
  geom_line(aes(y = air_temp_avg_c, color = "Daily Average")) +
  geom_line(aes(y = air_temp_avg_c_30d_ma, color = "30-day Moving Average")) +
  labs(title = "Average Temperature (Celsius)",
       x = "Date",
       y = "Temperature (°C)",
       color = "Legend") +
  theme_minimal()
