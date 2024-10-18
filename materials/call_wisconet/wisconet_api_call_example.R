install.packages("lubridate")
library(lubridate)
library(httr)
library(jsonlite)


station_id <- 'ALTN'
base_url <- 'https://wisconet.wisc.edu'
current <- Sys.time()
today_ct <- with_tz(current, tzone = "America/Chicago")

#three_months_ago <- as.Date(today_ct) - 8

out<-from_ct_to_gmt(today_ct, 3)
# Assigning start and end time from 'out' object (assumed to exist)
start_time <- out$start_time_gmt
end_time <- out$end_time_gmt

cat('Today: ', start_time, ' | Three months ago: ', end_time, '\n')

# Construct API endpoint and parameters
endpoint <- paste0('/api/v1/stations/', station_id, '/measures')

params <- list(
  end_time = end_time,
  start_time = start_time,
  fields = 'daily_air_temp_f_max,daily_air_temp_f_min,daily_relative_humidity_pct_max'
)

# Perform GET request
response <- GET(url = paste0(base_url, endpoint), query = params)

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(zoo)

fahrenheit_to_celsius <- function(fahrenheit) {
  (fahrenheit - 32) * 5/9
}

process_response <- function(response) {
  if (response$status_code == 200) {
    # Parse JSON response
    data1 <- fromJSON(content(response, as = "text"), flatten = TRUE)
    data <- data1$data
    
    # Convert collection_time to POSIXct and set timezone to Chicago
    ctime <- as.POSIXct(data$collection_time, origin = "1970-01-01")
    collection_time_chicago <- with_tz(ctime, tzone = "America/Chicago")
    
    # Initialize result data frame
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
        if (measures[j, 1] == 4) result_df$air_temp_max_f[i] <- measures[j, 2]  # Max temp
        if (measures[j, 1] == 6) result_df$air_temp_min_f[i] <- measures[j, 2]  # Min temp
        if (measures[j, 1] == 20) result_df$rh_max[i] <- measures[j, 2]         # Max RH
      }
    }
    
    # Convert Fahrenheit to Celsius
    result_df$air_temp_max_c <- fahrenheit_to_celsius(result_df$air_temp_max_f)
    result_df$air_temp_min_c <- fahrenheit_to_celsius(result_df$air_temp_min_f)
    
    # Calculate average temperature in both Fahrenheit and Celsius
    result_df$air_temp_avg_f <- rowMeans(result_df[c("air_temp_max_f", "air_temp_min_f")], na.rm = TRUE)
    result_df$air_temp_avg_c <- fahrenheit_to_celsius(result_df$air_temp_avg_f)
    
    # Sort data by collection time
    result_df <- result_df %>% arrange(collection_time)
    print("==================================================")
    result_df0 <- result_df[c("collection_time", 
                             "air_temp_max_c", 
                             "air_temp_min_c",
                             "air_temp_avg_c", 
                             "air_temp_max_f",
                             "air_temp_min_f", 
                             "air_temp_avg_f",
                             "rh_max")]
    print(result_df0)
    print("==================================================")
    
    # Calculate 30-day moving averages
    result_df$air_temp_min_c_30d_ma <- rollmean(result_df$air_temp_min_c, k = 30, fill = NA, align = "right")
    result_df$air_temp_max_c_30d_ma <- rollmean(result_df$air_temp_max_c, k = 30, fill = NA, align = "right")
    result_df$air_temp_avg_c_30d_ma <- rollmean(result_df$air_temp_avg_c, k = 30, fill = NA, align = "right")
    result_df$rh_max_30d_ma <- rollmean(result_df$rh_max, k = 30, fill = NA, align = "right")
    
    # Rearrange columns for better readability
    result_df <- result_df[c("collection_time", 
                             "air_temp_avg_c", "air_temp_min_c","air_temp_max_c",
                             "air_temp_min_c_30d_ma",
                             "air_temp_max_c_30d_ma", 
                             "air_temp_avg_c_30d_ma",
                             "rh_max_30d_ma", 
                             "rh_max")]
    
    # Sort by proximity to the current time
    current_time <- Sys.time()
    result_df1 <- result_df %>%
      arrange(abs(difftime(collection_time, current_time, units = "secs")))  # Sort by closest to current time
    
    
    return(result_df1)
  } else {
    cat("Error: Failed to fetch data. Status:", response$status_code, "\n")
    return(NULL)
  }
}


a<-process_response(response)
print(a)


weather_plot <- a %>%
  ggplot(aes(x = collection_time)) +
  
  # Min temperature and its 30-day moving average
  geom_line(aes(y = air_temp_min_c, color = "Min Temp (C)")) +
  geom_line(aes(y = air_temp_min_c_30d_ma, color = "Min Temp (C) (30d MA)"), linetype = "dashed") +
  
  # Avg temperature and its 30-day moving average
  geom_line(aes(y = air_temp_avg_c, color = "Avg Temp (C)")) +
  geom_line(aes(y = air_temp_avg_c_30d_ma, color = "Avg Temp (C) (30d MA)"), linetype = "dashed") +
  
  # Max temperature and its 30-day moving average
  geom_line(aes(y = air_temp_max_c, color = "Max Temp (C)")) +
  geom_line(aes(y = air_temp_max_c_30d_ma, color = "Max Temp (C) (30d MA)"), linetype = "dashed") +
  
  # Add RH to the plot using the secondary y-axis
  #geom_line(aes(y = rh_max, color = "Max RH (%)"), linetype = "solid") +
  
  # Primary y-axis for temperature
  #scale_y_continuous(
  #  name = "Temperature (C)",  # Label for the primary y-axis
  #  sec.axis = sec_axis(~ ., name = "Relative Humidity (%)")  # Secondary y-axis for RH
  #) +
  
  # Title, labels, and theme
labs(title = paste("Air Temperature (C) for", station),
     x = "Date",y='Air Temperature (C)') +
  
  # Minimal theme
  theme_minimal() +
  
  # Move the legend below the plot
  theme(
    legend.position = "bottom",         # Position the legend below the plot
    legend.direction = "horizontal",    # Arrange the legend items horizontally
    legend.title = element_blank(),     # Remove the legend title
    legend.text = element_text(size = 10)  # Customize legend text size
  ) +
  
  # Color manual assignment
  scale_color_manual(values = c(
    "Min Temp (C)" = "blue", 
    "Min Temp (C) (30d MA)" = "lightblue",
    "Avg Temp (C)" = "green", 
    "Avg Temp (C) (30d MA)" = "lightgreen",
    "Max Temp (C)" = "red",
    "Max Temp (C) (30d MA)" = "pink"#,
    #"Max RH (%)" = "purple"  # Color for RH
  ))

print(weather_plot)