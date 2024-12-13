library(httr)      # For API requests
library(jsonlite)  # For parsing JSON
library(dplyr)     # For data manipulation
library(purrr)
library(lubridate)
library(zoo)
library(tidyr)

####################################################################################################
####################################################################################################
########################## This is an attempt to do optimization of the API on risk for all stations
####################################################################################################
####################################################################################################

base_url <- "https://connect.doit.wisc.edu/pywisconet_wrapper/bulk_measures/"
station_id<-'ALTN'
start_date <- as.POSIXct('2024-12-12', tz = "US/Central")
date_utc_minus_31 <- start_date - days(38)

# Construct the full URL
url <- paste0(base_url, station_id)


###################################################### Define query parameters daily
query_params <- list(
  start_date = format(date_utc_minus_31, "%Y-%m-%d"),
  end_date = format(start_date, "%Y-%m-%d"),
  measurements = 'ALL',
  frequency = "DAILY"
)

# Optional headers (add Authorization if needed)
headers <- c(
  "accept" = "application/json"
)
print("---------------+++++++++++++++-----------------")
# Make the GET request
response_daily <- GET(url, query = query_params, add_headers(.headers = headers))
print(response_daily)
data_daily = fromJSON(rawToChar(response_daily$content))

library(dplyr)
library(tidyr)

result_df <- data_daily %>% select(collection_time_ct, 
                             standard_name, value) %>%
  pivot_wider(
    id_cols = c(collection_time_ct),  # Similar to the index in pivot_table
    names_from = standard_name,               # Columns to pivot on
    values_from = value                            # Values to populate in the new columns
  ) %>%
  # Reset the index (if needed), in R this is essentially just ensuring the columns are correct
  ungroup()

fahrenheit_to_celsius <- function(fahrenheit) {
  celsius <- (fahrenheit - 32) * 5 / 9
  return(celsius)
}
# View the result
head(result_df%>%select(collection_time_ct, daily_air_temp_f_max, daily_air_temp_f_avg,
                        daily_dew_point_f_min,daily_relative_humidity_pct_max, 
                        daily_wind_speed_mph_max))

#'daily_air_temp_f_max', 'daily_air_temp_f_avg','daily_dew_point_f_max','daily_relative_humidity_pct_max', 'daily_wind_speed_mph_max']:
daily_aggregations <- result_df%>%select(collection_time_ct, daily_air_temp_f_max, daily_air_temp_f_avg,daily_air_temp_f_min,
                                         daily_dew_point_f_min,daily_relative_humidity_pct_max, 
                                         daily_wind_speed_mph_max) %>%
  mutate(
    date = as.Date(collection_time_ct),
    air_temp_avg_value_30d_ma = rollmean(fahrenheit_to_celsius(daily_air_temp_f_avg), k = 30, fill = NA, align = "right"),
    air_temp_max_value_30d_ma = rollmean(fahrenheit_to_celsius(daily_air_temp_f_max), k = 30, fill = NA, align = "right"),
    air_temp_min_value_30d_ma = rollmean(fahrenheit_to_celsius(daily_air_temp_f_min), k = 21, fill = NA, align = "right"),
    daily_relative_humidity_pct_max_30d_ma = rollmean(daily_relative_humidity_pct_max, k = 30, fill = NA, align = "right"),
    daily_dew_point_f_min_30d_ma = rollmean(fahrenheit_to_celsius(daily_dew_point_f_min), k = 30, fill = NA, align = "right"),
    daily_wind_speed_mph_max_30d_ma = rollmean(daily_wind_speed_mph_max, k = 30, fill = NA, align = "right")
  )

print(daily_aggregations, n=38)
#dta<-rawToChar(content(response))
#data <- fromJSON(rawToChar(content(response)))
#data


###################################################### Define query parameters hourly
query_params_60min <- list(
  start_date = format(date_utc_minus_31, "%Y-%m-%d"),
  end_date = format(start_date, "%Y-%m-%d"),
  measurements = 'RELATIVE_HUMIDITY',
  frequency = "MIN60"
)

print("---------------+++++++++++++++-----------------")
# Make the GET request
response_hrly <- GET(url, query = query_params_60min, add_headers(.headers = headers))
print(response_hrly)
data_response_hrly <- fromJSON(rawToChar(response_hrly$content))

library(dplyr)
library(tidyr)

result_df_hrly <- data_response_hrly %>% select(collection_time_ct, 
                                   standard_name, value) %>%
  pivot_wider(
    id_cols = c(collection_time_ct),  # Similar to the index in pivot_table
    names_from = standard_name,               # Columns to pivot on
    values_from = value                            # Values to populate in the new columns
  ) %>%
  # Reset the index (if needed), in R this is essentially just ensuring the columns are correct
  ungroup()

library(dplyr)
library(lubridate)

# Assuming 'data' is your data frame with 'collection_time_ct' and '60min_relative_humidity_pct_avg'

# Convert 'collection_time_ct' to POSIXct if it's not already in that format
result_df_hrly$collection_time_ct <- as.POSIXct(result_df_hrly$collection_time_ct, tz = "UTC")

# Create the variables you need: hour and date
result_df_hrly <- result_df_hrly %>%
  mutate(
    hour = hour(collection_time_ct),  # Extract hour from collection_time_ct
    date = as.Date(collection_time_ct)  # Extract date from collection_time_ct
  )

# Aggregating by date
aggregated_data_hr <- result_df_hrly %>%
  group_by(date) %>%
  summarise(
    night_hours_above_90 = sum((hour >= 0 & hour < 6 | hour >= 20) & (`60min_relative_humidity_pct_avg` >= 90), na.rm = TRUE),  # Count night hours (00:00 - 06:00) where value > 90
    total_hours_above_80 = sum(`60min_relative_humidity_pct_avg` >= 80, na.rm = TRUE)  # Count all hours where value > 80
  )

# View the result
head(aggregated_data)

# View the result
head(result_df%>%select(collection_time_ct, daily_air_temp_f_max, daily_air_temp_f_avg,
                        daily_dew_point_f_min,daily_relative_humidity_pct_max, 
                        daily_wind_speed_mph_max))

#'daily_air_temp_f_max', 'daily_air_temp_f_avg','daily_dew_point_f_max','daily_relative_humidity_pct_max', 'daily_wind_speed_mph_max']:
hrly_aggregations <- aggregated_data_hr%>%select(date, night_hours_above_90, total_hours_above_80) %>%
  mutate(
    night_hours_above_90_14d_ma = rollmean(night_hours_above_90, k = 14, fill = NA, align = "right"),
    total_hours_above_80_30d_ma = rollmean(total_hours_above_80, k = 30, fill = NA, align = "right")
  )

print(hrly_aggregations, n=38)

## ----------------------------------------------------------------------- Merge
library(dplyr)

# Assuming daily_aggregations and hrly_aggregations are your data frames

result <- daily_aggregations %>%
  left_join(hrly_aggregations, by = "date")

# View the result
head(result)
print(result%>%select(date, air_temp_avg_value_30d_ma,
                      air_temp_max_value_30d_ma,
                      air_temp_min_value_30d_ma,
                      daily_relative_humidity_pct_max_30d_ma,
                      daily_dew_point_f_min_30d_ma,
                      daily_wind_speed_mph_max_30d_ma, night_hours_above_90_14d_ma,total_hours_above_80_30d_ma), n=38)