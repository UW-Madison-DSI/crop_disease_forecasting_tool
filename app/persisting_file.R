#### Code to persist the historical data as rds for quick readability

library(arrow)
library(dplyr)

historical_data <- read_parquet("snapshot_0224_0225_stations.parquet") %>%
  mutate(forecasting_date = as.POSIXct(forecasting_date, 
                                       format = "%Y-%m-%d %H:%M:%S", 
                                       tz = "CST6CDT"))

saveRDS(historical_data, "historical_data.rds")

#####- reading

historical_data <- historical_data <- readRDS("historical_data.rds") %>% 
  mutate(forecasting_date = as.POSIXct(forecasting_date, 
                                       format = "%Y-%m-%d %H:%M:%S", 
                                       tz = "CST6CDT"))
historical_data