#### Code to persist the historical data as rds for quick readability
#install.packages("arrow")
#library(arrow)
library(dplyr)

historical_data <- read_parquet("data/snapshot_0224_0225_stations.parquet") %>%
  mutate(forecasting_date = as.POSIXct(forecasting_date,format = "%Y-%m-%d %H:%M:%S", tz = "CST6CDT"))

colnames(historical_data)
# Assume df is your loaded data frame
column_classes <- sapply(historical_data, class)
print(column_classes)
sapply(df, class)


hiatorical_to_save <- historical_data%>%select("date","station_id","o_collection_time","collection_time","air_temp_max_f","air_temp_min_f",           
                                               "rh_max","min_dp","max_ws","min_dp_c",
                                               "air_temp_max_c","air_temp_min_c","air_temp_avg_c","rh_above_90_night_14d_ma",
                                               "rh_above_80_day_30d_ma","air_temp_min_c_21d_ma","air_temp_max_c_30d_ma","air_temp_avg_c_30d_ma",   
                                               "rh_max_30d_ma","max_ws_30d_ma","dp_min_30d_c_ma","tarspot_risk",
                                               "tarspot_risk_class","gls_risk","gls_risk_class","whitemold_nirr_risk",      
                                               "whitemold_nirr_risk_class","whitemold_irr_30in_risk","whitemold_irr_15in_risk","fe_risk",                 
                                               "fe_risk_class","id","campbell_cloud_id",                
                                               "station_slug","station_name","latitude","longitude",                
                                               "elevation","location","station_timezone","city",                    
                                               "county","region","state","earliest_api_date",     
                                               "forecast_date")
save(hiatorical_to_save, file = "app/data/historical_data.RData")

# Disconnect from Spark when done
saveRDS(historical_data, "historical_data.rds")


remove.packages("arrow")

############# Reading from RData
# This trick returns the names of the loaded objects and assigns the object to 'df'
df <- get(load("app/data/historical_data.RData"))
head(df)
#####- reading

historical_data <- historical_data <- readRDS("historical_data.rds") %>% 
  mutate(forecasting_date = as.POSIXct(forecasting_date, 
                                       format = "%Y-%m-%d %H:%M:%S", 
                                       tz = "CST6CDT"))
historical_data$forecast_date