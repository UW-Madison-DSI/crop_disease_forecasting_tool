#### Code to persist the historical data as rds for quick readability
#install.packages("arrow")
#library(arrow)
library(dplyr)

historical_data <- read_parquet("/Users/mariaoros/Documents/DataScienceInstitute/2024/Forecasting-models/corn_disease_forecast_api/data/snapshot_0224_0225_stations.parquet")
historical_data$forecasting_date <- as.Date(as.POSIXct(historical_data$forecasting_date, format = "%Y-%m-%d %H:%M:%S %Z"))

print(historical_data$forecasting_date)

remove.packages("arrow")

colnames(historical_data)
# Assume df is your loaded data frame
column_classes <- sapply(historical_data, class)
print(column_classes)
sapply(df, class)




hiatorical_to_save <- historical_data%>%select("date","station_id","collection_time","tarspot_risk",
                                               "tarspot_risk_class","gls_risk","gls_risk_class","whitemold_nirr_risk",      
                                               "whitemold_nirr_risk_class","whitemold_irr_30in_risk","whitemold_irr_15in_risk","fe_risk",                 
                                               "fe_risk_class",             
                                               "station_name","latitude","longitude",                
                                               "location","city",                    
                                               "county","region","state","earliest_api_date",     
                                               "forecasting_date")
save(hiatorical_to_save, file = "historical_data_2.RData")

saaply(hiatorical_to_save, class)
print(hiatorical_to_save$forecasting_date)


# Disconnect from Spark when done
saveRDS(historical_data, "historical_data.rds")




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