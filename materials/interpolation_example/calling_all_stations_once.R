install.packages("lubridate")
library(lubridate)

install.packages("httr")
library(httr)

install.packages("zoo")
library(zoo)

install.packages("dplyr")
library(dplyr)
# or
library(magrittr)

#######################################################################################################
#######################################################################################################
######################################################################### TARSPOT RISK FOR ALL STATIONS

# Initialize an empty dataframe to store the results, including station coordinates, name, code, and date
probability_results <- data.frame(
  Station_Code = character(),
  Station_Name = character(),
  Latitude = numeric(),
  Longitude = numeric(),
  Date = as.Date(character()),  # Add the date field
  Probability = numeric(),
  Risk_Class = character(),
  stringsAsFactors = FALSE
)

# Loop through all station codes
for (station_code in names(stations)) {
  station <- stations[[station_code]]
  print(station)
  
  # Get station name and coordinates
  station_name <- station$name
  station_lat <- station$latitude
  station_lon <- station$longitude
  
  cat("================== ", station_name, station_code, "\n")
  
  # Set the risk threshold (ensure it's between 0 and 1)
  risk_threshold <- 0.35
  
  # Use tryCatch to handle errors for each station
  tryCatch({
    # Call the function to get the probability and other risk information
    result <- call_tarspot_for_station(station_code, station_name, risk_threshold, Sys.Date())
    
    # Check if result is NULL or has issues
    if (is.null(result) || nrow(result) == 0) {
      cat("Error: No data returned for station", station_code, "\n")
      next  # Skip to the next iteration if no data is returned
    }
    
    # Add station details (code, name, lat, lon) to the result for each row
    result <- result %>%
      mutate(
        Station_Code = station_code,
        Station_Name = station_name,
        Latitude = station_lat,
        Longitude = station_lon
      )
    
    # Append the result (which contains 7 rows) to the probability_results dataframe
    probability_results <- rbind(probability_results, result)
    
  }, error = function(e) {
    # Handle errors by printing a message and moving to the next station
    cat("Error processing station", station_code, ":", e$message, "\n")
    next
  })
}


# Save the results to a CSV file including the station code, name, coordinates, date, and probability
write.csv(probability_results, file = "materials/probability_results_with_date.csv", row.names = FALSE)

# Print the final table with station names, coordinates, dates, and their probability values
print(probability_results)


#######################################################################################################
#######################################################################################################
######################################################################################### INTERPOLATION

install.packages(c("gstat", "sp"))
library(gstat)
library(sp)
library(ggplot2)

probability_results<- read.csv('materials/probability_results_with_date.csv')
df <- probability_results[probability_results$date_day == as.Date("2024-10-18"), ]
coordinates(df) <- ~Longitude+Latitude

v <- variogram(Risk ~ 1, data = df)
m <- fit.variogram(v, vgm("Sph"))

grid <- expand.grid(Longitude = seq(min(df$Longitude), max(df$Longitude), length.out = 100),
                    Latitude = seq(min(df$Latitude), max(df$Latitude), length.out = 100))
coordinates(grid) <- ~Longitude+Latitude
gridded(grid) <- TRUE

k <- krige(Risk ~ 1, df, grid, model = m)
k_df <- as.data.frame(k)


install.packages(c("ggplot2", "viridis", "ggmap", "maps"))

library(ggplot2)
library(viridis)
library(ggmap)
library(maps)

wisconsin_map <- map_data("state", region = "wisconsin")
# Assuming 'k_df' is your data frame with kriging results including 'Longitude', 'Latitude', and 'var1.pred'

ggplot() +
  # Add Wisconsin map as the background
  geom_polygon(data = wisconsin_map, aes(x = long, y = lat, group = group),
               fill = "grey90", color = "black") +
  
  # Overlay the kriging interpolation
  geom_tile(data = k_df, aes(x = Longitude, y = Latitude, fill = var1.pred), alpha = 0.8) +
  
  # Customize the fill gradient for the predicted risk
  scale_fill_viridis(option = "plasma", name = "Predicted Risk") +
  
  # Add title and labels
  labs(title = "Kriging Interpolation of Risk over Wisconsin",
       x = "Longitude", y = "Latitude") +
  
  # Clean up the plot appearance
  theme_minimal() +
  theme(legend.position = "right")

ggsave("materials/kriging_plot.png", width = 10, height = 8, dpi = 300)
