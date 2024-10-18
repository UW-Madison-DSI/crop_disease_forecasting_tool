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
  
  # Get station name and coordinates
  station_name <- station$name
  station_lat <- station$latitude
  station_lon <- station$longitude
  
  cat("================== ", station_name, station_code, "\n")
  
  # Set the risk threshold (ensure it's between 0 and 1)
  risk_threshold <- 0.35
  
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
}

# Save the results to a CSV file including the station code, name, coordinates, date, and probability
write.csv(probability_results, file = "materials/probability_results_with_date.csv", row.names = FALSE)

# Print the final table with station names, coordinates, dates, and their probability values
print(probability_results)
