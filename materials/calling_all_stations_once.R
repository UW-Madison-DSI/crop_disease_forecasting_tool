library(shiny)
library(leaflet)
library(shinydashboard)
library(scales)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(dplyr)
library(flexdashboard)

source("functions/stations.R")
source("functions/logic.R")

# Set risk threshold (modify as needed)
# Set risk threshold (modify as needed)
risk_threshold <- 35

# Initialize an empty data frame to store the results
probability_results <- data.frame(
  Station = character(),
  Probability = numeric(),
  stringsAsFactors = FALSE
)

# Loop through the list of stations
for (station_code in names(stations)) {
  station <- stations[[station_code]]
  
  # Get station name
  station_name <- station$name
  
  # Call the tarspot function for each station
  result <- call_tarspot_for_station(station_code, station_name, risk_threshold)
  print(result)
  # Extract the probability value
  probability <- result$Risk
  
  # Store the station name and its probability in the results data frame
  probability_results <- rbind(probability_results, data.frame(
    Station = station_code,
    Probability = probability
  ))
}

# Print the final table with station names and their probability values
print(probability_results)