for (station_code in names(stations)) {
  station <- stations[[station_code]]
  
  # Get station name
  station_name <- station$name
  cat("================== ", station_name, station_code, "\n")
  
  # Fetch relative humidity data
  rh_above_90_daily <- fetch_rh_above_90_daily(station_code)
  
  # Check if rh_above_90_daily is NULL
  if (is.null(rh_above_90_daily)) {
    cat("Error: No data returned for station", station_code, "\n")
    probability_results <- rbind(probability_results, data.frame(
      Station = station_code,
      Probability = -999  # Handle error case with -999
    ))
    next  # Skip to the next iteration
  }
  
  rh_above_90_daily <- rh_above_90_daily %>% mutate(date_day = floor_date(date, unit='days'))
  rh_above_90_daily1 <- rh_above_90_daily %>% slice(8)
  th_rh90_14ma <- rh_above_90_daily1$rh_above_90_daily_14d_ma[1]
  
  # Fetch temperature data
  at0 <- fetch_at(station_code)
  
  # Check if at0 is NULL
  if (is.null(at0)) {
    cat("Error: No data returned for station", station_code, "\n")
    probability_results <- rbind(probability_results, data.frame(
      Station = station_code,
      Probability = -999  # Handle error case with -999
    ))
    next  # Skip to the next iteration
  }
  
  at <- at0 %>% slice(1)
  mat_30dma <- at$air_temp_avg_c_30d_ma[1]
  max_rh_30dma <- at$rh_max_30d_ma[1]
  cat('th_rh90_14ma ', th_rh90_14ma,
      'max_rh_30dma ', max_rh_30dma,
      'mat_30dma ', mat_30dma, "\n")
  
  base_url <- "https://connect.doit.wisc.edu/forecasting_crop_disease/predict_tarspot_risk"
  
  params <- list(
    growth_stage = 'R1',
    fungicide_applied = 'no',
    risk_threshold = risk_threshold * 100,  # Ensure it's in percentage format
    mean_air_temp_30d_ma = mat_30dma,
    max_rh_30d_ma = max_rh_30dma,
    tot_nhrs_rh90_14d_ma = th_rh90_14ma
  )
  
  # API call to fetch the probability data
  response <- POST(url = base_url, query = params)
  
  if (status_code(response) == 200) {
    # Parse the content as JSON
    result <- content(response, as = "parsed", type = "application/json")
    
    # Retrieve relevant values
    probability <- result$probability[[1]]
    probability_class <- result$risk_class[[1]]
  
    cat('====Risk====', probability, probability_class, "\n")
    
    # Store the station name and its probability in the results data frame
    probability_results <- rbind(probability_results, data.frame(
      Station = station_code,
      Probability = probability  # Use the correct key here
    ))
  } else {
    cat("Error: API request failed with status code", status_code(response), "\n")
    probability_results <- rbind(probability_results, data.frame(
      Station = station_code,
      Probability = -999  # Handle error case with -999
    ))
  }
}

# Print the final table with station names and their probability values
print(probability_results)
