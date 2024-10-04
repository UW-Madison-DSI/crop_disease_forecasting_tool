api_call_wisconet <- function(station) {
  base_url <- 'https://wisconet.wisc.edu'  # Define the base URL
  
  endpoint <- paste0('/api/v1/stations/', station$station_id, '/measures')
  
  params <- list(
    end_time = 1728011869,
    start_time = 1726715869,
    fields = 'daily_air_temp_f_max,daily_air_temp_f_min,daily_relative_humidity_pct_max'
  )
  
  # Make the API request
  response <- GET(url = paste0(base_url, endpoint), query = params)
  
  # Check if the request was successful
  if (response$status_code == 200) {
    data1 <- fromJSON(content(response, as = "text"), flatten = TRUE)
    data <- data1$data
    
    # Extract the data and create a DataFrame
    extracted_data <- lapply(data, function(entry) {
      air_temp_max <- NA
      air_temp_min <- NA
      rh_max <- NA
      
      # Extract the measurements
      for (measure in entry$measures) {
        if (measure[1] == 4) {  # Air Temp Max
          air_temp_max <- measure[2]
        }
        if (measure[1] == 6) {  # Air Temp Min
          air_temp_min <- measure[2]
        }
        if (measure[1] == 20) {  # Relative Humidity Max
          rh_max <- measure[2]
        }
      }
      
      return(data.frame(
        collection_time = as.POSIXct(entry$collection_time, origin = "1970-01-01"),
        air_temp_avg = mean(c(air_temp_max, air_temp_min), na.rm = TRUE),
        rh_max = rh_max
      ))
    })
    
    # Convert to a single data frame
    df <- bind_rows(extracted_data)
    return(df)  # Return the data frame to the caller
    
  } else {
    print(paste("Error: ", response$status_code))
    return(NULL)  # Return NULL in case of an error
  }
}

