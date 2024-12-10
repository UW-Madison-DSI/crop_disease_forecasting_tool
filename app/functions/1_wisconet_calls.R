################################################################################
############      Main function on All stations Forecasting Data    ############
################################################################################

url_all_stations_api <- "https://connect.doit.wisc.edu/forecasting_crop_disease/predict_wisconet_stations_risk?forecasting_date=%s&disease_name=%s"


fetch_forecasting_data <- function(date, disease_name, input) {
  tryCatch({
    api_url <- sprintf(
      url_all_stations_api,
      date, disease_name
    )
    response <- POST(
      url = api_url,
      add_headers("Content-Type" = "application/json")
    )
    
    if (status_code(response) != 200) {
      stop(paste("API Error:", status_code(response), "Message:", content(response, as = "text")))
    }
    
    response_content <- content(response, as = "parsed", type = "application/json")
    
    if (is.null(response_content$stations_risk) || length(response_content$stations_risk) == 0) {
      stop("No stations_risk data in API response")
    }
    
    stations_data <- fromJSON(response_content$stations_risk[[1]])
    stations_df <- bind_rows(lapply(stations_data, bind_rows))

    
    if (disease_name == 'tarspot') {
      stations_df <- stations_df %>%
        mutate(
          across(c(latitude, longitude, tarspot_risk), as.numeric),
          risk = 100 * tarspot_risk,  # Scale risk
          popup_content = sprintf(
            "<strong>Station:</strong> %s<br><strong>Location:</strong> %s <br><strong>Region:</strong> %s<br><strong>Tar Spot Risk:</strong> %.1f%%<br><strong>Forecast Date:</strong> %s",
            station_name,
            location,
            region,
            risk,
            date
          )
        )
    }
    
    if (disease_name == 'gls') {
      stations_df <- stations_df %>%
        mutate(
          across(c(latitude, longitude, gls_risk), as.numeric),
          risk = 100 * gls_risk,  # Scale risk
          popup_content = sprintf(
            "<strong>Station:</strong> %s<br><strong>Location:</strong> %s <br><strong>Region:</strong> %s<br><strong>Gray Leaf Spot Risk:</strong> %.1f%%<br><strong>Forecast Date:</strong> %s",
            station_name,
            location,
            region,
            risk,
            date
          )
        )
    }  
    
    if (disease_name == 'frogeye_leaf_spot') {
      stations_df <- stations_df %>%
        mutate(
          across(c(latitude, longitude, frogeye_risk), as.numeric),
          risk = 100 * frogeye_risk,  # Scale risk
          popup_content = sprintf(
            "<strong>Station:</strong> %s<br><strong>Location:</strong> %s <br><strong>Region:</strong> %s<br><strong>Frog Eye Leaf Spot Risk:</strong> %.1f%%<br><strong>Forecast Date:</strong> %s",
            station_name,
            location,
            region,
            risk,
            date
          )
        )
    }
    
    if (disease_name == 'whitemold_irr_15in') {
      stations_df <- stations_df %>%
        mutate(
          across(c(latitude, longitude, whitemold_irr_15in_risk), as.numeric),
          risk = 100 * whitemold_irr_15in_risk,  # Scale risk
          popup_content = sprintf(
            "<strong>Station:</strong> %s<br><strong>Location:</strong> %s <br><strong>Region:</strong> %s<br><strong>Whitemold Irrigation Risk:</strong> %.1f%%<br><strong>Forecast Date:</strong> %s",
            station_name,
            location,
            region,
            risk,
            date
          )
        )
    }
    
    if (disease_name == 'whitemold_irr_30in') {
      stations_df <- stations_df %>%
        mutate(
          across(c(latitude, longitude, whitemold_irr_30in_risk), as.numeric),
          risk = 100 * whitemold_irr_30in_risk,  # Scale risk
          popup_content = sprintf(
            "<strong>Station:</strong> %s<br><strong>Location:</strong> %s <br><strong>Region:</strong> %s<br><strong>Whitemold Irrigation Risk:</strong> %.1f%%<br><strong>Forecast Date:</strong> %s",
            station_name,
            location,
            region,
            risk,
            date
          )
        )
    }
    
    if (disease_name == 'whitemold_noirr') {
      stations_df <- stations_df %>%
        mutate(
          across(c(latitude, longitude, whitemold_noirr_risk), as.numeric),
          risk = 100 * whitemold_noirr_risk,  # Scale risk
          popup_content = sprintf(
            "<strong>Station:</strong> %s<br><strong>Location:</strong> %s <br><strong>Region:</strong> %s<br><strong>Whitemold Irrigation Risk:</strong> %.1f%%<br><strong>Forecast Date:</strong> %s",
            station_name,
            location,
            region,
            risk,
            date
          )
        )
    }

    return(stations_df)
  }, error = function(e) {
    message(paste0("Error processing data: ", e$message))
    return(NULL)
  })
}