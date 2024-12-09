
############ this is a mini test to include the heat map
# api call, tarspot
call_tarspot_for_station <- function(station_id, risk_threshold, current) {
  tryCatch({
    today_ct <- with_tz(current, tzone = "US/Central")
    out <- from_ct_to_gmt(today_ct, 1.5)
    start_time <- out$start_time_gmt
    end_time <- out$end_time_gmt
    
    # Fetch data
    rh_data <- fetch_rh_above_90_daily(station_id, end_time)
    at_data <- api_call_wisconet_data_daily(station_id, end_time)
    
    # Merge and compute risk
    merged_ds <- merge(
      x = rh_data %>% mutate(date_day = as.Date(adjusted_date)),
      y = at_data %>% mutate(date_day = as.Date(collection_time) - 1),
      by = "date_day"
    ) %>%
      rowwise() %>%
      mutate(
        risk_output = list(get_risk_probability(
          station_id = station_id,
          risk_threshold = risk_threshold,
          mat_30dma = air_temp_avg_c_30d_ma,
          max_rh_30dma = rh_max_30d_ma,
          th_rh90_14ma = rh_above_90_daily_14d_ma,
          url_ts = url_ts
        )),
        Risk = risk_output$Risk,
        Risk_Class = risk_output$Risk_Class
      ) %>%
      select(-risk_output)
    
    return(merged_ds)
  }, error = function(e) {
    stop(paste("Failed to fetch data:", e$message))
  })
}


# Improved date input validation
validate_date <- function(date) {
  # Ensure the date is not in the future
  if (as.Date(date) > Sys.Date()) {
    stop("Date cannot be in the future")
  }
  paste0("The input date is ", date)
  return(as.character(date))
}

# Modify fetch_forecasting_data to use the validation
disease_config <- list(
  tarspot = list(name = "Tar Spot", risk_col = "tarspot_risk"),
  gls = list(name = "Gray Leaf Spot", risk_col = "gls_risk"),
  frogeye_leaf_spot = list(name = "Frogeye Leaf Spot", risk_col = "frogeye_risk")
)

# Transform function for tarspot_risk
transform_tarspot_risk <- function(value) {
  if (is.na(value)) return(NA)
  value <- as.character(value)
  value <- trimws(value)
  value <- gsub("[^0-9.]", "", value)
  return(as.numeric(value))
}

# Main processing function
process_stations_data <- function(stations_data, risk_col) {
  tryCatch({
    # Combine and process data
    stations_df <- bind_rows(lapply(stations_data, function(x) {
      if (is.list(x) && risk_col %in% names(x)) {
        df <- as.data.frame(x, stringsAsFactors = FALSE)
        df[[risk_col]] <- sapply(df[[risk_col]], transform_tarspot_risk)
        return(df)
      } else {
        warning(paste("Missing or invalid column:", risk_col, "in station data"))
        return(NULL)
      }
    }))
    
    
    # Check if risk_col exists in stations_df before mutate
    if (!(risk_col %in% names(stations_df))) {
      stop(paste("Column", risk_col, "is missing in the processed data"))
    }
    
    # Create risk and popup_content columns
    stations_df <- stations_df %>%
      mutate(
        risk = .data[[risk_col]],  # Safely assign the risk column
        popup_content = ifelse(
          is.na(station_name) | is.na(risk),
          "Incomplete info",
          sprintf(
            "<strong>Station:</strong> %s<br><strong>Location:</strong> %s <br><strong>Disease:</strong> %s<br><strong>Region:</strong> %s<br><strong>Risk:</strong> %.1f%%<br><strong>Forecast Date:</strong> %s",
            station_name,
            location,
            risk_col,
            region,
            risk
          )
        )
      ) %>%
      filter(!is.na(risk))  # Remove rows with NA risk
    
    return(stations_df)
  }, error = function(e) {
    warning(paste("Error processing the station info:", e$message))
    return(data.frame(
      station_name = character(),
      longitude = numeric(),
      latitude = numeric(),
      risk = numeric(),
      popup_content = character()
    ))
  })
}


