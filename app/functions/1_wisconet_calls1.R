library(shiny)
library(httr2)
library(jsonlite)
library(dplyr)
library(memoise)
library(tidyr)

base_url <- "https://connect.doit.wisc.edu/pywisconet_wrapper"

popup_content_str <- paste0(
  "<strong>Station:</strong> <mark>%s</mark><br>",
  #"<strong>Location:</strong> %s<br>",
  "<strong>Region:</strong> %s<br>",
  "<strong><mark>Corn Crop Disease Forecasting</mark></strong><br>",
  "<strong>Tarspot Risk:</strong> %s<br>",
  "<strong>Frogeye Leaf Spot Risk:</strong> %s<br>",
  "<strong>Gray Leaf Spot Risk:</strong> %s<br>",
  "<strong><mark>Soybean Crop Disease Forecasting</mark></strong><br>",
  "<strong>Whitemold Non-Irrigated Risk:</strong> %s<br>",
  "<strong>Whitemold Irrigation (30in) Risk:</strong> %s<br>",
  "<strong>Whitemold Irrigation (15in) Risk:</strong> %s"
)

fetch_forecasting_data_uncached <- function(forecast_date, risk_days = 7) {
  tryCatch({
    # 1) parse the date
    d <- as.Date(forecast_date, "%Y-%m-%d")
    url <- paste0(base_url, "/ag_models_wrappers/wisconet")
    print(paste("Calling URL:", url))
    print(paste("Date:", format(d, "%Y-%m-%d")))
    print(paste("Risk days:", risk_days))
    
    # 2) call the endpoint
    resp <- request(url) %>%
      req_url_query(
        forecasting_date = format(d, "%Y-%m-%d"),
        risk_days        = risk_days
      ) %>%
      req_headers(Accept = "application/json") %>%
      req_perform()
    
    print(paste("HTTP Status:", resp_status(resp)))
    
    if (resp_status(resp) != 200) {
      stop("API returned HTTP ", resp_status(resp))
    }
    
    # 3) flatten JSON into a data.frame
    raw_json <- resp_body_string(resp)
    print("Raw JSON response (first 500 chars):")
    print(substr(raw_json, 1, 500))
    
    raw <- fromJSON(raw_json, flatten = TRUE)
    df  <- as_tibble(raw)
    
    print("Data structure:")
    print(str(df))
    print("Column names:")
    print(colnames(df))
    print("First few rows:")
    print(head(df))
    
    # Check if required columns exist
    required_cols <- c("station_name", "city", "county", "region", "forecasting_date",
                       "tarspot_risk_class", "fe_risk_class", "gls_risk_class",
                       "whitemold_nirr_risk_class", "whitemold_irr_30in_class", 
                       "whitemold_irr_15in_class")
    
    missing_cols <- setdiff(required_cols, colnames(df))
    if (length(missing_cols) > 0) {
      warning("Missing columns: ", paste(missing_cols, collapse = ", "))
      print("Available columns:")
      print(colnames(df))
    }
    
    # 4) build your popup_content with safer column access
    df_result <- df %>%
      mutate(
        popup_content = sprintf(
          popup_content_str,
          if("station_name" %in% colnames(df)) station_name else "Unknown Station",               # 1                                                                              # 2
          if("region" %in% colnames(df)) region else "Unknown Region",                          # 3
          #format(forecasting_date_clean - 1, "%Y-%m-%d"),                                       # 4
          if("tarspot_risk_class" %in% colnames(df)) tarspot_risk_class else "N/A",            # 5
          if("fe_risk_class" %in% colnames(df)) fe_risk_class else "N/A",                      # 6
          if("gls_risk_class" %in% colnames(df)) gls_risk_class else "N/A",                    # 7
          if("whitemold_nirr_risk_class" %in% colnames(df)) whitemold_nirr_risk_class else "N/A", # 8
          if("whitemold_irr_30in_class" %in% colnames(df)) whitemold_irr_30in_class else "N/A",   # 9
          if("whitemold_irr_15in_class" %in% colnames(df)) whitemold_irr_15in_class else "N/A"    # 10
        )
      )
    
    return(df_result)
    
  }, error = function(e) {
    message("Error fetching data: ", e$message)
    print("Full error details:")
    print(e)
    return(NULL)
  })
}

# optional filesystem cache
cache_dir <- "api_cache"
if (!dir.exists(cache_dir)) dir.create(cache_dir)

fetch_forecasting_data <- memoise(
  fetch_forecasting_data_uncached,
  cache = cache_filesystem(cache_dir)
)

# Debug example with better error handling
#cat("Testing API call...\n")
#res <- fetch_forecasting_data("2025-05-27")
#print(res)

# Debug example with better error handling
#cat("Testing API call...\n")
#res <- fetch_forecasting_data("2025-05-23", 1)



####################################################### weather for an specific station id


fetch_bulk_measures <- function(station_id, forecast_date, days = 30) {
  end_date   <- as.Date(forecast_date, "%Y-%m-%d")-1
  start_date <- end_date - days
  
  url <- paste0(base_url, "/bulk_measures/", station_id)
  
  resp <- request(url) %>%
    req_url_query(
      start_date   = format(start_date, "%Y-%m-%d"),
      end_date     = format(end_date,   "%Y-%m-%d"),
      measurements = "ALL",
      frequency    = "DAILY"
    ) %>%
    req_headers(Accept = "application/json") %>%
    req_perform() %>%
    resp_check_status()
  
  # 1) grab the raw JSON list
  #raw <- resp_body_json(resp)
  raw <- fromJSON(resp_body_string(resp), flatten = TRUE)
  df  <- as_tibble(raw)
  # 2) bind it into a tibble
  #df <- bind_rows(raw)
  
  return(df)
}

# — Example usage —