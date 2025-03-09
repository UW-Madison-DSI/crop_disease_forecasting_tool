# global.R o al inicio de app.R
library(shiny)
library(httr2)
library(jsonlite)
library(dplyr)
library(memoise)

popup_content_str <- "<strong>Station:</strong> %s<br><strong>Location:</strong> %s <br><strong>Region:</strong> %s<br><strong>Forecasting Date:</strong> %s<br><strong>Risk Models</strong><br><strong>Tarspot:</strong> %.2f%%<br><strong>Frogeye Leaf Spot:</strong> %.2f%%<br><strong>Gray Leaf Spot:</strong> %.2f%%<br><strong>Whitemold Irrigation (30in):</strong> %.2f%%<br><strong>Whitemold Irrigation (15in):</strong> %.2f%%"
base_url <- "https://connect.doit.wisc.edu/pywisconet_wrapper/ag_models_wrappers/wisconet"

# simple function not cache
fetch_forecasting_data_uncached <- function(forecast_date) {
  tryCatch({
    converted_date <- as.Date(forecast_date, format = "%Y-%m-%d")
    init <- Sys.time()
    
    req <- request(base_url) %>%
      req_url_query(
        forecasting_date = format(converted_date, "%Y-%m-%d"),
        risk_days = 7
      ) %>%
      req_headers("Accept" = "application/json")
    
    response <- req_perform(req)
    
    if (resp_status(response) == 200) {
      data <- fromJSON(resp_body_string(response)) %>%
        mutate(
          popup_content = sprintf(
            popup_content_str,
            station_name,
            location,
            region,
            forecasting_date,
            tarspot_risk * 100,
            fe_risk * 100,
            gls_risk * 100,
            whitemold_irr_30in_risk * 100,
            whitemold_irr_15in_risk * 100
          )
        )
      
      end_time <- Sys.time()
      cat(paste("\nTime to retrieve api call response:", round(end_time - init, 2), "secs\n"))
      return(data)
    } else {
      cat("Request failed with status code:", resp_status(response), "\n")
      cat("Response body:\n", resp_body_string(response), "\n")
      return(NULL)
    }
    
  }, error = function(e) {
    message(paste0("Error processing data: ", e$message))
    return(NULL)
  })
}

# Cache config in filesystem
cache_dir <- "api_cache"
if (!dir.exists(cache_dir)) dir.create(cache_dir)
cache_backend <- cache_filesystem(cache_dir)

# memoized version of the function
fetch_forecasting_data <- memoise(fetch_forecasting_data_uncached, cache = cache_backend)
