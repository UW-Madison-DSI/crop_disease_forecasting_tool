library(shiny)
library(httr2)
library(jsonlite)
library(dplyr)
library(memoise)

base_url <- "https://connect.doit.wisc.edu/pywisconet_wrapper/ag_models_wrappers/wisconet"

popup_content_str <- paste0(
  "<strong>Station:</strong> %s<br>",
  "<strong>Location:</strong> %s<br>",
  "<strong>Region:</strong> %s<br>",
  "<strong>Forecasting Date:</strong> %s<br>",
  "<strong><mark>Corn Crop Disease Forecasting</mark></strong><br>",
  "<strong>Tarspot Risk:</strong> %s<br>",
  "<strong>Frogeye Leaf Spot Risk:</strong> %s<br>",
  "<strong>Gray Leaf Spot Risk:</strong> %s<br>",
  "<strong><mark>Soybean Crop Disease Forecasting</mark></strong><br>",
  "<strong>Whitemold Non-Irrigated Risk:</strong> %s<br>",
  "<strong>Whitemold Irrigation (30in) Risk:</strong> %s<br>",
  "<strong>Whitemold Irrigation (15in) Risk:</strong> %s"
)


# simple function not cache
fetch_forecasting_data_uncached <- function(forecast_date) {
  tryCatch({
    converted_date <- as.Date(forecast_date-1, format = "%Y-%m-%d")
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
            as.character(station_name),
            as.character(location),
            as.character(region),
            as.character(forecasting_date),
            as.character(tarspot_risk_class),
            as.character(fe_risk_class),
            as.character(gls_risk_class),
            as.character(whitemold_nirr_risk_class),  # updated column name
            as.character(whitemold_irr_30in_class),
            as.character(whitemold_irr_15in_class)
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
