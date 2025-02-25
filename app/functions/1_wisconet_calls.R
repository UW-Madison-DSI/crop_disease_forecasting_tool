################################################################################
##########  Main function on All Wisconet Stations Forecasting Data    #########
################################################################################

library(httr2)
library(jsonlite)
library(dplyr)
library(memoise)
library(httr2)

popup_content_str <- "<strong>Station:</strong> %s<br><strong>Location:</strong> %s <br><strong>Region:</strong> %s<br><strong>Forecasting Date:</strong> %s<br><strong>Risk Models</strong><br><strong>Tarspot:</strong> %.2f%%<br><strong>Frogeye Leaf Spot:</strong> %.2f%%<br><strong>Gray Leaf Spot:</strong> %.2f%%<br><strong>Whitemold Irrigation (30in):</strong> %.2f%%<br><strong>Whitemold Irrigation (15in):</strong> %.2f%%"
base_url <- "https://connect.doit.wisc.edu/pywisconet_wrapper/ag_models_wrappers/wisconet"


fetch_forecasting_data <- memoise(function(forecast_date) {
  tryCatch({
    start_time <- Sys.time()
    converted_date <- as.Date(forecast_date, format = "%Y-%m-%d")
    formatted_date <- format(converted_date, "%Y-%m-%d")
    
    req <- request(base_url) %>%
      req_url_query(
        forecasting_date = formatted_date,  # Use the formatted string
        risk_days = 1
      ) %>%
      req_headers("Accept" = "application/json")
    
    response <- req_perform(req)
    
    if (resp_status(response) == 200) {
      # Use resp_body_string() to get the response text, then parse with fromJSON()
      data <- fromJSON(resp_body_string(response))
      return(data %>%
               mutate(
                 #forecasting_date = as.Date(date)+1,
                 popup_content = sprintf(
                   popup_content_str,
                   station_name,
                   location,
                   region,
                   forecasting_date,
                   tarspot_risk * 100,
                   fe_risk * 100,
                   gls_risk * 100,
                   #whitemold_nirr_risk * 100,
                   whitemold_irr_30in_risk * 100,
                   whitemold_irr_15in_risk * 100
                 )
               ))
      
    } else {
      cat("Request failed with status code:", resp_status(response), "\n")
      cat("Response body:\n", resp_body_string(response), "\n")
      return(NULL)
    }
    
  }, error = function(e) {
    message(paste0("Error processing data: ", e$message))
    return(NULL)
  })
})


install.packages("arrow")

# Load the arrow package
library(arrow)

historical_data <- read_parquet("snapshot_0224_0225_stations.parquet")
