library(shiny)
library(leaflet)
library(leaflet.extras)
library(readr) 
library(httr) 

# Example
# Save this in fetch_forecasting_data.R or include in server.R
fetch_forecasting_data <- function(date) {
  api_url <- paste0("https://connect.doit.wisc.edu/forecasting_crop_disease/predict_wisconet_stations_risk?date=", date)
  response <- httr::POST(
    url = api_url,
    httr::add_headers("Content-Type" = "application/json")
  )
  if (httr::status_code(response) == 200) {
    response_content <- httr::content(response, as = "parsed", type = "application/json")
    json_string <- response_content$stations_risk[[1]]
    stations_data <- jsonlite::fromJSON(json_string)
    stations_df <- dplyr::bind_rows(lapply(stations_data, dplyr::bind_rows)) %>%
      dplyr::mutate(
        dplyr::across(c(latitude, longitude, tarspot_risk), as.numeric),
        tarspot_risk = 100 * tarspot_risk,
        popup_content = sprintf(
          "<strong>Station:</strong> %s<br><strong>Tar Spot Risk:</strong> %.1f%%",
          station_name,
          tarspot_risk
        )
      )
    return(stations_df)
  } else {
    stop(paste("Error:", httr::status_code(response)))
  }
}
