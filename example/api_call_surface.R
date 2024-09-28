###############################################################################
########################### REPLICATING FIGURES 6 PAPER, SURFACE ON GRID
###############################################################################

# Install and load required packages
if (!require(plotly)) install.packages("plotly")
if (!require(httr)) install.packages("httr")
if (!require(jsonlite)) install.packages("jsonlite")

library(plotly)
library(httr)
library(jsonlite)

# Sample Data
# Sample Data
n <- 5
temp <- seq(9.58, 20.580000, length.out = n)  # Adjusted for example
humidity <- seq(82.310000, 99.5, length.out = n)  # Adjusted for example
total_rh <- seq(4.01, 14.9, length.out = n)  # Adjusted for example

# Create a grid of all combinations
grid <- expand.grid(temp = temp, humidity = humidity, total_rh = total_rh)

# Function to call the API and get the probability (assuming it's defined as before)
get_probability <- function(temp, humidity, total_rh) {
  url <- "https://connect.doit.wisc.edu/forecasting_corn_disease/predict"
  body <- list(
    growth_stage = 'R1',
    fungicide_applied = 'no',
    risk_threshold = 35, 
    mean_air_temp_30_day_moving_avg = temp,
    max_relative_humidity_30_day_moving_avg = humidity,
    total_nighttime_rh_above_90_pct_14_day_moving_avg = total_rh
  )
  
  # Convert body to JSON and send the POST request
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  response <- httr::POST(url, body = body_json, httr::content_type_json())
  
  # Check the response status and handle errors
  if (httr::status_code(response) == 200) {
    content_raw <- httr::content(response, 'text', encoding = "UTF-8")
    probability <- as.numeric(jsonlite::fromJSON(content_raw)['probability'])
    #jsonlite::fromJSON(content_raw))
    return(probability)
  } else {
    warning(paste("API call failed with status code:", httr::status_code(response)))
    return(NA)
  }
}

# Use mapply to call the API for each row and store the result in a new column 'z'
# Capture warnings to check failed API calls
grid$z <- tryCatch({
  mapply(get_probability, grid$temp, grid$humidity, grid$total_rh)
}, warning = function(w) {
  message("Warning caught: ", conditionMessage(w))
  return(rep(NA, nrow(grid)))  # Return NAs in case of failure
}, error = function(e) {
  message("Error caught: ", conditionMessage(e))
  return(rep(NA, nrow(grid)))  # Return NAs in case of error
})

print(grid)

###############################################################################
# Ensure you have plotly installed
if (!require(plotly)) install.packages("plotly")
library(plotly)



# Number of unique temperatures and humidities
n_temp <- length(unique(grid$temp))
n_humidity <- length(unique(grid$humidity))

# Reshape the z values to a matrix for surface plot
z_matrix <- matrix(grid$z, nrow = n_temp, ncol = n_humidity)

# Reshape temp and humidity to grids (optional, but for clarity)
temp_grid <- matrix(grid$temp, nrow = n_temp, ncol = n_humidity)
humidity_grid <- matrix(grid$humidity, nrow = n_temp, ncol = n_humidity)

# Create the 3D surface plot
plot <- plot_ly(x= ~humidity_grid, y = ~temp_grid, z = ~z_matrix, type = "surface") %>%
  layout(
    scene = list(
      xaxis = list(title = "Max Relative Humidity (30-day Moving Avg)"),
      yaxis = list(title = "Mean Air Temp (30-day Moving Avg)"),
      zaxis = list(title = "Probability")
    ),
    title = "3D Surface Plot of Corn Disease Probability"
  )

# Display the plot
plot
