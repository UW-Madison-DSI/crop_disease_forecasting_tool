# Required Libraries
library(plotly)  # For 3D plotting
library(httr)    # For making API requests
library(jsonlite) # For handling JSON
library(tidyr)   # For working with data

# API URL
url <- "https://connect.doit.wisc.edu/forecasting_crop_disease"
url_ts <- paste0(url, "/predict_tarspot_risk")

# SETTINGS
n <- 100  # Grid size
mean_air_temp_range <- seq(16, 26, length.out = n)  # Range of mean air temperatures
tot_hrs_rh90_range <- seq(2, 14, length.out = n)  # Range of total hours RH>90%

# Create a mesh grid of mean_air_temp_30d_ma and tot_hrs_rh90_14d_ma
grid <- expand.grid(mean_air_temp = mean_air_temp_range, tot_hrs_rh90 = tot_hrs_rh90_range)

# Initialize a vector to store probabilities
prob_grid <- rep(NA, nrow(grid))

# Loop through the grid and call the API for each combination
for (i in 1:nrow(grid)) {
  mat_val <- grid$mean_air_temp[i]
  rh90_val <- grid$tot_hrs_rh90[i]
  
  # Define the data to be sent in the POST request
  body <- list(
    growth_stage = 'R1',
    fungicide_applied = 'no',
    risk_threshold = 35,
    mean_air_temp_30d_ma = mat_val,
    max_rh_30d_ma = 85.72,  # Fixed value for max RH
    tot_hrs_rh90_14d_ma = rh90_val
  )
  
  # Make the POST request
  response <- POST(url_ts, body = body, encode = "json")
  
  # Check the status code
  if (status_code(response) == 200) {
    # Parse the content as JSON
    response_content <- content(response, as = "parsed", type = "application/json")
    
    # Access the 'probability' field and store it in prob_grid
    prob_grid[i] <- response_content$probability[[1]]
  } else {
    print(paste("Request failed at index", i, "with status code", status_code(response)))
  }
}

# Reshape the probability data into a matrix for 3D plotting
prob_matrix <- matrix(prob_grid, nrow = n, ncol = n)

# Create a 3D surface plot of the results
fig <- plot_ly(
  x = tot_hrs_rh90_range,   # Y-axis: total hours RH>90%
  y = mean_air_temp_range,  # X-axis: mean air temp
  z = prob_matrix,          # Z-axis: probability (API result)
  type = "surface",
  colorscale = "Viridis"     # Color scale for the plot
)

# Customize layout with appropriate axis labels
fig <- fig %>% layout(
  title = 'Tar Spot Prediction - Fig 4b',
  scene = list(
    xaxis = list(title = 'Total Hours RH > 90% (14d MA)'),
    yaxis = list(title = 'Mean Air Temp (30d MA)'),
    zaxis = list(title = 'Risk Probability (%)'),
    colorbar = list(title = 'Risk')
  )
)

# Show the plot
fig
