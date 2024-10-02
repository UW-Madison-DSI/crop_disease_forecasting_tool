# Required Libraries
library(plotly)  # For 3D plotting
library(httr)    # For making API requests
library(jsonlite) # For handling JSON
library(tidyr)   # For working with data

# API URL
url <- "https://connect.doit.wisc.edu/forecasting_crop_disease"
url_ts <- paste0(url, "/predict_tarspot_risk")

# SETTINGS
n <- 50  # Grid size
mean_air_temp_range <- seq(16, 26, length.out = n)  # Range of mean air temperatures
max_rh_range <- seq(80, 100, length.out = n)  # Range of max relative humidity

# Create a mesh grid of mean_air_temp_30d_ma and max_rh_30d_ma
grid <- expand.grid(mean_air_temp = mean_air_temp_range, max_rh = max_rh_range)

# Initialize a vector to store probabilities
prob_grid <- rep(NA, nrow(grid))

# Loop through the grid and call the API for each combination
for (i in 1:nrow(grid)) {
  mat_val <- grid$mean_air_temp[i]
  rh_val <- grid$max_rh[i]
  
  # Define the data to be sent in the POST request
  body <- list(
    growth_stage = 'R1',
    fungicide_applied = 'no',
    risk_threshold = 35,
    mean_air_temp_30d_ma = mat_val,
    max_rh_30d_ma = rh_val,
    tot_hrs_rh90_14d_ma = 10
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

head(prob_matrix)


# Reshape the probability data into a matrix for 3D plotting
prob_matrix <- matrix(prob_grid, nrow = n, ncol = n)

# Create a 3D surface plot of the results
fig <- plot_ly(
  x = max_rh_range,         # Y-axis: max RH
  y = mean_air_temp_range,  # X-axis: mean air temp
  z = prob_matrix,          # Z-axis: probability (API result)
  type = "surface",
  colorscale = "Viridis"     # Color scale for the plot
)

# Customize layout with appropriate axis labels
fig <- fig %>% layout(
  title = 'Tar Spot Prediction - Fig 4a',
  scene = list(
    xaxis = list(title = 'Max RH (30d MA)'),
    yaxis = list(title = 'Mean Air Temp (30d MA)'),
    zaxis = list(title = 'Risk Probability (%)'),
    colorbar = list(title = 'Risk')
  )
)

# Show the plot
fig
