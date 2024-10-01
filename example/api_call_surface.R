# Required Libraries
library(plotly)  # For 3D plotting
library(httr)    # For making API requests
library(jsonlite) # For handling JSON
library(tidyr)   # For working with data

# Generate the grid for total_nighttime_rh_above_90_pct_14_day_moving_avg and mean_air_temp_30_day_moving_avg
x_range <- seq(0, 14, length.out = 50)  # total_nighttime_rh_above_90_pct_14_day_moving_avg (0-14)
y_range <- seq(16, 26, length.out = 50)  # mean_air_temp_30_day_moving_avg (16-26)

# Create a mesh grid of x and y
grid <- expand.grid(x = x_range, y = y_range)

# API URL
url <- "https://connect.doit.wisc.edu/forecasting_corn_disease/predict_tarspot_risk"

# Initialize a vector to store predictions
z_grid <- rep(NA, nrow(grid))

# Loop through the grid and predict z for each (x, y) combination
for (i in 1:nrow(grid)) {
  x_val <- grid$x[i]
  y_val <- grid$y[i]
  
  # Prepare API data
  api_data <- list(
    growth_stage = 'R1',  # Static value
    fungicide_applied = 'no',  # Static value
    risk_threshold = 50,  # Static value
    meanAT = y_val,
    maxRH = 95.72,  # Static value for maxRH
    rh90_night_tot = x_val
  )
  
  # Make the POST request
  response <- POST(url, body = toJSON(api_data), encode = "json")
  
  # Parse the response if successful
  if (status_code(response) == 200) {
    response_json <- content(response, as = "parsed", type = "application/json")
    prediction <- response_json$probability[[1]]  # Extract the probability from the response
    z_grid[i] <- prediction  # Store the prediction in the z_grid
  } else {
    print(paste("Request failed at index", i))
  }
}

# Reshape the data for 3D plotting (z_grid must be in matrix form for plotly)
z_matrix <- matrix(z_grid, nrow = length(y_range), ncol = length(x_range))

# Create a 3D surface plot
fig <- plot_ly(
  x = x_range, y = y_range, z = z_matrix, 
  type = "surface"
)

# Customize layout
fig <- fig %>% layout(
  title = 'Tar Spot Prediction',
  scene = list(
    xaxis = list(title = 'Total Nigh RH > 90% (14d MA)'),
    yaxis = list(title = 'Mean AT (30d MA)'),
    zaxis = list(title = 'Risk')
  )
)

# Show the plot
fig
#saved as: example/plots/tarspot_example_api_call.png