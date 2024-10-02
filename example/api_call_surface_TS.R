# Required Libraries
library(plotly)  # For 3D plotting
library(httr)    # For making API requests
library(jsonlite) # For handling JSON
library(tidyr)   # For working with data


url <- "https://connect.doit.wisc.edu/forecasting_crop_disease"

url_ts <- paste0(url, "/predict_tarspot_risk")
####### SETTINGS
# Generate the grid for total_nighttime_rh_above_90_pct_14_day_moving_avg and mean_air_temp_30_day_moving_avg
n<-50
x_range <- seq(2, 14, length.out = n)  # total_nighttime_rh_above_90_pct_14_day_moving_avg (0-14)
y_range <- seq(14, 26, length.out = n)  # mean_air_temp_30_day_moving_avg (16-26)

# Create a mesh grid of x and y
grid <- expand.grid(x = x_range, y = y_range)

####### API CALL ON GRID

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
    risk_threshold = 40,  # Static value
    mean_air_temp_30d_ma = y_val,
    max_rh_30d_ma = 95.72,  # Static value for maxRH
    tot_hrs_rh90_14d_ma = x_val
  )
  # Make the POST request
  response <- POST(url_ts, body = api_data, encode = "json")
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

############## PLOT 3D surface
# Create a 3D surface plot and label the palette as "risk"
fig <- plot_ly(
  y = x_range, x = y_range, z = z_matrix, 
  type = "surface",
  colorscale = "Viridis"  # You can choose a different color scale
)

# Customize layout with risk label for the color palette
fig <- fig %>% layout(
  title = 'Tar Spot Prediction',
  scene = list(
    yaxis = list(title = 'TotalNighHrRH>90(14dMA)'),
    xaxis = list(title = 'MeanAT(30dMA)'),
    zaxis = list(title = 'Risk Probab (%)'),
    colorbar = list(title = 'Risk')  # Label the color bar as "Risk"
  )
)

# Show the plot
fig
