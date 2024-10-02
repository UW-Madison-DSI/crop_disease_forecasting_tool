# Required Libraries
library(plotly)  # For 3D plotting
library(httr)    # For making API requests
library(jsonlite) # For handling JSON
library(tidyr)   # For working with data

################ CHECK

####### Settings
url <- "https://connect.doit.wisc.edu/forecasting_crop_disease"

url_gls <- paste0(url, "/predict_gray_leaf_spot_risk")

# Generate the grid 
n<-50
x_range <- seq(0, 22, length.out = n)  # min_air_temp_21d_ma C
y_range <- seq(4, 21, length.out = n)  # min_dp_30d_ma C

# Create a mesh grid of x and y
grid <- expand.grid(x = x_range, y = y_range)

# API URL

# Initialize a vector to store predictions
z_grid <- rep(NA, nrow(grid))


####### API CALL ON GRID
# Loop through the grid and predict z for each (x, y) combination
for (i in 1:nrow(grid)) {
  x_val <- grid$x[i]
  y_val <- grid$y[i]
  
  # Prepare API data
  api_data <- list(
    growth_stage = 'R1',
    fungicide_applied = 'no',
    risk_threshold = 60, 
    min_air_temp_21d_ma = x_val,
    min_dewpoint_30d_ma = y_val
  )
  # Make the POST request
  response <- POST(url_gls, body = api_data, encode = "json")
  # Parse the response if successful
  if (status_code(response) == 200) {
    response_json <- content(response, as = "parsed", type = "application/json")
    prediction <- response_json$probability[[1]]  # Extract the probability from the response
    z_grid[i] <- prediction  # Store the prediction in the z_grid
  } else {
    print(paste("Request failed at index", i))
  }
}

############## PLOT 3D surface

# Reshape the data for 3D plotting (z_grid must be in matrix form for plotly)
z_matrix <- matrix(z_grid, nrow = length(x_range), ncol = length(y_range))

# Create a 3D surface plot and label the palette as "risk"
fig <- plot_ly(
  x = x_range, y = y_range, z = z_matrix, 
  type = "surface",
  colorscale = "Viridis"  # You can choose a different color scale
)

# Customize layout with risk label for the color palette
fig <- fig %>% layout(
  title = 'GL Spot Prediction',
  scene = list(
    xaxis = list(title = 'MeanAT(30dMA)'),
    yaxis = list(title = 'min_dp_30d_ma'),
    zaxis = list(title = 'Risk Probab (%)'),
    colorbar = list(title = 'Risk')  # Label the color bar as "Risk"
  )
)

# Show the plot
fig
