################################################################################
############################ API CALL EXAMPLE ##################################
################################################################################

install.packages("httr")

# Load the required package
library(httr)

# Define the URL for the API
url <- "https://connect.doit.wisc.edu/forecasting_corn_disease/predict"

# Define the data to be sent in the POST request
body <- list(
  growth_stage = 'R1',
  fungicide_applied = 'no',
  risk_threshold = 35, 
  mean_air_temp_30_day_moving_avg = 17.186667,
  max_relative_humidity_30_day_moving_avg = 95.720,
  total_nighttime_rh_above_90_pct_14_day_moving_avg = 9.357
)

# Make the POST request
response <- POST(url, body = body, encode = "json")

# Check status code and print response content
status_code(response)
content <- content(response, 'text')
print(content)

# Result
# [1] "{\"disease\":[\"TarSpot\"],\"probability\":[8.65],\"risk_class\":[\"Low\"]}"