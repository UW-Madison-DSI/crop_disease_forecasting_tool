################################################################################
################### API CALL EXAMPLE - single point ############################
################################################################################

install.packages("httr")

# Load the required package
library(httr)

# Define the URL for the API
url <- "https://connect.doit.wisc.edu/forecasting_corn_disease/predict_tarspot_risk"


# Define the data to be sent in the POST request
body <- list(
  growth_stage = 'R1',
  fungicide_applied = 'no',
  risk_threshold = 35, 
  meanAT = 17.186667,
  maxRH = 95.720,
  rh90_night_tot = 9.357
)

# Make the POST request
response <- POST(url, body = body, encode = "json")

# Check status code and print response content
status_code(response)
content <- content(response, 'text')
print(content)

# Result
# [1] "{\"disease\":[\"TarSpot\"],\"probability\":[91.35],\"risk_class\":[\"High\"]}"