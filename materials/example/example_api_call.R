printList <- function(list) {
  
  for (item in 1:length(list)) {
    
    print(head(list[[item]]))
    
  }
}

library(listviewer)

################################################################################
################### API CALL EXAMPLE - single point ############################
################################################################################

# Load the required library
library(httr)

# Base URL for the API
base_url <- "https://connect.doit.wisc.edu/forecasting_crop_disease/predict_tarspot_risk"


# Query parameters as a list
params <- list(
  growth_stage = 'yes',
  fungicide_applied = 'no',
  risk_threshold = 35,  # percentage
  mean_air_temp_30d_ma = 24,
  max_rh_30d_ma = 97.56667,
  tot_nhrs_rh90_14d_ma = 6.142857
)

# Make the GET request
response <- POST(url = base_url, query = params)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the content as JSON
  result <- content(response, as = "parsed", type = "application/json")
  content <- content(response, 'text')
  print(content)
  # Retrieve only the probability value
  probability <- result$probability[[1]]  # Accessing the first element in the list
  cat("Probability:", probability, "\n")
  
} else {
  # Print error if the request fails
  cat("Error: API request failed with status code", status_code(response), "\n")
  print(content(response, as = "text"))
}


# Result
#[1] "{\"code\":[\"Success\"],\"disease\":[\"TarSpot\"],\"probability\":[3.35],\"risk_class\":[\"Low\"]}"



############################################################################ Gray Leaf Spot
url_gls <- paste0(url, "/predict_gray_leaf_spot_risk")


# Define the data to be sent in the POST request
body <- list(
  growth_stage = 'yes',
  fungicide_applied = 'no',
  risk_threshold = 60, 
  min_air_temp_21d_ma = 25.342857,
  min_dewpoint_30d_ma = 5.511667
)
# Make the POST request
response <- POST(url_gls, body = body, encode = "json")

# Check status code and print response content
status_code(response)
content <- content(response, 'text')
print(content)

#[1] "{\"disease\":[\"GrayLeaf\"],\"probability\":[42.79],\"risk_class\":[\"Medium\"]}"

############################################################################ Sporecaster
url_sc <- paste0(url, "/predict_sporecaster_risk")



# Define the data to be sent in the POST request
for (irr in c("yes","no")){
  body_sc <- list(
    #growth_stage = 'yes',
    #fungicide_applied = 'no',
    row_spacing=15,
    irrigated=irr,
    risk_threshold = 35, 
    max_air_temp_30d_ma = 23.6,
    max_windspeed_30d_ma = 3.9,
    max_rh_30d_ma = 96.358333
  )
  
  # Make the POST request
  response <- POST(url_sc, body = body_sc, encode = "json")
  
  # Check status code and print response content
  status_code(response)
  content <- content(response, 'text')
  print(content)
}


#No encoding supplied: defaulting to UTF-8.
#[1] "{\"disease\":[\"Sporecaster-Irr\"],\"probability\":[33.32],\"risk_class\":[\"NoClass\"]}"
#No encoding supplied: defaulting to UTF-8.
#[1] "{\"disease\":[\"Sporecaster-NIrr\"],\"probability\":[83.47],\"risk_class\":[\"NoClass\"]}"

############################################################
url_gls <- paste0(url, "/predict_frogeye_leaf_spot_risk")


# Define the data to be sent in the POST request
body <- list(
  growth_stage = 'yes',
  fungicide_applied = 'no',
  risk_threshold = 50, 
  max_air_temp_30d_ma = 25.342857,
  relative_humidity_80tot_30d_ma = 5.511667
)
# Make the POST request
response_fe_lsr <- POST(url_gls, body = body, encode = "json")

# Check status code and print response content
status_code(response_fe_lsr)
content <- content(response_fe_lsr, 'text')
print(content)

#[1] "{\"code\":[\"Success\"],\"disease\":[\"FrogeyeLeafSpot\"],\"probability\":[0.03],\"risk_class\":[\"Low\"]}"
