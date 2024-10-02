################################################################################
################### API CALL EXAMPLE - single point ############################
################################################################################

install.packages("httr")

# Load the required package
library(httr)

url <- "https://connect.doit.wisc.edu/forecasting_crop_disease"

############################################################################ Tarspot
# Define the URL for the API
url_ts <- paste0(url, "/predict_tarspot_risk")

n<-10
mean_air_temp_range <- seq(16, 26, length.out = n)
tot_hrs_rh90_range <- seq(5, 20, length.out = n)  # Range of total hours RH>90%

for (th in tot_hrs_rh90_range){
  for (mat in mean_air_temp_range){
    # Define the data to be sent in the POST request
    body <- list(
      growth_stage = 'R1',
      fungicide_applied = 'no',
      risk_threshold = 35, 
      mean_air_temp_30d_ma = mat,
      max_rh_30d_ma = 95.72,
      tot_hrs_rh90_14d_ma = th
    )
    
    # Make the POST request
    response <- POST(url_ts, body = body, encode = "json")
    
    # Check the status code
    if (status_code(response) == 200) {
      # Parse the content as JSON
      response_content <- content(response, as = "parsed", type = "application/json")
      
      # Access the 'probability' field from the parsed response
      print(paste(th, mat,response_content$probability[[1]]))
    } else {
      print(paste("Request failed with status code", status_code(response)))
    }
  }
}
# Result
#[1] "{\"disease\":[\"TarSpot\"],\"probability\":[6.92],\"risk_class\":[\"Low\"]}"


body <- list(
  growth_stage = 'R1',
  fungicide_applied = 'no',
  risk_threshold = 35, 
  mean_air_temp_30d_ma = 16,
  max_rh_30d_ma = 95.72,
  tot_hrs_rh90_14d_ma = 4.4
)

# Make the POST request
response <- POST(url_ts, body = body, encode = "json")

# Check status code and print response content
status_code(response)
content <- content(response, 'text')
print(content)
############################################################################ Gray Leaf Spot
url_gls <- paste0(url, "/predict_gray_leaf_spot_risk")


# Define the data to be sent in the POST request
body <- list(
  growth_stage = 'R1',
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
    growth_stage = 'R1',
    fungicide_applied = 'no',
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