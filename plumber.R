library(plumber)


source("R/logit_functions.R")
source("R/crop_mangm_validations.R")


#* @apiTitle Crop Disease Risk Prediction API
#* @apiDescription This API predicts the risk of crop diseases (Spore, Tarspoter, Gray Leaf Spot and Frog Eye Leaf Spot) based on environmental data and user inputs.


#* Predict Tarspot Risk
#* @param growth_stage Character: 'yes' if the growth stage of the crop is between ("V10" to "R3"), "no" otherwise
#* @param fungicide_applied Character: "yes" if fungicide was applied in the last 14 days, "no" otherwise
#* @param risk_threshold Numeric: Action threshold (default = 35%). Threshold must be between 20 and 50.
#* @param mean_air_temp_30d_ma Numeric: 30-day moving average of mean air temperature (°C)
#* @param max_rh_30d_ma Numeric: 30-day moving average of max relative humidity (%)
#* @param tot_nhrs_rh90_14d_ma Numeric: 14-day moving average of total nighttime hours with 90% relative humidity (%) or above for each day.
#* @post /predict_tarspot_risk
function(growth_stage = "yes", 
         fungicide_applied = "no", 
         risk_threshold = 35,
         mean_air_temp_30d_ma, 
         max_rh_30d_ma, 
         tot_nhrs_rh90_14d_ma) {
  
  # Validate inputs
  numeric_vars <- c("risk_threshold", 
                    "mean_air_temp_30d_ma", 
                    "max_rh_30d_ma", 
                    "tot_nhrs_rh90_14d_ma")
  
  # Convert these variables to numeric
  convert_to_numeric(environment(), numeric_vars)
  
  risk_threshold_low<-.20
  risk_threshold_up<-risk_threshold/100
  # Ensure risk threshold is between 20 and 50
  if (risk_threshold_up < .20 || risk_threshold_up > .50){
    risk_threshold_up<-.35
  }
  
  if (fungicide_applied=='yes' || growth_stage=='no') {
    return(list(valid = FALSE, 
                message = "We can not compute the Tarspot Risk.", 
                reason = "Fungicide applied in last 14d or growth stage not in the valid ranges."))
  } else {
    # Call the tarspot risk calculation function
    result <- calculate_tarspot_risk(mean_air_temp_30d_ma, 
                                   max_rh_30d_ma, 
                                   tot_nhrs_rh90_14d_ma, 
                                   risk_threshold_low,
                                   risk_threshold_up)
  
    if (is.null(result)) {
      return(list(error = "An error occurred while calculating Tarspot Risk."))
    }else{
      return(result)
    }
  }
}

#* Predict Gray Leaf Spot Risk
#* @param growth_stage Character: 'yes' if the growth stage of the crop is between ("V10" to "R3"), "no" otherwise
#* @param fungicide_applied Character: "yes" if fungicide was applied in the last 14 days, "no" otherwise
#* @param risk_threshold Numeric: Action threshold (default = 60%)
#* @param min_air_temp_21d_ma Numeric: 21-day moving average of minimum air temperature (°C)
#* @param min_dewpoint_30d_ma Numeric: 30-day moving average of minimum dew point (°C)
#* @post /predict_gray_leaf_spot_risk
function(growth_stage = "yes", 
         fungicide_applied = "no", 
         risk_threshold = 60,
         min_air_temp_21d_ma, 
         min_dewpoint_30d_ma) {
  
  numeric_vars <- c("risk_threshold", "min_air_temp_21d_ma", "min_dewpoint_30d_ma")
  
  # Convert these variables to numeric
  convert_to_numeric(environment(), numeric_vars)
  
  risk_threshold_low<-.5
  risk_threshold_up<-risk_threshold/100
  if (risk_threshold_up<risk_threshold_low || risk_threshold_up>.70){
    risk_threshold_up<-.6
  }
  
  if (fungicide_applied=='yes' || growth_stage=='no') {
    return(list(valid = FALSE, 
                message = "We can not compute the Gray Leaf Spot Risk.", 
                reason = "Fungicide applied in last 14d or growth stage not in the valid ranges."))
  } else {
    # Call the gray leaf spot risk calculation function
    result <- calculate_gray_leaf_spot_risk(min_air_temp_21d_ma, 
                                            min_dewpoint_30d_ma, 
                                            risk_threshold_low,
                                            risk_threshold_up)
    if(is.null(result)){
      return(NULL)
    }else{
      return(result)
    }
  }
}


#* Calculate Sporecaster risk in irrigated and non-irrigated fields
#* @param row_spacing Numeric: Row spacing in inches (either 15 or 30)
#* @param irrigated Character: "yes" if the field was irrigated, "no" otherwise
#* @param max_air_temp_30d_ma Numeric: 30-day moving average of maximum air temperature (°C)
#* @param max_windspeed_30d_ma Numeric: 30-day moving average of maximum wind speed (m/s)
#* @param max_rh_30d_ma Numeric: 30-day moving average of relative humidity (%) (only for irrigated fields)
#* @post /predict_sporecaster_risk
function(row_spacing, 
         irrigated,
         max_air_temp_30d_ma,
         max_windspeed_30d_ma,
         max_rh_30d_ma = NULL) {
  
  # Convert shared numeric variables
  numeric_vars <- c("max_air_temp_30d_ma", "max_windspeed_30d_ma")
  convert_to_numeric <- function(env, vars) {
    for (var in vars) {
      assign(var, as.numeric(get(var, envir = env)), envir = env)
    }
  }
  convert_to_numeric(environment(), numeric_vars)
  
  # Check for valid irrigated value
  irrigated <- tolower(irrigated)
  if (!irrigated %in% c("yes", "no")) {
    return(list(code='error', 
                error = "Invalid value for 'irrigated'. Please specify 'yes' or 'no'."))
  }
  
  # Initialize result
  result <- NULL
  
  # Conditional logic for irrigated and non-irrigated fields
  if (irrigated == "no") {
    # Calculate risk for non-irrigated fields
    result <- calculate_non_irrigated_risk(max_air_temp_30d_ma, 
                                           max_windspeed_30d_ma)
    
  } else if (irrigated == "yes") {
    # Ensure max_rh_30d_ma is provided for irrigated fields
    if (is.null(max_rh_30d_ma)) {
      return(list(code='error', 
                  error = "max_rh_30d_ma must be provided for irrigated fields."))
    }
    
    # Convert additional numeric variables for irrigated fields
    numeric_vars_irrigated <- c("max_rh_30d_ma")
    convert_to_numeric(environment(), numeric_vars_irrigated)
    
    # Calculate risk for irrigated fields
    result <- calculate_irrigated_risk(max_air_temp_30d_ma, 
                                       max_rh_30d_ma, 
                                       row_spacing)
  }
  
  # Return the result or an error if something went wrong
  if (is.null(result)) {
    return(list(error = "An error occurred while calculating Sporecaster Risk."))
  }else{
    return(result)
  }
}

#* Predict FrogEye Leaf Spot Risk
#* @param growth_stage Character: "yes" if the growth stage of your crop is between ("R1" to "R5"), "no" otherwise
#* @param fungicide_applied Character: "yes" if fungicide was applied in the last 14 days, "no" otherwise
#* @param risk_threshold Numeric: Action threshold (default = 50%)
#* @param max_air_temp_30d_ma Numeric: 21-day moving average of maximum air temperature (°C)
#* @param relative_humidity_80tot_30d_ma Numeric: 30-day moving average of is the daily total hours where relative humidity (%) was 80% or above. 
#* @post /predict_frogeye_leaf_spot_risk
function(growth_stage = "yes", 
         fungicide_applied = "no", 
         risk_threshold = 50,
         max_air_temp_30d_ma, 
         relative_humidity_80tot_30d_ma) {
  
  numeric_vars <- c("risk_threshold", "max_air_temp_30d_ma", "relative_humidity_80tot_30d_ma")
  
  convert_to_numeric(environment(), numeric_vars)
  
  risk_threshold_low<-.4
  risk_threshold_up<-risk_threshold/100
  if (risk_threshold_up<risk_threshold_low || risk_threshold_up>.6){
    risk_threshold_up<-.5
  }
  
  if (fungicide_applied=='yes' || growth_stage=='no') {
    return(list(valid = FALSE, 
                message = "We can not compute the FrogEye Risk.", 
                reason = "Fungicide applied in last 14d or growth stage not in the valid ranges."))
  } else {
    result <- calculate_frogeye_leaf_spot(max_air_temp_30d_ma, 
                                          relative_humidity_80tot_30d_ma, 
                                          risk_threshold_low,
                                          risk_threshold_up)
    
    if (is.null(result)) {
      return(list(error = "An error occurred while calculating Frogeye Risk."))
    }else{
      return(result)
    }
  }
}


#* Predict Wisconet Stations Risk
#* @param date Character: Data on which to retrieve the prediction, format YYYY-dd-MM
#* @param disease_name Character: tarspot, gray_leaf_spot, sporecaster-irr, sporecaster-noirr, frogeye_leaf_spot
#* @post /predict_wisconet_stations_risk
function(date = NULL, 
         disease_name = NULL) {

  if (is.null(date)){
    input_date <- Sys.Date()
  }else{
    input_date <- as.Date(date)
    if ((format(input_date, "%Y-%m") == "2024-04") 
        || (input_date < as.Date("2022-01-01"))
        || (input_date > Sys.Date())) {
      return(list(error = "Due to data avilability, we can not estimate the tarspot disease on April 2024 or previous to 2022"))
    }else{
      risk <- retrieve_tarspot_all_stations(input_date,
                                                        station_id = NULL, 
                                                        disease_name = 'tarspot')
    }
  }
  if (!disease_name %in% c('tarspot', 'gray_leaf_spot', 'sporecaster-irr', 'sporecaster-noirr', 'frogeye_leaf_spot')){
    return(list(error = "Please input a valid disease name."))
  }else{
    estimated_risk <- disease_risk(date, disease_name)
    return(list(risk = estimated_risk,
                disease_name = disease_name,
                date = input_date))
  }
}
