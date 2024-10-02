library(plumber)

# Load necessary functions
source("R/logit_functions.R")
source("R/crop_mangm_validations.R")
source("R/var_schema.R")

#* @apiTitle Crop Disease Risk Prediction API
#* @apiDescription This API predicts the risk of crop diseases (Spore, Tarspot, and Gray Leaf Spot) based on environmental data and user inputs.

#* Predict Tarspot Risk
#* @param growth_stage Character: The growth stage of the crop ("V10", "R1", "R2", "R3")
#* @param fungicide_applied Character: "yes" if fungicide was applied in the last 14 days, "no" otherwise
#* @param risk_threshold Numeric: Action threshold (default = 35%). Threshold must be between 20 and 50
#* @param mean_air_temp_30d_ma Numeric: 30-day moving average of mean air temperature (°C)
#* @param max_rh_30d_ma Numeric: 30-day moving average of max relative humidity (%)
#* @param tot_hrs_rh90_14d_ma Numeric: 14-day moving average of total nighttime hours with 90% relative humidity or above for each day
#* @post /predict_tarspot_risk
function(growth_stage = "R1", fungicide_applied = "no", risk_threshold = 35,
         mean_air_temp_30d_ma, max_rh_30d_ma, tot_hrs_rh90_14d_ma) {
  
  # Validate inputs
  numeric_vars <- c("risk_threshold", "mean_air_temp_30d_ma", 
                    "max_rh_30d_ma", "tot_hrs_rh90_14d_ma")
  
  # Convert these variables to numeric
  convert_to_numeric(environment(), numeric_vars)
  
  validation <- validate_growth_and_fungicide(growth_stage, fungicide_applied)
  if (!validation$valid) {
    return(validation)
  }
  
  # Ensure risk threshold is between 20 and 50
  if (risk_threshold < 20 || risk_threshold > 50) {
    return(list(error = "Threshold must be between 20 and 50"))
  }
  
  # Call the tarspot risk calculation function
  result <- calculate_tarspot_risk(mean_air_temp_30d_ma, 
                                   max_rh_30d_ma, 
                                   tot_hrs_rh90_14d_ma, 
                                   risk_threshold)
  
  # Return the result as JSON
  return(result)
}

#* Predict Gray Leaf Spot Risk
#* @param growth_stage Character: The growth stage of the crop ("V10", "R1", "R2", "R3")
#* @param fungicide_applied Character: "yes" if fungicide was applied in the last 14 days, "no" otherwise
#* @param risk_threshold Numeric: Action threshold (default = 60%)
#* @param minAT21 Numeric: 21-day moving average of minimum air temperature
#* @param minDP30 Numeric: 30-day moving average of minimum dew point
#* @post /predict_gray_leaf_spot_risk
function(growth_stage = "R1", fungicide_applied = "no", risk_threshold = 60,
         minAT21, minDP30) {
  
  # Validate inputs
  numeric_vars <- c("risk_threshold", "minAT21", "minDP30")
  
  # Convert these variables to numeric
  convert_to_numeric(environment(), numeric_vars)
  
  validation <- validate_growth_and_fungicide(growth_stage, fungicide_applied)
  if (!validation$valid) {
    return(validation)
  }
  
  # Ensure risk threshold is between 50 and 70
  if (risk_threshold < 50 || risk_threshold > 70) {
    return(list(error = "Threshold must be between 50 and 70"))
  }
  
  # Call the gray leaf spot risk calculation function
  result <- calculate_gray_leaf_spot_risk(minAT21, minDP30, risk_threshold)
  
  # Return the result as JSON
  return(result)
}


#* Calculate sporecaster risk in irrigated and non-irrigated fields
#* @param row_spacing Numeric: Row spacing in inches (either 15 or 30)
#* @param irrigated Character: "yes" if the field was irrigated in the last 14 days, "no" otherwise
#* @param maxAT30MA Numeric: 30-day moving average of maximum air temperature (°C)
#* @param maxWS30MA Numeric: 30-day moving average of maximum wind speed (m/s)
#* @param maxRH30MA Numeric: 30-day moving average of relative humidity (only for irrigated fields)
#* @post /predict_sporecaster_risk
calculate_sporecaster_risk <- function(row_spacing, irrigated,   
                                       maxAT30MA, maxWS30MA, maxRH30MA = NULL) {
  
  # Convert shared numeric variables
  numeric_vars <- c("maxAT30MA", "maxWS30MA")
  convert_to_numeric <- function(env, vars) {
    for (var in vars) {
      assign(var, as.numeric(get(var, envir = env)), envir = env)
    }
  }
  convert_to_numeric(environment(), numeric_vars)
  
  # Check for valid irrigated value
  irrigated <- tolower(irrigated)
  if (!irrigated %in% c("yes", "no")) {
    return(list(error = "Invalid value for 'irrigated'. Please specify 'yes' or 'no'."))
  }
  
  # Initialize result
  result <- NULL
  
  # Conditional logic for irrigated and non-irrigated fields
  if (irrigated == "no") {
    # Calculate risk for non-irrigated fields
    result <- calculate_non_irrigated_risk(maxAT30MA, maxWS30MA)
    
  } else if (irrigated == "yes") {
    # Ensure maxRH30MA is provided for irrigated fields
    if (is.null(maxRH30MA)) {
      return(list(error = "maxRH30MA must be provided for irrigated fields."))
    }
    
    # Convert additional numeric variables for irrigated fields
    numeric_vars_irrigated <- c("maxRH30MA")
    convert_to_numeric(environment(), numeric_vars_irrigated)
    
    # Calculate risk for irrigated fields
    result <- calculate_irrigated_risk(maxAT30MA, maxRH30MA, row_spacing)
  }
  
  # Return the result or an error if something went wrong
  if (is.null(result)) {
    return(list(error = "An error occurred while calculating risk."))
  }
  
  return(result)
}
