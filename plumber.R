library(plumber)

# Load functions
source("R/logit_functions.R")
source("R/crop_mangm_validations.R")
source("R/var_schema.R")

#* @apiTitle Crop Disease Risk Prediction API
#* @apiDescription This API predicts the risk of crop diseases (Tarspot and Gray Leaf Spot) based on environmental data and user inputs.



#* Predict Tarspot Risk
#* @param growth_stage Character: The growth stage of the crop ("V10", "R1", "R2", "R3")
#* @param fungicide_applied Character: "yes" if fungicide was applied in the last 14 days, "no" otherwise
#* @param risk_threshold Numeric: Action threshold (default = 35%)
#* @param meanAT21 Numeric: 30-day moving average of mean air temperature (°C)
#* @param maxRH30MA Numeric: 30-day moving average of max relative humidity (%)
#* @param totNTH14MA Numeric: 14-day moving average of nighttime RH > 90%
#* @post /predict_tarspot
function(growth_stage = "R1", fungicide_applied = "no", risk_threshold = 35,
         minAT21, maxRH30MA,
         totNTH14MA) {
  
  # Validate inputs
  risk_threshold <- as.numeric(risk_threshold)
  meanAT21 <- as.numeric(meanAT21)
  maxRH30MA <- as.numeric(maxRH30MA)
  totNTH14MA <- as.numeric(totNTH14MA)
  
  validation <- validate_growth_and_fungicide(growth_stage, fungicide_applied)
  if (!validation$valid) {
    return(validation)
  }
  
  # Ensure risk threshold is between 20 and 50
  if (risk_threshold < 20 || risk_threshold > 50) {
    return(list(error = "Threshold must be between 20 and 50"))
  }
  
  # Call the tarspot risk calculation function
  result <- calculate_tarspot_risk(meanAT21,
                                   maxRH30MA,
                                   total_nighttime_rh_above_90_pct_14_day_moving_avg,
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
#* @post /predict_gray_leaf_spot
function(growth_stage = "R1", fungicide_applied = "no", risk_threshold = 60,
         minAT21, minDP30) {
  
  # Validate inputs
  risk_threshold <- as.numeric(risk_threshold)
  minAT21 <- as.numeric(minAT21)
  minDP30 <- as.numeric(minDP30)
  
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


#* Calculate risk for apothecia non-irrigated fields
#* @param maxAT30MA Numeric: 30-day moving average of maximum air temperature (°C)
#* @param maxWS30MA Numeric: 30-day moving average of maximum wind speed (m/s)
#* @param threshold Numeric: Risk threshold (default = 40%)
#* @post /calculate_non_irrigated_sporecaster
function(maxAT30MA, maxWS30MA, threshold = 40) {
  # Convert inputs to numeric (in case they are passed as strings)
  maxAT30MA <- as.numeric(maxAT30MA)
  maxWS30MA <- as.numeric(maxWS30MA)
  threshold <- as.numeric(threshold)
  
  # Call the risk calculation function
  result <- calculate_non_irrigated_risk(maxAT30MA, maxWS30MA, threshold)
  
  # Return the result
  return(result)
}

#* Calculate risk for apothecia irrigated fields
#* @param maxAT30MA Numeric: 30-day moving average of maximum air temperature (°C)
#* @param maxRH30MA Numeric: 30-day moving average of maximum relative humidity (%)
#* @param row_spacing Numeric: Row spacing in inches (either 15 or 30)
#* @param threshold Numeric: Risk threshold (default = 50%)
#* @post /calculate_irrigated_sporecaster
function(maxAT30MA, maxRH30MA, row_spacing, threshold = 50) {
  # Convert inputs to numeric
  maxAT30MA <- as.numeric(maxAT30MA)
  maxRH30MA <- as.numeric(maxRH30MA)
  row_spacing <- as.numeric(row_spacing)
  threshold <- as.numeric(threshold)
  
  # Call the risk calculation function
  result <- calculate_irrigated_risk(maxAT30MA, maxRH30MA, row_spacing, threshold)
  
  # Return the result
  return(result)
}

