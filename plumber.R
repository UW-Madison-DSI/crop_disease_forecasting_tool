library(plumber)

# Load functions
source("R/logit_functions.R")
source("R/crop_mangm_validations.R")

#* @apiTitle Crop Disease Risk Prediction API
#* @apiDescription This API predicts the risk of crop diseases (Tarspot and Gray Leaf Spot) based on environmental data and user inputs.



#* Predict Tarspot Risk
#* @param growth_stage Character: The growth stage of the crop ("V10", "R1", "R2", "R3")
#* @param fungicide_applied Character: "yes" if fungicide was applied in the last 14 days, "no" otherwise
#* @param risk_threshold Numeric: Action threshold (default = 35%)
#* @param mean_air_temp_30_day_moving_avg Numeric: 30-day moving average of mean air temperature (°C)
#* @param max_relative_humidity_30_day_moving_avg Numeric: 30-day moving average of max relative humidity (%)
#* @param total_nighttime_rh_above_90_pct_14_day_moving_avg Numeric: 14-day moving average of nighttime RH > 90%
#* @post /predict_tarspot
function(growth_stage = "R1", fungicide_applied = "no", risk_threshold = 35,
         mean_air_temp_30_day_moving_avg, max_relative_humidity_30_day_moving_avg,
         total_nighttime_rh_above_90_pct_14_day_moving_avg) {
  
  # Validate inputs
  risk_threshold <- as.numeric(risk_threshold)
  mean_air_temp_30_day_moving_avg <- as.numeric(mean_air_temp_30_day_moving_avg)
  max_relative_humidity_30_day_moving_avg <- as.numeric(max_relative_humidity_30_day_moving_avg)
  total_nighttime_rh_above_90_pct_14_day_moving_avg <- as.numeric(total_nighttime_rh_above_90_pct_14_day_moving_avg)
  
  validation <- validate_growth_and_fungicide(growth_stage, fungicide_applied)
  if (!validation$valid) {
    return(validation)
  }
  
  # Ensure risk threshold is between 20 and 50
  if (risk_threshold < 20 || risk_threshold > 50) {
    return(list(error = "Threshold must be between 20 and 50"))
  }
  
  # Call the tarspot risk calculation function
  result <- calculate_tarspot_risk(mean_air_temp_30_day_moving_avg,
                                   max_relative_humidity_30_day_moving_avg,
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
