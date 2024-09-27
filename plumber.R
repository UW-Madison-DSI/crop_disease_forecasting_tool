# Load necessary packages
library(plumber)

#* @apiTitle Tarspot Prediction Algorithm API
#* @apiDescription Predict the risk of tar spot based on environmental data and user inputs.

# Define logistic function
logistic <- function(logit) {
  return(exp(logit) / (1 + exp(logit)))
}

# Logic on tarspot calculation
calculate_tarspot_risk <- function(meanAT, maxRH, rh90_night_tot, threshold = 35) {
  # Logistic regression formulas for the two models
  logit_LR4 <- 32.06987 - (0.89471 * meanAT) - (0.14373 * maxRH)
  logit_LR6 <- 20.35950 - (0.91093 * meanAT) - (0.29240 * rh90_night_tot)
  
  # Calculate probabilities
  prob_LR4 <- logistic(logit_LR4)
  prob_LR6 <- logistic(logit_LR6)
  
  # Ensemble the two probabilities (average)
  ensemble_prob <- (prob_LR4 + prob_LR6) / 2
  
  # Adjust colors based on the action threshold
  risk_class <- ifelse(ensemble_prob >= threshold / 100, "High",
                       ifelse(ensemble_prob >= 0.20, "Medium", 
                              ifelse(ensemble_prob > 0, "Low", 
                                     "NoRisk")))
  
  # Ensembled probability and color code
  list(
    disease='TarSpot',
    probability = round(ensemble_prob * 100, 2), 
    risk_class = risk_class)
}


#* Predict Tarspot Risk
#* @param growth_stage Character: The growth stage of the crop ("V10", "R1","R2","R3")
#* @param fungicide_applied Character: "yes" if fungicide was applied in the last 14 days, "no" otherwise
#* @param meanAT Numeric: 30-day moving average of mean air temperature (°C)
#* @param maxRH Numeric: 30-day moving average of max relative humidity (%)
#* @param rh90_night_tot Numeric: 14-day moving average of is the daily total hours between 20:00 and 6:00 where the humidity was 90% or above
#* @param threshold Numeric: Action threshold (default = 35%)
#* @post /predict
function(growth_stage="R1", fungicide_applied="no",
         meanAT, maxRH, rh90_night_tot, threshold = 35) {
  # Convert input parameters to numeric
  meanAT <- as.numeric(meanAT)
  maxRH <- as.numeric(maxRH)
  rh90_night_tot <- as.numeric(rh90_night_tot)
  threshold <- as.numeric(threshold)
  
  # Check if fungicide was applied and growth stage corresponds to V10 to R3
  allowed_stages <- c("V10", "V11", "V12", "V13", "R1", "R2", "R3")
  if ((fungicide_applied == "yes") || !(growth_stage %in% allowed_stages)) {
    return(list(message = "no possible", reason = "Fungicide was applied in the last 14 days"))
  }
  
  # Restrict the threshold to be between 20 and 50
  if (threshold < 20 || threshold > 50) {
    return(list(error = "Threshold must be between 20 and 50"))
  }
  
  # Call the tarspot risk calculation function
  result <- calculate_tarspot_risk(meanAT, maxRH, rh90_night_tot, threshold)
  
  # Return the result as JSON
  return(result)
}


