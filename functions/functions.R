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