# Logistic function to convert logit to probability
logistic <- function(logit) {
  return(exp(logit) / (1 + exp(logit)))
}

# Function to classify risk based on probability and threshold
classify_risk <- function(probability, high_threshold, medium_threshold, low_threshold = 0) {
  if (probability >= high_threshold) {
    return("High")
  } else if (probability >= medium_threshold) {
    return("Medium")
  } else if (probability > low_threshold) {
    return("Low")
  } else {
    return("NoRisk")
  }
}

# General function to calculate risk for any disease
calculate_disease_risk <- function(logit_values, thresholds = c(0.40, 0.20, 0), disease_name, threshold = 35) {
  # Calculate probabilities using logistic transformation
  probabilities <- sapply(logit_values, logistic)
  
  # Ensemble the probabilities by averaging
  ensemble_prob <- mean(probabilities)
  
  # Classify risk based on the action threshold
  risk_class <- classify_risk(ensemble_prob, threshold / 100, thresholds[1], thresholds[2])
  
  # Return results as a list
  return(list(
    disease = disease_name,
    probability = round(ensemble_prob * 100, 2), 
    risk_class = risk_class
  ))
}

# Function to calculate risk for Tar Spot
calculate_tarspot_risk <- function(meanAT, maxRH, rh90_night_tot, threshold = 35) {
  # Logistic regression formulas for the two models
  logit_LR4 <- 32.06987 - (0.89471 * meanAT) - (0.14373 * maxRH)
  logit_LR6 <- 20.35950 - (0.91093 * meanAT) - (0.29240 * rh90_night_tot)
  
  # Calculate risk using the general disease risk function
  return(calculate_disease_risk(
    logit_values = c(logit_LR4, logit_LR6),
    thresholds = c(0.20, 0),  
    disease_name = "TarSpot",
    threshold = threshold
  ))
}

# Function to calculate risk for Gray Leaf Spot
calculate_gray_leaf_spot_risk <- function(minAT21, minDP30, threshold = 60) {
  # Logistic regression formula for the model
  logit_LR <- 32.06987 - (0.89471 * minAT21) - (0.14373 * minDP30)
  
  # Calculate risk using the general disease risk function
  return(calculate_disease_risk(
    logit_values = c(logit_LR),
    thresholds = c(0.40, 0), 
    disease_name = "GrayLeaf",
    threshold = threshold
  ))
}

# Function to calculate risk for non-irrigated fields
calculate_non_irrigated_risk <- function(maxAT30MA, maxWS30MA, threshold = 40) {
  # Logistic regression formula for non-irrigated model
  logit_mu <- (-0.47 * maxAT30MA) - (1.01 * maxWS30MA) + 16.65
  
  # Calculate risk using the general disease risk function
  return(calculate_disease_risk(
    logit_values = c(logit_mu),
    thresholds = c(0.40, 0.20, 0),  #NO thresholds - need input here
    disease_name = "Apothecial(Non-Irrigated)",
    threshold = threshold
  ))
}

# Function to calculate risk for irrigated fields
calculate_irrigated_risk <- function(maxAT30MA, maxRH30MA, row_spacing, threshold = 50) {
  # Determine row value (0 for 15-inch, 1 for 30-inch)
  row <- ifelse(row_spacing == 30, 1, 0)
  
  # Logistic regression formula for irrigated model
  logit_mu <- (-2.38 * row) + (0.65 * maxAT30MA) + (0.38 * maxRH30MA) - 52.65
  
  # Calculate risk using the general disease risk function
  return(calculate_disease_risk(
    logit_values = c(logit_mu),
    thresholds = c(0.50, 0.30, 0),  # NO thresholds - need input here
    disease_name = "Apothecial(Irrigated)",
    threshold = threshold
  ))
}
