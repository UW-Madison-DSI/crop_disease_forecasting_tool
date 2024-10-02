# Logistic function to convert logit to probability
logistic <- function(logit) {
  exp(logit) / (1 + exp(logit))
}

# Function to classify risk based on probability and thresholds
classify_risk <- function(probability, high_threshold, medium_threshold, low_threshold = 0) {
  if (probability >= high_threshold) {
    return("High")
  } else if (probability >= medium_threshold) {
    return("Medium")
  } else {
    return("Low")
  }
}

# General function to calculate risk for any disease
calculate_disease_risk <- function(logit_values, thresholds, disease_name, threshold) {
  # Check if there are multiple logit values
  probabilities <- sapply(logit_values, logistic)
  ensemble_prob <- mean(probabilities)
  
  if (threshold == -1) {
    # Return results as a list
    risk_class <- 'NoClass'
  } else {
    # Ensure the classify_risk function is properly defined
    risk_class <- classify_risk(ensemble_prob, threshold / 100, thresholds[1], thresholds[2])
  }
    
  # Return results as a list
  return(list(
    code='ok',
    disease = disease_name,
    probability = round(ensemble_prob * 100, 2),  # Convert probability to percentage
    risk_class = risk_class
  ))
}

# Function to calculate risk for Tar Spot based on two logistic regression models
calculate_tarspot_risk <- function(meanAT, maxRH, rh90_night_tot, threshold = 35) {
  # Logistic regression formulas for the two models, no irrigation total needed
  logit_LR4 <- 32.06987 - (0.89471 * meanAT) - (0.14373 * maxRH) #paper page5
  logit_LR6 <- 20.35950 - (0.91093 * meanAT) - (0.29240 * rh90_night_tot) #paper page5
  
  # Calculate risk using the general disease risk function
  return(calculate_disease_risk(
    logit_values = c(logit_LR4, logit_LR6),      # Two models' logit values
    thresholds = c(0.35, 0.20),                  # Risk classification thresholds
    disease_name = "TarSpot",                    # Disease name
    threshold = threshold                        # User-defined threshold (default 35%)
  ))
}


# Function to calculate risk for Gray Leaf Spot
calculate_gray_leaf_spot_risk <- function(minAT21, minDP30, threshold = 60) {
  # Logistic regression formula, no rrigation needed
  logit_GLS <- -2.9467-(0.03729 * minAT21) + (0.6534 * minDP30)
  
  # Calculate risk using the general disease risk function
  return(calculate_disease_risk(
    logit_values = c(logit_GLS),
    thresholds = c(0.40, 0), 
    disease_name = "GrayLeafSpot",
    threshold = threshold
  ))
}

# Example for non-irrigated risk calculation
calculate_non_irrigated_risk <- function(maxAT30MA, maxWS30MA) {
  # Logistic regression formula for non-irrigated model
  logit_nirr <- (-0.47 * maxAT30MA) - (1.01 * maxWS30MA) + 16.65
  
  # Calculate disease risk using the general disease risk function
  return(calculate_disease_risk(
    logit_values = c(logit_nirr),
    thresholds = c(1.0, 1.0), #no threshold here, no class, just probab 
    disease_name = "Sporecaster-NIrr",
    threshold = -1 #no threshold here, no class, just probab
  ))
}

# Example for irrigated risk calculation
calculate_irrigated_risk <- function(maxAT30MA, maxRH30MA, row_spacing) {
  # Determine row value (0 for 15-inch, 1 for 30-inch)
  row <- ifelse(row_spacing == 30, 1, 0)
  
  # Logistic regression formula for irrigated model
  logit_irr <- (-2.38 * row) + (0.65 * maxAT30MA) + (0.38 * maxRH30MA) - 52.65
  
  # Calculate disease risk using the general disease risk function
  return(calculate_disease_risk(
    logit_values = c(logit_irr),
    thresholds = c(1.0, 1.0),  #no threshold here, no class, just probab
    disease_name = "Sporecaster-Irr",
    threshold = -1 #no threshold here, no class, just probab
  ))
}
