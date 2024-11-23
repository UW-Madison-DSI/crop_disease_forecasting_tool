# Logistic function to convert logit to probability
logistic <- function(logit) {
  probability<-exp(logit) / (1 + exp(logit))
  return(probability)
}

classify_risk<-function(ensemble_prob, l_thr, h_thr){
  if (ensemble_prob<=l_thr){
    return("Low")
  }else{
    if (ensemble_prob>=h_thr){
      return("High")
    }else{
      return("Moderate")
    }
  }
}

# function to calculate risk for any disease
calculate_disease_risk <- function(logit_values, thresholds, disease_name) {
  tryCatch({
    probabilities <- sapply(logit_values, logistic)
    cat("Probabilities ", disease_name, ': ', probabilities, '\n')
    
    ensemble_prob <- mean(probabilities)
    cat("Ensemble ", disease_name, ': ', ensemble_prob, '\n')
    
    if (thresholds[2] == 1.0 & thresholds[1] == 1.0) {
      # Return results as a list
      risk_classification <- 'NoRiskClass'
    } else {
      # Ensure the classify_risk function is properly defined
      risk_classification <- classify_risk(ensemble_prob, thresholds[1], thresholds[2])
    }
    cat("\nThresholds: ", thresholds[1], thresholds[2], "Risk Classification: ", risk_classification, '\n')
    
    # Return results as a list
    return(list(
      code = 'Success',
      disease = disease_name,
      probability = round(ensemble_prob * 100, 2),  # Convert probability to percentage
      risk_class = risk_classification
    ))
  }, error = function(e) {
    # Handle the error gracefully
    return(list(
      code = 'Error',
      message = paste("Failed to calculate disease risk:", e$message)
    ))
  })
}


# Function to calculate risk for Tar Spot based on two logistic regression models
calculate_tarspot_risk <- function(meanAT, maxRH, 
                                   rh90_night_tot, 
                                   threshold_low,
                                   threshold_up = .35
                                   ) {
  # Logistic regression formulas for the two models, no irrigation total needed
  logit_LR4 <- 32.06987 - (0.89471 * meanAT) - (0.14373 * maxRH) #paper page5
  logit_LR6 <- 20.35950 - (0.91093 * meanAT) - (0.29240 * rh90_night_tot) #paper page5
  
  # Calculate risk using the general disease risk function
  return(calculate_disease_risk(
    logit_values = c(logit_LR4, logit_LR6), # Two models' logit values
    thresholds = c(threshold_low, threshold_up),
    disease_name = "TarSpot"
    )
  )
}


# Function to calculate risk for Gray Leaf Spot
calculate_gray_leaf_spot_risk <- function(minAT21, 
                                          minDP30,
                                          threshold_low,
                                          threshold_up= .6
                                          ) {
  # Logistic regression formula, no rrigation needed
  logit_GLS <- -2.9467-(0.03729 * minAT21) + (0.6534 * minDP30)
  
  # Calculate risk using the general disease risk function
  return(calculate_disease_risk(
    logit_values = c(logit_GLS),
    thresholds = c(threshold_low, threshold_up), 
    disease_name = "GrayLeafSpot"  
    )
  )
}

# Non-irrigated risk calculation
calculate_non_irrigated_risk <- function(maxAT30MA, maxWS30MA) {
  # Logistic regression formula for non-irrigated model
  logit_nirr <- (-0.47 * maxAT30MA) - (1.01 * maxWS30MA) + 16.65
  
  # Calculate disease risk using the general disease risk function
  return(calculate_disease_risk(
    logit_values = c(logit_nirr),
    thresholds = c(1.0, 1.0), #no threshold here, no class, just probab 
    disease_name = "Sporecaster-NIrr"
  ))
}

# Irrigated risk calculation
calculate_irrigated_risk <- function(maxAT30MA, maxRH30MA, row_spacing) {
  # Determine row value (0 for 15-inch, 1 for 30-inch)
  row <- ifelse(row_spacing == 30, 1, 0)
  
  # Logistic regression formula for irrigated model
  logit_irr <- (-2.38 * row) + (0.65 * maxAT30MA) + (0.38 * maxRH30MA) - 52.65
  
  # Calculate disease risk using the general disease risk function
  return(calculate_disease_risk(
    logit_values = c(logit_irr),
    thresholds = c(1.0, 1.0),  #no threshold here, no class, just probab
    disease_name = "Sporecaster-Irr"
  ))
}


# Frogeye Leaf Spot
calculate_frogeye_leaf_spot <- function(maxAT30, rh80tot30, threshold_mid, threshold_high) {
  # Logistic regression formula, no rrigation needed
  logit_fe <- -5.92485 -(0.1220 * maxAT30) + (0.1732 * rh80tot30)
  
  # Calculate risk using the general disease risk function
  return(calculate_disease_risk(
    logit_values = c(logit_fe),
    thresholds = c(threshold_mid, threshold_high), 
    disease_name = "FrogeyeLeafSpot"
  ))
}
