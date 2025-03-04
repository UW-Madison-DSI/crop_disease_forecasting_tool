library(httr)      # For API requests
library(jsonlite)  # For parsing JSON
library(dplyr)     # For data manipulation
library(purrr)
library(lubridate)
library(zoo)
library(tidyr)
library(dplyr)
library(jsonlite)



############################################################# Risk functions Damon et al
calculate_tarspot_risk_function <- function(meanAT, maxRH, rh90_night_tot) {
  logit_LR4 <- 32.06987 - (0.89471 * meanAT) - (0.14373 * maxRH)
  logit_LR6 <- 20.35950 - (0.91093 * meanAT) - (0.29240 * rh90_night_tot)
  logit_values <- c(logit_LR4, logit_LR6)
  probability <- sapply(logit_values, logistic_f)
  ensemble_prob <- mean(probability)
  
  class <- if (ensemble_prob < 0.2) {
    "low"
  } else if (ensemble_prob > 0.35) {
    "high"
  } else {
    "moderate"
  }
  
  return(list(tarspot_risk = ensemble_prob, tarspot_risk_class = class))
}

calculate_gray_leaf_spot_risk_function <- function(minAT21, 
                                                   minDP30) {
  prob <- logistic_f(-2.9467-(0.03729 * minAT21) + (0.6534 * minDP30))
  
  class <- if (prob < 0.2) {
    "low"
  } else if (prob > 0.6) {
    "high"
  } else {
    "moderate"
  }
  
  return(list(gls_risk = prob, gls_risk_class = class))
}

calculate_non_irrigated_risk <- function(maxAT30MA, maxWS30MA) {
  # Logistic regression formula for non-irrigated model
  logit_nirr <- (-0.47 * maxAT30MA) - (1.01 * maxWS30MA) + 16.65
  ensemble_prob <- logistic_f(logit_nirr)
  
  return(list(sporec_nirr_risk = ensemble_prob, sporec_nirr_risk_class = "NoClass"))
}

# Irrigated Sporecaster Risk
calculate_irrigated_risk <- function(maxAT30MA, maxRH30MA) {
  # Logistic regression formula for irrigated model
  logit_irr_30 <- (-2.38 *1) + (0.65 * maxAT30MA) + (0.38 * maxRH30MA) - 52.65
  prob_logit_irr_30 <- logistic_f(logit_irr_30)
  
  logit_irr_15 <- (-2.38 * 0) + (0.65 * maxAT30MA) + (0.38 * maxRH30MA) - 52.65
  prob_logit_irr_15 <- logistic_f(logit_irr_15)
  
  return(list(sporec_irr_30in_risk = prob_logit_irr_30, 
              sporec_irr_15in_risk = prob_logit_irr_15))
}

calculate_sporecaster_risk <- function(maxAT30MA, maxWS30MA, maxRH30MA){
  # un used yet
  sporec_irr_risk = calculate_irrigated_risk(maxAT30MA, maxRH30MA)
  sporec_no_irr_risk = calculate_non_irrigated_risk(maxAT30MA, maxWS30MA)
  return(list(sporec_irr_30in_risk = sporec_irr_risk$sporec_irr_30in_risk, 
              sporec_irr_15in_risk = sporec_irr_risk$sporec_irr_15in_risk,
              sporec_no_irr_risk = sporec_no_irr_risk$sporec_nirr_risk))
}


# Frogeye Leaf Spot
calculate_frogeye_leaf_spot_function <- function(maxAT30, rh80tot30) {
  # Logistic regression formula, no rrigation needed
  logit_fe <- -5.92485 +(0.1220 * maxAT30) + (0.1732 * rh80tot30)
  prob_logit_fe <- logistic_f(logit_fe)
  
  class <- if (prob_logit_fe < 0.5) {
    "low"
  } else if (prob_logit_fe > 0.6) {
    "high"
  } else {
    "moderate"
  }
  
  # Calculate risk using the general disease risk function
  return(list(
    fe_risk = prob_logit_fe,
    fe_risk_class = class
  ))
}