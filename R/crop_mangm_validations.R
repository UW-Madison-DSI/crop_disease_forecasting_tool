# Validations
# Function to convert a list of variables to numeric
convert_to_numeric <- function(data, variables) {
  for (var in variables) {
    # Check if the variable exists in the data (environment)
    if (exists(var, envir = data)) {
      # Convert the variable to numeric, handling non-convertible values as NA
      assign(var, as.numeric(get(var, envir = data)), envir = data)
      
      # Optionally, print a message for confirmation
      message(paste("Converted", var, "to numeric"))
    } else {
      # Print a warning if the variable is not found in the environment
      warning(paste("Variable", var, "not found"))
    }
  }
}

GrowthStage <- list(
  YES = "yes",
  NO = "no"
)

FungicideApplied <- list(
  YES = "yes",
  NO = "no"
)

IrrigationApplied <- list(
  YES = "yes",
  NO = "no"
)


# Helper function to validate growth stage and fungicide application
validate_growth_and_fungicide <- function(growth_stage, fungicide_applied) {
  allowed_stages <- unlist(GrowthStage)
  allowed_fungicide <- unlist(FungicideApplied)
  
  if (!(fungicide_applied %in% allowed_fungicide)) {
    return(list(valid = FALSE, message = "Invalid fungicide input", reason = "Allowed values are 'yes' or 'no'"))
  }
  
  if (fungicide_applied == FungicideApplied$YES) {
    return(list(valid = FALSE, message = "Fungicide application not possible", reason = "Fungicide was applied in the last 14 days"))
  }
  
  if (!(growth_stage %in% allowed_stages)) {
    return(list(valid = FALSE, message = "Invalid growth stage response", reason = "Allowed values are 'yes' or 'no'"))
  }
  
  return(list(valid = TRUE))
}
