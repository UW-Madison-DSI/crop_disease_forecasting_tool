
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


