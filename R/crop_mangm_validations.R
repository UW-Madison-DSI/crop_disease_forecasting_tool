#
GrowthStage <- list(
  V10 = "V10",
  V11 = "V11",
  V12 = "V12",
  V13 = "V13",
  V14 = "V14",
  V15 = "V15",
  R1 = "R1",
  R2 = "R2",
  R3 = "R3"
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
    return(list(valid = FALSE, message = "Invalid growth stage", reason = "Allowed stages are V10 to R3"))
  }
  
  return(list(valid = TRUE))
}
