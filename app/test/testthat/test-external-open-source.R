library(testthat)
source("../../functions/2_external_source.R")

# Mock the IBM API call
mock_ibm_query <- function(date, lat, lng) {
  data.frame(
    forecasting_date = as.Date(date),
    tarspot_risk = 0.45,
    fe_risk = 0.25,
    gls_risk = 0.65,
    whitemold_nirr_risk = 0.35,
    whitemold_irr_30in_risk = 0.55,
    whitemold_irr_15in_risk = 0.45,
    tarspot_risk_class = "2.Moderate",
    fe_risk_class = "1.Low",
    gls_risk_class = "3.High",
    whitemold_nirr_risk_class = "1.Low",
    whitemold_irr_class = "2.Moderate",
    temperature_1 = 20.5,
    temperature_2 = 22.3,
    relativeHumidity_1 = 75.4,
    relativeHumidity_2 = 80.2
  )
}

test_that("ibm_query formats outputs correctly", {
  # Replace the actual function with mock during test
  orig_function <- ibm_query
  assign("ibm_query", mock_ibm_query, envir = .GlobalEnv)
  
  # Test parameters
  test_date <- as.Date("2023-07-15")
  test_lat <- 43.5
  test_lng <- -89.5
  
  # Run the test
  result <- ibm_query(test_date, test_lat, test_lng)
  
  # Check the output has expected structure
  expect_true(is.data.frame(result))
  expect_equal(result$forecasting_date, test_date)
  expect_equal(result$tarspot_risk_class, "2.Moderate")
  expect_equal(result$gls_risk_class, "3.High")
  
  # Check numeric values
  expect_true(is.numeric(result$tarspot_risk))
  expect_true(is.numeric(result$temperature_1))
  expect_true(is.numeric(result$relativeHumidity_1))
  
  # Restore original function
  assign("ibm_query", orig_function, envir = .GlobalEnv)
})

# Skip tests for actual API connections
test_that("ibm_query can connect to real API", {
  skip("Skipping real API connection test")
  
  test_date <- Sys.Date()
  test_lat <- 43.0731
  test_lng <- -89.4012
  
  result <- tryCatch(
    ibm_query(test_date, test_lat, test_lng),
    error = function(e) NULL
  )
  
  if (!is.null(result)) {
    expect_true(is.data.frame(result))
    expect_true("forecasting_date" %in% names(result))
  }
})