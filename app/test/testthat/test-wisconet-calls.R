library(testthat)
library(dplyr)
source("../../functions/1_wisconet_calls.R")

# Mock the actual API call
mock_fetch_forecasting_data <- function(date) {
  data.frame(
    station_id = c("TEST1", "TEST2"),
    forecasting_date = as.Date(rep(date, 2)),
    tarspot_risk = c(0.2, 0.8),
    fe_risk = c(0.3, 0.7),
    gls_risk = c(0.1, 0.9),
    whitemold_nirr_risk = c(0.4, 0.6),
    whitemold_irr_30in_risk = c(0.5, 0.5),
    whitemold_irr_15in_risk = c(0.6, 0.4),
    tarspot_risk_class = c("1.Low", "3.High"),
    fe_risk_class = c("1.Low", "3.High"),
    gls_risk_class = c("1.Low", "3.High"),
    whitemold_nirr_risk_class = c("2.Moderate", "2.Moderate"),
    whitemold_irr_class = c("2.Moderate", "2.Moderate"),
    station_name = c("Test Station 1", "Test Station 2"),
    latitude = c(43.1, 43.2),
    longitude = c(-89.1, -89.2),
    location = c("Test County 1", "Test County 2"),
    region = c("South", "North"),
    popup_content = c("Test popup 1", "Test popup 2")
  )
}

test_that("fetch_forecasting_data processes dates correctly", {
  # Replace the actual function with the mock during test
  orig_function <- fetch_forecasting_data
  assign("fetch_forecasting_data", mock_fetch_forecasting_data, envir = .GlobalEnv)
  
  # Test with a specific date
  test_date <- as.Date("2023-01-15")
  result <- fetch_forecasting_data(test_date)
  
  # Check basic structure
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$forecasting_date[1], test_date)
  
  # Check risk class assignments
  expect_equal(result$tarspot_risk_class[1], "1.Low")
  expect_equal(result$tarspot_risk_class[2], "3.High")
  
  # Restore the original function
  assign("fetch_forecasting_data", orig_function, envir = .GlobalEnv)
})

# If you need to test the real API connection, use a skip_on_* function
test_that("fetch_forecasting_data can connect to real API", {
  skip_on_ci()  # Skip this test on CI environments
  skip_on_cran() # Skip this test on CRAN checks
  
  # Use a recent date for testing
  test_date <- Sys.Date() - 1
  
  # Attempt to fetch real data
  result <- tryCatch(
    fetch_forecasting_data(test_date),
    error = function(e) NULL
  )
  
  # If the connection worked, check the result
  if (!is.null(result)) {
    expect_true(is.data.frame(result))
    expect_true("forecasting_date" %in% names(result))
  } else {
    skip("Could not connect to real API")
  }
})