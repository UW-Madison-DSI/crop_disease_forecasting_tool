library(testthat)
source("../functions/1_wisconet_calls.R")

test_that("fetch_forecasting_data returns a dataframe", {
  # Mock the API response
  mock_response <- list(
    station_name = "Station A",
    location = "Location A",
    region = "Region A",
    tarspot_risk = 0.2,
    fe_risk = 0.1,
    gls_risk = 0.3,
    whitemold_nirr_risk = 0.4,
    whitemold_irr_30in_risk = 0.5,
    whitemold_irr_15in_risk = 0.6
  )
  
  # simulate the API request/response
  stub(request, "GET", mock_response)
  
  result <- fetch_forecasting_data("2024-12-20")
  
  # Check if the result is a dataframe
  expect_true(is.data.frame(result))
  
  expect_true("station_name" %in% colnames(result))
  expect_true("location" %in% colnames(result))
  expect_true("popup_content" %in% colnames(result))
})

test_that("fetch_forecasting_data handles error gracefully", {
  stub(request, "GET", function(...) stop("API request failed"))
  
  result <- fetch_forecasting_data("2024-12-20")
  
  expect_null(result)
})

