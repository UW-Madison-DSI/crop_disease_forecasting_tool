# Load the testthat library
library(testthat)

# Test for the calculate_non_irrigated_risk function
test_that("calculate_non_irrigated_risk works as expected", {
  
  # Test case 1: Default threshold
  result <- calculate_non_irrigated_risk(maxAT30MA = 30, maxWS30MA = 20)
  
  # Expect the result to be a valid disease risk (assuming the return type is numeric or a list)
  expect_true(is.numeric(result) || is.list(result))
  
  # Test case 2: Different threshold
  result_with_threshold <- calculate_non_irrigated_risk(maxAT30MA = 25, maxWS30MA = 15, threshold = 35)
  
  # Expect the result to be valid and different from the default threshold case
  expect_true(is.numeric(result_with_threshold) || is.list(result_with_threshold))
  expect_false(identical(result, result_with_threshold))
  
  # Test case 3: Edge case with maxAT30MA and maxWS30MA as 0
  result_edge_case <- calculate_non_irrigated_risk(maxAT30MA = 0, maxWS30MA = 0)
  
  # Validate the result (for example, ensure it's not producing NaN or Inf)
  expect_true(is.finite(result_edge_case))
  
  # Test case 4: Check for specific output when maxAT30MA and maxWS30MA are extreme values
  result_extreme <- calculate_non_irrigated_risk(maxAT30MA = 100, maxWS30MA = 100)
  
  # Ensure the result is within reasonable bounds (you can adjust this depending on the expected range)
  expect_true(result_extreme >= 0 && result_extreme <= 1)
})
