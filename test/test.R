install.packages("testthat")

library(testthat)



test_that("calculate_tarspot_risk returns expected values for valid inputs", {
  result <- calculate_tarspot_risk(meanAT = 25, maxRH = 80, rh90_night_tot = 5, threshold = 35)
  
  expect_equal(result$disease, "TarSpot")
  expect_true(result$probability >= 0 && result$probability <= 100)
  expect_true(result$risk_class %in% c("High", "Medium", "Low", "NoRisk"))
})

test_that("calculate_tarspot_risk handles invalid threshold correctly", {
  result <- calculate_tarspot_risk(meanAT = 25, maxRH = 80, rh90_night_tot = 5, threshold = 10)
  
  # 
  expect_equal(result$probability, NA_real_)  # Assuming NA or error for invalid threshold
})

test_that("calculate_tarspot_risk handles boundary conditions", {
  result_low <- calculate_tarspot_risk(meanAT = 0, maxRH = 0, rh90_night_tot = 0, threshold = 35)
  result_high <- calculate_tarspot_risk(meanAT = 100, maxRH = 100, rh90_night_tot = 24, threshold = 35)
  
  expect_true(result_low$probability >= 0 && result_low$probability <= 100)
  expect_true(result_high$probability >= 0 && result_high$probability <= 100)
})
