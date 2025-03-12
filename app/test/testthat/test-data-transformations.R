library(testthat)
source("../../functions/7_data_transformations.R")

test_that("custom_disease_name returns correct disease names", {
  expect_equal(custom_disease_name("tarspot"), "Tar Spot")
  expect_equal(custom_disease_name("fe"), "Frogeye Leaf Spot")
  expect_equal(custom_disease_name("gls"), "Gray Leaf Spot")
  expect_equal(custom_disease_name("whitemold_nirr"), "White Mold (Non-irrigated)")
  expect_equal(custom_disease_name("whitemold_irr_30in"), "White Mold (Irrigated 30in)")
  expect_equal(custom_disease_name("unknown"), "Unknown Disease")
})

test_that("relabeling_class correctly updates risk class", {
  # Create test data
  test_data <- data.frame(
    tarspot_risk = c(0.2, 0.4, 0.6),
    tarspot_risk_class = c("1.Low", "2.Moderate", "3.High")
  )
  
  # Test with tarspot and threshold 0.5
  result <- relabeling_class(test_data, "tarspot", 0.5)
  expect_equal(result$tarspot_risk_class[2], "3.High")  # Should be updated
  expect_equal(result$tarspot_risk_class[1], "1.Low")   # Should remain the same
  
  # Test with non-tarspot disease (should not change)
  result2 <- relabeling_class(test_data, "gls", 0.5)
  expect_equal(result2$tarspot_risk_class, test_data$tarspot_risk_class)
  
  # Test with default threshold (0.35)
  result3 <- relabeling_class(test_data, "tarspot", 0.35)
  expect_equal(result3$tarspot_risk_class, test_data$tarspot_risk_class)
})