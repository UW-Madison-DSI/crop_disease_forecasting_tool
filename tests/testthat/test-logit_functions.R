install.packages("testthat")

library(testthat)
library(here)
source(here("R", "logit_functions.R"))

#source("R/logit_functions.R")  # Ensure this path correctly points to the functions you want to test

test_that("logistic function returns probabilities between 0 and 1", {
  expect_true(all(logistic(c(-5, 0, 5)) >= 0 & logistic(c(-5, 0, 5)) <= 1))
})

test_that("calculate_disease_risk handles threshold values correctly", {
  logit_values <- c(-1, 0, 1)
  thresholds <- c(0.3, 0.7)
  result <- calculate_disease_risk(logit_values, thresholds, "Test Disease")
  expect_equal(result$disease, "Test Disease")
  expect_true(result$probability >= 0 && result$probability <= 100)
})

test_that("classify_risk categorizes risk correctly", {
  expect_equal(classify_risk(0.2, 0.3, 0.7), "Low")
  expect_equal(classify_risk(0.5, 0.3, 0.7), "Medium")
  expect_equal(classify_risk(0.8, 0.3, 0.7), "High")
})
