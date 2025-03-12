library(testthat)
library(shiny)
library(leaflet)

test_that("risk color palette works correctly", {
  # This is testing a function used in your server logic
  risk_class_vector <- c("1.Low", "2.Moderate", "3.High", "Inactive")
  
  # Test the color palette creation
  pal <- colorFactor(
    palette = c("1.Low" = "#009E73", "2.Moderate" = "#F0E442", "3.High" = "#D55E00", "Inactive" = "gray"),
    domain = risk_class_vector
  )
  
  # Check if the palette returns the expected colors
  expect_equal(pal("1.Low"), "#009E73")
  expect_equal(pal("3.High"), "#D55E00")
  expect_equal(pal("Inactive"), "gray")
})

# Test popup content format
test_that("popup content string formats correctly", {
  # Test popup string formatting
  popup_content_str <- "<strong>Station:</strong> %s<br><strong>Location:</strong> %s <br><strong>Region:</strong> %s<br><strong>Forecasting Date:</strong> %s<br><strong>Risk Models</strong><br><strong>Tarspot:</strong> %.2f%%<br><strong>Frogeye Leaf Spot:</strong> %.2f%%<br><strong>Gray Leaf Spot:</strong> %.2f%%<br><strong>Whitemold Irrigation (30in):</strong> %.2f%%<br><strong>Whitemold Irrigation (15in):</strong> %.2f%%"
  
  # Test parameters
  station_name <- "TEST"
  location <- "Test Location"
  region <- "South"
  forecasting_date <- as.Date("2023-07-15")
  tarspot_risk <- 0.34
  fe_risk <- 0.45
  gls_risk <- 0.56
  whitemold_nirr_risk <- 0.67
  whitemold_irr_30in_risk <- 0.78
  whitemold_irr_15in_risk <- 0.89
  
  # Format the string
  formatted_popup <- sprintf(
    popup_content_str,
    station_name,
    location,
    region,
    forecasting_date,
    tarspot_risk * 100,
    fe_risk * 100,
    gls_risk * 100,
    whitemold_nirr_risk * 100,
    whitemold_irr_30in_risk * 100
  )
  
  # Check if the popup content contains expected information
  expect_true(grepl(station_name, formatted_popup))
  expect_true(grepl(location, formatted_popup))
  expect_true(grepl(as.character(forecasting_date), formatted_popup))
  expect_true(grepl("34.00%", formatted_popup))  # tarspot_risk * 100
})