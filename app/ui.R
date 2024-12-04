library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
library(leaflet.extras)
library(httr)
library(tigris)
library(sf)
options(tigris_use_cache = TRUE)
library(DT)
library(shinyWidgets)

source("functions/instructions.R")

logo_src = "logos/uw-logo-horizontal-color-web-digital.svg"
tool_title <- "Agricultural Forecasting and Advisory System"

# UI 
ui <- navbarPage(
  title = tool_title,
  theme = shinythemes::shinytheme("flatly"), 
  footer = div(class = "footer-text", "© 2024 UW-Madison"),
  
  # Tab 1: Weather Map
  tabPanel(
    "Disease Forecasting",
    sidebarLayout(
      sidebarPanel(
        div(
          class = "logo-container",
          tags$img(
            src = logo_src,
            style = "max-width: 100%; max-height: 80px; display: block; margin: 10px auto;" # Limit height
          )
        ),
        hr(),
        switchInput(
          inputId = "ibm_data", 
          label = "Pin my location", 
          onLabel = "ON", 
          offLabel = "OFF", 
          value = FALSE
        ),
        hr(),
        selectInput(
          "disease_name",
          "Select Disease:",
          choices = c(
            "Tar Spot" = 'tarspot',
            "Gray Leaf Spot" = 'gls',
            "Frogeye Leaf Spot" = 'frogeye_leaf_spot'
          )
        ),
        dateInput(
          "forecast_date",
          "Select Forecasting Date:",
          value = Sys.Date(),
          min = '2024-01-01',
          max = Sys.Date()
        ),
        hr(), 
        h4("Crop Management"),
        checkboxInput("no_fungicide", "No fungicide applied in the last 14 days?", value = TRUE),
        
        # Conditional panel for Tar Spot
        conditionalPanel(
          condition = "input.disease_name == 'tarspot'",
          checkboxInput("crop_growth_stage", "Growth stage in the V10-R3 range?", value = TRUE),
          sliderInput(
            "risk_threshold",
            "Risk Threshold:",
            min = 20,
            max = 50,
            value = 35,
            step = 1
          )
        ),
        
        # Conditional panel for Frogeye Leaf Spot
        conditionalPanel(
          condition = "input.disease_name == 'frogeye_leaf_spot'",
          checkboxInput("crop_growth_stage", "Growth stage in the R1-R5 range?", value = TRUE),
          sliderInput(
            "risk_threshold",
            "Risk Threshold:",
            min = 40,
            max = 50,
            value = 50,
            step = 1
          )
        ),
        
        # Conditional panel for GLS
        conditionalPanel(
          condition = "input.disease_name == 'gls'",
          checkboxInput("crop_growth_stage", "Growth stage in the V10-R3 range?", value = TRUE),
          sliderInput(
            "risk_threshold",
            "Risk Threshold:",
            min = 40,
            max = 60,
            value = 60,
            step = 1
          )
        ),
        hr(), 
        conditionalPanel(
          condition = "input.ibm_data == false",  # Use lowercase `false` in JavaScript
          h4("Map Layers"),
          checkboxInput("show_heatmap", "Show Heat Map", value = FALSE)
        )
      ),
      mainPanel(
        leafletOutput("risk_map", height = 750),
        conditionalPanel(
          condition = "input.ibm_data == false",
          div(
            textOutput("map_info"),
            style = "margin-top: 10px; color: #666;"
          )
        ),
        conditionalPanel(
          condition = "input.ibm_data != false",
          div(
            textOutput('click_coordinates'),
            style = "margin-top: 10px; color: #666;"
          )
        ),
        conditionalPanel(
          condition = "input.ibm_data == false",
          div(
            textOutput("station_count"),
            style = "margin-top: 10px; color: #666; font-size: 14px;"
        )
      )
      )
    )
  ),
  
  # Tab 2: Station Forecasting Risk and Weather Trends
  tabPanel(
    title = "Station Forecasting Risk and Weather Trends",
    fluidPage(
      h3("Station Forecasting Risk and Weather Trends"),
      mainPanel(
        textOutput('station_specifications'),
        hr(),
        #DTOutput("station_trend"),
        plotOutput("risk_trend", height = "400px", width = "100%"),   
        hr(),
        plotOutput('air_temperature_plot', height = "1200px", width = "100%")
      )
    )
  ),
  
  # Tab 3: Downloads
  tabPanel(
    title = "Downloads",
    fluidPage(
      h3("Downloads"),
      hr(),
      textOutput("download_reported"),
      p("Downloadable content as a summary of the risk trend for the specified disease, wisconet station and forecasting date."),
      div(
        downloadButton("download_report", "Download Report", 
                       class = "btn-primary", 
                       style = "text-align: center; margin-top: 10px;")
      ),
      hr(),
      p("All stations risk forecasting for the specified date as csv file."),
      downloadButton("download_stations", "Download csv", 
                     class = "btn-primary", 
                     style = "text-align: center; margin-top: 10px;")
    )
  ),
  
  # Tab 6: About
  tabPanel(
    title = "About",
    about_page
  )
  
)
