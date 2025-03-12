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

source("functions/5_instructions.R")

logo_src = "logos/uw-logo-horizontal-color-web-digital.svg"
tool_title <- "Agricultural Forecasting and Advisory System"

# UI 
ui <- navbarPage(
  title = tool_title,
  theme = shinythemes::shinytheme("flatly"), 
  #footer = div(class = "footer-text", "© 2024 UW-Madison"),
  # Footer as a div with "Powered by Wisconet"
  footer = div(
    class = "footer-text",
    "© 2024 UW-Madison | ",
    tags$a(href = "https://wisconet.wisc.edu", target = "_blank",
           style = "color:#D3D3D3; font-weight: bold;",
           "Powered by Wisconet"),
    style = "width:100%; text-align:center; padding:5px; background-color:#B22234;" # Removed position:fixed and reduced padding
  ),
  
  # Tab 1: Weather Map
  tabPanel(
    "Disease Forecasting",
    sidebarLayout(
      sidebarPanel(
        style = "height: 800px;",
        div(
          class = "logo-container",
          tags$img(
            src = logo_src,
            style = "max-width: 500px; max-height: 100px; display: block; margin: 10px auto;" # Limit height
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
        conditionalPanel(
          condition = "input.ibm_data == false",  # Condition to display disease selection
          selectInput(
            "disease_name",
            "Select Disease:",
            choices = c(
              "Tar Spot (Corn)" = 'tarspot',
              "Gray Leaf Spot (Corn)" = 'gls',
              "Frogeye Leaf Spot (Soybean)" = 'fe',
              "Whitemold Irr 30in (Soybean)" = 'whitemold_irr_30in',
              "Whitemold Irr 15in (Soybean)" = 'whitemold_irr_15in',
              "Whitemold Dry (Soybean)" = 'whitemold_nirr'
            )
          )
        ),
        dateInput(
          "forecasting_date",
          "Select Forecasting Date:",
          value = Sys.Date(),
          min = '2024-04-02',
          max = Sys.Date()
        ),
        hr(), 
        conditionalPanel(
          condition = "input.ibm_data == false",
          h4("Crop Management"),
          p(
            "Our model predictions are advised when air temperature is above 15°C in average from the last 30 days and the next conditions are satisfied.",
            style = "font-size: 0.6em; color: #777; font-style: italic; margin-top: 5px; margin-bottom: 5px;"
          )
        ),
        # Conditional panel for Frogeye Leaf Spot
        conditionalPanel(
          condition = "input.disease_name == 'whitemold_irr_15in' && input.ibm_data == false",
          checkboxInput("flowers_present", "Are Flowers present?", value = TRUE),

        ),
        conditionalPanel(
          condition = "(input.disease_name == 'whitemold_irr_30in' || input.disease_name == 'whitemold_nirr') && input.ibm_data == false",
          checkboxInput("flowers_present", "Are Flowers present?", value = TRUE),
          checkboxInput("row_closure", "Row Closure over threshold?", value = TRUE),

          div(
            class = "info_tooltip",
            tags$img(
              src = "IMG_0690.svg",
              style = "max-width: 2000px; max-height: 100px; display: block; margin: 10px auto;" # Limit height
            )
          )
        ),
        conditionalPanel(
          checkboxInput("no_fungicide", "No fungicide applied in the last 14 days?", value = TRUE),
          condition = "input.disease_name == 'fe' && input.ibm_data == false",
          checkboxInput("crop_growth_stage", "Growth stage in the R1-R5 range?", value = TRUE)
        ),
        
        # Conditional panel for GLS
        conditionalPanel(
          checkboxInput("no_fungicide", "No fungicide applied in the last 14 days?", value = TRUE),
          condition = "input.disease_name == 'gls' && input.ibm_data == false",
          checkboxInput("crop_growth_stage", "Growth stage in the V10-R3 range?", value = TRUE)
          
        ),
        
        # Conditional panel for Tar Spot
        conditionalPanel(
          checkboxInput("no_fungicide", "No fungicide applied in the last 14 days?", value = TRUE),
          condition = "input.disease_name == 'tarspot' && input.ibm_data == false",
          checkboxInput("crop_growth_stage", "Growth stage in the V10-R3 range?", value = TRUE)
        ),
        hr(), 
        #conditionalPanel(
        #  condition = "input.ibm_data == false",  # Use lowercase `false` in JavaScript
        #  h4("Map Layers"),
        #  checkboxInput("show_heatmap", "Show Heat Map", value = FALSE)
        #),
        conditionalPanel(
          condition = "input.ibm_data !== false",  # Ensure the condition is checking for exactly 'false'
          actionButton(
            inputId = "run_model", 
            label = "Run Model", 
            class = "btn-success"
          ),
          p(
            "This option provides a comprehensive summary of all diseases for the selected location and forecast date, assuming that the crop management practices are followed as recommended to assess the associated risk.",
            style = "font-size: 0.6em; color: #777; font-style: italic; margin-top: 5px; margin-bottom: 5px;"
          )
        )
      ),
      mainPanel(
        leafletOutput("risk_map", height = 800),
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
            uiOutput('click_coordinates'),
            style = "margin-top: 10px; color: #666;"
          )
        ),
        conditionalPanel(
          condition = "input.ibm_data == false",
          div(
            textOutput("station_count"),
            style = "margin-top: 10px; color: #666; font-size: 14px;"
          )
        ),
        p("Results will update after a short delay",
          style = "font-size: 0.6em; color: #777; font-style: italic; margin-top: 5px; margin-bottom: 5px;"
        )
      )
    )
  ),
  
  # Tab 2: Station Forecasting Risk and Weather Trends
  tabPanel(
    title = "Summary",
    fluidPage(
      h3("Station Summary"),
      mainPanel(
        textOutput('station_specifications'),
        hr(),
        radioButtons("disease", 
                     label = "Choose Disease",
                     choices = c("Gray Leaf Spot", "Tar Spot", "Frog Eye Leaf Spot",
                                 "Whitemold Irr (30in)", "Whitemold Irr (15in)", "Whitemold No Irr"),
                     selected = "Gray Leaf Spot",
                     inline = TRUE),
        hr(),
        plotOutput("risk_trend", width = "100%", height = "600px"),
        hr(),
        plotOutput("weather_trend", width = "100%", height = "600px")
      )
    )
  ),
  # Tab 3: Downloads
  tabPanel(
    title = "Downloads",
    fluidPage(
      h3("Downloads"),
      hr(),
      p("A tabular report on weather data and risk estimates for the selected location, such as a specific station or a location pinned on the map."),
      downloadButton("download_stations", "Download csv", 
                     class = "btn-primary", 
                     style = "text-align: center; margin-top: 10px;"),
      #hr(),
      #plotOutput('air_temperature_plot', height = "1200px", width = "100%")
    )
  ),
  
  # Tab 6: About
  tabPanel(
    title = "About",
    about_page
  )
)
