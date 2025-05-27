library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
library(leaflet.extras)
library(httr)
library(tigris)
#library(sf)
#options(tigris_use_cache = TRUE)
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
    "Ag Forecasting",
    sidebarLayout(
      sidebarPanel(
        style = "height: 1000px;",
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
          condition = "input.ibm_data == false",
          radioButtons(
            inputId  = "disease_name",
            label    = "Select Disease:",
            choices  = c(
              "Tar Spot (🌽)"           = "tarspot",
              "Gray Leaf Spot (🌽)"     = "gls",
              "Frogeye Leaf Spot (🫘 Soybean)" = "fe",
              "Whitemold Irr 30in (🫘 Soybean)" = "whitemold_irr_30in",
              "Whitemold Irr 15in (🫘 Soybean)" = "whitemold_irr_15in",
              "Whitemold Dry (🫘 Soybean)"     = "whitemold_nirr"
            ),
            selected = "tarspot",
            inline   = TRUE   # set FALSE (default) for vertical stacking
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
            "Our model predictions are advised when air temperature is above 59 °F (15°C) in average from the last 30 days and the next conditions are satisfied.",
            style = "font-size: 0.6em; color: #777; font-style: italic; margin-top: 5px; margin-bottom: 5px;"
          )
        ),
        # Conditional panel for Frogeye Leaf Spot
        #conditionalPanel(
        #  condition = "input.disease_name == 'whitemold_irr_15in' && input.ibm_data == false",
        #  checkboxInput("flowers_present", "Are Flowers present?", value = TRUE),
        
        #),
        conditionalPanel(
          condition = "(input.disease_name == 'whitemold_irr_30in' || input.disease_name == 'whitemold_nirr' || input.disease_name == 'whitemold_irr_15in') && input.ibm_data == false",
          checkboxInput("flowers_present", "Are Flowers present?", value = TRUE),
          checkboxInput("row_closure", "Row Closure over threshold?", value = TRUE),
          
          div(
            class = "info_tooltip",
            tags$img(
              src = "IMG_0690.svg",
              style = "max-width: 2000px; max-height: 200px; display: block; margin: 10px auto;" # Limit height
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
        conditionalPanel(
          condition = "input.ibm_data",
          h3(
            "Agricultural Forecasting System based on a custom location 📍",  
            style = "font-family: 'Arial', Times, serif;"
          ),
          div(
            uiOutput("click_coordinates"),
            style = "margin-top: 10px; color: #666;"
          )
        ),
        conditionalPanel(
          condition = "!input.ibm_data",
          h3(
            "Agricultural Forecasting System based on Wisconet Weather Stations 🗼",
            style = "font-family: 'Arial', Times, serif;"
          )
        ),
        leafletOutput("risk_map", height = "740px"),
        # “Station mode” panel shown when ibm_data is FALSE
        conditionalPanel(
          condition = "!input.ibm_data",
          div(
            textOutput("station_specifications"),
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
        wellPanel(
          style = "background: #f9f9f9; border: 1px solid #ddd; padding: 15px; margin-top: 20px;",
          h4("Download Data", style = "margin-top: 0;"),
          p("Export the currently displayed station-level forecast data as CSV."),
          downloadButton(
            "download_stations",
            label = "Download CSV",
            class = "btn-primary",
            style = "width: 100%;"
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
    title = "Weather",
    fluidPage(
      h3("Daily Weather and Crop Disease Risk Trends at the Given Location"),
      mainPanel(
        textOutput('station_specifications'),
        hr(),
        selectInput(
          "disease",
          "Select Disease:",
          choices = c(
            "Tar Spot (Corn)" = 'tarspot',
            "Gray Leaf Spot (Corn)" = 'gls',
            "Frogeye Leaf Spot (Soybean)" = 'fe',
            "Whitemold Irr 30in (Soybean)" = 'whitemold_irr_30in',
            "Whitemold Irr 15in (Soybean)" = 'whitemold_irr_15in',
            "Whitemold Dry (Soybean)" = 'whitemold_nirr'
          )
        ),
        hr(),
        plotOutput("risk_trend", width = "100%", height = "600px"),
        hr(),
        radioButtons(
          inputId  = "temp_unit",
          label    = "Temperature Units",
          choices  = c("°C" = "C", "°F" = "F"),
          selected = "F",
          inline   = TRUE
        ),
        plotOutput("weather_trend", width = "100%", height = "600px")
      )
    )
  ),
  
  # Tab 6: About
  tabPanel(
    title = "About",
    fluidPage(
      # your existing about_page UI (if it's HTML or markdown)
      about_page1,
      fluidRow(
        column(6,
               tags$img(
                 src    = "wisconet.png", 
                 style  = "width:100%; height:auto; display:block; margin:0 auto;",
                 alt    = "Wisconet Station"
               )
        ),
        column(6,
               tags$img(
                 src    = "pin.png", 
                 style  = "width:100%; height:auto; display:block; margin:0 auto;",
                 alt    = "Custom Location"
               )
        )
        
      ),
      # maybe a caption or extra text below
      tags$p("Forecasting Tool information based on location.",
             style = "text-align:center; color:#666; margin-top:10px;"),
      hr(),
      about_page2,
      # two side-by-side images from www/
      fluidRow(
        column(6,
               tags$img(
                 src    = "IMG_0690.PNG", 
                 style  = "width:100%; height:auto; display:block; margin:0 auto;",
                 alt    = "First Logo"
               )
        )
      ),
      
      # maybe a caption or extra text below
      tags$p("Row thresholds for soybean risk.",
             style = "text-align:center; color:#666; margin-top:10px;")
    )
  )
  
)
