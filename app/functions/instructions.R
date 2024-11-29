instructions_panel <- tags$div(
  style = "margin-top: 20px;",
  tags$div(
    id = "triangleToggle",
    style = "
                  width: 0; 
                  height: 0; 
                  border-left: 15px solid transparent; 
                  border-right: 15px solid transparent; 
                  border-top: 15px solid #007bff; 
                  cursor: pointer; 
                  margin: 0 auto;
                ",
    `data-toggle` = "collapse",
    `data-target` = "#collapseInstructions"
  )
)


instructions_block <- tags$div(
  id = "collapseInstructions",
  class = "collapse",
  style = "border: 1px solid #ccc; padding: 10px; margin-top: 10px; border-radius: 3px;",
  tags$h4("User Guide", style = "border: 1px solid #ccc; padding: 10px; margin-top: 0; 
                border-radius: 3px;"),
  tags$p("1. Use the Action Threshold slider to set the risk threshold. 
                Leave the slider at the research-based default 
               threshold unless you have informed reason to believe 
               it should be adjusted."),
  tags$p("2. Select a station from the dropdown menu."),
  tags$p("3. Pick a forecast date to view the risk data."),
  tags$p("4. Check if no fungicide has been applied in the last 14 days."),
  tags$p("5. Ensure the crop is within the V10-R3 growth stage."),
  tags$p("6. Push Run the Model to see the map and risk trend for insights."),
  tags$p("7. You can also download a PDF report of the forecast obtained for your location 
                of interest by pushing the “Download Report” button 
                that will appear after the forecast is obtained.")
)


instructions_section<- tags$p(
  "Need help getting started? Click below for step-by-step instructions tailored to this app.",
  style = "
            color: gray; 
            font-weight: sans; /* Corrected to font-weight */ 
            font-size: 12px; 
            margin-top: 35px; 
            width: 300px; /* Adjust the width as needed */
            margin-left: auto; 
            margin-right: auto;
          "
)
#################################################### Contact info
# Contact information block
contact_info <- tags$div(
  class = "contact-info",
  style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ccc; font-size: 14px;",
  tags$p(
    tags$strong("Contact Information:"),
    style = "font-size: 16px;"
  ),
  #tags$p("For questions or suggestions please contact:"),
  tags$div(
    tags$span(
      tags$img(src = "https://cdn-icons-png.flaticon.com/512/732/732200.png", 
               alt = "Email Icon", style = "width: 16px; height: 16px; margin-right: 8px;"),
      "Open Source Program Office: ",
      tags$a(href = "mailto:ospo@datascience.wisc.edu", "ospo@datascience.wisc.edu")
    ),
    style = "margin-bottom: 10px;"
  ),
  tags$div(
    tags$span(
      tags$img(src = "https://cdn-icons-png.flaticon.com/512/732/732200.png", 
               alt = "Email Icon", style = "width: 16px; height: 16px; margin-right: 8px;"),
      "Dr. Damon Smith: ",
      tags$a(href = "mailto:dlsmith26@wisc.edu", "dlsmith26@wisc.edu")
    ),
    style = "margin-bottom: 10px;"
  ),
  tags$div(
    tags$span(
      tags$img(src = "https://cdn-icons-png.flaticon.com/512/732/732200.png", 
               alt = "Email Icon", style = "width: 16px; height: 16px; margin-right: 8px;"),
      "Maria Oros (software mantainer): ",
      tags$a(href = "mailto:maria.oros@wisc.edu", "maria.oros@wisc.edu")
    ),
    style = "margin-bottom: 10px;"
  ),
  tags$div(
    tags$span(
      tags$img(src = "https://cdn-icons-png.flaticon.com/512/25/25231.png", 
               alt = "GitHub Icon", style = "width: 16px; height: 16px; margin-right: 8px;"),
      "Visit our GitHub repository: ",
      tags$a(href = "https://github.com/UW-Madison-DSI/corn_disease_forecast_api.git", 
             "https://github.com/UW-Madison-DSI/corn_disease_forecast_api.git")
    )
  )#,
  #tags$p("University of Wisconsin-Madison, Data Science Institute")
)



##################################################################### Actions
risk_buttom<-tags$div(
  `data-toggle` = "tooltip", 
  title = "The action threshold defaults to a research-based appropriate level. You are encouraged to leave the threshold at the default.",
  sliderInput("risk_threshold", "Action Threshold (%)", 
              min = 20, max = 50, value = 35, step = 1)
)

# DateInput with tooltip
forecast_date_buttom<-tags$div(
  `data-toggle` = "tooltip", 
  title = "Pick a date for which you would like a disease risk forecast.",
  dateInput("forecast_date", "Select Forecast Date", 
            value = Sys.Date(), 
            min = as.Date("2024-07-19"), 
            max = Sys.Date())
)

# CheckboxInput with tooltip
fungicide_applied_buttom<-tags$div(
  `data-toggle` = "tooltip", 
  title = "Check if no fungicide has been applied recently; Forecasts will only be made if no fungicide has been used in the past two weeks.",
  checkboxInput("fungicide_applied", "No Fungicide in the last 14 days?", value = FALSE)
)


crop_growth_stage_buttom<-tags$div(
  `data-toggle` = "tooltip", 
  title = "Check Check if no fungicide has been applied recently; Forecasts will only be made if the crop you are scouting is between V10 and R3 growth stages.",
  checkboxInput("crop_growth_stage", "Growth stage within V10-R3?", value = FALSE)
)


#---------------------------------------------------
about_page<-fluidPage(
  h3("About the Agricultural Forecasting and Advisory System"),
  p("This application provides weather-based forecasting and risk assessments for various crop diseases, helping farmers and agricultural researchers make data-driven decisions."),
  hr(),
  h4("Features:"),
  tags$ul(
    tags$li("Interactive weather map with disease risk visualization"),
    tags$li("Dynamic data for different forecasting dates and diseases"),
    tags$li("Downloadable Report")
  ),
  hr(),
  h4("How It Works:"),
  p("The application uses data from trusted weather and agricultural sources to forecast the risk of crop diseases."),
  tags$ul(
    tags$li("Select a disease and forecasting date to view the risk map."),
    tags$li("The map highlights disease risk levels across different weather stations."),
    tags$li("Users can click on stations to get more details and center the map on specific locations.")
  ),
  hr(),
  h4("Credits:"),
  p("This application was developed by a multidisciplinary team of data scientists and agricultural researchers."),
  tags$ul(
    tags$li("Weather data provided by: Wisconet Stations"),
    tags$li("Crop disease data provided by: Plant Pathology at UW Madison"),
    tags$li("This is an innitiative from: the Open Source Program Office at UW Madison")
  ),
  hr(),
  h4("Contact Us:"),
  p("For inquiries or feedback, please reach out to us:"),
  tags$ul(
    tags$li(tags$a(href = "mailto:contact@forecasting-system.com", "Email: ospo@datascienceinstitute.wisc.edu")),
    tags$li(tags$a(href = "mailto:contact@forecasting-system.com", "Email: damon.smith@wisc.edu")),
    tags$li(tags$a(href = "mailto:contact@forecasting-system.com", "Email: maria.oros@wisc.edu"))
  ),
  hr(),
  h4("Acknowledgments:"),
  p("This project is supported by The Open Source Program Office and relies on contributions from multiple research groups."),
  hr(),
  h4("For Developers:"),
  tags$div(
    style = "margin-bottom: 16px;",  # Add spacing between items
    tags$div(
      tags$span(
        tags$img(
          src = "https://cdn-icons-png.flaticon.com/512/25/25231.png", 
          alt = "GitHub Icon", 
          style = "width: 16px; height: 16px; margin-right: 8px;"
        ),
        "Visit our GitHub repository: ",
        tags$a(
          href = "https://github.com/UW-Madison-DSI/corn_disease_forecast_api.git",
          target = "_blank",  # Open link in a new tab
          style = "text-decoration: none; color: #007BFF;",  # Link styling
          "UW-Madison DSI GitHub Repository"
        )
      )
    )
  ),
  tags$div(
    style = "margin-bottom: 16px;",  # Add spacing between items
    tags$div(
      tags$span(
        tags$img(
          src = "https://cdn-icons-png.flaticon.com/512/103/103093.png", 
          alt = "API Icon", 
          style = "width: 24px; height: 24px; margin-right: 8px;"
        ),
        "Use our API: ",
        tags$a(
          href = "https://connect.doit.wisc.edu/forecasting_crop_disease/",
          target = "_blank",  # Open link in a new tab
          style = "text-decoration: none; color: #007BFF;",  # Link styling
          "Forecasting Disease API"
        )
      )
    )
  )
)
