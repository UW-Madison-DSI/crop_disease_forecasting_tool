about_page<-fluidPage(
  h3("About the Agricultural Forecasting and Advisory System"),
  p("This application provides weather-based forecasting and risk assessments for various crop diseases, helping farmers, crop advisors, and agricultural researchers to make data-driven decisions."),
  hr(),
  h4("Features:"),
  tags$ul(
    tags$li("Interactive weather map with disease risk visualization."),
    tags$li("Dynamic data for different forecasting dates and diseases."),
    tags$li("Downloadable reports.")
  ),
  hr(),
  h4("How It Works:"),
  p("The application uses data from trusted weather and university agricultural sources to forecast the risk of crop diseases."),
  tags$ul(
    tags$li("Select a disease and forecasting date to view the risk map."),
    tags$li("Users can click on stations to get more details and center the map on specific locations: this action will trigger the weather charts and downloadable information in the included tabs along the top of the window."),
    tags$li("The “Trends” tab provides more detailed information of a single station, including past weather data used to provide a disease forecast and disease risk trends over the past 7 days."),
    tags$li("The “Downloads” tab allows the user to download reports of the chosen station and a summary of all stations forecast (csv file).")
  ),
  hr(),
  h4("Credits:"),
  p("This project is supported by the Open Source Program Office and relies on contributions from multiple research groups."),
  tags$ul(
    tags$li("Weather data sourced from Wisconet and the IBM Environmental Intelligence Suite."),
    tags$li(
      "Crop disease models were developed based on disease information from University researchers around the country including partners of the ",
      tags$a(href = "https://cropprotectionnetwork.org", "Crop Protection Network", target = "_blank"),
      " and partially supported by the ",
      tags$a(href = "https://agpmt.org", "National Predictive Modeling Tool Initiative.", target = "_blank")
    ),
    tags$li(
      "Development support for this tool was made by the ",
      tags$a(href = "https://dsi.wisc.edu", "Data Science Institute at the University of Madison-Wisconsin.", target = "_blank")
    )
  ),
  hr(),
  h4("Contact Us:"),
  p("For inquiries or feedback, please reach out to us:"),
  tags$div(
    style = "margin-bottom: 16px;",  # Add spacing between items
    tags$div(
      tags$span(
        tags$img(
          src = "https://cdn-icons-png.flaticon.com/512/561/561127.png", 
          alt = "Mail Icon", 
          style = "width: 16px; height: 16px; margin-right: 8px;"
        ),
        tags$span(style = "font-weight: bold; color: black;", "Open Source Program Office: "),
        tags$a(
          href = "mailto:ospo@datascienceinstitute.wisc.edu", 
          style = "text-decoration: none; color: #007BFF;",
          "ospo@datascienceinstitute.wisc.edu"
        )
      )
    ),
    tags$div(
      tags$span(
        tags$img(
          src = "https://cdn-icons-png.flaticon.com/512/561/561127.png", 
          alt = "Mail Icon", 
          style = "width: 16px; height: 16px; margin-right: 8px;"
        ),
        tags$span(style = "font-weight: bold; color: black;", "Dr. Damon Smith (Extension Plant Pathologist): "),
        tags$a(
          href = "mailto:damon.smith@wisc.edu", 
          style = "text-decoration: none; color: #007BFF;",
          "damon.smith@wisc.edu"
        )
      )
    ),
    tags$div(
      tags$span(
        tags$img(
          src = "https://cdn-icons-png.flaticon.com/512/561/561127.png", 
          alt = "Mail Icon", 
          style = "width: 16px; height: 16px; margin-right: 8px;"
        ),
        tags$span(style = "font-weight: bold; color: black;", "Maria Oros (Data Scientist, Software Maintainer): "),
        tags$a(
          href = "mailto:maria.oros@wisc.edu", 
          style = "text-decoration: none; color: #007BFF;",
          "maria.oros@wisc.edu"
        )
      )
    )
  ),
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
