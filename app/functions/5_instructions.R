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
    tags$li("The “Station summary” tab provides more detailed information of a single station or given location, about the disease forecast and disease risk trends over the past 7 days and allows the user to download a PDF report available for the Station Speficification only."),
    tags$li("The “Trends” tab allows the user to download a CSV of the chosen station or location forecast data (csv file).")
  ),
  hr(),
  h4("Credits:"),
  p("This project is supported by the",
    tags$a(href="https://ospo.wisc.edu","Open Source Program Office", target = "_blank"),
    " and relies on contributions from multiple research groups at the University of Madison Wisconsin."),
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
  h4("Plant Disease Models"),
  p("Selected field crops and vegetable disease model outputs are provided. These models are subject to change. The calculations used to generate each model prediction can be viewed in the source code."),
  tags$ul(
    tags$li('White mold (aka Sporecaster) - dry, irrigated 15-inch row spacing, irrigated 30-inch row spacing - probability of apothecial presence. More information: ',
            tags$a(href = 'https://cropprotectionnetwork.org/news/smartphone-application-to-forecast-white-mold-in-soybean-now-available-to-growers',"Link", target = "_blank")),
    tags$li('Frogeye Leaf Spot of soybean - probability of presence. More information: ',
            tags$a(href = 'https://cropprotectionnetwork.org/encyclopedia/frogeye-leaf-spot-of-soybean',"Link", target = "_blank")),
    tags$li('Gray Leaf Spot of corn - probability of presence. More information: ',
            tags$a(href = 'https://cropprotectionnetwork.org/encyclopedia/gray-leaf-spot-of-corn',"Link", target = "_blank")),
    tags$li('Tar Spot of corn (aka Tarspotter) - probability of presence. More information:',
            tags$a(href = 'https://cropprotectionnetwork.org/encyclopedia/tar-spot-of-corn',"Link", target = "_blank"))
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
