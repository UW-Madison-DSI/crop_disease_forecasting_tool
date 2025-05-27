about_page1<-fluidPage(
  h3("About the Agricultural Forecasting and Advisory System"),
  p("This application provides weather-based forecasting and risk assessments for various crop diseases, helping farmers, crop advisors, and agricultural researchers to make data-driven decisions."),
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
    style = "margin-bottom: 16px;",
    
    # First URL
    tags$div(
      tags$span(
        tags$img(
          src    = "https://cdn-icons-png.flaticon.com/512/25/25231.png",
          alt    = "GitHub Icon",
          style  = "width: 16px; height: 16px; margin-right: 8px;"
        ),
        tags$a(
          href   = "https://github.com/UW-Madison-DSI/ag_forecasting_app.git",
          target = "_blank",
          style  = "text-decoration: none; color: #007BFF;",
          "UW-Madison DSI Ag Forecasting (this) APP"
        )
      )
    ),
    
    # Second URL
    tags$div(
      tags$span(
        tags$img(
          src = "https://cdn-icons-png.flaticon.com/512/103/103093.png", 
          alt = "API Icon", 
          style = "width: 24px; height: 24px; margin-right: 8px;"
        ),
        tags$a(
          href   = "https://github.com/UW-Madison-DSI/ag_forecasting_api.git",
          target = "_blank",
          style  = "text-decoration: none; color: #007BFF;",
          "UW-Madison DSI Ag Forecasting API"
        )
      )
    )
  ),
  hr(),
  h4("How It Works:"),
  p("The application uses data from WiscoNet and forecasting Models that were developed by researchers in the University of Madison-Wisconsin to forecast the risk of crop diseases. In this app:"),
  tags$ul(
    tags$li("The 'Ag Forecasting' tab provides a risk map on the Wisconet Stations and the option to choose a pin to retrieve forecasting information."),
    tags$ul(
      tags$li("Keep 'Pin my location' OFF if you want to select a Wisconet weather station, or turn it ON to pin your location in map."),
      tags$li("Select a crop disease and forecasting date to view the corresponding risk map."),
      tags$li("Select a wisconet weather station by double clicking on it, this will display a pop up with the summary of crop diseases and risk."),
      tags$li("If you choosed to pin your location, please pin it in the map and push 'Run Model', this will display a summary of crop diseases and risk at the top of the app."),
    ),
    tags$li("The 'Weather' tab provides detailed information about the selected station or location, including disease forecasts and risk trends for the past 7 days. A PDF report is available for the Station Specification."),
  )
)

about_page2<-fluidPage(
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
  )
)
