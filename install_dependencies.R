required_packages <- c("testthat", "dplyr", "httr","lubridate",
                       "jsonlite","zoo","ggplot2","scales",
                       'shiny','leaflet','shinydashboard','shinyWidgets','flexdashboard',
                       'gridExtra')  

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
