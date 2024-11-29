risk_class_function <- function(risk, disease_name, threshold) {
  if (disease_name == "tarspot") {
    return(ifelse(risk <= threshold/100, "Low",
                  ifelse(risk <= .5, "Moderate", "High")))
  } else if (disease_name == "gls") {
    return(ifelse(risk <= threshold/100, "Low",
                  ifelse(risk <= .6, "Moderate", "High")))
  } else if (disease_name == "frogeye_leaf_spot") {
    return(ifelse(risk <= threshold/100, "Low",
                  ifelse(risk <= threshold, "Moderate", "High")))
  } else {
    return("No Class") # Default case for unsupported diseases
  }
}

custom_disease_name <- function(disease){
  if (disease=='tarspot'){
    return(
      "Tar Spot"
    )
  }else if (disease=='gls'){
    return(
      "Gray Leaf Spot"
    )
  }else if (disease=='frogeye_leaf_spot'){
    return(
      "Frogeye Leaf Spot"
    )
  }
}

plot_data<-function(data, input){
  if (!is.null(data)) {
    # Select the appropriate risk column based on the disease
    if (input$disease_name == 'tarspot') {
      data$Risk_Class <- risk_class_function(data$tarspot_risk, input$disease_name, .6)
      
      plot_data <- data %>% 
        select(forecasting_date, tarspot_risk, Risk_Class) %>% 
        rename(Risk = tarspot_risk)
      
    } else if (input$disease_name == 'gls') {
      data$Risk_Class <- risk_class_function(data$gls_risk, input$disease_name, .6)
      
      plot_data <- data %>% 
        select(forecasting_date, gls_risk, Risk_Class) %>% 
        rename(Risk = gls_risk)
      
    } else if (input$disease_name == 'frogeye_leaf_spot') {
      data$Risk_Class <- risk_class_function(data$frogeye_risk, input$disease_name, .6)
      
      plot_data <- data %>% 
        select(forecasting_date, frogeye_risk, Risk_Class) %>% 
        rename(Risk = frogeye_risk)
    }
    return(plot_data)
  }
}

plot_trend_7days <- function(df){
  df$date <- as.Date(df$forecasting_date, format='%Y-%m-%d')
  ggplot(df, aes(x = date, y = Risk)) +
    geom_line(color = "#0C7BDC") +
    geom_point(aes(color = Risk_Class), size = 4) +  # Map color to Risk_Class
    geom_text(aes(label = Risk_Class),
              vjust = -0.5,
              color = "black",
              size = 5) +
    labs(#title = paste(station$name, "Station,", station$region, "Region,", station$state),
      x = "Date",
      y = "Risk (%)") +
    scale_y_continuous(labels = percent_format(scale = 1),
                       breaks = seq(0, 100, by = 20)) +
    
    # Set colors for Risk_Class categories
    scale_color_manual(values = c("High" = "black", "Medium" = "#FFC20A", "Low" = "darkgreen")) +
    
    # Control x-axis date formatting and frequency
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate date labels for readability
    )+guides(color = "none")  # Remove the color legend
}