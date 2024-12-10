################################################################ Risk labels
custom_disease_name <- function(disease){
  #Function to map the acronim of disease to the custom name
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

risk_class_function <- function(risk, disease_name, threshold) {
  #Risk class from Damon et al
  if (disease_name == "tarspot") {
    return(ifelse(risk < .2, "Low",
                  ifelse(risk > threshold/100, "High", "Moderate")))
  } else if (disease_name == "gls") {
    return(ifelse(risk < .4, "Low",
                  ifelse(risk > threshold/100, "High", "Moderate")))
  } else if (disease_name == "frogeye_leaf_spot") {
    return(ifelse(risk < .4, "Low",
                  ifelse(risk > threshold/100, "High", "Moderate")))
  } else {
    return("No Class")
  }
}


data_transform_risk_labels<-function(data, input){
  
  print(data)
  if (input$disease_name == 'tarspot') {
    data$risk <- data$tarspot_risk
    data$risk_class <- risk_class_function(data$tarspot_risk, 
                                           input$disease_name, 
                                           input$risk_threshold)
    
  } else if (input$disease_name == 'gls') {
    data$risk <- data$gls_risk
    data$risk_class <- risk_class_function(data$gls_risk, 
                                           input$disease_name, 
                                           input$risk_threshold)
    
  } else if (input$disease_name == 'frogeye_leaf_spot') {
    data$risk <- data$frogeye_risk
    data$risk_class <- risk_class_function(data$frogeye_risk, 
                                           input$disease_name, 
                                           input$risk_threshold)
  }
  data_f <- data %>%
    #select(forecasting_date, risk, risk_class) %>%
    rename(
      #Station = station_name,
      `Forecasting Date` = forecasting_date,
      Risk = risk,
      `Risk Class` = risk_class
    )
  return(data_f)
}
