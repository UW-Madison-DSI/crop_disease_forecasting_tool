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
  }else if (disease=='fe'){
    return(
      "Frogeye Leaf Spot"
    )
  }else if (disease=='whitemold_irr_30in'){
    return(
      "Whitemold Irrigation (30in)"
    )
  }else if (disease=='whitemold_irr_15in'){
    return(
      "Whitemold Irrigation (15in)"
    )
  }else if (disease=='whitemold_nirr'){
    return(
      "Whitemold Dry"
    )
  }
}

risk_class_function <- function(risk, disease_name, threshold) {
  #Risk class from Damon et al
  if (disease_name == "tarspot") {
    return(ifelse(risk < .2, "Low",
                  ifelse(risk > threshold/100, "High", "Moderate")))
  } else if (disease_name == "gls") {
    return(ifelse(risk < .5, "Low",
                  ifelse(risk > threshold/100, "High", "Moderate")))
  } else if (disease_name == "fe") {
    return(ifelse(risk < .4, "Low",
                  ifelse(risk > threshold/100, "High", "Moderate")))
  } else {
    return("No Class")
  }
}


custom_palette <- function(x) {
  if (x =="Low") {
    return("#88CCEE")
  } else if (x  =="Moderate") {
    return("#DDCC77")
  } else if (x =="High"){
    return("#CC6677")
  } else if (x  =="No Class"){
    return("gray")
  }
}

  
data_transform_risk_labels<-function(data, disease_name){
  if (disease_name == 'tarspot') {
    data$fill_color <- sapply(data$tarspot_risk_class, custom_palette)
    
  } else if (disease_name == 'gls') {
    data$fill_color <- sapply(data$gls_risk_class, custom_palette)
    
  } else if (disease_name == 'fe') {
    data$fill_color <- sapply(data$fe_risk_class, custom_palette)
    
  } else if(disease_name == 'whitemold_irr_30in'){
    data$Risk <- data$whitemold_irr_30in_risk

  }else if(disease_name == 'whitemold_irr_15in'){
    data$Risk <- data$whitemold_irr_15in_risk

  }else if(disease_name == 'whitemold_nirr'){
    data$Risk <- data$whitemold_nirr_risk
  }
  return(data)
}
