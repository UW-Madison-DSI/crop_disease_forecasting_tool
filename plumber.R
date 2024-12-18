library(plumber)



source("R/crop_mangm_validations.R")
source("R/logit_functions.R")

#* @apiTitle Crop Disease Risk Prediction API
#* @apiDescription This API predicts the risk of crop diseases (Spore, Tarspoter, Gray Leaf Spot and Frog Eye Leaf Spot) based on environmental data and user inputs.


#* Predict Tarspot Risk
#* @param mean_air_temp_30d_ma Numeric: 30-day moving average of mean air temperature (°C)
#* @param max_rh_30d_ma Numeric: 30-day moving average of max relative humidity (%)
#* @param tot_nhrs_rh90_14d_ma Numeric: 14-day moving average of total nighttime hours with 90% relative humidity (%) or above for each day.
#* @get /predict_tarspot_risk
function(
    mean_air_temp_30d_ma, 
    max_rh_30d_ma, 
    tot_nhrs_rh90_14d_ma) {
  
  # Validate inputs
  numeric_vars <- c(
    "mean_air_temp_30d_ma", 
    "max_rh_30d_ma", 
    "tot_nhrs_rh90_14d_ma")
  
  # Convert these variables to numeric
  convert_to_numeric(environment(), numeric_vars)
  
  
  if (is.null(mean_air_temp_30d_ma) || is.null(max_rh_30d_ma) || is.null(tot_nhrs_rh90_14d_ma)) {
    return(list(valid = FALSE, 
                message = "We can not compute the Tarspot Risk.", 
                reason = "Invalid input."))
  } else {
    # Call the tarspot risk calculation function
    result <- calculate_tarspot_risk_function(mean_air_temp_30d_ma, 
                                              max_rh_30d_ma, 
                                              tot_nhrs_rh90_14d_ma)
    
    if (is.null(result)) {
      return(list(error = "An error occurred while calculating Tarspot Risk."))
    }else{
      return(list(risk=result$tarspot_risk*100,
                  risk_class=result$tarspot_risk_class,
                  success=200,
                  disease_name='tarspot'))
    }
  }
}

#* Predict Gray Leaf Spot Risk
#* @param min_air_temp_21d_ma Numeric: 21-day moving average of minimum air temperature (°C)
#* @param min_dewpoint_30d_ma Numeric: 30-day moving average of minimum dew point (°C)
#* @get /predict_gray_leaf_spot_risk
function(
    min_air_temp_21d_ma, 
    min_dewpoint_30d_ma) {
  
  numeric_vars <- c("min_air_temp_21d_ma", "min_dewpoint_30d_ma")
  
  # Convert these variables to numeric
  convert_to_numeric(environment(), numeric_vars)
  
  
  if (is.null(min_air_temp_21d_ma) || is.null(min_dewpoint_30d_ma)) {
    return(list(valid = FALSE, 
                message = "We can not compute the Gray Leaf Spot Risk.", 
                reason = "Missing inputs"))
  } else {
    # Call the gray leaf spot risk calculation function
    result <- calculate_gray_leaf_spot_risk_function(min_air_temp_21d_ma, 
                                                     min_dewpoint_30d_ma)
    if(is.null(result)){
      return(NULL)
    }else{
      return(list(risk=result$gls_risk*100,
                  risk_class=result$gls_risk_class,
                  success=200,
                  disease_name='gls'))
    }
  }
}


#* Calculate Sporecaster risk in irrigated and non-irrigated fields
#* @param max_air_temp_30d_ma Numeric: 30-day moving average of maximum air temperature (°C)
#* @param max_windspeed_30d_ma Numeric: 30-day moving average of maximum wind speed (m/s)
#* @param max_rh_30d_ma Numeric: 30-day moving average of relative humidity (%) (only for irrigated fields)
#* @get /predict_sporecaster_risk
function(max_air_temp_30d_ma,
         max_windspeed_30d_ma,
         max_rh_30d_ma) {
  
  # Convert shared numeric variables
  numeric_vars <- c("max_air_temp_30d_ma", "max_windspeed_30d_ma", "max_rh_30d_ma")
  convert_to_numeric <- function(env, vars) {
    for (var in vars) {
      assign(var, as.numeric(get(var, envir = env)), envir = env)
    }
  }
  
  convert_to_numeric(environment(), numeric_vars)
  
  # Initialize result
  result <- calculate_sporecaster_risk(max_air_temp_30d_ma, 
                                       max_windspeed_30d_ma,
                                       max_rh_30d_ma)
  if (is.null(result)) {
    return(list(error = "An error occurred while calculating Sporecaster Risk."))
  }else{
    return(list(sporec_irr_30in_risk=result$sporec_irr_30in_risk*100,
                sporec_irr_15in_risk=result$sporec_irr_15in_risk*100,
                sporec_no_irr_risk=result$sporec_no_irr_risk*100,
                success=200,
                disease_name='sporecaster'))
  }
}

#* Predict FrogEye Leaf Spot Risk
#* @param max_air_temp_30d_ma Numeric: 21-day moving average of maximum air temperature (°C)
#* @param relative_humidity_80tot_30d_ma Numeric: 30-day moving average of is the daily total hours where relative humidity (%) was 80% or above. 
#* @get /predict_frogeye_leaf_spot_risk
function(max_air_temp_30d_ma, 
         relative_humidity_80tot_30d_ma) {
  
  numeric_vars <- c("max_air_temp_30d_ma", "relative_humidity_80tot_30d_ma")
  
  convert_to_numeric(environment(), numeric_vars)
  
  if (is.null(max_air_temp_30d_ma) || is.null(relative_humidity_80tot_30d_ma)) {
    return(list(valid = FALSE, 
                message = "We can not compute the FrogEye Risk.", 
                reason = "Fungicide applied in last 14d or growth stage not in the valid ranges."))
  } else {
    result <- calculate_frogeye_leaf_spot_function(max_air_temp_30d_ma, 
                                                   relative_humidity_80tot_30d_ma)
    
    if (is.null(result)) {
      return(list(error = "An error occurred while calculating Frogeye Risk."))
    }else{
      return(list(frogeye_ls=result$fe_risk*100,
                  frogeye_ls_class=result$fe_risk_class,
                  success=200,
                  disease_name='frogeye_leaf_spot'))
    }
  }
}