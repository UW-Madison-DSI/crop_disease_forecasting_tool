library(plumber)



source("R/crop_mangm_validations.R")
source("R/all_stations_api_functions.R")

#* @apiTitle Crop Disease Risk Prediction API
#* @apiDescription This API predicts the risk of crop diseases (Spore, Tarspoter, Gray Leaf Spot and Frog Eye Leaf Spot) based on environmental data and user inputs.


#* Predict Tarspot Risk
#* @param mean_air_temp_30d_ma Numeric: 30-day moving average of mean air temperature (°C)
#* @param max_rh_30d_ma Numeric: 30-day moving average of max relative humidity (%)
#* @param tot_nhrs_rh90_14d_ma Numeric: 14-day moving average of total nighttime hours with 90% relative humidity (%) or above for each day.
#* @post /predict_tarspot_risk
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
#* @post /predict_gray_leaf_spot_risk
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
#* @post /predict_sporecaster_risk
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
#* @post /predict_frogeye_leaf_spot_risk
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


#* Predict the previous disease Risk on the active Wisconet Stations
#* @param forecasting_date Character: Forecasting date, format YYYY-MM-dd
#* @param station_id Character: Station id eg ALTN
#* @param disease_name Character: tarspot, gls, sporecaster-irr, frogeye_leaf_spot // sporecaster-noirr
#* @post /predict_wisconet_stations_risk
function(forecasting_date, 
         station_id = NULL,
         disease_name = NULL) {
  
  # Default value for disease_name
  if (is.null(disease_name)) {
    disease_name <- 'tarspot'
  }
  
  # Validate and handle input_date
  input_date <- if (is.null(forecasting_date)) Sys.Date() else as.Date(forecasting_date)
  
  # Check for invalid dates
  if ((format(input_date, "%Y-%m") == "2024-04") || 
      (input_date < as.Date("2022-01-01")) || 
      (input_date > Sys.Date())) {
    return(list(
      error = "Data is unavailable for April 2024 or dates prior to 2022.",
      status = 400,
      disease_name = disease_name,
      forecasting_date = input_date
    ))
  }
  
  # Try retrieving risk and handle errors
  tryCatch({
    risk <- retrieve_tarspot_all_stations(input_date, station_id, disease_name)
    return(list(
      status = risk$status,
      disease_name = disease_name,
      forecasting_date = input_date,
      n_stations = risk$n_stations,
      stations_risk = risk$stations_risk
    ))
  }, error = function(e) {
    return(list(
      status = 400,
      disease_name = disease_name,
      forecasting_date = input_date,
      n_stations = 0,
      stations_risk = NULL,
      error = conditionMessage(e)  # Include error message for debugging
    ))
  })
}