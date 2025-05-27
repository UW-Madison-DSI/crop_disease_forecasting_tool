library(httr)

ibm_query<-function(end_date, lat, lon){
  url <- 'https://connect.doit.wisc.edu/pywisconet_wrapper/ag_models_wrappers/ibm'
  headers <- add_headers(accept = "application/json")
  params <- list(latitude = lat, 
                 longitude = lon, 
                 forecasting_date = end_date,
                 API_KEY = Sys.getenv("API_KEY"),
                 TENANT_ID = Sys.getenv("TENANT_ID"),
                 ORG_ID = Sys.getenv('ORG_ID')
  )
  
  # Make the GET request
  response <- GET(url, headers, query = params)
  
  if (status_code(response) == 200) {
    data = fromJSON(rawToChar(response$content))
    return(data)
  } else {
    print(paste("Error:", status_code(response), content(response, as = "text")))
    return(NULL)
  }
}

