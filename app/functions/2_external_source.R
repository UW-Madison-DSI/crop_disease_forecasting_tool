library(httr)

url_ibm <- Sys.getenv("URL_IBM")

ibm_query<-function(end_date, lat, lon){
  url <- paste0(url_ibm, end_date) 
  headers <- add_headers(accept = "application/json")
  params <- list(latitude = lat, 
                 longitude = lon, 
                 token = Sys.getenv("API_KEY")
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