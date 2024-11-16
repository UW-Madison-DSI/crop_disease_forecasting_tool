
library(tidyverse)
library(leaflet)


# Pull weather data from source ----

source_chunks <- function(start_date, end_date) {
  start_date <- as_datetime(start_date)
  end_date <- as_datetime(end_date) + hours(23)
  chunks <- seq(start_date, end_date, by = as.difftime(hours(999)))
  chunks <- lapply(1:length(chunks), function(i) {
    if (i < length(chunks))
      c(chunks[i], chunks[i+1])
    else
      c(chunks[i], end_date)
  })
  lapply(chunks, function(chunk) {
    format(chunk, "%Y-%m-%dT%H:%M:%S%z")
  })
}