#From Wisconet documentation, in particular in the endpoint /measures/,
#the station data is recorded using a Unix epoch in GMT, representing the start time (and end time) of the query
#in that matter, as each station belongs to the tzone America/Chicago
#we need to use a transformation in particular for the RH hours.

# Convert the string to a POSIXct object with GMT time zone
library(lubridate)

# Parse the date-time string and set the time zone to GMT
gmt_time <- ymd_hms("2024-10-16 16:51:36", tz = "GMT") #time of the query to Wisconet
ct_time <- with_tz(gmt_time, tzone = "America/Chicago")

utc_time <- ymd_hms("2024-10-16 16:51:36", tz = "UTC")
ct_time_from_utc <- with_tz(utc_time, tzone = "America/Chicago")

# Print the result
cat('GMT time',gmt_time, ' to CT' , ct_time)

# Print the GMT and Central Time in a readable format
cat('GMT time:', format(gmt_time, "%Y-%m-%d %H:%M:%S %Z"), '\n')
cat('CT time:', format(ct_time, "%Y-%m-%d %H:%M:%S %Z"), '\n')
cat('CT time:', format(ct_time, "%Y-%m-%d %H:%M:%S %Z"), '\n')
cat('CT time form UTC:', format(ct_time_from_utc, "%Y-%m-%d %H:%M:%S %Z"), '\n')
