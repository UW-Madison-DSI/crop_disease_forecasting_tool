# Install required packages if not already installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  leaflet,
  httr2,
  zoo,
  lubridate,
  janitor,
  lutz
)

# ---- Utility Functions ----
# Rolling mean for moving averages
roll_mean <- function(vec, width) {
  zoo::rollapply(vec, width, \(x) mean(x, na.rm = TRUE), fill = NA, partial = TRUE)
}

# Logistic function to convert logit to probability
logistic <- function(logit) {
  exp(logit) / (1 + exp(logit))
}

# Break date range into chunks for IBM API
ibm_chunks <- function(start_date, end_date) {
  start_date <- as_datetime(start_date)
  end_date <- as_datetime(end_date) + hours(23)
  chunks <- seq(start_date, end_date, by = as.difftime(hours(999)))
  chunks <- lapply(seq_along(chunks), function(i) {
    if (i < length(chunks)) {
      c(chunks[i], chunks[i + 1])
    } else {
      c(chunks[i], end_date)
    }
  })
  lapply(chunks, function(chunk) {
    format(chunk, "%Y-%m-%dT%H:%M:%S%z")
  })
}

# ---- IBM Weather Data Retrieval ----
# API function to fetch weather data in chunks
get_ibm <- function(lat, lng, start_date = Sys.Date() - 30, end_date = Sys.Date(), api_key) {
  chunks <- ibm_chunks(start_date, end_date)
  reqs <- lapply(chunks, function(dates) {
    request("https://api.weather.com/v3/wx/hod/r1/direct") %>%
      req_url_query(
        format = "json",
        geocode = paste(lat, lng, sep = ","),
        startDateTime = dates[1],
        endDateTime = dates[2],
        units = "m",
        apiKey = api_key
      )
  })
  resps <- req_perform_parallel(reqs)
  lapply(resps, function(resp) {
    resp %>%
      resp_body_json(simplifyVector = TRUE) %>%
      as_tibble()
  }) %>% bind_rows()
}

# ---- Data Builders ----
# Build hourly weather data
build_hourly <- function(ibm_response, tz) {
  ibm_response %>%
    select(
      lat = latitude,
      lng = longitude,
      dttm_utc = validTimeUtc,
      precip = precip1Hour,
      rh = relativeHumidity,
      temp = temperature,
      dp = temperatureDewPoint,
      ws = windSpeed
    ) %>%
    mutate(across(dttm_utc, ~parse_date_time(.x, "YmdHMSz"))) %>%
    arrange(dttm_utc) %>%
    distinct(dttm_utc, .keep_all = TRUE) %>%
    mutate(
      dttm = with_tz(dttm_utc, tz),
      tz = tz(dttm),
      date = as_date(dttm),
      hour = hour(dttm),
      night = !between(hour, 7, 19),
      date_since_night = as_date(dttm + hours(4))
    )
}

# Build daily weather summary
build_daily <- function(hourly_wx) {
  hourly_wx %>%
    summarize(
      across(c(temp, dp, rh, ws), list(min = min, mean = mean, max = max)),
      precip_total = sum(precip, na.rm = TRUE),
      hours_rh80 = sum(rh >= 80, na.rm = TRUE),
      hours_missing = 24 - n(),
      .by = date
    ) %>%
    filter(hours_missing < 6) %>%
    left_join(
      hourly_wx %>%
        filter(night) %>%
        summarize(hours_rh90_night = sum(rh >= 90), .by = date_since_night),
      join_by(date == date_since_night)
    )
}

# Add moving averages to daily weather
add_moving_averages <- function(daily_wx) {
  daily_wx %>%
    mutate(
      temp_min_30ma = roll_mean(temp_min, 30),
      temp_max_30ma = roll_mean(temp_max, 30),
      rh_max_30ma = roll_mean(rh_max, 30)
    )
}

# ---- Main Workflow ----
# Fetch weather data
get_weather <- function(lat, lng, start_date, end_date, api_key) {
  t <- Sys.time()
  tz <- lutz::tz_lookup_coords(lat, lng, warn = FALSE)
  message(sprintf("Fetching weather for point %s, %s (%s)", lat, lng, tz))
  
  hourly <- get_ibm(lat, lng, start_date, end_date, api_key) %>% build_hourly(tz)
  daily <- build_daily(hourly) %>% add_moving_averages()
  
  message("Completed in ", round(as.numeric(Sys.time() - t), 3), " seconds")
  list(hourly = hourly, daily = daily)
}
