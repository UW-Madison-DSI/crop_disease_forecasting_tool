# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)
library(zoo)
library(lubridate)
library(tidyr)
library(ggplot2)

#############################################################################################  
# function to standarize the risk models into a pivot tabular object and then plot the risk functions
seven_days_trend_plot <- function(data_prepared, location, selected_diseases){
  # Select and rename the relevant columns
  data_selected <- data_prepared %>%
    filter(!is.na(tarspot_risk)) %>%
    select(forecasting_date, tarspot_risk, gls_risk, fe_risk,
           whitemold_irr_30in_risk,
           whitemold_irr_15in_risk,
           whitemold_nirr_risk) %>%
    rename(
      `Tar Spot` = tarspot_risk,
      `Gray Leaf Spot` = gls_risk,
      `Frog Eye Leaf Spot` = fe_risk,
      `Whitemold Irr (30in)` = whitemold_irr_30in_risk,
      `Whitemold Irr (15in)` = whitemold_irr_15in_risk,
      `Whitemold No Irr` = whitemold_nirr_risk
    )
  
  # Reshape the data into long format
  data_long <- data_selected %>%
    pivot_longer(cols = c("Tar Spot", "Gray Leaf Spot", "Frog Eye Leaf Spot",
                          "Whitemold Irr (30in)","Whitemold Irr (15in)","Whitemold No Irr"), 
                 names_to = "Disease Model", 
                 values_to = "risk_value") #%>% filter(`Disease Model` %in% disease_choices)
  data_long$risk_value <- data_long$risk_value*100
  df_subset <- data_long %>% filter(`Disease Model` %in% c(selected_diseases))

  # Plot the trend of the specified risk variables over time
  ggplot(df_subset, aes(x = forecasting_date, y = risk_value, color = `Disease Model`)) +
    geom_line() +
    geom_point() +
    labs(title = paste0("Risk Trend in the last week (",location,')'),
         x = "Forecasting Date",
         y = "Risk (%)") +
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    scale_y_continuous(limits = c(0, min(100, max(df_subset$risk_value)))) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
}

######################################################################################## deprecated
# This is the endpoint to the pywisconet, a wrapper of Wisconet data https://github.com/UW-Madison-DSI/pywisconet.git

color_mapping <- c(
  "Max Temp (°C)" = "#E69F00",  # Orange
  "Min Temp (°C)" = "#56B4E9",  # Sky Blue
  "Avg Temp (°C)" = "#009E73",  # Green
  'count_90_8PM_6AM'="#E69F00",
  'max_rh_8PM_6AM' = "#E69F00",
  'max_rh' = "#56B4E9",
  'max_rh_30d_ma' = "#E69F00",
  "Max Temp (°C) 30D MA" = "#E69F00",  # Orange
  "Min Temp (°C) 30D MA" = "#56B4E9",  # Sky Blue
  "Avg Temp (°C) 30D MA" = "#009E73",   # Green
  'count_90_8PM_6AM_14d_ma'= "#E69F00" 
)

# Define linetypes
linetype_mapping <- c(
  "Max Temp (°C)" = "solid",
  "Min Temp (°C)" = "solid",
  "Avg Temp (°C)" = "solid",
  'count_90_8PM_6AM' = "solid",
  'max_rh_8PM_6AM' = "solid",
  'max_rh' = "solid",
  'max_rh_30d_ma' = "dotted",
  'count_90_8PM_6AM_14d_ma' = "dotted", 
  "Max Temp (°C) 30D MA" = "dotted",
  "Min Temp (°C) 30D MA" = "dotted",
  "Avg Temp (°C) 30D MA" = "dotted",
  'count_90_8PM_6AM_14d_ma'= "dotted"
)

plot_air_temp <- function(data) {
  # Pivot air temperature variables to long format
  air_temp_data <- data %>%
    select(
      collection_time_ct, 
      air_temp_max_c, air_temp_min_c, air_temp_avg_c,
      air_temp_max_value_30d_ma, air_temp_min_value_30d_ma, air_temp_avg_value_30d_ma
    ) %>%
    mutate(
      Date = as.Date(collection_time_ct, format="%Y-%m-%d")
    ) %>%
    rename(
      "Max Temp (°C)" = air_temp_max_c,
      "Min Temp (°C)" = air_temp_min_c,
      "Avg Temp (°C)" = air_temp_avg_c,
      "Max Temp (°C) 30D MA" = air_temp_max_value_30d_ma,
      "Min Temp (°C) 30D MA" = air_temp_min_value_30d_ma,
      "Avg Temp (°C) 30D MA" = air_temp_avg_value_30d_ma
    ) %>%  # Ensure proper conversion to Date
    select(-collection_time_ct) %>%
    pivot_longer(
      cols = c(
        "Max Temp (°C)", "Min Temp (°C)", "Avg Temp (°C)",
        "Max Temp (°C) 30D MA", "Min Temp (°C) 30D MA", "Avg Temp (°C) 30D MA"
      ),
      names_to = "Variable",
      values_to = "Value"
    )
  
  # Create the ggplot
  ggplot(air_temp_data, aes(x = Date, y = Value, color = Variable, linetype = Variable)) +
    geom_line(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
    labs(
      title = "Air Temperature (°C) Trends in the Last 30 Days",
      x = "Date",
      y = "Air Temperature (°C)",
      color = "Variable",
      linetype = "Variable"
    ) +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = linetype_mapping) +
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
}


plot_rh_dp <- function(data) {
  # Select and prepare data for plotting
  rh_dp_data <- data %>%
    select(collection_time_ct, max_rh_8PM_6AM, max_rh, max_rh_30d_ma) %>%
    mutate(
      Date = as.Date(collection_time_ct, format="%Y-%m-%d")  # Convert to Date
    ) %>%
    select(-collection_time_ct) %>%  # Drop original column
    pivot_longer(
      cols = c(max_rh_8PM_6AM, max_rh_30d_ma, max_rh),
      names_to = "Variable",
      values_to = "Value"
    )
  
  # Create the ggplot
  ggplot(rh_dp_data, aes(x = Date, y = Value, color = Variable)) +
    geom_line(size = 1.5) +
    labs(
      title = "Maximum Relative Humidity (%)",
      x = "Date",
      y = "Relative Humidity (%)",
      color = "Variable"
    ) +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = linetype_mapping) +
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)  
    )
}

plot_rh_nh_dp <- function(data) {
  # Select and prepare data for plotting
  rh_dp_data <- data %>%
    select(collection_time_ct, count_90_8PM_6AM_14d_ma, count_90_8PM_6AM) %>%
    mutate(
      Date = as.Date(collection_time_ct, format="%Y-%m-%d")  # Convert to Date
    ) %>%
    select(-collection_time_ct) %>%  # Drop original column
    pivot_longer(
      cols = c(count_90_8PM_6AM_14d_ma, count_90_8PM_6AM),
      names_to = "Variable",
      values_to = "Value"
    )
  
  # Create the ggplot
  ggplot(rh_dp_data, aes(x = Date, y = Value, color = Variable)) +
    geom_line(size = 1.5) +
    labs(
      title = "Total Night hours the Relative Humidity (%) was above 90%",
      x = "Date",
      y = "Total Night hours",
      color = "Variable"
    ) +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = linetype_mapping) +
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
}

