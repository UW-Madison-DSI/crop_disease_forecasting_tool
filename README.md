# **Tarspot Prediction Model API**

This API replicates and provides access to the forecasting model developed to predict the incidence of tar spot, a fungal disease primarily affecting maize crops. The model utilizes environmental data (temperature, humidity, etc.) to generate risk predictions using logistic regression, closely following the approach outlined in the 2023 Nature Scientific Reports paper.

### Reference:
- Original paper: [Nature Scientific Reports, 2023](https://www.nature.com/articles/s41598-023-44338-6)


## Features

- Predicts the probability of tar spot incidence based on key environmental variables.
- Customizable action thresholds that allow users to define risk levels for interventions.
- Returns an ensembled probability and a color-coded risk level:
  - WHITE: No risk (0% probability).
  - BLUE: Low risk (1-19% probability).
  - YELLOW: Medium risk (20-34% probability).
  - RED: High risk (≥35% probability).

API stops execution if fungicide application has been detected in the last 14 days or if the crop growth stage is outside the valid range.


## API Endpoints

/forecasting_corn_disease/
Method: POST
Description: Predicts tar spot risk based on environmental variables and user-specified thresholds.
Parameters:
  - meanAT: 30-day moving average of mean air temperature (°C).
  - maxRH: 30-day moving average of maximum relative humidity (%).
  - rh90_night_tot: 14-day moving average of total nighttime hours with 90% relative humidity or higher (hours).
  - threshold: Custom action threshold for risk classification (default: 35%, range: 20-50%).
  - growth_stage: The current growth stage of the crop (V10, V11, V12, V13, R1, R2, R3).
  - fungicide_applied: Indicate whether fungicide was applied in the last 14 days (yes or no).

Response: A JSON object containing the predicted probability of tar spot incidence and the associated risk level.

```
EXAMPLE
```


### License

This project is licensed under the MIT License - see the LICENSE file for details.
