# **Forecasting Model API**

This API leverages the power of environmental data to predict the likelihood of diseases like Tarspot, Spore and Gray Leaf Spot on corn crops. 


### Reference:
- Original paper: [Nature Scientific Reports, 2023](https://www.nature.com/articles/s41598-023-44338-6)


## Features

- Predicts the probability of disease incidence based on key environmental variables.
- Customizable action thresholds that allow users to define risk levels for interventions.
- Returns the probability and risk classification for the given thresholds.

## Project Structure
```plaintext
├── LICENSE.md          <- The top-level LICENSE for developers using this project.
├── README.md          <- The top-level README for developers using this project.
├── plumber.R          <- API endpoints definitions.
├── R                  <- Core API logic functions.
├── cleaning          <- Cleaning phase, the outputs are the ones in ready_for_analysis
├── test             <- Unit test
├── .github/workflows             <- CI-CD logic for deployment.
```

## API Endpoints
API [ULR](https://connect.doit.wisc.edu/forecasting_corn_disease/)

/predict_tarspot_risk

/predict_gray_leaf_spot

/predict_sporecaster_risk


Method: POST


Response: A JSON object containing the predicted probability of tar spot incidence and the associated risk level.

See example of the api call (here)[https://github.com/UW-Madison-DSI/corn_disease_forecast_api/blob/main/example/example_api_call.R]


### License

This project is licensed under the MIT License - see the LICENSE file for details.
