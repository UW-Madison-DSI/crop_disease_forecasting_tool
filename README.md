# **Forecasting Model Tools: API and Dashboard**

This API leverages the power of environmental data to predict the likelihood of diseases like Tarspot, Spore and Gray Leaf Spot on corn crops. 

## To install
```bash
git clone https://github.com/UW-Madison-DSI/corn_disease_forecast_api.git
cd corn_disease_forecast_api
```

## Usage
See example of how to do an api call on this tool [HERE](https://github.com/UW-Madison-DSI/corn_disease_forecast_api/blob/main/materials/example/example_api_call.R)


## Features

- Predicts the probability of disease incidence based on key environmental variables.
- Customizable action thresholds that allow users to define risk levels for interventions.
- Returns the probability and risk classification for the given thresholds.

## Dashboard 

[Visualization](https://connect.doit.wisc.edu/tarspot_forecasting_app/)

## API Endpoints

**Base URL**: [Forecast Crop Disease API](https://connect.doit.wisc.edu/forecasting_crop_disease/)

### Endpoints:

- `/predict_tarspot_risk`  
  Predicts the probability and risk level for **Tarspot**.

- `/predict_gray_leaf_spot_risk`  
  Predicts the probability and risk level for **Gray Leaf Spot**.

- `/predict_sporecaster_risk`  
  Predicts the probability and risk level for **Spore**.


Method: `POST` 

Response: A JSON object containing the predicted probability of tar spot incidence and the associated risk level.

## Reference:
- Original paper: [Nature Scientific Reports, 2023](https://www.nature.com/articles/s41598-023-44338-6)
- See the codebook of the variables used  [here](https://github.com/UW-Madison-DSI/corn_disease_forecast_api/blob/main/materials/docs/codebook - API-variablesuse.R)

## License

This project is licensed under the MIT License - see the LICENSE file for details.


### Acknowledgements

This software was created by the [Data Science Institute](https://datascience.wisc.edu) at the [University of Wisconsin-Madison](https://www.wisc.edu)