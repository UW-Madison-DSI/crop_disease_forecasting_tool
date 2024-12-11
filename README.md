[![R-CMD-check](https://github.com/UW-Madison-DSI/corn_disease_forecast_api/testthat/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/UW-Madison-DSI/corn_disease_forecast_api/testthat/actions/workflows/r-cmd-check.yml)



# **Forecasting Model Tools: API and Dashboard**

This open source plant disease forecasting API and Dashboard opens new opportunities for proactive and data-driven decision-making in agriculture. Such tools enable farmers, agronomists, and plant pathologists to monitor and predict disease outbreaks with greater accuracy and timeliness. By leveraging weather data, disease models, and historical disease spread patterns, our tool provides critical insights that empower users to take preventive actions, potentially reducing the impact of diseases on crops.

## App Features

- Predicts the probability of Tar Spot disease incidence based on key environmental variables from Wisconet.
- Includes customizable action thresholds that allow users to define risk levels and dates of interest.
- Returns the probability and risk classification for the given thresholds and a 7 days range on the settled date given by the user.
- Shows trends of the environmental variables that play a role in the forecasting model.

Visit our [dashboard here](https://connect.doit.wisc.edu/tarspot_forecasting_app/)

## API Endpoints

Our API includes the risk models: Tarspot, Gray Leaf Spot, FrogEye Leaf Spot and Sporecaster. 

**Base URL**: [Forecast Crop Disease API](https://connect.doit.wisc.edu/forecasting_crop_disease/)

### Endpoints:

- `/predict_tarspot_risk`  
  Predicts the probability and risk level for **Tarspot**.

- `/predict_gray_leaf_spot_risk`  
  Predicts the probability and risk level for **Gray Leaf Spot**.

- `/predict_sporecaster_risk`  
  Predicts the probability and risk level for **Spore**.

- `/predict_frogeye_leaf_spot_risk`  
  Predicts the probability and risk level for **Frogeye**.

- `/predict_wisconet_stations_risk`  
  Predicts the probability of the previous models for **all the active stations, or the specified one**, given a forecast date and disease name as inputs.
  
Method: `POST` 

Response: A JSON object containing the predicted probability and undelying variables.

## To install
```bash
git clone https://github.com/UW-Madison-DSI/corn_disease_forecast_api.git
cd corn_disease_forecast_api
```

## Usage
See example of how to do an api call on this tool [HERE](https://github.com/UW-Madison-DSI/corn_disease_forecast_api/blob/main/materials/example/example_api_call.R)

## Project Structure 
```
├── LICENSE
├── README.md                       <- The top-level README for developers using this project.
├── plumber.R                       <- Logic to create API
├── R                               <- Main functionalities for API
│   ├── crop_mangm_validations.R      <- Crop management validations
│   ├── logit_function.R              <- Core functions for forecasting disease api
│   ├── all_stations_api_functions.R  <- Construction of the stations risk estimates
├── materials                     <- Examples of API calls and plug in with Wisconet
│   ├── call_wisconet                 <- Wisconet call example
│   ├── docs                          <- Documentation of the schema
│   ├── example                       <- Examples to call the API
├── app                           <- Shinny dashboard code.
│   ├── app.R                         <- app logic, ui and server
│   ├── server.R                      <- app logic, ui and server
│   ├── ui.R                          <- app logic, ui and server
│   ├── report_template.Rmd         <- markdown for pdf report
│   ├── www                         <- web styles and pages
│   ├── logos                       <- logos
│   ├── functions                   <- APP functions
│   │   ├── api_calls_logic.R         <- Logic to call the Wisconet Station weather data and call the forecasting models on the preparation of the inputs
│   │   ├── auxiliar_functions.R      <- List of stations currently active from Wisconet  
│   │   ├── instructions.R            <- List of stations currently active from Wisconet  
│   │   ├── punctual_estimates.R      <- TBD 
├── test                            <- Code Testing modules
│   ├── testthat.R    
 ```

## License

This project is licensed under the MIT License - see the LICENSE file for details.


### Acknowledgements
- This work is an Open-Source initiative from the [Open Source Program Office at the University of Madison Wisconsin](https://ospo.wisc.edu), aimed at fostering collaboration and innovation in open source forecasting tools.
- The models presented are based on plant pathology research in the University of Madison Wisconsin, paper: [Nature Scientific Reports, 2023](https://www.nature.com/articles/s41598-023-44338-6)
- This software was created by the [Data Science Institute](https://datascience.wisc.edu) at the [University of Wisconsin-Madison](https://www.wisc.edu)

Mantainer: Maria Oros, maria.oros@wisc.edu
