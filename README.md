[![R-CMD-check](https://github.com/UW-Madison-DSI/corn_disease_forecast_api/testthat/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/UW-Madison-DSI/corn_disease_forecast_api/testthat/actions/workflows/r-cmd-check.yml)


# **Forecasting Model Tools: API and Dashboard**

This open-source plant disease forecasting API and Dashboard enables proactive and data-driven decision-making in agriculture. By leveraging weather data, disease models, and historical disease spread patterns, the tool empowers farmers, agronomists, and plant pathologists to monitor and predict disease outbreaks with accuracy and timeliness. The insights provided help in taking preventive actions, potentially reducing the impact of diseases on crops.

## Shiny APP

Visit our [dashboard here](https://connect.doit.wisc.edu/tarspot_forecasting_app/)

## API


Method: `GET` 
Response: A JSON object containing the predicted probability and underlying variables.

### API to our models in top of Wisconet and IBM:
- Our API provides the prediction of a number of crop diseases based on historical data sourced from 1) Wisconet and 2) IBM-source. Our API is defided to retrieve the historical data and provide the forecasting risk estimates on such locations.
[LINK](https://github.com/UW-Madison-DSI/pywisconet)
[Endpoints](https://connect.doit.wisc.edu/pywisconet_wrapper/docs)



### Usage
See example of how to do an api call on this tool in the materials section of the repository [HERE](https://github.com/UW-Madison-DSI/corn_disease_forecast_api/blob/main/materials/example/example_api_call.R)


## For Developers

### To install
```bash
git clone https://github.com/UW-Madison-DSI/corn_disease_forecast_api.git
cd corn_disease_forecast_api
```

### Project Structure 
```
├── LICENSE
├── README.md                       <- The top-level README for developers using this project.
├── plumber.R                       <- Logic to create API
├── R                               <- Main functionalities for API
│   ├── crop_mangm_validations.R      <- Crop management validations
│   ├── logit_functions.R              <- Core functions for forecasting disease api
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

## Plant disease models

Selected field crops and vegetable disease model outputs are provided. These models are subject to change. The calculations used to generate each model prediction can be viewed in the source code.

- White mold (aka Sporecaster) - dry, irrigated 15-inch row spacing, irrigated 30-inch row spacing - probability of apothecial presence. More information: https://cropprotectionnetwork.org/news/smartphone-application-to-forecast-white-mold-in-soybean-now-available-to-growers
- Frogeye Leaf Spot of soybean - probability of presence. More information: https://cropprotectionnetwork.org/encyclopedia/frogeye-leaf-spot-of-soybean
- Gray Leaf Spot of corn - probability of presence. More information: https://cropprotectionnetwork.org/encyclopedia/gray-leaf-spot-of-corn
- Tar Spot of corn (aka Tarspotter) - probability of presence. More information: https://cropprotectionnetwork.org/encyclopedia/tar-spot-of-corn

## License

This project is licensed under the MIT License - see the LICENSE file for details.


### Acknowledgements
- This work is an Open-Source initiative from the [Open Source Program Office at the University of Madison Wisconsin](https://ospo.wisc.edu), aimed at fostering collaboration and innovation in open source forecasting tools.
- The models presented are based on plant pathology research in the University of Madison Wisconsin, paper: [Nature Scientific Reports, 2023](https://www.nature.com/articles/s41598-023-44338-6)
- This software was created by the [Data Science Institute](https://datascience.wisc.edu) at the [University of Wisconsin-Madison](https://www.wisc.edu)

Mantainer: Maria Oros, maria.oros@wisc.edu
