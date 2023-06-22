# Yield_forecast_Sunflowers

## Software and programming language 

Data was compiled, cleaned, and processed using a combination of the R programming language (R Core Team 2023) and ArcGIS Pro software (version 2.9). All data cleaning, visualizations and modeling were performed using various packages created in the R programming environment. 

## Study area, data sources and data preperation

Yield and weather data acquired programmatically from USDA and NOAA databases for sunflower seed analysis. Historical data spans 1976-2022, covering 189 counties in 7 states. Future projections obtained from WorldClim repository (CMIP6 models) for 2021-2080, with four Shared Socioeconomic Pathways (SSPs). All data reprojected and extracted at county centroids. Tidyverse used for data preparation. Missing data removed, resulting in 177 counties. Training/testing datasets split at 2004. State-level datasets created for each of the 7 states. Training datasets contain 70-80% of data. 

## Workflow flochart 

![Figure 1](https://github.com/SamMajumder/Yield_forecast_Sunflowers/assets/83839244/4187735e-cd3b-4058-a9bb-ada02f13b4a2)



Workflow for training and validating ML models used in Chapter 4. Applied to national and state-level datasets with historical gridded monthly precipitation, temperature, area planted/harvested, and yield data. Datasets divided into training/testing at 2005. Recursive Feature Elimination (RFE) used to select optimal predictors. Training datasets used to train RF, GBM, and XGB regression models. Predictive performance evaluated using normalized RMSE on testing datasets. Best model chosen to compute ALE and generate yield forecasts for future periods under four socioeconomic pathways (SSP 1-2.6, SSP 2-4.5, SSP 3-7.5, SSP 5-8.5).
