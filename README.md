# Yield_forecast_Sunflowers

## Background ## 

Adverse impacts to global food production due to climate change necessitate accurate crop yield forecasting. Sunflower is an important oilseed crop in the US and is sensitive to shifts in climate. Thus accurate foreacating and understanidng of the factors that lead to changes in yield is necessary for future planning and management strategies. In this work I use, explainable AI (XAI) to identify climate variables influencing yield at national and regional scales. I also use ML to forecast sunflower yield per acre in the US for future periods under different emissions scenarios. The approach helps identify key climate variables, their thresholds, and their effects on yield, aiding in understanding and adaptation strategies.

## Software and programming language used

Data was compiled, cleaned, and processed using a combination of the R programming language (R Core Team 2023) and ArcGIS Pro software (version 2.9). All data cleaning, visualizations and modeling were performed using various packages created in the R programming environment. 

## Study area, data sources and data preperation

Yield and weather data acquired programmatically from USDA and NOAA databases for sunflower seed analysis. Historical data spans 1976-2022, covering 189 counties in 7 states. Future projections obtained from WorldClim repository (CMIP6 models) for 2021-2080, with four Shared Socioeconomic Pathways (SSPs). All data reprojected and extracted at county centroids. Tidyverse used for data preparation. Missing data removed, resulting in 177 counties. Training/testing datasets split at 2004. State-level datasets created for each of the 7 states. Training datasets contain 70-80% of data. 

## Workflow flochart 

![Figure 1](https://github.com/SamMajumder/Yield_forecast_Sunflowers/assets/83839244/b9f2eeb2-83ce-4a68-917d-76d4a00529dc)


Workflow for training and validating ML models used in this project. Applied to national and state-level datasets with historical gridded monthly precipitation, temperature, area planted/harvested, and yield data. Datasets divided into training/testing at 2005. Recursive Feature Elimination (RFE) used to select optimal predictors. Training datasets used to train RF, GBM, and XGB regression models. Predictive performance evaluated using normalized RMSE on testing datasets. Best model chosen to compute ALE and generate yield forecasts for future periods under four socioeconomic pathways (SSP 1-2.6, SSP 2-4.5, SSP 3-7.5, SSP 5-8.5). 

## Shiny APP ## 

The resulting Shiny app highlighting the main results from this work can be found here: https://sammajumder.shinyapps.io/SunScope/ 

The GitHub repository containing the Shiny App code can be found here: https://github.com/SamMajumder/Yield_forecast_Sunflowers_Shiny 

## Main results ## 


![Figure 2](https://github.com/SamMajumder/Yield_forecast_Sunflowers/assets/83839244/03e73fbd-6268-4680-8625-fab48abe2767) 

Recursive feature elimination with a random forest algorithm to identify the most important predictors for yield at the national level. The predictors were ranked based on their relative importance in predicting yield using the mean decrease of root mean square error metric. 

![Figure 3](https://github.com/SamMajumder/Yield_forecast_Sunflowers/assets/83839244/144cd39c-a7ec-4750-84b1-9daa2dd6ebe0) 

The impact of four important predictors (total precipitation in July, maximum July temperature, minimum May temperature, and minimum September temperature) on yield. Accumulated Local Effects (ALE) were calculated for these predictors using both training and test datasets. The ALE plots illustrate how changes in predictor values affect yield (measured in lbs/acre). 

![Figure 4](https://github.com/SamMajumder/Yield_forecast_Sunflowers/assets/83839244/b15e32bd-a688-4009-bcd1-d68cf4eb5971) 

The predictive performance of the best models at the national and state levels was evaluated using Pearson's correlation (r) between predicted and actual values on the testing dataset. The trend of the relationship between predicted and actual values was visualized using a locally estimated scatterplot smoothing line with a 95% confidence interval. The analysis showed that the national model, based on the Random Forest regressor, performed better than state-level models in North Dakota and South Dakota, but state-level models outperformed the national model in the other states. The best state-level models for Colorado, Kansas, and Minnesota also used the Random Forest regressor, while the Gradient Boosting Machine regressor was used for Nebraska and Texas. 

![Figure 5](https://github.com/SamMajumder/Yield_forecast_Sunflowers/assets/83839244/cfe28ecf-ebf5-42a1-9a58-198a97a263ed) 

Average yield forecasts at the national level were analyzed for three time periods and under different Socio-Economic Pathways (SSP). The average annual yield for 2022 was approximately 1940 lbs/acre, with error bars representing the standard deviation around this mean. Future yield forecasts were depicted as point estimates with error bars indicating variation (standard error), and lines illustrated yield changes across four time periods and under three SSPs. The SSPs included SSP 1-26 ("sustainable future"), SSP 2-45 ("middle of the road"), SSP 3-75 ("regional rivalry"), and SSP 5-85 (fossil-fueled development). Each SSP was characterized by specific factors such as global temperature rise, carbon dioxide emissions, economic growth, inequality, and environmental concerns. 

![Figure S15](https://github.com/SamMajumder/Yield_forecast_Sunflowers/assets/83839244/1aa1b3dc-18bf-4b7a-8ebb-23a293772cde) 

Average yield forecasts for each of the seven states were examined for three time periods and under different Socio-Economic Pathways (SSP). The latest average annual yield was represented by point estimates, with error bars indicating the standard deviation around the mean. Future yield forecasts were also depicted as point estimates with error bars reflecting the variation (standard error) around these predictions. The lines portrayed yield changes across the time periods and under four SSPs: SSP 1-26 ("sustainable future"), SSP 2-45 ("middle of the road"), SSP 3-75 ("regional rivalry"), and SSP 5-85 (fossil-fueled development). Each SSP was characterized by specific factors such as global temperature rise, carbon dioxide emissions, economic growth, inequality, and environmental concerns.












