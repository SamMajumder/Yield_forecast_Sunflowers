

rm(list = ls())

packages <- list("tidyverse","caret","here")

lapply(packages, require,character.only=T)

#####

source(here::here("Code","00_Functions.R"))

#### Error file ##

Error <- read_csv(here::here("Processed_Datasets","Errors.csv"))


##### First evaluating model performance for the national level ####


#### Dividing the dataset based on what model was used in that area ###

## For North and South Dakota we used the National model 

## North Dakota ## Train

Error_ND_train <- Error %>% dplyr::filter(Model == "National") %>% 
                      dplyr::filter(State ==  "ND") %>% 
                      dplyr::filter(Dataset == "Train") 


Error_ND_test <- Error %>% dplyr::filter(Model == "National") %>% 
                            dplyr::filter(State ==  "ND") %>% 
                            dplyr::filter(Dataset == "Test") 
    


Model_perf_ND <- Model_perf(actual_train = Error_ND_train$Yield,
                            actual_test =  Error_ND_test$Yield,
                            predicted_train = Error_ND_train$Predictions,
                            predicted_test = Error_ND_test$Predictions) 




## For North and South Dakota we used the National model 

## South Dakota ## Train 

Error_SD_train <- Error %>% dplyr::filter(Model == "National") %>% 
                            dplyr::filter(State ==  "SD") %>% 
                            dplyr::filter(Dataset == "Train") 

## Test ###

Error_SD_test <- Error %>% dplyr::filter(Model == "National") %>% 
                           dplyr::filter(State ==  "SD") %>% 
                           dplyr::filter(Dataset == "Test") 


### Model_perf SD ###

Model_perf_SD <- Model_perf(actual_train = Error_SD_train$Yield,
                            actual_test =  Error_SD_test$Yield,
                            predicted_train = Error_SD_train$Predictions,
                            predicted_test = Error_SD_test$Predictions) 



## For other states we used their respective state level model 

## Nebraska ## Train 

Error_NE_train <- Error %>% dplyr::filter(Model == "Nebraska") %>% 
  dplyr::filter(State ==  "NE") %>% 
  dplyr::filter(Dataset == "Train") 

## Test ###

Error_NE_test <- Error %>% dplyr::filter(Model == "Nebraska") %>% 
  dplyr::filter(State ==  "NE") %>% 
  dplyr::filter(Dataset == "Test") 


### Model_perf NE ###

Model_perf_NE <- Model_perf(actual_train = Error_NE_train$Yield,
                            actual_test =  Error_NE_test$Yield,
                            predicted_train = Error_NE_train$Predictions,
                            predicted_test = Error_NE_test$Predictions) 



## Kansas ## Train 

Error_KS_train <- Error %>% dplyr::filter(Model == "Kansas") %>% 
  dplyr::filter(State ==  "KS") %>% 
  dplyr::filter(Dataset == "Train") 

## Test ###

Error_KS_test <- Error %>% dplyr::filter(Model == "Kansas") %>% 
  dplyr::filter(State ==  "KS") %>% 
  dplyr::filter(Dataset == "Test") 


### Model_perf KS ###

Model_perf_KS <- Model_perf(actual_train = Error_KS_train$Yield,
                            actual_test =  Error_KS_test$Yield,
                            predicted_train = Error_KS_train$Predictions,
                            predicted_test = Error_KS_test$Predictions) 




## Minnesota ## Train 

Error_MN_train <- Error %>% dplyr::filter(Model == "Minnesota") %>% 
  dplyr::filter(State ==  "MN") %>% 
  dplyr::filter(Dataset == "Train") 

## Test ###

Error_MN_test <- Error %>% dplyr::filter(Model == "Minnesota") %>% 
  dplyr::filter(State ==  "MN") %>% 
  dplyr::filter(Dataset == "Test") 


### Model_perf MN ###

Model_perf_MN <- Model_perf(actual_train = Error_MN_train$Yield,
                            actual_test =  Error_MN_test$Yield,
                            predicted_train = Error_MN_train$Predictions,
                            predicted_test = Error_MN_test$Predictions) 


### Model_perf ### TX ### 

Error_TX_train <- Error %>% dplyr::filter(Model == "Texas") %>% 
  dplyr::filter(State ==  "TX") %>% 
  dplyr::filter(Dataset == "Train") 

## Test ###

Error_TX_test <- Error %>% dplyr::filter(Model == "Texas") %>% 
  dplyr::filter(State ==  "TX") %>% 
  dplyr::filter(Dataset == "Test") 


### Model_perf MN ###

Model_perf_TX <- Model_perf(actual_train = Error_TX_train$Yield,
                            actual_test =  Error_TX_test$Yield,
                            predicted_train = Error_TX_train$Predictions,
                            predicted_test = Error_TX_test$Predictions) 


### Model_perf ### CO ### 

Error_CO_train <- Error %>% dplyr::filter(Model == "Colorado") %>% 
  dplyr::filter(State ==  "CO") %>% 
  dplyr::filter(Dataset == "Train") 

## Test ###

Error_CO_test <- Error %>% dplyr::filter(Model == "Colorado") %>% 
  dplyr::filter(State ==  "CO") %>% 
  dplyr::filter(Dataset == "Test") 


### Model_perf CO ###

Model_perf_CO <- Model_perf(actual_train = Error_CO_train$Yield,
                            actual_test =  Error_CO_test$Yield,
                            predicted_train = Error_CO_train$Predictions,
                            predicted_test = Error_CO_test$Predictions) 


#########
###### Now computing performance for the national level model ###
#### 


#### Error file ##

Error_national <- read_csv(here::here("Processed_Datasets","Errors_national.csv"))


Error_train <- Error_national %>% dplyr::filter(Dataset == "Train") 


Error_test <- Error %>% dplyr::filter(Dataset == "Test") 



Model_perf_national <- Model_perf(actual_train = Error_train$Yield,
                            actual_test =  Error_test$Yield,
                            predicted_train = Error_train$Predictions,
                            predicted_test = Error_test$Predictions) 














