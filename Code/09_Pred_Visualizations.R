rm(list = ls())

packages <- c("tidyverse","here","sf")


lapply(packages, require,character.only = T)


source(here::here("Code","00_Functions.R"))


############
#### COMBINED ###
############### 

train <- read.csv(here::here("Processed_Datasets","train.csv"))

test <- read.csv(here::here("Processed_Datasets","test.csv"))

##########
## Importing the most important variables ####
########## 

Imp_variables <- read.csv(here::here("Processed_Datasets",
                                     "Rfe_best_subset_imp_combined_yield.csv"))

###############
### predictors train ###
############# 


predictors_train <- train %>% dplyr::select(Imp_variables$Features)

predictors_test <- test %>% dplyr::select(Imp_variables$Features)

#############
### Model ##
##########

Combined_model <- readRDS(here::here("Models","Rf_national_model.rds"))

######
## Reference train ###
######## 

Reference_train <- train %>% dplyr::select(Yield,Year,State,County,Longitude,
                                           Latitude)

Reference_test <- test %>% dplyr::select(Yield,Year,State,County,Longitude,
                                         Latitude)


#### The shape file for the region ###

### read in the shapefile for all states ##
################


States <- st_read(here::here("Shape_files","c_08mr23",
                             "c_08mr23.shp")) %>% 
                      dplyr::rename(State = STATE,
                                    County = COUNTYNAME) %>% 
                      dplyr::mutate(County = toupper(County)) %>%
                      dplyr::rename(Longitude = LON,
                                    Latitude = LAT)



seed <- set.seed(1234)

Combined_model_prediction <- Model_predict(predictors_train,predictors_test,
                                           Combined_model,Reference_train,
                                           Reference_test,States,seed)


Combined_model_prediction <- Combined_model_prediction %>% 
                              dplyr::mutate(Error = abs(`Yield` - `Predictions`)) %>% 
                              dplyr::mutate(Model = "National")


Combined_model_prediction_df <- Combined_model_prediction %>% st_drop_geometry()

### exporting this file out ###

write.csv(Combined_model_prediction_df,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Errors_national.csv",
          row.names = F)


###########
#### MN ###
######## 

train_MN <- read.csv(here::here("Processed_Datasets","train_MN.csv"))

test_MN <- read.csv(here::here("Processed_Datasets","test_MN.csv"))

##########
## Importing the most important variables ####
########## 

Imp_variables_MN <- read.csv(here::here("Processed_Datasets",
                                     "Rfe_best_subset_imp_MN.csv"))

###############
### predictors train ###
############# 


predictors_train_MN <- train_MN %>% dplyr::select(Imp_variables_MN$Features)

predictors_test_MN <- test_MN %>% dplyr::select(Imp_variables_MN$Features)

#############
### Model ##
##########

MN_model <- readRDS(here::here("Models",
                               "Rf_MN_model.rds"))

######
## Reference train ###
######## 

Reference_train_MN <- train_MN %>% dplyr::select(Yield,Year,State,County,Longitude,
                                           Latitude)

Reference_test_MN <- test_MN %>% dplyr::select(Yield,Year,State,County,Longitude,
                                         Latitude)


#### The shape file for the region ###

### read in the shapefile for all states ##
################
#### MN state sf ###

States_MN <- States %>% filter(State == "MN")



seed <- set.seed(1234)

MN_predictions <- Model_predict(predictors_train_MN,predictors_test_MN,
                                MN_model,Reference_train_MN,
                                Reference_test_MN,States_MN,seed)


MN_predictions <- MN_predictions %>% 
                            dplyr::mutate(Error = abs(`Yield` - `Predictions`))  %>% 
                            dplyr::mutate(Model = "Minnesota")


                         
###########
#### CO ###
######## 

train_CO <- read.csv(here::here("Processed_Datasets","train_CO.csv"))

test_CO <- read.csv(here::here("Processed_Datasets","test_CO.csv"))

##########
## Importing the most important variables ####
########## 

Imp_variables_CO <- read.csv(here::here("Processed_Datasets",
                                        "Rfe_best_subset_imp_CO.csv"))

###############
### predictors train ###
############# 


predictors_train_CO <- train_CO %>% dplyr::select(Imp_variables_CO$Features)

predictors_test_CO <- test_CO %>% dplyr::select(Imp_variables_CO$Features)

#############
### Model ##
##########

CO_model <- readRDS(here::here("Models",
                               "Rf_CO_model.rds"))

######
## Reference train ###
######## 

Reference_train_CO <- train_CO %>% dplyr::select(Yield,Year,State,County,Longitude,
                                                 Latitude)

Reference_test_CO <- test_CO %>% dplyr::select(Yield,Year,State,County,Longitude,
                                               Latitude)


#### The shape file for the region ###

### read in the shapefile for all states ##
################
#### MN state sf ###

States_CO <- States %>% filter(State == "CO")



seed <- set.seed(1234)

CO_predictions <- Model_predict(predictors_train_CO,predictors_test_CO,
                                CO_model,Reference_train_CO,
                                Reference_test_CO,States_CO,seed)


CO_predictions <- CO_predictions %>% 
                             dplyr::mutate(Error = abs(`Yield` - `Predictions`)) %>% 
                             dplyr::mutate(Model = "Colorado")



###########
#### NEBRASKA ###
######## 

train_NE <- read.csv(here::here("Processed_Datasets","train_NE.csv"))

test_NE <- read.csv(here::here("Processed_Datasets","test_NE.csv"))

##########
## Importing the most important variables ####
########## 

Imp_variables_NE <- read.csv(here::here("Processed_Datasets",
                                        "Rfe_best_subset_imp_NE.csv"))

###############
### predictors train ###
############# 


predictors_train_NE <- train_NE %>% dplyr::select(Imp_variables_NE$Features)

predictors_test_NE <- test_NE %>% dplyr::select(Imp_variables_NE$Features)

#############
### Model ##
##########

NE_model <- readRDS(here::here("Models",
                               "GBM_NE_model.rds"))

######
## Reference train ###
######## 

Reference_train_NE <- train_NE %>% dplyr::select(Yield,Year,State,County,Longitude,
                                                 Latitude)

Reference_test_NE <- test_NE %>% dplyr::select(Yield,Year,State,County,Longitude,
                                               Latitude)


#### The shape file for the region ###

### read in the shapefile for all states ##
################
#### NE state sf ###

States_NE <- States %>% filter(State == "NE")



seed <- set.seed(1234)

NE_predictions <- Model_predict(predictors_train_NE,predictors_test_NE,
                                NE_model,Reference_train_NE,
                                Reference_test_NE,States_NE,seed)


NE_predictions <- NE_predictions %>% 
                           dplyr::mutate(Error = abs(`Yield` - `Predictions`)) %>% 
                           dplyr::mutate(Model = "Nebraska")


##########
## KANSAS ##
########### 

train_KS <- read.csv(here::here("Processed_Datasets","train_KS.csv"))

test_KS <- read.csv(here::here("Processed_Datasets","test_KS.csv"))

##########
## Importing the most important variables ####
########## 

Imp_variables_KS <- read.csv(here::here("Processed_Datasets",
                                        "Rfe_best_subset_imp_KS.csv"))

###############
### predictors train ###
############# 


predictors_train_KS <- train_KS %>% dplyr::select(Imp_variables_KS$Features)

predictors_test_KS <- test_KS %>% dplyr::select(Imp_variables_KS$Features)

#############
### Model ##
##########

KS_model <- readRDS(here::here("Models",
                               "Rf_KS_model.rds"))

######
## Reference train ###
######## 

Reference_train_KS <- train_KS %>% dplyr::select(Yield,Year,State,County,Longitude,
                                                 Latitude)

Reference_test_KS <- test_KS %>% dplyr::select(Yield,Year,State,County,Longitude,
                                               Latitude)


#### The shape file for the region ###

### read in the shapefile for all states ##
################
#### KS state sf ###

States_KS <- States %>% filter(State == "KS")



seed <- set.seed(1234)

KS_predictions <- Model_predict(predictors_train_KS,predictors_test_KS,
                                KS_model,Reference_train_KS,
                                Reference_test_KS,States_KS,seed)


KS_predictions <- KS_predictions %>% 
                             dplyr::mutate(Error = abs(`Yield` - `Predictions`)) %>% 
                             dplyr::mutate(Model = "Kansas")



##########
## TEXAS ##
########### 

train_TX <- read.csv(here::here("Processed_Datasets","train_TX.csv"))

test_TX <- read.csv(here::here("Processed_Datasets","test_TX.csv"))

##########
## Importing the most important variables ####
########## 

Imp_variables_TX <- read.csv(here::here("Processed_Datasets",
                                        "Rfe_best_subset_imp_TX.csv"))

###############
### predictors train ###
############# 


predictors_train_TX <- train_TX %>% dplyr::select(Imp_variables_TX$Features)

predictors_test_TX <- test_TX %>% dplyr::select(Imp_variables_TX$Features)

#############
### Model ##
##########

TX_model <- readRDS(here::here("Models",
                               "GBM_TX_model.rds"))

######
## Reference train ###
######## 

Reference_train_TX <- train_TX %>% dplyr::select(Yield,Year,State,County,Longitude,
                                                 Latitude)

Reference_test_TX <- test_TX %>% dplyr::select(Yield,Year,State,County,Longitude,
                                               Latitude)


#### The shape file for the region ###

### read in the shapefile for all states ##
################
#### TX state sf ###

States_TX <- States %>% filter(State == "TX")



seed <- set.seed(1234)

TX_predictions <- Model_predict(predictors_train_TX,predictors_test_TX,
                                TX_model,Reference_train_TX,
                                Reference_test_TX,States_TX,seed)


TX_predictions <- TX_predictions %>% 
                             dplyr::mutate(Error = abs(`Yield` - `Predictions`)) %>% 
                             dplyr::mutate(Model = "Texas")




#######
## Adding all the predictions in one dataframe ### Containing the best models for a given region ##
######### 

ND_SD <- c("ND","SD")

ND_SD_predictions <- Combined_model_prediction %>% 
                                          filter(State %in% ND_SD)
                                         


Train_test_errors_sf <- rbind(ND_SD_predictions,
                              MN_predictions,CO_predictions,
                              KS_predictions,NE_predictions,
                              TX_predictions) 


Train_test_errors <- Train_test_errors_sf %>% st_drop_geometry()

Average_Test_errors <- Train_test_errors %>% 
                               dplyr::filter(Dataset == "Test") %>% 
                               dplyr::select(FIPS,State,County,Year,Error) %>% 
                               dplyr::group_by(State,County) %>% 
                               dplyr::summarize(Average_Error = mean(Error,
                                                                     na.rm=T))


FIPS_test <- Train_test_errors_sf %>% 
                                 st_drop_geometry() %>% 
                                 filter(Dataset == "Test") %>% 
                                 group_by(County,State,FIPS) %>% 
                                 distinct(FIPS)



Average_Test_errors <- dplyr::inner_join(FIPS_test,Average_Test_errors,
                                          by = c("State","County"))


write.csv(Train_test_errors,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Errors.csv",
          row.names = F)


write.csv(Average_Test_errors,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Average_test_errors.csv",row.names = F)




saveRDS(Train_test_errors,
        "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/RDS_objects/Errors.RDS")


saveRDS(Average_Test_errors,
        "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/RDS_objects/Average_test_errors.RDS")





















