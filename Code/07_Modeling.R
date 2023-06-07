

rm(list = ls())

packages <- list("tidyverse","caret","CAST","here",
                 "randomForest","gbm","xgboost")

lapply(packages, require,character.only=T)

####

source(here::here("Code","00_Functions.R"))


################# loading in the datasets 

train <- read.csv(here("Processed_Datasets","train.csv"))

test <- read.csv(here("Processed_Datasets","test.csv"))




######################################
###### INSIGHTS FROM THE COMBINED DATASET ##
################################# 

## CREATE SPACE TIME FOLDS ###
## each year is a fold ####

set.seed(1234)
indices <- CreateSpacetimeFolds(train,spacevar = "SpaceVar", k=29)

### setting up the predictors ###

predictors_train <- train %>% select(-c(Year,State,County,Yield,Longitude,
                                        Latitude,SpaceVar,Latest,Oldest))

predictors_test <- test %>% select(-c(Year,State,County,Yield,Longitude,
                                    Latitude,SpaceVar,Latest,Oldest))


### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                         verbose = T,index=indices$index)


subsets <- c(1:38)

set.seed(1234)

RFE_analysis <- RFE(predictors_train,
                    train$Yield,
                    subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_combined_yield.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_combined_yield.csv",
          row.names = FALSE) 



###### 
####  
### Selecting only the optimal subset of variables ###
############ 

##### 
###########

Rfe_Imp_best_subset <- read_csv("C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_combined_yield.csv")

train_reduced <- train %>% select(Yield,Rfe_Imp_best_subset$Features)

test_reduced <- test %>% select(Yield,Rfe_Imp_best_subset$Features)

############ 
## Random Forest ####
#############

### setting the predictors ###

predictors_train_reduced <- train_reduced %>% 
                            select(Rfe_Imp_best_subset$Features)

predictors_test_reduced <- test_reduced %>% 
                          select(Rfe_Imp_best_subset$Features)

######### ### setting up the paramters ##

params_modeling <- trainControl(method='cv',number = 10,
                                verbose = T,index=indices$index)

set.seed(1234) 


RF_combined <- Random_forest_analysis(predictors_train_reduced,
                                      response = train_reduced$Yield,
                                      params = params_modeling,
                                      test_predictors = predictors_test_reduced,
                                      test_response = test_reduced$Yield)


################
## GBM ###
############ 
## DEFINE GRID ##

grid <- expand.grid(n.trees=c(600,1000),
                    interaction.depth=c(4,6),
                    shrinkage=0.1,
                    n.minobsinnode=10)    

set.seed(1234)

GBM_combined <- Gbm_analysis(predictors_train_reduced,
                             response = train_reduced$Yield,
                             grid = grid, 
                             params = params_modeling,
                             test_predictors = predictors_test_reduced,
                             test_response = test_reduced$Yield)




###########
## XG BOOST ###
#############  


grid <- expand.grid(nrounds= seq(from = 200, to = 1000, by = 50),
                    max_depth = c(2,3,4,5,6),
                    eta = c(0.025,0.05,0.3), 
                    gamma = 0,
                    colsample_bytree=1,
                    min_child_weight=1,
                    subsample=1)


set.seed(1234)

XGB_combined <- XGB_analysis(predictors_train_reduced,
                             response = train_reduced$Yield,
                             grid = grid, params = params_modeling,
                             test_predictors = predictors_test_reduced,
                             test_response = test_reduced$Yield)




######
###  

## saving the models ### 
## save only the best models ## based on RMSE on test

## RF
Rf_national_model <- RF_combined[["Rf"]] 

## saving the trained model for further use ##

saveRDS(Rf_national_model,"Rf_national_model.rds")


### GBM
#gbm_combined_model <- GBM_combined[["GBM"]] 

### saving the trained model for further use ###

#saveRDS(gbm_combined_model,"GBM_combined_model.rds")

### XGB ### BEST MODEL as per the RMSE ###

#xgb_combined_model <- XGB_combined[["XGB"]]

### saving the model for further use ### 

#saveRDS(xgb_combined_model,"XGB_combined_model_yield.rds")

## 

###########################
###### LOCAL MODELS ###
##### STATEWISE ####
####################  

######## NORTH DAKOTA ###

rm(list = ls()) 

###

source(here("Code","00_Functions.R"))


################# loading in the datasets 

train_ND <- read.csv(here("Processed_Datasets",
                       "train_ND.csv"))


test_ND <- read.csv(here("Processed_Datasets",
                     "test_ND.csv"))


####### 
## RECURSIVE FEATURE ELIMINATION ###
########### 

## CREATE SPACE TIME FOLDS ###

set.seed(1234)
indices <- CreateSpacetimeFolds(train_ND,spacevar = "SpaceVar", k=29)

### setting up the predictors ###

predictors_train_ND <- train_ND %>% select(-c(Year,State,County,Yield,Longitude,
                                              Latitude,SpaceVar,Latest,Oldest))

predictors_test_ND <- test_ND %>% select(-c(Year,State,County,Yield,Longitude,
                                          Latitude,SpaceVar,Latest,Oldest))

### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:38)

set.seed(1234)

RFE_analysis <- RFE(predictors_train_ND,train_ND$Yield,subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_ND.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_ND.csv",
          row.names = FALSE) 



###### 
####  
### Selecting only the optimal subset of variables ###
############ 

##### 
###########

train_reduced_ND <- train_ND %>% select(Yield,Rfe_Imp_best_subset$Features)

test_reduced_ND <- test_ND %>% select(Yield,Rfe_Imp_best_subset$Features)

############ 
## Random Forest ####
#############

### setting the predictors ###

predictors_train_reduced_ND <- train_reduced_ND %>% 
                               select(Rfe_Imp_best_subset$Features)

predictors_test_reduced_ND <- test_reduced_ND %>% 
                             select(Rfe_Imp_best_subset$Features)

######### ### setting up the paramters ##

params_modeling <- trainControl(method='cv',number = 10,
                                verbose = T,index=indices$index)

set.seed(1234) 


RF_ND <- Random_forest_analysis(predictors_train_reduced_ND,
                                response = train_reduced_ND$Yield,
                                params = params_modeling,
                                test_predictors = predictors_test_reduced_ND,
                                test_response = test_reduced_ND$Yield)





################
## GBM ###
############ 
## DEFINE GRID ##

grid <- expand.grid(n.trees=c(600,1000),
                    interaction.depth=c(4,6),
                    shrinkage=0.1,
                    n.minobsinnode=10)    


set.seed(1234)

GBM_ND <- Gbm_analysis(predictors_train_reduced_ND,
                       response = train_reduced_ND$Yield,
                       grid = grid, 
                       params = params_modeling,
                       test_predictors = predictors_test_reduced_ND,
                       test_response = test_reduced_ND$Yield)




###########
## XG BOOST ###
#############  


grid <- expand.grid(nrounds= seq(from = 200, to = 1000, by = 50),
                    max_depth = c(2,3,4,5,6),
                    eta = c(0.025,0.05,0.3), 
                    gamma = 0,
                    colsample_bytree=1,
                    min_child_weight=1,
                    subsample=1)


set.seed(1234)

XGB_ND <- XGB_analysis(predictors_train_reduced_ND,
                       response = train_reduced_ND$Yield,
                       grid = grid, params = params_modeling,
                       test_predictors = predictors_test_reduced_ND,
                       test_response = test_reduced_ND$Yield)





######
###  

## saving the models ### 

## RF
Rf_ND_model <- RF_ND[["Rf"]] 

## saving the trained model for further use ##

saveRDS(Rf_ND_model,"Rf_ND_model.rds")


### GBM
#gbm_ND_model <- GBM_ND[["GBM"]] 

### saving the trained model for further use ###

#saveRDS(gbm_ND_model,"GBM_ND_model.rds")

### XGB ###

#xgb_ND_model <- XGB_ND[["XGB"]]

### saving the model for further use ### 

#saveRDS(xgb_ND_model,"XGB_ND_model.rds")


###############################
######## SD ###########
###################

rm(list = ls())


###

source(here::here("Code","00_Functions.R"))


################# loading in the datasets 

train_SD <- read.csv(here("Processed_Datasets",
                          "train_SD.csv"))


test_SD <- read.csv(here("Processed_Datasets",
                        "test_SD.csv"))



####### 
## RECURSIVE FEATURE ELIMINATION ###
########### 

## CREATE SPACE TIME FOLDS ###

set.seed(1234)
indices <- CreateSpacetimeFolds(train_SD,spacevar = "SpaceVar", k=28)

### setting up the predictors ###

predictors_train_SD <- train_SD %>% select(-c(Year,State,County,Yield,Longitude,
                                               Latitude,SpaceVar,Latest,Oldest))

predictors_test_SD <- test_SD %>% dplyr::select(-c(Year,State,County,Yield,Longitude,
                                           Latitude,SpaceVar,Latest,Oldest))

### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:38)

set.seed(1234)

RFE_analysis <- RFE(predictors_train_SD,train_SD$Yield,subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


##
### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_SD.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_SD.csv",
          row.names = FALSE) 



###### 
####  
### Selecting only the optimal subset of variables ###
############ 

##### 
###########


train_reduced_SD <- train_SD %>% dplyr::select(Yield,Rfe_Imp_best_subset$Features)

test_reduced_SD <- test_SD %>% dplyr::select(Yield,Rfe_Imp_best_subset$Features)

############ 
## Random Forest ####
#############

### setting the predictors ###

predictors_train_reduced_SD <- train_reduced_SD %>% 
                               dplyr::select(Rfe_Imp_best_subset$Features)

predictors_test_reduced_SD <- test_reduced_SD %>% 
                                dplyr::select(Rfe_Imp_best_subset$Features)

######### ### setting up the paramters ##

params_modeling <- trainControl(method='cv',number = 10,
                                verbose = T,index=indices$index)

set.seed(1234) 


RF_SD <- Random_forest_analysis(predictors_train_reduced_SD,
                                response = train_reduced_SD$Yield,
                                params = params_modeling,
                                test_predictors = predictors_test_reduced_SD,
                                test_response = test_reduced_SD$Yield)





################
## GBM ###
############ 
## DEFINE GRID ##

grid <- expand.grid(n.trees=c(600,1000),
                    interaction.depth=c(4,6),
                    shrinkage=0.1,
                    n.minobsinnode=10)    

set.seed(1234)
GBM_SD <- Gbm_analysis(predictors_train_reduced_SD,
                       response = train_reduced_SD$Yield,
                       grid = grid, 
                       params = params_modeling,
                       test_predictors = predictors_test_reduced_SD,
                       test_response = test_reduced_SD$Yield)




###########
## XG BOOST ###
#############  


grid <- expand.grid(nrounds= seq(from = 200, to = 1000, by = 50),
                    max_depth = c(2,3,4,5,6),
                    eta = c(0.025,0.05,0.3), 
                    gamma = 0,
                    colsample_bytree=1,
                    min_child_weight=1,
                    subsample=1)

set.seed(1234)

XGB_SD <- XGB_analysis(predictors_train_reduced_SD,
                       response = train_reduced_SD$Yield,
                       grid = grid, params = params_modeling,
                       test_predictors = predictors_test_reduced_SD,
                       test_response = test_reduced_SD$Yield)


######
###  

## saving the models ### 

## RF
#Rf_SD_model <- RF_SD[["Rf"]] 

## saving the trained model for further use ##

#saveRDS(Rf_SD_model,"Rf_SD_model.rds")


### GBM
#gbm_SD_model <- GBM_SD[["GBM"]] 

### saving the trained model for further use ###

#saveRDS(gbm_SD_model,"GBM_SD_model.rds")

### XGB ###

xgb_SD_model <- XGB_SD[["XGB"]]

### saving the model for further use ### 

saveRDS(xgb_SD_model,"XGB_SD_model.rds")


#####################
## MINNESOTA ####
#######################

rm(list = ls())


###

source(here("Code","00_Functions.R"))


################# loading in the datasets 

train_MN <- read.csv(here("Processed_Datasets",
                          "train_MN.csv"))


test_MN <- read.csv(here("Processed_Datasets",
                        "test_MN.csv"))


####### 
## RECURSIVE FEATURE ELIMINATION ###
########### 

## CREATE SPACE TIME FOLDS ###

set.seed(1234)
indices <- CreateSpacetimeFolds(train_MN,spacevar = "SpaceVar", k=28)

### setting up the predictors ###

predictors_train_MN <- train_MN %>% select(-c(Year,State,County,Yield,Longitude,
                                              Latitude,SpaceVar,Latest,Oldest))

predictors_test_MN <- test_MN %>% select(-c(Year,State,County,Yield,Longitude,
                                          Latitude,SpaceVar,Latest,Oldest))

### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:38)

set.seed(1234)

RFE_analysis <- RFE(predictors_train_MN,train_MN$Yield,subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_MN.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_MN.csv",
          row.names = FALSE) 



###### 
####  
### Selecting only the optimal subset of variables ###
############ 

##### 
###########

train_reduced_MN <- train_MN %>% select(Yield,Rfe_Imp_best_subset$Features)

test_reduced_MN <- test_MN %>% select(Yield,Rfe_Imp_best_subset$Features)

############ 
## Random Forest ####
#############

### setting the predictors ###

predictors_train_reduced_MN <- train_reduced_MN %>% 
                               select(Rfe_Imp_best_subset$Features)

predictors_test_reduced_MN <- test_reduced_MN %>% 
                             select(Rfe_Imp_best_subset$Features)

######### ### setting up the paramters ##

params_modeling <- trainControl(method='cv',number = 10,
                                verbose = T,index=indices$index)

set.seed(1234) 


RF_MN <- Random_forest_analysis(predictors_train_reduced_MN,
                                response = train_reduced_MN$Yield,
                                params = params_modeling,
                                test_predictors = predictors_test_reduced_MN,
                                test_response = test_reduced_MN$Yield)





################
## GBM ###
############ 
## DEFINE GRID ##

grid <- expand.grid(n.trees=c(600,1000),
                    interaction.depth=c(4,6),
                    shrinkage=0.1,
                    n.minobsinnode=10)    

set.seed(1234)

GBM_MN <- Gbm_analysis(predictors_train_reduced_MN,
                       response = train_reduced_MN$Yield,
                       grid = grid, 
                       params = params_modeling,
                       test_predictors = predictors_test_reduced_MN,
                       test_response = test_reduced_MN$Yield)




###########
## XG BOOST ###
#############  


grid <- expand.grid(nrounds= seq(from = 200, to = 1000, by = 50),
                    max_depth = c(2,3,4,5,6),
                    eta = c(0.025,0.05,0.3), 
                    gamma = 0,
                    colsample_bytree=1,
                    min_child_weight=1,
                    subsample=1)

set.seed(1234)

XGB_MN <- XGB_analysis(predictors_train_reduced_MN,
                       response = train_reduced_MN$Yield,
                       grid = grid, params = params_modeling,
                       test_predictors = predictors_test_reduced_MN,
                       test_response = test_reduced_MN$Yield)



######
###  

## saving the models ### 

## RF
Rf_MN_model <- RF_MN[["Rf"]] 

## saving the trained model for further use ##

saveRDS(Rf_MN_model,"Rf_MN_model.rds")


### GBM
#gbm_MN_model <- GBM_MN[["GBM"]] 

### saving the trained model for further use ###

#saveRDS(gbm_MN_model,"GBM_MN_model.rds")

### XGB ###

#xgb_MN_model <- XGB_MN[["XGB"]]

### saving the model for further use ### 

#saveRDS(xgb_MN_model,"XGB_MN_model.rds")


#################
#### COLORADO ####
################# 

rm(list = ls())


###

source(here("Code","00_Functions.R"))


################# loading in the datasets 

train_CO <- read.csv(here("Processed_Datasets",
                          "train_CO.csv"))


test_CO <- read.csv(here("Processed_Datasets",
                        "test_CO.csv"))


####### 
## RECURSIVE FEATURE ELIMINATION ###
########### 

## CREATE SPACE TIME FOLDS ###

set.seed(1234)
indices <- CreateSpacetimeFolds(train_CO,spacevar = "SpaceVar", k=14)

### setting up the predictors ###

predictors_train_CO <- train_CO %>% select(-c(Year,State,County,Yield,Longitude,
                                              Latitude,SpaceVar,Latest,Oldest))

predictors_test_CO <- test_CO %>% select(-c(Year,State,County,Yield,Longitude,
                                          Latitude,SpaceVar,Latest,Oldest))

### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:38)

set.seed(1234)

RFE_analysis <- RFE(predictors_train_CO,train_CO$Yield,subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_CO.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_CO.csv",
          row.names = FALSE) 



###### 
####  
### Selecting only the optimal subset of variables ###
############ 

##### 
###########

train_reduced_CO <- train_CO %>% select(Yield,Rfe_Imp_best_subset$Features)

test_reduced_CO <- test_CO %>% select(Yield,Rfe_Imp_best_subset$Features)

############ 
## Random Forest ####
#############

### setting the predictors ###

predictors_train_reduced_CO <- train_reduced_CO %>% 
                               select(Rfe_Imp_best_subset$Features)

predictors_test_reduced_CO <- test_reduced_CO %>% 
                             select(Rfe_Imp_best_subset$Features)

######### ### setting up the paramters ##

params_modeling <- trainControl(method='cv',number = 10,
                                verbose = T,index=indices$index)

set.seed(1234) 


RF_CO <- Random_forest_analysis(predictors_train_reduced_CO,
                                response = train_reduced_CO$Yield,
                                params = params_modeling,
                                test_predictors = predictors_test_reduced_CO,
                                test_response = test_reduced_CO$Yield)





################
## GBM ###
############ 
## DEFINE GRID ##

grid <- expand.grid(n.trees=c(600,1000),
                    interaction.depth=c(4,6),
                    shrinkage=0.1,
                    n.minobsinnode=10)    

set.seed(1234)

GBM_CO <- Gbm_analysis(predictors_train_reduced_CO,
                       response = train_reduced_CO$Yield,
                       grid = grid, 
                       params = params_modeling,
                       test_predictors = predictors_test_reduced_CO,
                       test_response = test_reduced_CO$Yield)




###########
## XG BOOST ###
#############  


grid <- expand.grid(nrounds= seq(from = 200, to = 1000, by = 50),
                    max_depth = c(2,3,4,5,6),
                    eta = c(0.025,0.05,0.3), 
                    gamma = 0,
                    colsample_bytree=1,
                    min_child_weight=1,
                    subsample=1)


set.seed(1234)

XGB_CO <- XGB_analysis(predictors_train_reduced_CO,
                       response = train_reduced_CO$Yield,
                       grid = grid, params = params_modeling,
                       test_predictors = predictors_test_reduced_CO,
                       test_response = test_reduced_CO$Yield)





######
###  

## saving the models ### 

## RF
Rf_CO_model <- RF_CO[["Rf"]] 

## saving the trained model for further use ##

saveRDS(Rf_CO_model,"Rf_CO_model.rds")


### GBM
#gbm_CO_model <- GBM_CO[["GBM"]] 

### saving the trained model for further use ###

#saveRDS(gbm_CO_model,"GBM_CO_model.rds")

### XGB ###

#xgb_CO_model <- XGB_CO[["XGB"]]

### saving the model for further use ### 

#saveRDS(xgb_CO_model,"XGB_CO_model.rds")


###############
## KANSAS ####
################# 

rm(list = ls())


###

source(here("Code","00_Functions.R"))


################# loading in the datasets 

train_KS <- read.csv(here("Processed_Datasets",
                          "train_KS.csv"))


test_KS <- read.csv(here("Processed_Datasets",
                        "test_KS.csv"))



####### 
## RECURSIVE FEATURE ELIMINATION ###
########### 

## CREATE SPACE TIME FOLDS ###

set.seed(1234)
indices <- CreateSpacetimeFolds(train_KS,spacevar = "SpaceVar", k=10)

### setting up the predictors ###

predictors_train_KS <- train_KS %>% select(-c(Year,State,County,Yield,Longitude,
                                        Latitude,SpaceVar,Latest,Oldest))

predictors_test_KS <- test_KS %>% select(-c(Year,State,County,Yield,Longitude,
                                            Latitude,SpaceVar,Latest,Oldest))

### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:38)

set.seed(1234)

RFE_analysis <- RFE(predictors_train_KS,train_KS$Yield,subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_KS.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_KS.csv",
          row.names = FALSE) 



###### 
####  
### Selecting only the optimal subset of variables ###
############ 

##### 
###########

train_reduced_KS <- train_KS %>% select(Yield,Rfe_Imp_best_subset$Features)

test_reduced_KS <- test_KS %>% select(Yield,Rfe_Imp_best_subset$Features)

############ 
## Random Forest ####
#############

### setting the predictors ###

predictors_train_reduced_KS <- train_reduced_KS %>% 
                               select(Rfe_Imp_best_subset$Features)

predictors_test_reduced_KS <- test_reduced_KS %>% 
                             select(Rfe_Imp_best_subset$Features)

######### ### setting up the paramters ##

params_modeling <- trainControl(method='cv',number = 10,
                                verbose = T,index=indices$index)

set.seed(1234) 


RF_KS <- Random_forest_analysis(predictors_train_reduced_KS,
                                response = train_reduced_KS$Yield,
                                params = params_modeling,
                                test_predictors = predictors_test_reduced_KS,
                                test_response = test_reduced_KS$Yield)





################
## GBM ###
############ 
## DEFINE GRID ##

grid <- expand.grid(n.trees=c(600,1000),
                    interaction.depth=c(4,6),
                    shrinkage=0.1,
                    n.minobsinnode=10)    

set.seed(1234)

GBM_KS <- Gbm_analysis(predictors_train_reduced_KS,
                       response = train_reduced_KS$Yield,
                       grid = grid, 
                       params = params_modeling,
                       test_predictors = predictors_test_reduced_KS,
                       test_response = test_reduced_KS$Yield)




###########
## XG BOOST ###
#############  


grid <- expand.grid(nrounds= seq(from = 200, to = 1000, by = 50),
                    max_depth = c(2,3,4,5,6),
                    eta = c(0.025,0.05,0.3), 
                    gamma = 0,
                    colsample_bytree=1,
                    min_child_weight=1,
                    subsample=1)

set.seed(1234)

XGB_KS <- XGB_analysis(predictors_train_reduced_KS,
                       response = train_reduced_KS$Yield,
                       grid = grid, params = params_modeling,
                       test_predictors = predictors_test_reduced_KS,
                       test_response = test_reduced_KS$Yield)





######
###  

## saving the models ### 

## RF
Rf_KS_model <- RF_KS[["Rf"]] 

## saving the trained model for further use ##

saveRDS(Rf_KS_model,"Rf_KS_model.rds")


### GBM
#gbm_KS_model <- GBM_KS[["GBM"]] 

### saving the trained model for further use ###

#saveRDS(gbm_KS_model,"GBM_KS_model.rds")

### XGB ###

#xgb_KS_model <- XGB_KS[["XGB"]]

### saving the model for further use ### 

#saveRDS(xgb_KS_model,"XGB_KS_model.rds")


#####################
#### NEBRASKA ####
############# 


rm(list = ls())


###

source(here::here("Code","00_Functions.R"))


################# loading in the datasets 

train_NE <- read.csv(here::here("Processed_Datasets",
                          "train_NE.csv"))


test_NE <- read.csv(here::here("Processed_Datasets",
                        "test_NE.csv"))



####### 
## RECURSIVE FEATURE ELIMINATION ###
########### 

## CREATE SPACE TIME FOLDS ###

set.seed(1234)
indices <- CreateSpacetimeFolds(train_NE,spacevar = "SpaceVar", k=13)

### setting up the predictors ###

predictors_train_NE <- train_NE %>% select(-c(Year,State,County,Yield,Longitude,
                                              Latitude,SpaceVar,Latest,Oldest))

predictors_test_NE <- test_NE %>% select(-c(Year,State,County,Yield,Longitude,
                                          Latitude,SpaceVar,Latest,Oldest))
### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:38)

set.seed(1234)

RFE_analysis <- RFE(predictors_train_NE,train_NE$Yield,subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_NE.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_NE.csv",
          row.names = FALSE) 



###### 
####  
### Selecting only the optimal subset of variables ###
############ 

##### 
###########


train_reduced_NE <- train_NE %>% dplyr::select(Yield,Rfe_Imp_best_subset$Features)

test_reduced_NE <- test_NE %>% dplyr::select(Yield,Rfe_Imp_best_subset$Features)

############ 
## Random Forest ####
#############

### setting the predictors ###

predictors_train_reduced_NE <- train_reduced_NE %>% 
                               dplyr::select(Rfe_Imp_best_subset$Features)

predictors_test_reduced_NE <- test_reduced_NE %>% 
                                 dplyr::select(Rfe_Imp_best_subset$Features)

######### ### setting up the paramters ##

params_modeling <- trainControl(method='cv',number = 10,
                                verbose = T,index=indices$index)


set.seed(1234) 


RF_NE <- Random_forest_analysis(predictors_train_reduced_NE,
                                response = train_reduced_NE$Yield,
                                params = params_modeling,
                                test_predictors = predictors_test_reduced_NE,
                                test_response = test_reduced_NE$Yield)





################
## GBM ###
############ 
## DEFINE GRID ##

grid <- expand.grid(n.trees=c(600,1000),
                    interaction.depth=c(4,6),
                    shrinkage=0.1,
                    n.minobsinnode=10)    

set.seed(1234)

GBM_NE <- Gbm_analysis(predictors_train_reduced_NE,
                       response = train_reduced_NE$Yield,
                       grid = grid, 
                       params = params_modeling,
                       test_predictors = predictors_test_reduced_NE,
                       test_response = test_reduced_NE$Yield)




###########
## XG BOOST ###
#############  


grid <- expand.grid(nrounds= seq(from = 200, to = 1000, by = 50),
                    max_depth = c(2,3,4,5,6),
                    eta = c(0.025,0.05,0.3), 
                    gamma = 0,
                    colsample_bytree=1,
                    min_child_weight=1,
                    subsample=1)

set.seed(1234)


XGB_NE <- XGB_analysis(predictors_train_reduced_NE,
                       response = train_reduced_NE$Yield,
                       grid = grid, params = params_modeling,
                       test_predictors = predictors_test_reduced_NE,
                       test_response = test_reduced_NE$Yield)





######
###  

## saving the models ### 

## RF
#Rf_NE_model <- RF_NE[["Rf"]] 

## saving the trained model for further use ##

#saveRDS(Rf_NE_model,"Rf_NE_model.rds")


### GBM
gbm_NE_model <- GBM_NE[["GBM"]] 

### saving the trained model for further use ###

saveRDS(gbm_NE_model,"GBM_NE_model.rds")

### XGB ###

#xgb_NE_model <- XGB_NE[["XGB"]]

### saving the model for further use ### 

#saveRDS(xgb_NE_model,"XGB_NE_model.rds")


#######################
### TEXAS ###########
################# 

rm(list = ls())


###

source(here("Code","00_Functions.R"))


################# loading in the datasets 

train_TX <- read.csv(here("Processed_Datasets",
                          "train_TX.csv"))


test_TX <- read.csv(here("Processed_Datasets",
                        "test_TX.csv"))



####### 
## RECURSIVE FEATURE ELIMINATION ###
########### 

## CREATE SPACE TIME FOLDS ###

set.seed(1234)
indices <- CreateSpacetimeFolds(train_TX,spacevar = "SpaceVar", k=27)

### setting up the predictors ###

predictors_train_TX <- train_TX %>% select(-c(Year,State,County,Yield,Longitude,
                                              Latitude,SpaceVar,Latest,Oldest))

predictors_test_TX <- test_TX %>% select(-c(Year,State,County,Yield,Longitude,
                                          Latitude,SpaceVar,Latest,Oldest))

### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:38)

set.seed(1234)

RFE_analysis <- RFE(predictors_train_TX,train_TX$Yield,subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_TX.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_TX.csv",
          row.names = FALSE) 



###### 
####  
### Selecting only the optimal subset of variables ###
############ 

##### 
###########

train_reduced_TX <- train_TX %>% select(Yield,Rfe_Imp_best_subset$Features)

test_reduced_TX <- test_TX %>% select(Yield,Rfe_Imp_best_subset$Features)

############ 
## Random Forest ####
#############

### setting the predictors ###

predictors_train_reduced_TX <- train_reduced_TX %>% 
                               select(Rfe_Imp_best_subset$Features)

predictors_test_reduced_TX <- test_reduced_TX %>% 
                             select(Rfe_Imp_best_subset$Features)

######### ### setting up the paramters ##

params_modeling <- trainControl(method='cv',number = 10,
                                verbose = T,index=indices$index)

set.seed(1234) 


RF_TX <- Random_forest_analysis(predictors_train_reduced_TX,
                                response = train_reduced_TX$Yield,
                                params = params_modeling,
                                test_predictors = predictors_test_reduced_TX,
                                test_response = test_reduced_TX$Yield)





################
## GBM ###
############ 
## DEFINE GRID ##

grid <- expand.grid(n.trees=c(600,1000),
                    interaction.depth=c(4,6),
                    shrinkage=0.1,
                    n.minobsinnode=10)    


set.seed(1234)

GBM_TX <- Gbm_analysis(predictors_train_reduced_TX,
                       response = train_reduced_TX$Yield,
                       grid = grid, 
                       params = params_modeling,
                       test_predictors = predictors_test_reduced_TX,
                       test_response = test_reduced_TX$Yield)




###########
## XG BOOST ###
#############  


grid <- expand.grid(nrounds= seq(from = 200, to = 1000, by = 50),
                    max_depth = c(2,3,4,5,6),
                    eta = c(0.025,0.05,0.3), 
                    gamma = 0,
                    colsample_bytree=1,
                    min_child_weight=1,
                    subsample=1)



set.seed(1234)

XGB_TX <- XGB_analysis(predictors_train_reduced_TX,
                       response = train_reduced_TX$Yield,
                       grid = grid, params = params_modeling,
                       test_predictors = predictors_test_reduced_TX,
                       test_response = test_reduced_TX$Yield)






######
###  

## saving the models ### 

## RF
#Rf_TX_model <- RF_TX[["Rf"]] 

## saving the trained model for further use ##

#saveRDS(Rf_TX_model,"Rf_TX_model.rds")


### GBM
gbm_TX_model <- GBM_TX[["GBM"]] 

### saving the trained model for further use ###

saveRDS(gbm_TX_model,"GBM_TX_model.rds")

### XGB ###

#xgb_TX_model <- XGB_TX[["XGB"]]

### saving the model for further use ### 

#saveRDS(xgb_TX_model,"XGB_TX_model.rds")



















