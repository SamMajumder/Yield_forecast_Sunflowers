

#### This function will read in all excel files from the folder 
## aggregate them and change the names of the columns where appropriate ###


#######

Future_data_agrregate <- function(path,RCP,Year,
                                  file_pattern,
                                  full_names,
                                  Pattern_to_remove_1=NULL,
                                  Pattern_to_remove_2=NULL){
  
  file_names <- list.files(path = path,
                           pattern = file_pattern,
                           full.names = full_names) 
  
  
  
  file_list <- lapply(file_names, read_excel)  
  
  
  Future_Climate <- file_list %>% purrr::reduce(inner_join)
  

  #### extracting the old names
  
  Old_names <- colnames(Future_Climate) 
  
  
  ### Add the year and RCP column ### 
  
  Future_Climate <- Future_Climate %>% 
    mutate(RCP = RCP,
           Year = Year)

  ##### setting up new names 
  New_names <- Old_names %>% 
    str_remove(Pattern_to_remove_1) %>% 
    str_remove(Pattern_to_remove_2)
  
  
  
  
  ### Now renaming the data frame 
  
  Future_Climate <- Future_Climate %>% 
                     dplyr::rename_at(vars(Old_names), ~ New_names)
  
  return(Future_Climate)
  
} 


Future_data_compile <- function(path_1,path_2,path_3,path_4,
                                RCP_1,RCP_2,RCP_3,RCP_4,
                                Year, file_pattern,
                                full_names,Pattern_to_remove_1,
                                Pattern_to_remove_126,
                                Pattern_to_remove_245,
                                Pattern_to_remove_370,
                                Pattern_to_remove_585){
  
  
  Future_Data_126 <- Future_data_agrregate(path = path_1,
                                           RCP = RCP_1,
                                           Year = Year,
                                           file_pattern,
                                           full_names,
                                           Pattern_to_remove_1 = Pattern_to_remove_1,
                                           Pattern_to_remove_2 = Pattern_to_remove_126) 
  
  
  Future_Data_245 <- Future_data_agrregate(path = path_2,
                                           RCP = RCP_2,
                                           Year = Year,
                                           file_pattern,
                                           full_names,
                                           Pattern_to_remove_1 = Pattern_to_remove_1,
                                           Pattern_to_remove_2 = Pattern_to_remove_245) 
  
  
  Future_Data_370 <- Future_data_agrregate(path = path_3,
                                           RCP = RCP_3,
                                           Year = Year,
                                           file_pattern,
                                           full_names,
                                           Pattern_to_remove_1 = Pattern_to_remove_1,
                                           Pattern_to_remove_2 = Pattern_to_remove_370) 
  
  
  Future_Data_585 <- Future_data_agrregate(path = path_4,
                                           RCP = RCP_4,
                                           Year = Year,
                                           file_pattern,
                                           full_names,
                                           Pattern_to_remove_1 = Pattern_to_remove_1,
                                           Pattern_to_remove_2 = Pattern_to_remove_585)
  
  
  Future_Data <- list("RCP_126" = Future_Data_126,
                      "RCP_245" = Future_Data_245,
                      "RCP_370" = Future_Data_370,
                      "RCP_585" = Future_Data_585) 
  
  return(Future_Data)
  
  
}

Missing_data_insights <- function(df,columns_to_remove = NULL){
  
  df_1 <- df %>% 
    select(-c(columns_to_remove)) 
  
  Missing_values <- df_1 %>%
    gather(key = "key", value = "val") %>%
    mutate(is.missing = is.na(val)) %>%
    group_by(key, is.missing) %>%
    summarise(num.missing = n()) %>%
    filter(is.missing==T) %>%
    select(-is.missing) %>%
    arrange(desc(num.missing))  
  
  
  Missing_percent <- df_1 %>%
    gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    group_by(key) %>%
    mutate(total = n()) %>%
    group_by(key, total, isna) %>%
    summarise(num.isna = n()) %>%
    mutate(pct = num.isna / total * 100) %>%
    mutate(Type = case_when(isna == "FALSE" ~ "Not Missing",
                            isna == "TRUE" ~ "Missing"))  
  
  
  p <-  ggplot(Missing_percent, aes(fill=Type, y=pct, x=key)) + 
    geom_bar(position='stack', stat='identity') +
    labs(x = "Variable", y = "Percent Missing") +
    coord_flip() +
    ggtitle("Percent Missing Value from each variable") +
    theme(text = element_text(size = 10))  
  
  
  Insights <-  list("Missing_data_count" = Missing_values,
                    "Missing_data_percent" = Missing_percent, 
                    "Missing_Valu_plot" = p) 
  
  return(Insights)
}


##############################
##### RFE FUNCTION #########
############################## 

RFE <- function(df,response,subsets,params){
  features_rfe <- rfe(response~.,data = df,
                      sizes=subsets,rfeControl=params)
  
  ## these are the predictors in the optimal subset 
  
  optimal_subset_rfe <- data.frame(Features = predictors(features_rfe))
  
  #### Importance of each variable 
  
  Rfe_Imp <- data.frame(varImp(features_rfe))
  
  Rfe_Imp <- data.frame(Features = rownames(Rfe_Imp),
                        Overall = Rfe_Imp$Overall)
  
  Rfe_Imp_best_subset <- Rfe_Imp %>%
    dplyr::filter(Features %in% optimal_subset_rfe$Features)
  
  
  ### plotting the variation of accuracy with the removal of variables
  p_1 <- ggplot(features_rfe)  
  
  p_2 <- ggplot(data = Rfe_Imp_best_subset,
                aes(x=reorder(Features,Overall), y = Overall, fill = Features)) +
    geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
    coord_flip() + 
    theme_bw() + theme(legend.position = "none") + 
    ggtitle("Optimal subset of factors") +
    theme(text = element_text(size = 10))  
  
  list <- list("Plot of variation of accuracy with the removal of variables" = p_1,
               "Importance plot" = p_2,
               "optimal subset" = optimal_subset_rfe,
               "Importance values" = Rfe_Imp_best_subset
  ) 
  return(list)
  
  
}



################
### RANDOM FOREST ###
##############  

Random_forest_analysis <- function(df,response,params,
                                   test_predictors,
                                   test_response){
  
  Rf <- train(df, response,method="rf",trControl=params,verbose=F)  
  
  print(Rf)
  
  p_rf <- predict(Rf,test_predictors) 
  
  RMSE_rf <- RMSE(p_rf,test_response) 
  
  print(RMSE_rf)
  
  list <- list("Rf" = Rf, "predicted_rf" = p_rf,"RMSE" = RMSE_rf)
  
  return(list)
  
}  


###################### 
### GBM ####
################## 

Gbm_analysis <- function(df,response,
                         grid,params,test_predictors,
                         test_response) {
  
  gbm <- train(df, response,
               method="gbm",trControl=params, 
               verbose=F,tuneGrid=grid) 
  
  print(gbm)
  
  p_gbm <- predict(gbm,test_predictors) 
  
  RMSE_gbm <- RMSE(p_gbm,test_response) 
  
  print(RMSE_gbm)
  
  list <- list("GBM" = gbm, "predicted_gbm" = p_gbm, "RMSE" = RMSE_gbm)
  

  return(list)
  
}




####################
### XGBOOST ####
################

XGB_analysis <- function(df,response,grid,params,test_predictors,
                         test_response){
  
  xgb <- train(df,response,method="xgbTree",trControl=params,verbose=F,
               tuneGrid=grid)
  
  print(xgb)
  
  p_xgb <- predict(xgb,test_predictors) 
  
  RMSE_xgb <- RMSE(p_xgb,test_response) 
  
  print(RMSE_xgb)
  
  list <- list("XGB" = xgb, "predicted_xgb" = p_xgb,"RMSE" = RMSE_xgb)
  
  
  return(list)
  
}

#####
### Predict ###
#######


Model_predict <- function(predictors_train,predictors_test,
                          Model,Reference_train,
                          Reference_test,State_sf,seed) {
  
  #### making predictions on the training set
  seed 
  predictions_train <- predict(Model,predictors_train) 
  
  #### Adding the predictions to the Reference train dataframe
  Reference_train <- cbind(Reference_train,predictions_train)
  
  Reference_train <- Reference_train %>% 
    dplyr::mutate(Dataset = "Train") %>% 
    dplyr::rename(Predictions = predictions_train)
  
  
  ### making predictions on the test set 
  seed
  predictions_test <- predict(Model,predictors_test) 
  
  ##### Adding the predictions to the Reference test dataframe
  Reference_test <- cbind(Reference_test,predictions_test) 
  
  Reference_test <- Reference_test %>% 
    dplyr::mutate(Dataset = "Test") %>% 
    dplyr::rename(Predictions = predictions_test)
  
  
  Predictions <- rbind(Reference_train,Reference_test) 
  
  Predictions <- dplyr::inner_join(State_sf,Predictions)
  
  return(Predictions)
  
}





## CREATING A FUNCTION THAT CREATES FUTURE PREDICTION MAPS FOR ONE TIMEFRAME ##
##### Reference (a df containing county names and the coordinates)

Future_predictions <- function(Future_State_data_126,
                               Future_State_data_245,
                               Future_State_data_370,
                               Future_State_data_585,
                               Ag_prac_data,
                               Drought_data,
                               Imp_variables,
                               Model,
                               State_sf,
                               seed) {
  
  
  ####################
  ####### 126 ######
  ############## 
  
  #### renaming the column of the Ag_prac data
  Ag_prac_data <- Ag_prac_data %>% dplyr::rename(Year_latest = Year)
  
  ##### renaming the column of Drought data ###
  Drought_data <- Drought_data %>% dplyr::rename(Year_latest = Year)
  
  ### Joining the ag_prac data to the Future Climate data for the state 
  
  Future_State_data_126 <- Future_State_data_126 %>% dplyr::rename(State = STATE,
                                                            County = COUNTY) %>% 
                              dplyr::inner_join(Ag_prac_data, by = c("State","County")) %>% 
                              dplyr::inner_join(Drought_data, by = c("State","County")) %>%
                              dplyr::mutate(RCP = "126") %>% 
                              dplyr::mutate(Timeframe = "2040")
  
  
  
  ### now filtering out by important variables as deemed by RFE ##
  
  State_imp_126 <- Future_State_data_126 %>% 
                           dplyr::select(Imp_variables$Features)
  
  
  ### create a prediction file for State ### 
  
  ### 
  seed
  predictions_126 <- predict(Model,State_imp_126) 
  
  
  ### creating a reference Dataframe ###
  ## we will add the predictions to this df ### 
  
  Reference <- Future_State_data_126 %>% 
                          dplyr::select(LAT, LON, County) 
  
  ### Adding the predictions to the Reference dataset ## 
  
  Predictions_126_reference <- cbind(predictions_126,Reference) %>% 
                                      dplyr::rename(Predictions = predictions_126)
  
  
  #### Add the error data to the future prediction file ### 
  
  #Predictions_126_reference <- dplyr::inner_join(Predictions_126_reference,Error_data)  

  ### Add the predictions to the shapefile ### 
  
  Predictions_sf_126 <- dplyr::inner_join(State_sf,Predictions_126_reference)
  
  #### Plot the predictions ### 
  
  State_126_map <- Predictions_sf_126 %>% 
    ggplot(aes(fill=Predictions_126,
               text = str_c(County, ": ", Predictions_126))) +
    geom_sf(colour = NA)  +
    scale_fill_viridis("Yield predictions (LB/Acre)",
                       direction = -1) +
    theme_void()
  
  
  ############
  ### 245 ###
  
  ### Joining the ag_prac data to the Future Climate data for the state 
  ## Doing this eliminates the possibillity of including counties for which 
  ## the model wasn't trained on 
  ### Example: Counties that do not grow sunflowers ### 
  
  Future_State_data_245 <- Future_State_data_245 %>% 
                                                  dplyr::rename(State = STATE,
                                                            County = COUNTY) %>% 
                            dplyr::inner_join(Ag_prac_data, by = c("State","County")) %>% 
                            dplyr::inner_join(Drought_data, by = c("State","County"))
  
  
  
  ### now filtering out by important variables as deemed by RFE ##
  
  State_imp_245 <- Future_State_data_245 %>% 
                                      dplyr::select(Imp_variables$Features)
  
  
  ### create a prediction file for State ### 
  
  ### 
  seed
  predictions_245 <- predict(Model,State_imp_245) 
  
  
  ### creating a reference Dataframe ###
  ## we will add the predictions to this df ### 
  
  Reference <- Future_State_data_245 %>% dplyr::select(LAT, LON, County) 
  
  ### Adding the predictions to the Reference dataset ## 
  
  Predictions_245_reference <- cbind(predictions_245,Reference) %>% 
                                     dplyr::rename(Predictions = predictions_245)
  
  #### Add the error data to the future prediction file ### 
  
  #Predictions_245_reference <- dplyr::inner_join(Predictions_245_reference,Error_data)  
  
  ### Add the predictions to the shapefile ### 
  
  Predictions_sf_245 <- dplyr::inner_join(State_sf,Predictions_245_reference)
  
  #### Plot the predictions ### 
  
  State_245_map <- Predictions_sf_245 %>% 
    ggplot(aes(fill=Predictions_245,
               text = str_c(County, ": ", Predictions_245))) +
    geom_sf(colour = NA)  +
    scale_fill_viridis("Yield predictions (LB/Acre)",
                       direction = -1) + 
    theme_void()
  
  
  
  ############
  ### 370 ###
  
  ### Joining the ag_prac data to the Future Climate data for the state 
  ## Doing this eliminates the possibillity of including counties for which 
  ## the model wasn't trained on 
  ### Example: Counties that do not grow sunflowers ### 
  
  Future_State_data_370 <- Future_State_data_370 %>% dplyr::rename(State = STATE,
                                                            County = COUNTY) %>% 
                             dplyr::inner_join(Ag_prac_data, by = c("State","County")) %>%
                             dplyr::inner_join(Drought_data, by = c("State","County"))
  
  
  
  ### now filtering out by important variables as deemed by RFE ##
  
  State_imp_370 <- Future_State_data_370 %>% 
    dplyr::select(Imp_variables$Features)
  
  
  ### create a prediction file for State ### 
  
  ### 
  seed
  predictions_370 <- predict(Model,State_imp_370) 
  
  
  ### creating a reference Dataframe ###
  ## we will add the predictions to this df ### 
  
  Reference <- Future_State_data_370 %>% dplyr::select(LAT, LON, County) 
  
  ### Adding the predictions to the Reference dataset ## 
  
  Predictions_370_reference <- cbind(predictions_370,Reference) %>% 
                                   dplyr::rename(Predictions = predictions_370)
  
  ### Adding the Error data ###
  
  #Predictions_370_reference <- dplyr::inner_join(Predictions_370_reference,Error_data)  
  

  ### Add the predictions to the shapefile ### 
  
  Predictions_sf_370 <- dplyr::inner_join(State_sf,Predictions_370_reference)
  
  #### Plot the predictions ### 
  
  State_370_map <- Predictions_sf_370 %>% 
    ggplot(aes(fill=Predictions_370,
               text = str_c(County, ": ", Predictions_370))) +
    geom_sf(colour = NA)  +
    scale_fill_viridis("Yield predictions (LB/Acre)",
                       direction = -1) + 
    theme_void()
  
  
  ############
  ### 585 ###
  
  ### Joining the ag_prac data to the Future Climate data for the state 
  ## Doing this eliminates the possibillity of including counties for which 
  ## the model wasn't trained on 
  ### Example: Counties that do not grow sunflowers ### 
  
  Future_State_data_585 <- Future_State_data_585 %>% dplyr::rename(State = STATE,
                                                            County = COUNTY) %>% 
                           dplyr::inner_join(Ag_prac_data, by = c("State","County")) %>%
                           dplyr::inner_join(Drought_data, by = c("State","County"))
  
  
  
  ### now filtering out by important variables as deemed by RFE ##
  
  State_imp_585 <- Future_State_data_585 %>% 
    dplyr::select(Imp_variables$Features)
  
  
  ### create a prediction file for State ### 
  
  ### 
  seed
  predictions_585 <- predict(Model,State_imp_585) 
  
  
  ### creating a reference Dataframe ###
  ## we will add the predictions to this df ### 
  
  Reference <- Future_State_data_585 %>% dplyr::select(LAT, LON, County) 
  
  ### Adding the predictions to the Reference dataset ## 
  
  Predictions_585_reference <- cbind(predictions_585,Reference) %>% 
    dplyr::rename(Predictions = predictions_585)
  
  
  #Predictions_585_reference <- dplyr::inner_join(Predictions_585_reference,Error_data)  
  ### Add the predictions to the shapefile ### 
  
  Predictions_sf_585 <- dplyr::inner_join(State_sf,Predictions_585_reference)
  
  #### Plot the predictions ### 
  
  State_585_map <- Predictions_sf_585 %>% 
    ggplot(aes(fill=Predictions_585,
               text = str_c(County, ": ", Predictions_585))) +
    geom_sf(colour = NA)  +
    scale_fill_viridis("Yield predictions (LB/Acre)",
                       direction = -1) +
    theme_void()
  
  
  State_results <- list("State_126_map" = State_126_map,
                     "State_245_map" = State_245_map,
                     "State_370_map" = State_370_map,
                     "State_585_map" = State_585_map,
                     "Prediction_126" = Predictions_sf_126,
                     "Prediction_245" = Predictions_sf_245,
                     "Prediction_370" = Predictions_sf_370,
                     "Prediction_585" = Predictions_sf_585)

  
  return(State_results)
  
}

## CREATING A FUNCTION THAT CREATES FUTURE PREDICTION MAPS FOR ONE TIMEFRAME ##
## ALL TIME FRAMES #####
########## writing a function that creates all plots for the state of ND 
## for all timeframes across 4 RCPS ###


Future_predictions_all_timeframes <- function(Future_State_data_2040_126,
                                                    Future_State_data_2040_245,
                                                    Future_State_data_2040_370,
                                                    Future_State_data_2040_585,
                                                    Future_State_data_2060_126,
                                                    Future_State_data_2060_245,
                                                    Future_State_data_2060_370,
                                                    Future_State_data_2060_585,
                                                    Future_State_data_2080_126,
                                                    Future_State_data_2080_245,
                                                    Future_State_data_2080_370,
                                                    Future_State_data_2080_585,
                                                    Ag_prac_data,
                                                    Drought_data,
                                                    Imp_variables,
                                                    Model,
                                                    State_sf,
                                                    seed) {
  
  
  ## TIMEFRAME ####
  ###########
  ### 2040 ####
  
  Prediction_2040 <- Future_predictions(Future_State_data_2040_126,
                                        Future_State_data_2040_245,
                                        Future_State_data_2040_370,
                                        Future_State_data_2040_585,
                                        Ag_prac_data,
                                        Drought_data,
                                        Imp_variables,
                                        Model,
                                        State_sf,
                                        seed)
  
  
  
  ## TIMEFRAME ####
  ###########
  ### 2060 ####
  
  Prediction_2060 <- Future_predictions(Future_State_data_2060_126,
                                        Future_State_data_2060_245,
                                        Future_State_data_2060_370,
                                        Future_State_data_2060_585,
                                        Ag_prac_data,
                                        Drought_data,
                                        Imp_variables,
                                        Model,
                                        State_sf,
                                        seed)
  
  
  
  ## TIMEFRAME ####
  ###########
  ### 2080 ####
  
  Prediction_2080 <- Future_predictions(Future_State_data_2060_126,
                                        Future_State_data_2060_245,
                                        Future_State_data_2060_370,
                                        Future_State_data_2060_585,
                                        Ag_prac_data,
                                        Drought_data,
                                        Imp_variables,
                                        Model,
                                        State_sf,
                                        seed)
  
  
  
  #### extracting all the plots as separate elements ###
  #### First 2040 ###
  
  Prediction_2040_126 <- Prediction_2040[["Prediction_126"]] 
  
  Prediction_2040_126 <- Prediction_2040_126 %>% 
                                          mutate(RCP = "126",
                                                 Timeframe = "2021-2040")
  
  Prediction_2040_245 <- Prediction_2040[["Prediction_245"]]  
  
  Prediction_2040_245 <- Prediction_2040_245 %>% 
                                           mutate(RCP = "245",
                                                  Timeframe = "2021-2040")
  
  Prediction_2040_370 <- Prediction_2040[["Prediction_370"]] 
  
  Prediction_2040_370 <- Prediction_2040_370 %>% 
                                             mutate(RCP = "370",
                                                    Timeframe = "2021-2040")
  
   
  Prediction_2040_585 <- Prediction_2040[["Prediction_585"]]    
  
  Prediction_2040_585 <- Prediction_2040_585 %>% 
                                                mutate(RCP = "585",
                                                       Timeframe = "2021-2040")
  
  ### Now 2060 ####
  Prediction_2060_126 <- Prediction_2060[["Prediction_126"]] 
  
  Prediction_2060_126 <- Prediction_2060_126 %>% 
                                             dplyr::mutate(RCP = "126",
                                                    Timeframe = "2041-2060")
  
  Prediction_2060_245 <- Prediction_2060[["Prediction_245"]]  
  
  Prediction_2060_245 <- Prediction_2060_245 %>% 
                                              dplyr:: mutate(RCP = "245",
                                                      Timeframe = "2041-2060")
  
  Prediction_2060_370 <- Prediction_2060[["Prediction_370"]]
  
  Prediction_2060_370 <- Prediction_2060_370 %>% 
                                                dplyr::mutate(RCP = "370",
                                                       Timeframe = "2041-2060")
  
  
  
  Prediction_2060_585 <- Prediction_2060[["Prediction_585"]]   
  
  Prediction_2060_585 <- Prediction_2060_585 %>% 
                                            mutate(RCP = "585",
                                                   Timeframe = "2041-2060")
  
  
  
  ### Now 2080 ###
  
  Prediction_2080_126 <- Prediction_2080[["Prediction_126"]] 
  
  Prediction_2080_126 <- Prediction_2080_126 %>% 
                                              mutate(RCP = "126",
                                                     Timeframe = "2061-2080")
  
  
  Prediction_2080_245 <- Prediction_2080[["Prediction_245"]]  
  
  Prediction_2080_245 <- Prediction_2080_245 %>% 
                                             mutate(RCP = "245",
                                                    Timeframe = "2061-2080")
  
  
  Prediction_2080_370 <- Prediction_2080[["Prediction_370"]]
  
  Prediction_2080_370 <- Prediction_2080_370 %>% 
                                             mutate(RCP = "370",
                                                    Timeframe = "2061-2080")
  
  Prediction_2080_585 <- Prediction_2080[["Prediction_585"]]    
  
  Prediction_2080_585 <- Prediction_2080_585 %>% 
                                              mutate(RCP = "585",
                                                     Timeframe = "2061-2080")
  
  Predictions <- rbind(Prediction_2040_126,Prediction_2040_245,
                      Prediction_2040_370,Prediction_2040_585,
                      Prediction_2060_126,Prediction_2060_245,
                      Prediction_2060_370,Prediction_2060_585,
                      Prediction_2080_126,Prediction_2080_245,
                      Prediction_2080_370,Prediction_2080_585)
  
  
  return(Predictions)
}



######## 
### FUNCTION THAT COMPUTES ALE VALUES FROM TRAINING AND TEST ###
################ 

ALE_values <- function(predictors_train,
                       predictors_test,
                       response_train,
                       response_test,
                       model) { 
  
  Ale_train <- Predictor$new(model, 
                             data = predictors_train,
                             y = response_train) 
  
  Ale_test <- Predictor$new(model, 
                            data = predictors_test,
                            y = response_test) 
  
  
  ## computing the local effects on the training dataset
  ALE_train_effects <- FeatureEffects$new(Ale_train,
                                          method = "ale")
  
  
  ## computing the local effects on the test dataset
  ALE_test_effects <- FeatureEffects$new(Ale_test,
                                         method = "ale")
  
  
  ## extracting the local effects from the training dataset
  ALE_train_values <- ALE_train_effects[["results"]]
  
  
  ## extracting the local effects from the test dataset
  ALE_test_values <- ALE_test_effects[["results"]]
  
  
  
  ## put all the effects in one dataframe ### train
  ALE_train_values <- do.call(rbind,ALE_train_values) 
  
  ALE_train_values <- ALE_train_values %>% mutate(Dataset = "Train")
  
  
  ## put all the effects in one dataframe ### test
  ALE_test_values <- do.call(rbind,ALE_test_values) 
  
  ALE_test_values <- ALE_test_values %>% mutate(Dataset = "Test")
  
  
  ALE <- rbind(ALE_train_values,ALE_test_values) 
  
  return(ALE)
  
}

















