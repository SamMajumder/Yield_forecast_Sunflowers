

rm(list = ls())

packages <- list("tidyverse","caret","CAST","here",
                 "randomForest","RColorBrewer")

lapply(packages, require,character.only=T)

###

source(here::here("Code","00_Functions.R"))

########## TEST DATA #######
################# loading in the datasets 

test <- read.csv(here::here("Processed_Datasets","test.csv"))

######################################
###### INSIGHTS FROM THE COMBINED DATASET ##
################################# 

## CREATE SPACE TIME FOLDS ###
## each year is a fold ####

set.seed(1234)
indices <- CreateSpacetimeFolds(test,spacevar = "SpaceVar", k=17)

### setting up the predictors ###

predictors_test <- test %>% dplyr::select(-c(Year,State,County,Yield,Longitude,
                                      Latitude,SpaceVar,Latest,Oldest,
                                      Crop_Loss))

### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:86)

set.seed(1234)

RFE_analysis <- RFE(predictors_test,
                    test$Yield,
                    subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_combined_yield_test.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_combined_yield_test.csv",
          row.names = FALSE) 







###########################
###### LOCAL MODELS ###
##### STATEWISE ####
####################  

######## NORTH DAKOTA ###

rm(list = ls()) 

###

source(here("Code","00_Functions.R"))


################# loading in the datasets 

test_ND <- read.csv(here("Processed_Datasets",
                         "test_ND.csv"))


####### 
## RECURSIVE FEATURE ELIMINATION ###
########### 

## CREATE SPACE TIME FOLDS ###

set.seed(1234)
indices <- CreateSpacetimeFolds(test_ND,spacevar = "SpaceVar", k=17)

### setting up the predictors ###

predictors_test_ND <- test_ND %>% select(-c(Year,State,County,Yield,Longitude,
                                            Latitude,SpaceVar,Latest,Oldest,
                                            Crop_Loss))

### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:86)

set.seed(1234)

RFE_analysis <- RFE(predictors_test_ND,test_ND$Yield,subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_test_ND.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_test_ND.csv",
          row.names = FALSE) 





###############################
######## SD ###########
###################

rm(list = ls())


###

source(here("Code","00_Functions.R"))


################# loading in the datasets 


test_SD <- read.csv(here("Processed_Datasets",
                         "test_SD.csv"))



####### 
## RECURSIVE FEATURE ELIMINATION ###
########### 

## CREATE SPACE TIME FOLDS ###

set.seed(1234)
indices <- CreateSpacetimeFolds(test_SD,spacevar = "SpaceVar", k=14)

### setting up the predictors ###


predictors_test_SD <- test_SD %>% select(-c(Year,State,County,Yield,Longitude,
                                            Latitude,SpaceVar,Latest,Oldest,
                                            Crop_Loss))

### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:86)

set.seed(1234)

RFE_analysis <- RFE(predictors_test_SD,test_SD$Yield,subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_test_SD.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_test_SD.csv",
          row.names = FALSE) 


#####################
## MINNESOTA ####
#######################

rm(list = ls())


###

source(here("Code","00_Functions.R"))


################# loading in the datasets 

test_MN <- read.csv(here("Processed_Datasets",
                         "test_MN.csv"))


####### 
## RECURSIVE FEATURE ELIMINATION ###
########### 

## CREATE SPACE TIME FOLDS ###

set.seed(1234)
indices <- CreateSpacetimeFolds(test_MN,spacevar = "SpaceVar", k=12)

### setting up the predictors ###


predictors_test_MN <- test_MN %>% select(-c(Year,State,County,Yield,Longitude,
                                            Latitude,SpaceVar,Latest,Oldest,
                                            Crop_Loss))

### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:86)

set.seed(1234)

RFE_analysis <- RFE(predictors_test_MN,test_MN$Yield,subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_test_MN.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_test_MN.csv",
          row.names = FALSE) 


#################
#### COLORADO ####
################# 

rm(list = ls())


###

source(here("Code","00_Functions.R"))


################# loading in the datasets 

test_CO <- read.csv(here("Processed_Datasets",
                         "test_CO.csv"))


####### 
## RECURSIVE FEATURE ELIMINATION ###
########### 

## CREATE SPACE TIME FOLDS ###

set.seed(1234)
indices <- CreateSpacetimeFolds(test_CO,spacevar = "SpaceVar", k=12)

### setting up the predictors ###


predictors_test_CO <- test_CO %>% select(-c(Year,State,County,Yield,Longitude,
                                            Latitude,SpaceVar,Latest,Oldest,
                                            Crop_Loss))

### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:86)

set.seed(1234)

RFE_analysis <- RFE(predictors_test_CO,test_CO$Yield,subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_test_CO.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_test_CO.csv",
          row.names = FALSE) 


###############
## KANSAS ####
################# 

rm(list = ls())


###

source(here("Code","00_Functions.R"))


################# loading in the datasets 


test_KS <- read.csv(here("Processed_Datasets",
                         "test_KS.csv"))



####### 
## RECURSIVE FEATURE ELIMINATION ###
########### 

## CREATE SPACE TIME FOLDS ###

set.seed(1234)
indices <- CreateSpacetimeFolds(test_KS,spacevar = "SpaceVar", k=10)

### setting up the predictors ###

predictors_test_KS <- test_KS %>% select(-c(Year,State,County,Yield,Longitude,
                                            Latitude,SpaceVar,Latest,Oldest,
                                            Crop_Loss))

### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:86)

set.seed(1234)

RFE_analysis <- RFE(predictors_test_KS,test_KS$Yield,subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_test_KS.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_test_KS.csv",
          row.names = FALSE) 



#####################
#### NEBRASKA ####
############# 


rm(list = ls())


###

source(here::here("Code","00_Functions.R"))


################# loading in the datasets 

test_NE <- read.csv(here::here("Processed_Datasets",
                               "test_NE.csv"))



####### 
## RECURSIVE FEATURE ELIMINATION ###
########### 

## CREATE SPACE TIME FOLDS ###

set.seed(1234)
indices <- CreateSpacetimeFolds(test_NE,spacevar = "SpaceVar", k=9)

### setting up the predictors ###

predictors_test_NE <- test_NE %>% select(-c(Year,State,County,Yield,Longitude,
                                            Latitude,SpaceVar,Latest,Oldest,
                                            Crop_Loss))
### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:86)

set.seed(1234)

RFE_analysis <- RFE(predictors_test_NE,test_NE$Yield,subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_test_NE.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_test_NE.csv",
          row.names = FALSE) 



#######################
### TEXAS ###########
################# 

rm(list = ls())


###

source(here("Code","00_Functions.R"))


################# loading in the datasets 

test_TX <- read.csv(here("Processed_Datasets",
                         "test_TX.csv"))



####### 
## RECURSIVE FEATURE ELIMINATION ###
########### 

## CREATE SPACE TIME FOLDS ###

set.seed(1234)
indices <- CreateSpacetimeFolds(test_TX,spacevar = "SpaceVar", k=12)

### setting up the predictors ###

predictors_test_TX <- test_TX %>% select(-c(Year,State,County,Yield,Longitude,
                                            Latitude,SpaceVar,Latest,Oldest,
                                            Crop_Loss))

### setting up parameters for RFE ###

params <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                     verbose = T,index=indices$index)


subsets <- c(1:86)

set.seed(1234)

RFE_analysis <- RFE(predictors_test_TX,test_TX$Yield,subsets,params)

### Table containing the importance of each variable in the best subset
Rfe_Imp_best_subset <- RFE_analysis[["optimal subset"]]


## 

here("Processed_Datasets")


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_best_subset,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_test_TX.csv",
          row.names = FALSE) 


##### Plotting the importance values ###

RFE_plot <- RFE_analysis[["Importance plot"]]

### Table containing best subset along with their importance values ##

Importance_values <- RFE_analysis[["Importance values"]] 

write.csv(Importance_values,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Rfe_best_subset_imp_test_TX.csv",
          row.names = FALSE) 


#############
### Creating plots for on the test dataset for both combined and state levels###
###############

### RFE IMPORTANCE FROM THE WHOLE (NATIONAL DATASET)

Var_Imp_Global_test <- read.csv(here("Processed_Datasets",
                                     "Rfe_best_subset_imp_combined_yield_test.csv")) %>% 
  mutate(Model_Type = "Global") %>% 
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))



### setting levels for variable type
##

Var_Imp_Global_test$`Variable Type` <- factor(Var_Imp_Global_test$`Variable Type`, 
                                              levels = c("Maximum Temperature",
                                                         "Precipitation",
                                                         "Minimum Temperature",
                                                         "Ag Practice",
                                                         "Drought"))


#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(12,'Paired')


brewer.pal(12, 'Paired') ## displaying the hex codes 





####### Create a variable importance plot ####

Imp_global_test <- ggplot(data = Var_Imp_Global_test,
                          aes(x=reorder(Features,Overall), y = Overall, 
                              fill = `Variable Type`)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C",
                               "#A6CEE3",
                               "#FDBF6F",
                               "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors", 
       y= "Variable Importance(RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 


########
##
######### 

### RFE IMPORTANCE FROM NORTH DAKOTA 

Var_Imp_ND_test <- read.csv(here("Processed_Datasets",
                                 "Rfe_best_subset_imp_test_ND.csv")) %>% 
  mutate(Model_Type = "North Dakota") %>% 
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))





### setting levels for variable type
##

Var_Imp_ND_test$`Variable Type` <- factor(Var_Imp_ND_test$`Variable Type`, 
                                          levels = c("Maximum Temperature",
                                                     "Precipitation",
                                                     "Minimum Temperature",
                                                     "Ag Practice",
                                                     "Drought"))


####### Create a variable importance plot ####

Imp_ND_test <- ggplot(data = Var_Imp_ND_test,
                      aes(x=reorder(Features,Overall), y = Overall, 
                          fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C",
                               "#A6CEE3",
                               "#FDBF6F",
                               "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors",
       y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 


######## RFE IMPORTANCE FROM SOUTH DAKOTA 

Var_Imp_SD_test <- read.csv(here("Processed_Datasets",
                            "Rfe_best_subset_imp_test_SD.csv")) %>% 
  mutate(Model_Type = "South Dakota") %>% 
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))


### setting levels for variable type
##

Var_Imp_SD_test$`Variable Type` <- factor(Var_Imp_SD_test$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice",
                                                "Drought"))


####### Create a variable importance plot ####

Imp_SD_test <- ggplot(data = Var_Imp_SD_test,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C",
                               "#A6CEE3",
                               "#FDBF6F",
                               "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors", 
       y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 

######## RFE IMPORTANCE FROM COLORADO 

Var_Imp_CO_test <- read.csv(here("Processed_Datasets",
                            "Rfe_best_subset_imp_test_CO.csv")) %>% 
  mutate(Model_Type = "Colorado") %>% 
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))


### setting levels for variable type
##

Var_Imp_CO_test$`Variable Type` <- factor(Var_Imp_CO_test$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice",
                                                "Drought"))



####### Create a variable importance plot ####

Imp_CO_test <- ggplot(data = Var_Imp_CO_test,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C",
                               "#A6CEE3",
                               "#FDBF6F",
                               "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors", y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 


#### RFE IMPORTANCE FROM KANSAS 

Var_Imp_KS_test <- read.csv(here("Processed_Datasets",
                            "Rfe_best_subset_imp_test_KS.csv")) %>% 
  mutate(Model_Type = "Kansas") %>%
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))


### setting levels for variable type
##

Var_Imp_KS_test$`Variable Type` <- factor(Var_Imp_KS_test$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice",
                                                "Drought"))




####### Create a variable importance plot ####

Imp_KS_test <- ggplot(data = Var_Imp_KS_test,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C",
                               "#A6CEE3",
                               "#FDBF6F",
                               "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors", y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 



###

#### RFE IMPORTANCE FROM NEBRASKA

Var_Imp_NE_test <- read.csv(here("Processed_Datasets",
                            "Rfe_best_subset_imp_test_NE.csv")) %>% 
  mutate(Model_Type = "Nebraska") %>% 
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))


### setting levels for variable type
##

Var_Imp_NE_test$`Variable Type` <- factor(Var_Imp_NE_test$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice",
                                                "Drought"))


####### Create a variable importance plot ####

Imp_NE_test <- ggplot(data = Var_Imp_NE_test,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C",
                               "#A6CEE3",
                               "#FDBF6F",
                               "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors", y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 







######## RFE IMPORTANCE FROM MINNESOTA             

Var_Imp_MN_test <- read.csv(here("Processed_Datasets",
                            "Rfe_best_subset_imp_test_MN.csv")) %>% 
  mutate(Model_Type = "Minnesota") %>% 
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))




### setting levels for variable type
##

Var_Imp_MN_test$`Variable Type` <- factor(Var_Imp_MN_test$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice",
                                                "Drought"))


####### Create a variable importance plot ####

Imp_MN_test <- ggplot(data = Var_Imp_MN_test,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C", "#A6CEE3",
                               "#FDBF6F", "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors", y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 



######## RFE IMPORTANCE FROM TEXAS             

Var_Imp_TX_test <- read.csv(here("Processed_Datasets",
                            "Rfe_best_subset_imp_test_TX.csv")) %>% 
  mutate(Model_Type = "Texas") %>% 
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))



### setting levels for variable type
##

Var_Imp_TX_test$`Variable Type` <- factor(Var_Imp_TX_test$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice",
                                                "Drought"))


####### Create a variable importance plot ####

Imp_TX_test <- ggplot(data = Var_Imp_TX_test,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C", "#A6CEE3",
                               "#FDBF6F", "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors", y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 



############ Combine all of the data into dataframe ###

Var_IMP_test <- list(Var_Imp_Global_test,Var_Imp_CO_test,Var_Imp_KS_test,
                     Var_Imp_MN_test,Var_Imp_ND_test,Var_Imp_NE_test,
                     Var_Imp_SD_test,Var_Imp_TX_test) %>% reduce(rbind) 



### adding a column called dataset to the table ###

Var_IMP_test <- Var_IMP_test %>% 
                              mutate(Dataset = "Test")



####################
#### TRAINING DATA ###
############# 

### RFE IMPORTANCE FROM THE WHOLE (GLOBAL DATASET)

Var_Imp_Global <- read.csv(here("Processed_Datasets",
                                "Rfe_best_subset_imp_combined_yield.csv")) %>% 
  mutate(Model_Type = "Global") %>% 
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))



### setting levels for variable type
##

Var_Imp_Global$`Variable Type` <- factor(Var_Imp_Global$`Variable Type`, 
                                         levels = c("Maximum Temperature",
                                                    "Precipitation",
                                                    "Minimum Temperature",
                                                    "Ag Practice",
                                                    "Drought"))


#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(12,'Paired')


brewer.pal(12, 'Paired') ## displaying the hex codes 





####### Create a variable importance plot ####

Imp_global <- ggplot(data = Var_Imp_Global,
                     aes(x=reorder(Features,Overall), y = Overall, 
                         fill = `Variable Type`)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C",
                               "#A6CEE3",
                               "#FDBF6F",
                               "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors", 
       y= "Variable Importance(RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 

ggsave("Figure 2.svg")

### RFE IMPORTANCE FROM NORTH DAKOTA 

Var_Imp_ND <- read.csv(here("Processed_Datasets",
                            "Rfe_best_subset_imp_ND.csv")) %>% 
  mutate(Model_Type = "North Dakota") %>% 
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))





### setting levels for variable type
##

Var_Imp_ND$`Variable Type` <- factor(Var_Imp_ND$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice",
                                                "Drought"))


####### Create a variable importance plot ####

Imp_ND <- ggplot(data = Var_Imp_ND,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C",
                               "#A6CEE3",
                               "#FDBF6F",
                               "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors",
       y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 




######## RFE IMPORTANCE FROM SOUTH DAKOTA 

Var_Imp_SD <- read.csv(here("Processed_Datasets",
                            "Rfe_best_subset_imp_SD.csv")) %>% 
  mutate(Model_Type = "South Dakota") %>% 
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))


### setting levels for variable type
##

Var_Imp_SD$`Variable Type` <- factor(Var_Imp_SD$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice",
                                                "Drought"))


####### Create a variable importance plot ####

Imp_SD <- ggplot(data = Var_Imp_SD,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C",
                               "#A6CEE3",
                               "#FDBF6F",
                               "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors", 
       y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 






######## RFE IMPORTANCE FROM COLORADO 

Var_Imp_CO <- read.csv(here("Processed_Datasets",
                            "Rfe_best_subset_imp_CO.csv")) %>% 
  mutate(Model_Type = "Colorado") %>% 
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))


### setting levels for variable type
##

Var_Imp_CO$`Variable Type` <- factor(Var_Imp_CO$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice",
                                                "Drought"))



####### Create a variable importance plot ####

Imp_CO <- ggplot(data = Var_Imp_CO,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C",
                               "#A6CEE3",
                               "#FDBF6F",
                               "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors", y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 


#### RFE IMPORTANCE FROM KANSAS 

Var_Imp_KS <- read.csv(here("Processed_Datasets",
                            "Rfe_best_subset_imp_KS.csv")) %>% 
  mutate(Model_Type = "Kansas") %>%
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))


### setting levels for variable type
##

Var_Imp_KS$`Variable Type` <- factor(Var_Imp_KS$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice",
                                                "Drought"))




####### Create a variable importance plot ####

Imp_KS <- ggplot(data = Var_Imp_KS,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C",
                               "#A6CEE3",
                               "#FDBF6F",
                               "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors", y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 



###

#### RFE IMPORTANCE FROM NEBRASKA

Var_Imp_NE <- read.csv(here("Processed_Datasets",
                            "Rfe_best_subset_imp_NE.csv")) %>% 
  mutate(Model_Type = "Nebraska") %>% 
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))


### setting levels for variable type
##

Var_Imp_NE$`Variable Type` <- factor(Var_Imp_NE$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice",
                                                "Drought"))


####### Create a variable importance plot ####

Imp_NE <- ggplot(data = Var_Imp_NE,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C",
                               "#A6CEE3",
                               "#FDBF6F",
                               "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors", y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 







######## RFE IMPORTANCE FROM MINNESOTA             

Var_Imp_MN <- read.csv(here("Processed_Datasets",
                            "Rfe_best_subset_imp_MN.csv")) %>% 
  mutate(Model_Type = "Minnesota") %>% 
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))




### setting levels for variable type
##

Var_Imp_MN$`Variable Type` <- factor(Var_Imp_MN$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice",
                                                "Drought"))


####### Create a variable importance plot ####

Imp_MN <- ggplot(data = Var_Imp_MN,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C", "#A6CEE3",
                               "#FDBF6F", "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors", y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 



######## RFE IMPORTANCE FROM TEXAS             

Var_Imp_TX <- read.csv(here("Processed_Datasets",
                            "Rfe_best_subset_imp_TX.csv")) %>% 
  mutate(Model_Type = "Texas") %>% 
  ##### Setting variable type based on variable name
  mutate(`Variable Type` = case_when(str_detect(Features,
                                                "_Precip") ~ 
                                       "Precipitation",
                                     str_detect(Features,
                                                "_Tmin") ~ 
                                       "Minimum Temperature",
                                     str_detect(Features,
                                                "_Tmax") ~ 
                                       "Maximum Temperature",
                                     str_detect(Features,
                                                "Acres_") ~ 
                                       "Ag Practice",
                                     str_detect(Features,
                                                "_pearson") ~
                                       "Drought",
                                     str_detect(Features,
                                                "_gamma") ~
                                       "Drought")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))



### setting levels for variable type
##

Var_Imp_TX$`Variable Type` <- factor(Var_Imp_TX$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice",
                                                "Drought"))


####### Create a variable importance plot ####

Imp_TX <- ggplot(data = Var_Imp_TX,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#E31A1C", "#A6CEE3",
                               "#FDBF6F", "#B2DF8A",
                               "#FF7F00")) +
  labs(x= "Factors", 
       y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 



############ Combine all of the data into dataframe ###

Var_IMP <- list(Var_Imp_Global,Var_Imp_CO,Var_Imp_KS,Var_Imp_MN,Var_Imp_ND,
                Var_Imp_NE,Var_Imp_SD,Var_Imp_TX) %>% reduce(rbind) 


Var_IMP <- Var_IMP %>% mutate(Dataset = "Train")


###### Binding the two datasets ###

Var_IMP_total <- rbind(Var_IMP,Var_IMP_test)

#### Exporting the dataset as an rds object

here("RDS_objects")

saveRDS(Var_IMP_total,
        "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/RDS_objects/Var_IMP_total.RDS")








