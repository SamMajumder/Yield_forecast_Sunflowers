

rm(list = ls())

packages <- list("here","tidyverse","sf",
                 "ggplot2","ggrepel","viridis",
                 "plotly")

lapply(packages, require,character.only = T)


###

source(here::here("Code","00_Functions.R"))


##### TIME FRAME ### 2021-2040
####################
### YEAR 2040 #####
##########################

#######################
### 126 ###  

### LOAD DATASETS ####

Future_Data_2040_126 <- read.csv(here::here("Processed_Datasets",
                                      "Future_Data_2040_126.csv"))  

Future_Data_2060_126 <- read.csv(here::here("Processed_Datasets",
                                      "Future_Data_2060_126.csv"))  

Future_Data_2080_126 <- read.csv(here::here("Processed_Datasets",
                                      "Future_Data_2080_126.csv"))  



### 245 ###

Future_Data_2040_245 <- read.csv(here::here("Processed_Datasets",
                                      "Future_Data_2040_245.csv")) 


Future_Data_2060_245 <- read.csv(here::here("Processed_Datasets",
                                      "Future_Data_2060_245.csv"))  

Future_Data_2080_245 <- read.csv(here::here("Processed_Datasets",
                                      "Future_Data_2080_245.csv")) 



### 370 ####

Future_Data_2040_370 <- read.csv(here::here("Processed_Datasets",
                                      "Future_Data_2040_370.csv"))  


Future_Data_2060_370 <- read.csv(here::here("Processed_Datasets",
                                      "Future_Data_2060_370.csv"))  

Future_Data_2080_370 <- read.csv(here::here("Processed_Datasets",
                                      "Future_Data_2080_370.csv"))  



### 585 ####

Future_Data_2040_585 <- read.csv(here::here("Processed_Datasets",
                                      "Future_Data_2040_585.csv")) 


Future_Data_2060_585 <- read.csv(here::here("Processed_Datasets",
                                      "Future_Data_2060_585.csv")) 

Future_Data_2080_585 <- read.csv(here::here("Processed_Datasets",
                                      "Future_Data_2080_585.csv")) 

##################
### read in the shapefile for all states ##
################


States <- st_read(here::here("Shape_files","c_08mr23",
                       "c_08mr23.shp")) %>% 
                  dplyr::rename(State = STATE,
                                County = COUNTYNAME) %>% 
                  dplyr::mutate(County = toupper(County))


##### Loading in the Agricultural practise data ###

Ag_prac_latest <- read_csv(here::here("Processed_Datasets",
                                     "Ag_practices_latest.csv"))


### Important variables ###

Imp_variables <- read_csv(here::here("Processed_Datasets",
                                     "Rfe_best_subset_imp_combined_yield.csv")) 


##############
### MINNESOTA ###
#################

###

source(here::here("Code","00_Functions.R"))

### loading the best model ### 

MN_model <- readRDS(here::here("Models","Rf_MN_model.rds"))

#### Loading in the important variables ###

Imp_variables_MN <- read_csv(here::here("Processed_Datasets",
                                        "Rfe_best_subset_imp_MN.csv"))

#### MN state sf ###

States_MN <- States %>% filter(State == "MN")


#### Filtering out the Ag prac data ## for MN

Ag_prac_latest_MN <- Ag_prac_latest %>% 
                                     filter(State == "MN")

#### Filtering out Future data for MN ### 2040

Future_Data_2040_126_MN <- Future_Data_2040_126 %>% 
                                               filter(STATE == "MN")

Future_Data_2040_245_MN <- Future_Data_2040_245 %>% 
                                               filter(STATE == "MN")

Future_Data_2040_370_MN <- Future_Data_2040_370 %>% 
                                                filter(STATE == "MN")

Future_Data_2040_585_MN <- Future_Data_2040_585 %>% 
                                                filter(STATE == "MN")

#### Filtering out Future data for MN ### 2060

Future_Data_2060_126_MN <- Future_Data_2060_126 %>% 
                                             filter(STATE == "MN")

Future_Data_2060_245_MN <- Future_Data_2060_245 %>% 
                                              filter(STATE == "MN")

Future_Data_2060_370_MN <- Future_Data_2060_370 %>% 
                                              filter(STATE == "MN")

Future_Data_2060_585_MN <- Future_Data_2060_585 %>% 
                                              filter(STATE == "MN")
#### Filtering out Future data for MN ### 2080

Future_Data_2080_126_MN <- Future_Data_2080_126 %>% 
                                                filter(STATE == "MN")

Future_Data_2080_245_MN <- Future_Data_2080_245 %>% 
                                                filter(STATE == "MN")

Future_Data_2080_370_MN <- Future_Data_2080_370 %>% 
                                                 filter(STATE == "MN")

Future_Data_2080_585_MN <- Future_Data_2080_585 %>% 
                                                  filter(STATE == "MN")

seed <- set.seed(1234)

######

Future_predictions_MN <- Future_predictions_all_timeframes(Future_State_data_2040_126=Future_Data_2040_126_MN,
                                                        Future_State_data_2040_245=Future_Data_2040_245_MN,
                                                        Future_State_data_2040_370=Future_Data_2040_370_MN,
                                                        Future_State_data_2040_585=Future_Data_2040_585_MN,
                                                        Future_State_data_2060_126=Future_Data_2060_126_MN,
                                                        Future_State_data_2060_245=Future_Data_2060_245_MN,
                                                        Future_State_data_2060_370=Future_Data_2060_370_MN,
                                                        Future_State_data_2060_585=Future_Data_2060_585_MN,
                                                        Future_State_data_2080_126=Future_Data_2080_126_MN,
                                                        Future_State_data_2080_245=Future_Data_2080_245_MN,
                                                        Future_State_data_2080_370=Future_Data_2080_370_MN,
                                                        Future_State_data_2080_585=Future_Data_2080_585_MN,
                                                        Ag_prac_data=Ag_prac_latest_MN,
                                                        Imp_variables=Imp_variables_MN,
                                                        Model=MN_model,
                                                        State_sf=States_MN,
                                                        seed=seed)


Future_predictions_MN <- Future_predictions_MN %>% 
                                         mutate(Model = "Minnesota")




##############
### COLORADO ###
#################

###

source(here::here("Code","00_Functions.R"))

### loading the best model ### 

CO_model <- readRDS(here::here("Models","Rf_CO_model.rds"))

#### Loading in the important variables ###

Imp_variables_CO <- read_csv(here::here("Processed_Datasets",
                                        "Rfe_best_subset_imp_CO.csv"))

#### CO state sf ###

States_CO <- States %>% filter(State == "CO")

#### Filtering out the Ag prac data ## for CO

Ag_prac_latest_CO <- Ag_prac_latest %>% 
                                 filter(State == "CO")



#### Filtering out Future data for MN ### 2040

Future_Data_2040_126_CO <- Future_Data_2040_126 %>% 
  filter(STATE == "CO")

Future_Data_2040_245_CO <- Future_Data_2040_245 %>% 
  filter(STATE == "CO")

Future_Data_2040_370_CO <- Future_Data_2040_370 %>% 
  filter(STATE == "CO")

Future_Data_2040_585_CO <- Future_Data_2040_585 %>% 
  filter(STATE == "CO")

#### Filtering out Future data for MN ### 2060

Future_Data_2060_126_CO <- Future_Data_2060_126 %>% 
  filter(STATE == "CO")

Future_Data_2060_245_CO <- Future_Data_2060_245 %>% 
  filter(STATE == "CO")

Future_Data_2060_370_CO <- Future_Data_2060_370 %>% 
  filter(STATE == "CO")

Future_Data_2060_585_CO <- Future_Data_2060_585 %>% 
  filter(STATE == "CO")
#### Filtering out Future data for MN ### 2080

Future_Data_2080_126_CO <- Future_Data_2080_126 %>% 
  filter(STATE == "CO")

Future_Data_2080_245_CO <- Future_Data_2080_245 %>% 
  filter(STATE == "CO")

Future_Data_2080_370_CO <- Future_Data_2080_370 %>% 
  filter(STATE == "CO")

Future_Data_2080_585_CO <- Future_Data_2080_585 %>% 
  filter(STATE == "CO")


seed <- set.seed(1234)

######

Future_predictions_CO <- Future_predictions_all_timeframes(Future_State_data_2040_126=Future_Data_2040_126_CO,
                                                           Future_State_data_2040_245=Future_Data_2040_245_CO,
                                                           Future_State_data_2040_370=Future_Data_2040_370_CO,
                                                           Future_State_data_2040_585=Future_Data_2040_585_CO,
                                                           Future_State_data_2060_126=Future_Data_2060_126_CO,
                                                           Future_State_data_2060_245=Future_Data_2060_245_CO,
                                                           Future_State_data_2060_370=Future_Data_2060_370_CO,
                                                           Future_State_data_2060_585=Future_Data_2060_585_CO,
                                                           Future_State_data_2080_126=Future_Data_2080_126_CO,
                                                           Future_State_data_2080_245=Future_Data_2080_245_CO,
                                                           Future_State_data_2080_370=Future_Data_2080_370_CO,
                                                           Future_State_data_2080_585=Future_Data_2080_585_CO,
                                                           Ag_prac_data=Ag_prac_latest_CO,
                                                           Imp_variables=Imp_variables_CO,
                                                           Model=CO_model,
                                                           State_sf=States_CO,
                                                           seed=seed)


Future_predictions_CO <- Future_predictions_CO %>% 
                                           mutate(Model = "Colorado")


#################
### NEBRASKA #### 
############### 

###

source(here::here("Code","00_Functions.R"))

### loading the best model ### 

NE_model <- readRDS(here::here("Models","GBM_NE_model.rds"))

#### Loading in the important variables ###

Imp_variables_NE <- read_csv(here::here("Processed_Datasets",
                                        "Rfe_best_subset_imp_NE.csv"))

#### MN state sf ###

States_NE <- States %>% filter(State == "NE")

#### Filtering out the Ag prac data ## for NE

Ag_prac_latest_NE <- Ag_prac_latest %>% 
  filter(State == "NE")



#### Filtering out Future data for NE ### 2040

Future_Data_2040_126_NE <- Future_Data_2040_126 %>% 
  filter(STATE == "NE")

Future_Data_2040_245_NE <- Future_Data_2040_245 %>% 
  filter(STATE == "NE")

Future_Data_2040_370_NE <- Future_Data_2040_370 %>% 
  filter(STATE == "NE")

Future_Data_2040_585_NE <- Future_Data_2040_585 %>% 
  filter(STATE == "NE")

#### Filtering out Future data for MN ### 2060

Future_Data_2060_126_NE <- Future_Data_2060_126 %>% 
  filter(STATE == "NE")

Future_Data_2060_245_NE <- Future_Data_2060_245 %>% 
  filter(STATE == "NE")

Future_Data_2060_370_NE <- Future_Data_2060_370 %>% 
  filter(STATE == "NE")

Future_Data_2060_585_NE <- Future_Data_2060_585 %>% 
  filter(STATE == "NE")
#### Filtering out Future data for MN ### 2080

Future_Data_2080_126_NE <- Future_Data_2080_126 %>% 
  filter(STATE == "NE")

Future_Data_2080_245_NE <- Future_Data_2080_245 %>% 
  filter(STATE == "NE")

Future_Data_2080_370_NE <- Future_Data_2080_370 %>% 
  filter(STATE == "NE")

Future_Data_2080_585_NE <- Future_Data_2080_585 %>% 
  filter(STATE == "NE")


seed <- set.seed(1234)

######

Future_predictions_NE <- Future_predictions_all_timeframes(Future_State_data_2040_126=Future_Data_2040_126_NE,
                                                           Future_State_data_2040_245=Future_Data_2040_245_NE,
                                                           Future_State_data_2040_370=Future_Data_2040_370_NE,
                                                           Future_State_data_2040_585=Future_Data_2040_585_NE,
                                                           Future_State_data_2060_126=Future_Data_2060_126_NE,
                                                           Future_State_data_2060_245=Future_Data_2060_245_NE,
                                                           Future_State_data_2060_370=Future_Data_2060_370_NE,
                                                           Future_State_data_2060_585=Future_Data_2060_585_NE,
                                                           Future_State_data_2080_126=Future_Data_2080_126_NE,
                                                           Future_State_data_2080_245=Future_Data_2080_245_NE,
                                                           Future_State_data_2080_370=Future_Data_2080_370_NE,
                                                           Future_State_data_2080_585=Future_Data_2080_585_NE,
                                                           Ag_prac_data=Ag_prac_latest_NE,
                                                           Imp_variables=Imp_variables_NE,
                                                           Model=NE_model,
                                                           State_sf=States_NE,
                                                           seed=seed)



Future_predictions_NE <- Future_predictions_NE %>% 
                                              mutate(Model = "Nebraska")




################
### KANSAS ###
################# 

### loading the best model ### 

KS_model <- readRDS(here::here("Models","Rf_KS_model.rds"))

#### Loading in the important variables ###

Imp_variables_KS <- read_csv(here::here("Processed_Datasets",
                                        "Rfe_best_subset_imp_KS.csv"))

#### KS state sf ###

States_KS <- States %>% filter(State == "KS")

#### Filtering out the Ag prac data ## for KS

Ag_prac_latest_KS <- Ag_prac_latest %>% 
  filter(State == "KS")



#### Filtering out Future data for NE ### 2040

Future_Data_2040_126_KS <- Future_Data_2040_126 %>% 
  filter(STATE == "KS")

Future_Data_2040_245_KS <- Future_Data_2040_245 %>% 
  filter(STATE == "KS")

Future_Data_2040_370_KS <- Future_Data_2040_370 %>% 
  filter(STATE == "KS")

Future_Data_2040_585_KS <- Future_Data_2040_585 %>% 
  filter(STATE == "KS")

#### Filtering out Future data for KS ### 2060

Future_Data_2060_126_KS <- Future_Data_2060_126 %>% 
  filter(STATE == "KS")

Future_Data_2060_245_KS <- Future_Data_2060_245 %>% 
  filter(STATE == "KS")

Future_Data_2060_370_KS <- Future_Data_2060_370 %>% 
  filter(STATE == "KS")

Future_Data_2060_585_KS <- Future_Data_2060_585 %>% 
  filter(STATE == "KS")

#### Filtering out Future data for KS ### 2080

Future_Data_2080_126_KS <- Future_Data_2080_126 %>% 
  filter(STATE == "KS")

Future_Data_2080_245_KS <- Future_Data_2080_245 %>% 
  filter(STATE == "KS")

Future_Data_2080_370_KS <- Future_Data_2080_370 %>% 
  filter(STATE == "KS")

Future_Data_2080_585_KS <- Future_Data_2080_585 %>% 
  filter(STATE == "KS")


seed <- set.seed(1234)

######

Future_predictions_KS <- Future_predictions_all_timeframes(Future_State_data_2040_126=Future_Data_2040_126_KS,
                                                           Future_State_data_2040_245=Future_Data_2040_245_KS,
                                                           Future_State_data_2040_370=Future_Data_2040_370_KS,
                                                           Future_State_data_2040_585=Future_Data_2040_585_KS,
                                                           Future_State_data_2060_126=Future_Data_2060_126_KS,
                                                           Future_State_data_2060_245=Future_Data_2060_245_KS,
                                                           Future_State_data_2060_370=Future_Data_2060_370_KS,
                                                           Future_State_data_2060_585=Future_Data_2060_585_KS,
                                                           Future_State_data_2080_126=Future_Data_2080_126_KS,
                                                           Future_State_data_2080_245=Future_Data_2080_245_KS,
                                                           Future_State_data_2080_370=Future_Data_2080_370_KS,
                                                           Future_State_data_2080_585=Future_Data_2080_585_KS,
                                                           Ag_prac_data=Ag_prac_latest_KS,
                                                           Imp_variables=Imp_variables_KS,
                                                           Model=KS_model,
                                                           State_sf=States_KS,
                                                           seed=seed)




Future_predictions_KS <- Future_predictions_KS %>% 
                                            mutate(Model = "Kansas")





################
### TEXAS ###
#################

### loading the best model ### 

TX_model <- readRDS(here::here("Models","GBM_TX_model.rds"))

#### Loading in the important variables ###

Imp_variables_TX <- read_csv(here::here("Processed_Datasets",
                                        "Rfe_best_subset_imp_TX.csv"))

#### TX state sf ###

States_TX <- States %>% filter(State == "TX")


#### Filtering out the Ag prac data ## for KS

Ag_prac_latest_TX <- Ag_prac_latest %>% 
  filter(State == "TX")



#### Filtering out Future data for TX ### 2040

Future_Data_2040_126_TX <- Future_Data_2040_126 %>% 
  filter(STATE == "TX")

Future_Data_2040_245_TX <- Future_Data_2040_245 %>% 
  filter(STATE == "TX")

Future_Data_2040_370_TX <- Future_Data_2040_370 %>% 
  filter(STATE == "TX")

Future_Data_2040_585_TX <- Future_Data_2040_585 %>% 
  filter(STATE == "TX")

#### Filtering out Future data for KS ### 2060

Future_Data_2060_126_TX <- Future_Data_2060_126 %>% 
  filter(STATE == "TX")

Future_Data_2060_245_TX <- Future_Data_2060_245 %>% 
  filter(STATE == "TX")

Future_Data_2060_370_TX <- Future_Data_2060_370 %>% 
  filter(STATE == "TX")


Future_Data_2060_585_TX <- Future_Data_2060_585 %>% 
  filter(STATE == "TX")

#### Filtering out Future data for TX ### 2080

Future_Data_2080_126_TX <- Future_Data_2080_126 %>% 
  filter(STATE == "TX")

Future_Data_2080_245_TX <- Future_Data_2080_245 %>% 
  filter(STATE == "TX")

Future_Data_2080_370_TX <- Future_Data_2080_370 %>% 
  filter(STATE == "TX")

Future_Data_2080_585_TX <- Future_Data_2080_585 %>% 
  filter(STATE == "TX")


seed <- set.seed(1234)

######

Future_predictions_TX <- Future_predictions_all_timeframes(Future_State_data_2040_126=Future_Data_2040_126_TX,
                                                           Future_State_data_2040_245=Future_Data_2040_245_TX,
                                                           Future_State_data_2040_370=Future_Data_2040_370_TX,
                                                           Future_State_data_2040_585=Future_Data_2040_585_TX,
                                                           Future_State_data_2060_126=Future_Data_2060_126_TX,
                                                           Future_State_data_2060_245=Future_Data_2060_245_TX,
                                                           Future_State_data_2060_370=Future_Data_2060_370_TX,
                                                           Future_State_data_2060_585=Future_Data_2060_585_TX,
                                                           Future_State_data_2080_126=Future_Data_2080_126_TX,
                                                           Future_State_data_2080_245=Future_Data_2080_245_TX,
                                                           Future_State_data_2080_370=Future_Data_2080_370_TX,
                                                           Future_State_data_2080_585=Future_Data_2080_585_TX,
                                                           Ag_prac_data=Ag_prac_latest_TX,
                                                           Imp_variables=Imp_variables_TX,
                                                           Model=TX_model,
                                                           State_sf=States_TX,
                                                           seed=seed)




Future_predictions_TX <- Future_predictions_TX %>% 
                                           mutate(Model = "Texas")



########################
### COMBINED ####
########################  

### Model ###

Model <- readRDS(here::here("Models",
                            "Rf_national_model.rds"))


#### setting seed ##

seed <- set.seed(1234)


######

Future_predictions <- Future_predictions_all_timeframes(Future_State_data_2040_126=Future_Data_2040_126,
                                                        Future_State_data_2040_245=Future_Data_2040_245,
                                                        Future_State_data_2040_370=Future_Data_2040_370,
                                                        Future_State_data_2040_585=Future_Data_2040_585,
                                                        Future_State_data_2060_126=Future_Data_2060_126,
                                                        Future_State_data_2060_245=Future_Data_2060_245,
                                                        Future_State_data_2060_370=Future_Data_2060_370,
                                                        Future_State_data_2060_585=Future_Data_2060_585,
                                                        Future_State_data_2080_126=Future_Data_2080_126,
                                                        Future_State_data_2080_245=Future_Data_2080_245,
                                                        Future_State_data_2080_370=Future_Data_2080_370,
                                                        Future_State_data_2080_585=Future_Data_2080_585,
                                                        Ag_prac_data=Ag_prac_latest,
                                                        Imp_variables=Imp_variables,
                                                        Model=Model,
                                                        State_sf=States,
                                                        seed=seed)




Future_predictions <- Future_predictions %>% 
                                             mutate(Model = "National")


####################
##### Create a dataframe containing predictions for each region
################ 

### First extracting prediction values for North and South Dakota from the main combined dataset###


ND_SD <- c("ND","SD")

ND_SD_future_predictions <- Future_predictions %>% 
                                                  filter(State %in% ND_SD)





### Then adding ND and SD predictions to predictions from MN,CO,TX,NE,KS 

Predictions_future <- rbind(ND_SD_future_predictions,Future_predictions_CO,
                            Future_predictions_MN,Future_predictions_KS,
                            Future_predictions_NE,Future_predictions_TX) 


#### importing the average error dataframe ####

Average_errors <- read_csv(here::here("Processed_Datasets",
                                      "Average_test_errors.csv"))



####### 

Predictions_future <- inner_join(Predictions_future,Average_errors,
                                 by = c("FIPS","State","County"))



saveRDS(Predictions_future,
        "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/RDS_objects/Future_predictions.RDS")   












