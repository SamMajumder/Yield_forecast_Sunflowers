


#####################3 
#### MAKING A DATAFRAME FOR ALE PLOTS ### 
#############

###########################
### GLOBAL MODEL ##################
################################
#########################

rm(list = ls())

packages <- list("tidyverse","ggplot2","iml","here","plotly",
                 "RColorBrewer")

lapply(packages, require, character.only = T)


###
source(here::here("Code","00_Functions.R"))

### Load model ### 

Global_model <- readRDS(here::here("Models",
                            "Rf_national_model.rds"))

### Load dataset 

train <- read_csv(here::here("Processed_Datasets",
                       "train.csv"))
  
test <- read_csv(here::here("Processed_Datasets",
                     "test.csv"))   

### Load important variables ####

Var_Imp_Global <- read.csv(here::here("Processed_Datasets",
                                "Rfe_best_subset_imp_combined_yield.csv"))



### filtering to contain the optimal subset of features ###

train_reduced <- train %>% dplyr::select(Yield,Var_Imp_Global$Features)

test_reduced <- test %>% dplyr::select(Yield,Var_Imp_Global$Features)

####################### 
### 

predictors_train_reduced <- train_reduced %>% dplyr::select(-Yield)

predictors_test_reduced <- test_reduced %>% dplyr::select(-Yield)

################

##################
## GLOBAL ####

set.seed(1234)

ALE_Global <- ALE_values(predictors = predictors_train_reduced,
                         predictors_test = predictors_test_reduced,
           response_train = train_reduced$Yield,
           response_test = test_reduced$Yield,
           model = Global_model)


ALE_Global <- ALE_Global %>% 
                          mutate(`Model Type` = "National")


### dsiplaying all color blind friendly pallettes 

display.brewer.all(colorblindFriendly = TRUE)

### display the pallette

display.brewer.pal(n = 9, name = 'YlOrRd')

## display the hex code #

brewer.pal(n = 9, name = "YlOrRd")


brewer.pal(n = 12, name = "Paired")



#### Filtering out the ALE_Global dataset to contain some of the top variables ##

Top_four <- c("Jul_Tmax","Sep_Tmin","May_Tmin","Jul_Precip")


ALE_top_four <- ALE_Global %>% 
                       filter(.feature %in% Top_four)  %>% 
                mutate(.feature = str_replace_all(.feature,"_"," ")) %>%
             ##### Setting variable type based on variable name
            mutate(Dataset = case_when(str_detect(Dataset,
                                              "Train") ~ 
                                     "Train (Before 2005)",
                                   str_detect(Dataset,
                                              "Test") ~ 
                                     "Test (After 2005)"))


# New facet label names for supp variable
feature_labs <- c("Jul Precip (mm)", "Jul Tmax (Celsius)",
                  "May Tmin (Celsius)", "Sep Tmin (Celsius)")

names(feature_labs) <- c("Jul Precip", "Jul Tmax", 
                         "May Tmin","Sep Tmin")


#### ALE Plot showing the top 4 variables in training and test ###

ggplot(ALE_top_four, 
       aes(x = .borders, 
           y = .value)) + 
  geom_line(aes(color=.feature),
            size = 1) +
  facet_grid(Dataset ~ .feature,
             scales = "free",
             labeller = labeller(.feature = feature_labs))  +  
  scale_color_manual("Factors",
                    values = c("#1F78B4",
                               "#BD0026",
                               "#FEB24C",
                               "#FEB24C")) +
  labs(x = "Predictors", 
       y = "Accumulated Local Effects") +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme(legend.position = "none")


ggsave("Figure 3.svg")

################## 
### NORTH DAKOTA ###
################# 

### Load model ### 

ND_model <- readRDS(here::here("Models",
                         "Rf_ND_model.rds"))


### Load dataset 

train_ND <- read_csv(here::here("Processed_Datasets",
                       "train_ND.csv"))

test_ND <- read_csv(here::here("Processed_Datasets",
                     "test_ND.csv"))   

### Load important variables ####

Var_Imp_ND <- read.csv(here::here("Processed_Datasets",
                              "Rfe_best_subset_imp_ND.csv"))




### filtering to contain the optimal subset of features ###

train_reduced_ND <- train_ND %>% 
                              dplyr::select(Yield,Var_Imp_ND$Features)

test_reduced_ND <- test_ND %>% 
                           dplyr::select(Yield,Var_Imp_ND$Features)

####################### 
### 

predictors_train_reduced_ND <- train_reduced_ND %>% 
                                               dplyr::select(-Yield)

predictors_test_reduced_ND <- test_reduced_ND %>% 
                                              dplyr::select(-Yield)


########################
############

set.seed(1234)

ALE_ND <- ALE_values(predictors = predictors_train_reduced_ND,
                         predictors_test = predictors_test_reduced_ND,
                         response_train = train_reduced_ND$Yield,
                         response_test = test_reduced_ND$Yield,
                         model = ND_model)


ALE_ND <- ALE_ND %>% 
                mutate(`Model Type` = "North Dakota")



#### Filtering out the ALE_ND dataset to only contain the top 4 variables ##

Top_four <- c("Sep_Tmin","Jul_Tmax","Jul_Precip","Jul_Tmin")


ALE_ND_top_four <- ALE_ND %>% 
  filter(.feature %in% Top_four)  %>% 
  mutate(.feature = str_replace_all(.feature,"_"," ")) %>%
  ##### Setting variable type based on variable name
  mutate(Dataset = case_when(str_detect(Dataset,
                                        "Train") ~ 
                               "Train (Before 2005)",
                             str_detect(Dataset,
                                        "Test") ~ 
                               "Test (After 2005)"))

# New facet label names for supp variable
feature_labs <- c("Sep Tmin (Celsius)", "Jul Tmax (Celsius)",
                  "Jul Precip (mm)", "Jul Tmin (Celsius)")

names(feature_labs) <- c("Sep Tmin", "Jul Tmax", 
                         "Jul Precip","Jul Tmin")


#### ALE Plot showing the top 4 variables in training and test ###

ggplot(ALE_ND_top_four, 
       aes(x = .borders, 
           y = .value)) + 
  geom_line(aes(color=.feature),
            size = 1) +
  facet_grid(Dataset ~ .feature,
             scales = "free",
             labeller = labeller(.feature = feature_labs))  +  
  scale_color_manual("Factors",
                     values = c("#1F78B4",
                                "#BD0026",
                                "#FEB24C",
                                "#FEB24C")) +
  labs(x = "Predictors", 
       y = "Accumulated Local Effects") +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme(legend.position = "none")



ggsave("Figure S13.svg")




#####
## SOUTH DAKOTA ###
###### 

### Load model ### 

SD_model <- readRDS(here::here("Models",
                               "XGB_SD_model.rds"))

### Load dataset 

train_SD <- read_csv(here::here("Processed_Datasets",
                                "train_SD.csv"))

test_SD <- read_csv(here::here("Processed_Datasets",
                               "test_SD.csv"))   

### Load important variables ####

Var_Imp_SD <- read.csv(here::here("Processed_Datasets",
                                  "Rfe_best_subset_imp_SD.csv"))




### filtering to contain the optimal subset of features ###

train_reduced_SD <- train_SD %>% 
                            dplyr::select(Yield,
                                    Var_Imp_SD$Features)

test_reduced_SD <- test_SD %>% 
                             dplyr::select(Yield,
                                    Var_Imp_SD$Features)

####################### 
### 

predictors_train_reduced_SD <- train_reduced_SD %>% 
                                            dplyr::select(-Yield)

predictors_test_reduced_SD <- test_reduced_SD %>% 
                                          dplyr::select(-Yield)


########################
############

set.seed(1234)

ALE_SD <- ALE_values(predictors = predictors_train_reduced_SD,
                     predictors_test = predictors_test_reduced_SD,
                     response_train = train_reduced_SD$Yield,
                     response_test = test_reduced_SD$Yield,
                     model = SD_model)


ALE_SD <- ALE_SD %>% 
               mutate(`Model Type` = "South Dakota")


#### Filtering out the ALE_SD dataset to only contain some of the most important variables ##

Top_four <- c("Jul_Tmax","Oct_Tmin","Acres_Harvested","Acres_Planted")

########

ALE_SD_top_four <- ALE_SD %>% 
  filter(.feature %in% Top_four)  %>% 
  mutate(.feature = str_replace_all(.feature,"_"," ")) %>%
  ##### Setting variable type based on variable name
  mutate(Dataset = case_when(str_detect(Dataset,
                                        "Train") ~ 
                               "Train (Before 2005)",
                             str_detect(Dataset,
                                        "Test") ~ 
                               "Test (After 2005)"))

### dsiplaying all color blind friendly pallettes 

display.brewer.all(colorblindFriendly = TRUE)

### display the pallette

display.brewer.pal(n = 9, name = 'YlOrRd')

## display the hex code #

brewer.pal(n = 9, name = "YlOrRd")


brewer.pal(n = 12, name = "Paired")


# New facet label names for supp variable
feature_labs <- c("Jul Tmax (Celsius)", "Oct Tmin (Celsius)",
                  "Acres Harvested (acres)", "Acres Planted (acres)")

names(feature_labs) <- c("Jul Tmax", "Oct Tmin", 
                         "Acres Harvested","Acres Planted")



#### ALE Plot showing the top 4 variables in training and test ###

ggplot(ALE_SD_top_four, 
       aes(x = .borders, 
           y = .value)) + 
  geom_line(aes(color=.feature),
            size = 1) +
  facet_grid(Dataset ~ .feature,
             scales = "free",
             labeller = labeller(.feature = feature_labs))  +  
  scale_color_manual("Factors",
                     values = c("#A6D854",
                                "#A6D854",
                                "#BD0026",
                                "#FEB24C")) +
  labs(x = "Predictors", 
       y = "Accumulated Local Effects") +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme(legend.position = "none")



ggsave("Figure S8.svg")



#####
## COLORADO ###
###### 

### Load model ### 

CO_model <- readRDS(here::here("Models",
                               "Rf_CO_model.rds"))


### Load dataset 

train_CO <- read_csv(here::here("Processed_Datasets",
                                "train_CO.csv"))

test_CO <- read_csv(here::here("Processed_Datasets",
                               "test_CO.csv"))   

### Load important variables ####

Var_Imp_CO <- read.csv(here::here("Processed_Datasets",
                                  "Rfe_best_subset_imp_CO.csv"))




### filtering to contain the optimal subset of features ###

train_reduced_CO <- train_CO %>% 
                          dplyr::select(Yield,
                                        Var_Imp_CO$Features)

test_reduced_CO <- test_CO %>% 
                           dplyr::select(Yield,
                                         Var_Imp_CO$Features)

####################### 
### 

predictors_train_reduced_CO <- train_reduced_CO %>% 
                                          dplyr::select(-Yield)

predictors_test_reduced_CO <- test_reduced_CO %>% 
                                          dplyr::select(-Yield)


########################
############

set.seed(1234)

ALE_CO <- ALE_values(predictors = predictors_train_reduced_CO,
                     predictors_test = predictors_test_reduced_CO,
                     response_train = train_reduced_CO$Yield,
                     response_test = test_reduced_CO$Yield,
                     model = CO_model)



ALE_CO <- ALE_CO %>% 
                 mutate(`Model Type` = "Colorado")



### dsiplaying all color blind friendly pallettes 

display.brewer.all(colorblindFriendly = TRUE)

### display the pallette

display.brewer.pal(n = 9, name = 'YlOrRd')

## display the hex code #

brewer.pal(n = 9, name = "YlOrRd")


brewer.pal(n = 12, name = "Paired")



#### Filtering out the ALE_SD dataset to only contain the top 4 variables ##

Top_four <- c("Jul_Precip","Oct_Tmin","Oct_Tmax","Aug_Tmax")



ALE_CO_top_four <- ALE_CO %>% 
  filter(.feature %in% Top_four)  %>% 
  mutate(.feature = str_replace_all(.feature,"_"," ")) %>%
  ##### Setting variable type based on variable name
  mutate(Dataset = case_when(str_detect(Dataset,
                                        "Train") ~ 
                               "Train (Before 2005)",
                             str_detect(Dataset,
                                        "Test") ~ 
                               "Test (After 2005)"))

# New facet label names for supp variable
feature_labs <- c("Jul Precip (mm)", "Oct Tmin (Celsius)",
                  "Oct Tmax (Celsius)", "Aug Tmax (Celsisus)")

names(feature_labs) <- c("Jul Precip", "Oct Tmin", 
                         "Oct Tmax","Aug Tmax")

#### ALE Plot showing the top 4 variables in training and test ###

ggplot(ALE_CO_top_four, 
       aes(x = .borders, 
           y = .value)) + 
  geom_line(aes(color=.feature),
            size = 1) +
  facet_grid(Dataset ~ .feature,
             scales = "free",
             
             labeller = labeller(.feature = feature_labs))  +  
  scale_color_manual("Factors",
                     values = c("#BD0026",
                                "#1F78B4",
                                "#BD0026",
                                "#FEB24C"
                                )) +
  labs(x = "Predictors", 
       y = "Accumulated Local Effects") +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme(legend.position = "none")



ggsave("Figure S12.svg")



#####
## TEXAS ###
###### 

### Load model ### 

TX_model <- readRDS(here::here("Models",
                               "GBM_TX_model.rds"))


### Load dataset 

train_TX <- read_csv(here::here("Processed_Datasets",
                                "train_TX.csv"))

test_TX <- read_csv(here::here("Processed_Datasets",
                               "test_TX.csv"))   

### Load important variables ####

Var_Imp_TX <- read.csv(here::here("Processed_Datasets",
                                  "Rfe_best_subset_imp_TX.csv"))




### filtering to contain the optimal subset of features ###

train_reduced_TX <- train_TX %>% 
                                dplyr::select(Yield,
                                              Var_Imp_TX$Features)

test_reduced_TX <- test_TX %>% 
                             dplyr::select(Yield,
                                           Var_Imp_TX$Features)

####################### 
### 

predictors_train_reduced_TX <- train_reduced_TX %>% 
                                             dplyr::select(-Yield)

predictors_test_reduced_TX <- test_reduced_TX %>% 
                                             dplyr::select(-Yield)


########################
############

set.seed(1234)

ALE_TX <- ALE_values(predictors = predictors_train_reduced_TX,
                     predictors_test = predictors_test_reduced_TX,
                     response_train = train_reduced_TX$Yield,
                     response_test = test_reduced_TX$Yield,
                     model = TX_model)



ALE_TX <- ALE_TX %>% 
                   mutate(`Model Type` = "Texas")

#### Filtering out the ALE_SD dataset to only contain the top 4 variables ##

Top_four <- c("Jun_Tmax","Dec_Tmax","Jul_Tmax","Jun_Tmin")

### dsiplaying all color blind friendly pallettes 

display.brewer.all(colorblindFriendly = TRUE)

### display the pallette

display.brewer.pal(n = 9, name = 'YlOrRd')

## display the hex code #

brewer.pal(n = 9, name = "YlOrRd")


brewer.pal(n = 12, name = "Paired")


#################


ALE_TX_top_four <- ALE_TX %>% 
  filter(.feature %in% Top_four)  %>% 
  mutate(.feature = str_replace_all(.feature,"_"," ")) %>%
  ##### Setting variable type based on variable name
  mutate(Dataset = case_when(str_detect(Dataset,
                                        "Train") ~ 
                               "Train (Before 2005)",
                             str_detect(Dataset,
                                        "Test") ~ 
                               "Test (After 2005)"))

# New facet label names for supp variable
feature_labs <- c("Jun Tmax (Celsius)", "Dec Tmax (Celsius)",
                  "Jul Tmax (Celsius)", "Jun Tmin (Celsius)")

names(feature_labs) <- c("Jun Tmax", "Dec Tmax", 
                         "Jul Tmax","Jun Tmin")

#### ALE Plot showing the top 4 variables in training and test ###

ggplot(ALE_TX_top_four, 
       aes(x = .borders, 
           y = .value)) + 
  geom_line(aes(color=.feature),
            size = 1) +
  facet_grid(Dataset ~ .feature,
             scales = "free",
             labeller = labeller(.feature = feature_labs))  +  
  scale_color_manual("Factors",
                     values = c("#BD0026",
                                "#BD0026",
                                "#BD0026",
                                "#FEB24C")) +
  labs(x = "Predictors", 
       y = "Accumulated Local Effects") +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme(legend.position = "none")


ggsave("Figure S10.svg")




#######
## KANSAS ###
######

### Load model ### 

KS_model <- readRDS(here::here("Models",
                               "Rf_KS_model.rds"))


### Load dataset 

train_KS <- read_csv(here::here("Processed_Datasets",
                                "train_KS.csv"))

test_KS <- read_csv(here::here("Processed_Datasets",
                               "test_KS.csv"))   

### Load important variables ####

Var_Imp_KS <- read.csv(here::here("Processed_Datasets",
                                  "Rfe_best_subset_imp_KS.csv"))




### filtering to contain the optimal subset of features ###

train_reduced_KS <- train_KS %>% dplyr::select(Yield,
                                               Var_Imp_KS$Features)

test_reduced_KS <- test_KS %>% dplyr::select(Yield,
                                             Var_Imp_KS$Features)

####################### 
### 

predictors_train_reduced_KS <- train_reduced_KS %>% 
                                          dplyr::select(-Yield)

predictors_test_reduced_KS <- test_reduced_KS %>% 
                                         dplyr::select(-Yield)


########################
############

set.seed(1234)

ALE_KS <- ALE_values(predictors = predictors_train_reduced_KS,
                     predictors_test = predictors_test_reduced_KS,
                     response_train = train_reduced_KS$Yield,
                     response_test = test_reduced_KS$Yield,
                     model = KS_model)



ALE_KS <- ALE_KS %>% 
              mutate(`Model Type` = "Kansas")



#### Filtering out the ALE_SD dataset to only contain the top 4 variables ##

Top_four <- c("Jun_Tmin","Aug_Precip","Aug_Tmax","Feb_Tmin")


### dsiplaying all color blind friendly pallettes 

display.brewer.all(colorblindFriendly = TRUE)

### display the pallette

display.brewer.pal(n = 9, name = 'YlOrRd')

## display the hex code #

brewer.pal(n = 9, name = "YlOrRd")


brewer.pal(n = 12, name = "Paired")


###########################


ALE_KS_top_four <- ALE_KS %>% 
  filter(.feature %in% Top_four)  %>% 
  mutate(.feature = str_replace_all(.feature,"_"," ")) %>%
  ##### Setting variable type based on variable name
  mutate(Dataset = case_when(str_detect(Dataset,
                                        "Train") ~ 
                               "Train (Before 2005)",
                             str_detect(Dataset,
                                        "Test") ~ 
                               "Test (After 2005)"))

# New facet label names for supp variable
feature_labs <- c("Jun Tmin (Celsius)", "Aug Precip (mm)",
                  "Aug Tmax (Celsius)", "Feb Tmin (Celsius)")

names(feature_labs) <- c("Jun Tmin","Aug Precip",
                         "Aug Tmax","Feb Tmin")

#### ALE Plot showing the top 4 variables in training and test ###

ggplot(ALE_KS_top_four, 
       aes(x = .borders, 
           y = .value)) + 
  geom_line(aes(color=.feature),
            size = 1) +
  facet_grid(Dataset ~ .feature,
             scales = "free",
             labeller = labeller(.feature = feature_labs))  +  
  scale_color_manual("Factors",
                     values = c("#1F78B4",
                                "#BD0026",
                                "#FEB24C",
                                "#BD0026")) +
  labs(x = "Predictors", 
       y = "Accumulated Local Effects") +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme(legend.position = "none")


ggsave("Figure S11.svg")




##############
### NEBRASKA ###
############ 


### Load model ### 

NE_model <- readRDS(here::here("Models",
                               "GBM_NE_model.rds"))


### Load dataset 

train_NE <- read_csv(here::here("Processed_Datasets",
                                "train_NE.csv"))

test_NE <- read_csv(here::here("Processed_Datasets",
                               "test_NE.csv"))   

### Load important variables ####

Var_Imp_NE <- read.csv(here::here("Processed_Datasets",
                                  "Rfe_best_subset_imp_NE.csv"))




### filtering to contain the optimal subset of features ###

train_reduced_NE <- train_NE %>% dplyr::select(Yield,
                                               Var_Imp_NE$Features)

test_reduced_NE <- test_NE %>% dplyr::select(Yield,
                                             Var_Imp_NE$Features)

####################### 
### 

predictors_train_reduced_NE <- train_reduced_NE %>% 
                                             dplyr::select(-Yield)

predictors_test_reduced_NE <- test_reduced_NE %>% 
                                             dplyr::select(-Yield)


########################
############

set.seed(1234)

ALE_NE <- ALE_values(predictors = predictors_train_reduced_NE,
                     predictors_test = predictors_test_reduced_NE,
                     response_train = train_reduced_NE$Yield,
                     response_test = test_reduced_NE$Yield,
                     model = NE_model)



ALE_NE <- ALE_NE %>% 
                mutate(`Model Type` = "Nebraska")

#### Filtering out the ALE_SD dataset to only contain the top 4 variables ##

Top_four <- c("Oct_Tmax","Aug_Precip","Aug_Tmax","May_Tmax")

### dsiplaying all color blind friendly pallettes 

display.brewer.all(colorblindFriendly = TRUE)

### display the pallette

display.brewer.pal(n = 9, name = 'YlOrRd')

## display the hex code #

brewer.pal(n = 9, name = "YlOrRd")


brewer.pal(n = 12, name = "Paired")


################

ALE_NE_top_four <- ALE_NE %>% 
  filter(.feature %in% Top_four)  %>% 
  mutate(.feature = str_replace_all(.feature,"_"," ")) %>%
  ##### Setting variable type based on variable name
  mutate(Dataset = case_when(str_detect(Dataset,
                                        "Train") ~ 
                               "Train (Before 2005)",
                             str_detect(Dataset,
                                        "Test") ~ 
                               "Test (After 2005)"))

# New facet label names for supp variable
feature_labs <- c("Oct Tmax (Celsius)","Aug Precip (mm)",
                  "Aug Tmax (Celsius)","May Tmax (Celsius)")

names(feature_labs) <- c("Oct Tmax","Aug Precip",
                         "Aug Tmax","May Tmax")

#### ALE Plot showing the top 4 variables in training and test ###

ggplot(ALE_NE_top_four, 
       aes(x = .borders, 
           y = .value)) + 
  geom_line(aes(color=.feature),
            size = 1) +
  facet_grid(Dataset ~ .feature,
             scales = "free",
             labeller = labeller(.feature = feature_labs))  +  
  scale_color_manual("Factors",
                     values = c("#1F78B4",
                                "#BD0026",
                                "#BD0026",
                                "#BD0026")) +
  labs(x = "Predictors", 
       y = "Accumulated Local Effects") +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme(legend.position = "none")



ggsave("Figure S9.svg")



############
### MINNESOTA ###
##### 

### Load model ### 

MN_model <- readRDS(here::here("Models",
                               "Rf_MN_model.rds"))


### Load dataset 

train_MN <- read_csv(here::here("Processed_Datasets",
                                "train_MN.csv"))

test_MN <- read_csv(here::here("Processed_Datasets",
                               "test_MN.csv"))   

### Load important variables ####

Var_Imp_MN <- read.csv(here::here("Processed_Datasets",
                                  "Rfe_best_subset_imp_MN.csv"))




### filtering to contain the optimal subset of features ###

train_reduced_MN <- train_MN %>% dplyr::select(Yield,
                                               Var_Imp_MN$Features)

test_reduced_MN <- test_MN %>% dplyr::select(Yield,
                                             Var_Imp_MN$Features)

####################### 
### 

predictors_train_reduced_MN <- train_reduced_MN %>% 
                                            dplyr::select(-Yield)

predictors_test_reduced_MN <- test_reduced_MN %>% 
                                         dplyr::select(-Yield)


########################
############

set.seed(1234)

ALE_MN <- ALE_values(predictors = predictors_train_reduced_MN,
                     predictors_test = predictors_test_reduced_MN,
                     response_train = train_reduced_MN$Yield,
                     response_test = test_reduced_MN$Yield,
                     model = MN_model)




ALE_MN <- ALE_MN %>% 
              mutate(`Model Type` = "Minnesota")

#### Filtering out the ALE_SD dataset to only contain the top 4 variables ##

Top_four <- c("Oct_Tmax","May_Tmin","May_Tmax","Oct_Precip")

### dsiplaying all color blind friendly pallettes 

display.brewer.all(colorblindFriendly = TRUE)

### display the pallette

display.brewer.pal(n = 9, name = 'YlOrRd')

## display the hex code #

brewer.pal(n = 9, name = "YlOrRd")


brewer.pal(n = 12, name = "Paired")

########

ALE_MN_top_four <- ALE_MN %>% 
  filter(.feature %in% Top_four)  %>% 
  mutate(.feature = str_replace_all(.feature,"_"," ")) %>%
  ##### Setting variable type based on variable name
  mutate(Dataset = case_when(str_detect(Dataset,
                                        "Train") ~ 
                               "Train (Before 2005)",
                             str_detect(Dataset,
                                        "Test") ~ 
                               "Test (After 2005)"))

# New facet label names for supp variable
feature_labs <- c("Oct Tmax (Celsius)","May Tmin (Celsius)",
                  "May Tmax (Celsius)","Oct Precip (mm)")

names(feature_labs) <- c("Oct Tmax","May Tmin",
                         "May Tmax","Oct Precip")

#### ALE Plot showing the top 4 variables in training and test ###

ggplot(ALE_MN_top_four, 
       aes(x = .borders, 
           y = .value)) + 
  geom_line(aes(color=.feature),
            size = 1) +
  facet_grid(Dataset ~ .feature,
             scales = "free",
             labeller = labeller(.feature = feature_labs))  +  
  scale_color_manual("Factors",
                     values = c("#BD0026",
                                "#FEB24C",
                                "#1F78B4",
                                "#BD0026")) +
  labs(x = "Predictors", 
       y = "Accumulated Local Effects") +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme(legend.position = "none")


ggsave("Figure S14.svg")




######## ADD A COLUMN WITH THE NAME OF THE REGION(GLOBAL OR STATE)
### JOIN THE DATAFRAMES ####

ALE <- rbind(ALE_Global,ALE_CO,ALE_KS,ALE_MN,ALE_ND,ALE_NE,ALE_SD,ALE_TX)


colnames(ALE)

ALE <- ALE %>% 
             dplyr::rename(ALE = `.value`,
                          Variable_values = `.borders`,
                          Feature = `.feature`)







##### writing out this file ###

write.csv(ALE,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/ALE_values.csv",
          row.names = F)


saveRDS(ALE,
        "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/RDS_objects/ALE.RDS")





