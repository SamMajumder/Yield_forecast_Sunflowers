

rm(list = ls())

packages <- list("tidyverse","caret","CAST","here",
                 "randomForest","RColorBrewer")

lapply(packages, require,character.only=T)

###

source(here::here("Code","00_Functions.R"))


####################
#### TRAINING DATA ###
############# 

### RFE IMPORTANCE FROM THE WHOLE (GLOBAL DATASET)

Var_Imp_Global <- read.csv(here::here("Processed_Datasets",
                                "Rfe_best_subset_imp_combined_yield.csv")) %>% 
                   mutate(Model_Type = "National") %>% 
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
                                       "Ag Practice")) %>% 
                      mutate(Features = str_replace_all(Features,"_"," "))

                          

         

### setting levels for variable type
##

Var_Imp_Global$`Variable Type` <- factor(Var_Imp_Global$`Variable Type`, 
                                         levels = c("Maximum Temperature",
                                                    "Precipitation",
                                                    "Minimum Temperature",
                                                    "Ag Practice"))


#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

### dsiplaying all color blind friendly pallettes 

display.brewer.all(colorblindFriendly = TRUE)

### display the pallette

display.brewer.pal(n = 9, name = 'YlOrRd')

display.brewer.pal(n = 12, name = 'Paired')

## display the hex code #

brewer.pal(n = 9, name = "YlOrRd")


brewer.pal(n = 12, name = "Paired")





####### Create a variable importance plot ####

ggplot(data = Var_Imp_Global,
                     aes(x=reorder(Features,Overall), y = Overall, 
                         fill = `Variable Type`)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#BD0026",
                               "#1F78B4",
                               "#FD8D3C",
                               "#A6D854")) +
  labs(x= "Predictors", 
       y= "Variable Importance (mean decrease of RMSE)") +
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
                                       "Ag Practice")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))





### setting levels for variable type
##

Var_Imp_ND$`Variable Type` <- factor(Var_Imp_ND$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice"))


####### Create a variable importance plot ####

ggplot(data = Var_Imp_ND,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#BD0026",
                               "#1F78B4",
                               "#FD8D3C",
                               "#A6D854")) +
  labs(x= "Predictors",
       y= "Variable Importance (mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 

ggsave("Figure S6.svg")


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
                                       "Ag Practice")) %>%
  mutate(Features = str_replace_all(Features,"_"," "))


### setting levels for variable type
##

Var_Imp_SD$`Variable Type` <- factor(Var_Imp_SD$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice"))


####### Create a variable importance plot ####

ggplot(data = Var_Imp_SD,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#BD0026",
                               "#1F78B4",
                               "#FD8D3C",
                               "#A6D854")) +
  labs(x= "Predictors", 
       y= "Variable Importance (mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 


ggsave("Figure S1.svg")



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
                                       "Ag Practice")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))


### setting levels for variable type
##

Var_Imp_CO$`Variable Type` <- factor(Var_Imp_CO$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice"))



####### Create a variable importance plot ####

ggplot(data = Var_Imp_CO,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#BD0026",
                               "#1F78B4",
                               "#FD8D3C",
                               "#A6D854")) +
  labs(x= "Predictors", 
       y= "Variable Importance (mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.86, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 

ggsave("Figure S5.svg")


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
                                       "Ag Practice")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))


### setting levels for variable type
##

Var_Imp_KS$`Variable Type` <- factor(Var_Imp_KS$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice"))




####### Create a variable importance plot ####

ggplot(data = Var_Imp_KS,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#BD0026",
                               "#1F78B4",
                               "#FD8D3C",
                               "#A6D854")) +
  labs(x= "Predictors", 
       y= "Variable Importance (mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 

ggsave("Figure S4.svg")

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
                                       "Ag Practice")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))


### setting levels for variable type
##

Var_Imp_NE$`Variable Type` <- factor(Var_Imp_NE$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice"))


####### Create a variable importance plot ####

ggplot(data = Var_Imp_NE,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#BD0026",
                               "#1F78B4",
                               "#FD8D3C",
                               "#A6D854")) +
  labs(x= "Predictors", 
       y= "Variable Importance (mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 


ggsave("Figure S2.svg")




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
                                       "Ag Practice")) %>% 
  mutate(Features = str_replace_all(Features,"_"," "))




### setting levels for variable type
##

Var_Imp_MN$`Variable Type` <- factor(Var_Imp_MN$`Variable Type`, 
                                     levels = c("Maximum Temperature",
                                                "Precipitation",
                                                "Minimum Temperature",
                                                "Ag Practice"))


####### Create a variable importance plot ####

ggplot(data = Var_Imp_MN,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#BD0026",
                               "#1F78B4",
                               "#FD8D3C",
                               "#A6D854")) +
  labs(x= "Predictors", 
       y= "Variable Importance (mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 

ggsave("Figure S7.svg")

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
                                       "Ag Practice")) %>% 
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

ggplot(data = Var_Imp_TX,
                 aes(x=reorder(Features,Overall), y = Overall, 
                     fill = `Variable Type`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#BD0026",
                               "#1F78B4",
                               "#FD8D3C",
                               "#A6D854")) +
  labs(x= "Predictors", 
       y= "Variable Importance(mean decrease of RMSE)") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = c(0.78, 0.15),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  theme(text = element_text(size = 10)) 

ggsave("Figure S3.svg")

############ Combine all of the data into dataframe ###

Var_IMP <- list(Var_Imp_Global,Var_Imp_CO,Var_Imp_KS,Var_Imp_MN,Var_Imp_ND,
                Var_Imp_NE,Var_Imp_SD,Var_Imp_TX) %>% reduce(rbind) 


#### Exporting the dataset as an rds object

here("RDS_objects")

saveRDS(Var_IMP,
        "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/RDS_objects/Var_IMP_total.RDS")








