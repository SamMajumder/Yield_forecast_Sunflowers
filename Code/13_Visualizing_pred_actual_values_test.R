

rm(list = ls())

packages <- c("here","tidyverse","ggpubr","stringi")

lapply(packages, require,character.only=T)


### Loading the files which contain the test errors ##

Errors_test <- read.csv(here::here("Processed_Datasets",
                              "Errors.csv")) %>% 
                                            filter(Dataset == "Test") 


Pred_versus_actual_state <- Errors_test %>% 
                               dplyr::select(State,Predictions,Yield,Year) 


Pred_versus_actual_state_National <- Pred_versus_actual_state

#replace multiple patterns in name column
Pred_versus_actual_state_National$State <- stri_replace_all_regex(Pred_versus_actual_state_National$State,
                                                      pattern=c('CO', 'KS', 'TX',
                                                                'NE','ND','SD',
                                                                'MN'),
                                          replacement=c('National','National',
                                                        'National','National',
                                                        'National','National',
                                                        'National'),
                                                         vectorize=FALSE)

National_and_state_pred_versus_actual <- rbind(Pred_versus_actual_state_National,
                                               Pred_versus_actual_state) 

National_and_state_pred_versus_actual <- National_and_state_pred_versus_actual %>% 
                                          mutate(State = case_when(str_detect(State,
                                                                                "CO") ~ 
                                                                       "Colorado",
                                                                     str_detect(State,
                                                                                "KS") ~ 
                                                                       "Kansas",
                                                                     str_detect(State,
                                                                                "MN") ~ 
                                                                       "Minnesota",
                                                                     str_detect(State,
                                                                                "ND") ~ 
                                                                       "North Dakota",
                                                                     str_detect(State,
                                                                                "NE") ~
                                                                       "Nebraska",
                                                                     str_detect(State,
                                                                                "SD") ~
                                                                       "South Dakota",
                                                                   str_detect(State,
                                                                              "TX") ~
                                                                     "Texas",
                                                                   str_detect(State,
                                                                              "National") ~
                                                                     "National"))

National_and_state_pred_versus_actual$State <- factor(National_and_state_pred_versus_actual$State,
                                                      levels = c("National",
                                                                 "South Dakota",
                                                                 "Nebraska",
                                                                 "Texas",
                                                                 "Kansas",
                                                                 "Colorado",
                                                                 "North Dakota",
                                                                 "Minnesota"))


#### Plotting the prediction versus actual ### 

National_and_state_pred_versus_actual %>% 
  ggplot(aes(x= Predictions,
             y= Yield,
             group=State,
             color=State)) +
  geom_abline(linetype = "dashed") +
  geom_point() +
  xlim(0,3000) +
  geom_smooth(color = 'black') +
  stat_cor(aes(label = ..r.label..),
           label.x=30, label.y=2500)+
  scale_color_manual("Regions",
                     values = c("#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854",
                                "#FFD92F","#E5C494","#B3B3B3")) +
  facet_wrap(~State) +
  labs(x = "Predicted Yield (LB/Acre)",
       y="Actual Yield (LB/Acre)") +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme(legend.position = "none")


ggsave("Figure 4.svg")

