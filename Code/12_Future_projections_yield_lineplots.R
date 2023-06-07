

rm(list = ls())

library(here)
library(tidyverse)
library(sf) 
library(RColorBrewer)

Future_predictions <- readRDS(here::here("RDS_objects",
                                   "Future_predictions.RDS")) %>% 
                                                          st_drop_geometry() 

### Average overall yearly yield ## at a national level ###

Average_overall_yearly_yield <- Future_predictions %>% 
                                    dplyr::group_by(RCP,Timeframe) %>% 
                                    dplyr::summarize(Average_overall_yearly_Yield = mean(Predictions),
                                              Standard_deviations = sd(Predictions))




## Setting factors ### RCPS are factors ### 


Average_overall_yearly_yield$RCP <- factor(Average_overall_yearly_yield$RCP, 
                                     levels = c("126","245","370","585"))

######## Display color pallette ##

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(9,'YlOrRd')


brewer.pal(9, 'YlOrRd') ## displaying the hex codes 



Average_overall_yearly_yield %>% 
  ggplot(aes(x= Timeframe,
             y = Average_overall_yearly_Yield,
             group=RCP,
             colour = RCP)) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin=Average_overall_yearly_Yield - Standard_deviations, 
                    ymax=Average_overall_yearly_Yield + Standard_deviations), 
                width=.2,
                position=position_dodge(0.05)) + 
  scale_color_manual("Socioeconomic Pathways",
                     values = c("#FEB24C","#FD8D3C","#BD0026","#800026"),
                     labels = c("Sustainability","Middle of the Road",
                                "Inequality","Fossil-fueled Development"))+
  labs(x = "Timeperiod",
       y="Average Yearly Yield (LB/Acre)")  +
  theme(text = element_text(size = 10)) +
  theme_bw()



ggsave("Figure 5.svg")

######################
#######
### Average yearly yield at a state wide level ###                                                  
Average_Yearly_Yield <- Future_predictions %>% 
                             dplyr::group_by(State,RCP,Timeframe) %>% 
                             dplyr::summarize(Average_Yearly_Yield = mean(Predictions),
                                              Standard_deviations = sd(Predictions))


#Average_National_Error <- Future_predictions %>% group_by(RCP,Timeframe) %>% summarize(Average_National_Error = mean(Mean_Error))
                           


#Average_National_Yield <- Average_National_Yield %>% inner_join(Average_National_Error)

Average_Yearly_Yield <- Average_Yearly_Yield %>% 
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
                                                                     "Texas"))




###### Setting the factors ## each state is a factor ### we will facet by RCP ##

Average_Yearly_Yield$State <- factor(Average_Yearly_Yield$State, 
                                     levels = c("South Dakota",
                                                "Nebraska",
                                                "Texas",
                                                "North Dakota",
                                                "Colorado",
                                                "Minnesota",
                                                "Kansas"))

######## Display color pallette ##

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(8,'Set2')


brewer.pal(8, 'Set2') ## displaying the hex codes 

######## new facet strip names ###

# New facet label names for supp variable
SSP_labs <- c("Sustainability", 
              "Middle of the Road",
              "Regional Rivalry",
              "Fossil-fueled Development")

names(SSP_labs) <- c("126", "245","370","585")




Average_Yearly_Yield %>% 
  ggplot(aes(x= Timeframe,
        y=Average_Yearly_Yield,
        group=State,
        colour = State)) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin=Average_Yearly_Yield - Standard_deviations, 
                    ymax=Average_Yearly_Yield + Standard_deviations), 
                width=.2,
                position=position_dodge(0.05)) +
  scale_color_manual("Regions",
                    values = c("#FC8D62","#8DA0CB","#E78AC3","#A6D854",
                               "#FFD92F","#E5C494","#B3B3B3")) +
  facet_wrap(~RCP,
             labeller = labeller(RCP = SSP_labs)) +
  labs(x = "Timeperiod",
       y="Average Yearly Yield (LB/Acre)")  +
  theme(text = element_text(size = 10)) +
  theme_bw()


ggsave("Figure S15.svg")
























