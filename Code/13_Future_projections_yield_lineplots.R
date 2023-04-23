

rm(list = ls())

library(here)
library(tidyverse)
library(sf) 
library(RColorBrewer)

Future_predictions <- readRDS(here("RDS_objects",
                                   "Future_predictions.RDS")) %>% 
                                                          st_drop_geometry()
                                                  
Average_Yearly_Yield <- Future_predictions %>% 
                             group_by(State,RCP,Timeframe) %>% 
                           summarize(Average_Yearly_Yield = mean(Predictions))


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




###### Setting the factors ### for RCPS

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



Average_Yearly_Yield %>% 
  ggplot(aes(x= Timeframe,
        y=Average_Yearly_Yield,
        group=State,
        colour = State)) +
  geom_line(size = 1) +
  geom_point() +
  scale_color_manual("Regions",
                    values = c("#FC8D62","#8DA0CB","#E78AC3","#A6D854",
                               "#FFD92F","#E5C494","#B3B3B3")) +
  facet_wrap(~RCP) +
  labs(x = "Timeframe",
       y="Average Yearly Yield")  +
  theme(text = element_text(size = 10)) +
  theme_bw()


ggsave("Figure 5.svg")




