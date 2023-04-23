

rm(list = ls())

library(here)
library(tidyverse)
library(sf) 
library(RColorBrewer)

Future_predictions <- readRDS(here("RDS_objects",
                                   "Future_predictions.RDS")) %>% 
                                                          st_drop_geometry()
                                                  
Average_National_Yield <- Future_predictions %>% 
                             group_by(RCP,Timeframe) %>% 
                           summarize(Average_National_Yield = mean(Predictions))


Average_National_Error <- Future_predictions %>% 
                                       group_by(RCP,Timeframe) %>% 
                           summarize(Average_National_Error = mean(Mean_Error))
                           


Average_National_Yield <- Average_National_Yield %>% 
                                            inner_join(Average_National_Error)


###### Setting the factors ### for RCPS

Average_National_Yield$RCP <- factor(Average_National_Yield$RCP, 
                                     levels = c("126","245","370","585"))

######## Display color pallette ##

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(9,'YlOrRd')


brewer.pal(9, 'YlOrRd') ## displaying the hex codes 



Average_National_Yield %>% 
  ggplot(aes(x= Timeframe,
        y=Average_National_Yield,
        group=RCP,
        color=RCP)) +
  geom_line()  +
  geom_point() +
  facet_wrap(~RCP) +
  labs(x = "Timeframe",
       y="Average National Yield") +
  theme(text = element_text(size = 10)) 


ggsave("Figure 4.svg")




