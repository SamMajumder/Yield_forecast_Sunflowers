rm(list = ls())

library(here)
library(tidyverse)
library(sf) 
library(RColorBrewer)
library(tidyUSDA)

Future_predictions <- readRDS(here::here("RDS_objects",
                                   "Future_predictions.RDS")) %>% 
                                                          st_drop_geometry() 

### Average overall yearly yield ## at a national level #####

Average_overall_yearly_yield <- Future_predictions %>% 
                                    dplyr::group_by(RCP,Timeframe) %>% 
                                    dplyr::summarize(Average_overall_yearly_Yield = mean(Predictions),
                                              Standard_deviations = sd(Predictions)) %>% 
                                    dplyr::rename(SSP=RCP)



## Setting factors ### RCPS are factors ### 


Average_overall_yearly_yield$SSP <- factor(Average_overall_yearly_yield$SSP, 
                                     levels = c("126","245","370","585"))




#################
### NOW ADDING THE HISTORICAL DATA ### 
################ 


key <- "D923D273-EDCC-3FA9-AE2B-E5513DD00E06"


years <- c("1976","1977","1978","1979","1980","1981","1982","1983","1984",
           "1985","1986","1987","1988","1989","1990","1991","1992","1993",
           "1994","1995","1996","1997","1998","1999","2000","2001","2002",
           "2003","2004","2005","2006","2007","2008","2009","2010","2011",
           "2012","2013","2014","2015","2016","2017","2018","2019","2020",
           "2021","2022")


Sunflower_Yield <- getQuickstat(sector='CROPS',
                                group = "FIELD CROPS",
                                commodity = "SUNFLOWER",
                                category = "YIELD",
                                domain = "TOTAL",
                                key = key,
                                program = 'SURVEY',
                                data_item = "SUNFLOWER, OIL TYPE - YIELD, MEASURED IN LB / ACRE",
                                geographic_level = 'COUNTY',
                                year = years)

## Viewing what the data looks like after download ##

### Looking at the counties and viewing how many times they are represented
Sunflower_Yield %>% dplyr::group_by(county_name) %>% summarise(n()) %>% View()


### Subsetting the datasets based on time ### 2000 to 2022 ##
### then take the average and standard deviations of the yield within this timeframe ### 

Historical_yield_data <- Sunflower_Yield %>% 
                               dplyr::filter(state_name != "CALIFORNIA") %>% 
                               dplyr::filter(state_name != "OKLAHOMA") %>%   
                               dplyr::filter(between(year,2000,2020)) %>% 
                               dplyr::summarize(Average_overall_yearly_Yield = mean(Value),
                                                Standard_deviations = sd(Value)) %>% 
                               dplyr::mutate(Timeframe = "2000-2020")
                               


#### Repeating the dataframe ###
### RCP ### 

SSP <- rep(c("126","245","370","585"),3)

#### 

Historical_data <- cbind(SSP,Historical_yield_data)


########
#### Adding the Latest National Average to this dataset ###
########### 

Average_overall_yearly_yield <- rbind(Historical_data,
                                      Average_overall_yearly_yield)



## Setting factors ### Timeperiod are factors ### 


Average_overall_yearly_yield$Timeframe <- factor(Average_overall_yearly_yield$Timeframe, 
                                                levels = c("2000-2020","2021-2040",
                                                           "2041-2060","2061-2080"))






######## Display color pallette ##

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(9,'YlOrRd')


brewer.pal(9, 'YlOrRd') ## displaying the hex codes 



Average_overall_yearly_yield %>% 
  ggplot(aes(x= Timeframe,
             y = Average_overall_yearly_Yield,
             group=SSP,
             colour = SSP)) +
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
  labs(x = "Time Period",
       y="Average Yearly Yield (lb/acre)")  +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 45,hjust=1)) 



ggsave("Figure 5.svg")

######################
#######
### Average yearly yield at a state wide level ###                                                  

Average_Yearly_Yield <- Future_predictions %>% 
                             dplyr::group_by(State,RCP,Timeframe) %>% 
                             dplyr::summarize(Average_Yearly_Yield = mean(Predictions),
                                              Standard_deviations = sd(Predictions)) %>% 
                             dplyr::rename(SSP=RCP)


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



###########
### Historical data ##
####### 

#################
### NOW ADDING THE HISTORICAL DATA ### 
################ 


key <- "D923D273-EDCC-3FA9-AE2B-E5513DD00E06"


years <- c("1976","1977","1978","1979","1980","1981","1982","1983","1984",
           "1985","1986","1987","1988","1989","1990","1991","1992","1993",
           "1994","1995","1996","1997","1998","1999","2000","2001","2002",
           "2003","2004","2005","2006","2007","2008","2009","2010","2011",
           "2012","2013","2014","2015","2016","2017","2018","2019","2020",
           "2021","2022")


Sunflower_Yield <- getQuickstat(sector='CROPS',
                                group = "FIELD CROPS",
                                commodity = "SUNFLOWER",
                                category = "YIELD",
                                domain = "TOTAL",
                                key = key,
                                program = 'SURVEY',
                                data_item = "SUNFLOWER, OIL TYPE - YIELD, MEASURED IN LB / ACRE",
                                geographic_level = 'COUNTY',
                                year = years)






### State level average ### 2000 to 2020 ###

Historical_yield_data_state <- Sunflower_Yield %>% 
                                          dplyr::filter(state_name != "CALIFORNIA") %>% 
                                          dplyr::filter(state_name != "OKLAHOMA") %>% 
                                          dplyr::filter(between(year,2000,2020)) %>%  
                                          dplyr::group_by(state_name) %>% 
                                          dplyr::summarize(Average_Yearly_Yield = mean(Value),
                                                           Standard_deviations = mean(Value)) 



#######
## Repeating each state four times ## once for each SSP ##
######## three timeframes 


Average_overall_yearly_yield_state <- Historical_yield_data_state %>% 
                                                         slice(rep(1:n(), 12))


Average_overall_yearly_yield_state <- Average_overall_yearly_yield_state %>% 
                                                                   arrange(state_name) %>% 
                                                                   dplyr::rename(State = state_name) %>% 
                                                                   dplyr::mutate(Timeframe = "2000-2020")

## Change the names of the state from all upper case to first letter uppercase and all lowercase

Average_overall_yearly_yield_state$State <- str_to_title(Average_overall_yearly_yield_state$State)


## extracting the SSPs
SSP <- Average_Yearly_Yield$SSP


## Adding the RCP to the latest Yield values for each state 
Average_overall_yearly_yield_state <- cbind(SSP,Average_overall_yearly_yield_state)


### Join the historical and future prediction data ### 

Average_future_yearly_yield <- rbind(Average_Yearly_Yield,
                                     Average_overall_yearly_yield_state)


########################

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


## Setting factors ### Timeperiod are factors ### 

Average_future_yearly_yield$Timeframe <- factor(Average_future_yearly_yield$Timeframe, 
                                                 levels = c("2000-2020",
                                                            "2021-2040",
                                                            "2041-2060","2061-2080"))

## Setting up the factors for regions ####

Average_future_yearly_yield$State <- factor(Average_future_yearly_yield$State,
                                                      levels = c("South Dakota",
                                                                 "Nebraska",
                                                                 "Texas",
                                                                 "Kansas",
                                                                 "Colorado",
                                                                 "North Dakota",
                                                                 "Minnesota"))






Average_future_yearly_yield %>% 
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
  facet_wrap(~SSP,
             labeller = labeller(SSP = SSP_labs)) +
  labs(x = "Time Period",
       y="Average Yearly Yield (lb/acre)")  +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 45,hjust=1)) 


ggsave("Figure S15.svg")





























