

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
                                              Standard_deviations = sd(Predictions))



## Setting factors ### RCPS are factors ### 


Average_overall_yearly_yield$RCP <- factor(Average_overall_yearly_yield$RCP, 
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


### National Average #### Latest value 

Latest_national_average <- Sunflower_Yield %>% 
                               dplyr::filter(year == 2022) %>% 
                               dplyr::summarize(Average_overall_yearly_Yield = mean(Value),
                                                Standard_deviations = sd(Value)) %>% 
                               dplyr::mutate(Timeframe = 2022)


#### Repeating the dataframe ###
### RCP ### 

RCP <- rep(c("126","245","370","585"),3)

#### 

Latest_national_average <- cbind(RCP,Latest_national_average)


########
#### Adding the Latest National Average to this dataset ###
########### 

Average_overall_yearly_yield <- rbind(Latest_national_average,
                                      Average_overall_yearly_yield)


## Setting factors ### Timeperiod are factors ### 


Average_overall_yearly_yield$Timeframe <- factor(Average_overall_yearly_yield$Timeframe, 
                                                levels = c("2022","2021-2040",
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



###########
### Historical data ##
####### 


### State level average ### Latest value from each state ###

Latest_Year <- Sunflower_Yield %>% 
                     dplyr::select(Value,year,state_name) %>% 
                     aggregate(year ~ state_name, max)




Latest_Yield_value <- Sunflower_Yield %>% 
  dplyr::select(Value,year,state_name) %>% 
  aggregate(year ~ state_name, max) %>% 
  dplyr::left_join(Sunflower_Yield) %>% 
  dplyr::group_by(state_name) %>% 
  dplyr::summarise(mean(Value),
                   sd(Value))


Latest_Yield_value <- inner_join(Latest_Year,Latest_Yield_value) 

### Removing Okhlahoma and California #### 

Latest_Yield_value <- Latest_Yield_value %>% 
  dplyr::filter(state_name != "CALIFORNIA") %>%
  dplyr::filter(state_name != "OKLAHOMA") %>% 
  dplyr::rename(Average_Yearly_Yield = `mean(Value)`) %>% 
  dplyr::rename(Standard_deviations = `sd(Value)`) %>%
  dplyr::mutate(Standard_deviations = replace_na(Standard_deviations,0)) %>% 
  dplyr::rename(Timeframe = `year`) %>% 
  dplyr::rename(State = state_name)


#######
## Repating each state four times ## once for each SSP ##
######## three timeframes 


Latest_Yield_value <- Latest_Yield_value %>% 
                                  slice(rep(1:n(), 12))




Latest_Yield_value <- Latest_Yield_value %>% 
                                         arrange(State) 
## Change the names of the state from all upper case to first letter uppercase and all lowercase

Latest_Yield_value$State <- str_to_title(Latest_Yield_value$State)


## extracting the SSPs
RCP <- Average_Yearly_Yield$RCP


## Adding the RCP to the latest Yield values for each state 
Latest_Yield_value <- cbind(RCP,Latest_Yield_value)

### Now add the future Yield forecast for each state to Latest Yield Value ###

Average_Yearly_Yield <- rbind(Latest_Yield_value,Average_Yearly_Yield)


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

Average_Yearly_Yield$Timeframe <- factor(Average_Yearly_Yield$Timeframe, 
                                                 levels = c("2016","2017","2018","2022",
                                                            "2021-2040",
                                                            "2041-2060","2061-2080"))

## Setting up the factors for regions ####

Average_Yearly_Yield$State <- factor(Average_Yearly_Yield$State,
                                                      levels = c("South Dakota",
                                                                 "Nebraska",
                                                                 "Texas",
                                                                 "Kansas",
                                                                 "Colorado",
                                                                 "North Dakota",
                                                                 "Minnesota"))






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
  labs(x = "Time Period",
       y="Average Yearly Yield (lb/acre)")  +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 45,hjust=1)) 


ggsave("Figure S15.svg")
























