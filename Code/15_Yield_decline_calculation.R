

rm(list = ls())

######### Create a table that show percent yield decline across three timeperiods ###

library(here)
library(tidyverse)
library(sf)
library(tidyUSDA)


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

## Viewing what the data looks like after download #####

### Looking at the counties and viewing how many times they are represented
Sunflower_Yield %>% dplyr::group_by(county_name) %>% summarise(n()) %>% View()


### National Average #### Latest value 

Latest_national_average <- Sunflower_Yield %>% 
                               dplyr::filter(year == 2022) %>% 
                               dplyr::summarize(Latest_National_Average = mean(Value)) 


############ Future Data #####


Future_predictions <- readRDS(here::here("RDS_objects",
                                         "Future_predictions.RDS")) %>% 
                                          st_drop_geometry() 

### Average overall yearly yield ## at a national level ###

Average_overall_yearly_yield <- Future_predictions %>% 
  dplyr::group_by(RCP,Timeframe) %>% 
  dplyr::summarize(Average_overall_yearly_Yield = mean(Predictions))




National_Level_Yield_Decline <- Average_overall_yearly_yield %>%
                                      mutate(Latest_National_Average = Latest_national_average$Latest_National_Average) %>% 
                                      pivot_wider(names_from = Timeframe, 
                                      values_from = Average_overall_yearly_Yield) %>% 
                                      mutate(Percent_Change_First_Period = ((`2021-2040` - Latest_National_Average)/Latest_National_Average) * 100) %>% 
                                      mutate(Percent_Change_First_to_Second_Period = ((`2041-2060` - `2021-2040`)/`2021-2040`) * 100) %>% 
                                      dplyr::rename(First_Period = `2021-2040`,
                                             Second_Period = `2041-2060`,
                                             Third_Period = `2061-2080`,
                                             SSP = RCP)
  



write.csv(National_Level_Yield_Decline,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Nationa_Yield_change.csv",
          row.names = FALSE)




##################
##### STATE LEVEL CHANGE #### 
######## 


### State level average ### Latest value from each state ###

Latest_Year <- Sunflower_Yield %>% 
                  dplyr::select(Value,year,state_name) %>% 
                  aggregate(year ~ state_name, max)
 



Latest_Yield_value <- Sunflower_Yield %>% 
                          dplyr::select(Value,year,state_name) %>% 
                          aggregate(year ~ state_name, max) %>% 
                          dplyr::left_join(Sunflower_Yield) %>% 
                          dplyr::group_by(state_name) %>% 
                          dplyr::summarise(mean(Value))


Latest_Yield_value <- inner_join(Latest_Year,Latest_Yield_value) 

### Removing Okhlahoma and California #### 

Latest_Yield_value <- Latest_Yield_value %>% 
                            dplyr::filter(state_name != "CALIFORNIA") %>%
                            dplyr::filter(state_name != "OKLAHOMA") %>% 
                            dplyr::rename(Latest_Average = `mean(Value)`) %>% 
                            dplyr::rename(Latest_Year = `year`) %>% 
                            dplyr::rename(State = state_name)





#########################
#######
### Average yearly yield predictions at a state wide level ### 

Average_Yearly_Yield <- Future_predictions %>% 
  dplyr::group_by(State,RCP,Timeframe) %>% 
  dplyr::summarize(Average_Yearly_Yield = mean(Predictions))


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
                             "Texas")) %>% 
  mutate(State = toupper(State))



##### Joining the year and the latest yield value with the future Yield projections

Latest_Yield_value <- Latest_Yield_value %>% 
                                right_join(Average_Yearly_Yield)


##############


State_level_yield_change <- Latest_Yield_value %>%
                                  pivot_wider(names_from = Timeframe, 
                                  values_from = Average_Yearly_Yield) %>% 
                                  mutate(Percent_Change_First_Period = 100 *(`2021-2040` - Latest_Average)/Latest_Average) %>% 
                                  mutate(Percent_Change_First_to_Second_Period = 100 *(`2041-2060` - `2021-2040`)/`2021-2040`) %>% 
                                  dplyr::rename(First_Period = `2021-2040`,
                                                Second_Period = `2041-2060`,
                                                Third_Period = `2061-2080`,
                                                SSP = RCP)





write.csv(State_level_yield_change,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/State_Level_Yield_change.csv",
          row.names = FALSE)







  





