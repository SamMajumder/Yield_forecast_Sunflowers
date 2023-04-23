

rm(list = ls())

library(tidyUSDA)
library(tidyverse)



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

### removing some of the states and counties outside of our study 

Sunflower_Yield <- Sunflower_Yield %>% 
                   dplyr::filter(county_name != "OTHER (COMBINED) COUNTIES") %>%
                   dplyr::filter(county_name != "OTHER COUNTIES") %>%
                   dplyr::filter(state_name != "CALIFORNIA") %>%
                   dplyr::select(year,state_name,county_name,Value) %>%
                   dplyr::rename(Yield = Value)




########### AREA HARVESTED DATA ##### 

Area_Harvested <- getQuickstat(sector='CROPS',
                                group = "FIELD CROPS",
                                commodity = "SUNFLOWER",
                                category = "AREA HARVESTED",
                                domain = "TOTAL",
                                key = key,
                                program = 'SURVEY',
                                data_item = "SUNFLOWER, OIL TYPE - ACRES HARVESTED",
                                geographic_level = 'COUNTY',
                                year = years)


### removing some of the states and counties outside of our study 

Area_Harvested <- Area_Harvested %>% 
                  dplyr::filter(county_name != "OTHER (COMBINED) COUNTIES") %>%
                  dplyr::filter(county_name != "OTHER COUNTIES") %>%
                  dplyr::filter(state_name != "CALIFORNIA") %>%
                  dplyr::select(year,state_name,county_name,Value) %>%
                  dplyr::rename(Acres_Harvested = Value)



###### AREA PLANTED DATA #### 

Area_Planted <- getQuickstat(sector='CROPS',
                               group = "FIELD CROPS",
                               commodity = "SUNFLOWER",
                               category = "AREA PLANTED",
                               domain = "TOTAL",
                               key = key,
                               program = 'SURVEY',
                               data_item = "SUNFLOWER, OIL TYPE - ACRES PLANTED",
                               geographic_level = 'COUNTY',
                               year = years)


### removing some of the states and counties outside of our study 

Area_Planted <- Area_Planted %>% 
                  dplyr::filter(county_name != "OTHER (COMBINED) COUNTIES") %>%
                  dplyr::filter(county_name != "OTHER COUNTIES") %>%
                  dplyr::filter(state_name != "CALIFORNIA") %>%
                  dplyr::select(year,state_name,county_name,Value) %>%
                  dplyr::rename(Acres_Planted = Value)



##### Joining these three dataframes into one ### 

Sunflower_Yield <- list(Area_Planted,Area_Harvested,Sunflower_Yield) %>% 
                   reduce(inner_join)


######################
## States and counties in in my study
######################

######## Viewing the oldest and latest values for each counties ###
### Identifying which counties have yield values on the either side of the year 2005

## Study counties ###
Counties <- Sunflower_Yield %>% 
                  group_by(state_name,county_name) %>% 
                  summarise(Latest = max(year),
                            Oldest = min(year)) %>% 
                          arrange(desc(Latest)) %>% 
                          filter(Latest >= 2005,
                                 Oldest <= 2004)

##### Filtering the Sunflower Yield dataframe based on these counties 

Sunflower_Yield <- Sunflower_Yield %>% 
                            inner_join(Counties)



####  removing rows containing Yield values = 0 ##

Sunflower_Yield <- Sunflower_Yield %>% 
                        filter(Yield > 0)


########################
## EXPORTING THE FILES IN A FOLDER CALLED Raw_Datasets and in a subfolder called Yield##
#####################


### write out the yield and the region file ###

write.csv(Sunflower_Yield,
          "~/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Raw_Datasets/Yield/Sunflower_Yield.csv",
          row.names = FALSE)

write.csv(Counties,
          "~/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Raw_Datasets/Regions/Study_Regions.csv",
          row.names = F)   
















