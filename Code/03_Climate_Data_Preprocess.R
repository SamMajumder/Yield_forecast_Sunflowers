
rm(list = ls())

packages <- c("tidyverse","readxl","here","lubridate")


lapply(packages, require,character.only =T)


###############################################
############### First preprocessing the T_min dataframe ###
###################################

Tmin <- read_excel(here("Raw_Datasets",
                      "Historical_Weather_Data",
                      "Historical_Tmin_1976_2022.xlsx")) %>% 
                   mutate(StdTime = as.Date(StdTime)) %>% 
                   mutate(full_Date = format(StdTime,format = "%Y %b %d")) %>%
                   separate(full_Date, into = c("YEAR","MONTH","DAY"), 
                            sep = " ") %>% 
                  select(-c(DAY,OBJECTID,Join_Count,TARGET_FID,LOCATIONID,X,Y,
                            StdTime,Dimensions)) %>% 
                  pivot_wider(names_from =  MONTH,
                              values_from = tmin) %>% 
                  select(-state_name)

#### renaming the names of the dataframe ##### 

Old_names <- colnames(Tmin)

New_names <- c("State","County","Longitude","Latitude",
               "Year","Jan_Tmin","Feb_Tmin","Mar_Tmin",
               "Apr_Tmin","May_Tmin","Jun_Tmin","Jul_Tmin",
               "Aug_Tmin","Sep_Tmin","Oct_Tmin","Nov_Tmin",
               "Dec_Tmin")


Tmin <- Tmin %>% rename_at(vars(Old_names), ~ New_names)




#######################################
############### Now for the Tmax data #### 
################


Tmax <- read_excel(here("Raw_Datasets",
                        "Historical_Weather_Data",
                        "Historical_Tmax_1976_2022.xlsx")) %>% 
        mutate(StdTime = as.Date(StdTime)) %>% 
        mutate(full_Date = format(StdTime,format = "%Y %b %d")) %>%
        separate(full_Date, into = c("YEAR","MONTH","DAY"), 
           sep = " ") %>% 
        select(-c(DAY,OBJECTID,Join_Count,TARGET_FID,LOCATIONID,X,Y,
            StdTime,Dimensions)) %>% 
        pivot_wider(names_from =  MONTH,
              values_from = tmax) %>% 
        select(-state_name)

#### renaming the names of the dataframe ##### 

Old_names <- colnames(Tmax)

New_names <- c("State","County","Longitude","Latitude",
               "Year","Jan_Tmax","Feb_Tmax","Mar_Tmax",
               "Apr_Tmax","May_Tmax","Jun_Tmax","Jul_Tmax",
               "Aug_Tmax","Sep_Tmax","Oct_Tmax","Nov_Tmax",
               "Dec_Tmax")


Tmax <- Tmax %>% rename_at(vars(Old_names), ~ New_names)



#######################################
###### Now preprocessing the Precipitation data ###
########################## 

Precip <- read_excel(here("Raw_Datasets",
                        "Historical_Weather_Data",
                        "Historical_Precip_1976_2022.xlsx")) %>% 
          mutate(StdTime = as.Date(StdTime)) %>% 
          mutate(full_Date = format(StdTime,format = "%Y %b %d")) %>%
          separate(full_Date, into = c("YEAR","MONTH","DAY"), 
           sep = " ") %>% 
          select(-c(DAY,OBJECTID,Join_Count,TARGET_FID,LOCATIONID,X,Y,
            StdTime,Dimensions)) %>% 
          pivot_wider(names_from =  MONTH,
              values_from = prcp) %>% 
          select(-state_name)

#### renaming the names of the dataframe ##### 

Old_names <- colnames(Precip)

New_names <- c("State","County","Longitude","Latitude",
               "Year","Jan_Precip","Feb_Precip","Mar_Precip",
               "Apr_Precip","May_Precip","Jun_Precip","Jul_Precip",
               "Aug_Precip","Sep_Precip","Oct_Precip","Nov_Precip",
               "Dec_Precip")


Precip <- Precip %>% rename_at(vars(Old_names), ~ New_names)


###### Now merging the three dataframes together 

Climate_data <- list(Tmax,Tmin,Precip) %>% 
                                        reduce(inner_join,
                                               by = c("State","County",
                                                      "Latitude",
                                                  "Longitude","Year")) 


##### writing out this file ###

write.csv(Climate_data,
          "~/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Climate_data.csv",
          row.names = FALSE)











