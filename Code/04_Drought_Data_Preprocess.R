rm(list = ls())

packages <- c("tidyverse","readxl","here","lubridate")


lapply(packages, require,character.only =T)


###############################################
############### First preprocessing the SPEI GAMMA dataframe ###
###################################

Spei_gamma <- read_excel(here("Raw_Datasets",
                        "Historical_Drought_Data",
                        "SPEI_GAMMA.xlsx")) %>% 
  mutate(StdTime = as.Date(StdTime)) %>% 
  mutate(full_Date = format(StdTime,format = "%Y %b %d")) %>%
  separate(full_Date, into = c("YEAR","MONTH","DAY"), 
           sep = " ") %>% 
  select(-c(DAY,OBJECTID,Join_Count,TARGET_FID,LOCATIONID,X,Y,
            StdTime,Dimensions)) %>% 
  pivot_wider(names_from =  MONTH,
              values_from = spei_01) %>% 
  select(-state_name)

#### renaming the names of the dataframe ##### 

Old_names <- colnames(Spei_gamma)

New_names <- c("State","County","Longitude","Latitude",
               "Year","Jan_Spei_gamma","Feb_Spei_gamma",
               "Mar_Spei_gamma","Apr_Spei_gamma",
               "May_Spei_gamma","Jun_Spei_gamma","Jul_Spei_gamma",
               "Aug_Spei_gamma","Sep_Spei_gamma","Oct_Spei_gamma",
               "Nov_Spei_gamma","Dec_Spei_gamma")


Spei_gamma <- Spei_gamma %>% 
                 rename_at(vars(Old_names), ~ New_names)




#######################################
############### Now for the SPEI pearson data #### 
################


Spei_pearson <- read_excel(here("Raw_Datasets",
                        "Historical_Drought_Data",
                        "SPEI_PEARSON.xlsx")) %>% 
  mutate(StdTime = as.Date(StdTime)) %>% 
  mutate(full_Date = format(StdTime,format = "%Y %b %d")) %>%
  separate(full_Date, into = c("YEAR","MONTH","DAY"), 
           sep = " ") %>% 
  select(-c(DAY,OBJECTID,Join_Count,TARGET_FID,LOCATIONID,X,Y,
            StdTime,Dimensions)) %>% 
  pivot_wider(names_from =  MONTH,
              values_from = spei_01) %>% 
  select(-state_name)

#### renaming the names of the dataframe ##### 

Old_names <- colnames(Spei_pearson)

New_names <- c("State","County","Longitude","Latitude",
               "Year","Jan_Spei_pearson","Feb_Spei_pearson",
               "Mar_Spei_pearson","Apr_Spei_pearson",
               "May_Spei_pearson","Jun_Spei_pearson",
               "Jul_Spei_pearson","Aug_Spei_pearson",
               "Sep_Spei_pearson","Oct_Spei_pearson",
               "Nov_Spei_pearson","Dec_Spei_pearson")


Spei_pearson <- Spei_pearson %>% 
                  rename_at(vars(Old_names), ~ New_names)



#######################################
###### Now preprocessing the SPI gamma data ###
########################## 

Spi_gamma <- read_excel(here("Raw_Datasets",
                          "Historical_Drought_Data",
                          "SPI_GAMMA.xlsx")) %>% 
  mutate(StdTime = as.Date(StdTime)) %>% 
  mutate(full_Date = format(StdTime,format = "%Y %b %d")) %>%
  separate(full_Date, into = c("YEAR","MONTH","DAY"), 
           sep = " ") %>% 
  select(-c(DAY,OBJECTID,Join_Count,TARGET_FID,LOCATIONID,X,Y,
            StdTime,Dimensions)) %>% 
  pivot_wider(names_from =  MONTH,
              values_from = spi_01) %>% 
  select(-state_name)

#### renaming the names of the dataframe ##### 

Old_names <- colnames(Spi_gamma)

New_names <- c("State","County","Longitude","Latitude",
               "Year","Jan_Spi_gamma","Feb_Spi_gamma",
               "Mar_Spi_gamma","Apr_Spi_gamma","May_Spi_gamma",
               "Jun_Spi_gamma","Jul_Spi_gamma","Aug_Spi_gamma",
               "Sep_Spi_gamma","Oct_Spi_gamma","Nov_Spi_gamma",
               "Dec_Spi_gamma")


Spi_gamma <- Spi_gamma %>% 
                 rename_at(vars(Old_names), ~ New_names)

##################################################
################## Now processing SPI pearson ##
######## ################

Spi_pearson <- read_excel(here("Raw_Datasets",
                             "Historical_Drought_Data",
                             "SPI_PEARSON.xlsx")) %>% 
  mutate(StdTime = as.Date(StdTime)) %>% 
  mutate(full_Date = format(StdTime,format = "%Y %b %d")) %>%
  separate(full_Date, into = c("YEAR","MONTH","DAY"), 
           sep = " ") %>% 
  select(-c(DAY,OBJECTID,Join_Count,TARGET_FID,LOCATIONID,X,Y,
            StdTime,Dimensions)) %>% 
  pivot_wider(names_from =  MONTH,
              values_from = spi_01) %>% 
  select(-state_name)

#### renaming the names of the dataframe ##### 

Old_names <- colnames(Spi_pearson)


New_names <- c("State","County","Longitude","Latitude",
               "Year","Jan_Spi_pearson","Feb_Spi_pearson",
               "Mar_Spi_pearson","Apr_Spi_pearson","May_Spi_pearson",
               "Jun_Spi_pearson","Jul_Spi_pearson","Aug_Spi_pearson",
               "Sep_Spi_pearson","Oct_Spi_pearson","Nov_Spi_pearson",
               "Dec_Spi_pearson")


Spi_pearson <- Spi_pearson %>% 
                 rename_at(vars(Old_names), ~ New_names)







####### Now merging the four dataframes together 

Drought_data <- list(Spei_gamma,Spei_pearson,
                     Spi_gamma,Spi_pearson) %>% 
                             reduce(inner_join)

##### writing out this file ###

write.csv(Drought_data,
          "~/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Drought_data.csv",
          row.names = FALSE)


