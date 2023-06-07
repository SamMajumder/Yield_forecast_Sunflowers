
rm(list = ls())

packages <- list("tidyverse","here","readxl")

### lets load all the packages ###
lapply(packages, require,character.only=T)


###

source(here::here("Code","00_Functions.R"))


###################
######## FUTURE YEARS BETWEEN 2021 to 2040 #####
####################################

### Setting up the arguments of the function 

path_1 <- here::here("Raw_Datasets",
               "Future_Climate_Data",
               "2021_2040_csv","126") 


path_2 <- here::here("Raw_Datasets",
               "Future_Climate_Data",
               "2021_2040_csv","245")


path_3 <- here::here("Raw_Datasets",
               "Future_Climate_Data",
               "2021_2040_csv","370") 



path_4 <- here::here("Raw_Datasets",
               "Future_Climate_Data",
               "2021_2040_csv","585") 



RCP_1 <- "126"

RCP_2 <- "245" 

RCP_3 <- "370"

RCP_4 <- "585"

file_pattern <- "*.xlsx"

full_names <- T

Year <- "2040"

Pattern_to_remove_1 <- "ProjectRaster_OutRaster_wc2_1_2_5m_"

Pattern_to_remove_126 <- "ACCESS_ESM1_5_ssp126_"  

Pattern_to_remove_245 <- "ACCESS_ESM1_5_ssp245_"  

Pattern_to_remove_370 <- "ACCESS_ESM1_5_ssp370_"  

Pattern_to_remove_585 <- "ACCESS_ESM1_5_ssp585_"


Future_Data_2040 <- Future_data_compile(path_1 = path_1, path_2 = path_2,
                                        path_3 = path_3,path_4 = path_4,
                                        RCP_1 = RCP_1, RCP_2 = RCP_2,
                                        RCP_3 = RCP_3, RCP_4 = RCP_4,
                                        Year = Year, 
                                        file_pattern = file_pattern,
                                        full_names = full_names,
                                        Pattern_to_remove_1 = Pattern_to_remove_1,
                                        Pattern_to_remove_126 = Pattern_to_remove_126,
                                        Pattern_to_remove_245 = Pattern_to_remove_245,
                                        Pattern_to_remove_370 = Pattern_to_remove_370,
                                        Pattern_to_remove_585 = Pattern_to_remove_585)



##### unlisting the dataframes to the global environment ### 
## and writing them out one by one ###

list2env(Future_Data_2040,envir = .GlobalEnv)

#### renaming the column names #### 

#### extracting the old names

Old_names <- colnames(RCP_126) 


### setting up the names 
New_names <- Old_names %>% 
  str_replace("prec_202","Jan_Precip") %>% 
  str_replace("prec_203","Feb_Precip") %>%
  str_replace("prec_204","Mar_Precip") %>% 
  str_replace("prec_205","Apr_Precip") %>% 
  str_replace("prec_206","May_Precip") %>% 
  str_replace("prec_207","Jun_Precip") %>% 
  str_replace("prec_208","Jul_Precip") %>% 
  str_replace("prec_209","Aug_Precip") %>% 
  str_replace("prec_210","Sep_Precip") %>% 
  str_replace("prec_211","Oct_Precip") %>% 
  str_replace("prec_212","Nov_Precip") %>% 
  str_replace("prec_213","Dec_Precip") %>% 
  str_replace("tmax_202","Jan_Tmax") %>% 
  str_replace("tmax_203","Feb_Tmax") %>%
  str_replace("tmax_204","Mar_Tmax") %>% 
  str_replace("tmax_205","Apr_Tmax") %>% 
  str_replace("tmax_206","May_Tmax") %>% 
  str_replace("tmax_207","Jun_Tmax") %>% 
  str_replace("tmax_208","Jul_Tmax") %>% 
  str_replace("tmax_209","Aug_Tmax") %>% 
  str_replace("tmax_210","Sep_Tmax") %>% 
  str_replace("tmax_211","Oct_Tmax") %>% 
  str_replace("tmax_212","Nov_Tmax") %>% 
  str_replace("tmax_213","Dec_Tmax") %>%
  str_replace("tmin_202","Jan_Tmin") %>% 
  str_replace("tmin_203","Feb_Tmin") %>%
  str_replace("tmin_204","Mar_Tmin") %>% 
  str_replace("tmin_205","Apr_Tmin") %>% 
  str_replace("tmin_206","May_Tmin") %>% 
  str_replace("tmin_207","Jun_Tmin") %>% 
  str_replace("tmin_208","Jul_Tmin") %>% 
  str_replace("tmin_209","Aug_Tmin") %>% 
  str_replace("tmin_210","Sep_Tmin") %>% 
  str_replace("tmin_211","Oct_Tmin") %>% 
  str_replace("tmin_212","Nov_Tmin") %>% 
  str_replace("tmin_213","Dec_Tmin")


### Now renaming the columns of the data frame ## RCP 126

RCP_126 <- RCP_126 %>% 
                  rename_at(vars(Old_names), ~ New_names)

write.csv(RCP_126,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Future_Data_2040_126.csv",
          row.names=F)


### Now renaming the columns of the data frame ## RCP 245

RCP_245 <- RCP_245 %>% 
                 rename_at(vars(Old_names), ~ New_names)


write.csv(RCP_245,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Future_Data_2040_245.csv",
          row.names=F)


### Now renaming the columns of the data frame ## RCP 370

RCP_370 <- RCP_370 %>% 
              rename_at(vars(Old_names), ~ New_names)

write.csv(RCP_370,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Future_Data_2040_370.csv",
          row.names=F)


### Now renaming the columns of the data frame ## RCP 370

RCP_585 <- RCP_585 %>% 
                 rename_at(vars(Old_names), ~ New_names) 

write.csv(RCP_585,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Future_Data_2040_585.csv",
          row.names=F)


###################
######## FUTURE YEARS BETWEEN 2041 to 2060 #####
####################################

rm(list = ls())


###

source(here("Code","00_Functions.R"))


### Setting up the arguments of the function 

path_1 <- here("Raw_Datasets",
             "Future_Climate_Data",
             "2041_2060_csv","126") 


path_2 <- here("Raw_Datasets",
               "Future_Climate_Data",
               "2041_2060_csv","245")


path_3 <- here("Raw_Datasets",
               "Future_Climate_Data",
               "2041_2060_csv","370") 



path_4 <- here("Raw_Datasets",
               "Future_Climate_Data",
               "2041_2060_csv","585") 



RCP_1 <- "126"

RCP_2 <- "245" 

RCP_3 <- "370"

RCP_4 <- "585"

file_pattern <- "*.xlsx"

full_names <- T

Year <- "2060"

Pattern_to_remove_1 <- "ProjectRaster_OutRaster_wc2_1_2_5m_"

Pattern_to_remove_126 <- "ACCESS_ESM1_5_ssp126_"  

Pattern_to_remove_245 <- "ACCESS_ESM1_5_ssp245_"  

Pattern_to_remove_370 <- "ACCESS_ESM1_5_ssp370_"  

Pattern_to_remove_585 <- "ACCESS_ESM1_5_ssp585_"


Future_Data_2060 <- Future_data_compile(path_1 = path_1, path_2 = path_2,
                                          path_3 = path_3,path_4 = path_4,
                                          RCP_1 = RCP_1, RCP_2 = RCP_2,
                                          RCP_3 = RCP_3, RCP_4 = RCP_4,
                                          Year = Year, 
                                          file_pattern = file_pattern,
                                          full_names = full_names,
                                          Pattern_to_remove_1 = Pattern_to_remove_1,
                                          Pattern_to_remove_126 = Pattern_to_remove_126,
                                          Pattern_to_remove_245 = Pattern_to_remove_245,
                                          Pattern_to_remove_370 = Pattern_to_remove_370,
                                          Pattern_to_remove_585 = Pattern_to_remove_585)



##### unlisting the dataframes to the global environment ### 
## and writing them out one by one ###

list2env(Future_Data_2060,envir = .GlobalEnv)

#### renaming the column names #### 

#### extracting the old names

Old_names <- colnames(RCP_126) 


### setting up the names 
New_names <- Old_names %>% 
  str_replace("prec_204","Jan_Precip") %>% 
  str_replace("prec_205","Feb_Precip") %>%
  str_replace("prec_206","Mar_Precip") %>% 
  str_replace("prec_207","Apr_Precip") %>% 
  str_replace("prec_208","May_Precip") %>% 
  str_replace("prec_209","Jun_Precip") %>% 
  str_replace("prec_210","Jul_Precip") %>% 
  str_replace("prec_211","Aug_Precip") %>% 
  str_replace("prec_212","Sep_Precip") %>% 
  str_replace("prec_213","Oct_Precip") %>% 
  str_replace("prec_214","Nov_Precip") %>% 
  str_replace("prec_215","Dec_Precip") %>% 
  str_replace("tmax_204","Jan_Tmax") %>% 
  str_replace("tmax_205","Feb_Tmax") %>%
  str_replace("tmax_206","Mar_Tmax") %>% 
  str_replace("tmax_207","Apr_Tmax") %>% 
  str_replace("tmax_208","May_Tmax") %>% 
  str_replace("tmax_209","Jun_Tmax") %>% 
  str_replace("tmax_210","Jul_Tmax") %>% 
  str_replace("tmax_211","Aug_Tmax") %>% 
  str_replace("tmax_212","Sep_Tmax") %>% 
  str_replace("tmax_213","Oct_Tmax") %>% 
  str_replace("tmax_214","Nov_Tmax") %>% 
  str_replace("tmax_215","Dec_Tmax") %>%
  str_replace("tmin_204","Jan_Tmin") %>% 
  str_replace("tmin_205","Feb_Tmin") %>%
  str_replace("tmin_206","Mar_Tmin") %>% 
  str_replace("tmin_207","Apr_Tmin") %>% 
  str_replace("tmin_208","May_Tmin") %>% 
  str_replace("tmin_209","Jun_Tmin") %>% 
  str_replace("tmin_210","Jul_Tmin") %>% 
  str_replace("tmin_211","Aug_Tmin") %>% 
  str_replace("tmin_212","Sep_Tmin") %>% 
  str_replace("tmin_213","Oct_Tmin") %>% 
  str_replace("tmin_214","Nov_Tmin") %>% 
  str_replace("tmin_215","Dec_Tmin")


### Now renaming the columns of the data frame ## RCP 126

RCP_126 <- RCP_126 %>% 
              rename_at(vars(Old_names), ~ New_names)


write.csv(RCP_126,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Future_Data_2060_126.csv",
          row.names=F)


### Now renaming the columns of the data frame ## RCP 245

RCP_245 <- RCP_245 %>% 
               rename_at(vars(Old_names), ~ New_names)


write.csv(RCP_245,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Future_Data_2060_245.csv",
          row.names=F)



### Now renaming the columns of the data frame ## RCP 370

RCP_370 <- RCP_370 %>% 
               rename_at(vars(Old_names), ~ New_names)


write.csv(RCP_370,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Future_Data_2060_370.csv",
          row.names=F)



### Now renaming the columns of the data frame ## RCP 370

RCP_585 <- RCP_585 %>% 
               rename_at(vars(Old_names), ~ New_names)


write.csv(RCP_585,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Future_Data_2060_585.csv",
          row.names=F)



#############################

###################
######## FUTURE YEARS BETWEEN 2061 to 2080 #####
####################################

rm(list = ls())

###

source(here("Code","00_Functions.R"))

### Setting up the arguments of the function 

path_1 <- here("Raw_Datasets",
               "Future_Climate_Data",
               "2061_2080_csv","126") 


path_2 <- here("Raw_Datasets",
               "Future_Climate_Data",
               "2061_2080_csv","245")


path_3 <- here("Raw_Datasets",
               "Future_Climate_Data",
               "2061_2080_csv","370") 



path_4 <- here("Raw_Datasets",
               "Future_Climate_Data",
               "2061_2080_csv","585") 



RCP_1 <- "126"

RCP_2 <- "245" 

RCP_3 <- "370"

RCP_4 <- "585"

file_pattern <- "*.xlsx"

full_names <- T

Year <- "2080"

Pattern_to_remove_1 <- "ProjectRaster_OutRaster_wc2_1_2_5m_"

Pattern_to_remove_126 <- "ACCESS_ESM1_5_ssp126_"  

Pattern_to_remove_245 <- "ACCESS_ESM1_5_ssp245_"  

Pattern_to_remove_370 <- "ACCESS_ESM1_5_ssp370_"  

Pattern_to_remove_585 <- "ACCESS_ESM1_5_ssp585_"


Future_Data_2080 <- Future_data_compile(path_1 = path_1, path_2 = path_2,
                                        path_3 = path_3,path_4 = path_4,
                                        RCP_1 = RCP_1, RCP_2 = RCP_2,
                                        RCP_3 = RCP_3, RCP_4 = RCP_4,
                                        Year = Year, 
                                        file_pattern = file_pattern,
                                        full_names = full_names,
                                        Pattern_to_remove_1 = Pattern_to_remove_1,
                                        Pattern_to_remove_126 = Pattern_to_remove_126,
                                        Pattern_to_remove_245 = Pattern_to_remove_245,
                                        Pattern_to_remove_370 = Pattern_to_remove_370,
                                        Pattern_to_remove_585 = Pattern_to_remove_585)




######
##### unlisting the dataframes to the global environment ### 
## and writing them out one by one ###

list2env(Future_Data_2080,envir = .GlobalEnv)

#### renaming the column names #### 

#### extracting the old names

Old_names <- colnames(RCP_126) 


### setting up the names 
New_names <- Old_names %>% 
  str_replace("prec_206","Jan_Precip") %>% 
  str_replace("prec_207","Feb_Precip") %>%
  str_replace("prec_208","Mar_Precip") %>% 
  str_replace("prec_209","Apr_Precip") %>% 
  str_replace("prec_210","May_Precip") %>% 
  str_replace("prec_211","Jun_Precip") %>% 
  str_replace("prec_212","Jul_Precip") %>% 
  str_replace("prec_213","Aug_Precip") %>% 
  str_replace("prec_214","Sep_Precip") %>% 
  str_replace("prec_215","Oct_Precip") %>% 
  str_replace("prec_216","Nov_Precip") %>% 
  str_replace("prec_217","Dec_Precip") %>% 
  str_replace("tmax_206","Jan_Tmax") %>% 
  str_replace("tmax_207","Feb_Tmax") %>%
  str_replace("tmax_208","Mar_Tmax") %>% 
  str_replace("tmax_209","Apr_Tmax") %>% 
  str_replace("tmax_210","May_Tmax") %>% 
  str_replace("tmax_211","Jun_Tmax") %>% 
  str_replace("tmax_212","Jul_Tmax") %>% 
  str_replace("tmax_213","Aug_Tmax") %>% 
  str_replace("tmax_214","Sep_Tmax") %>% 
  str_replace("tmax_215","Oct_Tmax") %>% 
  str_replace("tmax_216","Nov_Tmax") %>% 
  str_replace("tmax_217","Dec_Tmax") %>%
  str_replace("tmin_206","Jan_Tmin") %>% 
  str_replace("tmin_207","Feb_Tmin") %>%
  str_replace("tmin_208","Mar_Tmin") %>% 
  str_replace("tmin_209","Apr_Tmin") %>% 
  str_replace("tmin_210","May_Tmin") %>% 
  str_replace("tmin_211","Jun_Tmin") %>% 
  str_replace("tmin_212","Jul_Tmin") %>% 
  str_replace("tmin_213","Aug_Tmin") %>% 
  str_replace("tmin_214","Sep_Tmin") %>% 
  str_replace("tmin_215","Oct_Tmin") %>% 
  str_replace("tmin_216","Nov_Tmin") %>% 
  str_replace("tmin_217","Dec_Tmin")


### Now renaming the columns of the data frame ## RCP 126

RCP_126 <- RCP_126 %>% 
              rename_at(vars(Old_names), ~ New_names)

write.csv(RCP_126,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Future_Data_2080_126.csv",
          row.names=F)


### Now renaming the columns of the data frame ## RCP 245

RCP_245 <- RCP_245 %>% 
              rename_at(vars(Old_names), ~ New_names)


write.csv(RCP_245,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Future_Data_2080_245.csv",
          row.names=F)



Future_Data_2080_370 <- Future_Data_2080[[3]] 

write.csv(RCP_370,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Future_Data_2080_370.csv",
          row.names=F)



Future_Data_2080_585 <- Future_Data_2080[[4]] 

write.csv(RCP_585,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Future_Data_2080_585.csv",
          row.names=F)













