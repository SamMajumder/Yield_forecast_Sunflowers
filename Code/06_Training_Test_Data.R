
### Creating a combined data table with Yield, Historical Climate and Drought ##
## 

rm(list = ls())

packages <- list("here","tidyverse")

lapply(packages, require,character.only =T)


source(here("Code","00_Functions.R"))

## reading in the Yield data ###
##### 

Yield <- read.csv(here("Raw_Datasets","Yield","Sunflower_Yield.csv")) 

### Renaming the columns to match the other subsequent dataframes ###

Yield <- Yield %>% 
               rename(State = state_name,
                          County = county_name,
                          Year = year)  %>% 
                          mutate(State = case_match(State,
                                 "COLORADO" ~ "CO",
                                 "SOUTH DAKOTA" ~ "SD",
                                 "NORTH DAKOTA" ~ "ND",
                                 "TEXAS" ~ "TX",
                                 "KANSAS" ~ "KS",
                                 "MINNESOTA" ~ "MN",
                                 "NEBRASKA" ~ "NE"))


#### Climate data ### 

Weather <- read.csv(here("Processed_Datasets","Weather_data.csv"))

#### Drought Data data ###

Drought <- read.csv(here("Processed_Datasets","Drought_data.csv"))






###################
#### Joining the yield, climate and drought data ### 
############### and dropping NA values 

Data <- list(Yield,Weather,Drought) %>% 
                               reduce(inner_join) %>% 
                               drop_na()


#############
### Creating Column with Crop Loss Data ###
########## 

Data$Crop_Loss <- (Data$Acres_Planted-Data$Acres_Harvested)/Data$Acres_Planted



############ add a spacevar column ##
## space id ### 


## first finding out how many time each year is represented 
Year_frequency <- data.frame(Frequency = table(Data$Year))

Space_Var <- data.frame(Space_Var = rep(c(1:46), 
                                        times=Year_frequency$Frequency.Freq))

### Adding this column to the main dataset ###

Data <- Data %>% arrange(Year) %>% 
        mutate(SpaceVar = Space_Var$Space_Var)


#### Write this dataframe out ### 

here("Processed_Datasets")

write.csv(Data,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Combined_Dataset.csv",
          row.names = FALSE)


#######################################
## Getting the latest Agriculture practices data from each STATE and COUNTY ###
#####

Ag_practices_data <- Data %>% select(Year,State,County,
                                Acres_Planted,Acres_Harvested)


Ag_practices_latest <- Ag_practices_data %>% 
                             aggregate(Year ~ County,max) %>% 
                                                 left_join(Ag_practices_data)

###########################
### Getting the latest drought index data from each STATE and COUNTY ###

### This dataset only contains the data from the 177 counties in our study
Drought_data <- Data %>% 
                        select(Year,State,County,
                               Jan_Spei_gamma:Dec_Spi_pearson)


Drought_data_latest <- Drought_data %>% 
                               aggregate(Year ~ County,max) %>% 
                               left_join(Drought_data)



###### Divide the dataset into a training and a test dataset based on time # 
### 2004 is the cut off point ###


train <- Data %>% filter(Year <= 2004)

test <- Data %>% filter(Year >= 2005)


######
## writing these files out ## 

## get the path 
here("Processed_Datasets")

### train ###

write.csv(train,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/train.csv",
          row.names = FALSE)


### test ###

write.csv(test,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/test.csv",
          row.names = FALSE)


##### Ag practices ###

write.csv(Ag_practices_latest,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Ag_practices_latest.csv")


##### Drought data ####

write.csv(Drought_data_latest,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/Drought_data_latest.csv")



####################
## now creating separate dataframe per state ###
################# 

## First removing the SpaceVar column as we need a new one for each state###
##

Data <- Data %>% select(-SpaceVar)


Data_groups <- split(Data,f=Data$State)

#### unlist all elements within this group ###

list2env(Data_groups,envir = .GlobalEnv)

########################
## create space time folds ###

## first finding out how many time each year is represented 
Year_frequency <- data.frame(Frequency = table(ND$Year))

Space_Var <- data.frame(Space_Var = rep(c(1:46), 
                                        times=Year_frequency$Frequency.Freq))


### Add this variable to the ND dataset ### 

ND <- ND %>% arrange(Year) %>% 
             mutate(SpaceVar = Space_Var$Space_Var)


###############
## creating a train and test for each state ####
###########
### NORTH DAKOTA ####

###### Divide the dataset into a training and a test dataset based on time # 

train_ND <- ND %>% filter(Year <= 2004)

test_ND <- ND %>% filter(Year >= 2005) 


## writing out the train dataset

write.csv(train_ND,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/train_ND.csv",
          row.names = FALSE)


### test ###

write.csv(test_ND,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/test_ND.csv",
          row.names = FALSE)



############
## SOUTH DAKOTA ###
#######

### create space time folds

## first finding out how many time each year is represented 
Year_frequency <- data.frame(Frequency = table(SD$Year))

Space_Var <- data.frame(Space_Var = rep(c(1:42), 
                                        times=Year_frequency$Frequency.Freq))


### Add this variable to the ND dataset ### 

### Adding this column to the main dataset ###

SD <- SD %>% arrange(Year) %>% 
      mutate(SpaceVar = Space_Var$Space_Var)


###### Divide the dataset into a training and a test dataset based on time # 

train_SD <- SD %>% filter(Year <= 2004)

test_SD <- SD %>% filter(Year >= 2005) 

## writing out the train and test dataset 

write.csv(train_SD,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/train_SD.csv",
          row.names = FALSE)


### test ###

write.csv(test_SD,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/test_SD.csv",
          row.names = FALSE)


################# 
### COLORADO ###
############ 

### create space time folds

## first finding out how many time each year is represented 
Year_frequency <- data.frame(Frequency = table(CO$Year))

Space_Var <- data.frame(Space_Var = rep(c(1:26), 
                                        times=Year_frequency$Frequency.Freq))


## Adding this to the Colorado dataset ##
##################

CO <- CO %>% arrange(Year) %>% 
      mutate(SpaceVar = Space_Var$Space_Var)




###### Divide the dataset into a training and a test dataset based on time # 


train_CO <- CO %>% filter(Year <= 2004)

test_CO <- CO %>% filter(Year >= 2005) 


## writing out the train and test dataset 

write.csv(train_CO,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/train_CO.csv",
          row.names = FALSE)


### test ###

write.csv(test_CO,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/test_CO.csv",
          row.names = FALSE)


###################
#### MINNESOTA ###
################# 

### create space time folds ###

## first finding out how many time each year is represented 
Year_frequency <- data.frame(Frequency = table(MN$Year))

Space_Var <- data.frame(Space_Var = rep(c(1:40), 
                                        times=Year_frequency$Frequency.Freq))


## Adding this to the Minnesota dataset
##################

MN <- MN %>% arrange(Year) %>% 
      mutate(SpaceVar = Space_Var$Space_Var)

##############


train_MN <- MN %>% filter(Year <= 2004)

test_MN <- MN %>% filter(Year >= 2005)  


## writing out the train and test dataset 

write.csv(train_MN,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/train_MN.csv",
          row.names = FALSE)


### val ###

write.csv(test_MN,
          "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/test_MN.csv",
          row.names = FALSE)



#######################
### TEXAS ###
############# 

### create space time folds 


## first finding out how many time each year is represented 
Year_frequency <- data.frame(Frequency = table(TX$Year))

Space_Var <- data.frame(Space_Var = rep(c(1:39), 
                                        times=Year_frequency$Frequency.Freq))

#########
## Adding this to the Texas dataset
##################

TX <- TX %>% arrange(Year) %>% 
      mutate(SpaceVar = Space_Var$Space_Var)


train_TX <- TX %>% filter(Year <= 2004)

test_TX <- TX %>% filter(Year >= 2005) 


## writing out the train and test dataset 

write.csv(train_TX,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/train_TX.csv",
          row.names = FALSE)


### val ###

write.csv(test_TX,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/test_TX.csv",
          row.names = FALSE)



################
## NEBRASKA ###
#################### 

### create space time folds ###

## first finding out how many time each year is represented 
Year_frequency <- data.frame(Frequency = table(NE$Year))

Space_Var <- data.frame(Space_Var = rep(c(1:22), 
                                        times=Year_frequency$Frequency.Freq))


## Adding this to the Nebraska dataset
##################

NE <- NE %>% arrange(Year) %>% 
      mutate(SpaceVar = Space_Var$Space_Var)


################

train_NE <- NE %>% filter(Year <= 2004)

test_NE <- NE %>% filter(Year >= 2005)  


## writing out the train and test dataset 

write.csv(train_NE,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/train_NE.csv",
          row.names = FALSE)


### val ###

write.csv(test_NE,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/test_NE.csv",
          row.names = FALSE)




#######################
### KANSAS ###
######## 


### create space time folds

## first finding out how many time each year is represented 
Year_frequency <- data.frame(Frequency = table(KS$Year))

Space_Var <- data.frame(Space_Var = rep(c(1:21), 
                                        times=Year_frequency$Frequency.Freq))



## Adding this to the Kansas dataset
##################

KS <- KS %>% arrange(Year) %>% 
      mutate(SpaceVar = Space_Var$Space_Var)


train_KS <- KS %>% filter(Year <= 2005)

test_KS <- KS %>% filter(Year >= 2006) 

## writing out the train and test dataset 

write.csv(train_KS,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/train_KS.csv",
          row.names = FALSE)


### test ###

write.csv(test_KS,"C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Processed_Datasets/test_KS.csv",
          row.names = FALSE)





