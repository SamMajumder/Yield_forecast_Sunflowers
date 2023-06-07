

rm(list = ls())

packages <- c("sf","tidyverse","here")

lapply(packages, require,character.only = T)


### Reading in the data and                   
#### Recoding the full state names to abbreviatiojns ###

Regions <- read.csv(here("Raw_Datasets",
                         "Regions",
                         "Study_Regions.csv")) %>% 
                   mutate(STATE = case_match(state_name, 
                                             "COLORADO" ~ "CO",
                                             "KANSAS" ~ "KS",
                                              "NEBRASKA" ~ "NE",
                                             "MINNESOTA" ~ "MN",
                                             "NORTH DAKOTA" ~ "ND",
                                             "SOUTH DAKOTA" ~ "SD",
                                             "TEXAS" ~ "TX")) %>% 
                   rename(COUNTYNAME = county_name) %>% 
                   select(-c(Latest,Oldest))



###########
## Extracting the coordinates (centroids) from all US counties###
######3

US_states <- st_read("~/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Raw_Datasets/Shape_Files/c_08mr23/c_08mr23.shp")

US_states <- US_states %>% 
                     st_drop_geometry() %>% 
                     select(STATE,COUNTYNAME,LON,LAT)


Study_States <- c("ND","SD","TX","KS","NE","MN","CO")

States <- filter(US_states,STATE %in% Study_States)

### making the state and county names Upper case ##
### The county name LA MOURE is LAMOURE IN THE centroid file##
### So I am editing that to match the Regions file ##

States <- States %>% 
          mutate(COUNTYNAME = toupper(COUNTYNAME)) %>% 
          mutate(COUNTYNAME = recode(COUNTYNAME,
                                     "LAMOURE" = "LA MOURE"))
          

### Joining the dataframe containing coordinates and the dataframe
## containing county names ###

Study_Counties <- States %>% 
                           inner_join(Regions)

##

### exporting coordinates ### 
## We will need this information to extract historical climate data ###
##

write.csv(Study_Counties,
          "~/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Raw_Datasets/Regions/Study_coordinates.csv",
          row.names = F)



