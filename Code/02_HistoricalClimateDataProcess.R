
rm(list = ls())
#### Packages ###

packages <- list("here","tidyverse","ggplot2","sf","raster","stars","ncdf4",
                 "stringr")
lapply(packages, require,character.only = T)


source(here("Code","00_Functions.R"))

##### Data download #### 

### Maximum Temp 

#### setting a high timeout limit ## setting it to 10 minutes 

options(timeout=600)


# Define the URL
url <- "https://www.ncei.noaa.gov/data/nclimgrid-monthly/access/nclimgrid_tmax.nc"


# Define the destination file path using here()
dest_file <- here("RawDatasets", 
                  "nclimgrid_tmax.nc")


# Download the file
download.file(url, dest_file, mode = "wb")


### Minimum Temp 

url <- "https://www.ncei.noaa.gov/data/nclimgrid-monthly/access/nclimgrid_tmin.nc"


# Define the destination file path using here()
dest_file <- here("RawDatasets", 
                  "nclimgrid_tmin.nc")


# Download the file
download.file(url, dest_file, mode = "wb")


### Total precipitation

url <- "https://www.ncei.noaa.gov/data/nclimgrid-monthly/access/nclimgrid_prcp.nc"


# Define the destination file path using here()
dest_file <- here("RawDatasets", 
                  "nclimgrid_prcp.nc")


# Download the file
download.file(url, dest_file, mode = "wb")



### Read the climate data ###

# Read the NetCDF file
temp_data_max <- brick(here("RawDatasets",
                                 "nclimgrid_tmax.nc"))

temp_data_min <- brick(here("RawDatasets","nclimgrid_tmin.nc"))

prcp_data_total <- brick(here("RawDatasets","nclimgrid_prcp.nc"))


# Create a directory for the Historical weather data
extract_dir <- here("RawDatasets", "HistoricalTempMax")
extract_dir_2 <- here("RawDatasets", "HistoricalTempMin") 
extract_dir_3 <- here("RawDatasets", "HistoricalPrcp")

dir.create(extract_dir, showWarnings = FALSE)  
dir.create(extract_dir_2, showWarnings = FALSE) 
dir.create(extract_dir_3, showWarnings = FALSE) 



### Slice the netcdf files to only the layers we want 

#################
### these are the layers we want from the raster file #
### 973 to 1536

layer_indices <- 973:1536


# Specify the path to the 'HistoricalTempMax' folder
# Define the path to the HistoricalTempMax folder within the RawDatasets folder
output_dir <- here("RawDatasets", "HistoricalTempMax")
output_dir_2 <- here("RawDatasets", "HistoricalTempMin")
output_dir_3 <- here("RawDatasets", "HistoricalPrcp")


### export the layers from the netcdf files (temp max)


export_layers(raster_brick = temp_data_max,
              layer_indices = layer_indices,
              output_dir = output_dir)


export_layers(raster_brick = temp_data_min,
              layer_indices = layer_indices,
              output_dir = output_dir_2) 

export_layers(raster_brick = prcp_data_total,
              layer_indices = layer_indices,
              output_dir = output_dir_3)





### Read in the Sunflower Yield shapefile 
## and transforming the datasets to get accurate coordinates for this project ##


Sunflower_Yield <- st_read(here("Shapefiles","SunflowerYield",
                                "SunflowerYield.shp")) 



### Extract the coordinates from the file ###

Coordinates <- Sunflower_Yield %>% 
                    dplyr::select(YEAR, STATE,COUNTYN,LON,LAT) %>% 
                    sf::st_drop_geometry() %>% 
                    dplyr::filter(YEAR >= 1976 & YEAR <= 2020)


########### TMAX RASTERS #####

### use these coordinates to extract data at points from all rasters ###

## listing all tmax files 
rasters <- list.files(
  path = here("RawDatasets", "HistoricalTempMax"),
  pattern = ".*\\.tif$",
  full.names = TRUE
)

# Create a vector of search strings based on the unique years in your dataframe


unique_years <- unique(Coordinates$YEAR)

search_strings <- paste0(unique_years)

#########################################################


# EXTRACT VALUES AT THE COORDINATES 
temp_max_list <- geoRflow_raster_pipeline_point(inputs  = rasters,
                                                df = Coordinates,
                                                lat_col = "LAT",
                                                lon_col = "LON",
                                                split_id = "YEAR",
                                                search_strings = search_strings,
                                                method = "terra",
                                                resample_factor = NULL,
                                                crs = "EPSG:4326",
                                                method_resampling = "bilinear",
                                                no_data_value = -9999,
                                                reference_shape = NULL,
                                                use_bilinear = TRUE)



################ 
#### Extracting the list that contains the dataframes with values ###

# Define the new column names
new_column_names <- c("YEAR", "STATE", "COUNTYN", "LON", "LAT", 
                      "Jan_tmax", "Feb_tmax", "Mar_tmax", "Apr_tmax", 
                      "May_tmax", "June_tmax", "July_tmax", "August_tmax", 
                      "Sept_tmax", "Oct_tmax", "Nov_tmax", "Dec_tmax")


Max_temp <- process_dataframes(temp_max_list,
                               "dataframes_with_values",
                   new_col_names = new_column_names,
                   extract_index = 1)




############ TMIN ###########

####### 
### use these coordinates to extract data at points from all rasters ###

## listing all tmax files 
rasters_2 <- list.files(
  path = here("RawDatasets", "HistoricalTempMin"),
  pattern = ".*\\.tif$",
  full.names = TRUE
)

# Create a vector of search strings based on the unique years in your dataframe


unique_years <- unique(Coordinates$YEAR)

search_strings <- paste0(unique_years)

#########################################################


# Set up the call to raster_pipeline_point
temp_min_list <- geoRflow_raster_pipeline_point(inputs  = rasters_2,
                                                df = Coordinates,
                                                lat_col = "LAT",
                                                lon_col = "LON",
                                                split_id = "YEAR",
                                                search_strings = search_strings,
                                                method = "terra",
                                                resample_factor = NULL,
                                                crs = "EPSG:4326",
                                                method_resampling = "bilinear",
                                                no_data_value = -9999,
                                                reference_shape = NULL,
                                                use_bilinear = TRUE)



################ 
#### Extracting the list that contains the dataframes with values ###

# Define the new column names
new_column_names <- c("YEAR", "STATE", "COUNTYN", "LON", "LAT", 
                      "Jan_tmin", "Feb_tmin", "Mar_tmin", "Apr_tmin", 
                      "May_tmin", "June_tmin", "July_tmin", "August_tmin", 
                      "Sept_tmin", "Oct_tmin", "Nov_tmin", "Dec_tmin")


Min_temp <- process_dataframes(temp_min_list,
                               "dataframes_with_values",
                               new_col_names = new_column_names,
                               extract_index = 1)



#####################
#### PRCP #############
############# 

####### 
### use these coordinates to extract data at points from all rasters ###

## listing all tmax files 
rasters_3 <- list.files(
  path = here("RawDatasets", "HistoricalPrcp"),
  pattern = ".*\\.tif$",
  full.names = TRUE
)

# Create a vector of search strings based on the unique years in your dataframe


unique_years <- unique(Coordinates$YEAR)

search_strings <- paste0(unique_years)

#########################################################


# Set up the call to raster_pipeline_point
prcp_list <- geoRflow_raster_pipeline_point(inputs  = rasters_3,
                                                df = Coordinates,
                                                lat_col = "LAT",
                                                lon_col = "LON",
                                                split_id = "YEAR",
                                                search_strings = search_strings,
                                                method = "terra",
                                                resample_factor = NULL,
                                                crs = "EPSG:4326",
                                                method_resampling = "bilinear",
                                                no_data_value = -9999,
                                                reference_shape = NULL,
                                                use_bilinear = TRUE)



################ 
#### Extracting the list that contains the dataframes with values ###

# Define the new column names
new_column_names <- c("YEAR", "STATE", "COUNTYN", "LON", "LAT", 
                      "Jan_prcp", "Feb_prcp", "Mar_prcp", "Apr_prcp", 
                      "May_prcp", "June_prcp", "July_prcp", "August_prcp", 
                      "Sept_prcp", "Oct_prcp", "Nov_prcp", "Dec_prcp")


Total_prcp <- process_dataframes(prcp_list,
                               "dataframes_with_values",
                               new_col_names = new_column_names,
                               extract_index = 1)



#######
### Joining the climate data into one dataframe ####
### 

Climate_data <- list(Max_temp,Min_temp,Total_prcp) %>%
                                      purrr::reduce(inner_join)



###### Now adding the climate data to the Yield data ###

Yield <- Sunflower_Yield %>% 
                      sf::st_drop_geometry() %>%
                      dplyr::filter(YEAR >= 1976 & YEAR <=2020) %>%
                      dplyr::inner_join(Climate_data)



########
### writing out this dataset ###

### Creating an output folder ###

ProcessedDatasets <- here("ProcessedDatasets")

dir.create(ProcessedDatasets, showWarnings = FALSE) 

path <- here("ProcessedDatasets","HistoricalYieldClimateData.csv")

write.csv(Yield,path,row.names = FALSE)


























