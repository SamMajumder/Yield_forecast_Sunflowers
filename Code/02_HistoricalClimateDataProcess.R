
rm(list = ls())
#### Packages ###

packages <- list("here","tidyverse","ggplot2","sf","raster","stars","ncdf4")
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


### export the layers from the netcdf files (temp max)


export_layers(raster_brick = temp_data_max,
              layer_indices = layer_indices,
              output_dir = output_dir)





### Read in the Sunflower Yield shapefile 


Sunflower_Yield <- st_read(here("Shapefiles","SunflowerYield",
                                "SunflowerYield.shp"))

### Extract the coordinates from the file ###

Coordinates <- Sunflower_Yield %>% 
                    dplyr::select(YEAR, STATE,COUNTYN,LON,LAT) %>% 
                    sf::st_drop_geometry() %>% 
                    dplyr::filter(YEAR >= 1976 & YEAR <= 2020)




### use these coordinates to extract data at points from all rasters ###

## listing all tmax files 
inputs <- list.files(
  path = here("RawDatasets", "HistoricalTempMax"),
  pattern = ".*\\.tif$",
  full.names = TRUE
)

# Create a vector of search strings based on the unique years in your dataframe

unique_years <- unique(Coordinates$YEAR)

search_strings <- paste0(unique_years)



#########################################################


# Set up the call to raster_pipeline_point
temp_max_list <- geoRflow_raster_pipeline_point(inputs = inputs,
                                          df = Coordinates,
                                          lat_col = "LAT",
                                          lon_col = "LON",
                                          split_id = "YEAR",
                                          search_strings = search_strings,
                                          method = "stars",
                                          resample_factor = NULL,
                                          crs = st_crs(9822),
                                          method_resampling = "bilinear",
                                          no_data_value = -9999,
                                          reference_shape = NULL,
                                          use_bilinear = TRUE)




temp_max_list_2 <- geoRflow_raster_pipeline_point(inputs = inputs,
                                                df = Coordinates,
                                                lat_col = "LAT",
                                                lon_col = "LON",
                                                split_id = "YEAR",
                                                search_strings = search_strings,
                                                method = "stars",
                                                resample_factor = NULL,
                                                crs = st_crs(4735),
                                                method_resampling = "bilinear",
                                                no_data_value = -9999,
                                                reference_shape = NULL,
                                                use_bilinear = TRUE)








####### 


raster_files_tmin <- list.files(path = here("RawDatasets", "HistoricalTempMin"), 
                                full.names = TRUE)

result <- geoRflow_raster_pipeline_point(
  inputs = inputs,  # replace with your list of raster file paths
  df = Coordinates,
  lat_col = "LAT",
  lon_col = "LON",
  split_id = "YEAR",
  search_strings = search_string_1976_1977,
  method = "stars",
  resample_factor = NULL,  # or specify a resample factor if needed
  crs = st_crs(4326),
  method_resampling = "bilinear",
  no_data_value = -9999,
  reference_shape = NULL,  # or specify a reference shape if needed
  use_bilinear = TRUE
)



##### Listing all prcp files ##### 

raster_files_prcp <- list.files(path = here("RawDatasets", "HistoricalPrcp"), 
                                full.names = TRUE)



#### Join the four datasets ###
### Tmax, Tmin, Precip, Yield 






























