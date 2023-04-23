
## Download future climate data ### 
rm(list = ls())


library(here)
library(geodata)



######### 2021-2040 ####

######## Tmin

path = "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Raw_Datasets/Future_Climate_Data/2021_2040_raster"

cmip6_world(model = 'ACCESS-ESM1-5',ssp = '126', time = "2021-2040",
            var = "tmin", res = 2.5, 
            path = path)


cmip6_world(model = 'ACCESS-ESM1-5',ssp = '245', time = "2021-2040",
            var = "tmin", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '370', time = "2021-2040",
            var = "tmin", res = 2.5, 
            path = path)


cmip6_world(model = 'ACCESS-ESM1-5',ssp = '585', time = "2021-2040",
            var = "tmin", res = 2.5, 
            path = path) 



####### Tmax 

cmip6_world(model = 'ACCESS-ESM1-5',ssp = '126', time = "2021-2040",
            var = "tmax", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '245', time = "2021-2040",
            var = "tmax", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '370', time = "2021-2040",
            var = "tmax", res = 2.5, 
            path = path)


cmip6_world(model = 'ACCESS-ESM1-5',ssp = '585', time = "2021-2040",
            var = "tmax", res = 2.5, 
            path = path) 



####### Precipitation 

cmip6_world(model = 'ACCESS-ESM1-5',ssp = '126', time = "2021-2040",
            var = "prec", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '245', time = "2021-2040",
            var = "prec", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '370', time = "2021-2040",
            var = "prec", res = 2.5, 
            path = path)


cmip6_world(model = 'ACCESS-ESM1-5',ssp = '585', time = "2021-2040",
            var = "prec", res = 2.5, 
            path = path) 





###################### 2041-2060 ############ 

######## Tmin

path = "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Raw_Datasets/Future_Climate_Data/2041_2060_raster"

cmip6_world(model = 'ACCESS-ESM1-5',ssp = '126', time = "2041-2060",
            var = "tmin", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '245', time = "2041-2060",
            var = "tmin", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '370', time = "2041-2060",
            var = "tmin", res = 2.5, 
            path = path)


cmip6_world(model = 'ACCESS-ESM1-5',ssp = '585', time = "2041-2060",
            var = "tmin", res = 2.5, 
            path = path) 

######## Tmax


cmip6_world(model = 'ACCESS-ESM1-5',ssp = '126', time = "2041-2060",
            var = "tmax", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '245', time = "2041-2060",
            var = "tmax", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '370', time = "2041-2060",
            var = "tmax", res = 2.5, 
            path = path)


cmip6_world(model = 'ACCESS-ESM1-5',ssp = '585', time = "2041-2060",
            var = "tmax", res = 2.5, 
            path = path) 


##### Precipitation  


cmip6_world(model = 'ACCESS-ESM1-5',ssp = '126', time = "2041-2060",
            var = "prec", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '245', time = "2041-2060",
            var = "prec", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '370', time = "2041-2060",
            var = "prec", res = 2.5, 
            path = path)


cmip6_world(model = 'ACCESS-ESM1-5',ssp = '585', time = "2041-2060",
            var = "prec", res = 2.5, 
            path = path) 



############## 2061-2080 #############

######## Tmin

path = "C:/Users/samba/Documents/Yield_forecast_Sunflowers/Yield_forecast_Sunflowers/Raw_Datasets/Future_Climate_Data/2061_2080_raster"

cmip6_world(model = 'ACCESS-ESM1-5',ssp = '126', time = "2061-2080",
            var = "tmin", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '245', time = "2061-2080",
            var = "tmin", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '370', time = "2061-2080",
            var = "tmin", res = 2.5, 
            path = path)


cmip6_world(model = 'ACCESS-ESM1-5',ssp = '585', time = "2061-2080",
            var = "tmin", res = 2.5, 
            path = path) 


########## Tmax 


cmip6_world(model = 'ACCESS-ESM1-5',ssp = '126', time = "2061-2080",
            var = "tmax", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '245', time = "2061-2080",
            var = "tmax", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '370', time = "2061-2080",
            var = "tmax", res = 2.5, 
            path = path)


cmip6_world(model = 'ACCESS-ESM1-5',ssp = '585', time = "2061-2080",
            var = "tmax", res = 2.5, 
            path = path) 


#### Precipitation 


cmip6_world(model = 'ACCESS-ESM1-5',ssp = '126', time = "2061-2080",
            var = "prec", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '245', time = "2061-2080",
            var = "prec", res = 2.5, 
            path = path)



cmip6_world(model = 'ACCESS-ESM1-5',ssp = '370', time = "2061-2080",
            var = "prec", res = 2.5, 
            path = path)


cmip6_world(model = 'ACCESS-ESM1-5',ssp = '585', time = "2061-2080",
            var = "prec", res = 2.5, 
            path = path) 




