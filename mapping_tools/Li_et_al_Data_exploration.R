library(R.matlab)
library(sf)
library(raster)
#library(sp)
library(concaveman)
library(terra)
library(mapview)
library(tidyverse)

#load in saved land shapfile
land.sf <- st_read("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/Shp/CAN_US_GRNLND.shp")

#Sediment mobility frequencies -------------------------------------------------------

data.csv <- read.csv("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/data/shearvel__SMF_Atlantic.csv")
data.sf <- st_as_sf(data.csv, coords = c("LON", "LAT"), crs = 4326) #Convert dataframe to sf object
plot(data.sf[8])

variable_ranges <- data.sf %>%
  st_drop_geometry() %>% # Drop geometry for non-spatial operations
  summarize(across(where(is.numeric), ~ list(range(.x, na.rm = TRUE))))

variable_ranges

#Wave and current data -------------------------------------------------------

data.csv <- read.csv("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/data/Current__wave_data_Atlantic.csv")
data.sf <- st_as_sf(data.csv, coords = c("LON", "LAT"), crs = 4326) #Convert dataframe to sf object
plot(data.sf[5])

variable_ranges <- data.sf %>%
  st_drop_geometry() %>% # Drop geometry for non-spatial operations
  summarize(across(where(is.numeric), ~ list(range(.x, na.rm = TRUE))))

variable_ranges

#Seabed Disturbance Index and Sediment Mobility Index  -------------------------------------------------------

data.csv <- read.csv("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/data/SDI__SMI_DistClass_Atlantic.csv")
data.sf <- st_as_sf(data.csv, coords = c("LON", "LAT"), crs = 4326) #Convert dataframe to sf object
plot(data.sf[2]) #SDI

variable_ranges <- data.sf %>%
  st_drop_geometry() %>% # Drop geometry for non-spatial operations
  summarize(across(where(is.numeric), ~ list(range(.x, na.rm = TRUE))))

variable_ranges

#Atlantic grain size (US, CAN, GREENLAND)  -------------------------------------------------------

data.csv <- read.csv("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/data/atl_grainsize_gridded.csv")
data.sf <- st_as_sf(data.csv, coords = c("long", "lat"), crs = 4326) #Convert dataframe to sf object

plot(data.sf[4]) #phi
plot(data.sf[5]) # mm

variable_ranges <- data.sf %>%
  st_drop_geometry() %>% # Drop geometry for non-spatial operations
  summarize(across(where(is.numeric), ~ list(range(.x, na.rm = TRUE))))

variable_ranges

#read in the data boundary shapefile for all data above  -------------------------------------------------------

concave_hull <- st_read("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/shp/concave_hull_boundary.shp")
# Calculate the concave hull (contour of the data points)
#concave_hull <- concaveman(data.sf, concavity = 2) 
#plot(concave_hull)
concave_hull_final <- st_difference(concave_hull, st_union(land.sf))
mapview(concave_hull_final)
# Write the concave hull to a shapefile
#st_write(concave_hull_final, "Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/shp/concave_hull_boundary.shp", driver = "ESRI Shapefile", append = FALSE)



#Canada Atlantic Grain size -------------------------------------------------------
#CANADA'S ATLANTIC COAST
data.csv <- read.csv("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/data/Canada_080603_Atlantic_grain size.csv")
data.sf <- st_as_sf(data.csv, coords = c("STN_LON", "STN_LAT"), crs = 4326) #Convert dataframe to sf object
plot(data.sf[11]) #GRAVEL
mapview(data.sf[11])


# Calculate the concave hull (contour of the data points)
concave_hull <- concaveman(data.sf, concavity = 2) 
plot(concave_hull)
mapview(data.sf[11])+
mapview(concave_hull)
concave_hull_final <- st_difference(concave_hull, st_union(land.sf))
mapview(concave_hull_final)
# Write the concave hull to a shapefile
#st_write(concave_hull_final, "Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/shp/concave_hull_atlantic_gs.shp", driver = "ESRI Shapefile", append = FALSE)

#USGS US seabed grainsize  -------------------------------------------------------
#GULF OF MAINE, GEORGES BANK, GERMAN BANK and BAY OF FUNDY
data.csv <- read.csv("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/data/USGS_ECST_usSEABED_grainsize.csv",check.names = F) #read.csv doesn't like the micron unit in the header
data.sf <- st_as_sf(data.csv, coords = c("LONG", "LAT"), crs = 4326) #Convert dataframe to sf object
plot(data.sf[16])
mapview(data.sf[16])


variable_ranges <- data.sf %>%
  st_drop_geometry() %>% # Drop geometry for non-spatial operations
  summarize(across(where(is.numeric), ~ list(range(.x, na.rm = TRUE))))

variable_ranges #-9999 values and NAs

summary(data.sf)

# Calculate the concave hull (contour of the data points)
#concave_hull <- concaveman(data.sf, concavity = 3) 
#plot(concave_hull)
#mapview(data.sf[11])+
#  mapview(concave_hull)
#concave_hull_final <- st_difference(concave_hull, st_union(land.sf))
#mapview(concave_hull_final)
# Write the concave hull to a shapefile
#st_write(concave_hull_final, "Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/shp/concave_hull_USseabed_gs.shp", driver = "ESRI Shapefile", append = FALSE)


# Read in .csv file containing the data points for Grand Bank  -------------------------------------------------------
#GRAND BANK
data.csv <- read.csv("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/data/GB_texture_grain size_090128.csv")
data.sf <- st_as_sf(data.csv, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) #Convert dataframe to sf object
plot(data.sf[4]) #Median Grainsize
mapview(data.sf[4])

# Calculate the concave hull (contour of the data points)
#concave_hull <- concaveman(data.sf, concavity = 3) 
#plot(concave_hull)
#mapview(data.sf[4])+
#  mapview(concave_hull)
#concave_hull_final <- st_difference(concave_hull, st_union(land.sf))
#mapview(concave_hull_final)
# Write the concave hull to a shapefile
#st_write(concave_hull_final, "Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/shp/concave_hull_GB_gs.shp", driver = "ESRI Shapefile", append = FALSE)

# Read in .mat file -------------------------------------------------------

#data.mat <- readMat("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/model_statistics_prctile95.mat") #read file name.mat


#extract the data from list data.mat:

#Bathy
#data <- data.frame(X=c(data.mat$LON), Y=c(data.mat$LAT), Depth=c(data.mat$bathy))

#data[data==0] <- NA #Remove land (values of 0)
#data.sf <- st_as_sf(data, coords = c("X", "Y"), crs = 4326) #Convert dataframe to sf object

#plot(data.sf[1]) #test plot to check, *takes a while*. 
#crs(data.sf)


#Need to crop land from the data boundary -------------------------------------------------------

#to crop the EBK layers we will use the concave hull shapefile and
#land Shapefile for Canada, US, and Greenland found here: https://gadm.org/download_country.html

#load in saved land shapfile
land.sf <- st_read("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/shp/CAN_US_GRNLND.shp")

#crop land from data boundary
concave_hull_final <- st_difference(concave_hull, st_union(land.sf))
mapview(concave_hull_final)
#st_write(concave_hull, "Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/shp/concave_hull_boundary.shp", driver = "ESRI Shapefile", append = FALSE)


#if exported EBK as contours this is how we crop:-------------------------------------------------------
#gs.mm <- st_read(paste0(drive,":/Li_et_al_2025/Data/Contours/Grainsize_mm_kbessel.shp"))
#mapview(gs.mm, zcol = "Classes",legend = TRUE)

#crop land
#gs.mm <- st_difference(land.sf, st_union(gs.mm))
#plot(gs.mm[1])

#crop to data extent
#gs.mm.fin <- st_intersection(concave_hull, gs.mm)
#plot(gs.mm.fin[1])


#Crop to land, and extent of original data points (concave_hull shapefile) ---------------------------------------

#First crop/mask to land
#raster.crop <- list()
#for(i in 1:length(raster_list)) {
#  raster.crop[[i]] <- terra::crop(raster_list[[i]], vect(land.sf))
#}
#plot(raster.crop[[1]])

#masked_raster <-  list()
#for(i in 1:length(raster_list)) {
#  masked_raster[[i]] <- terra::mask(raster.crop[[i]], vect(land.sf), inverse = TRUE)
#}
#plot(masked_raster[[1]])


#Lastly, mask to extent of data points using concave_hull shapefile
#raster.final <-  list()
#for(i in 1:length(raster_list)) {
#  raster.final[[i]] <- terra::mask(masked_raster[[i]], vect(concave_hull), inverse = FALSE)
#}
#plot(raster.final[[1]])

# Save each SpatRaster in the list to a file
#for (i in 1:length(raster_files)) {
#  writeRaster(raster.final[[i]], filename = paste0("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/Rasters","cropped_", raster_files[[i]]), overwrite = TRUE)
#}


