library(terra)
library(sf)
library(mapview)


# Calculating Cumulative Winter Bottom Temps ------------------------------

#define:
var <- "^BOF_GOM_BottomT"
year <- 2019
months <- 1:3
path <- paste0("Z:/Projects/OceanographicData/GLORYs/Monthly_BtmTemp_cropped_to_BOF_and_GOM_domain/",year)

#Get rasters
ras_list<- list() #Create dataframes with lat lons
for(i in 1:length(months)){
ras_list[[i]] <- dir(path, pattern = paste0(var, "_", year, "_",i, ".","tif$"), full.names = TRUE, recursive = TRUE)
}
r <- lapply(ras_list, terra::rast) #bring in as raster (list)

#Stack rasters
r <- c(r[[1]], r[[2]],r[[3]]) #terra raster stack
plot(r)

#Combine:
rsum <- terra::app(r, sum) #combine Jan, Feb and March rasters (sum)
plot(rsum)
#res(rsum) #0.08333333 0.08333333, lat long
#res(bathy) #77.97146 77.97146, UTM, meters
#plot(rsum)


#fill in gaps near land
rsum.new  <- focal(rsum , w = 3, fun ="mean", na.policy="only", na.rm=TRUE)
#rast <- raster(rsum.new)
#mapview(rast)
#Crop to land
#rsum.new <- crop(rsum.new, land.sf)
#rsum.new <- mask(rsum.new, land.sf, inverse = TRUE, touches=FALSE)
#plot(rsum.new)
#plot(land.sf, add = T)

#Resampling GLORYs data of Orla
bathy <- rast("Z:/GISdata/Private/BoF_GoM_dataset/ofi_DEM_UTMZ20.tif")

#project to UTM (to match bathy) and resample:
r.utm <- project(rsum.new, bathy, method = "bilinear", mask=FALSE, align=FALSE)
#res(r.utm)
r.utm  <- crop(r.utm, bathy, snap="near", mask=TRUE, touches=FALSE)
r.utm  <- mask(r.utm, bathy)
plot(r.utm)
writeRaster(r.utm, filename = paste0("Z:/Projects/OceanographicData/GLORYs/Cumulative_WinterBTMs/Winter_BT",year,".tif"), filetype = "GTiff")

######Checking Data:############################################################################


#Get rasters
path <- "Z:/Projects/OceanographicData/GLORYs/Cumulative_WinterBTMs/"
ras_list<- dir(path, pattern = paste0("tif$"), full.names = TRUE, recursive = TRUE)

r <- lapply(ras_list, terra::rast) #bring in as raster (list)
plot(r[[10]])

#Stack rasters
r.1993.2002 <- c(r[[1]],r[[2]],r[[3]],r[[4]],r[[5]],r[[6]],r[[7]],r[[8]],r[[9]],r[[10]])
 #terra raster stack
plot(r.1993.2002)

r.2003.2012 <- c(r[[1]],r[[2]],r[[3]],r[[4]],r[[5]],r[[6]],r[[7]],r[[8]],r[[9]],r[[10]])
plot(r.2003.2012)

r.2013.2020 <- c(r[[1]],r[[2]],r[[3]],r[[4]],r[[5]],r[[6]],r[[7]],r[[8]])
plot(r.2013.2020)


#######Snapping all layers to Bathy.###########################################################################


library(terra)
library(sf)


path <- "Z:/Projects/OceanographicData/GLORYs/Cumulative_WinterBTMs/"
ras_list<- dir(path, pattern = paste0("tif$"), full.names = TRUE, recursive = TRUE)
r <- lapply(ras_list, terra::rast)

bathy <- rast("Z:/GISdata/Private/BoF_GoM_dataset/ofi_DEM_UTMZ20.tif")

crop_fn <- function(y) {
  y <- project(y, bathy, method = "bilinear", mask=FALSE, align=FALSE)
  y <- crop(y, bathy, snap="near", mask=TRUE, touches=FALSE)
  y <- mask(y, bathy)
}

rastList_crop <- lapply(r, crop_fn)
plot(rastList_crop[[1]])

ras_list.nu <- dir(path, pattern = paste0("tif$"), full.names = FALSE, recursive = TRUE)
names <- ras_list.nu  #to use for saving raster stacks. Check that these are as you want them.
names[28]

#check file name, and save raster stack.
for(i in 1:length(rastList_crop)) {
writeRaster(rastList_crop[[i]], filename= paste0("Z:/Projects/OceanographicData/GLORYs/Cumulative_WinterBTMs/snapped_to_bathy/", names[i]), filetype = "GTiff")
}

######Projecting to UTM############################################################################


start_time <- Sys.time()
year <- 2001
path <- paste0("Z:/Projects/OceanographicData/GLORYs/Monthly_BtmTemp_cropped_to_BOF_and_GOM_domain/",year,"/")
bathy <- rast("Z:/GISdata/Private/BoF_GoM_dataset/ofi_DEM_UTMZ20.tif")

#Get rasters
ras_list <- dir(path, pattern = paste0("tif$"), full.names = TRUE, recursive = TRUE)
r <- lapply(ras_list, terra::rast) #bring in as raster

#Create dataframes with lat lons
r.new <- list()
for(i in 1:length(r)){
r.new[[i]]  <- focal(r[[i]] , w = 3, fun ="mean", na.policy="only", na.rm=TRUE)
}
#library(raster)
#r.new.ras <- raster(r.new[[1]])
#mapview(r.new.ras)

crop_fn <- function(y) {
  y <- project(y, bathy, method = "bilinear", mask=FALSE, align=FALSE)
  y <- crop(y, bathy, snap="near", mask=TRUE, touches=FALSE)
  y <- mask(y, bathy)
}

rastList_crop <- lapply(r.new, crop_fn)
plot(rastList_crop[[1]])

ras_list.nu <- dir(path, pattern = paste0("tif$"), full.names = FALSE, recursive = TRUE)
names <- ras_list.nu  #to use for saving raster stacks. Check that these are as you want them.
names

#check file name, and save raster stack.
for(i in 1:length(rastList_crop)) {
  writeRaster(rastList_crop[[i]], filename= paste0("Z:/Projects/OceanographicData/GLORYs/Monthly_BtmTemp_cropped_to_BOF_and_GOM_domain/UTMz20/", names[i]), filetype = "GTiff")
}
end_time <- Sys.time()
end_time - start_time

######Average Btm Temp for each year############################################################################

#define:
var <- "^BOF_GOM_BottomT" 
year <- 2020
months <- 1:12
path <- "Z:/Projects/OceanographicData/GLORYs/Monthly_BtmTemp_cropped_to_BOF_and_GOM_domain/UTMz20"

#Get rasters
ras_list<- list() #Create dataframes with lat lons
for(i in 1:length(months)){
  ras_list[[i]] <- dir(path, pattern = paste0(var, "_", year, "_",i, ".","tif$"), full.names = TRUE, recursive = TRUE)
}
r <- lapply(ras_list, terra::rast) #bring in as raster (list)

#Stack rasters
r <- c(r[[1]],r[[2]],r[[3]],r[[4]],r[[5]],r[[6]],r[[7]],r[[8]],r[[9]],r[[10]],r[[11]],r[[12]]) #terra raster stack - jan-dec
#plot(r)

#Combine:
ravg <- terra::app(r, mean) #mean of Jan - Dec
plot(ravg)

#Snap to bathy:
bathy <- rast("Z:/GISdata/Private/BoF_GoM_dataset/ofi_DEM_UTMZ20.tif")

ravg  <- crop(ravg, bathy, snap="near", mask=TRUE, touches=FALSE)
ravg  <- mask(ravg, bathy)
plot(ravg)
writeRaster(ravg, filename = paste0("Z:/Projects/OceanographicData/GLORYs/Mean_Yearly_BtmTemp_cropped_to_BOF_and_GOM_domain/Mean_BOF_GOM_BtmTemp_1-12_",year,"_UTMz20.tif"), filetype = "GTiff")



######Average Btm Temp 1993-2020 ############################################################################

years <- 1993:2020
path <- "Z:/Projects/OceanographicData/GLORYs/Mean_Yearly_BtmTemp_cropped_to_BOF_and_GOM_domain"

#Get rasters
ras_list<- list() #Create dataframes with lat lons
for(i in 1:length(years)){
  ras_list[[i]] <- dir(path, pattern = paste0("Mean_BOF_GOM_BtmTemp_1-12_", years[i], "_UTMz20.tif"), full.names = TRUE, recursive = TRUE)
}
r <- lapply(ras_list, terra::rast) #bring in as raster (list)

#Stack rasters
r <- c(r[[1]],r[[2]],r[[3]],r[[4]],r[[5]],r[[6]],r[[7]],r[[8]],r[[9]],r[[10]],r[[11]],r[[12]],r[[13]],r[[14]],r[[15]],r[[16]],r[[17]],r[[18]],r[[19]],r[[20]],r[[21]],r[[22]],r[[23]],r[[24]],r[[25]],r[[26]],r[[27]],r[[28]]) #terra raster stack - jan-dec
#terra::plot(r)

#Combine:
ravg <- terra::app(r, mean) #mean of 1993-2020
terra::plot(ravg)

#Snap to bathy:
bathy <- rast("Z:/GISdata/Private/BoF_GoM_dataset/ofi_DEM_UTMZ20.tif")

ravg  <- crop(ravg, bathy, snap="near", mask=TRUE, touches=FALSE)
ravg  <- mask(ravg, bathy)
plot(ravg)
writeRaster(ravg, filename = paste0("Z:/Projects/OceanographicData/GLORYs/Mean_Yearly_BtmTemp_cropped_to_BOF_and_GOM_domain/Mean_1993-2020_BOF_GOM_BtmTemp_UTMz20.tif"), filetype = "GTiff")


######Average Btm Temp 2017-2020 ############################################################################

years <- 2017:2020
path <- "Z:/Projects/OceanographicData/GLORYs/Mean_Yearly_BtmTemp_cropped_to_BOF_and_GOM_domain"

#Get rasters
ras_list<- list() #Create dataframes with lat lons
for(i in 1:length(years)){
  ras_list[[i]] <- dir(path, pattern = paste0("Mean_BOF_GOM_BtmTemp_1-12_", years[i], "_UTMz20.tif"), full.names = TRUE, recursive = TRUE)
}
r <- lapply(ras_list, terra::rast) #bring in as raster (list)

#Stack rasters
r <- c(r[[1]],r[[2]],r[[3]],r[[4]]) #terra raster stack - 2017:2020
#terra::plot(r)

#Combine:
ravg <- terra::app(r, mean) #mean of 2017-2020
terra::plot(ravg)

#Snap to bathy:
bathy <- rast("Z:/GISdata/Private/BoF_GoM_dataset/ofi_DEM_UTMZ20.tif")

ravg  <- crop(ravg, bathy, snap="near", mask=TRUE, touches=FALSE)
ravg  <- mask(ravg, bathy)
plot(ravg)
writeRaster(ravg, filename = paste0("Z:/Projects/OceanographicData/GLORYs/Mean_Yearly_BtmTemp_cropped_to_BOF_and_GOM_domain/Mean_2017-2020_BOF_GOM_BtmTemp_UTMz20.tif"), filetype = "GTiff")



######Average Btm Temp 2015-2019 ############################################################################

years <- 2015:2019
path <- "Z:/Projects/OceanographicData/GLORYs/Mean_Yearly_BtmTemp_cropped_to_BOF_and_GOM_domain"

#Get rasters
ras_list<- list() #Create dataframes with lat lons
for(i in 1:length(years)){
  ras_list[[i]] <- dir(path, pattern = paste0("Mean_BOF_GOM_BtmTemp_1-12_", years[i], "_UTMz20.tif"), full.names = TRUE, recursive = TRUE)
}
r <- lapply(ras_list, terra::rast) #bring in as raster (list)

#Stack rasters
r <- c(r[[1]],r[[2]],r[[3]],r[[4]],r[[5]]) #terra raster stack - 2017:2020
#terra::plot(r)

#Combine:
ravg <- terra::app(r, mean) #mean of 2015-2019
terra::plot(ravg)

#Snap to bathy:
bathy <- rast("Z:/GISdata/Private/BoF_GoM_dataset/ofi_DEM_UTMZ20.tif")

ravg  <- crop(ravg, bathy, snap="near", mask=TRUE, touches=FALSE)
ravg  <- mask(ravg, bathy)
plot(ravg)
writeRaster(ravg, filename = paste0("Z:/Projects/OceanographicData/GLORYs/Mean_Yearly_BtmTemp_cropped_to_BOF_and_GOM_domain/Mean_2015-2019_BOF_GOM_BtmTemp_UTMz20.tif"), filetype = "GTiff")
