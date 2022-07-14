library(sf)
library(raster)
library(tidyverse)

#####################################
## Cropping rasters to study area  ##
#####################################

#This script contains examples on how to take multiple or single grided rasters and crop them to a particular area using templates or rasters of a desired extent. This is particularily useful when preparing layers for modeling with Maxent (which requires gridded layers with equal extents and aligned grids.)

# Read in shapefile or raster with desired extent, resolution, and coordinate reference system ------------------------

#e.g. WITH SHAPEFILE
SA_ext <- st_read("Z:/Projects/OFI/BEcoME/Survey_Planning/shp/GBbbox.shp") %>% 
  st_transform(crs = st_crs(4326))

#OR MAKE ONE :) - This is pretty easy to do with the mapedit package.
#library(mapedit)
#SA_ext <- editMap()

#SA_ext <- SA_ext 
#st_transform(32719) %>% 
#select(ID = X_leaflet_id)

#create raster grid
r <- raster(extent(SA_ext), resolution = 0.00487, crs= crs(SA_ext)) #desired resolution in appropriate units (degrees in this case).
SA_ext_ras <- rasterize(SA_ext, r, "ID", mean)
res(SA_ext_ras) #check resolution
plot(SA_ext_ras)#check

#e.g. WITH RASTER
bathy <- raster("Z:/Projects/OFI/BEcoME/Survey_Planning/grids/GB_Bank_layers/bathy_bank/w001001.adf")

#check res, crs
res(bathy)
crs(bathy)


# Read in layer(s) to crop ------------------------------------------------
RastList <- list.files("Z:/Projects/OceanographicData/BNAM/Grids/BNAM_1990-2019_monthly/1990", pattern =".asc$",full.names = TRUE) #Change path
rasterList1 <- lapply(RastList, raster)

# Raster List into a RasterStack: Note - won't stack if layers are different extents/resolutions
Raststack <- raster::stack(rasterList1) #RastList


#or Read in a single layer:
Raster <- raster("Z:/Projects/OceanographicData/BNAM/Grids/BNAM_1990-2019_monthly/1990/BtmSalinity_Apr_1990.asc")


# Crop function -----------------------------------------------------------
# Replace with object of desired resolution, extent and crs.
#May need to re-arrange order of operations within the function depending on the layers/overlap etc...

#e.g. WITH SHAPEFILE (made into raster, see above)
crop_fn <- function(y) {
  y <- projectRaster(y, crs = crs(SA_ext_ras))
  #y <- crop(y, SA_ext)
  #y <- mask(y, mask = SA_ext)
  y <- raster::resample(y, SA_ext_ras, method = "bilinear") #method = ngb for categorical rasters.
  y <- crop(y, SA_ext)
  y <- mask(y, mask = SA_ext)
}

#e.g. WITH RASTER
crop_fn <- function(y) {
  y <- projectRaster(y, crs = crs(bathy))
  #y <- crop(y, bathy)
  #y <- mask(y, mask = bathy)
  y <- raster::resample(y, bathy, method = "bilinear")
  y <- crop(y, bathy)
  y <- mask(y, mask = bathy)
}


# Apply crop function to Raster stack or single raster --------------------

#e.g. raster stack

rastList_crop <- lapply(Raststack, crop_fn) #OR replace stack for Raster list (rasters won't stack if extents are different). 
plot(rastList_crop[[5]]) #check, eg. plots the 5th raster in the stack

rasstack <- stack(rastList_crop)
res(rasstack[[1]]) #check resolution (e.g. checks res of 1st raster in stack) *Won't save as .asc if unequal vert and horizontal resolutions*
names <- RastList  #to use for saving raster stacks. Check that these are as you want them.

setwd("Z:/Projects/OFI/BEcoME/Survey_Planning/grids") #set directory to save layers to

#check file name, and save raster stack.
writeRaster(rasstack, ".", filename= paste0("GB_",names, ".asc"), bylayer = TRUE, format = 'ascii', prj = T, overwrite = T) 


#E.g. single raster
#Apply function to single Raster
Sbtm <- crop_fn(Raster) 
plot(Sbtm)
crs(Sbtm)
writeRaster(Sbtm, "Z:/Projects/OFI/BEcoME/Survey_Planning/grids/GB_BNAM_BtmSal_50m.asc",format = 'ascii', prj = T, overwrite = T)

