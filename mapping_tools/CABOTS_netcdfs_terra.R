
#Reading CABOTS data

library(ncdf4)
#library(raster)
library(ggplot2)
library(terra)
library(sf)
#sf::sf_use_s2(FALSE)


#### Monthly data #######################################################################################

season <- "fall" # "spring", "summer"

dir <- "Y:/Projects/OceanographicData/CABOTS/FRDR_dataset_964_download_552_202605200648/CABOTS_NetCDF_Files/CABOTS_"

domain <- read_sf("Y:/Projects/OFI/BEcoME/Data/GIS_data/shapefiles/extent/MaritimeExtent.shp")
crs(domain)
#domain <- read_sf("Z:/GISdata/Private/BoF_GoM_dataset/Extent_shp/BoF_GOM_extent.shp")

Cabots <- rast(paste0(dir,season, ".nc"))

plot(Cabots)
plot(Cabots[[1]])


