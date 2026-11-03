library(terra)
library(sf)
library(ggplot2)
#library(mapview)


# Calculating Cumulative Summer Bottom Temps ------------------------------

#define:
var <- "^MaritimeExt_BottomT"
year <- 2025
months <- 7:9
path <- paste0("Z:/Projects/OceanographicData/GLORYs/Monthly_BtmTemp_cropped_to_MaritimeExtent/",year,"/")

#Load Land shapefile from github
temp <- tempfile()
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/Inshore_Spatial_Layers_Mar2025.zip", temp)
temp2 <- tempfile()
unzip(zipfile=temp, exdir=temp2)

land_sf <- st_read(paste0(temp2,"/Inshore_Spatial_Layers_Mar2025/Atl_region_land.shp"))

#Get rasters
ras_list<- list() #Create dataframes with lat lons
for(i in c(months)){
ras_list[[i]] <- dir(path, pattern = paste0(var, "_", year, "_",i, ".","tif$"), full.names = TRUE, recursive = TRUE)
}
r <- lapply(ras_list[7:9], terra::rast) #bring in as raster (list)

#Stack rasters
r <- c(r[[1]], r[[2]],r[[3]]) #terra raster stack
plot(r)

#Combine:
ravg <- terra::app(r, mean) #combine Jun, July and Aug rasters (sum)
plot(ravg)
#res(ravg) #0.08333333 0.08333333, lat long


#writeRaster(rsum, filename = paste0("Z:/Projects/OceanographicData/GLORYs/Cumulative_SummerBTMs/Summer_BT",year,".tif"), filetype = "GTiff", overwrite = T)

#fill in gaps near land
#rsum.new  <- focal(rsum , w = 3, fun ="mean", na.policy="only", na.rm=TRUE)
#rast <- raster::raster(rsum.new)
#mapview::mapview(rast)

#Crop to land
#rsum.new <- crop(rsum.new, land_sf)
#rsum.new <- mask(rsum.new, land_sf, inverse = TRUE, touches=FALSE)
#plot(rsum.new)
#plot(land.sf, add = T)

#Resampling GLORYs data
bathy <- rast("Z:/Projects/OFI/BEcoME/Data/GIS_data/Rasters/BathyCHS_GEBCO_SEAM_mixedData_MartitimeExtentClip_500m_ll.tif")
#plot(bathy)
#crs(bathy) == crs(rsum)
res(bathy)
res(ravg)

#crop and resample (to match bathy):

r.ll <- resample(ravg , bathy, method = "bilinear")
#res(bathy) == res(r.ll)
#ext(bathy) == ext(r.ll)
#crs(bathy) == crs(r.ll)

r.ll <- project(r.ll, bathy, method = "bilinear", mask=TRUE, align=TRUE)
#res(bathy) == res(r.ll)
#ext(bathy) == ext(r.ll)
#crs(bathy) == crs(r.ll)

r.ll <- crop(r.ll , bathy, snap="near", mask=FALSE, touches=FALSE)
res(bathy) == res(r.ll)
ext(bathy) == ext(r.ll)
crs(bathy) == crs(r.ll)

#r.ll  <- mask(r.ll, bathy) #This step takes a looooong time.
#res(bathy) == res(r.ll)
#ext(bathy) == ext(r.ll)
#crs(bathy) == crs(r.ll)

writeRaster(r.ll, filename = paste0("Z:/Projects/OceanographicData/GLORYs/Mean_SummerBTMs/500m_resolution/Summer_BT_7-9_",year,".tif"), filetype = "GTiff", overwrite = T)


#r.utm <- project(r.ll, "EPSG:32620", method = "bilinear")
#res(r.utm)


######Average Btm Temp for each year############################################################################

start_time <- Sys.time()

library(terra)
library(sf)
library(ggplot2)

#define:
var <- "^MaritimeExt_BottomT" 
year <- 2024
months <- 1:12
path <- paste0("Z:/Projects/OceanographicData/GLORYs/Monthly_BtmTemp_cropped_to_MaritimeExtent/",year,"/") #",year,"/"

#Get rasters
ras_list<- list() #Create dataframes with lat lons
for(i in 1:length(months)){
  ras_list[[i]] <- list.files(path, pattern = paste0(var, "_", year, "_",i, ".","tif"), full.names = TRUE, recursive = FALSE)
}
r <- lapply(ras_list, terra::rast) #bring in as raster (list)

#Stack rasters
r <- c(r[[1]],r[[2]],r[[3]],r[[4]],r[[5]],r[[6]],r[[7]],r[[8]],r[[9]],r[[10]],r[[11]],r[[12]]) #terra raster stack - jan-dec #,r[[12]]
#plot(r)

#Combine:
ravg <- terra::app(r, mean) #mean of Jan - Dec
plot(ravg)

#Snap to bathy:
bathy <- rast("Z:/Projects/OFI/BEcoME/Data/GIS_data/Rasters/BathyCHS_GEBCO_SEAM_mixedData_MartitimeExtentClip_500m_ll.tif")

ravg <- resample(ravg , bathy, method = "bilinear")
ravg <- project(ravg, bathy, method = "bilinear", mask=TRUE, align=TRUE)
ravg  <- crop(ravg, bathy, snap="near", mask=FALSE, touches=FALSE)

res(bathy) == res(ravg)
ext(bathy) == ext(ravg)
crs(bathy) == crs(ravg)
plot(ravg)

writeRaster(ravg, filename = paste0("Z:/Projects/OceanographicData/GLORYs/Mean_Yearly_BtmTemp_cropped_to_MaritimeExtent/Mean_MaritimeExt_BtmTemp_1-12_",year,".tif"), filetype = "GTiff", overwrite = T)

end_time <- Sys.time()
end_time - start_time
