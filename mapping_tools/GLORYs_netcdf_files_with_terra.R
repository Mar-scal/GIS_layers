
#Reading in NetCDF files from Copernicus (Glorys oceaongraphic data)
#after manually download data from website 

library(ncdf4)
#library(raster)
library(ggplot2)
library(terra)
library(sf)
#sf::sf_use_s2(FALSE)
terraOptions(tempdir = file.path(tempdir(),"C:/Users/WILSONB/Documents/Temp"))

#### Monthly data #######################################################################################

#define:
year <- "2025"
dir <- "Y:/Projects/OceanographicData/GLORYs/"
save.dir <- "Y:/Projects/OceanographicData/GLORYs/Mixed_Layer_Depth/MLD_Maritime_Extent/"
domain <- read_sf("Y:/Projects/OFI/BEcoME/Data/GIS_data/shapefiles/extent/MaritimeExtent.shp")
#crs(domain)

var <- "mlotst"
extent.name <- "MaritimeExtent" #description of extent
varname <- paste0(extent.name,"_MLD") #Name for saving
#Options are: 
#"mlotst" - mixed layer thickness (sigma theta)
#"zos" - Sea surface height (m)
#"bottomT" - Seafloor potential temperature (C)
#"sithick" - Sea ice thickness (m)
#"siconc" - Sea ice concentration (l)
#"usi" - Eastward sea ice velocity (m/s)
#"vsi" - Northward sea ice velocity (m/s)

#read in the netcdf files for the selected variable:
rList <- list.files(paste0(dir,"/cmems_mod_glo_phy_my_0.083_P1M-m/",year), full.names = TRUE)
rList <- lapply(rList, terra::rast, drivers = "NETCDF", subd = var) # Read in all netcdfs in this year. #the subd argument hopefully eliminates the need to watch for changes in the order of the netcdfs. but will still need to check.

#-------------------------------------------------------------------------------------------------------------------------------------
#CRS not being recognized using Terra package for me (Brittany). Read in as Raster. Will need to troubleshoot

#r_layer <- raster(paste0(dir,"/cmems_mod_glo_phy_my_0.083_P1M-m/2025/mercatorglorys12v1_gl12_mean_202511.nc"), varname = "bottomT")
#domain <- st_transform(domain, crs(r_layer))
#r <- crop(r_layer, domain, snap="near", mask=TRUE, touches=FALSE)
#plot(r)
#writeRaster(r, "Z:/Projects/OceanographicData/GLORYs/Monthly_BtmTemp_cropped_to_MaritimeExtent/2025_missing_Dec/MaritimeExt_BottomT_2025_11.tif", filetype = 'GTiff', overwrite = T)

# I know the crs is 4326 and the domain is 4326 so I can assign it instead.
rList <- lapply(rList, function(r) {
  crs(r) <- crs(domain)
  return(r)
})

print(crs(rList[[1]]))
crs(domain) == crs(rList[[1]])
#-------------------------------------------------------------------------------------------------------------------------------------
#If crs is being properly assigned with Terra, proceed with this code.

domain <- st_transform(domain, crs(rList[[1]]))

#For each month - crop the variable layer to domain
r <- list()
for(i in 1:length(rList)){
  r[[i]] <- rList[[i]]
  r[[i]] <- crop(r[[i]], domain, snap="near", mask=TRUE, touches=FALSE)
}

# Stack the rasters. 
rstack <- rast(r)

names <- c(1:12) #Assumes they stay in same order - will need to check

#Check using rast::time() - timestamps are within the raster file.
#rstack[[1]]
#time(rstack[[1]])
#time(rstack[[5]])

#save at original resolution:  
writeRaster(rstack, paste0(save.dir, year,"/",varname,"_", year, "_", names, ".tif"), filetype = 'GTiff', overwrite = T)



#### RESAMPLING or REPROJECTING #######################################################################################

#Resampling and or Reprojecting to the OFI Bathy layer (78 m horizontal reso, WGS84) and then save:
library(ncdf4)
#library(raster)
library(ggplot2)
library(terra)
library(sf)

start_time <- Sys.time()

year <- 2023

#bathy <- rast("Z:/Projects/OFI/BEcoME/Data/GIS_data/Rasters/BathyCHS_GEBCO_SEAM_mixedData_MartitimeExtentClip_100m_LatLong.asc")
bathy <- rast("Y:/Projects/OFI/BEcoME/Data/GIS_data/Rasters/BathyCHS_GEBCO_SEAM_mixedData_MartitimeExtentClip_500m_ll.tif") #500m resolution
#bathy <- rast("Z:/Projects/OFI/BEcoME/Data/GIS_data/Rasters/BathyCHS_GEBCO_SEAM_mixedData_MartitimeExtentClip_500m_utm.tif")

##################################################################################################################################
#Run once to produce the bathy at the desired resolution:

#if reducing resolution:
#bathy.utm <- project(bathy, "EPSG:32620")

# 2. Calculate factor to reach 500m
# Target Resolution / Current Resolution = Factor
# 500 / 78 = 6
#fact_val <- 6

# 3. Aggregate
#bathy_500m <- aggregate(bathy.utm, fact=fact_val, fun=mean)

# Verify results
#res(bathy_500m)
#plot(bathy_500m)

#writeRaster(bathy_500m, filename = "Z:/Projects/OFI/BEcoME/Data/GIS_data/Rasters/BathyCHS_GEBCO_SEAM_mixedData_MartitimeExtentClip_500m_utm.tif", filetype = "GTiff", overwrite=FALSE)

#Convert back to lat long:
#bathy.500m.ll <- project(bathy_500m, "EPSG:4326")
#crs(bathy.500m.ll)
#bathy <- bathy.500m.ll

#writeRaster(bathy.500m.ll, filename = "Z:/Projects/OFI/BEcoME/Data/GIS_data/Rasters/BathyCHS_GEBCO_SEAM_mixedData_MartitimeExtentClip_500m_ll.tif", filetype = "GTiff", overwrite=FALSE)

###############################################################################################################


rList <- list.files(paste0("Z:/Projects/OceanographicData/GLORYs/Monthly_BtmTemp_cropped_to_MaritimeExtent/",year,"/"), full.names = TRUE) 
rList <- lapply(rList, terra::rast) # Read in all netcdfs in this year.

r <- list()
for(i in 1:length(rList)){
  #r[[i]] <- rList[[i]][[3]]
  r[[i]] <- terra::project(rList[[i]], bathy, method = "bilinear", mask=TRUE, align=FALSE) #project 
  r[[i]] <- resample(r[[i]], bathy, method = "bilinear")
  r[[i]] <- crop(r[[i]], bathy, snap="near", mask=FALSE, touches=FALSE)
  #r[[i]] <- mask(r[[i]], bathy)
}
#plot(r[[12]])
time(r[[2]])
names(r[[2]])
plot(r[[6]])

#check
res(bathy) == res(r[[1]])
ext(bathy) == ext(r[[1]])
crs(bathy) == crs(r[[1]])

rstack <- rast(r)
#time(rstack[[6]])
#time(rstack[[2]])

names <- names(nc_path) #Make sure they are in the same order as the raster stack!

writeRaster(rstack, paste0("Z:/Projects/OceanographicData/GLORYs/Monthly_BtmTemp_cropped_to_MaritimeExtent/500m_resolution/",year,"/", names), filetype = 'GTiff', overwrite = T)

end_time <- Sys.time()
end_time - start_time

#check.month <- rast("Z:/Projects/OceanographicData/GLORYs/Monthly_BtmTemp_cropped_to_MaritimeExtent/500m_resolution/1994/MaritimeExt_BottomT_1994_3.tif")

#time(check.month)

####################################################################

#cropping to the static bathy layer...

#bathy <- rast("E:/GLORYs/cmems_mod_glo_phy_my_0.083-static/GLO-MFC_001_030_mask_bathy.nc")

#bathy <- crop(bathy, domain, snap="near", mask=TRUE, touches=FALSE)
#plot(bathy)

###########################################################################################

#### Climatology data #######################################################################################

dir <- "C:/Users/WILSONB/Documents/1_GISdata/Glorys_climatology/"

bof <- rast("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/MBES_Layers/Grids/Bathymetry/CHS_10mFinal_FrBrian/BOF_ALLBath_2010_100m_adj_dodd_gsc_finalc_z20.asc")

t1 <- terra::rast(paste0(dir,"mercatorglorys12v1_gl12_mean_1993_2016_12.nc"))
dim(t1)
names(t1)
plot(t1[names(t1)[3]])
#plot(t1[names(t1)[153]])

t1 <- t1[names(t1)[135]]
plot(t1)

bof <- project(bof, t1)

#Crop to BOF
bof.t1 <- crop(bof, t1, snap="near", mask=TRUE, touches=FALSE)


writeRaster(bof.t1, filename = paste0(dir,"mercatorglorys12v1_uo_depth-266_mean_1993_2016_12.tif"), filetype = "GTiff", overwrite=FALSE)
writeRaster(bof.t1, filename = paste0(dir,"mercatorglorys12v1_uo_depth-109_mean_1993_2016_12.tif"), filetype = "GTiff", overwrite=FALSE)

