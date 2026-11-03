

#--------CLEAN Li et al Layers----------------------------------------------------------------------------------------

#When changing the resolution of the Li et al layers, coastline becomes wonky. Needs a bit of editing around the edges before masking to another raster.
#Here we use one of the GLORYs rasters that was resampled and cropped to the OFI Bathy.


library(ggplot2)
library(terra)
library(sf)


# Load in bathymetry to snap/crop/mask to bathy ------------------------------------------------

#bathy <- rast("Z:/Projects/OFI/BEcoME/Data/GIS_data/Rasters/BathyCHS_GEBCO_SEAM_mixedData_MartitimeExtentClip_100m_LatLong.asc")
#crs(bathy)

#Use a GLORYs bottom temperature layer for cropping and masking.. Doesn't take as long as the ofi bathy
Btm_temp <- rast("Z:/Projects/OceanographicData/GLORYs/Monthly_BtmTemp_cropped_to_MaritimeExtent/500m_resolution/1993/MaritimeExt_BottomT_1993_1.tif")
plot(Btm_temp)

#Load Land shapefile from github
temp <- tempfile()
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/Inshore_Spatial_Layers_Mar2025.zip", temp)
temp2 <- tempfile()
unzip(zipfile=temp, exdir=temp2)

land_sf <- st_read(paste0(temp2,"/Inshore_Spatial_Layers_Mar2025/Atl_region_land.shp"))

#Load Li et al layers
gs.phi <- rast("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/Rasters/Grainsize_phi.tif")
ust.comb <- rast("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/Rasters/UST_comb.tif")
plot(gs.phi)
plot(ust.comb)

#fill in gaps near land
gs.phi <- focal(gs.phi, w = 3, fun ="mean", na.policy="only", na.rm=TRUE)
plot(gs.phi)

ust.comb  <- focal(ust.comb , w = 3, fun ="mean", na.policy="only", na.rm=TRUE)
plot(ust.comb)

#resample
gs.phi <- resample(gs.phi, Btm_temp, method = "bilinear")
plot(gs.phi)
ust.comb <- resample(ust.comb, Btm_temp, method = "bilinear")
plot(ust.comb)

#Project
gs.phi <- project(gs.phi, Btm_temp, method = "bilinear", mask=TRUE, align=TRUE)
plot(gs.phi)
ust.comb <- project(ust.comb, Btm_temp, method = "bilinear", mask=TRUE, align=TRUE)
plot(ust.comb)

#Crop
gs.phi <- crop(gs.phi , Btm_temp, snap="near", mask=FALSE, touches=FALSE)
plot(gs.phi)
ust.comb <- crop(ust.comb, Btm_temp, snap="near", mask=FALSE, touches=FALSE)
plot(ust.comb)

#Mask
gs.phi  <- mask(gs.phi, Btm_temp)
plot(gs.phi)
ust.comb <- mask(ust.comb, Btm_temp)
plot(ust.comb)


#Saving individual layers? - use:
writeRaster(gs.phi, paste0("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/Rasters/MaritimeExtent/500m_resolution/Grainsize_phi_MaritimeExtent.tif"), filetype = 'GTiff', overwrite = T)

writeRaster(ust.comb, paste0("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/Rasters/MaritimeExtent/500m_resolution/UST_Comb_MaritimeExtent.tif"), filetype = 'GTiff', overwrite = T)


