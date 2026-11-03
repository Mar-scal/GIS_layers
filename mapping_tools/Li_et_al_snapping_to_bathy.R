
library(ggplot2)
library(terra)
library(sf)


# Load in bathymetry to snap/crop/mask to bathy ------------------------------------------------

#bathy <- rast("Z:/Projects/OFI/BEcoME/Data/GIS_data/Rasters/BathyCHS_GEBCO_SEAM_mixedData_MartitimeExtentClip_100m_LatLong.asc")
bathy <- rast("Z:/Projects/OFI/BEcoME/Data/GIS_data/Rasters/BathyCHS_GEBCO_SEAM_mixedData_MartitimeExtentClip_500m_ll.tif")
crs(bathy)

#---------------------------------------------------------------------------

# Read in layer(s) to snap ------------------------------------------------
RastList <- list.files("D:/Li_et_al_2025/Data/Rasters/BOF_GOM_extent", pattern =".tif$",full.names = TRUE) #Change file path
RastList
rasterList <- lapply(RastList, rast) # creates list of SpatRasters
res(bathy)


#Single layer?
gs.phi <- rast("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/Rasters/Grainsize_phi.tif")
smf.comb <- rast("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/Rasters/SMF_comb.tif")
plot(gs.phi)
plot(smf.comb)


#---------------------------------------------------------------------------

#Function to re-project, re-sample, crop and mask to bathy
crop_fn <- function(y) {
  y <- project(y, crs(bathy))
  y <- raster::resample(y, bathy, method = "bilinear") # to match extent
  y <- crop(y, bathy)
  #y <- mask(y, mask = bathy)
}

#---------------------------------------------------------------------------

#Multiple layers? use:
rastList_clean <- lapply(rasterList, crop_fn)

#apply function to individual layers - use:
gs.phi <- crop_fn(gs.phi)
plot(gs.phi)
smf.comb <- crop_fn(smf.comb)
plot(smf.comb)

#check
res(bathy) == res(gs.phi)
ext(bathy) == ext(gs.phi)
crs(bathy) == crs(gs.phi)

res(bathy) == res(smf.comb)
ext(bathy) == ext(smf.comb)
crs(bathy) == crs(smf.comb)

#---------------------------------------------------------------------------

#Saving multiple layers at once? - use:
rastList_clean <- rast(rastList_clean) #stack rasters for saving
plot(rastList_clean[[6]])
plot(bathy)

#save
names <- list.files("D:/Li_et_al_2025/Data/Rasters/BOF_GOM_extent", pattern =".tif$",full.names = FALSE) #Change file path


writeRaster(rastList_clean, paste0("D:/Li_et_al_2025/Data/Rasters/BOF_GOM_extent/snapped_to_ofi_bathy/", names), filetype = 'GTiff', overwrite = F) #Change file path

#---------------------------------------------------------------------------

#Saving individual layers? - use:
writeRaster(gs.phi, paste0("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/Rasters/MaritimeExtent/Grainsize_phi_MaritimeExtent.tif"), filetype = 'GTiff', overwrite = F)

writeRaster(smf.comb, paste0("Z:/Projects/OceanographicData/Li_et_al_2025_layers/Data/Rasters/MaritimeExtent/SMF_Comb_MaritimeExtent.tif"), filetype = 'GTiff', overwrite = F)
