
#-----Derive terrain attributes for Bay of Fundy and Gulf of Maine Domain-------------------------------------------------------------

#libraries
library(sf)
library(terra)
library(MultiscaleDTM) 
library(mapview)
library(tidyverse)
library(raster)

#Bathy Olex and OFI layers were compared to determine which one was best to use.
#OFI layer (approx 100m reso) was the better of the two (visually determined by looking at hillshade)

##########Cropping, Hillshade and saving layer for use in deriving variables #########################################

#ofi_dem <- rast("Z:/Projects/OFI/BEcoME/Data/GIS_data/Rasters/BathyCHS_GEBCO_SEAM_mixedData_MartitimeExtentClip_100m_LatLong.asc")
#olex_dem <- rast("Y:/Inshore/StandardDepth/ScotianShelfDEM_Olex/mdem_olex/w001001.adf")
#ofi_dem <- project(ofi_dem, "+proj=utm +zone=20 +datum=WGS84") #reproject to UTM Zone 20 ("epsg:32620")

#Load domain shp
#domain <- st_read("Z:/GISdata/Private/BoF_GOM_extent.shp") |> 
  #st_transform(crs = 32620)

#HILLSHADE
#ofi_slope <- terrain(ofi_dem, "slope", unit="radians")
#ofi_aspect <- terrain(ofi_dem, "aspect", unit="radians")
#ofi_hill <- shade(ofi_slope, ofi_aspect)

#olex_slope <- terrain(olex_dem, "slope", unit="radians")
#olex_aspect <- terrain(olex_dem, "aspect", unit="radians")
#olex_hill <- shade(olex_slope, olex_aspect)
#olex_hill <- shade(olex_slope, olex_aspect, angle = 35, direction = 45)
#plot(olex_hill)
#ofi_hill <- crop(ofi_hill, domain)
#olex_hill <- crop(olex_hill, domain)


#CROP AND MASK AND SAVE
#OFI layer determined to be the better option of the two.
#ofi_dem <- mask(ofi_dem, mask = domain, inverse = FALSE)
#olex_dem <- mask(olex_dem, mask = domain, inverse = TRUE)
#ofi_dem <- crop(ofi_dem, domain)
#olex_dem <- crop(olex_dem, domain)
#plot(ofi_dem)

#writeRaster(olex_hill, filename = "Z:/People/Brittany/1_GISdata/Olex_vs_OFI_DEM/olex_hillshade.tif", filetype = "GTiff")
#writeRaster(ofi_hill, filename = "Z:/People/Brittany/1_GISdata/Olex_vs_OFI_DEM/ofi_hillshade.tif", filetype = "GTiff")
#writeRaster(ofi_dem, filename = "Z:/GISdata/Private/BoF_GoM_dataset/ofi_DEM_UTMZ20.tif", filetype = "GTiff")

#-------------------------------------------------------------------------------------------

########## Deriving variables using Multiscale_DTM package #####################################################

bathy <- rast("Z:/GISdata/Private/BoF_GoM_dataset/ofi_DEM_UTMZ20.tif")
plot(bathy)

#-----Individual terrain attributes-----------------------------------------------------------------

# Slope, Aspect, and Curvature -------------------------------------------

#From Ilich et al. 2023:

#Slope - The slope is a measure of the steepness of the focal cell and is obtained by calculating the angle between the horizontal plane and the one tangential to the surface (i.e. The maximum rate of change in elevation values)

#Aspect - The orientation of the slope in the downslope direction

#Eastness - The east/west components of the orientation of the slope are calculated as the sine of aspect. Thus, it ranges between −1 (due West) and 1 (due East)

#Northness - The north/south components of the orientation of the slope are calculated as the cosine of aspect. Thus, it ranges between −1 (due South) and 1 (due North)

#method = queen (8 neighbor case) (Horn, 1981), method = rook (4 neighbors) (Fleming & Hoffer, 1979; Ritter, 1987)

w <- c(11,11)
slp_asp<- SlpAsp(r = bathy, w = w, unit = "degrees", method = "queen", metrics = c("slope", "eastness", "northness"))
plot(slp_asp)

#save
#writeRaster(slp_asp[[1]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/slope_3x3nw.tif",  filetype = "GTiff")
#writeRaster(slp_asp[[2]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/eastness_3x3nw.tif",  filetype = "GTiff")
#writeRaster(slp_asp[[3]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/northness_3x3nw.tif",  filetype = "GTiff")
#writeRaster(slp_asp[[1]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/slope_5x5nw.tif",  filetype = "GTiff")
#writeRaster(slp_asp[[2]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/eastness_5x5nw.tif",  filetype = "GTiff")
#writeRaster(slp_asp[[3]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/northness_5x5nw.tif",  filetype = "GTiff")
#writeRaster(slp_asp[[1]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/slope_9x9nw.tif",  filetype = "GTiff")
#writeRaster(slp_asp[[2]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/eastness_9x9nw.tif",  filetype = "GTiff")
#writeRaster(slp_asp[[3]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/northness_9x9nw.tif",  filetype = "GTiff")
#writeRaster(slp_asp[[1]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/slope_11x11nw.tif",  filetype = "GTiff")
#writeRaster(slp_asp[[2]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/eastness_11x11nw.tif",  filetype = "GTiff")
#writeRaster(slp_asp[[3]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/northness_11x11nw.tif",  filetype = "GTiff")


#slp_asp<- SlpAsp(r = bathy, w = c(9,9), unit = "degrees", method = "rook", metrics = c("slope", "aspect", "eastness", "northness"))
#plot(slp_asp)
#writeRaster(slp_asp[[1]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/slope_9x9nw_rook.tif",  filetype = "GTiff")
# visually - Results show less emphasis on steeper slopes than queen method..


# Q-metrics - Curvatures -------------------------------------------

#From Ilich et al. 2023:

#profile curvature (profc) - The level of convexity or concavity along the direction of the maximum slope

#Platform curvature (planc) - The level of convexity or concavity perpendicular to the direction of the maximum slope

#Twisting curvature (twistc) - The amount of local twisting is measured as the change in slope angle per unit distance along the direction perpendicular to the slope

#Mean curvature (meanc) - The average of the minimum and maximum curvatures

# Min/Max curvature (minc/maxc) - The minimum/maximum curvature in any plane
w <- c(11,11)
qmetrics<- Qfit(bathy, w = w, unit = "degrees", metrics = c("profc", "planc", "twistc", "meanc", "maxc", "minc"), na.rm = TRUE)
plot(qmetrics)


#Save
#writeRaster(qmetrics[[1]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/profc_11x11nw.tif",  filetype = "GTiff")
#writeRaster(qmetrics[[2]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/planc_11x11nw.tif",  filetype = "GTiff")
#writeRaster(qmetrics[[3]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/twistc_11x11nw.tif",  filetype = "GTiff")
#writeRaster(qmetrics[[4]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/meanc_11x11nw.tif",  filetype = "GTiff")
#writeRaster(qmetrics[[5]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/maxc_11x11nw.tif",  filetype = "GTiff")
#writeRaster(qmetrics[[6]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/minc_11x11nw.tif",  filetype = "GTiff")

# Rugosity ----------------------------------------------------------------

#VRM
w = c(27,27)
vrm<- VRM(bathy, w=w, na.rm = TRUE)
plot(vrm)

#writeRaster(vrm, filename= "Z:/GISdata/Private/BoF_GoM_dataset/vrm_3x3nw.tif",  filetype = "GTiff")
#writeRaster(vrm, filename= "Z:/GISdata/Private/BoF_GoM_dataset/vrm_5x5nw.tif",  filetype = "GTiff")
#writeRaster(vrm, filename= "Z:/GISdata/Private/BoF_GoM_dataset/vrm_9x9nw.tif",  filetype = "GTiff")
#writeRaster(vrm, filename= "Z:/GISdata/Private/BoF_GoM_dataset/vrm_11x11nw.tif",  filetype = "GTiff")
#writeRaster(vrm, filename= "Z:/GISdata/Private/BoF_GoM_dataset/vrm_17x17nw.tif",  filetype = "GTiff")
#writeRaster(vrm, filename= "Z:/GISdata/Private/BoF_GoM_dataset/vrm_27x27nw.tif",  filetype = "GTiff")

#SAPA
w <- c(5,5)
sapa<- SAPA(bathy, w=w, slope_correction = TRUE)
plot(sapa)

#writeRaster(sapa, filename= "Z:/GISdata/Private/BoF_GoM_dataset/sapa_3x3nw.tif",  filetype = "GTiff")
#writeRaster(sapa, filename= "Z:/GISdata/Private/BoF_GoM_dataset/sapa_5x5nw.tif",  filetype = "GTiff")
#writeRaster(sapa, filename= "Z:/GISdata/Private/BoF_GoM_dataset/sapa_9x9nw.tif",  filetype = "GTiff")
#writeRaster(sapa, filename= "Z:/GISdata/Private/BoF_GoM_dataset/sapa_11x11nw.tif",  filetype = "GTiff")
#writeRaster(sapa, filename= "Z:/GISdata/Private/BoF_GoM_dataset/sapa_17x17nw.tif",  filetype = "GTiff")
#writeRaster(sapa, filename= "Z:/GISdata/Private/BoF_GoM_dataset/sapa_27x27nw.tif",  filetype = "GTiff")



#Adjusted SD
w <- c(11,11)
adj_SD<- AdjSD(bathy, w=w, na.rm = TRUE)
plot(adj_SD)

#writeRaster(adj_SD, filename= "Z:/GISdata/Private/BoF_GoM_dataset/adj_SD_3x3nw.tif",  filetype = "GTiff")
#writeRaster(adj_SD, filename= "Z:/GISdata/Private/BoF_GoM_dataset/adj_SD_5x5nw.tif",  filetype = "GTiff")
#writeRaster(adj_SD, filename= "Z:/GISdata/Private/BoF_GoM_dataset/adj_SD_9x9nw.tif",  filetype = "GTiff")
#writeRaster(adj_SD, filename= "Z:/GISdata/Private/BoF_GoM_dataset/adj_SD_11x11nw.tif",  filetype = "GTiff")

#Roughness Index-Elevation
w <- c(11,11)
rie<- RIE(bathy, w=w, na.rm = TRUE)
plot(rie)

#writeRaster(rie, filename= "Z:/GISdata/Private/BoF_GoM_dataset/rie_3x3nw.tif",  filetype = "GTiff")
#writeRaster(rie, filename= "Z:/GISdata/Private/BoF_GoM_dataset/rie_5x5nw.tif",  filetype = "GTiff")
#writeRaster(rie, filename= "Z:/GISdata/Private/BoF_GoM_dataset/rie_9x9nw.tif",  filetype = "GTiff")
#writeRaster(rie, filename= "Z:/GISdata/Private/BoF_GoM_dataset/rie_11x11nw.tif",  filetype = "GTiff")

# Relative Position -------------------------------------------------------

#Relative Position
#rp<- RelPos(bathy, w=matrix(data = c(1,NA,1), nrow = 3, ncol=3), shape = "custom", fun = "median", na.rm = TRUE)

#TPI
#tpi<- TPI(bathy, w=c(11,11), na.rm = TRUE)
#plot(tpi)

#DMV
w <- 20
sdmv<- DMV(bathy, w=w, shape= "circle", na.rm = TRUE, stand="range")
plot(sdmv)

#Save dmv
#writeRaster(sdmv, filename= "Z:/GISdata/Private/BoF_GoM_dataset/sdmv_2circlenw.tif",  filetype = "GTiff")
#writeRaster(sdmv, filename= "Z:/GISdata/Private/BoF_GoM_dataset/sdmv_6circlenw.tif",  filetype = "GTiff")
#writeRaster(sdmv, filename= "Z:/GISdata/Private/BoF_GoM_dataset/sdmv_10circlenw.tif",  filetype = "GTiff")
#writeRaster(sdmv, filename= "Z:/GISdata/Private/BoF_GoM_dataset/sdmv_20circlenw.tif",  filetype = "GTiff")

#BPI
#annulus_window(radius = c(5,10), unit = "cell")

#broad BPI (standardized (relative position is standardized by dividing by the standard deviation))
sbbpi<- BPI(bathy, w = annulus_window(radius = c(5,100), unit = "cell"), stand="sd", na.rm = TRUE)
plot(sbbpi)
#bpi<- as(bpi, "Raster")#convert to RasterLayer to view in mapview
#mapview(bpi)

sbbpi2<- BPI(bathy, w = annulus_window(radius = c(50,100), unit = "cell"), stand="sd", na.rm = TRUE)
plot(bbpi2)

#fine BPI (standardized (relative position is standardized by dividing by the standard deviation))
sfbpi<- BPI(bathy, w = annulus_window(radius = c(1,10), unit = "cell"), stand="sd", na.rm = TRUE)
plot(sfbpi)
#global(sfbpi, fun="isNA")
#global(sfbpi, fun="notNA")

sfbpi2 <- BPI(bathy, w = annulus_window(radius = c(1,5), unit = "cell"), stand="sd", na.rm = TRUE)
plot(sfbpi)

#Save bpis
#writeRaster(sbbpi, filename= "Z:/GISdata/Private/BoF_GoM_dataset/sbbpi_5x100.tif",  filetype = "GTiff")
#writeRaster(sbbpi2, filename= "Z:/GISdata/Private/BoF_GoM_dataset/sbbpi_25x50.tif",  filetype = "GTiff")
#writeRaster(sfbpi, filename= "Z:/GISdata/Private/BoF_GoM_dataset/sfbpi_1x10.tif",  filetype = "GTiff")
#writeRaster(sfbpi2, filename= "Z:/GISdata/Private/BoF_GoM_dataset/sfbpi_1x5.tif",  filetype = "GTiff")


#-----Creating multiple terrain attributes at once (seperate loops) -----------------------------------------------------------------

TA <- rast()
  for(w in c(3,5,7,11,17,27,43,69)) {
  Rugosity <- AdjSD(bathy, w)
  TA <- c(TA, Rugosity)
  }


for(w in c(3,5,7,11,17,27,43,69)) {
  Curv <- Qfit(bathy, w, metrics = c("profc", "planc", "twistc", "meanc", "maxc", "minc"))
  SlpAsp <- Qfit(bathy, w, metrics = c("qslope", "qaspect", "qeastness", "qnorthness"))
  TA <- c(TA, Curv, SlpAsp)
}

TA <- c(Rugosity, RelativePosition, Curv, SlpAsp)

#-----Creating multiple terrain attributes at once - in one loop -----------------------------------------------------------------

 #Initialize Raster for Terrain Attributes

for(w in c(3,5,7,11,17,27,43,69)) {
  
  SlpAspCurv <- Qfit(bathy, w, metrics = c("qslope", "qaspect", "qeastness", "qnorthness", "profc", "planc", "twistc", "meanc", "maxc", "minc", "features"))
  
  Rugosity <- AdjSD(bathy, w)
  
  RelativePosition <- TPI(bathy, w) }
  
  TA <- c(TA, Rugosity, RelativePosition) #Append layers to stack #SlpAspCurv, 
  


####Snapping all layers to Bathy###################################################################

  library(terra)
  library(sf)
  
  
  path <- "Z:/GISdata/Private/BoF_GoM_dataset/"
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
  names[61]
  
  #check file name, and save raster stack.
  for(i in 1:length(rastList_crop)) {
    writeRaster(rastList_crop[[i]], filename= paste0("Z:/GISdata/Private/BoF_GoM_dataset/snapped_to_bathy/", names[i]), filetype = "GTiff")
  }
  
  
  #### BOF Backscatter ###################################################################
  
  #smaller extent
  
  #Haar et al. 2023 bulkshift backscatter - https://doi.org/10.1080/17445647.2023.2223629
  
  bkscatter <- rast("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/MBES_Layers/Grids/FromBen/resampled-to-50meters/bulkshift_bs_50m_UTMZ20.asc")
  crs(bkscatter) <- crs(bathy)
  
  plot(bkscatter)
  crs(bkscatter)
  
  crop_fn <- function(y) {
    y <- project(y, bathy, method = "bilinear", mask=FALSE, align=FALSE)
    y <- crop(y, bathy, snap="near", mask=TRUE, touches=FALSE)
    y <- mask(y, bathy)
  }
  
  bs.new <- lapply(bkscatter, crop_fn)
  plot(bs.new[[1]])
 summary(bs.new)
  
  writeRaster(bs.new[[1]], filename= paste0("Z:/GISdata/Private/BoF_GoM_dataset/snapped_to_bathy/bulkshift_backscatter_BOF_UTMZ20.tif"), filetype = "GTiff")
  
  #### BOF Benthoscape as raster ###################################################################
  
  #smaller extent
  #using raster package:
  benthoscape <- rast("Z:/People/Brittany/1_Projects/Publications_working/BOF_SDMs/Projects/Modiolus/Data/archived/Benthoscape_categorical_UTMZ20.asc")
  plot(benthoscape)
  #crs(benthoscape) <- crs(bathy)
  
  #crop_fn <- function(y) {
  #  y <- project(y, bathy, method = "ngb", mask=TRUE, align=TRUE)
  #  y <- crop(y, bathy, snap="near", mask=TRUE, touches=FALSE)
  #  y <- mask(y, bathy)
  #}
  benthoscape.new <- project(benthoscape, bathy, method = "near", mask=TRUE, align=TRUE)
  crs(benthoscape.new)
  #benthoscape.new <- lapply(benthoscape, crop_fn)
  plot(benthoscape.new)
  summary(benthoscape.new)
  
  writeRaster(benthoscape.new, filename= paste0("Z:/GISdata/Private/BoF_GoM_dataset/snapped_to_bathy/benthoscape_BOF_UTMZ20.tif"), filetype = "GTiff",overwrite=TRUE)
  