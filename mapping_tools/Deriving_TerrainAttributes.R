
#-----Derive terrain attributes for Bay of Fundy and Gulf of Maine Domain-------------------------------------------------------------

#libraries
library(sf)
library(terra)
library(MultiscaleDTM) 
library(mapview)

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

slp_asp<- SlpAsp(r = bathy, w = c(9,9), unit = "degrees", method = "queen", metrics = c("slope", "aspect", "eastness", "northness"))
plot(slp_asp)
#save
#writeRaster(slp_asp[[1]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/slope_9x9nw.tif",  filetype = "GTiff")
#writeRaster(slp_asp[[2]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/aspect_9x9nw.tif",  filetype = "GTiff")
#writeRaster(slp_asp[[3]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/eastness_9x9nw.tif",  filetype = "GTiff")
#writeRaster(slp_asp[[4]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/northness_9x9nw.tif",  filetype = "GTiff")

slp_asp<- SlpAsp(r = bathy, w = c(5,5), unit = "degrees", method = "queen", metrics = c("slope", "aspect", "eastness", "northness"))
plot(slp_asp)
#writeRaster(slp_asp[[1]], filename= "Z:/GISdata/Private/BoF_GoM_dataset/slope_5x5nw.tif",  filetype = "GTiff")
#visually - Stronger, more defined than the 9x9 window.

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

qmetrics<- Qfit(bathy, w = c(5,5), unit = "degrees", metrics = c("profc", "planc", "twistc", "meanc", "maxc", "minc"), na.rm = TRUE)
plot(qmetrics)
qmetrics<- as(qmetrics[[12]], "Raster")

#Save features
#writeRaster(qmetrics[[12]], filename= paste0("E:/BOF_envirodataset/features_multiscale"), format = 'AAIGrid', prj = T, overwrite = T)

# Rugosity ----------------------------------------------------------------

#VRM
vrm<- VRM(bathy, w=c(5,5), na.rm = TRUE)
plot(vrm)

#SAPA
sapa<- SAPA(bathy, w=c(5,5), slope_correction = TRUE)
plot(sapa)

#Adjusted SD
adj_SD<- AdjSD(bathy, w=c(5,5), na.rm = TRUE)
plot(adj_SD)
crs(adj_SD)
adj_SD<- as(adj_SD, "Raster")
#writeRaster(adj_SD, filename= paste0("E:/BOF_envirodataset/adj_SD_multiscale.gri"), format = 'raster', prj = T, overwrite = T)

# Relative Position -------------------------------------------------------

#TPI
tpi<- TPI(bathy, w=c(5,5), na.rm = TRUE)
plot(tpi)

#RDMV
rdmv<- RDMV(bathy, w=c(5,5), na.rm = TRUE, method="range")
plot(rdmv)
rdmv<- as(rdmv, "Raster")

#Save rdmv
#writeRaster(rdmv, filename= paste0("E:/BOF_envirodataset/rdmv_multiscale.gri"), format = 'raster', prj = T, overwrite = T)

#BPI
#annulus_window(radius = c(5,10), unit = "cell")

#broad BPI
bbpi<- BPI(bathy, radius = c(5,100), unit = "cell", na.rm = TRUE)
plot(bbpi)
#bpi<- as(bpi, "Raster")#convert to RasterLayer to view in mapview
#mapview(bpi)

bbpi2<- BPI(bathy, radius = c(50,500), unit = "cell", na.rm = TRUE)
plot(bbpi2)

#fine BPI
fbpi<- BPI(bathy, radius = c(1,10), unit = "cell", na.rm = TRUE)
plot(fbpi)

#Save bpis
#writeRaster(bbpi, filename= "Z:/GISdata/Private/BoF_GoM_dataset/bbpi_5x100.tif",  filetype = "GTiff")
#writeRaster(fbpi, filename= "Z:/GISdata/Private/BoF_GoM_dataset/fbpi_1x10.tif",  filetype = "GTiff")

#-----Creating multiple terrain attributes at once-----------------------------------------------------------------

TA <- rast() #Initialize Raster for Terrain Attributes

for(w in c(3,5,6,11,17,27,43,69)) {
  
  SlpAspCurv <- Qfit(bathy, q, metrics = c("qslope", "qaspect", "qeastness", "qnorthness", "profc", "planc", 
                                           "twistc", "meanc", "maxc", "minc", "features"))
  Rugosity <- AdjSD(bathy, w)
  
  RelativePosition <- TPI(bathy, w)
  
  TA <- c(TA, SlpAspCurv, Rugosity, RelativePosition) #Append layers to stack
  
}#Calculated 72 terrain attributes


