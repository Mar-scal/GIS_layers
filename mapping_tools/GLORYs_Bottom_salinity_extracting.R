
#Extracting a bottom salinity layer from GLORYs data:
# GLORYS has 50 standard depth levels for variables like salinity. There is not a seamless bottom salinity layer for the whole ocean, however, we can look at each cell's depth profile and find the cell's max depth with a non zero value. This can be done using the ncdf4 package or terra. Not sure which one is better to use memory wise. Need to test this.

library(ncdf4)
library(terra)
library(sf)
library(stringr)


# TERRA method: -----------------------------------------------------------

###################################################################################

#Batch runs by year

year <- 2025
dir <- "D:/GLORYs"
nc_path <- list.files(paste0(dir,"/cmems_mod_glo_phy_my_0.083_P1M-m/",year), full.names = TRUE)


get_bottom <- function(x) {
  # Get index of the last value that is not NA
  last_idx <- max(which(!is.na(x)))
  return(x[last_idx])
}

#Run Loop:
# Load as a SpatRaster
for(i in 1:length(nc_path)){
  r_stack <- rast(nc_path[i], subd = "so")
  crs(r_stack) <- "EPSG:4326"
  
  # Apply to the spatial stack
  # Note: This might be memory intensive. Process time steps individually if necessary.
  bottom_salinity <- app(r_stack, fun = get_bottom)
  plot(bottom_salinity)
  
  names <- str_sub(nc_path[i], -9, -4)
  
  writeRaster(bottom_salinity, paste0("D:/GLORYs/Monthly_Btm_Salinity/",year,"/BS_",names,".tif"), filetype = 'GTiff', overwrite = T)
  
}


####################################################################################
#Run individual months:

# Path to GLORYS netcdf file
nc_path <- "C:/Users/WILSONB/Documents/Test/mercatorglorys12v1_gl12_mean_201604.nc"

# Load as a SpatRaster
r_stack <- rast(nc_path, subd = "so")
crs(r_stack) <- "EPSG:4326"

# Function to get the last non-NA value in a pixel stack (bottom layer)
get_bottom <- function(x) {
  # Get index of the last value that is not NA
  last_idx <- max(which(!is.na(x)))
  return(x[last_idx])
}

# Apply to the spatial stack
# Note: This might be memory intensive. Process time steps individually if necessary.
bottom_salinity <- app(r_stack, fun = get_bottom)
plot(bottom_salinity)

#writeRaster(bottom_salinity, paste0("Z:/Projects/OceanographicData/GLORYs/Monthly_Salinity_cropped_to_MaritimeExtent/BS_1994_1_Terra.tif"), filetype = 'GTiff', overwrite = T)


######################################################################################################

# Cropping to domain (bathy) and resampling: -----------------------------------------------------------
year <- 2025
#Lost time information during extracting bottom salinity - to add back in, we need to load the netcdf files again:
dir <- "D:/GLORYs"
nc_path <- list.files(paste0(dir,"/cmems_mod_glo_phy_my_0.083_P1M-m/",year), full.names = TRUE)
r <- lapply(nc_path, terra::rast)
time(r[[1]][[1]]) #use any data layer (all are the same date)

#Create list of times
r.time <- list()
for(i in 1:length(r)){
  r.time[[i]]  <- time(r[[i]][[1]])
}
r.time

#Now to crop to domain:

#500m reso ofi bathy
bathy <- rast("Z:/Projects/OFI/BEcoME/Data/GIS_data/Rasters/BathyCHS_GEBCO_SEAM_mixedData_MartitimeExtentClip_500m_ll.tif") #500m resolution

rList <- list.files(paste0("D:/GLORYs/Monthly_Btm_Salinity/",year,"/"), full.names = TRUE) 
rList <- lapply(rList, terra::rast) # Read in all netcdfs in this year.

r <- list()
for(i in 1:length(rList)){
  r[[i]] <- terra::project(rList[[i]], bathy, method = "bilinear", mask=TRUE, align=FALSE) #project 
  r[[i]] <- resample(r[[i]], bathy, method = "bilinear")
  r[[i]] <- crop(r[[i]], bathy, snap="near", mask=FALSE, touches=FALSE)
  #r[[i]] <- mask(r[[i]], bathy)
  time(r[[i]]) <- r.time[[i]]
}

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

names <- list.files(paste0("D:/GLORYs/Monthly_Btm_Salinity/",year,"/"), full.names = FALSE)

names.nu <- list()
for(i in 1:length(names)){
names.nu[i] <- str_sub(names[i], -10, -7)
names.nu[i] <- paste0("MaritimeExt_BottomS_",names.nu[i], "_", str_sub(names[i], -6, -1))
}

writeRaster(rstack, paste0("Z:/Projects/OceanographicData/GLORYs/Monthly_Salinity_cropped_to_MaritimeExtent/snapped_to_ofibathy_500m/",year,"/",names.nu), filetype = 'GTiff', overwrite = T)

###

#check

#btm_sal <- rast("Z:/Projects/OceanographicData/GLORYs/Monthly_Salinity_cropped_to_MaritimeExtent/snapped_to_ofibathy_500m/1993/MaritimeExt_BottomS_1993_10.tif")

#time(btm_sal)
#plot(btm_sal)

######################################################################################################


# Ncdf4 method: -----------------------------------------------------------

#Test on one file:

# Path to GLORYS netcdf file
nc_path <- "C:/Users/WILSONB/Documents/Test/mercatorglorys12v1_gl12_mean_201604.nc"
nc <- nc_open(nc_path)

# View variables and dimensions
print(nc)

# Extract dimensions
lon <- ncvar_get(nc, "longitude")
lat <- ncvar_get(nc, "latitude")
depths <- ncvar_get(nc, "depth")
time <- ncvar_get(nc, "time")

# Read all salinity data (Lon, Lat, Depth) - 
so_array <- ncvar_get(nc, "so")  #THIS IS HIT OR MISS WHEN WORKING ON SKY....memory issues, need to do locally?

# Array dim is [lon, lat, depth]
bottom_so <- matrix(NA, nrow=length(lon), ncol=length(lat))

for(i in 1:length(lon)){
  for(j in 1:length(lat)){
    # Extract depth profile for this pixel
    profile <- so_array[i, j, ] 
    # Find last non-NA value
    valid_depths <- which(!is.na(profile))
    if(length(valid_depths) > 0){
      bottom_so[i, j] <- profile[max(valid_depths)]
    }
  }
}

r_bottom <- rast(t(bottom_so), crs="+proj=longlat +datum=WGS84")
plot(r_bottom)

# Flip and rotate to fix lat/lon orientation if necessary
r_bottom <- flip(r_bottom, direction="vertical") 

# Set extent to global
ext(r_bottom) <- c(min(lon), max(lon), min(lat), max(lat))

# Visualize
plot(r_bottom, main="Bottom Salinity")

#writeRaster(r_bottom, paste0("Z:/Projects/OceanographicData/GLORYs/Monthly_Salinity_cropped_to_MaritimeExtent/BS_1994_1_netcdf.tif"), filetype = 'GTiff', overwrite = T)


###################################################################################################################

# Diagnostics: -----------------------------------------------------------

#Is there a difference between these two methods?

so_netcdf <- r_bottom
so_terra <- bottom_salinity

so_netcdf
so_terra

#Extents are slightly different (by a few decimal points) - not concerning

#Resolutions differ slightly as well: where netcdf is not in x and y.
#netcdf - extent      : -180, 179.9167, -80, 90 
#terra - extent      : -180.0417, 179.9583, -80.04167, 90.04167

#Salinity value ranges are slightly different: min value only by a few decimals - #the difference varies by year it seems...

#1993
#max value   : 44.74166 vs max value   : 43.188268645
44.74166-43.188268645 #difference is 1.553391

#1994
#max value   : 45.677053090 vs max value   : 43.951231381
45.677053090-43.951231381 #difference is 1.725822

#Find where differences are greatest: 
#First need to match extents/reso

so_netcdf <- resample(so_netcdf, so_terra, method = "near") #Don't want to change values of cell - use nearest neighbours method.
so_netcdf
so_terra

stack <- c(so_netcdf, so_terra)

diff.so <- diff(stack)
plot(diff.so)

#plot values that have a difference of:
plot(diff.so > 0.05)
plot(diff.so > 0.005)

plot(diff.so > 20.0)

#writeRaster(diff.so, paste0("Z:/Projects/OceanographicData/GLORYs/Monthly_Salinity_cropped_to_MaritimeExtent/diff_btwn_methods_1994.tif"), filetype = 'GTiff', overwrite = T)

#Difference is minimal.


