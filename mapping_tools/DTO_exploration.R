

#DIGITAL TWIN OCEANS DATA EXPLORATION 2016-2024:

library(ncdf4)
library(terra)
library(sf)
library(mapview)
library(stars)

#### FUNDY5000 Monthly data #############################################################

year <- "2024"
product <- "mean" #"max", "min", "std"

dir <- "Z:/Projects/OceanographicData/Digital_Twin_Oceans/Fundy500/monthly_nc/"
nc_path <- paste0(dir,"Eastern_Gulf_of_Maine-Bay_of_Fundy_",year,"_1m_grid_T_2D_",product,".nc")
nc <- nc_open(nc_path)
# View variables and dimensions
print(nc)

#Lat and longs in data file have 0s. Use bathy lat longs to make grid.
bathy_path <- "Z:/Projects/OceanographicData/Digital_Twin_Oceans/Fundy500/Tests_from_MikeCasey/Bathymetry_Fundy500_consistent_sub.nc"
bathy.nc <- nc_open(bathy_path)
#print(bathy.nc)


#variables:
#sbs - sea bottom salinity
#sbspeed - Hourly mean ocean bottom speed
#sbt - sea bottom temp
#sbu_rot - Hourly eastward ocean bottom current
#sbv_rot - Hourly northward ocean bottom current

#Define variable:
var <- "sbt"

# Retrieve actual 2D coordinates and 2D data
lon <- ncvar_get(bathy.nc, "nav_lon") #matrix
lat <- ncvar_get(bathy.nc, "nav_lat") #matrix
raw_time <- ncvar_get(nc, "time_counter") 
time_units <- ncatt_get(nc, "time_counter", "units")$value # seconds since 1950-01-01 00:00:00
data.array <- ncvar_get(nc, var) #array (note: 2016 is missing Jan...)

nc_close(nc)

#need to convert time to something more meaningful:
time_units
origin <- "1950-01-01 00:00:00"
real_time <- as.POSIXct(raw_time, origin = origin, tz = "UTC")
real_time

#now make dataframe
data <- data.array[ ,  , 1] #just look at first month

# Flatten and create a dataframe
df <- data.frame(lon = as.vector(lon),
                 lat = as.vector(lat),
                 var = as.vector(data))

df.sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)

# 1. Create a template raster with desired extent and resolution
template_rast <- rast(ext(df.sf), resolution = 0.0057, crs = st_crs(df.sf)$wkt)
res(template_rast) <- 0.0057

# 2. Convert sf to terra SpatVector and rasterize
# 'field' is the column name in your sf object you want to use for cell values
var_rast <- rasterize(vect(df.sf), template_rast, field = "var", resolution = c(0.0057,0.0057))
plot(var_rast)

time(var_rast) <- real_time[1] #Will need to edit for looping.
names(var_rast) <- "Bottom Temperature"

var_rast

writeRaster(var_rast, paste0("Z:/Projects/OceanographicData/Digital_Twin_Oceans/Fundy500/Monthly_Geotiffs/test_bottomT_jan2016.tif"), filetype = 'GTiff', overwrite = T)


#### CIOPSE Monthly data#############################################################
#Files are in 2 year chunks

