# Coordinates provided by Dvora Hart, note the slight modificaiton by DK to align with
# EEZ used by Canada.  Uncleaer what coordinate system these are in, assumed 4326, this 
# should be fine for our science purposes.
library(sf)
library(tidyverse)
source("D:/Github/Assessment_fns/Maps/pectinid_projector_sf.R")
CA1=data.frame(lon =  -1*c(69.3833, 68.5, 68.5, 68.75), lat=c(41.5, 41.5, 40.75, 40.75)  )
# Note that DK edited the second longitude to be 66.43 from 66.413 as 66.413 appears to be inside Canada
# while 66.43 falls exactly on the ICJ line that we use.  
CA2=data.frame(lon= -1*c(67.333, 66.43, 66.5967, 67.333), lat = c(42.3667, 41.31, 41.0, 41.0)  )

CA1.sf <- st_as_sf(CA1,coords = c("lon",'lat'), crs = 4326)
CA1.sf <- st_cast(st_combine(CA1.sf),"POLYGON")
CA2.sf <- st_as_sf(CA2,coords = c("lon",'lat'), crs = 4326)
CA2.sf <- st_cast(st_combine(CA2.sf),"POLYGON")

pecjector(area = "GOM",add_layer= list(eez = 'eez'),c_sys = 4326) + geom_sf(data = CA2.sf, fill = NA) + geom_sf(data = CA1.sf)

# now I want to save these are shapefiles...
st_write(CA1.sf,dsn ="d:/github/CAs/CA1.shp")
st_write(CA2.sf,dsn = "d:/github/CAs/CA2.shp")
