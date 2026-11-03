
#Create a Grid of a specified size #

#----- libraries
library(sf)
library(mapview)

#----- Load spatial data
#e.g.
dat <- read_csv("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/Shapefiles/HorseMussel_shapefiles/HM_count_data/HM_Presence_updated.csv") #Sky
dat <- drop_na(dat)

dat.sf <- st_as_sf(dat, coords = c("Longitude","Latitude"), crs = 4326)  
#plot(dat.sf)

#----- Project to UTM - meters
dat.sf <- st_transform(dat.sf, crs = 32620)

#### GRID DATA ####

#----- Adjust cell size to change grid size (meters)

# E.g. 1 km Grid:
grid <- st_make_grid(dat.sf, cellsize = 1000, square=TRUE) #Square grid shape - 1km grid

# E.g. 10 km Grid:
#grid <- st_make_grid(dat.sf, cellsize = 10000, square=TRUE)

#---- re-formats grid and give each cell an id number
grid <- st_as_sf(data.table(id_grd=1:length(grid), geom=sf::st_as_text(grid)), wkt='geom', crs = 32620)

#-----plot to visualize

#plot(st_geometry(grid))
#plot(dat.sf, add = TRUE)

#mapview(st_geometry(grid))+
  #mapview(dat.sf)

#--- can also do this with hexagons

#hex <- st_make_grid(dat.sf , cellsize= 10000, square=FALSE) #Hexagon grid shape
#hex <- st_as_sf(data.table(id_hex=1:length(hex), geom=sf::st_as_text(hex)), wkt='geom', crs = 32620) #id_hex = 1:#of elements in grid
#plot to visualize
#plot(st_geometry(hex))
#plot(dat.sf, add = TRUE)


#Now we can summarize data within each grid (This may take a while depending on the size of grid, extent of data etc.):
grid.data <- grid %>% 
  st_join(dat.sf, join=st_contains) %>%
  group_by(id_grd) %>%
  dplyr::summarise(mean_Count=mean(Count, na.rm=F)) #set na.rm = T to remove empty grid squares
  

mapview::mapview(grid.data, zcol="mean_Count")
