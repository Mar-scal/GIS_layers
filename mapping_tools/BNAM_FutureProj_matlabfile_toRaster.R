library(R.matlab)
library(sf)
library(raster)
#library(sp)


# Read in .mat file -------------------------------------------------------
year <- 2055
setwd("Z:/Projects/BEcoME/BNAM/Data_frm_DaveBrickman/2055") #set directory where files are located
land.mask <- raster("Z:/Projects/BEcoME/BNAM/Data_frm_DaveBrickman/1990-2019_Monthly/BNAM_landmask.asc")
data.mat <- readMat("dUV_btm_F_R45_2046-2065.mat") #read file name.mat

#Check data by plotting
#plot(data.mat$nav.lon,data.mat$nav.lat, pch=ifelse(data.mat$land.mask==0,'','.'))

#extract the following data from list data.mat:

### FOR yearly Projected layer:####
#(___.ann) - change in script depending on data used

data <- data.frame(X=c(data.mat$nav.lon), Y=c(data.mat$nav.lat),Depth=c(data.mat$Bathy.depth), Vbtm.ann = c(data.mat$RCP45.2055.dV.ann) #**change file name**
                   , land=c(data.mat$land.mask),EID = 1:length(data.mat$nav.lon))
data[data==0] <- NA #Remove land (values of 0)

data.sf <- st_as_sf(data, coords = c("X", "Y"), crs = 4326) #Convert dataframe to sf object

plot(data.sf[2]) #test plot to check, *takes a while*. 
crs(data.sf) #check projection

#Build raster template with desired resolution in the units of the projected data (in this case, degrees)
proj <- CRS("+proj=longlat +datum=WGS84 +no_defs")
r <- raster(extent(land.mask), resolution = 0.14, crs= proj)

raster <- rasterize(data.sf, r, "Vbtm.ann", mean) #**change name**
#test <- projectRaster(raster, crs = CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "))

#Check data
plot(raster)
crs(raster)

#Clean up edge of raster East of Greanland - Interpolating gaps
#raster1 <- focal(raster, matrix(1, nrow = 3, ncol = 3), fun = mean, na.rm = TRUE, NAonly = T) # only fills in NA value
#raster1 <- mask(raster1, mask = land.mask, inverse = TRUE) #Mask using land to crop interpolation along edges

#plot(raster1)

#Save raster
setwd(paste0("Z:/Projects/BEcoME/BNAM/BNAM_Rasters/",year))
writeRaster(raster, "2055_RCP45_Vbtm.asc", format = 'ascii', prj = T, overwrite = T) #**change file name**


### FOR Monthly Projected layers:####

#extract the following data from list: Lat/Long, depth land and EID   
LLDepth <- data.frame(X=c(data.mat$nav.lon), Y=c(data.mat$nav.lat),Depth=c(data.mat$Bathy.depth), land=c(data.mat$land.mask),EID = 1:length(data.mat$nav.lon))

var.names <- names(data.mat) #creates a list of data names in .mat file

var.ind <- grep('^RCP45.2055.dV$', var.names) #**change file name**

# (From Adam Cook's script - modified)
m <- 0
#for variable indicated under var.ind object, combine the columns from the variable for each month or single year into a List
var <- list()
for(i in var.ind){ 
  m <- m+1
  out <- c()
  for(j in 1:12){ #1-12 months
    out = cbind(out,c(data.mat[[i]][j,,])) #combine columns from the variable for each month or single year depending on variable of interest.
  }
  if(m==1)	out <- cbind(1:length(data.mat$nav.lon),out)
  out <- na.omit(out)
  var[[m]] <- out
  names(var[[m]]) <- var.names[i]
}

#variables cropped to ocean - converted to dataframe (i.e. spatial data points) #Will need to rewrite something here for bottom stress
#Check other variables work with this too.....
var <- do.call(cbind,var)
ocean <- unique(var[,1]) #Only works for temp - looks for and removes duplicated values (i.e. removes land points). This assumes no temp values are the same.
LLDepth.1 <- subset(LLDepth, EID %in% ocean)
LLDepth.1 <- LLDepth.1[order(LLDepth.1$EID),]
var <- var[order(var[,1]),]
LLDepth.1$EID <- 1:nrow(LLDepth.1)
var[,1] <- 1:nrow(var)
#saveRDS(list(LLDepth.1 = LLDepth.1,var = var), file="Test/btm_stress.rds") #Optional: save R data to desired directory before continuting.

# (End of Adam Cook's script - modified)

#variables cropped to ocean - converted to dataframe (i.e. spatial data points) #Will need to rewrite something here for bottom stress
#Check other variables work with this too.....
#var <- do.call(cbind,var) #List to matrix
var <- as.data.frame(var) #matrix to df

### ONLY FOR MONTHLY AVERAGE CLIMATOLOGY:  ***MAY NEED TO ADD "EID" as column name***
colnames(var) <- c("EID", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")  
str(var)

#combine oceanographic variable back with Lat, Long, depth and EID
full.data <- data.frame(var, LLDepth.1[,1:3])
full.data[full.data==0] <- NA #Remove land (values of 0)

data.sf <- st_as_sf(full.data, coords = c("X", "Y"), crs = 4326) #convert to sf object defining latitude and longitude (x and y) and setting projection to latlong.
str(data.sf)

#class(data.sf)
plot(data.sf[2]) #test plot to check, takes a while. 
crs(data.sf) #check projection


#Build raster template with desired resolution in the units of the projected data (in this case, degrees)
proj <- CRS("+proj=longlat +datum=WGS84 +no_defs")
r <- raster(extent(land.mask), resolution = 0.14, crs= proj)
names <- colnames(var[-1]) #setting up names for each layer

x <- list()
for(i in 1:length(names)) {
  x[[i]] <- rasterize(data.sf, r, names[i], mean, update = TRUE, updateValue = "NA")
}


#Stack rasters from List x
rasstack <- stack(unlist(x))

plot(rasstack[[1]])
crs(rasstack[[8]])

#Save raster, **change file name**
setwd(paste0("Z:/Projects/BEcoME/BNAM/BNAM_Rasters/",year))
writeRaster(rasstack, ".", filename= paste0("2055_RCP45_Vbtm_", names, "_", year, ".asc"), bylayer = TRUE, format = 'ascii', prj = T, overwrite = T)
