# Function Specific to BNAM data in skyscallop directory -----------------------------------------------------

#These outputs of this function are saved here: Z:\Projects\BEcoME\BNAM\BNAM_Rasters\1990-2019_monthlyscale_BNAM_variables

#This function does not need to be run again (unless something is wrong with the layers that are already written). It is mainly for making Brittany's life easier by converting the BNAM model runs for each variable, year and month from .mat files in the directory Z:/Projects/BEcoME/BNAM/Data_frm_DaveBrickman/1990-2019_Monthly/ to .asc rasters and saves them to Z:\Projects\BEcoME\BNAM\BNAM_Rasters\1990-2019_monthlyscale_BNAM_variables.

#It takes approx. 45 mins for 12 layers

#Arguments:

#1. var      BNAM variables for rasterization: "BS", "Bstress", "BT", "BU", "BV", "MLD", "SS", "ST"
#2. year     Year of model run. years between 1990 - 2019 are accepted.  This is used to specify the folder the data is from and where it will be saved.

#Example:
#start_time <- Sys.time()
#mat2raster(var = "BS", year = "1991")
#mat2raster(var = "Bstress", year = "1991")
#mat2raster(var = "BT", year = "1991")
#mat2raster(var = "BU", year = "1991")
#mat2raster(var = "BV", year = "1991")
#mat2raster(var = "MLD", year = "1991")
#mat2raster(var = "SS", year = "1991")
#mat2raster(var = "ST", year = "1991")
#end_time <- Sys.time()
#end_time - start_time


#Function:

mat2raster = function(var = "BS", year = "1990")
  
  
{
  require(R.matlab) || stop("Install R.matlab")
  require(sf) || stop("Install sf")
  require(raster) || stop("Install raster")
  require(lubridate) || stop("Install lubridate")
  require(tidyverse)|| stop("Install tidyverse")
  
  file.dir <- paste0("Z:/Projects/BEcoME/BNAM/Data_frm_DaveBrickman/1990-2019_Monthly/",year)
  #setwd(file.dir)
  coord <- readMat("Z:/Projects/BEcoME/BNAM/Data_frm_DaveBrickman/1990-2019_Monthly/latlon.mat")
  land.mask <- raster("Z:/Projects/BEcoME/BNAM/Data_frm_DaveBrickman/1990-2019_Monthly/BNAM_landmask.asc")
  
  #Build raster template with desired resolution in the units of the projected data (in this case, degrees)
  proj <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  r <- raster(extent(land.mask), resolution = 0.1, crs= proj)
  
  
  if(var == "BS")
  {
    matList <- list.files(file.dir, pattern =  "*BS_", full.names = TRUE)
    matList1 <- lapply(matList, readMat)
    
    dat <- list() #Create dataframes with lat lons
    for(i in 1:length(matList1)){
      dat[[i]] <- data.frame(X=c(coord$Lons), Y=c(coord$Lats), BS = c(matList1[[i]]$BS), EID = 1:length(matList1[[i]]$BS))
      dat[[i]] <- na.omit(dat[[i]])
    }
    
    dat.sf <- list() #convert to sf objects
    for(i in 1:length(dat)){
      dat.sf[[i]] <- st_as_sf(dat[[i]], coords = c("X", "Y"), crs = 4326)
    }
    
    rasters <- list()
    for(i in 1:length(dat.sf)){
      rasters[[i]] <- rasterize(dat.sf[[i]], r, "BS", mean)
    }
  }
  
  if(var == "BT")
  {
    matList <- list.files(file.dir, pattern =  "*BT_", full.names = TRUE)
    matList1 <- lapply(matList, readMat)
    
    dat <- list() #Create dataframes with lat lons
    for(i in 1:length(matList1)){
      dat[[i]] <- data.frame(X=c(coord$Lons), Y=c(coord$Lats), BT = c(matList1[[i]]$BT), EID = 1:length(matList1[[i]]$BT))
      dat[[i]] <- na.omit(dat[[i]])
    }
    
    dat.sf <- list() #convert to sf objects
    for(i in 1:length(dat)){
      dat.sf[[i]] <- st_as_sf(dat[[i]], coords = c("X", "Y"), crs = 4326)
    }
    
    rasters <- list()
    for(i in 1:length(dat.sf)){
      rasters[[i]] <- rasterize(dat.sf[[i]], r, "BT", mean)
    }
  }
  
  if(var == "BU")
  {
    matList <- list.files(file.dir, pattern =  "*BU_", full.names = TRUE)
    matList1 <- lapply(matList, readMat)
    
    dat <- list() #Create dataframes with lat lons
    for(i in 1:length(matList1)){
      dat[[i]] <- data.frame(X=c(coord$Lons), Y=c(coord$Lats), BU = c(matList1[[i]]$BU), EID = 1:length(matList1[[i]]$BU))
      dat[[i]][dat[[i]]==0] <- NA #Remove land (values of 0)
      dat[[i]] <- na.omit(dat[[i]])
    }
    
    dat.sf <- list() #convert to sf objects
    for(i in 1:length(dat)){
      dat.sf[[i]] <- st_as_sf(dat[[i]], coords = c("X", "Y"), crs = 4326)
    }
    
    rasters <- list()
    for(i in 1:length(dat.sf)){
      rasters[[i]] <- rasterize(dat.sf[[i]], r, "BU", mean)
    }
  }
  
  if(var == "BV")
  {
    matList <- list.files(file.dir, pattern =  "*BV_", full.names = TRUE)
    matList1 <- lapply(matList, readMat)
    
    dat <- list() #Create dataframes with lat lons
    for(i in 1:length(matList1)){
      dat[[i]] <- data.frame(X=c(coord$Lons), Y=c(coord$Lats), BV = c(matList1[[i]]$BV), EID = 1:length(matList1[[i]]$BV))
      dat[[i]][dat[[i]]==0] <- NA #Remove land (values of 0)
      dat[[i]] <- na.omit(dat[[i]])
    }
    
    dat.sf <- list() #convert to sf objects
    for(i in 1:length(dat)){
      dat.sf[[i]] <- st_as_sf(dat[[i]], coords = c("X", "Y"), crs = 4326)
    }
    
    rasters <- list()
    for(i in 1:length(dat.sf)){
      rasters[[i]] <- rasterize(dat.sf[[i]], r, "BV", mean)
    }
  }
  
  if(var == "Bstress")
  {
    matList <- list.files(file.dir, pattern =  "*Bstress_", full.names = TRUE)
    matList1 <- lapply(matList, readMat)
    
    dat <- list() #Create dataframes with lat lons
    for(i in 1:length(matList1)){
      dat[[i]] <- data.frame(X=c(coord$Lons), Y=c(coord$Lats), Bstress = c(matList1[[i]]$stress), EID = 1:length(matList1[[i]]$stress))
      dat[[i]][dat[[i]]==0] <- NA #Remove land (values of 0)
      dat[[i]] <- na.omit(dat[[i]])
    }
    
    dat.sf <- list() #convert to sf objects
    for(i in 1:length(dat)){
      dat.sf[[i]] <- st_as_sf(dat[[i]], coords = c("X", "Y"), crs = 4326)
    }
    
    rasters <- list()
    for(i in 1:length(dat.sf)){
      rasters[[i]] <- rasterize(dat.sf[[i]], r, "Bstress", mean)
    }
  }
  
  if(var == "MLD")
  {
    matList<- readMat("MLD.mat")
    matList1 <- list()
    for(i in 1:12){
      matList1[[i]] <- cbind(matList[[1]][i,,])
    }
    
    names(matList1) <- c(1:12)
    
    dat <- list() #Create dataframes with lat lons
    for(i in 1:length(matList1)){
      dat[[i]] <- data.frame(X=c(coord$Lons), Y=c(coord$Lats), MLD = c(matList1[[i]]), EID = 1:length(matList1[[i]]))
      dat[[i]][dat[[i]]==0] <- NA
      dat[[i]] <- na.omit(dat[[i]])
    }
    
    dat.sf <- list() #convert to sf objects
    for(i in 1:length(dat)){
      dat.sf[[i]] <- st_as_sf(dat[[i]], coords = c("X", "Y"), crs = 4326)
    }
    
    rasters <- list()
    for(i in 1:length(dat.sf)){
      rasters[[i]] <- rasterize(dat.sf[[i]], r, "MLD", mean)
    }
  }
  
  if(var == "SS")
  {
    matList <- list.files(file.dir, pattern =  "*SS_", full.names = TRUE)
    matList1 <- lapply(matList, readMat)
    
    dat <- list() #Create dataframes with lat lons
    for(i in 1:length(matList1)){
      dat[[i]] <- data.frame(X=c(coord$Lons), Y=c(coord$Lats), SS = c(matList1[[i]]$SS), EID = 1:length(matList1[[i]]$SS))
      dat[[i]] <- na.omit(dat[[i]])
    }
    
    dat.sf <- list() #convert to sf objects
    for(i in 1:length(dat)){
      dat.sf[[i]] <- st_as_sf(dat[[i]], coords = c("X", "Y"), crs = 4326)
    }
    
    rasters <- list()
    for(i in 1:length(dat.sf)){
      rasters[[i]] <- rasterize(dat.sf[[i]], r, "SS", mean)
    }
  }
  
  if(var == "ST")
  {
    matList <- list.files(file.dir, pattern =  "*ST_", full.names = TRUE)
    matList1 <- lapply(matList, readMat)
    
    dat <- list() #Create dataframes with lat lons
    for(i in 1:length(matList1)){
      dat[[i]] <- data.frame(X=c(coord$Lons), Y=c(coord$Lats), ST = c(matList1[[i]]$ST), EID = 1:length(matList1[[i]]$ST))
      dat[[i]] <- na.omit(dat[[i]])
    }
    
    dat.sf <- list() #convert to sf objects
    for(i in 1:length(dat)){
      dat.sf[[i]] <- st_as_sf(dat[[i]], coords = c("X", "Y"), crs = 4326)
    }
    
    rasters <- list()
    for(i in 1:length(dat.sf)){
      rasters[[i]] <- rasterize(dat.sf[[i]], r, "ST", mean)
    }
  }
  
  # Stack the rasters and interpolate gaps (Eastcoast of Greenland) and mask to land. 
  rasstack <- stack(rasters)
  
  rasstack.interp <- list()
  for(i in 1:length(dat.sf)){
    rasstack.interp[[i]] <- focal(rasstack[[i]], matrix(1, nrow = 3, ncol = 3), fun = mean, na.rm = TRUE, NAonly = T) # only fills in NA value
    rasstack.interp[[i]] <- mask(rasstack.interp[[i]], mask = land.mask, inverse = TRUE) #Mask using land to crop interpolation along edges
  }
  
  # Stack the new rasters
  rasstack.interp <- stack(rasstack.interp)
  
  #Save rasters
  dir.create(paste0("Z:/Projects/BEcoME/BNAM/BNAM_Rasters/1990-2019_monthly_BNAM_variables/",year), showWarnings = FALSE) #create year folder if it doesn't exist
  setwd(paste0("Z:/Projects/BEcoME/BNAM/BNAM_Rasters/1990-2019_monthly_BNAM_variables/",year))
  
  if(var == "MLD"){
    names <- month.abb[c(1:12)] #The MLD.mat is formatted differently and needs to be named this way.
  } else {
    
    names <- substr(matList, 70, nchar(matList)-4) #extract number of characters from file name up to layer name and month numeric value, and removes .mat
    names <- month.abb[parse_number(names)] # convert to month abbreviation for raster file naming
  }
  
  
  if(var == "BS")
  {
    writeRaster(rasstack.interp, ".", filename= paste0("BtmSalinity_", names, "_", year, ".asc"), bylayer = TRUE, format = 'ascii', prj = T, overwrite = T)
  }
  
  if(var == "BT")
  {
    writeRaster(rasstack.interp, ".", filename= paste0("BtmTemp_", names, "_", year, ".asc"), bylayer = TRUE, format = 'ascii', prj = T, overwrite = T)
  }
  
  if(var == "BU")
  {
    writeRaster(rasstack.interp, ".", filename= paste0("BtmUCurrent_", names, "_", year, ".asc"), bylayer = TRUE, format = 'ascii', prj = T, overwrite = T)
  }
  
  if(var == "BV")
  {
    writeRaster(rasstack.interp, ".", filename= paste0("BtmVCurrent_", names, "_", year, ".asc"), bylayer = TRUE, format = 'ascii', prj = T, overwrite = T)
  }
  
  if(var == "Bstress")
  {
    writeRaster(rasstack.interp, ".", filename= paste0("BtmStress_", names, "_", year, ".asc"), bylayer = TRUE, format = 'ascii', prj = T, overwrite = T)
  }
  
  if(var == "MLD")
  {
    writeRaster(rasstack.interp, ".", filename= paste0("MLD_", names, "_", year, ".asc"), bylayer = TRUE, format = 'ascii', prj = T, overwrite = T)
  }
  
  if(var == "SS")
  {
    writeRaster(rasstack.interp, ".", filename= paste0("SurfaceSalinity_", names, "_", year, ".asc"), bylayer = TRUE, format = 'ascii', prj = T, overwrite = T)
  }
  
  if(var == "ST")
  {
    writeRaster(rasstack.interp, ".", filename= paste0("SurfaceTemp_", names, "_", year, ".asc"), bylayer = TRUE, format = 'ascii', prj = T, overwrite = T)
  }
}