
################################################################################

#Averaging all years or months to get bottom BNAM climatology layer (1990-2019)

################################################################################

#Using parallel processing

library(raster)
library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)     #Provides foreach looping construct
library(ClusterR)  #Used for calculating mean layer

#SELECT BNAM VARIABLE
var <- "^SurfaceTemp" #^BtmSalinity, ^BtmTemp, ^SurfaceTemp, ^MLD, ^BtmStress
month <- "Apr" #"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
path <- "E:/BNAM/Revised/BNAM_1990-2019_monthly/" #Where have you saved these!?  ¯\(°_o)/¯

#Define how many cores you want to use
UseCores <- detectCores() -1

#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)


#ras_list <- dir(path, pattern = paste0(var,".","*asc"), full.names = TRUE, recursive = TRUE) #All layers 
ras_list <- dir(path, pattern = paste0("^", var, "_", month, ".","*asc"), full.names = TRUE, recursive = TRUE) #By month


start_time <- Sys.time()
#Use foreach loop and %dopar% command
ras <- foreach(i=1:length(ras_list)) %dopar% {
  library(raster)
  rasterList1 <- lapply(ras_list[i], raster)
  #ras.stack  <- stack(rasterList1)
}
#end cluster
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


ras.stack  <- stack(unlist(ras))
#class(ras.stack)

# Now calculate the mean of all layers in stack using ClusterR.
start_time <- Sys.time()
beginCluster()
ras.mean <- clusterR(ras.stack, calc, args=list(mean, na.rm=T))
endCluster()
end_time <- Sys.time()
end_time - start_time

plot(ras.mean)
crs(ras.mean) #check coord ref sys.

#Save Raster (full extent):
#writeRaster(ras.mean, filename= "E:/BOF_envirodataset/BNAM_MLD_1990-2019_mnthly_mean.gri", format = 'raster', prj = T, overwrite = T)

# -----Re-project and crop to extent of study area:-------------------------------------------------------

#Load Raster with desired extent/resolution
bathy <- raster("E:/BOF_envirodataset/BOF_ALLBath_2010_50m_adj_dodd_gsc_finalc_z20.grd")
crs(bathy)

ras.mean <- projectRaster(ras.mean, bathy, crs = crs(bathy))
ras.mean <- crop(ras.mean, bathy)

#Resample to resolution of bathy:
ras.mean <- resample(ras.mean, bathy, method = "bilinear")
ras.mean <- mask(ras.mean, bathy)

plot(ras.mean)

var.name <- substring(var, 2)
writeRaster(ras.mean, filename= paste0("E:/BOF_envirodataset/BOF_BNAM_",var.name,"_",month,"_90-19_mean.gri"), format = 'raster', prj = T, overwrite = T)

