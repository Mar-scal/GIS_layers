library(terra)

##VISUAL CHECK MONTHLY LAYERS - BY YEAR
#Get rasters
year <- "2025"
#path <- paste0("Z:/Projects/OceanographicData/GLORYs/Monthly_BtmTemp_cropped_to_MaritimeExtent/500m_resolution/",year)
#path <- paste0("W:/OffshoreWind/Boundaries_Covariates/Envt_Covariates/Glorys/Glorys_MonthlyBottomTemp/Snapped_to_ofiBathy/",year)

path <- paste0("Z:/Projects/OceanographicData/GLORYs/Monthly_Salinity_cropped_to_MaritimeExtent/snapped_to_ofibathy_500m/",year)

#path <- paste0("Z:/Projects/OceanographicData/GLORYs/Monthly_BtmTemp_cropped_to_MaritimeExtent/Snapped_to_ofiBathy/",year)
ras_list<- dir(path, pattern = paste0("tif$"), full.names = TRUE, recursive = TRUE)

r <- lapply(ras_list, terra::rast) #bring in as raster (list)
#plot(r[[10]])

#Stack rasters
r.1.12 <- c(r[[1]],r[[2]],r[[3]],r[[4]],r[[5]],r[[6]],r[[7]],r[[8]],r[[9]],r[[10]],r[[11]],r[[12]])
plot(r.1.12)

plot(r[[1]])
plot(r[[7]])

#Check crs
#crs(r[[1]])
#crs(r[[5]])
#crs(r[[11]])
#crs(r[[11]])
#res(r[[11]])

####################################################################


#Get rasters
#path <- "Z:/Projects/OceanographicData/GLORYs/Mean_SummerBTMs/500m_resolution"

path <- "Z:/Projects/OceanographicData/GLORYs/Mean_SummerBTMs/500m_resolution/JulytoSept/"

ras_list<- dir(path, pattern = paste0("tif$"), full.names = TRUE, recursive = TRUE)

r <- lapply(ras_list, terra::rast) #bring in as raster (list)
plot(r[[10]])

#Stack rasters
r.1993.2002 <- c(r[[1]],r[[2]],r[[3]],r[[4]],r[[5]],r[[6]],r[[7]],r[[8]],r[[9]],r[[10]])
#terra raster stack
plot(r.1993.2002)

r.2003.2012 <- c(r[[11]],r[[12]],r[[13]],r[[14]],r[[15]],r[[16]],r[[17]],r[[18]],r[[19]],r[[20]])
plot(r.2003.2012)

r.2013.2022 <- c(r[[21]],r[[22]],r[[23]],r[[24]],r[[25]],r[[26]],r[[27]],r[[28]],r[[29]],r[[30]])
plot(r.2013.2022)


r.2023.2025 <- c(r[[31]],r[[32]],r[[33]])
plot(r.2023.2025)

####################################################################

library(terra)

#Get rasters
path <- "Z:/Projects/OceanographicData/GLORYs/Mean_Yearly_BtmTemp_cropped_to_MaritimeExtent/500m_resolution"
ras_list<- dir(path, pattern = paste0("tif$"), full.names = TRUE, recursive = TRUE)

r <- lapply(ras_list, terra::rast) #bring in as raster (list)
plot(r[[10]])

#Stack rasters
r.1993.2002 <- c(r[[1]],r[[2]],r[[3]],r[[4]],r[[5]],r[[6]],r[[7]],r[[8]],r[[9]],r[[10]])
#terra raster stack
plot(r.1993.2002)

r.2003.2012 <- c(r[[11]],r[[12]],r[[13]],r[[14]],r[[15]],r[[16]],r[[17]],r[[18]],r[[19]],r[[20]])
plot(r.2003.2012)

r.2013.2022 <- c(r[[21]],r[[22]],r[[23]],r[[24]],r[[25]],r[[26]],r[[27]],r[[28]],r[[29]],r[[30]])
plot(r.2013.2022)


r.2023.2024 <- c(r[[31]],r[[32]])
plot(r.2023.2024)