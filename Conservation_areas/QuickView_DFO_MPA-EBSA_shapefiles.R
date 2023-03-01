#Downloaded shapefiles March 01, 2022

library(mapview)
library(sf)

#Oceans Act Marine Protected Areas - Existing Network site
# https://open.canada.ca/data/en/dataset/a1e18963-25dd-4219-a33f-1a38c4971250
shp.1 <- st_read("Z:/GISdata/Conservation_areas/DFO_MPA_MPO_ZPM_SHP/DFO_MPA_MPO_ZPM.shp")
mapview::mapview(shp.1)
unique(shp.1$NAME_E)


#Other Effective Area-Based Conservation Measures
# https://open.canada.ca/data/en/dataset/44769543-7a23-4991-a53f-c2cf7c7a946f
shp.2 <- st_read("Z:/GISdata/Conservation_areas/DFO_OECM_MPO_AMCEZ_SHP/DFO_OECM_MPO_AMCEZ.shp")
mapview::mapview(shp.2)
unique(shp.2$NAME_E)

#EBSAS
# https://open.canada.ca/data/en/dataset/d2d6057f-d7c4-45d9-9fd9-0a58370577e0
shp.3 <- st_read("Z:/GISdata/Conservation_areas/DFO_EBSA/DFO_EBSA.shp")
mapview::mapview(shp.3)
unique(shp.3$Name)
