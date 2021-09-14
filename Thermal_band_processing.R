

rm(list = ls())

library(rgdal)
library(raster)
library(stringr)
library(gdalUtils)
library(gstat)
library(sf)
library(leaflet)
# devtools::install_github("16EAGLE/getSpatialData")
library(getSpatialData)


## load shape file of all the builidng in Bracciano (from OpenStreetMap)

dir <- "D:/Landsat8_Thermal_BRACCIANO/Bracciano_shp"
### shapefile for Sicily
# shp_BRACCiANO <- readOGR(dsn = dir, layer = "buildings_Bracciano")
shp_BRACCiANO <- readOGR(dsn = dir, layer = "borders_Bracciano")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_BRACCiANO <- spTransform(shp_BRACCiANO, CRS("+init=epsg:4326"))
crs(shp_BRACCiANO)


shp_BRACCiANO@data$name <- 1:nrow(shp_BRACCiANO)
plot(shp_BRACCiANO)
e <- extent(shp_BRACCiANO)

plot(e)

# make a spatial polygon from the extent
p <- as(e, "SpatialPolygons")
plot(p)
proj4string(p) <- CRS("+proj=longlat +datum=WGS84")
# crs(p) <- "proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# save shp file for the rectangular DOMAIN
setwd("D:/Landsat8_Thermal_BRACCIANO/Bracciano_shp")
shapefile(p, "rectangular_domain.shp", overwrite=TRUE)

# reload and plot domain

dir <- "D:/Landsat8_Thermal_BRACCIANO/Bracciano_shp"
shp_rect <- readOGR(dsn = dir, layer = "rectangular_domain")
# ----- Transform to EPSG 4326 - WGS84 (required)
shp_rect <- spTransform(shp_rect, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
shp_rect@data$name <- 1:nrow(shp_rect)
plot(shp_rect)
## reporject shp_rect into meters
shp_rect  <- spTransform(shp_rect, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))



# read the Surface Temperature band from Collection 2 Level 2 of Landsat 8

##---JANUARY
B10_band <- raster("D:/Landsat8_Thermal_BRACCIANO/Landsat_Bracciano_TIR_2020/LC08_L2SP_191031_20200104_20200823_02_T1_ST_B10.tif")


plot(B10_band)
crs(B10_band)


## reproject the raster
###....use a reference raster
plot(B10_band)
### crop all over the BUILDINGS of BRACCIANO only
B10_band <- crop(B10_band, extent(shp_rect))
B10_band <- mask(B10_band, shp_rect) 
plot(B10_band)


## reproject reference from meters into latlong
B10_band <- projectRaster(B10_band, crs='+proj=longlat')
plot(B10_band)

### crop all over the BUILDINGS of BRACCIANO only
B10_band <- crop(B10_band, extent(shp_BRACCiANO))
B10_band <- mask(B10_band, shp_BRACCiANO) 
plot(B10_band)
plot(shp_BRACCiANO, add = TRUE)
## get all temperature points (lat, lon, temperature..)
points_B10 <-   rasterToPoints(B10_band) 
colnames(points_B10) <- c("x", "y", "B10_temp")
points_B10 <- as.data.frame((points_B10))


###---- paramenters to convertes Thermal Infrared bands into surface temperature
## https://www.usgs.gov/faqs/how-do-i-use-a-scale-factor-landsat-level-2-science-products?qt-news_science_products=0#qt-news_science_products

scale_factor <- 0.00341802 
offset <- 149

## calculate temperature in Celsius
TOA_B10 <- (scale_factor * points_B10$B10_temp + offset) - 273.15
TOA_B10 <- as.data.frame(TOA_B10)
TOA_B10$lon <- points_B10$x
TOA_B10$lat <- points_B10$y
write.csv(TOA_B10, file = paste("D:/Landsat8_Thermal_BRACCIANO/surf_temp.csv", sep = ""), row.names=FALSE)

### convert and save raster raster
B10_band_st <- (scale_factor * B10_band + offset) - 273.15
writeRaster(B10_band_st, paste0("D:/Landsat8_Thermal_BRACCIANO/B10_band_builidngs_Bracciano.tif") , options= "INTERLEAVE=BAND", overwrite=T)



#### make iterative map
map <- leaflet() %>%
  addTiles() %>%
  # addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addRasterImage(B10_band_st, opacity = 0.4, group = "Temperature_B0") %>%
addLayersControl(
  baseGroups = c("Toner"),
  overlayGroups = c("Temperature_B0"))
map





# ### save reference
# writeRaster(B10_band, paste0("D:/reference_landsat_BRACCIANO_meters.tif") , options= "INTERLEAVE=BAND", overwrite=T)
# 
# ## REFERENCE raster
# reference <- raster("D:/reference_landsat_BRACCIANO_meters.tif")
# crs(reference)
# plot(reference)
# ## reproject reference from meters into latlong
# reference <- projectRaster(reference, crs='+proj=longlat')
# plot(reference)
# 
# ### crop all over the BUILDINGS of BRACCIANO only
# reference <- crop(reference, extent(shp_BRACCiANO))
# reference <- mask(reference, shp_BRACCiANO) 
# plot(reference)


