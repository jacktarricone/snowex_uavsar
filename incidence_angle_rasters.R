# incidence angle raster creation
# need to make two seperate ones for both days
# first just do it for anlge, and then use dem to create "local" angle
# 3/23

#geolocating is right, talk to HP about it

library(raster)
library(data.table)
library(rgdal)
library(gdalUtils)
library(sp)
library(caTools)
library(rgdal)
library(rgeos)
library(ggplot2)

# import DEM
jemez_DEM <-raster("/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/DEM/alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.hgt.grd.tiff")
values(jemez_DEM)[values(jemez_DEM) == -10000] = NA
hist(jemez_DEM)
plot(jemez_DEM)
jemez_DEM

# 2/12 - 2/19: 
# Average Look Angle in Near Range (deg() = 28.01
# Average Look Angle in Far Range (deg) = 68.9

# important i_angle raster from 2/12
# made the envi header by hand but need to rework my python reader

i_angle <-raster("/Volumes/JT/projects/uavsar/jemez/inc/alamos_35915_20005_003_200212_L090_CX_01.inc")
values(i_angle)[values(i_angle) == -10000] = NA
plot(i_angle)
#writeRaster(i_angle, "/Volumes/JT/projects/uavsar/jemez/inc/i_angle_rads.tif")

#convert rads to degrees
i_angle_deg <- calc(i_angle, fun=function(x){x * (180/pi)})
plot(i_angle_deg)
writeRaster(i_angle_deg, "/Volumes/JT/projects/uavsar/jemez/inc/i_angle_deg.tif")

# import dem
dem <-raster("/Volumes/JT/projects/uavsar/jemez/inc/alamos_35915_20005_003_200212_L090_CX_01.hgt")
values(dem)[values(dem) == -10000] = NA
plot(dem)
dem

#make slope raster from dem
slope <-terrain(dem, opt="slope", unit="degrees", neighbors = "8")
plot(slope)
writeRaster(slope, "/Volumes/JT/projects/uavsar/jemez/inc/slope.tif")

#make aspect raster from dem
aspect <-terrain(dem, opt="aspect", unit="degrees", neighbors = "8")
plot(aspect)
writeRaster(aspect, "/Volumes/JT/projects/uavsar/jemez/inc/aspect.tif")
