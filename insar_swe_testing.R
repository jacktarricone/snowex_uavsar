# first attempt at making delta SWE rasters
# 

library(raster)
library(data.table)
library(rgdal)
library(gdalUtils)
library(sp)
library(caTools)
library(rgdal)
library(rgeos)
library(ggplot2)

i_angle <-raster("/Volumes/JT/projects/uavsar/jemez/inc/i_angle_deg.tif")
i_angle

density <- .33
di_elc <- 1.7
wL <- 23.8403545

constant <-cos(look_angle) - sqrt(di_elc - sin((look_angle)^2))

phase_change <-((4*pi)/wL)*(-constant)(deltaSWE)
deltaSWE = phase_change*(wL/(4*pi))*(-constant)
final_constant <-(wL/(4*pi))*(-constant)
                                 
fsca <-raster("/Volumes/JT/projects/uavsar/jemez/fsca/jemez_fsca.tif")
fsca1 <-raster("/Volumes/JT/projects/uavsar/jemez/fsca/LC08_CU_010012_20200218_20200227_C01_V01_SNOW/LC08_CU_010012_20200218_20200227_C01_V01_SNOW.tif") 
hist(fsca1)
plot(fsca1)
