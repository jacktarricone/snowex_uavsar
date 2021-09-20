# create path length raster
# updated sept 15th

library(rgdal)
library(data.table)
library(caTools)
library(sf)
library(ggmap)
library(terra)

setwd("/Volumes/JT/projects/uavsar/jemez/look_vector/good_llh_vrt/")

###########################################################
##### read in look vector new geotiffs from python code ###
###########################################################

# LKV file (.lkv): look vector at the target pointing from the aircraft to the ground, 
# in ENU (east, north, up) components.

# up in meters
up <-rast("/Volumes/JT/projects/uavsar/jemez/look_vector/good_llh_vrt/geocoded_up.tif")
up
plot(up)

# north in meters
north <-rast("/Volumes/JT/projects/uavsar/jemez/look_vector/good_llh_vrt/geocoded_north.tif")
north
plot(north)

# east in meters
east <-rast("//Volumes/JT/projects/uavsar/jemez/look_vector/good_llh_vrt/geocoded_east.tif")
east
plot(east)

# stack just bc
lvk_list <-list.files(pattern ="/*.tif", full.names = T)

# delete raw rasters, check to make sure it's right onces numbers change when deleted
lvk_list <-lvk_list[-1]
lvk_list <-lvk_list[-2]
lvk_list <-lvk_list[-3]

# works
lvk_stack <-rast(lvk_list)
plot(lvk_stack)


# function triangulate distance to plane from up and east rasters
plv_km_convert <-function(east_rast, up_rast){
  
plv_m <-((east_rast^2)+(up_rast^2))^.5
plot(plv_m)
plv_km <-plv_m/1000
return(plv_km)

}

# convert
plv_km <-plv_km_convert(east,up)
plot(plv_km)
plv_km
# writeRaster(plv_km, "/Volumes/JT/projects/uavsar/jemez/look_vector/good_llh_vrt/plv_km.tif")

