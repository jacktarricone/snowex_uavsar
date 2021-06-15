# june 10th 2021
# testing working with RTC corrected sentinal data in R

library(gridExtra)
library(data.table)
library(gdalUtils)
library(sp)
library(caTools)
library(rgdal)
library(rgeos)
library(ggplot2)
library(raster)
library(zoo)


##### sentinal data
# bring in rasters
#vv <-raster("/Users/jacktarricone/Downloads/S1B_IW_20191231T010049_DVP_RTC30_G_saunem_3303/S1B_IW_20191231T010049_DVP_RTC30_G_saunem_3303_VV.tif")
#vv2 <-raster("/Users/jacktarricone/Downloads/S1B_IW_20191008T010051_DVP_RTC30_G_saunem_AC9F/S1B_IW_20191008T010051_DVP_RTC30_G_saunem_AC9F_VV.tif")

files_list <-list.files("/Users/jacktarricone/Desktop/sen1_data", pattern = ".tif", full.names = TRUE)
rast_list <-lapply(files_list, function(x) raster(x))
rast_list # list of rasters

# set crop extent
crop <-extent(357000, 382000, 3950000, 4000000)

# function to crop
crop_list <-function(x){
  crop(x, extent(crop))
}

# lapp
cropped <-lapply(rast_list, crop_list)
vv_stack <-stack(cropped)
vv_db_stack <-10*log10(vv_stack)
plot(vv_db_stack[[19]])
vv_db_stack[[15]]
writeRaster(vv_db_stack[[15]], "/Volumes/JT/projects/uavsar/jemez/sen1/sen1_2020_01_17_VV.tif", overwrite = TRUE)

jemez_stack <-raster("/Volumes/JT/projects/uavsar/jemez/sen1/jemez_sen1_VV_stack.tif")
