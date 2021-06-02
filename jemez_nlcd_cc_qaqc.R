# resample canovery cover data and save it

library(raster)
library(data.table)
library(rgdal)
library(gdalUtils)
library(sp)
library(caTools)
library(rgdal)
library(rgeos)
library(ggplot2)

# bring in raw nlcd cc data
cc_2016_raw <-raster("/Volumes/JT/projects/uavsar/jemez/nlcd/canopy_cover/NLCD_2016_Tree_Canopy_L48_20190831_qjgnp0f0GtbvngXSVCOA.tiff")
plot(cc_2016_raw) # check
freq(cc_2016_raw, digits = 1)

# reproject to same as all o ther data
cc1 <-projectRaster(cc_2016_raw,
                     crs=crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(cc1)
freq(cc1, digits = 1)

#crop to DEM
cc_crop <-crop(cc1, cor)
plot(cc_crop)
hist(cc_crop)

# resample to get down to DEM res and save it
cc_crop_resamp <- resample(cc_crop, cor, method='bilinear')
cc_crop_resamp
plot(cc_crop_resamp)
writeRaster(cc_crop_resamp, "/Volumes/JT/projects/uavsar/jemez/nlcd/cc_crop_resamp.tif")

# mask for cor values and save it
cc_final <-mask(cc_crop_resamp, cor, maskvalue = NA)
plot(cc_final)
plot(cc_final, add=TRUE)
writeRaster(cc_final, "/Volumes/JT/projects/uavsar/jemez/nlcd/cc_final.tif")
