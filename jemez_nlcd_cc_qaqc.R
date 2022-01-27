# save and crop the NLCD land cover and canopy cover data
# jan 27th, 2022

library(terra)
library(raster)

# set saving location
setwd("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/")

#bring in DEM
dem <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/dem_feb12-19.tif")
dem

# bring in raw nlcd 2026 cc data
cc_raw <-rast("/Users/jacktarricone/ch1_jemez_data/nlcd/NLCD_2016_Tree_Canopy_L48_20190831_Y6k85Ek4KV2m251VVyWr.tiff")
cc_raw
plot(cc_raw)

# landcover 2019
lc_raw <-rast("/Users/jacktarricone/ch1_jemez_data/nlcd/NLCD_2019_Land_Cover_L48_20210604_Y6k85Ek4KV2m251VVyWr.tiff")
lc_raw
plot(lc_raw)

#################
# resample, crop, and mask canopy cover
##############

cc_crop <-resample(cc_raw, dem, method = "bilinear")
cc <-mask(cc_crop, dem, maskvalue = NA)

# test plot
plot(dem)
plot(cc, add=TRUE)
# writeRaster(cc, "cc_percent.tif")



#################
# resample, crop, and mask canopy cover
##############

# used nearest neighbor for categorical data here
lc_crop <-resample(lc_raw, dem, method = "near")
plot(lc_crop)
lc <-mask(lc_crop, dem, maskvalue = NA)

# test plot
plot(dem)
plot(lc, add=TRUE)
# writeRaster(lc, "nlcd_land_cover.tif")
