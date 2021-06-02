# fsca transformation for second pair (need to see if i did this right)
# 4/28

library(raster)
library(data.table)
library(rgdal)
library(gdalUtils)
library(sp)
library(caTools)
library(rgdal)
library(rgeos)
library(ggplot2)

#bring in DEM
jemez_DEM <-raster("/Volumes/JT/projects/uavsar/jemez/rasters/02192020_02262020/DEM/alamos_35915_20008-000_20013-000_0007d_s01_L090HH_01.hgt.grd.tiff")
values(jemez_DEM)[values(jemez_DEM) == -10000] = NA
hist(jemez_DEM)
jemez_DEM


# bring in merged fsca, should do this in R next time so i know how

#fsca <-raster("/Volumes/JT/projects/uavsar/jemez/fsca/jemez_fsca.tif")
#values(fsca)[values(fsca) == 0] = NA

# bring in single image that is what we need
fsca_raw <-raster("/Volumes/JT/projects/uavsar/jemez/fsca/02_18_2020/LC08_CU_010012_20200218_20200227_C01_V01_SNOW/LC08_CU_010012_20200218_20200227_C01_V01_SNOW.tif")
fsca <-projectRaster(fsca_raw,
                     crs=crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
fsca #check 
fsca_raw # compare
values(fsca)[values(fsca) == 0] = NA

#test plot with DEM, this looks good but resolution all off?
plot(jemez_DEM)
plot(fsca, add=TRUE)

#crop to DEM
fsca_crop <-crop(fsca, jemez_DEM)
plot(fsca_crop)
hist(fsca_crop)

plot(jemez_DEM)
plot(fsca_crop, add=TRUE)

#save cropped
#writeRaster(fsca_crop, "/Volumes/JT/projects/uavsar/jemez/fsca/fsca_crop.tif")

# resample to get down to DEM res and save it
fsca_crop_resamp <- resample(fsca_crop, jemez_DEM, method='bilinear')
fsca_crop_resamp
#writeRaster(fsca_crop_resamp, "/Volumes/JT/projects/uavsar/jemez/fsca/fsca_crop_resamp.tif")

# mask for cor values and save it
fsca_final <-mask(fsca_crop_resamp, cor, maskvalue = NA)
plot(cor)
plot(fsca_final, add=TRUE)
writeRaster(fsca_final, "/Volumes/JT/projects/uavsar/jemez/fsca/fsca_final_secondpair.tif")

#create snow mask for pixels that are over 90 percent snow
snow_mask <-fsca_final
values(snow_mask)[values(snow_mask) <= 900.00] = NA
plot(snow_mask)
writeRaster(snow_mask, "/Volumes/JT/projects/uavsar/jemez/fsca/snow_mask_90p.tif")

#create no snow mask for pixels that are over 90 percent snow
no_snow_mask <-fsca_final
values(no_snow_mask)[values(is.na(no_snow_mask))] = 1
values(no_snow_mask)[values(no_snow_mask) < 900.00] = 1
values(no_snow_mask)[values(no_snow_mask) > 900.00] = NA
no_snow_mask <-mask(no_snow_mask, cor, maskvalue = NA)
plot(no_snow_mask)
hist(no_snow_mask)
plot(snow_mask, add = TRUE)
writeRaster(no_snow_mask, "/Volumes/JT/projects/uavsar/jemez/fsca/02_18_2020/no_snow_mask_90p.tif")


