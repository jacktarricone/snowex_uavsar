# fsca transformation (need to see if i did this right)
# 3/23

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
jemez_DEM <-raster("/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/DEM/alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.hgt.grd.tiff")
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
plot(dem)
plot(fsca, add=TRUE)

#crop to DEM
fsca_crop <-crop(fsca, insar_stack)
plot(fsca_crop)
hist(fsca_crop)

plot(dem)
plot(fsca_crop, add=TRUE)

# resample to get down to DEM res and save it
fsca_crop_resamp <- resample(fsca_crop, insar_stack, method='bilinear')
fsca_crop_resamp

# mask for cor values and save it
fsca_final <-mask(fsca_crop_resamp, cor, maskvalue = NA)
plot(cor)
plot(fsca_final, add=TRUE)
writeRaster(fsca_final, "/Volumes/JT/projects/uavsar/jemez/atm_correct/fsca.tif")

#create snow mask for pixels that are over 90 percent snow
snow_mask <-fsca_final
values(snow_mask)[values(snow_mask) > 0] = 1
plot(snow_mask)
#writeRaster(snow_mask, "/Volumes/JT/projects/uavsar/jemez/atm_correct/snow_mask.tif")

#create no snow mask for pixels that are over 90 percent snow
no_snow_mask <-fsca_final
values(no_snow_mask)[values(is.na(no_snow_mask))] = -999
values(no_snow_mask)[values(no_snow_mask) > 0] = NA
values(no_snow_mask)[values(no_snow_mask) < 0] = 1
no_snow_mask <-mask(no_snow_mask, cor, maskvalue = NA)
plot(no_snow_mask)
hist(no_snow_mask)
plot(snow_mask, add = TRUE)
#writeRaster(no_snow_mask, "/Volumes/JT/projects/uavsar/jemez/atm_correct/no_snow_mask.tif")





