# fsca transformation for the march 5th data

library(terra)

#bring in DEM
dem <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/dem_feb12-19.tif")
dem

# bringg in raw fSCA data downloaded from landsat web portal
fsca_raw <-rast(" /Users/jacktarricone/ch1_jemez_data/landsat_fsca/mar_05/mar_05/fsac_mar_05.tif")
fsca_raw # check
values(fsca_raw)[values(fsca_raw) == 0] = NA # 0 to NaN
fsca_raw <-fsca_raw/10 # correct [%] scale
plot(fsca_raw) # test plot

# resample, crop, and mask
fsca_crop <-resample(fsca_raw, dem, method = "bilinear")
fsca <-mask(fsca_crop, dem, maskvalue = NA)

# test plot
plot(dem)
plot(fsca, add=TRUE)

#save
writeRaster(fsca, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/landsat_fsca_3-05.tif")










# old code

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





