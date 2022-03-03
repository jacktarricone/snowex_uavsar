# fsca transformation 
# jan 27th, 2022

library(terra)

#bring in DEM
dem <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/dem_feb12-19.tif")
dem

# bringg in raw fSCA data downloaded from landsat web portal
fsca_raw <-rast("/Users/jacktarricone/ch1_jemez_data/landsat_fsca/feb_18/LC08_CU_010012_20200218_20200227_C01_V01_SNOW.tif")
fsca_raw # check
# values(fsca_raw)[values(fsca_raw) == 0] = NA # 0 to NaN
fsca_raw <-fsca_raw/10 # correct [%] scale
plot(fsca_raw) # test plot

# resample, crop, and mask
fsca_crop <-resample(fsca_raw, dem, method = "bilinear")
fsca <-mask(fsca_crop, dem, maskvalue = NA)

# test plot
plot(dem)
plot(fsca, add=TRUE)

# save
# writeRaster(fsca, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/landsat_fsca_2-18v2.tif")



