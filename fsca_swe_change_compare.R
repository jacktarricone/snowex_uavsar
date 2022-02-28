# jack tarricone
# fsca loss vs cum swe change

library(terra)

setwd("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/")
list.files()

# read in fsca files
fsca_0218 <-rast("landsat_fsca_2-18v2.tif")
fsca_0305 <-rast("landsat_fsca_3-05v2.tif")

# calc change 
d_fsca_raw <-fsca_0305 - fsca_0218 
plot(d_fsca_raw)

# read in cum swe
d_swe <-rast("delta_swe_cum.tif")
plot(d_swe)

# crop to d_swe
fsca_crop <-crop(d_fsca_raw, ext(d_swe))
d_fsca <-mask(fsca_crop, d_swe)
plot(d_fsca)

# save
writeRaster(fsca_crop, "fsca_change_0218_0305.tif")
writeRaster(d_fsca, "mask_fsca_change_0218_0305.tif")

