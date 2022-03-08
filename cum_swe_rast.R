# create cumulativ swe change raster
# redid swe change code to be proper
# march 7th, 2022

library(terra)

setwd("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/final_swe_change/")

# read in rasters we just made
dswe_feb19_26 <-rast("dswe_feb19-26.tif")
dswe_feb12_19_v1 <-rast("dswe_feb12-19.tif")

# crop down slightly
dswe_feb12_19 <-crop(dswe_feb12_19_v1, ext(dswe_feb19_26))

pair_1 <-mask(dswe_feb12_19, dswe_feb19_26, maskvalue = NA)
pair_2 <-mask(dswe_feb19_26, pair_1, maskvalue = NA)

# substract
dswe_cum <-pair_1 + pair_2
plot(dswe_cum)
writeRaster(dswe_cum,"dswe_cum.tif")
