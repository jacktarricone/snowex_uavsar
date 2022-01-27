# rasters for ryan
# jan 27

library(terra)

#bring in UAVSAR rasters from the 2/12-2/19 pair
tifs <-list.files("/Users/jacktarricone/ch1_jemez_data/feb12-19", pattern = "*L090HH_*", full.names = TRUE)
print(tifs)
tifs <-tifs[-3] # delete .hgt

# define rasters
amp1 <-rast(tifs[1])
amp2 <-rast(tifs[2])
cor <-rast(tifs[3])
dem <-rast(tifs[4])
unw <-rast(tifs[5])

##### clean rasters
setwd("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan") 

# amp1
values(amp1)[values(amp1) == 0] = NA
amp1_dB <- 10.0 * log10(amp1) # conver to dB
plot(amp1_dB)
# writeRaster(amp1, "amp1_lin_feb12-19.tif")
# writeRaster(amp1_dB, "amp1_dB_feb12-19.tif")

# amp2
values(amp2)[values(amp2) == 0] = NA
amp2_dB <- 10.0 * log10(amp2)
plot(amp2_dB)
# writeRaster(amp2, "amp2_lin_feb12-19.tif")
# writeRaster(amp2_dB, "amp2_dB_feb12-19.tif")

# cor
values(cor)[values(cor) == 0] = NA
plot(cor)
#writeRaster(cor, "cor_feb12-19.tif")

# unw
values(unw)[values(unw) == 0] = NA
plot(unw)
# writeRaster(unw, "unw_raw_feb12-19.tif")


# den
values(dem)[values(dem) == -10000] = NA
plot(dem)
# writeRaster(dem, "dem_feb12-19.tif")


