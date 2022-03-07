library(terra)

# RBG HLS imagery over the jemez for march 5th

# list list rgb bands from both days
setwd("/Users/jacktarricone/ch1_jemez_data/landsat_fsca/feb_18_tc/")
list <-list.files(pattern = "SR", full.names = TRUE) # list all files
print(list)

# full 12 band winter
hls_raw <-rast(list) # create "SpatRaster" for winter image
hls_raw # inspect
plot(hls_raw[[2]], col = gray(0:100 / 100)) # grayscale test plot


#bring in DEM
dem <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/dem_feb12-19.tif")
dem

# resample, crop, and mask
hls_crop <-resample(hls, dem, method = "bilinear")
hls <-mask(hls_crop, dem, maskvalue = NA)

# test plot using terra's plotRGB function
plotRGB(hls, r = 4, g = 3, b = 2, stretch = "hist")

# create RGB 3 band rasters to save
mar_05_rgb <-c(hls[[4]], hls[[3]], hls[[2]])

# test plot these
plotRGB(mar_05_rgb, stretch = "lin")
plotRGB(mar_05_rgb, stretch = "hist", bgalpha = 0, add = TRUE)

writeRaster(mar_05_rgb, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/hls_rbg_mar05.tif")
