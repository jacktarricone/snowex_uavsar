library(terra)

# landsat imagery over the jemez for feb 18

# list list rgb bands from both days
setwd("/Users/jacktarricone/ch1_jemez_data/landsat_fsca/feb_18_tc/")
list <-list.files(pattern = "SR", full.names = TRUE) # list all files
print(list)

# full 12 band winter
landsat_raw <-rast(list) # create "SpatRaster" for winter image
landsat_raw # inspect
plot(landsat_raw[[2]], col = gray(0:100 / 100)) # grayscale test plot


#bring in DEM
dfsca <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/dfsca_30m.tif")
dfsca

# resample, crop, and mask
landsat_v1 <-resample(landsat_raw, dfsca, method = "bilinear")
plot(landsat_v1[[6]])
# hls <-mask(hls_crop, dem, maskvalue = NA)

# test plot using terra's plotRGB function
plotRGB(landsat_v1, r = 4, g = 3, b = 2, stretch = "hist")

# create RGB 3 band rasters to save
feb_18_rgb <-c(landsat_v1[[4]], landsat_v1[[3]], landsat_v1[[2]])

# test plot these
plotRGB(feb_18_rgb, stretch = "lin")
plotRGB(feb_18_rgb, stretch = "hist", bgalpha = 0, add = TRUE)

writeRaster(feb_18_rgb, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/landsat_feb18_rbg.tif")
