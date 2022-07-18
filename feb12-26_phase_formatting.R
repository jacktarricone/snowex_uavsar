library(terra)

unw_isce <-rast("/Users/jacktarricone/Desktop/jemez_geoloc_rough/unw_gecoded_feb12-26.tif")
unw_isce
plot(unw_isce[[2]])

unw_cm <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/unw_cm.tif")
unw_cm
plot(unw_cm, add = TRUE)

unw_isce_v1 <-crop(unw_isce[[2]], ext(unw_cm))
plot(unw_isce_v1)

unw_isce_v2 <-resample(unw_isce_v1, unw_cm)
unw_isce_v2
plot(unw_isce_v2)
plot(unw_cm, add = TRUE)

unw_isce_v3 <-mask(unw_isce_v2, unw_cm)
unw_isce_v3
plot(unw_isce_v3)
plot(unw_cm, add = TRUE)
writeRaster(unw_isce_v3, "/Users/jacktarricone/Desktop/jemez_geoloc_rough/unw_gecoded_feb12-26_v3.tif")


unw_isce_v4 <-unw_isce_v3 -4
plot(unw_isce_v4)
writeRaster(unw_isce_v4, "/Users/jacktarricone/Desktop/jemez_geoloc_rough/unw_gecoded_feb12-26_v4.tif")


