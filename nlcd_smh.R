library(terra)

nlcd <-rast("/Users/jacktarricone/ch1_jemez_data/nlcd/NLCD_2019_Land_Cover_L48_20210604_Y6k85Ek4KV2m251VVyWr.tiff")
plot(nlcd)

shp<-vect("/Users/jacktarricone/ch1_jemez_data/vector_data/jpl_dem_extent.shp")

cor <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/cor_feb12-19.tif")
plot(cor)

nlcd_m <-mask(nlcd, shp)
plot(nlcd_m)
nlcd_cm <-mask(nlcd_m,shp)
plot(nlcd_cm)
writeRaster(nlcd_cm, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/nlcd_jpl_extent.tif")



nlcd_v2 <-project(nlcd, "epsg:4326")
plot(nlcd_v2)
nlcd_v2

nlcd_v3 <-mask(nlcd_v2, shp)
plot(nlcd_v3)

nlcd_v4 <-crop(nlcd_v3, cor)
plot(nlcd_v4)
writeRaster(nlcd_v4, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/nlcd_jpl_extent_v2.tif")
